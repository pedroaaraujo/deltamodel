program test_pool;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, syncobjs, DB, SQLDB, sqlite3conn,
  DeltaModel, DeltaModel.ORM.Pool, DeltaModel.ORM.Connection,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Types, DatabaseURLParser;

type
  { Worker thread for concurrency test }
  TWorkerThread = class(TThread)
  private
    FPool: TDeltaConnectionPool;
    FWorkerId: Integer;
    FSuccess: Boolean;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TDeltaConnectionPool; AId: Integer);
    property Success: Boolean read FSuccess;
    property ErrorMessage: string read FErrorMessage;
  end;

constructor TWorkerThread.Create(APool: TDeltaConnectionPool; AId: Integer);
begin
  inherited Create(True);
  FPool := APool;
  FWorkerId := AId;
  FSuccess := False;
  FErrorMessage := '';
  FreeOnTerminate := False;
end;

procedure TWorkerThread.Execute;
var
  Lease: IDeltaPooledEngine;
  DS: TDataSet;
  I: Integer;
begin
  try
    for I := 1 to 5 do
    begin
      // 1. Acquire pooled connection via RAII (IDeltaPooledEngine)
      Lease := FPool.Acquire(5000);
      try
        // 2. Execute operations on shared database
        Lease.ExecuteDirect('PRAGMA busy_timeout = 5000');
        Lease.StartTransaction;
        Lease.ExecuteDirect(Format(
          'INSERT INTO test_log (thread_id, iteration, msg) VALUES (%d, %d, ''trabalhando'')',
          [FWorkerId, I]));
        Lease.Commit;

        DS := Lease.ExecuteQuery(Format(
          'SELECT COUNT(*) FROM test_log WHERE thread_id = %d',
          [FWorkerId]));
        try
          if DS.Fields[0].AsInteger < I then
            raise Exception.CreateFmt('Contagem inconsistente na thread %d', [FWorkerId]);
        finally
          DS.Free;
        end;

        Sleep(10);
      finally
        Lease := nil;
      end;
      Sleep(5);
    end;
    FSuccess := True;
  except
    on E: Exception do
    begin
      FSuccess := False;
      FErrorMessage := E.Message;
    end;
  end;
end;

procedure LogActionCallback(AEngine: TDeltaORMEngine);
begin
  AEngine.StartTransaction;
  AEngine.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (888, 1, ''via callback'')');
  AEngine.Commit;
end;

procedure LogTxCallback(AEngine: TDeltaORMEngine);
begin
  AEngine.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (888, 2, ''em transacao'')');
end;

procedure RunTests;
const
  TEST_DB_PATH = '/tmp/deltamodel_pool_test.db';
var
  DbURL: string;
  Pool: TDeltaConnectionPool;
  Cfg: TDeltaPoolConfig;
  Lease: IDeltaPooledEngine;
  L1, L2, L3, L4: IDeltaPooledEngine;
  RawEng: TDeltaORMEngine;
  TimeoutCaught: Boolean;
  Threads: array[0..9] of TWorkerThread;
  I: Integer;
  DS: TDataSet;
  St: TDeltaPoolStats;
begin
  WriteLn('====================================================');
  WriteLn('   Iniciando Testes do Connection Pool DeltaModel   ');
  WriteLn('====================================================');

  if FileExists(TEST_DB_PATH) then
    DeleteFile(TEST_DB_PATH);

  ExecuteProcess('/usr/bin/sqlite3', [TEST_DB_PATH, 'PRAGMA journal_mode=WAL; CREATE TABLE test_log (id INTEGER PRIMARY KEY AUTOINCREMENT, thread_id INTEGER, iteration INTEGER, msg TEXT);']);

  DbURL := Format('sqlite://%s?pool_min=2&pool_max=4&pool_timeout=1500&pool_test=true', [TEST_DB_PATH]);

  // TEST 1: URL configuration parser
  Write('Teste 1: Parser de configuracao da URL... ');
  Cfg := PoolConfigFromURL(DbURL);
  if (Cfg.MinConnections <> 2) or (Cfg.MaxConnections <> 4) or (Cfg.AcquireTimeoutMs <> 1500) or (not Cfg.TestOnBorrow) then
  begin
    WriteLn('FALHOU!');
    WriteLn(Format('Min: %d (esperado 2), Max: %d (esperado 4), Timeout: %d (esperado 1500)',
      [Cfg.MinConnections, Cfg.MaxConnections, Cfg.AcquireTimeoutMs]));
    Halt(1);
  end;
  WriteLn('OK!');

  // TEST 2: Pool creation and WarmUp
  Write('Teste 2: Criacao do Pool e WarmUp (MinConnections)... ');
  Pool := TDeltaConnectionPool.Create(DbURL);
  try
    if Pool.TotalCount <> 2 then
    begin
      WriteLn(Format('FALHOU: TotalCount=%d (esperado 2)', [Pool.TotalCount]));
      Halt(1);
    end;
    if Pool.IdleCount <> 2 then
    begin
      WriteLn(Format('FALHOU: IdleCount=%d (esperado 2)', [Pool.IdleCount]));
      Halt(1);
    end;
    WriteLn('OK!');

    // TEST 3: RAII lease and automatic release on scope exit
    Write('Teste 3: Emprestimo e devolucao automatica RAII... ');
    Lease := Pool.Acquire;
    if Pool.ActiveCount <> 1 then
    begin
      WriteLn(Format('FALHOU: ActiveCount=%d (esperado 1)', [Pool.ActiveCount]));
      Halt(1);
    end;
    Lease := nil; // Release
    if Pool.ActiveCount <> 0 then
    begin
      WriteLn(Format('FALHOU: ActiveCount apos release=%d (esperado 0)', [Pool.ActiveCount]));
      Halt(1);
    end;
    WriteLn('OK!');

    // TEST 4: Pending transaction sanitization on release
    Write('Teste 4: Sanitizacao de transacao pendente (Auto-Rollback)... ');
    RawEng := Pool.AcquireRaw;
    RawEng.StartTransaction;
    RawEng.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (999, 1, ''nao comitado'')');
    // Release while transaction is still active!
    Pool.Release(RawEng);

    // Verify rollback occurred
    Lease := Pool.Acquire;
    DS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id = 999');
    try
      if DS.Fields[0].AsInteger <> 0 then
      begin
        WriteLn('FALHOU: Registro nao comitado nao foi revertido!');
        Halt(1);
      end;
    finally
      DS.Free;
    end;
    Lease := nil;
    WriteLn('OK!');

    // TEST 5: Multi-threaded concurrency
    WriteLn('Teste 5: Execucao multithread concorrente (10 threads vs pool max 4)...');
    for I := 0 to 9 do
      Threads[I] := TWorkerThread.Create(Pool, I + 1);

    for I := 0 to 9 do
      Threads[I].Start;

    for I := 0 to 9 do
    begin
      Threads[I].WaitFor;
      if not Threads[I].Success then
      begin
        WriteLn(Format('FALHOU na thread %d: %s', [I + 1, Threads[I].ErrorMessage]));
        Halt(1);
      end;
      Threads[I].Free;
    end;

    // Verify total rows written across all threads (10 threads * 5 inserts = 50)
    Lease := Pool.Acquire;
    DS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id <= 10');
    try
      if DS.Fields[0].AsInteger <> 50 then
      begin
        WriteLn(Format('FALHOU: Esperado 50 registros, encontrado %d', [DS.Fields[0].AsInteger]));
        Halt(1);
      end;
    finally
      DS.Free;
    end;
    Lease := nil;
    WriteLn('   -> 10 threads concorrentes finalizaram com 100% de sucesso! OK!');

    // TEST 6: Pool exhaustion and Timeout
    Write('Teste 6: Esgotamento do pool e Timeout... ');
    TimeoutCaught := False;
    L1 := Pool.Acquire;
    L2 := Pool.Acquire;
    L3 := Pool.Acquire;
    L4 := Pool.Acquire;

    try
      // Attempt to acquire 5th connection with 300ms timeout
      Pool.Acquire(300);
    except
      on E: EDeltaPoolTimeoutException do
        TimeoutCaught := True;
    end;

    // Release all 4 connections
    L1 := nil;
    L2 := nil;
    L3 := nil;
    L4 := nil;

    if not TimeoutCaught then
    begin
      WriteLn('FALHOU: Esperava EDeltaPoolTimeoutException ao estourar o limite!');
      Halt(1);
    end;
    WriteLn('OK!');

    // TEST 7: Scoped Pool.Execute and Pool.InTransaction
    Write('Teste 7: Callbacks Pool.Execute e Pool.InTransaction... ');
    Pool.Execute(@LogActionCallback);
    Pool.InTransaction(@LogTxCallback);

    Lease := Pool.Acquire;
    DS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id = 888');
    try
      if DS.Fields[0].AsInteger <> 2 then
      begin
        WriteLn(Format('FALHOU: Esperado 2 registros, encontrado %d', [DS.Fields[0].AsInteger]));
        Halt(1);
      end;
    finally
      DS.Free;
    end;
    Lease := nil;
    WriteLn('OK!');

    // TEST 8: Pool metrics (Observability)
    Write('Teste 8: Metricas e Observabilidade (Stats)... ');
    St := Pool.Stats;
    if (St.TotalAcquisitions <= 0) or (St.TotalReleases <= 0) or (St.ConnectionsCreated <= 0) or (St.PeakActiveConnections <= 0) then
    begin
      WriteLn('FALHOU: Metricas inconsistentes');
      Halt(1);
    end;
    WriteLn(Format('OK! (Aquisicoes: %d, Devolucoes: %d, Criadas: %d, Pico Ativo: %d, Timeouts: %d)',
      [St.TotalAcquisitions, St.TotalReleases, St.ConnectionsCreated, St.PeakActiveConnections, St.TotalTimeouts]));

  finally
    Pool.Free;
  end;

  if FileExists(TEST_DB_PATH) then
    DeleteFile(TEST_DB_PATH);

  WriteLn('====================================================');
  WriteLn('   TODOS OS TESTES FORAM CONCLUÍDOS COM SUCESSO!    ');
  WriteLn('====================================================');
end;

begin
  try
    RunTests;
  except
    on E: Exception do
    begin
      WriteLn('ERRO FATAL: ', E.ClassName, ': ', E.Message);
      DumpExceptionBackTrace(output);
      Halt(1);
    end;
  end;
end.
