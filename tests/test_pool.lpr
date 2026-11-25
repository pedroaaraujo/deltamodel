program test_pool;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, syncobjs, sqlite3conn,
  DeltaModel, DeltaModel.ORM.Pool, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Types, DeltaModel.ORM.Dialects,
  DeltaModel.ORM.Driver.SQLDB, DatabaseURLParser;

type
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
  Lease: IDeltaConnection;
  RS: IDeltaResultSet;
  I: Integer;
begin
  try
    for I := 1 to 5 do
    begin
      Lease := FPool.Acquire(5000);
      try
        Lease.ExecuteDirect('PRAGMA busy_timeout = 5000');
        Lease.StartTransaction;
        Lease.ExecuteDirect(Format(
          'INSERT INTO test_log (thread_id, iteration, msg) VALUES (%d, %d, ''trabalhando'')',
          [FWorkerId, I]));
        Lease.Commit;

        RS := Lease.ExecuteQuery(Format(
          'SELECT COUNT(*) FROM test_log WHERE thread_id = %d',
          [FWorkerId]));
        try
          if (not RS.Next) or (RS.AsInteger(0) < I) then
            raise Exception.CreateFmt('Contagem inconsistente na thread %d', [FWorkerId]);
        finally
          RS.Close;
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

procedure LogActionCallback(const AConn: IDeltaConnection);
begin
  AConn.StartTransaction;
  AConn.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (888, 1, ''via callback'')');
  AConn.Commit;
end;

procedure LogTxCallback(const AConn: IDeltaConnection);
begin
  AConn.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (888, 2, ''em transacao'')');
end;

procedure RunTests;
const
  TEST_DB_PATH = '/tmp/deltamodel_pool_test.db';
var
  DbURL: string;
  Pool: TDeltaConnectionPool;
  Cfg: TDeltaPoolConfig;
  Lease: IDeltaConnection;
  L1, L2, L3, L4: IDeltaConnection;
  RawConn: IDeltaConnection;
  TimeoutCaught: Boolean;
  Threads: array[0..9] of TWorkerThread;
  I: Integer;
  RS: IDeltaResultSet;
  St: TDeltaPoolStats;
begin
  WriteLn('====================================================');
  WriteLn('   Iniciando Testes do Connection Pool DeltaModel   ');
  WriteLn('====================================================');

  if FileExists(TEST_DB_PATH) then
    DeleteFile(TEST_DB_PATH);

  ExecuteProcess('/usr/bin/sqlite3', [TEST_DB_PATH, 'PRAGMA journal_mode=WAL; CREATE TABLE test_log (id INTEGER PRIMARY KEY AUTOINCREMENT, thread_id INTEGER, iteration INTEGER, msg TEXT);']);

  DbURL := Format('sqlite://%s?pool_min=2&pool_max=4&pool_timeout=1500&pool_test=true', [TEST_DB_PATH]);

  Write('Teste 1: Parser de configuracao da URL... ');
  Cfg := PoolConfigFromURL(DbURL);
  if (Cfg.MinConnections <> 2) or (Cfg.MaxConnections <> 4) or (Cfg.AcquireTimeoutMs <> 1500) or (not Cfg.TestOnBorrow) then
  begin
    WriteLn('FALHOU!');
    Halt(1);
  end;
  WriteLn('OK!');

  Write('Teste 2: Criacao do Pool e WarmUp (MinConnections)... ');
  Pool := TDeltaConnectionPool.Create(DbURL);
  try
    if (Pool.TotalCount <> 2) or (Pool.IdleCount <> 2) then
    begin
      WriteLn(Format('FALHOU: Total=%d Idle=%d', [Pool.TotalCount, Pool.IdleCount]));
      Halt(1);
    end;
    WriteLn('OK!');

    Write('Teste 3: Emprestimo e devolucao automatica RAII... ');
    Lease := Pool.Acquire;
    if Pool.ActiveCount <> 1 then Halt(1);
    Lease := nil;
    if Pool.ActiveCount <> 0 then Halt(1);
    WriteLn('OK!');

    Write('Teste 4: Sanitizacao de transacao pendente (Auto-Rollback)... ');
    RawConn := Pool.AcquireRaw;
    RawConn.StartTransaction;
    RawConn.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (999, 1, ''nao comitado'')');
    Pool.Release(RawConn);
    RawConn := nil;

    Lease := Pool.Acquire;
    RS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id = 999');
    try
      if (not RS.Next) or (RS.AsInteger(0) <> 0) then
      begin
        WriteLn('FALHOU: Registro nao comitado nao foi revertido!');
        Halt(1);
      end;
    finally
      RS.Close;
    end;
    Lease := nil;
    WriteLn('OK!');

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

    Lease := Pool.Acquire;
    RS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id <= 10');
    try
      if (not RS.Next) or (RS.AsInteger(0) <> 50) then
      begin
        WriteLn('FALHOU: Esperado 50 registros');
        Halt(1);
      end;
    finally
      RS.Close;
    end;
    Lease := nil;
    WriteLn('   -> 10 threads concorrentes finalizaram com 100% de sucesso! OK!');

    Write('Teste 6: Esgotamento do pool e Timeout... ');
    TimeoutCaught := False;
    L1 := Pool.Acquire;
    L2 := Pool.Acquire;
    L3 := Pool.Acquire;
    L4 := Pool.Acquire;

    try
      Pool.Acquire(300);
    except
      on E: EDeltaPoolTimeoutException do
        TimeoutCaught := True;
    end;

    L1 := nil; L2 := nil; L3 := nil; L4 := nil;
    if not TimeoutCaught then
    begin
      WriteLn('FALHOU: Esperava EDeltaPoolTimeoutException!');
      Halt(1);
    end;
    WriteLn('OK!');

    Write('Teste 7: Callbacks Pool.Execute e Pool.InTransaction... ');
    Pool.Execute(@LogActionCallback);
    Pool.InTransaction(@LogTxCallback);

    Lease := Pool.Acquire;
    RS := Lease.ExecuteQuery('SELECT COUNT(*) FROM test_log WHERE thread_id = 888');
    try
      if (not RS.Next) or (RS.AsInteger(0) <> 2) then
      begin
        WriteLn('FALHOU: Esperado 2 registros para thread_id=888');
        Halt(1);
      end;
    finally
      RS.Close;
    end;
    Lease := nil;
    WriteLn('OK!');

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
  WriteLn('   TODOS OS TESTES FORAM CONCLUIDOS COM SUCESSO!    ');
  WriteLn('====================================================');
end;

begin
  try
    RunTests;
  except
    on E: Exception do
    begin
      WriteLn('ERRO FATAL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
