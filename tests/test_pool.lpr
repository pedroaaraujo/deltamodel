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
  { Thread trabalhadora para teste de concorrência }
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
      // 1. Adquire conexão do pool usando RAII (IDeltaPooledEngine)
      Lease := FPool.Acquire(5000);
      try
        // 2. Executa operações no banco compartilhado
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

  // TESTE 1: Parser de configuração na URL
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

  // TESTE 2: Criação e WarmUp do Pool
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

    // TESTE 3: RAII Empréstimo e devolução automática ao sair de escopo
    Write('Teste 3: Emprestimo e devolucao automatica RAII... ');
    Lease := Pool.Acquire;
    if Pool.ActiveCount <> 1 then
    begin
      WriteLn(Format('FALHOU: ActiveCount=%d (esperado 1)', [Pool.ActiveCount]));
      Halt(1);
    end;
    Lease := nil; // Libera
    if Pool.ActiveCount <> 0 then
    begin
      WriteLn(Format('FALHOU: ActiveCount apos release=%d (esperado 0)', [Pool.ActiveCount]));
      Halt(1);
    end;
    WriteLn('OK!');

    // TESTE 4: Sanitização de transação pendente na devolução
    Write('Teste 4: Sanitizacao de transacao pendente (Auto-Rollback)... ');
    RawEng := Pool.AcquireRaw;
    RawEng.StartTransaction;
    RawEng.ExecuteDirect('INSERT INTO test_log (thread_id, iteration, msg) VALUES (999, 1, ''nao comitado'')');
    // Devolve com transação ainda ativa!
    Pool.Release(RawEng);

    // Verifica se foi revertido
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

    // TESTE 5: Concorrência com múltiplas Threads simultâneas
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

    // Confere número total de registros gravados por todas as threads (10 threads * 5 inserts = 50)
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

    // TESTE 6: Esgotamento do pool e Timeout
    Write('Teste 6: Esgotamento do pool e Timeout... ');
    TimeoutCaught := False;
    L1 := Pool.Acquire;
    L2 := Pool.Acquire;
    L3 := Pool.Acquire;
    L4 := Pool.Acquire;

    try
      // Tenta pegar a 5ª conexão com timeout de 300ms
      Pool.Acquire(300);
    except
      on E: EDeltaPoolTimeoutException do
        TimeoutCaught := True;
    end;

    // Libera as 4 conexões
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

    // TESTE 7: Execução encapsulada Pool.Execute e Pool.InTransaction
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

    // TESTE 8: Métricas do Pool (Observabilidade)
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
