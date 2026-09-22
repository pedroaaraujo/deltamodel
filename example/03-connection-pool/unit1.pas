unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, DB, SQLDB,
  DeltaModel.ORM.Pool, DeltaModel.ORM.Connection;

type

  { TBaseBenchWorker }
  TBaseBenchWorker = class(TThread)
  protected
    FWorkerId: Integer;
    FOperations: Integer;
    FSuccess: Boolean;
    FErrorMessage: string;
  public
    constructor Create(AId, AOps: Integer);
    property Success: Boolean read FSuccess;
    property ErrorMessage: string read FErrorMessage;
  end;

  { TPooledWorker }
  TPooledWorker = class(TBaseBenchWorker)
  private
    FPool: TDeltaConnectionPool;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TDeltaConnectionPool; AId, AOps: Integer);
  end;

  { TUnpooledWorker }
  TUnpooledWorker = class(TBaseBenchWorker)
  private
    FDbURL: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const ADbURL: string; AId, AOps: Integer);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnClear: TButton;
    btnCompare: TButton;
    btnBenchNoPool: TButton;
    btnBenchWithPool: TButton;
    edtURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mmoLog: TMemo;
    pnlTop: TPanel;
    speMaxPool: TSpinEdit;
    speMinPool: TSpinEdit;
    speOps: TSpinEdit;
    speThreads: TSpinEdit;
    procedure btnClearClick(Sender: TObject);
    procedure btnBenchWithPoolClick(Sender: TObject);
    procedure btnBenchNoPoolClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
  private
    procedure PrepareDatabase;
    function RunPooledBenchmark: QWord;
    function RunUnpooledBenchmark: QWord;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TBaseBenchWorker }

constructor TBaseBenchWorker.Create(AId, AOps: Integer);
begin
  inherited Create(True);
  FWorkerId := AId;
  FOperations := AOps;
  FSuccess := False;
  FErrorMessage := '';
  FreeOnTerminate := False;
end;

{ TPooledWorker }

constructor TPooledWorker.Create(APool: TDeltaConnectionPool; AId, AOps: Integer);
begin
  inherited Create(AId, AOps);
  FPool := APool;
end;

procedure TPooledWorker.Execute;
var
  Lease: IDeltaPooledEngine;
  DS: TDataSet;
  I: Integer;
begin
  try
    for I := 1 to FOperations do
    begin
      Lease := FPool.Acquire;
      try
        Lease.ExecuteDirect('PRAGMA busy_timeout = 5000');
        Lease.StartTransaction;
        Lease.ExecuteDirect(Format(
          'INSERT INTO bench_log (worker_id, iter, val) VALUES (%d, %d, %d)',
          [FWorkerId, I, Random(10000)]));
        Lease.Commit;

        DS := Lease.ExecuteQuery(Format(
          'SELECT COUNT(*) FROM bench_log WHERE worker_id = %d',
          [FWorkerId]));
        try
          if (not DS.IsEmpty) and (DS.Fields[0].AsInteger < I) then
            raise Exception.Create('Contagem divergente.');
        finally
          DS.Free;
        end;
      finally
        Lease := nil;
      end;
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

{ TUnpooledWorker }

constructor TUnpooledWorker.Create(const ADbURL: string; AId, AOps: Integer);
begin
  inherited Create(AId, AOps);
  FDbURL := ADbURL;
end;

procedure TUnpooledWorker.Execute;
var
  Engine: TDeltaORMEngine;
  DS: TDataSet;
  I: Integer;
begin
  try
    for I := 1 to FOperations do
    begin
      // Cria, conecta, opera, desconecta e destroi toda vez (sem pool)
      Engine := TDeltaORMEngine.Create(FDbURL);
      try
        Engine.Connection.Open;
        Engine.ExecuteDirect('PRAGMA busy_timeout = 5000');
        Engine.StartTransaction;
        Engine.ExecuteDirect(Format(
          'INSERT INTO bench_log (worker_id, iter, val) VALUES (%d, %d, %d)',
          [FWorkerId, I, Random(10000)]));
        Engine.Commit;

        DS := Engine.ExecuteQuery(Format(
          'SELECT COUNT(*) FROM bench_log WHERE worker_id = %d',
          [FWorkerId]));
        try
          if (not DS.IsEmpty) and (DS.Fields[0].AsInteger < I) then
            raise Exception.Create('Contagem divergente.');
        finally
          DS.Free;
        end;
      finally
        Engine.Free;
      end;
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

{ TForm1 }

procedure TForm1.PrepareDatabase;
var
  Eng: TDeltaORMEngine;
begin
  Eng := TDeltaORMEngine.Create(edtURL.Text);
  try
    Eng.Connection.Open;
    try
      Eng.ExecuteDirect('PRAGMA busy_timeout = 5000');
      Eng.ExecuteDirect('CREATE TABLE IF NOT EXISTS bench_log (id INTEGER PRIMARY KEY AUTOINCREMENT, worker_id INTEGER, iter INTEGER, val INTEGER)');
      Eng.StartTransaction;
      Eng.ExecuteDirect('DELETE FROM bench_log');
      Eng.Commit;
    except
    end;
  finally
    Eng.Free;
  end;
end;

function TForm1.RunPooledBenchmark: QWord;
var
  Pool: TDeltaConnectionPool;
  Cfg: TDeltaPoolConfig;
  Workers: array of TPooledWorker;
  ThreadCount, OpsCount, I: Integer;
  StartTime: QWord;
  Stats: TDeltaPoolStats;
begin
  ThreadCount := speThreads.Value;
  OpsCount := speOps.Value;

  PrepareDatabase;

  Cfg := DefaultPoolConfig(edtURL.Text);
  Cfg.MinConnections := speMinPool.Value;
  Cfg.MaxConnections := speMaxPool.Value;
  Cfg.AcquireTimeoutMs := 10000;
  Cfg.HousekeeperIntervalSec := 10;

  mmoLog.Lines.Add(Format('[POOL] Iniciando Pool com Min: %d, Max: %d...', [Cfg.MinConnections, Cfg.MaxConnections]));
  Pool := TDeltaConnectionPool.Create(Cfg);
  try
    SetLength(Workers, ThreadCount);
    for I := 0 to ThreadCount - 1 do
      Workers[I] := TPooledWorker.Create(Pool, I + 1, OpsCount);

    StartTime := GetTickCount64;

    for I := 0 to ThreadCount - 1 do
      Workers[I].Start;

    for I := 0 to ThreadCount - 1 do
    begin
      Workers[I].WaitFor;
      if not Workers[I].Success then
        mmoLog.Lines.Add(Format('   [ERRO] Worker %d: %s', [I + 1, Workers[I].ErrorMessage]));
      Workers[I].Free;
    end;

    Result := GetTickCount64 - StartTime;

    Stats := Pool.Stats;
    mmoLog.Lines.Add(Format('[POOL] Tempo total: %d ms | Total Ops: %d | Throughput: %.1f ops/s',
      [Result, ThreadCount * OpsCount, (ThreadCount * OpsCount) / (Result / 1000.0)]));
    mmoLog.Lines.Add(Format('       Métricas: Aquisições: %d, Devoluções: %d, Conexões criadas: %d, Pico ativo: %d',
      [Stats.TotalAcquisitions, Stats.TotalReleases, Stats.ConnectionsCreated, Stats.PeakActiveConnections]));
  finally
    Pool.Free;
  end;
end;

function TForm1.RunUnpooledBenchmark: QWord;
var
  Workers: array of TUnpooledWorker;
  ThreadCount, OpsCount, I: Integer;
  StartTime: QWord;
begin
  ThreadCount := speThreads.Value;
  OpsCount := speOps.Value;

  PrepareDatabase;

  mmoLog.Lines.Add('[SEM POOL] Iniciando benchmark abrindo nova conexão a cada iteração...');
  SetLength(Workers, ThreadCount);
  for I := 0 to ThreadCount - 1 do
    Workers[I] := TUnpooledWorker.Create(edtURL.Text, I + 1, OpsCount);

  StartTime := GetTickCount64;

  for I := 0 to ThreadCount - 1 do
    Workers[I].Start;

  for I := 0 to ThreadCount - 1 do
  begin
    Workers[I].WaitFor;
    if not Workers[I].Success then
      mmoLog.Lines.Add(Format('   [ERRO] Worker %d: %s', [I + 1, Workers[I].ErrorMessage]));
    Workers[I].Free;
  end;

  Result := GetTickCount64 - StartTime;

  mmoLog.Lines.Add(Format('[SEM POOL] Tempo total: %d ms | Total Ops: %d | Throughput: %.1f ops/s',
    [Result, ThreadCount * OpsCount, (ThreadCount * OpsCount) / (Result / 1000.0)]));
end;

procedure TForm1.btnBenchWithPoolClick(Sender: TObject);
begin
  btnBenchWithPool.Enabled := False;
  try
    RunPooledBenchmark;
  finally
    btnBenchWithPool.Enabled := True;
  end;
end;

procedure TForm1.btnBenchNoPoolClick(Sender: TObject);
begin
  btnBenchNoPool.Enabled := False;
  try
    RunUnpooledBenchmark;
  finally
    btnBenchNoPool.Enabled := True;
  end;
end;

procedure TForm1.btnCompareClick(Sender: TObject);
var
  TimeNoPool, TimePool: QWord;
  Speedup: Double;
begin
  btnCompare.Enabled := False;
  try
    mmoLog.Lines.Add('===========================================================');
    mmoLog.Lines.Add('               BENCHMARK COMPARATIVO DE PERFORMANCE        ');
    mmoLog.Lines.Add('===========================================================');

    TimePool := RunPooledBenchmark;
    mmoLog.Lines.Add('-----------------------------------------------------------');
    TimeNoPool := RunUnpooledBenchmark;
    mmoLog.Lines.Add('-----------------------------------------------------------');

    if TimePool > 0 then
    begin
      Speedup := TimeNoPool / TimePool;
      mmoLog.Lines.Add(Format('>>> RESULTADO: O Pool de Conexões foi %.2fx MAIS RÁPIDO!', [Speedup]));
      mmoLog.Lines.Add(Format('    Tempo economizado: %d ms (%.1f%% de redução na latência)',
        [TimeNoPool - TimePool, ((TimeNoPool - TimePool) / TimeNoPool) * 100.0]));
    end;
    mmoLog.Lines.Add('===========================================================');
  finally
    btnCompare.Enabled := True;
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

end.
