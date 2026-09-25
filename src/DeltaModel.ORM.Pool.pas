unit DeltaModel.ORM.Pool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, DB, SQLDB, Variants,
  DeltaModel, DatabaseURLParser, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Types, DeltaModel.ORM.Connection, DeltaModel.ORM.DML;

type
  { Exceções do Pool }
  EDeltaPoolException = class(Exception);
  EDeltaPoolTimeoutException = class(EDeltaPoolException);
  EDeltaPoolExhaustedException = class(EDeltaPoolException);

  { Estatísticas e Métricas do Pool }
  TDeltaPoolStats = record
    TotalAcquisitions: Int64;
    TotalReleases: Int64;
    TotalTimeouts: Int64;
    ConnectionsCreated: Int64;
    ConnectionsDestroyed: Int64;
    PeakActiveConnections: Integer;
  end;

  { Configuração do Pool }
  TDeltaPoolConfig = record
    DatabaseURL: string;
    MinConnections: Integer;        // Conexões pré-aquecidas mantidas ativas (default: 2)
    MaxConnections: Integer;        // Limite máximo de conexões simultâneas (default: 10)
    AcquireTimeoutMs: Integer;      // Tempo limite de espera para obter conexão (default: 5000 ms)
    IdleTimeoutSeconds: Integer;    // Tempo máximo de ociosidade para conexões extras (default: 300 s)
    MaxLifetimeSeconds: Integer;    // Tempo máximo de vida de uma conexão (default: 1800 s = 30 min, 0 = sem limite)
    HousekeeperIntervalSec: Integer;// Intervalo do limpador automático em background (default: 30 s, 0 = desativado)
    TestOnBorrow: Boolean;          // Testa conexão antes de entregar (default: True)
    ValidationQuery: string;        // Query de teste (vazio = automático por dialeto)
  end;

  TDeltaConnectionPool = class;

  { TDeltaPoolHousekeeper - Thread de manutenção em segundo plano }
  TDeltaPoolHousekeeper = class(TThread)
  private
    FPool: TDeltaConnectionPool;
    FIntervalMs: Integer;
    FWakeupEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APool: TDeltaConnectionPool; AIntervalSec: Integer);
    destructor Destroy; override;
    procedure Stop;
  end;

  { IDeltaPooledEngine - Interface com liberação automática (RAII) }
  IDeltaPooledEngine = interface(IDeltaORMEngine)
    ['{3A5C9B21-789E-4B0D-9E33-1F4F9C2B81A0}']
    function Engine: TDeltaORMEngine;
    procedure ReleaseToPool;
    procedure Disconnect;

    // Métodos DML/ORM diretos
    function Insert(AModel: TDeltaModel): Boolean;
    function InsertReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Merge(AModel: TDeltaModel): Boolean;
    function MergeReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Save(AModel: TDeltaModel): Boolean;
    function Delete(AModel: TDeltaModel): Boolean;
    function DeleteById(AModelClass: TDeltaModelClass; const AId: Variant): Boolean;
    function BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload;
    function BulkInsert(AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload;
    function Find(AModelClass: TDeltaModelClass; const AId: Variant): TDeltaModel;
    function Count(AModelClass: TDeltaModelClass; const AWhere: string = ''): Int64;
    function Query(AModelClass: TDeltaModelClass): TQuery;
    function NewQuery: TQuery;
  end;

  { Tipos de Callback para Execução Segura }
  TDeltaPoolAction = procedure(AEngine: TDeltaORMEngine) of object;
  TDeltaPoolStaticAction = procedure(AEngine: TDeltaORMEngine);

  { Item interno do Pool }
  TDeltaPoolItem = class
  public
    Engine: TDeltaORMEngine;
    InUse: Boolean;
    CreatedAt: TDateTime;
    LastUsedAt: TDateTime;
    destructor Destroy; override;
  end;

  { TDeltaPooledEngine - Implementação do wrapper RAII }
  TDeltaPooledEngine = class(TInterfacedObject, IDeltaPooledEngine, IDeltaORMEngine)
  private
    FPool: TDeltaConnectionPool;
    FEngine: TDeltaORMEngine;
    FReleased: Boolean;
    procedure CheckReleased;
  public
    constructor Create(APool: TDeltaConnectionPool; AEngine: TDeltaORMEngine);
    destructor Destroy; override;

    function Engine: TDeltaORMEngine;
    procedure ReleaseToPool;
    procedure Disconnect;

    // IDeltaORMEngine delegates
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function TransactionActive: Boolean;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload;
    function Connection: TSQLConnector;
    function Dialect: TDatabaseDialect;
    function NewDataset: TSQLQuery;
    function ExecuteQuery(const ASQL: string): TDataSet;
    procedure ExecuteDirect(const ASQL: string);
    function ExecuteScalar(const ASQL: string): Variant;

    // ORM Helpers delegates
    function Insert(AModel: TDeltaModel): Boolean;
    function InsertReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Merge(AModel: TDeltaModel): Boolean;
    function MergeReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Save(AModel: TDeltaModel): Boolean;
    function Delete(AModel: TDeltaModel): Boolean;
    function DeleteById(AModelClass: TDeltaModelClass; const AId: Variant): Boolean;
    function BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload;
    function BulkInsert(AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload;
    function Find(AModelClass: TDeltaModelClass; const AId: Variant): TDeltaModel;
    function Count(AModelClass: TDeltaModelClass; const AWhere: string = ''): Int64;
    function Query(AModelClass: TDeltaModelClass): TQuery;
    function NewQuery: TQuery;
  end;

  { TDeltaConnectionPool }
  TDeltaConnectionPool = class
  private
    FLock: TCriticalSection;
    FWaitEvent: TEvent;
    FItems: TFPList;
    FActiveLeases: TFPList;
    FConfig: TDeltaPoolConfig;
    FStats: TDeltaPoolStats;
    FHousekeeper: TDeltaPoolHousekeeper;
    FTerminated: Boolean;

    class var FDefaultPool: TDeltaConnectionPool;
    class var FDefaultPoolLock: TCriticalSection;

    function FindIdleItem: TDeltaPoolItem;
    function CreateNewItem: TDeltaPoolItem;
    procedure RecreateConnection(AItem: TDeltaPoolItem);
    procedure ValidateOrReconnect(AItem: TDeltaPoolItem);
    procedure SanitizeConnection(AEngine: TDeltaORMEngine);
    procedure Init(const AConfig: TDeltaPoolConfig);
    procedure RegisterLease(ALease: TDeltaPooledEngine);
    procedure UnregisterLease(ALease: TDeltaPooledEngine);
  public
    constructor Create(const ADatabaseURL: string); overload;
    constructor Create(const ADatabaseURL: string; AMinConnections, AMaxConnections: Integer); overload;
    constructor Create(const AConfig: TDeltaPoolConfig); overload;
    destructor Destroy; override;

    // Obtenção e devolução de conexões
    function Acquire(ATimeoutMs: Integer = -1): IDeltaPooledEngine;
    function AcquireRaw(ATimeoutMs: Integer = -1): TDeltaORMEngine;
    procedure Release(AEngine: TDeltaORMEngine);

    // Operações ORM de alta performance
    function BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload;
    function BulkInsert(AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload;

    // Execução encapsulada
    procedure Execute(AAction: TDeltaPoolAction); overload;
    procedure Execute(AAction: TDeltaPoolStaticAction); overload;
    procedure InTransaction(AAction: TDeltaPoolAction); overload;
    procedure InTransaction(AAction: TDeltaPoolStaticAction); overload;

    // Manutenção e métricas
    procedure WarmUp;
    procedure CleanUpIdle;
    procedure Clear;

    function ActiveCount: Integer;
    function IdleCount: Integer;
    function TotalCount: Integer;
    function Stats: TDeltaPoolStats;
    procedure ResetStats;

    property Config: TDeltaPoolConfig read FConfig;
    property Terminated: Boolean read FTerminated;

    // Singleton / Pool padrão
    class function DefaultPool: TDeltaConnectionPool;
    class procedure SetDefaultPool(APool: TDeltaConnectionPool);
  end;

function DefaultPoolConfig(const ADatabaseURL: string = ''): TDeltaPoolConfig;
function PoolConfigFromURL(const ADatabaseURL: string): TDeltaPoolConfig;

implementation

function DefaultPoolConfig(const ADatabaseURL: string): TDeltaPoolConfig;
begin
  Result.DatabaseURL := ADatabaseURL;
  Result.MinConnections := 2;
  Result.MaxConnections := 10;
  Result.AcquireTimeoutMs := 5000;
  Result.IdleTimeoutSeconds := 300;
  Result.MaxLifetimeSeconds := 1800; // 30 minutos
  Result.HousekeeperIntervalSec := 30; // varredura a cada 30s
  Result.TestOnBorrow := True;
  Result.ValidationQuery := '';
end;

function PoolConfigFromURL(const ADatabaseURL: string): TDeltaPoolConfig;
var
  DbConfig: TDatabaseConfig;
  V: string;
begin
  Result := DefaultPoolConfig(ADatabaseURL);
  DbConfig := ParseDatabaseURL(ADatabaseURL);
  try
    if Assigned(DbConfig.Params) then
    begin
      V := DbConfig.Params.Values['pool_min'];
      if V.IsEmpty then V := DbConfig.Params.Values['min_pool'];
      if not V.IsEmpty then Result.MinConnections := StrToIntDef(V, Result.MinConnections);

      V := DbConfig.Params.Values['pool_max'];
      if V.IsEmpty then V := DbConfig.Params.Values['max_pool'];
      if not V.IsEmpty then Result.MaxConnections := StrToIntDef(V, Result.MaxConnections);

      V := DbConfig.Params.Values['pool_timeout'];
      if V.IsEmpty then V := DbConfig.Params.Values['timeout'];
      if not V.IsEmpty then Result.AcquireTimeoutMs := StrToIntDef(V, Result.AcquireTimeoutMs);

      V := DbConfig.Params.Values['pool_idle'];
      if not V.IsEmpty then Result.IdleTimeoutSeconds := StrToIntDef(V, Result.IdleTimeoutSeconds);

      V := DbConfig.Params.Values['pool_max_lifetime'];
      if V.IsEmpty then V := DbConfig.Params.Values['max_lifetime'];
      if not V.IsEmpty then Result.MaxLifetimeSeconds := StrToIntDef(V, Result.MaxLifetimeSeconds);

      V := DbConfig.Params.Values['pool_housekeeper'];
      if V.IsEmpty then V := DbConfig.Params.Values['housekeeper_interval'];
      if not V.IsEmpty then Result.HousekeeperIntervalSec := StrToIntDef(V, Result.HousekeeperIntervalSec);

      V := DbConfig.Params.Values['pool_test'];
      if not V.IsEmpty then Result.TestOnBorrow := StrToBoolDef(V, Result.TestOnBorrow);

      V := DbConfig.Params.Values['validation_query'];
      if not V.IsEmpty then Result.ValidationQuery := V;
    end;
  finally
    DbConfig.Params.Free;
  end;
end;

{ TDeltaPoolHousekeeper }

constructor TDeltaPoolHousekeeper.Create(APool: TDeltaConnectionPool; AIntervalSec: Integer);
begin
  inherited Create(True);
  FPool := APool;
  FIntervalMs := AIntervalSec * 1000;
  if FIntervalMs <= 0 then
    FIntervalMs := 30000;
  FWakeupEvent := TEvent.Create(nil, False, False, '');
  FreeOnTerminate := False;
  Start;
end;

destructor TDeltaPoolHousekeeper.Destroy;
begin
  Stop;
  FWakeupEvent.Free;
  inherited Destroy;
end;

procedure TDeltaPoolHousekeeper.Stop;
begin
  Terminate;
  if Assigned(FWakeupEvent) then
    FWakeupEvent.SetEvent;
  WaitFor;
end;

procedure TDeltaPoolHousekeeper.Execute;
var
  WaitRes: TWaitResult;
begin
  while not Terminated do
  begin
    WaitRes := FWakeupEvent.WaitFor(Cardinal(FIntervalMs));
    if Terminated then
      Break;
    if WaitRes = wrTimeout then
    begin
      try
        if Assigned(FPool) and (not FPool.Terminated) then
          FPool.CleanUpIdle;
      except
      end;
    end;
  end;
end;

{ TDeltaPoolItem }

destructor TDeltaPoolItem.Destroy;
begin
  if Assigned(Engine) then
  begin
    try
      Engine.Disconnect;
    except
    end;
    FreeAndNil(Engine);
  end;
  inherited Destroy;
end;

{ TDeltaPooledEngine }

constructor TDeltaPooledEngine.Create(APool: TDeltaConnectionPool; AEngine: TDeltaORMEngine);
begin
  inherited Create;
  FPool := APool;
  FEngine := AEngine;
  FReleased := False;
  if Assigned(FPool) then
    FPool.RegisterLease(Self);
end;

destructor TDeltaPooledEngine.Destroy;
begin
  ReleaseToPool;
  inherited Destroy;
end;

procedure TDeltaPooledEngine.CheckReleased;
begin
  if FReleased or (not Assigned(FEngine)) then
    raise EDeltaPoolException.Create('Pooled connection has already been released to the pool.');
end;

function TDeltaPooledEngine.Engine: TDeltaORMEngine;
begin
  CheckReleased;
  Result := FEngine;
end;

procedure TDeltaPooledEngine.ReleaseToPool;
var
  PoolRef: TDeltaConnectionPool;
  EngRef: TDeltaORMEngine;
begin
  if not FReleased then
  begin
    FReleased := True;
    PoolRef := FPool;
    EngRef := FEngine;
    FEngine := nil;
    FPool := nil;
    if Assigned(PoolRef) then
    begin
      PoolRef.UnregisterLease(Self);
      if Assigned(EngRef) then
        PoolRef.Release(EngRef);
    end;
  end;
end;

procedure TDeltaPooledEngine.Disconnect;
begin
  ReleaseToPool;
end;

procedure TDeltaPooledEngine.StartTransaction;
begin
  CheckReleased;
  FEngine.StartTransaction;
end;

procedure TDeltaPooledEngine.Commit;
begin
  CheckReleased;
  FEngine.Commit;
end;

procedure TDeltaPooledEngine.Rollback;
begin
  CheckReleased;
  FEngine.Rollback;
end;

function TDeltaPooledEngine.TransactionActive: Boolean;
begin
  CheckReleased;
  Result := FEngine.TransactionActive;
end;

procedure TDeltaPooledEngine.InTransaction(AProc: TDeltaTransactionProc);
begin
  CheckReleased;
  FEngine.InTransaction(AProc);
end;

procedure TDeltaPooledEngine.InTransaction(AProc: TDeltaTransactionStaticProc);
begin
  CheckReleased;
  FEngine.InTransaction(AProc);
end;

function TDeltaPooledEngine.Connection: TSQLConnector;
begin
  CheckReleased;
  Result := FEngine.Connection;
end;

function TDeltaPooledEngine.Dialect: TDatabaseDialect;
begin
  CheckReleased;
  Result := FEngine.Dialect;
end;

function TDeltaPooledEngine.NewDataset: TSQLQuery;
begin
  CheckReleased;
  Result := FEngine.NewDataset;
end;

function TDeltaPooledEngine.ExecuteQuery(const ASQL: string): TDataSet;
begin
  CheckReleased;
  Result := FEngine.ExecuteQuery(ASQL);
end;

procedure TDeltaPooledEngine.ExecuteDirect(const ASQL: string);
begin
  CheckReleased;
  FEngine.ExecuteDirect(ASQL);
end;

function TDeltaPooledEngine.ExecuteScalar(const ASQL: string): Variant;
begin
  CheckReleased;
  Result := FEngine.ExecuteScalar(ASQL);
end;

function TDeltaPooledEngine.Insert(AModel: TDeltaModel): Boolean;
begin
  CheckReleased;
  Result := FEngine.Insert(AModel);
end;

function TDeltaPooledEngine.InsertReturning(AModel: TDeltaModel;
  AClass: TDeltaModelClass): TDeltaModel;
begin
  CheckReleased;
  Result := FEngine.InsertReturning(AModel, AClass);
end;

function TDeltaPooledEngine.Merge(AModel: TDeltaModel): Boolean;
begin
  CheckReleased;
  Result := FEngine.Merge(AModel);
end;

function TDeltaPooledEngine.MergeReturning(AModel: TDeltaModel;
  AClass: TDeltaModelClass): TDeltaModel;
begin
  CheckReleased;
  Result := FEngine.MergeReturning(AModel, AClass);
end;

function TDeltaPooledEngine.Save(AModel: TDeltaModel): Boolean;
begin
  CheckReleased;
  Result := FEngine.Save(AModel);
end;

function TDeltaPooledEngine.Delete(AModel: TDeltaModel): Boolean;
begin
  CheckReleased;
  Result := FEngine.Delete(AModel);
end;

function TDeltaPooledEngine.DeleteById(AModelClass: TDeltaModelClass;
  const AId: Variant): Boolean;
begin
  CheckReleased;
  Result := FEngine.DeleteById(AModelClass, AId);
end;

function TDeltaPooledEngine.BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer): Integer;
begin
  CheckReleased;
  Result := FEngine.BulkInsert(AModels, ABatchSize);
end;

function TDeltaPooledEngine.BulkInsert(AList: TDeltaModelList; ABatchSize: Integer): Integer;
begin
  CheckReleased;
  Result := FEngine.BulkInsert(AList, ABatchSize);
end;

function TDeltaPooledEngine.Find(AModelClass: TDeltaModelClass;
  const AId: Variant): TDeltaModel;
begin
  CheckReleased;
  Result := FEngine.Find(AModelClass, AId);
end;

function TDeltaPooledEngine.Count(AModelClass: TDeltaModelClass;
  const AWhere: string): Int64;
begin
  CheckReleased;
  Result := FEngine.Count(AModelClass, AWhere);
end;

function TDeltaPooledEngine.Query(AModelClass: TDeltaModelClass): TQuery;
begin
  CheckReleased;
  Result := FEngine.Query(AModelClass);
end;

function TDeltaPooledEngine.NewQuery: TQuery;
begin
  CheckReleased;
  Result := FEngine.NewQuery;
end;

{ TDeltaConnectionPool }

constructor TDeltaConnectionPool.Create(const ADatabaseURL: string);
begin
  inherited Create;
  Init(PoolConfigFromURL(ADatabaseURL));
end;

constructor TDeltaConnectionPool.Create(const ADatabaseURL: string;
  AMinConnections, AMaxConnections: Integer);
var
  Cfg: TDeltaPoolConfig;
begin
  inherited Create;
  Cfg := PoolConfigFromURL(ADatabaseURL);
  Cfg.MinConnections := AMinConnections;
  Cfg.MaxConnections := AMaxConnections;
  Init(Cfg);
end;

constructor TDeltaConnectionPool.Create(const AConfig: TDeltaPoolConfig);
begin
  inherited Create;
  Init(AConfig);
end;

procedure TDeltaConnectionPool.Init(const AConfig: TDeltaPoolConfig);
begin
  FConfig := AConfig;
  if FConfig.MinConnections < 0 then
    FConfig.MinConnections := 0;
  if FConfig.MaxConnections < 1 then
    FConfig.MaxConnections := 1;
  if FConfig.MinConnections > FConfig.MaxConnections then
    FConfig.MinConnections := FConfig.MaxConnections;
  if FConfig.AcquireTimeoutMs <= 0 then
    FConfig.AcquireTimeoutMs := 5000;
  if FConfig.IdleTimeoutSeconds <= 0 then
    FConfig.IdleTimeoutSeconds := 300;

  FillChar(FStats, SizeOf(FStats), 0);
  FLock := TCriticalSection.Create;
  FWaitEvent := TEvent.Create(nil, False, False, '');
  FItems := TFPList.Create;
  FActiveLeases := TFPList.Create;
  FTerminated := False;

  if FConfig.MinConnections > 0 then
    WarmUp;

  if FConfig.HousekeeperIntervalSec > 0 then
    FHousekeeper := TDeltaPoolHousekeeper.Create(Self, FConfig.HousekeeperIntervalSec)
  else
    FHousekeeper := nil;
end;

destructor TDeltaConnectionPool.Destroy;
begin
  if Assigned(FHousekeeper) then
  begin
    FHousekeeper.Stop;
    FreeAndNil(FHousekeeper);
  end;
  Clear;
  FActiveLeases.Free;
  FItems.Free;
  FWaitEvent.Free;
  FLock.Free;
  inherited Destroy;
end;

function TDeltaConnectionPool.FindIdleItem: TDeltaPoolItem;
var
  I: Integer;
  Item: TDeltaPoolItem;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    Item := TDeltaPoolItem(FItems[I]);
    if not Item.InUse then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

function TDeltaConnectionPool.CreateNewItem: TDeltaPoolItem;
var
  NewEngine: TDeltaORMEngine;
begin
  NewEngine := TDeltaORMEngine.Create(FConfig.DatabaseURL);
  try
    NewEngine.Connection.Open;
  except
    on E: Exception do
    begin
      NewEngine.Free;
      raise EDeltaPoolException.CreateFmt('Failed to establish database connection for pool: %s', [E.Message]);
    end;
  end;

  Result := TDeltaPoolItem.Create;
  Result.Engine := NewEngine;
  Result.InUse := False;
  Result.CreatedAt := Now;
  Result.LastUsedAt := Now;
  Inc(FStats.ConnectionsCreated);
end;

procedure TDeltaConnectionPool.RecreateConnection(AItem: TDeltaPoolItem);
var
  OldEngine, NewEngine: TDeltaORMEngine;
begin
  NewEngine := TDeltaORMEngine.Create(FConfig.DatabaseURL);
  try
    NewEngine.Connection.Open;
  except
    on E: Exception do
    begin
      NewEngine.Free;
      raise EDeltaPoolException.CreateFmt('Failed to recreate database connection: %s', [E.Message]);
    end;
  end;

  OldEngine := AItem.Engine;
  AItem.Engine := NewEngine;
  AItem.CreatedAt := Now;
  AItem.LastUsedAt := Now;
  Inc(FStats.ConnectionsCreated);
  Inc(FStats.ConnectionsDestroyed);

  if Assigned(OldEngine) then
  begin
    try
      OldEngine.Disconnect;
    except
    end;
    OldEngine.Free;
  end;
end;

procedure TDeltaConnectionPool.ValidateOrReconnect(AItem: TDeltaPoolItem);
var
  DS: TDataSet;
  Dialect: TDatabaseDialect;
  CheckSQL: string;
  LifeSec: Double;
begin
  if not Assigned(AItem.Engine) then
    Exit;

  // 1. Verifica tempo máximo de vida da conexão
  if (FConfig.MaxLifetimeSeconds > 0) then
  begin
    LifeSec := (Now - AItem.CreatedAt) * 86400.0;
    if LifeSec >= FConfig.MaxLifetimeSeconds then
    begin
      RecreateConnection(AItem);
      Exit;
    end;
  end;

  if not AItem.Engine.Connection.Connected then
  begin
    AItem.Engine.Connection.Open;
    Exit;
  end;

  if not FConfig.ValidationQuery.IsEmpty then
    CheckSQL := FConfig.ValidationQuery
  else
  begin
    Dialect := AItem.Engine.Dialect;
    case Dialect of
      ddOracle: CheckSQL := 'SELECT 1 FROM DUAL';
      ddFirebird: CheckSQL := 'SELECT 1 FROM RDB$DATABASE';
      else
        CheckSQL := 'SELECT 1';
    end;
  end;

  try
    DS := AItem.Engine.ExecuteQuery(CheckSQL);
    try
      // Conexão ativa e responsiva
    finally
      DS.Free;
    end;
    if AItem.Engine.TransactionActive then
      AItem.Engine.Commit;
  except
    // Falhou na consulta de validação, tenta reconectar uma vez
    try
      AItem.Engine.Disconnect;
      AItem.Engine.Connection.Open;
    except
      on E: Exception do
        raise EDeltaPoolException.CreateFmt('Database connection dropped and failed to reconnect: %s', [E.Message]);
    end;
  end;
end;

procedure TDeltaConnectionPool.SanitizeConnection(AEngine: TDeltaORMEngine);
begin
  if not Assigned(AEngine) then
    Exit;

  try
    if AEngine.TransactionActive then
      AEngine.Rollback;
  except
    try
      AEngine.Disconnect;
    except
    end;
  end;
end;

function TDeltaConnectionPool.Acquire(ATimeoutMs: Integer): IDeltaPooledEngine;
var
  Eng: TDeltaORMEngine;
begin
  Eng := AcquireRaw(ATimeoutMs);
  Result := TDeltaPooledEngine.Create(Self, Eng);
end;

function TDeltaConnectionPool.AcquireRaw(ATimeoutMs: Integer): TDeltaORMEngine;
var
  Item: TDeltaPoolItem;
  WaitLimitMs: Integer;
  StartTime, Elapsed: QWord;
  RemainingMs: Integer;
  WaitRes: TWaitResult;
begin
  if ATimeoutMs < 0 then
    WaitLimitMs := FConfig.AcquireTimeoutMs
  else
    WaitLimitMs := ATimeoutMs;

  StartTime := GetTickCount64;

  while True do
  begin
    Item := nil;

    FLock.Enter;
    try
      if FTerminated then
        raise EDeltaPoolException.Create('Connection pool is terminated.');

      // 1. Procura item ocioso
      Item := FindIdleItem;
      if Assigned(Item) then
      begin
        Item.InUse := True;
        Item.LastUsedAt := Now;
      end
      else if FItems.Count < FConfig.MaxConnections then
      begin
        // 2. Cria nova conexão dentro da capacidade máxima
        Item := CreateNewItem;
        Item.InUse := True;
        Item.LastUsedAt := Now;
        FItems.Add(Item);
      end;
    finally
      FLock.Leave;
    end;

    // Se obtivemos um item, validamos fora da seção crítica
    if Assigned(Item) then
    begin
      try
        if FConfig.TestOnBorrow then
          ValidateOrReconnect(Item);

        FLock.Enter;
        try
          Inc(FStats.TotalAcquisitions);
          if ActiveCount > FStats.PeakActiveConnections then
            FStats.PeakActiveConnections := ActiveCount;
        finally
          FLock.Leave;
        end;

        Result := Item.Engine;
        Exit;
      except
        // Se a validação falhar, liberamos e relançamos
        FLock.Enter;
        try
          Item.InUse := False;
        finally
          FLock.Leave;
        end;
        raise;
      end;
    end;

    // 3. Pool esgotado: aguarda notificação com timeout
    Elapsed := GetTickCount64 - StartTime;
    if Elapsed >= QWord(WaitLimitMs) then
    begin
      FLock.Enter;
      try
        Inc(FStats.TotalTimeouts);
      finally
        FLock.Leave;
      end;
      raise EDeltaPoolTimeoutException.CreateFmt(
        'Timeout waiting for available connection from pool (max: %d, waited: %d ms)',
        [FConfig.MaxConnections, Elapsed]);
    end;

    RemainingMs := WaitLimitMs - Integer(Elapsed);
    if RemainingMs <= 0 then
      RemainingMs := 1;

    WaitRes := FWaitEvent.WaitFor(Cardinal(RemainingMs));
    if WaitRes = wrTimeout then
    begin
      Elapsed := GetTickCount64 - StartTime;
      if Elapsed >= QWord(WaitLimitMs) then
      begin
        FLock.Enter;
        try
          Inc(FStats.TotalTimeouts);
        finally
          FLock.Leave;
        end;
        raise EDeltaPoolTimeoutException.CreateFmt(
          'Timeout waiting for available connection from pool (max: %d, waited: %d ms)',
          [FConfig.MaxConnections, Elapsed]);
      end;
    end;
  end;
end;

procedure TDeltaConnectionPool.Release(AEngine: TDeltaORMEngine);
var
  I: Integer;
  Item: TDeltaPoolItem;
begin
  if not Assigned(AEngine) then
    Exit;

  // 1. Sanitiza transações pendentes
  SanitizeConnection(AEngine);

  FLock.Enter;
  try
    Item := nil;
    for I := 0 to FItems.Count - 1 do
    begin
      if TDeltaPoolItem(FItems[I]).Engine = AEngine then
      begin
        Item := TDeltaPoolItem(FItems[I]);
        Break;
      end;
    end;

    if Assigned(Item) then
    begin
      Item.InUse := False;
      Item.LastUsedAt := Now;
      Inc(FStats.TotalReleases);
      // Notifica thread em espera
      FWaitEvent.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer): Integer;
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  Result := Lease.BulkInsert(AModels, ABatchSize);
end;

function TDeltaConnectionPool.BulkInsert(AList: TDeltaModelList; ABatchSize: Integer): Integer;
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  Result := Lease.BulkInsert(AList, ABatchSize);
end;

procedure TDeltaConnectionPool.Execute(AAction: TDeltaPoolAction);
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  AAction(Lease.Engine);
end;

procedure TDeltaConnectionPool.Execute(AAction: TDeltaPoolStaticAction);
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  AAction(Lease.Engine);
end;

procedure TDeltaConnectionPool.InTransaction(AAction: TDeltaPoolAction);
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  Lease.StartTransaction;
  try
    AAction(Lease.Engine);
    Lease.Commit;
  except
    Lease.Rollback;
    raise;
  end;
end;

procedure TDeltaConnectionPool.InTransaction(AAction: TDeltaPoolStaticAction);
var
  Lease: IDeltaPooledEngine;
begin
  Lease := Acquire;
  Lease.StartTransaction;
  try
    AAction(Lease.Engine);
    Lease.Commit;
  except
    Lease.Rollback;
    raise;
  end;
end;

procedure TDeltaConnectionPool.WarmUp;
var
  Item: TDeltaPoolItem;
begin
  FLock.Enter;
  try
    while (FItems.Count < FConfig.MinConnections) and (FItems.Count < FConfig.MaxConnections) do
    begin
      Item := CreateNewItem;
      FItems.Add(Item);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.CleanUpIdle;
var
  I: Integer;
  Item: TDeltaPoolItem;
  NowDT: TDateTime;
  IdleSec, LifeSec: Double;
begin
  FLock.Enter;
  try
    NowDT := Now;
    for I := FItems.Count - 1 downto 0 do
    begin
      Item := TDeltaPoolItem(FItems[I]);
      if not Item.InUse then
      begin
        IdleSec := (NowDT - Item.LastUsedAt) * 86400.0;
        LifeSec := (NowDT - Item.CreatedAt) * 86400.0;

        // Se excedeu MaxLifetime, recicla ou descarta
        if (FConfig.MaxLifetimeSeconds > 0) and (LifeSec >= FConfig.MaxLifetimeSeconds) then
        begin
          if FItems.Count <= FConfig.MinConnections then
            RecreateConnection(Item)
          else
          begin
            FItems.Delete(I);
            Inc(FStats.ConnectionsDestroyed);
            Item.Free;
          end;
        end
        else if (FItems.Count > FConfig.MinConnections) and (IdleSec >= FConfig.IdleTimeoutSeconds) then
        begin
          FItems.Delete(I);
          Inc(FStats.ConnectionsDestroyed);
          Item.Free;
        end;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.RegisterLease(ALease: TDeltaPooledEngine);
begin
  FLock.Enter;
  try
    if (not FTerminated) and Assigned(FActiveLeases) then
      FActiveLeases.Add(ALease);
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.UnregisterLease(ALease: TDeltaPooledEngine);
begin
  FLock.Enter;
  try
    if Assigned(FActiveLeases) then
      FActiveLeases.Remove(ALease);
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.Clear;
var
  I: Integer;
  Item: TDeltaPoolItem;
  LeaseObj: TDeltaPooledEngine;
begin
  FLock.Enter;
  try
    FTerminated := True;

    // Desconecta e invalida quaisquer leases ativos
    if Assigned(FActiveLeases) then
    begin
      for I := 0 to FActiveLeases.Count - 1 do
      begin
        LeaseObj := TDeltaPooledEngine(FActiveLeases[I]);
        LeaseObj.FPool := nil;
        LeaseObj.FEngine := nil;
        LeaseObj.FReleased := True;
      end;
      FActiveLeases.Clear;
    end;

    for I := 0 to FItems.Count - 1 do
    begin
      Item := TDeltaPoolItem(FItems[I]);
      Item.Free;
    end;
    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.ActiveCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  FLock.Enter;
  try
    for I := 0 to FItems.Count - 1 do
      if TDeltaPoolItem(FItems[I]).InUse then
        Inc(Result);
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.IdleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  FLock.Enter;
  try
    for I := 0 to FItems.Count - 1 do
      if not TDeltaPoolItem(FItems[I]).InUse then
        Inc(Result);
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.TotalCount: Integer;
begin
  FLock.Enter;
  try
    Result := FItems.Count;
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.Stats: TDeltaPoolStats;
begin
  FLock.Enter;
  try
    Result := FStats;
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.ResetStats;
begin
  FLock.Enter;
  try
    FillChar(FStats, SizeOf(FStats), 0);
  finally
    FLock.Leave;
  end;
end;

class function TDeltaConnectionPool.DefaultPool: TDeltaConnectionPool;
begin
  FDefaultPoolLock.Enter;
  try
    Result := FDefaultPool;
  finally
    FDefaultPoolLock.Leave;
  end;
end;

class procedure TDeltaConnectionPool.SetDefaultPool(APool: TDeltaConnectionPool);
begin
  FDefaultPoolLock.Enter;
  try
    FDefaultPool := APool;
  finally
    FDefaultPoolLock.Leave;
  end;
end;

initialization
  TDeltaConnectionPool.FDefaultPool := nil;
  TDeltaConnectionPool.FDefaultPoolLock := TCriticalSection.Create;

finalization
  if Assigned(TDeltaConnectionPool.FDefaultPool) then
    FreeAndNil(TDeltaConnectionPool.FDefaultPool);
  FreeAndNil(TDeltaConnectionPool.FDefaultPoolLock);

end.
