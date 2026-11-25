unit DeltaModel.ORM.Pool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Variants,
  DatabaseURLParser, DeltaModel, DeltaModel.ORM.Types,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry, DeltaModel.ORM.Persister;

type
  EDeltaPoolException = class(Exception);
  EDeltaPoolTimeoutException = class(EDeltaPoolException);
  EDeltaPoolExhaustedException = class(EDeltaPoolException);

  TDeltaPoolStats = record
    TotalAcquisitions: Int64;
    TotalReleases: Int64;
    TotalTimeouts: Int64;
    ConnectionsCreated: Int64;
    ConnectionsDestroyed: Int64;
    PeakActiveConnections: Integer;
  end;

  TDeltaPoolConfig = record
    DatabaseURL: string;
    DriverName: string;
    MinConnections: Integer;
    MaxConnections: Integer;
    AcquireTimeoutMs: Integer;
    IdleTimeoutSeconds: Integer;
    MaxLifetimeSeconds: Integer;
    HousekeeperIntervalSec: Integer;
    TestOnBorrow: Boolean;
    ValidationQuery: string;
  end;

  TDeltaPoolAction = procedure(const AConn: IDeltaConnection) of object;
  TDeltaPoolStaticAction = procedure(const AConn: IDeltaConnection);
  TDeltaPoolConnectionFactory = function(const AConfig: TDeltaPoolConfig): IDeltaConnection;

  TDeltaConnectionPool = class;

  { TDeltaPoolHousekeeper }
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

  { TDeltaPoolItem }
  TDeltaPoolItem = class
  public
    Connection: IDeltaConnection;
    InUse: Boolean;
    CreatedAt: TDateTime;
    LastUsedAt: TDateTime;
    destructor Destroy; override;
  end;

  { TDeltaPooledConnection: RAII IDeltaConnection lease returning to pool on release }
  TDeltaPooledConnection = class(TInterfacedObject, IDeltaConnection)
  private
    FPool: TDeltaConnectionPool;
    FConnection: IDeltaConnection;
    FReleased: Boolean;
    procedure CheckReleased;
  public
    constructor Create(APool: TDeltaConnectionPool;
      const AConnection: IDeltaConnection);
    destructor Destroy; override;
    procedure ReleaseToPool;

    function Dialect: IDeltaDialect;
    function DriverName: string;
    function Connected: Boolean;
    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function TransactionActive: Boolean;
    function BeginTransaction: IDeltaTransaction;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload;
    function CreateStatement(const ASQL: string = ''): IDeltaStatement;
    procedure ExecuteDirect(const ASQL: string);
    function ExecuteQuery(const ASQL: string): IDeltaResultSet;
    function ExecuteScalar(const ASQL: string): Variant;
    procedure GetTableNames(ATables: TStrings);
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
    FConnectionFactory: TDeltaPoolConnectionFactory;

    class var FDefaultPool: TDeltaConnectionPool;
    class var FDefaultPoolLock: TCriticalSection;

    function CreateUnderlyingConnection: IDeltaConnection;
    function FindIdleItem: TDeltaPoolItem;
    function CreateNewItem: TDeltaPoolItem;
    procedure RecreateConnection(AItem: TDeltaPoolItem);
    procedure ValidateOrReconnect(AItem: TDeltaPoolItem);
    procedure SanitizeConnection(const AConn: IDeltaConnection);
    procedure Init(const AConfig: TDeltaPoolConfig;
      AConnectionFactory: TDeltaPoolConnectionFactory);
    procedure RegisterLease(ALease: TDeltaPooledConnection);
    procedure UnregisterLease(ALease: TDeltaPooledConnection);
  public
    constructor Create(const ADatabaseURL: string;
      AConnectionFactory: TDeltaPoolConnectionFactory = nil); overload;
    constructor Create(const ADatabaseURL: string;
      AMinConnections, AMaxConnections: Integer;
      AConnectionFactory: TDeltaPoolConnectionFactory = nil); overload;
    constructor Create(const AConfig: TDeltaPoolConfig;
      AConnectionFactory: TDeltaPoolConnectionFactory = nil); overload;
    destructor Destroy; override;

    function Acquire(ATimeoutMs: Integer = -1): IDeltaConnection;
    function AcquireRaw(ATimeoutMs: Integer = -1): IDeltaConnection;
    procedure Release(const AConn: IDeltaConnection);

    function BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; overload;
    function BulkInsert(AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; overload;

    procedure Execute(AAction: TDeltaPoolAction); overload;
    procedure Execute(AAction: TDeltaPoolStaticAction); overload;
    procedure InTransaction(AAction: TDeltaPoolAction); overload;
    procedure InTransaction(AAction: TDeltaPoolStaticAction); overload;

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

    class function DefaultPool: TDeltaConnectionPool;
    class procedure SetDefaultPool(APool: TDeltaConnectionPool);
  end;

function DefaultPoolConfig(const ADatabaseURL: string = ''): TDeltaPoolConfig;
function PoolConfigFromURL(const ADatabaseURL: string): TDeltaPoolConfig;

implementation

function DefaultPoolConfig(const ADatabaseURL: string): TDeltaPoolConfig;
begin
  Result.DatabaseURL := ADatabaseURL;
  Result.DriverName := '';
  Result.MinConnections := 2;
  Result.MaxConnections := 10;
  Result.AcquireTimeoutMs := 5000;
  Result.IdleTimeoutSeconds := 300;
  Result.MaxLifetimeSeconds := 1800;
  Result.HousekeeperIntervalSec := 30;
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
      V := DbConfig.Params.Values['driver'];
      if not V.IsEmpty then
        Result.DriverName := V;

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

constructor TDeltaPoolHousekeeper.Create(APool: TDeltaConnectionPool;
  AIntervalSec: Integer);
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
  if Connection <> nil then
  begin
    try
      Connection.Disconnect;
    except
    end;
    Connection := nil;
  end;
  inherited Destroy;
end;

{ TDeltaPooledConnection }

constructor TDeltaPooledConnection.Create(APool: TDeltaConnectionPool;
  const AConnection: IDeltaConnection);
begin
  inherited Create;
  FPool := APool;
  FConnection := AConnection;
  FReleased := False;
  if Assigned(FPool) then
    FPool.RegisterLease(Self);
end;

destructor TDeltaPooledConnection.Destroy;
begin
  ReleaseToPool;
  inherited Destroy;
end;

procedure TDeltaPooledConnection.CheckReleased;
begin
  if FReleased or (FConnection = nil) then
    raise EDeltaPoolException.Create('Pooled connection has already been released to the pool.');
end;

procedure TDeltaPooledConnection.ReleaseToPool;
var
  PoolRef: TDeltaConnectionPool;
  ConnRef: IDeltaConnection;
begin
  if not FReleased then
  begin
    FReleased := True;
    PoolRef := FPool;
    ConnRef := FConnection;
    FConnection := nil;
    FPool := nil;
    if Assigned(PoolRef) then
    begin
      PoolRef.UnregisterLease(Self);
      if ConnRef <> nil then
        PoolRef.Release(ConnRef);
    end;
  end;
end;

function TDeltaPooledConnection.Dialect: IDeltaDialect;
begin
  CheckReleased;
  Result := FConnection.Dialect;
end;

function TDeltaPooledConnection.DriverName: string;
begin
  CheckReleased;
  Result := FConnection.DriverName;
end;

function TDeltaPooledConnection.Connected: Boolean;
begin
  CheckReleased;
  Result := FConnection.Connected;
end;

procedure TDeltaPooledConnection.Connect;
begin
  CheckReleased;
  FConnection.Connect;
end;

procedure TDeltaPooledConnection.Disconnect;
begin
  ReleaseToPool;
end;

procedure TDeltaPooledConnection.StartTransaction;
begin
  CheckReleased;
  FConnection.StartTransaction;
end;

procedure TDeltaPooledConnection.Commit;
begin
  CheckReleased;
  FConnection.Commit;
end;

procedure TDeltaPooledConnection.Rollback;
begin
  CheckReleased;
  FConnection.Rollback;
end;

function TDeltaPooledConnection.TransactionActive: Boolean;
begin
  CheckReleased;
  Result := FConnection.TransactionActive;
end;

function TDeltaPooledConnection.BeginTransaction: IDeltaTransaction;
begin
  CheckReleased;
  Result := FConnection.BeginTransaction;
end;

procedure TDeltaPooledConnection.InTransaction(AProc: TDeltaTransactionProc);
begin
  CheckReleased;
  FConnection.InTransaction(AProc);
end;

procedure TDeltaPooledConnection.InTransaction(AProc: TDeltaTransactionStaticProc);
begin
  CheckReleased;
  FConnection.InTransaction(AProc);
end;

function TDeltaPooledConnection.CreateStatement(const ASQL: string): IDeltaStatement;
begin
  CheckReleased;
  Result := FConnection.CreateStatement(ASQL);
end;

procedure TDeltaPooledConnection.ExecuteDirect(const ASQL: string);
begin
  CheckReleased;
  FConnection.ExecuteDirect(ASQL);
end;

function TDeltaPooledConnection.ExecuteQuery(const ASQL: string): IDeltaResultSet;
begin
  CheckReleased;
  Result := FConnection.ExecuteQuery(ASQL);
end;

function TDeltaPooledConnection.ExecuteScalar(const ASQL: string): Variant;
begin
  CheckReleased;
  Result := FConnection.ExecuteScalar(ASQL);
end;

procedure TDeltaPooledConnection.GetTableNames(ATables: TStrings);
begin
  CheckReleased;
  FConnection.GetTableNames(ATables);
end;

{ TDeltaConnectionPool }

constructor TDeltaConnectionPool.Create(const ADatabaseURL: string;
  AConnectionFactory: TDeltaPoolConnectionFactory);
begin
  inherited Create;
  Init(PoolConfigFromURL(ADatabaseURL), AConnectionFactory);
end;

constructor TDeltaConnectionPool.Create(const ADatabaseURL: string;
  AMinConnections, AMaxConnections: Integer;
  AConnectionFactory: TDeltaPoolConnectionFactory);
var
  Cfg: TDeltaPoolConfig;
begin
  inherited Create;
  Cfg := PoolConfigFromURL(ADatabaseURL);
  Cfg.MinConnections := AMinConnections;
  Cfg.MaxConnections := AMaxConnections;
  Init(Cfg, AConnectionFactory);
end;

constructor TDeltaConnectionPool.Create(const AConfig: TDeltaPoolConfig;
  AConnectionFactory: TDeltaPoolConnectionFactory);
begin
  inherited Create;
  Init(AConfig, AConnectionFactory);
end;

procedure TDeltaConnectionPool.Init(const AConfig: TDeltaPoolConfig;
  AConnectionFactory: TDeltaPoolConnectionFactory);
begin
  FConfig := AConfig;
  FConnectionFactory := AConnectionFactory;

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

function TDeltaConnectionPool.CreateUnderlyingConnection: IDeltaConnection;
begin
  if Assigned(FConnectionFactory) then
    Result := FConnectionFactory(FConfig)
  else
    Result := TDeltaORM.CreateConnection(FConfig.DatabaseURL, FConfig.DriverName);
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
      Exit(Item);
  end;
end;

function TDeltaConnectionPool.CreateNewItem: TDeltaPoolItem;
var
  NewConn: IDeltaConnection;
begin
  NewConn := CreateUnderlyingConnection;
  try
    NewConn.Connect;
  except
    on E: Exception do
    begin
      NewConn := nil;
      raise EDeltaPoolException.CreateFmt(
        'Failed to establish database connection for pool: %s', [E.Message]);
    end;
  end;

  Result := TDeltaPoolItem.Create;
  Result.Connection := NewConn;
  Result.InUse := False;
  Result.CreatedAt := Now;
  Result.LastUsedAt := Now;
  Inc(FStats.ConnectionsCreated);
end;

procedure TDeltaConnectionPool.RecreateConnection(AItem: TDeltaPoolItem);
var
  OldConn, NewConn: IDeltaConnection;
begin
  NewConn := CreateUnderlyingConnection;
  try
    NewConn.Connect;
  except
    on E: Exception do
    begin
      NewConn := nil;
      raise EDeltaPoolException.CreateFmt(
        'Failed to recreate database connection: %s', [E.Message]);
    end;
  end;

  OldConn := AItem.Connection;
  AItem.Connection := NewConn;
  AItem.CreatedAt := Now;
  AItem.LastUsedAt := Now;
  Inc(FStats.ConnectionsCreated);
  Inc(FStats.ConnectionsDestroyed);

  if OldConn <> nil then
  begin
    try
      OldConn.Disconnect;
    except
    end;
    OldConn := nil;
  end;
end;

procedure TDeltaConnectionPool.ValidateOrReconnect(AItem: TDeltaPoolItem);
var
  RS: IDeltaResultSet;
  CheckSQL: string;
  LifeSec: Double;
begin
  if FTerminated or (AItem = nil) or (AItem.Connection = nil) then
    Exit;

  try
    if FConfig.MaxLifetimeSeconds > 0 then
    begin
      LifeSec := (Now - AItem.CreatedAt) * 86400.0;
      if LifeSec >= FConfig.MaxLifetimeSeconds then
      begin
        try
          RecreateConnection(AItem);
        except
        end;
        Exit;
      end;
    end;

    if not AItem.Connection.Connected then
    begin
      AItem.Connection.Connect;
      Exit;
    end;

    if not FConfig.ValidationQuery.IsEmpty then
      CheckSQL := FConfig.ValidationQuery
    else
      CheckSQL := AItem.Connection.Dialect.SQL.HealthCheckSQL;

    try
      RS := AItem.Connection.ExecuteQuery(CheckSQL);
      RS.Close;
      if AItem.Connection.TransactionActive then
        AItem.Connection.Commit;
    except
      try
        AItem.Connection.Disconnect;
        AItem.Connection.Connect;
      except
        on E: Exception do
          raise EDeltaPoolException.CreateFmt(
            'Database connection dropped and failed to reconnect: %s', [E.Message]);
      end;
    end;
  except
  end;
end;

procedure TDeltaConnectionPool.SanitizeConnection(const AConn: IDeltaConnection);
begin
  if AConn = nil then
    Exit;

  try
    if AConn.TransactionActive then
      AConn.Rollback;
  except
    try
      AConn.Disconnect;
    except
    end;
  end;
end;

function TDeltaConnectionPool.Acquire(ATimeoutMs: Integer): IDeltaConnection;
var
  RawConn: IDeltaConnection;
begin
  RawConn := AcquireRaw(ATimeoutMs);
  Result := TDeltaPooledConnection.Create(Self, RawConn);
end;

function TDeltaConnectionPool.AcquireRaw(ATimeoutMs: Integer): IDeltaConnection;
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

      Item := FindIdleItem;
      if Assigned(Item) then
      begin
        Item.InUse := True;
        Item.LastUsedAt := Now;
      end
      else if FItems.Count < FConfig.MaxConnections then
      begin
        Item := CreateNewItem;
        Item.InUse := True;
        Item.LastUsedAt := Now;
        FItems.Add(Item);
      end;
    finally
      FLock.Leave;
    end;

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

        Result := Item.Connection;
        Exit;
      except
        FLock.Enter;
        try
          Item.InUse := False;
        finally
          FLock.Leave;
        end;
        raise;
      end;
    end;

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

procedure TDeltaConnectionPool.Release(const AConn: IDeltaConnection);
var
  I: Integer;
  Item: TDeltaPoolItem;
  TargetUnk, ItemUnk: IUnknown;
begin
  if AConn = nil then
    Exit;

  TargetUnk := AConn as IUnknown;

  FLock.Enter;
  try
    if FTerminated then
    begin
      for I := 0 to FItems.Count - 1 do
      begin
        if TDeltaPoolItem(FItems[I]).Connection <> nil then
        begin
          ItemUnk := TDeltaPoolItem(FItems[I]).Connection as IUnknown;
          if ItemUnk = TargetUnk then
          begin
            TDeltaPoolItem(FItems[I]).Connection := nil;
            Break;
          end;
        end;
      end;
      try
        AConn.Disconnect;
      except
      end;
      Exit;
    end;
  finally
    FLock.Leave;
  end;

  SanitizeConnection(AConn);

  FLock.Enter;
  try
    Item := nil;
    for I := 0 to FItems.Count - 1 do
    begin
      if TDeltaPoolItem(FItems[I]).Connection <> nil then
      begin
        ItemUnk := TDeltaPoolItem(FItems[I]).Connection as IUnknown;
        if ItemUnk = TargetUnk then
        begin
          Item := TDeltaPoolItem(FItems[I]);
          Break;
        end;
      end;
    end;

    if Assigned(Item) then
    begin
      Item.InUse := False;
      Item.LastUsedAt := Now;
      Inc(FStats.TotalReleases);
      FWaitEvent.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
end;

function TDeltaConnectionPool.BulkInsert(AModels: array of TDeltaModel;
  ABatchSize: Integer): Integer;
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  Result := TDeltaPersister.BulkInsert(Lease, AModels, ABatchSize);
end;

function TDeltaConnectionPool.BulkInsert(AList: TDeltaModelList;
  ABatchSize: Integer): Integer;
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  Result := TDeltaPersister.BulkInsert(Lease, AList, ABatchSize);
end;

procedure TDeltaConnectionPool.Execute(AAction: TDeltaPoolAction);
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  if Assigned(AAction) then
    AAction(Lease);
end;

procedure TDeltaConnectionPool.Execute(AAction: TDeltaPoolStaticAction);
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  if Assigned(AAction) then
    AAction(Lease);
end;

procedure TDeltaConnectionPool.InTransaction(AAction: TDeltaPoolAction);
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  Lease.StartTransaction;
  try
    if Assigned(AAction) then
      AAction(Lease);
    Lease.Commit;
  except
    Lease.Rollback;
    raise;
  end;
end;

procedure TDeltaConnectionPool.InTransaction(AAction: TDeltaPoolStaticAction);
var
  Lease: IDeltaConnection;
begin
  Lease := Acquire;
  Lease.StartTransaction;
  try
    if Assigned(AAction) then
      AAction(Lease);
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

procedure TDeltaConnectionPool.RegisterLease(ALease: TDeltaPooledConnection);
begin
  FLock.Enter;
  try
    if (not FTerminated) and Assigned(FActiveLeases) then
      FActiveLeases.Add(ALease);
  finally
    FLock.Leave;
  end;
end;

procedure TDeltaConnectionPool.UnregisterLease(ALease: TDeltaPooledConnection);
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
  LeaseObj: TDeltaPooledConnection;
begin
  FLock.Enter;
  try
    FTerminated := True;

    if Assigned(FActiveLeases) then
    begin
      for I := 0 to FActiveLeases.Count - 1 do
      begin
        LeaseObj := TDeltaPooledConnection(FActiveLeases[I]);
        LeaseObj.FPool := nil;
        LeaseObj.FConnection := nil;
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
