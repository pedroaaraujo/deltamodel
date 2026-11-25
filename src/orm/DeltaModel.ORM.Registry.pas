unit DeltaModel.ORM.Registry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  DatabaseURLParser, DeltaModel.ORM.Interfaces;

type
  TDeltaDriverEntry = record
    Name: string;
    Driver: IDeltaDriver;
  end;

  TDeltaDialectEntry = record
    Alias: string;
    Dialect: IDeltaDialect;
  end;

  { TDeltaORMRegistry: Thread-safe registry for pluggable drivers and dialects }
  TDeltaORMRegistry = class
  private
    class var FLock: TCriticalSection;
    class var FDrivers: array of TDeltaDriverEntry;
    class var FDialects: array of TDeltaDialectEntry;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterDriver(const ADriver: IDeltaDriver); static;
    class procedure RegisterDialect(const AAliases: array of string;
      const ADialect: IDeltaDialect); static;

    class function FindDriver(const ANameOrProtocol: string): IDeltaDriver; static;
    class function ResolveDriver(const ANameOrProtocol: string): IDeltaDriver; static;
    class function FindDialect(const ANameOrProtocol: string): IDeltaDialect; static;
    class function ResolveDialect(const ANameOrProtocol: string): IDeltaDialect; static;

    class procedure RegisteredDrivers(AList: TStrings); static;
    class procedure RegisteredDialects(AList: TStrings); static;
    class procedure Clear; static;
  end;

  { TDeltaORM: High-level connection factory facade }
  TDeltaORM = class
  public
    class function CreateConnection(const ADatabaseURL: string;
      const ADriverName: string = ''): IDeltaConnection; static;
    class function Connect(const ADatabaseURL: string;
      const ADriverName: string = ''): IDeltaConnection; static;
    class function Dialect(const ANameOrProtocol: string): IDeltaDialect; static;
  end;

implementation

{ TDeltaORMRegistry }

class constructor TDeltaORMRegistry.Create;
begin
  FLock := TCriticalSection.Create;
  SetLength(FDrivers, 0);
  SetLength(FDialects, 0);
end;

class destructor TDeltaORMRegistry.Destroy;
begin
  SetLength(FDrivers, 0);
  SetLength(FDialects, 0);
  FreeAndNil(FLock);
end;

class procedure TDeltaORMRegistry.RegisterDriver(const ADriver: IDeltaDriver);
var
  I: Integer;
  Key: string;
begin
  if ADriver = nil then
    Exit;
  Key := LowerCase(Trim(ADriver.Name));
  if Key.IsEmpty then
    raise Exception.Create('Cannot register a database driver with an empty name.');

  FLock.Enter;
  try
    for I := 0 to High(FDrivers) do
    begin
      if FDrivers[I].Name = Key then
      begin
        FDrivers[I].Driver := ADriver;
        Exit;
      end;
    end;
    SetLength(FDrivers, Length(FDrivers) + 1);
    FDrivers[High(FDrivers)].Name := Key;
    FDrivers[High(FDrivers)].Driver := ADriver;
  finally
    FLock.Leave;
  end;
end;

class procedure TDeltaORMRegistry.RegisterDialect(const AAliases: array of string;
  const ADialect: IDeltaDialect);
var
  I, J: Integer;
  Key: string;
  Found: Boolean;
begin
  if ADialect = nil then
    Exit;

  FLock.Enter;
  try
    for I := Low(AAliases) to High(AAliases) do
    begin
      Key := LowerCase(Trim(AAliases[I]));
      if Key.IsEmpty then
        Continue;

      Found := False;
      for J := 0 to High(FDialects) do
      begin
        if FDialects[J].Alias = Key then
        begin
          FDialects[J].Dialect := ADialect;
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        SetLength(FDialects, Length(FDialects) + 1);
        FDialects[High(FDialects)].Alias := Key;
        FDialects[High(FDialects)].Dialect := ADialect;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

class function TDeltaORMRegistry.FindDriver(const ANameOrProtocol: string): IDeltaDriver;
var
  I: Integer;
  Key: string;
begin
  Result := nil;
  Key := LowerCase(Trim(ANameOrProtocol));

  FLock.Enter;
  try
    for I := 0 to High(FDrivers) do
      if FDrivers[I].Name = Key then
        Exit(FDrivers[I].Driver);

    for I := 0 to High(FDrivers) do
      if FDrivers[I].Driver.SupportsProtocol(ANameOrProtocol) then
        Exit(FDrivers[I].Driver);
  finally
    FLock.Leave;
  end;
end;

class function TDeltaORMRegistry.ResolveDriver(const ANameOrProtocol: string): IDeltaDriver;
begin
  Result := FindDriver(ANameOrProtocol);
  if Result = nil then
    raise Exception.CreateFmt(
      'No database driver registered for "%s". Include a driver unit such as DeltaModel.ORM.Driver.SQLDB.',
      [ANameOrProtocol]);
end;

class function TDeltaORMRegistry.FindDialect(const ANameOrProtocol: string): IDeltaDialect;
var
  I: Integer;
  Key: string;
begin
  Result := nil;
  Key := LowerCase(Trim(ANameOrProtocol));

  FLock.Enter;
  try
    for I := 0 to High(FDialects) do
      if FDialects[I].Alias = Key then
        Exit(FDialects[I].Dialect);
  finally
    FLock.Leave;
  end;
end;

class function TDeltaORMRegistry.ResolveDialect(const ANameOrProtocol: string): IDeltaDialect;
begin
  Result := FindDialect(ANameOrProtocol);
  if Result = nil then
    raise Exception.CreateFmt(
      '"%s" is not a registered Database Dialect. Include DeltaModel.ORM.Dialects or the specific dialect unit.',
      [ANameOrProtocol]);
end;

class procedure TDeltaORMRegistry.RegisteredDrivers(AList: TStrings);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  AList.Clear;
  FLock.Enter;
  try
    for I := 0 to High(FDrivers) do
      AList.Add(FDrivers[I].Name);
  finally
    FLock.Leave;
  end;
end;

class procedure TDeltaORMRegistry.RegisteredDialects(AList: TStrings);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  AList.Clear;
  FLock.Enter;
  try
    for I := 0 to High(FDialects) do
      AList.Add(FDialects[I].Alias);
  finally
    FLock.Leave;
  end;
end;

class procedure TDeltaORMRegistry.Clear;
begin
  FLock.Enter;
  try
    SetLength(FDrivers, 0);
    SetLength(FDialects, 0);
  finally
    FLock.Leave;
  end;
end;

{ TDeltaORM }

class function TDeltaORM.CreateConnection(const ADatabaseURL: string;
  const ADriverName: string): IDeltaConnection;
var
  Config: TDatabaseConfig;
  EffectiveDriverName: string;
  Driver: IDeltaDriver;
  DialectObj: IDeltaDialect;
begin
  Config := ParseDatabaseURL(ADatabaseURL);
  try
    EffectiveDriverName := Trim(ADriverName);
    if EffectiveDriverName.IsEmpty and Assigned(Config.Params) then
      EffectiveDriverName := Trim(Config.Params.Values['driver']);

    DialectObj := TDeltaORMRegistry.ResolveDialect(Config.Protocol);
    if EffectiveDriverName.IsEmpty then
      Driver := TDeltaORMRegistry.ResolveDriver(Config.Protocol)
    else
      Driver := TDeltaORMRegistry.ResolveDriver(EffectiveDriverName);

    Result := Driver.CreateConnection(Config, DialectObj);
  finally
    Config.Params.Free;
  end;
end;

class function TDeltaORM.Connect(const ADatabaseURL: string;
  const ADriverName: string): IDeltaConnection;
begin
  Result := CreateConnection(ADatabaseURL, ADriverName);
  Result.Connect;
end;

class function TDeltaORM.Dialect(const ANameOrProtocol: string): IDeltaDialect;
begin
  Result := TDeltaORMRegistry.ResolveDialect(ANameOrProtocol);
end;

end.
