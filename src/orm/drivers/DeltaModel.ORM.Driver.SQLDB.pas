unit DeltaModel.ORM.Driver.SQLDB;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, DB, SQLDB,
  DatabaseURLParser, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Connection, DeltaModel.ORM.Registry;

type
  { ISQLDBConnectionAccess: Optional escape hatch to underlying SQLDB components }
  ISQLDBConnectionAccess = interface
    ['{8E4F01A2-1122-4455-8899-AABBCCDD1101}']
    function Connector: TSQLConnector;
    function Transaction: TSQLTransaction;
  end;

  { TSQLDBResultSet }
  TSQLDBResultSet = class(TBaseDeltaResultSet)
  private
    FQuery: TSQLQuery;
    FFirstRowPending: Boolean;
    function MapFieldType(AFieldType: TFieldType): TDeltaParamType;
  public
    constructor Create(AQuery: TSQLQuery);
    destructor Destroy; override;

    function Next: Boolean; override;
    function EOF: Boolean; override;
    function IsEmpty: Boolean; override;
    function FieldCount: Integer; override;
    function FieldName(AIndex: Integer): string; override;
    function FieldType(AIndex: Integer): TDeltaParamType; override;
    function FindField(const AName: string): Integer; override;

    function IsNull(AIndex: Integer): Boolean; override;
    function AsInteger(AIndex: Integer): Integer; override;
    function AsLargeInt(AIndex: Integer): Int64; override;
    function AsFloat(AIndex: Integer): Double; override;
    function AsBoolean(AIndex: Integer): Boolean; override;
    function AsDateTime(AIndex: Integer): TDateTime; override;
    function AsString(AIndex: Integer): string; override;
    function AsVariant(AIndex: Integer): Variant; override;
    procedure Close; override;
  end;

  { TSQLDBStatement }
  TSQLDBStatement = class(TInterfacedObject, IDeltaStatement)
  private
    FConnector: TSQLConnector;
    FTransaction: TSQLTransaction;
    FSQL: string;
    FParams: TDeltaParams;
    procedure EnsureConnected;
    procedure BindParamsToQuery(AQuery: TSQLQuery);
  public
    constructor Create(AConnector: TSQLConnector;
      ATransaction: TSQLTransaction; const ASQL: string = '');
    destructor Destroy; override;

    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    function GetParams: TDeltaParams;

    function Execute: Int64;
    function ExecuteQuery: IDeltaResultSet;
    function ExecuteScalar: Variant;
    procedure ClearParams;
  end;

  { TSQLDBConnection }
  TSQLDBConnection = class(TBaseDeltaConnection, ISQLDBConnectionAccess)
  private
    FConnector: TSQLConnector;
    FTransaction: TSQLTransaction;
    procedure ConfigureConnector(const AConfig: TDatabaseConfig);
  public
    constructor Create(const AConfig: TDatabaseConfig;
      const ADialect: IDeltaDialect);
    destructor Destroy; override;

    function Connector: TSQLConnector;
    function Transaction: TSQLTransaction;

    function DriverName: string; override;
    function Connected: Boolean; override;
    procedure Connect; override;
    procedure Disconnect; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function TransactionActive: Boolean; override;

    function CreateStatement(const ASQL: string = ''): IDeltaStatement; override;
    procedure GetTableNames(ATables: TStrings); override;
  end;

  { TSQLDBDriver }
  TSQLDBDriver = class(TInterfacedObject, IDeltaDriver)
  public
    function Name: string;
    function SupportsProtocol(const AProtocol: string): Boolean;
    function CreateConnection(const AConfig: TDatabaseConfig;
      const ADialect: IDeltaDialect): IDeltaConnection;
  end;

implementation

{ TSQLDBResultSet }

constructor TSQLDBResultSet.Create(AQuery: TSQLQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FFirstRowPending := True;
end;

destructor TSQLDBResultSet.Destroy;
begin
  Close;
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TSQLDBResultSet.MapFieldType(AFieldType: TFieldType): TDeltaParamType;
begin
  case AFieldType of
    ftSmallint, ftInteger, ftWord, ftAutoInc:
      Result := dptInteger;
    ftLargeint:
      Result := dptLargeInt;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      Result := dptFloat;
    ftBoolean:
      Result := dptBoolean;
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := dptDateTime;
    ftMemo, ftWideMemo, ftFmtMemo:
      Result := dptMemo;
  else
    Result := dptString;
  end;
end;

function TSQLDBResultSet.Next: Boolean;
begin
  if (FQuery = nil) or (not FQuery.Active) then
    Exit(False);

  if FFirstRowPending then
  begin
    FFirstRowPending := False;
    Result := not FQuery.EOF;
  end
  else
  begin
    FQuery.Next;
    Result := not FQuery.EOF;
  end;
end;

function TSQLDBResultSet.EOF: Boolean;
begin
  Result := (FQuery = nil) or (not FQuery.Active) or FQuery.EOF;
end;

function TSQLDBResultSet.IsEmpty: Boolean;
begin
  Result := (FQuery = nil) or (not FQuery.Active) or FQuery.IsEmpty;
end;

function TSQLDBResultSet.FieldCount: Integer;
begin
  if (FQuery <> nil) and FQuery.Active then
    Result := FQuery.FieldCount
  else
    Result := 0;
end;

function TSQLDBResultSet.FieldName(AIndex: Integer): string;
begin
  Result := FQuery.Fields[AIndex].FieldName;
end;

function TSQLDBResultSet.FieldType(AIndex: Integer): TDeltaParamType;
begin
  Result := MapFieldType(FQuery.Fields[AIndex].DataType);
end;

function TSQLDBResultSet.FindField(const AName: string): Integer;
var
  F: TField;
begin
  if (FQuery = nil) or (not FQuery.Active) then
    Exit(-1);
  F := FQuery.FindField(AName);
  if F <> nil then
    Result := F.Index
  else
    Result := -1;
end;

function TSQLDBResultSet.IsNull(AIndex: Integer): Boolean;
begin
  Result := FQuery.Fields[AIndex].IsNull;
end;

function TSQLDBResultSet.AsInteger(AIndex: Integer): Integer;
begin
  Result := FQuery.Fields[AIndex].AsInteger;
end;

function TSQLDBResultSet.AsLargeInt(AIndex: Integer): Int64;
begin
  Result := FQuery.Fields[AIndex].AsLargeInt;
end;

function TSQLDBResultSet.AsFloat(AIndex: Integer): Double;
begin
  Result := FQuery.Fields[AIndex].AsFloat;
end;

function TSQLDBResultSet.AsBoolean(AIndex: Integer): Boolean;
begin
  Result := FQuery.Fields[AIndex].AsBoolean;
end;

function TSQLDBResultSet.AsDateTime(AIndex: Integer): TDateTime;
begin
  Result := FQuery.Fields[AIndex].AsDateTime;
end;

function TSQLDBResultSet.AsString(AIndex: Integer): string;
begin
  Result := FQuery.Fields[AIndex].AsString;
end;

function TSQLDBResultSet.AsVariant(AIndex: Integer): Variant;
begin
  Result := FQuery.Fields[AIndex].Value;
end;

procedure TSQLDBResultSet.Close;
begin
  if (FQuery <> nil) and FQuery.Active then
    FQuery.Close;
end;

{ TSQLDBStatement }

constructor TSQLDBStatement.Create(AConnector: TSQLConnector;
  ATransaction: TSQLTransaction; const ASQL: string);
begin
  inherited Create;
  FConnector := AConnector;
  FTransaction := ATransaction;
  FSQL := ASQL;
  FParams := TDeltaParams.Create;
end;

destructor TSQLDBStatement.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

function TSQLDBStatement.GetSQL: string;
begin
  Result := FSQL;
end;

procedure TSQLDBStatement.SetSQL(const AValue: string);
begin
  FSQL := AValue;
end;

function TSQLDBStatement.GetParams: TDeltaParams;
begin
  Result := FParams;
end;

procedure TSQLDBStatement.EnsureConnected;
begin
  if (FConnector <> nil) and (not FConnector.Connected) then
    FConnector.Open;
end;

procedure TSQLDBStatement.BindParamsToQuery(AQuery: TSQLQuery);
var
  I: Integer;
  Src: TDeltaParam;
  Dst: TParam;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    Src := FParams[I];
    Dst := AQuery.Params.FindParam(Src.Name);
    if Dst = nil then
      Continue;

    if Src.IsNull then
    begin
      Dst.Clear;
      Continue;
    end;

    case Src.ParamType of
      dptInteger:  Dst.AsInteger := Src.AsInteger;
      dptLargeInt: Dst.AsLargeInt := Src.AsLargeInt;
      dptFloat:    Dst.AsFloat := Src.AsFloat;
      dptBoolean:  Dst.AsBoolean := Src.AsBoolean;
      dptDateTime: Dst.AsDateTime := Src.AsDateTime;
      dptMemo:
        begin
          Dst.DataType := ftMemo;
          Dst.AsMemo := Src.AsString;
        end;
    else
      Dst.AsString := Src.AsString;
    end;
  end;
end;

function TSQLDBStatement.Execute: Int64;
var
  Qry: TSQLQuery;
begin
  EnsureConnected;

  if (FParams.Count = 0) and ((FTransaction = nil) or (not FTransaction.Active)) then
  begin
    FConnector.ExecuteDirect(FSQL);
    Exit(0);
  end;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.DataBase := FConnector;
    Qry.Transaction := FTransaction;
    Qry.SQL.Text := FSQL;
    BindParamsToQuery(Qry);
    Qry.ExecSQL;
    Result := Qry.RowsAffected;
  finally
    Qry.Free;
  end;
end;

function TSQLDBStatement.ExecuteQuery: IDeltaResultSet;
var
  Qry: TSQLQuery;
begin
  EnsureConnected;

  Qry := TSQLQuery.Create(nil);
  try
    Qry.DataBase := FConnector;
    Qry.Transaction := FTransaction;
    Qry.UniDirectional := False;
    Qry.PacketRecords := -1;
    Qry.SQL.Text := FSQL;
    BindParamsToQuery(Qry);
    Qry.Open;
    Result := TSQLDBResultSet.Create(Qry);
  except
    Qry.Free;
    raise;
  end;
end;

function TSQLDBStatement.ExecuteScalar: Variant;
var
  RS: IDeltaResultSet;
begin
  Result := Null;
  RS := ExecuteQuery;
  try
    if RS.Next and (RS.FieldCount > 0) then
      Result := RS.AsVariant(0);
  finally
    RS.Close;
  end;
end;

procedure TSQLDBStatement.ClearParams;
begin
  FParams.Clear;
end;

{ TSQLDBConnection }

constructor TSQLDBConnection.Create(const AConfig: TDatabaseConfig;
  const ADialect: IDeltaDialect);
begin
  inherited Create(ADialect);
  FConnector := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommitRetaining;
  FTransaction.DataBase := FConnector;
  ConfigureConnector(AConfig);
end;

destructor TSQLDBConnection.Destroy;
begin
  Disconnect;
  FTransaction.Free;
  FConnector.Free;
  inherited Destroy;
end;

procedure TSQLDBConnection.ConfigureConnector(const AConfig: TDatabaseConfig);
var
  I: Integer;
  ParamName: string;
begin
  FConnector.ConnectorType := AConfig.Protocol;
  FConnector.HostName := AConfig.Host;
  FConnector.UserName := AConfig.Username;
  FConnector.Password := AConfig.Password;
  FConnector.DatabaseName := AConfig.Database;

  if not AConfig.Charset.IsEmpty then
    FConnector.CharSet := AConfig.Charset
  else if (Dialect <> nil) and SameText(Dialect.Name, 'MySQL') then
    FConnector.CharSet := 'utf8mb4'
  else
    FConnector.CharSet := 'UTF8';

  if AConfig.Port > 0 then
  begin
    FConnector.Params.Values['Port'] := AConfig.Port.ToString;
    FConnector.Params.Values['port'] := AConfig.Port.ToString;
  end;

  if Assigned(AConfig.Params) then
  begin
    for I := 0 to AConfig.Params.Count - 1 do
    begin
      ParamName := AConfig.Params.Names[I];
      if not ParamName.IsEmpty and (not SameText(ParamName, 'driver')) then
        FConnector.Params.Values[ParamName] := AConfig.Params.ValueFromIndex[I];
    end;
  end;
end;

function TSQLDBConnection.Connector: TSQLConnector;
begin
  Result := FConnector;
end;

function TSQLDBConnection.Transaction: TSQLTransaction;
begin
  Result := FTransaction;
end;

function TSQLDBConnection.DriverName: string;
begin
  Result := 'sqldb';
end;

function TSQLDBConnection.Connected: Boolean;
begin
  Result := FConnector.Connected;
end;

procedure TSQLDBConnection.Connect;
begin
  if not FConnector.Connected then
    FConnector.Open;
end;

procedure TSQLDBConnection.Disconnect;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
  if FConnector.Connected then
    FConnector.Close(True);
end;

procedure TSQLDBConnection.StartTransaction;
begin
  Connect;
  if not FTransaction.Active then
    FTransaction.StartTransaction;
end;

procedure TSQLDBConnection.Commit;
begin
  if FTransaction.Active then
    FTransaction.Commit;
end;

procedure TSQLDBConnection.Rollback;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
end;

function TSQLDBConnection.TransactionActive: Boolean;
begin
  Result := FTransaction.Active;
end;

function TSQLDBConnection.CreateStatement(const ASQL: string): IDeltaStatement;
begin
  Result := TSQLDBStatement.Create(FConnector, FTransaction, ASQL);
end;

procedure TSQLDBConnection.GetTableNames(ATables: TStrings);
begin
  if ATables = nil then
    Exit;
  Connect;
  FConnector.GetTableNames(ATables);
end;

{ TSQLDBDriver }

function TSQLDBDriver.Name: string;
begin
  Result := 'sqldb';
end;

function TSQLDBDriver.SupportsProtocol(const AProtocol: string): Boolean;
var
  P: string;
begin
  P := LowerCase(Trim(AProtocol));
  Result := (P = 'sqlite') or (P = 'sqlite3') or
            (P = 'postgresql') or (P = 'postgres') or (P = 'pgsql') or (P = 'pg') or
            (Pos('mysql', P) > 0) or (P = 'mariadb') or
            (P = 'firebird') or (P = 'fdb') or (P = 'interbase') or (P = 'ib') or
            (P = 'mssql') or (P = 'mssqlserver') or (P = 'sqlserver') or
            (P = 'oracle') or (P = 'ora') or
            (P = 'odbc') or (P = 'sybase');
end;

function TSQLDBDriver.CreateConnection(const AConfig: TDatabaseConfig;
  const ADialect: IDeltaDialect): IDeltaConnection;
begin
  Result := TSQLDBConnection.Create(AConfig, ADialect);
end;

initialization
  TDeltaORMRegistry.RegisterDriver(TSQLDBDriver.Create);

end.
