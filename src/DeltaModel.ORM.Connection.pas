unit DeltaModel.ORM.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, DB, SQLDB,
  DeltaModel, DatabaseURLParser, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Types, DeltaModel.ORM.DML;

type

  { TDeltaORMEngine }

  TDeltaORMEngine = class(TInterfacedObject, IDeltaORMEngine)
  private
    FConnection: TSQLConnector;
    FTransaction: TSQLTransaction;
    FDialect: TDatabaseDialect;
    procedure SetupDialect(const AProtocol: string);
  protected
    function _AddRef: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    function Connection: TSQLConnector;
    function Dialect: TDatabaseDialect;

    constructor Create(const ADatabaseURL: string); overload;
    constructor Create(AConnectorType, AHost, ADatabase, AUser, APass: string; APort: Integer = 0); overload;
    destructor Destroy; override;

    procedure StartTransaction;
    function TransactionActive: Boolean;
    procedure Commit;
    procedure Rollback;
    procedure InTransaction(AProc: TDeltaTransactionProc); overload;
    procedure InTransaction(AProc: TDeltaTransactionStaticProc); overload;
    procedure Disconnect;
    //procedure Disconect; // Para retrocompatibilidade de escrita

    function ExecuteQuery(const ASQL: string): TDataSet;
    procedure ExecuteDirect(const ASQL: string);
    function ExecuteScalar(const ASQL: string): Variant;
    function NewDataset: TSQLQuery;

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

implementation

{ TDeltaORMEngine }

function TDeltaORMEngine._AddRef: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TDeltaORMEngine._Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

procedure TDeltaORMEngine.SetupDialect(const AProtocol: string);
var
  Prot: string;
begin
  Prot := LowerCase(AProtocol);
  if (Prot = 'sqlite3') or (Prot = 'sqlite') then
    FDialect := ddSQLite
  else
  if (Prot = 'postgresql') or (Prot = 'postgres') then
    FDialect := ddPostgreSQL
  else
  if (Prot = 'firebird') or (Prot = 'interbase') then
    FDialect := ddFirebird
  else
  if Pos('mysql', Prot) > 0 then
    FDialect := ddMySQL
  else
  if (Prot = 'mariadb') then
    FDialect := ddMySQL
  else
  if (Prot = 'mssqlserver') or (Prot = 'mssql') or (Prot = 'sqlserver') then
    FDialect := ddMSSQL
  else
  if (Prot = 'oracle') then
    FDialect := ddOracle
  else
    FDialect := TDatabaseDialectHelper.FromString(AProtocol);
end;

function TDeltaORMEngine.Dialect: TDatabaseDialect;
begin
  Result := FDialect;
end;

function TDeltaORMEngine.Connection: TSQLConnector;
begin
  Result := FConnection;
end;

constructor TDeltaORMEngine.Create(const ADatabaseURL: string);
var
  Config: TDatabaseConfig;
  I: Integer;
begin
  FConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommitRetaining;
  FTransaction.DataBase := FConnection;

  Config := ParseDatabaseURL(ADatabaseURL);
  try
    SetupDialect(Config.Protocol);

    FConnection.ConnectorType := Config.Protocol;
    FConnection.HostName := Config.Host;
    FConnection.UserName := Config.Username;
    FConnection.Password := Config.Password;
    FConnection.DatabaseName := Config.Database;

    if not Config.Charset.IsEmpty then
      FConnection.CharSet := Config.Charset
    else
      FConnection.CharSet := 'UTF8';

    if Config.Port > 0 then
      FConnection.Params.Values['port'] := Config.Port.ToString;

    // Transfere quaisquer parâmetros adicionais da URL
    if Assigned(Config.Params) then
    begin
      for I := 0 to Pred(Config.Params.Count) do
        FConnection.Params.Values[Config.Params.Names[I]] := Config.Params.ValueFromIndex[I];
    end;
  finally
    Config.Params.Free;
  end;
end;

constructor TDeltaORMEngine.Create(AConnectorType, AHost, ADatabase, AUser,
  APass: string; APort: Integer);
begin
  FConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommitRetaining;
  FTransaction.DataBase := FConnection;

  SetupDialect(AConnectorType);

  FConnection.ConnectorType := AConnectorType;
  FConnection.HostName := AHost;
  FConnection.UserName := AUser;
  FConnection.Password := APass;
  FConnection.DatabaseName := ADatabase;
  FConnection.CharSet := 'UTF8';

  if APort > 0 then
    FConnection.Params.Values['port'] := IntToStr(APort);
end;

destructor TDeltaORMEngine.Destroy;
begin
  Disconnect;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TDeltaORMEngine.StartTransaction;
begin
  if not FTransaction.Active then
    FTransaction.StartTransaction;
end;

function TDeltaORMEngine.TransactionActive: Boolean;
begin
  Result := FTransaction.Active;
end;

procedure TDeltaORMEngine.Commit;
begin
  if FTransaction.Active then
    FTransaction.Commit;
end;

procedure TDeltaORMEngine.Rollback;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
end;

procedure TDeltaORMEngine.InTransaction(AProc: TDeltaTransactionProc);
begin
  StartTransaction;
  try
    AProc();
    Commit;
  except
    Rollback;
    raise;
  end;
end;

procedure TDeltaORMEngine.InTransaction(AProc: TDeltaTransactionStaticProc);
begin
  StartTransaction;
  try
    AProc();
    Commit;
  except
    Rollback;
    raise;
  end;
end;

procedure TDeltaORMEngine.Disconnect;
begin
  if FTransaction.Active then
    FTransaction.Rollback;

  if FConnection.Connected then
    FConnection.Close(True);
end;

//procedure TDeltaORMEngine.Disconect;
//begin
//  Disconnect;
//end;

function TDeltaORMEngine.ExecuteQuery(const ASQL: string): TDataSet;
var
  Qry: TSQLQuery;
begin
  Qry := Self.NewDataset;
  try
    Qry.SQL.Text := ASQL;
    Qry.Open;
    Result := Qry;
  except
    Qry.Free;
    raise;
  end;
end;

procedure TDeltaORMEngine.ExecuteDirect(const ASQL: string);
begin
  FConnection.ExecuteDirect(ASQL);
end;

function TDeltaORMEngine.ExecuteScalar(const ASQL: string): Variant;
var
  DS: TSQLQuery;
begin
  Result := Null;
  DS := NewDataset;
  try
    DS.SQL.Text := ASQL;
    DS.Open;
    if (not DS.IsEmpty) and (DS.FieldCount > 0) then
      Result := DS.Fields[0].Value;
    DS.Close;
  finally
    DS.Free;
  end;
end;

function TDeltaORMEngine.NewDataset: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConnection;
  Result.Transaction := FTransaction;
end;

function TDeltaORMEngine.Insert(AModel: TDeltaModel): Boolean;
begin
  Result := TInsert.InsertObject(Self, AModel);
end;

function TDeltaORMEngine.InsertReturning(AModel: TDeltaModel;
  AClass: TDeltaModelClass): TDeltaModel;
begin
  Result := TInsert.InsertObject(Self, AModel, AClass);
end;

function TDeltaORMEngine.Merge(AModel: TDeltaModel): Boolean;
begin
  Result := TUpdate.UpdateObject(Self, AModel);
end;

function TDeltaORMEngine.MergeReturning(AModel: TDeltaModel;
  AClass: TDeltaModelClass): TDeltaModel;
begin
  Result := TUpdate.UpdateObject(Self, AModel, AClass);
end;

function TDeltaORMEngine.Save(AModel: TDeltaModel): Boolean;
begin
  Result := TSave.SaveObject(Self, AModel);
end;

function TDeltaORMEngine.Delete(AModel: TDeltaModel): Boolean;
begin
  Result := TDelete.Exec(Self, AModel);
end;

function TDeltaORMEngine.DeleteById(AModelClass: TDeltaModelClass;
  const AId: Variant): Boolean;
begin
  Result := TDelete.ExecById(Self, AModelClass, AId);
end;

function TDeltaORMEngine.BulkInsert(AModels: array of TDeltaModel; ABatchSize: Integer): Integer;
begin
  Result := TInsert.BulkInsertObjects(Self, AModels, ABatchSize);
end;

function TDeltaORMEngine.BulkInsert(AList: TDeltaModelList; ABatchSize: Integer): Integer;
begin
  Result := TInsert.BulkInsertObjects(Self, AList, ABatchSize);
end;

function TDeltaORMEngine.Find(AModelClass: TDeltaModelClass;
  const AId: Variant): TDeltaModel;
var
  Q: TQuery;
begin
  Q := TQuery.Create(Self);
  try
    Q.SetModel(AModelClass);
    Result := Q.FindById(AId);
  finally
    Q.Free;
  end;
end;

function TDeltaORMEngine.Count(AModelClass: TDeltaModelClass;
  const AWhere: string): Int64;
var
  Q: TQuery;
begin
  Q := TQuery.Create(Self);
  try
    Q.SetModel(AModelClass);
    if not AWhere.IsEmpty then
      Q.Where(AWhere);
    Result := Q.Count;
  finally
    Q.Free;
  end;
end;

function TDeltaORMEngine.Query(AModelClass: TDeltaModelClass): TQuery;
begin
  Result := TQuery.Create(Self);
  Result.SetModel(AModelClass);
end;

function TDeltaORMEngine.NewQuery: TQuery;
begin
  Result := TQuery.Create(Self);
end;

end.
