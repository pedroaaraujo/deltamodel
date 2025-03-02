unit DeltaModel.ORM.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DB, SQLDB,
  DeltaModel, DatabaseURLParser, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Types, DeltaModel.ORM.DML;

type

  { TDeltaORMEngine }
  TDeltaORMEngine = class(TInterfacedObject, IDeltaORMEngine)
  private
    FConnection: TSQLConnector;
    FTransaction: TSQLTransaction;
    FQuery: TQuery;
  public
    function Connection: TSQLConnector;

    constructor Create(const ADatabaseURL: string);
    destructor destroy; override;

    procedure StartTransaction;
    function TransactionActive: Boolean;
    procedure Commit;
    procedure Rollback;
    procedure Disconect;

    function ExecuteQuery(const ASQL: string): TDataSet;
    procedure ExecuteDirect(const ASQL: string);
    function NewDataset: TSQLQuery;

    function Delete(AModel: TDeltaModel): Boolean;
    function Merge(AModel: TDeltaModel): Boolean;
    function MergeReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Insert(AModel: TDeltaModel): Boolean;
    function InsertReturning(AModel: TDeltaModel; AClass: TDeltaModelClass): TDeltaModel;
    function Query(AModelClass: TDeltaModelClass): TQuery;

    function Dialect: TDatabaseDialect;
  end;

implementation

{ TDeltaORMEngine }

function TDeltaORMEngine.Dialect: TDatabaseDialect;
begin
  Result := TDatabaseDialectHelper.FromString(FConnection.ConnectorType);
end;

function TDeltaORMEngine.Connection: TSQLConnector;
begin
  Result := FConnection;
end;

constructor TDeltaORMEngine.Create(const ADatabaseURL: string);
var
  Config: TDatabaseConfig;
begin
  FConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommitRetaining;
  FTransaction.DataBase := FConnection;
  FQuery := TQuery.Create(Self);

  Config := ParseDatabaseURL(ADatabaseURL);

  FConnection.ConnectorType := Config.Protocol;
  FConnection.HostName := Config.Host;
  FConnection.UserName := Config.Username;
  FConnection.Password := Config.Password;
  FConnection.DatabaseName := Config.Database;

  FConnection.CharSet := Config.Charset;
  if Config.Charset.IsEmpty then
    FConnection.CharSet := 'UTF8';

  if Config.Port > 0 then
    FConnection.Params.Values['port'] := Config.Port.ToString;
end;

destructor TDeltaORMEngine.destroy;
begin
  Disconect;
  FTransaction.Free;
  FConnection.Free;
  FQuery.Free;

  inherited destroy;
end;

procedure TDeltaORMEngine.StartTransaction;
begin
  FTransaction.StartTransaction;
end;

function TDeltaORMEngine.TransactionActive: Boolean;
begin
  Result := FTransaction.Active;
end;

procedure TDeltaORMEngine.Commit;
begin
  FTransaction.Commit;
end;

procedure TDeltaORMEngine.Rollback;
begin
  FTransaction.Rollback;
end;

procedure TDeltaORMEngine.Disconect;
begin
  FConnection.Close(True);
end;

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

function TDeltaORMEngine.NewDataset: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConnection;
  Result.Transaction := FTransaction;
end;

function TDeltaORMEngine.Delete(AModel: TDeltaModel): Boolean;
begin
  Result := TDelete.Exec(Self, AModel);
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

function TDeltaORMEngine.Insert(AModel: TDeltaModel): Boolean;
begin
  Result := TInsert.InsertObject(Self, AModel);
end;

function TDeltaORMEngine.InsertReturning(AModel: TDeltaModel;
  AClass: TDeltaModelClass): TDeltaModel;
begin
  Result := TInsert.InsertObject(Self, AModel, AClass);
end;

function TDeltaORMEngine.Query(AModelClass: TDeltaModelClass): TQuery;
begin
  FQuery.SetModel(AModelClass);
  Result := FQuery;
end;

end.

