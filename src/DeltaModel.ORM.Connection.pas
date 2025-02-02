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
    property Connection: TSQLConnector read FConnection;

    constructor Create(const ADatabaseURL: string);
    destructor Destroy; override;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    function ExecuteQuery(const ASQL: string): TDataSet;
    function NewDataset: TSQLQuery;

    function Delete(AModel: TDeltaModel): Boolean;
    function Merge(AModel: TDeltaModel): Boolean;
    function Insert(AModel: TDeltaModel): Boolean;
    function Query(AModelClass: TDeltaModelClass): TQuery;

    function Dialect: TDatabaseDialect;
  end;

var
  DeltaORMEngine: TDeltaORMEngine;

implementation

{ TDeltaORMEngine }

function TDeltaORMEngine.Dialect: TDatabaseDialect;
begin
  Result := TDatabaseDialectHelper.FromString(FConnection.ConnectorType);
end;

constructor TDeltaORMEngine.Create(const ADatabaseURL: string);
var
  Config: TDatabaseConfig;
begin
  FConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommit;
  FTransaction.DataBase := FConnection;
  FQuery := TQuery.Create(Self);

  Config := ParseDatabaseURL(ADatabaseURL);

  FConnection.ConnectorType := Config.Protocol;
  FConnection.HostName := Config.Host;
  FConnection.CharSet := Config.Charset; 
  FConnection.UserName := Config.Username;
  FConnection.Password := Config.Password;
  FConnection.DatabaseName := Config.Database;

  if Config.Port > 0 then
    FConnection.Params.Values['Port'] := Config.Port.ToString;
end;

destructor TDeltaORMEngine.Destroy;
begin
  FTransaction.Free;
  FConnection.Free;
  FQuery.Free;
  inherited Destroy;
end;

procedure TDeltaORMEngine.StartTransaction;
begin
  FTransaction.StartTransaction;
end;

procedure TDeltaORMEngine.Commit;
begin
  FTransaction.Commit;
end;

procedure TDeltaORMEngine.Rollback;
begin
  FTransaction.Rollback;
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

function TDeltaORMEngine.Insert(AModel: TDeltaModel): Boolean;
begin
  Result := TInsert.InsertObject(Self, AModel);
end;

function TDeltaORMEngine.Query(AModelClass: TDeltaModelClass): TQuery;
begin
  FQuery.SetModel(AModelClass);
  Result := FQuery;
end;

end.

