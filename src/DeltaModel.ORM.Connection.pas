unit DeltaModel.ORM.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DeltaModel, DeltaModel.SQLBuilder,
  DatabaseURLParser, DeltaModel.DataSetConverter, StrUtils;

type
  IDeltaORMEngine = interface
  ['{5FCD6178-213B-42D9-8712-33B953EEDACD}']
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  end;

  { TDeltaORMEngine }
  TDeltaORMEngine = class(TInterfacedObject, IDeltaORMEngine)
  private
    FConnection: TSQLConnector;
    FTransaction: TSQLTransaction;
    function Dialect: TDatabaseDialect;
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
  end;

var
  DeltaORMEngine: TDeltaORMEngine;

implementation

{ TDeltaORMEngine }

function TDeltaORMEngine.Dialect: TDatabaseDialect;
begin
  TDatabaseDialectHelper.FromString(FConnection.ConnectorType);
end;

constructor TDeltaORMEngine.Create(const ADatabaseURL: string);
var
  Config: TDatabaseConfig;
begin
  FConnection := TSQLConnector.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Action := caCommit;
  FTransaction.DataBase := FConnection;

  Config := ParseDatabaseURL(ADatabaseURL);

  FConnection.ConnectorType := Config.Protocol;
  FConnection.HostName := Config.Host;
  FConnection.CharSet := 'UTF-8';
  FConnection.Params.Values['UserName'] := Config.Username;
  FConnection.Params.Values['Password'] := Config.Password;
  FConnection.Params.Values['Database'] := Config.Database;

  if Config.Port > 0 then
    FConnection.Params.Values['Port'] := Config.Port.ToString;
end;

destructor TDeltaORMEngine.Destroy;
begin
  FTransaction.Free;
  FConnection.Free;
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
  Query: TSQLQuery;
begin
  Query := Self.NewDataset;
  try
    Query.SQL.Text := ASQL;
    Query.Open;
    Result := Query;
  except
    Query.Free;
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
var
  DS: TSQLQuery;
begin
  DS := NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateDelete(
      AModel,
      Dialect
    );
    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
  finally
    DS.Free;
  end;
end;

function TDeltaORMEngine.Merge(AModel: TDeltaModel): Boolean;
var
  DS: TSQLQuery;
begin
  DS := NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateUpdateReturning(
      AModel,
      Dialect
    );
    DS.Open;
    Result := not DS.IsEmpty;
    FromDataSet(AModel, DS);
  finally
    DS.Free;
  end;
end;

end.

