unit DeltaModel.ORM.DML;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DeltaModel, DeltaModel.Fields, DeltaValidator,
  DeltaModel.SQLBuilder, DeltaModel.ORM.Interfaces, DeltaModel.DataSetConverter;

type

  { TQuery }

  TQuery = class
  private
    FModelClass: TDeltaModelClass;
    FConn: IDeltaORMEngine;
    FFilter: string;
    FOrderBy: string;
  public
    constructor Create(AConn: IDeltaORMEngine);
    destructor Destroy; override;

    function OrderBy(const AField: string): TQuery;
    function Filter(const Value: string): TQuery;
    function SetModel(AModel: TDeltaModelClass): TQuery;
    function First: TDeltaModel;
    function All(Limit: Integer = -1; Offset: Integer = -1): TDeltaModelList;

    procedure Clear;
  end;

  { TDelete }

  TDelete = class
  public
    class function Exec(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static;
  end;

  { TUpdate }

  TUpdate = class
  public
    class function UpdateObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static; overload;
    class function UpdateObject(AConn: IDeltaORMEngine; AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel; static; overload;
  end;

  { TInsert }

  TInsert = class
  public
    class function InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static; overload;
    class function InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel; static; overload;
  end;

implementation

constructor TQuery.Create(AConn: IDeltaORMEngine);
begin
  FConn := AConn;

  FFilter := '';
end;

destructor TQuery.Destroy;
begin
  inherited Destroy;
end;

function TQuery.OrderBy(const AField: string): TQuery;
begin
  FOrderBy := AField;
  Result := Self;
end;

function TQuery.Filter(const Value: string): TQuery;
begin
  Result := Self;
  FFilter := Value;
end;

function TQuery.SetModel(AModel: TDeltaModelClass): TQuery;
begin
  Result := Self;
  FModelClass := AModel;
end;

function TQuery.First: TDeltaModel;
var
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
begin
  Result := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(Result, FConn.Dialect);
  try
    DS.SQL.Text :=
      SQLBuilder
      .Select
      .Limit(1)
      .OrderBy(FOrderBy)
      .Where(FFilter)
      .Build;
    DS.Open;
    if DS.IsEmpty then
    begin
      Result.Free;
      Exit(nil);
    end;

    FromDataSet(Result, DS);
    DS.Close;
  finally
    SQLBuilder.Free;
    DS.Free;
  end;
end;

function TQuery.All(Limit: Integer; Offset: Integer): TDeltaModelList;
var
  ObjTemp, Obj: TDeltaModel;
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
begin
  ObjTemp := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(ObjTemp, FConn.Dialect);
  Result := TDeltaModelList.Create;
  try
    Result.DeltaModelClass := FModelClass;
    DS.SQL.Text :=
      SQLBuilder
      .Select
      .Limit(Limit)
      .Offset(Offset)
      .Where(FFilter)
      .OrderBy(FOrderBy)
      .Build;
    DS.Open;
    if DS.IsEmpty then
    begin
      Exit();
    end;

    DS.First;
    while not DS.EOF do
    begin
      Obj := FModelClass.Create;
      FromDataSet(Obj, DS);
      Result.Records.Add(Obj);
      DS.Next;
    end;
    DS.Close;
  finally
    SQLBuilder.Free;
    ObjTemp.Free;
    DS.Free;
  end;
end;

procedure TQuery.Clear;
begin
  FFilter := '';
end;

{ TDelete }

class function TDelete.Exec(AConn: IDeltaORMEngine; AModel: TDeltaModel
  ): Boolean;
var
  DS: TSQLQuery;
begin
  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateDelete(
      AModel,
      AConn.Dialect
    );
    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
  finally
    DS.Free;
  end;
end;

{ TUpdate }

class function TUpdate.UpdateObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel): Boolean;
var
  DS: TSQLQuery;
begin
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
    begin
      raise EDeltaValidation.Create(Message);
    end;
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateUpdate(
      AModel,
      AConn.Dialect,
      TDMSQLBuilder.WhereClausePK(AModel)
    );
    ToDatasetParams(AModel, DS);

    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
  finally
    DS.Free;
  end;
end;

class function TUpdate.UpdateObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel;
var
  DS: TSQLQuery;
begin
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
    begin
      raise EDeltaValidation.Create(Message);
    end;
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateUpdateReturning(
      AModel,
      AConn.Dialect,
      TDMSQLBuilder.WhereClausePK(AModel)
    );
    ToDatasetParams(AModel, DS);

    DS.Open;
    if DS.IsEmpty then
    begin
      Exit(nil);
    end;

    Result := Return.Create;
    FromDataSet(Result, DS);
  finally
    DS.Free;
  end;
end;

{ TInsert }

class function TInsert.InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel
  ): Boolean;
var
  DS: TSQLQuery;
begin
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
    begin
      raise EDeltaValidation.Create(Message);
    end;
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateInsert(
      AModel,
      AConn.Dialect
    );
    ToDatasetParams(AModel, DS);

    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
  finally
    DS.Free;
  end;
end;

class function TInsert.InsertObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel;
var
  DS: TSQLQuery;
begin
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
    begin
      raise EDeltaValidation.Create(Message);
    end;
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateInsertReturning(
      AModel,
      AConn.Dialect
    );
    ToDatasetParams(AModel, DS);

    DS.Open;
    if DS.IsEmpty then
    begin
      Exit(nil);
    end;

    Result := Return.Create;
    FromDataSet(Result, DS);
  finally
    DS.Free;
  end;
end;

end.

