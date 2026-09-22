unit DeltaModel.ORM.DML;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Variants, TypInfo,
  DeltaModel, DeltaValidator, DeltaModel.Fields, DeltaModel.ORM.Types,
  DeltaModel.SQLBuilder, DeltaModel.ORM.Interfaces, DeltaModel.DataSetConverter;

type

  { TQuery }

  TQuery = class
  private
    FModelClass: TDeltaModelClass;
    FConn: IDeltaORMEngine;
    FFilter: string;
    FOrderBy: string;
    FSelectFields: string;
    FTableAlias: string;
    FJoins: TStringList;
    FLimit: Integer;
    FOffset: Integer;
    FParams: TParams;
    procedure BindParams(AQuery: TSQLQuery);
  public
    constructor Create(AConn: IDeltaORMEngine);
    destructor Destroy; override;

    function OrderBy(const AField: string): TQuery;
    function Filter(const Value: string): TQuery;
    function TableAlias(const AAlias: string): TQuery;
    function Join(AJoinType: TJoinType; const ATable, AOnCondition: string; const AAlias: string = ''): TQuery; overload;
    function Join(const AJoinClause: string): TQuery; overload;
    function InnerJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TQuery;
    function LeftJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TQuery;
    function RightJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TQuery;
    function FullJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TQuery;
    function Where(const Value: string): TQuery; overload;
    function Where(const AField: string; const AValue: Variant): TQuery; overload;
    function Where(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery; overload;
    function Where(const AField: string; const AOpStr: string; const AValue: Variant): TQuery; overload;
    function AndWhere(const Value: string): TQuery; overload;
    function AndWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function AndWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery; overload;
    function AndWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery; overload;
    function OrWhere(const Value: string): TQuery; overload;
    function OrWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function OrWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery; overload;
    function OrWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery; overload;
    function WhereBetween(const AField: string; const AVal1, AVal2: Variant): TQuery;
    function WhereIn(const AField: string; const AValues: array of Variant): TQuery;
    function WhereNotIn(const AField: string; const AValues: array of Variant): TQuery;
    function WhereNull(const AField: string): TQuery;
    function WhereNotNull(const AField: string): TQuery;
    function Limit(const ALimit: Integer): TQuery;
    function Offset(const AOffset: Integer): TQuery;
    function Page(const APageNumber, APageSize: Integer): TQuery;
    function Select(const AColumns: string): TQuery; overload;
    function Select(const AColumns: array of string): TQuery; overload;
    function Param(const AName: string; const AValue: Variant): TQuery;
    function SetModel(AModel: TDeltaModelClass): TQuery;

    function First: TDeltaModel;
    function All(ALimit: Integer = -1; AOffset: Integer = -1): TDeltaModelList;
    function FindById(const AId: Variant): TDeltaModel;
    function Count: Int64;
    function Exists: Boolean;
    function Delete: Boolean;

    procedure Clear;
  end;

  { TDelete }

  TDelete = class
  public
    class function Exec(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static;
    class function ExecById(AConn: IDeltaORMEngine; AModelClass: TDeltaModelClass; const AId: Variant): Boolean; static;
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
    class function BulkInsertObjects(AConn: IDeltaORMEngine; AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; static; overload;
    class function BulkInsertObjects(AConn: IDeltaORMEngine; AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; static; overload;
  end;

  { TSave - Decide entre Insert e Update baseado na Chave Primária }

  TSave = class
  public
    class function HasPKValue(AModel: TDeltaModel): Boolean; static;
    class function SaveObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static;
  end;

implementation

{ TQuery }

constructor TQuery.Create(AConn: IDeltaORMEngine);
begin
  inherited Create;
  FConn := AConn;
  FFilter := '';
  FOrderBy := '';
  FSelectFields := '';
  FTableAlias := '';
  FJoins := TStringList.Create;
  FLimit := -1;
  FOffset := -1;
  FParams := TParams.Create(nil);
end;

destructor TQuery.Destroy;
begin
  FJoins.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TQuery.BindParams(AQuery: TSQLQuery);
var
  I: Integer;
  TargetParam: TParam;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    TargetParam := AQuery.Params.FindParam(FParams[I].Name);
    if TargetParam <> nil then
      TargetParam.Value := FParams[I].Value;
  end;
end;

function TQuery.Param(const AName: string; const AValue: Variant): TQuery;
var
  P: TParam;
begin
  P := FParams.FindParam(AName);
  if P = nil then
    P := FParams.CreateParam(ftUnknown, AName, ptInput);
  P.Value := AValue;
  Result := Self;
end;

function TQuery.Select(const AColumns: string): TQuery;
begin
  FSelectFields := AColumns;
  Result := Self;
end;

function TQuery.Select(const AColumns: array of string): TQuery;
var
  I: Integer;
begin
  FSelectFields := '';
  for I := Low(AColumns) to High(AColumns) do
  begin
    if not FSelectFields.IsEmpty then
      FSelectFields := FSelectFields + ', ';
    FSelectFields := FSelectFields + AColumns[I].Trim;
  end;
  Result := Self;
end;

function TQuery.OrderBy(const AField: string): TQuery;
begin
  FOrderBy := AField;
  Result := Self;
end;

function TQuery.Filter(const Value: string): TQuery;
begin
  FFilter := Value;
  Result := Self;
end;

function TQuery.TableAlias(const AAlias: string): TQuery;
begin
  FTableAlias := AAlias.Trim;
  Result := Self;
end;

function TQuery.Join(AJoinType: TJoinType; const ATable, AOnCondition: string; const AAlias: string): TQuery;
var
  Clause: string;
begin
  Clause := TSQLCriteriaHelper.JoinTypeToString(AJoinType) + ' ' + ATable;
  if not AAlias.Trim.IsEmpty then
    Clause := Clause + ' ' + AAlias.Trim;
  Clause := Clause + ' ON ' + AOnCondition;
  FJoins.Add(Clause);
  Result := Self;
end;

function TQuery.Join(const AJoinClause: string): TQuery;
begin
  if not AJoinClause.Trim.IsEmpty then
    FJoins.Add(AJoinClause.Trim);
  Result := Self;
end;

function TQuery.InnerJoin(const ATable, AOnCondition: string; const AAlias: string): TQuery;
begin
  Result := Join(jtInner, ATable, AOnCondition, AAlias);
end;

function TQuery.LeftJoin(const ATable, AOnCondition: string; const AAlias: string): TQuery;
begin
  Result := Join(jtLeft, ATable, AOnCondition, AAlias);
end;

function TQuery.RightJoin(const ATable, AOnCondition: string; const AAlias: string): TQuery;
begin
  Result := Join(jtRight, ATable, AOnCondition, AAlias);
end;

function TQuery.FullJoin(const ATable, AOnCondition: string; const AAlias: string): TQuery;
begin
  Result := Join(jtFull, ATable, AOnCondition, AAlias);
end;

function TQuery.Where(const Value: string): TQuery;
begin
  Result := Filter(Value);
end;

function TQuery.Where(const AField: string; const AValue: Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AValue, FConn.Dialect))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FConn.Dialect));
end;

function TQuery.Where(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FConn.Dialect))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FConn.Dialect));
end;

function TQuery.Where(const AField: string; const AOpStr: string; const AValue: Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FConn.Dialect))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FConn.Dialect));
end;

function TQuery.AndWhere(const Value: string): TQuery;
begin
  if Value.IsEmpty then Exit(Self);
  if FFilter.IsEmpty then
    FFilter := Value
  else
    FFilter := '(' + FFilter + ') AND (' + Value + ')';
  Result := Self;
end;

function TQuery.AndWhere(const AField: string; const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FConn.Dialect));
end;

function TQuery.AndWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FConn.Dialect));
end;

function TQuery.AndWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FConn.Dialect));
end;

function TQuery.OrWhere(const Value: string): TQuery;
begin
  if Value.IsEmpty then Exit(Self);
  if FFilter.IsEmpty then
    FFilter := Value
  else
    FFilter := '(' + FFilter + ') OR (' + Value + ')';
  Result := Self;
end;

function TQuery.OrWhere(const AField: string; const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FConn.Dialect));
end;

function TQuery.OrWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FConn.Dialect));
end;

function TQuery.OrWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FConn.Dialect));
end;

function TQuery.WhereBetween(const AField: string; const AVal1, AVal2: Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildBetween(AField, AVal1, AVal2, FConn.Dialect))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildBetween(AField, AVal1, AVal2, FConn.Dialect));
end;

function TQuery.WhereIn(const AField: string; const AValues: array of Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FConn.Dialect, False))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildIn(AField, AValues, FConn.Dialect, False));
end;

function TQuery.WhereNotIn(const AField: string; const AValues: array of Variant): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FConn.Dialect, True))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildIn(AField, AValues, FConn.Dialect, True));
end;

function TQuery.WhereNull(const AField: string): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, True))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildNullCondition(AField, True));
end;

function TQuery.WhereNotNull(const AField: string): TQuery;
begin
  if FFilter.IsEmpty then
    Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, False))
  else
    Result := AndWhere(TSQLCriteriaHelper.BuildNullCondition(AField, False));
end;

function TQuery.Limit(const ALimit: Integer): TQuery;
begin
  FLimit := ALimit;
  Result := Self;
end;

function TQuery.Offset(const AOffset: Integer): TQuery;
begin
  FOffset := AOffset;
  Result := Self;
end;

function TQuery.Page(const APageNumber, APageSize: Integer): TQuery;
begin
  if (APageNumber > 0) and (APageSize > 0) then
  begin
    FLimit := APageSize;
    FOffset := (APageNumber - 1) * APageSize;
  end;
  Result := Self;
end;

function TQuery.SetModel(AModel: TDeltaModelClass): TQuery;
begin
  FModelClass := AModel;
  Result := Self;
end;

function TQuery.First: TDeltaModel;
var
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
  I: Integer;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');

  Result := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(Result, FConn.Dialect);
  try
    SQLBuilder.TableAlias(FTableAlias);
    for I := 0 to FJoins.Count - 1 do
      SQLBuilder.Join(FJoins[I]);

    if not FSelectFields.IsEmpty then
      SQLBuilder.Select(FSelectFields)
    else
      SQLBuilder.Select;

    DS.SQL.Text :=
      SQLBuilder
      .Limit(1)
      .OrderBy(FOrderBy)
      .Where(FFilter)
      .Build;
    BindParams(DS);
    DS.Open;
    if DS.IsEmpty then
    begin
      Result.Free;
      Result := nil;
      Exit;
    end;

    FromDataSet(Result, DS);
    DS.Close;
  finally
    SQLBuilder.Free;
    DS.Free;
  end;
end;

function TQuery.All(ALimit: Integer; AOffset: Integer): TDeltaModelList;
var
  ObjTemp, Obj: TDeltaModel;
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
  EffLimit, EffOffset: Integer;
  I: Integer;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');

  EffLimit := ALimit;
  if EffLimit = -1 then EffLimit := FLimit;
  EffOffset := AOffset;
  if EffOffset = -1 then EffOffset := FOffset;

  ObjTemp := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(ObjTemp, FConn.Dialect);
  Result := TDeltaModelList.Create;
  try
    Result.DeltaModelClass := FModelClass;
    SQLBuilder.TableAlias(FTableAlias);
    for I := 0 to FJoins.Count - 1 do
      SQLBuilder.Join(FJoins[I]);

    if not FSelectFields.IsEmpty then
      SQLBuilder.Select(FSelectFields)
    else
      SQLBuilder.Select;

    DS.SQL.Text :=
      SQLBuilder
      .Limit(EffLimit)
      .Offset(EffOffset)
      .Where(FFilter)
      .OrderBy(FOrderBy)
      .Build;
    BindParams(DS);
    DS.Open;
    if not DS.IsEmpty then
    begin
      DS.First;
      while not DS.EOF do
      begin
        Obj := FModelClass.Create;
        FromDataSet(Obj, DS);
        Result.Records.Add(Obj);
        DS.Next;
      end;
    end;
    DS.Close;
  finally
    SQLBuilder.Free;
    ObjTemp.Free;
    DS.Free;
  end;
end;

function TQuery.FindById(const AId: Variant): TDeltaModel;
var
  ObjTemp: TDeltaModel;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  PKFieldName: string;
  NestedObj: TObject;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery.');

  PKFieldName := '';
  ObjTemp := FModelClass.Create;
  try
    PropCount := GetPropList(ObjTemp.ClassInfo, tkProperties, nil);
    if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(Pointer));
      try
        GetPropList(ObjTemp.ClassInfo, tkProperties, PropList, False);
        for I := 0 to PropCount - 1 do
        begin
          PropInfo := PropList^[I];
          if PropInfo^.PropType^.Kind = tkClass then
          begin
            NestedObj := GetObjectProp(ObjTemp, PropInfo^.Name);
            if (NestedObj is DeltaModel.Fields.TDeltaField) and (dboPrimaryKey in (NestedObj as DeltaModel.Fields.TDeltaField).DBOptions) then
            begin
              PKFieldName := (NestedObj as DeltaModel.Fields.TDeltaField).FieldName;
              Break;
            end;
          end;
        end;
      finally
        FreeMem(PropList, PropCount * SizeOf(Pointer));
      end;
    end;
  finally
    ObjTemp.Free;
  end;

  if PKFieldName.IsEmpty then
    PKFieldName := 'id';

  FFilter := PKFieldName + ' = :__pk_id';
  Param('__pk_id', AId);

  Result := First;
end;

function TQuery.Count: Int64;
var
  ObjTemp: TDeltaModel;
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
  I: Integer;
begin
  Result := 0;
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery.');

  ObjTemp := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(ObjTemp, FConn.Dialect);
  try
    SQLBuilder.TableAlias(FTableAlias);
    for I := 0 to FJoins.Count - 1 do
      SQLBuilder.Join(FJoins[I]);

    DS.SQL.Text := SQLBuilder.Count.Where(FFilter).Build;
    BindParams(DS);
    DS.Open;
    if not DS.IsEmpty then
      Result := DS.Fields[0].AsLargeInt;
    DS.Close;
  finally
    SQLBuilder.Free;
    ObjTemp.Free;
    DS.Free;
  end;
end;

function TQuery.Exists: Boolean;
begin
  Result := Count > 0;
end;

function TQuery.Delete: Boolean;
var
  ObjTemp: TDeltaModel;
  DS: TSQLQuery;
begin
  Result := False;
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery.');

  ObjTemp := FModelClass.Create;
  DS := FConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateDelete(ObjTemp, FConn.Dialect, FFilter);
    BindParams(DS);
    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
  finally
    ObjTemp.Free;
    DS.Free;
  end;
end;

procedure TQuery.Clear;
begin
  FFilter := '';
  FOrderBy := '';
  FSelectFields := '';
  FTableAlias := '';
  FJoins.Clear;
  FLimit := -1;
  FOffset := -1;
  FParams.Clear;
end;

{ TDelete }

class function TDelete.Exec(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean;
var
  DS: TSQLQuery;
begin
  AModel.BeforeDelete;
  DS := AConn.NewDataset;
  try
    DS.SQL.Text := TDMSQLBuilder.CreateDelete(
      AModel,
      AConn.Dialect
    );
    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
    if Result then
      AModel.AfterDelete;
  finally
    DS.Free;
  end;
end;

class function TDelete.ExecById(AConn: IDeltaORMEngine; AModelClass: TDeltaModelClass;
  const AId: Variant): Boolean;
var
  Q: TQuery;
begin
  Q := TQuery.Create(AConn);
  try
    Q.SetModel(AModelClass);
    Q.Where('id = :__pk_id');
    Q.Param('__pk_id', AId);
    Result := Q.Delete;
  finally
    Q.Free;
  end;
end;

{ TUpdate }

class function TUpdate.UpdateObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel): Boolean;
var
  DS: TSQLQuery;
begin
  AModel.BeforeUpdate;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
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
    if Result then
      AModel.AfterUpdate;
  finally
    DS.Free;
  end;
end;

class function TUpdate.UpdateObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel;
var
  DS: TSQLQuery;
begin
  AModel.BeforeUpdate;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
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
      Exit(nil);

    Result := Return.Create;
    FromDataSet(Result, DS);
    DS.Close;
    if Assigned(Result) then
      AModel.AfterUpdate;
  finally
    DS.Free;
  end;
end;

{ TInsert }

class function TInsert.InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel
  ): Boolean;
var
  DS: TSQLQuery;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: DeltaModel.Fields.TDeltaField;
  NestedObj: TObject;
  LastId: Variant;
begin
  AModel.BeforeInsert;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
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
    if Result then
    begin
      PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
      if PropCount > 0 then
      begin
        GetMem(PropList, PropCount * SizeOf(Pointer));
        try
          GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
          for I := 0 to PropCount - 1 do
          begin
            PropInfo := PropList^[I];
            if PropInfo^.PropType^.Kind = tkClass then
            begin
              NestedObj := GetObjectProp(AModel, PropInfo^.Name);
              if NestedObj is DeltaModel.Fields.TDeltaField then
              begin
                Obj := NestedObj as DeltaModel.Fields.TDeltaField;
                if (dboAutoInc in Obj.DBOptions) and (Obj.IsNull or (VarIsNumeric(Obj.Value) and (Double(Obj.Value) = 0))) then
                begin
                  case AConn.Dialect of
                    ddSQLite:
                    begin
                      LastId := AConn.ExecuteScalar('SELECT last_insert_rowid()');
                      if not VarIsNull(LastId) then
                        Obj.Value := LastId;
                    end;
                    ddPostgreSQL:
                    begin
                      LastId := AConn.ExecuteScalar('SELECT lastval()');
                      if not VarIsNull(LastId) then
                        Obj.Value := LastId;
                    end;
                    ddMySQL:
                    begin
                      LastId := AConn.ExecuteScalar('SELECT LAST_INSERT_ID()');
                      if not VarIsNull(LastId) then
                        Obj.Value := LastId;
                    end;
                    ddMSSQL:
                    begin
                      LastId := AConn.ExecuteScalar('SELECT SCOPE_IDENTITY()');
                      if not VarIsNull(LastId) then
                        Obj.Value := LastId;
                    end;
                  end;
                  Break;
                end;
              end;
            end;
          end;
        finally
          FreeMem(PropList, PropCount * SizeOf(Pointer));
        end;
      end;
      AModel.AfterInsert;
    end;
  finally
    DS.Free;
  end;
end;

class function TInsert.InsertObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel;
var
  DS: TSQLQuery;
begin
  AModel.BeforeInsert;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
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
      Exit(nil);

    Result := Return.Create;
    FromDataSet(Result, DS);
    DS.Close;
    if Assigned(Result) then
      AModel.AfterInsert;
  finally
    DS.Free;
  end;
end;

class function TInsert.BulkInsertObjects(AConn: IDeltaORMEngine;
  AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer;
var
  DS: TSQLQuery;
  I, TotalCount, RowsAff: Integer;
  OwnsTransaction: Boolean;
begin
  Result := 0;
  TotalCount := Length(AModels);
  if TotalCount = 0 then Exit;

  for I := 0 to TotalCount - 1 do
  begin
    AModels[I].BeforeInsert;
    AModels[I].Validate;
    with AModels[I].Validator.Validate do
    begin
      if not OK then
        raise EDeltaValidation.Create(Message);
    end;
  end;

  OwnsTransaction := not AConn.TransactionActive;
  if OwnsTransaction then
    AConn.StartTransaction;

  try
    DS := AConn.NewDataset;
    try
      DS.SQL.Text := TDMSQLBuilder.CreateInsert(AModels[0], AConn.Dialect);

      DS.Prepare;

      for I := 0 to TotalCount - 1 do
      begin
        ToDatasetParams(AModels[I], DS);
        DS.ExecSQL;

        RowsAff := DS.RowsAffected;
        if RowsAff <= 0 then
          RowsAff := 1;

        Result := Result + RowsAff;
        AModels[I].AfterInsert;
      end;
    finally
      DS.Free;
    end;

    if OwnsTransaction then
      AConn.Commit;
  except
    if OwnsTransaction then
    begin
      try
        AConn.Rollback;
      except
      end;
    end;
    raise;
  end;
end;

class function TInsert.BulkInsertObjects(AConn: IDeltaORMEngine;
  AList: TDeltaModelList; ABatchSize: Integer): Integer;
var
  Arr: array of TDeltaModel;
  I: Integer;
begin
  if (AList = nil) or (AList.Records = nil) or (AList.Records.Count = 0) then
    Exit(0);

  SetLength(Arr, AList.Records.Count);
  for I := 0 to AList.Records.Count - 1 do
    Arr[I] := AList.Records[I];

  Result := BulkInsertObjects(AConn, Arr, ABatchSize);
end;

{ TSave }

class function TSave.HasPKValue(AModel: TDeltaModel): Boolean;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: DeltaModel.Fields.TDeltaField;
  NestedObj: TObject;
begin
  Result := False;
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(AModel, PropInfo^.Name);
        if NestedObj is DeltaModel.Fields.TDeltaField then
        begin
          Obj := NestedObj as DeltaModel.Fields.TDeltaField;
          if (dboPrimaryKey in Obj.DBOptions) and (not Obj.IsNull) then
          begin
            if VarIsNumeric(Obj.Value) then
            begin
              if Double(Obj.Value) > 0 then
              begin
                Result := True;
                Exit;
              end;
            end
            else
            if VarIsStr(Obj.Value) then
            begin
              if not VarToStr(Obj.Value).Trim.IsEmpty then
              begin
                Result := True;
                Exit;
              end;
            end
            else
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TSave.SaveObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean;
begin
  AModel.BeforeSave;
  if HasPKValue(AModel) then
    Result := TUpdate.UpdateObject(AConn, AModel)
  else
    Result := TInsert.InsertObject(AConn, AModel);
  if Result then
    AModel.AfterSave;
end;

end.
