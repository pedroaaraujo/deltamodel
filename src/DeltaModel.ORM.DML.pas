unit DeltaModel.ORM.DML;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Variants, TypInfo, fpjson,
  DeltaModel, DeltaValidator, DeltaModel.Fields, DeltaModel.ORM.Types,
  DeltaModel.SQLBuilder, DeltaModel.ORM.Interfaces, DeltaModel.DataSetConverter,
  DeltaModel.Metadata;

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
    FCursorDS: TSQLQuery;
    FCursorBindingMap: TColumnBindingMap;
    FCursorSQLBuilder: TDMSQLBuilder;
    FCursorModel: TDeltaModel;
    FCursorOwnsModel: Boolean;
    FIsOpen: Boolean;
    FBOF: Boolean;
    FEOF: Boolean;
    procedure BindParams(AQuery: TSQLQuery);
  public
    constructor Create(AConn: IDeltaORMEngine);
    destructor Destroy; override;

    function OrderBy(const AField: string): TQuery; overload;
    function OrderBy(AField: TDeltaField; ADirection: TDeltaOrderDirection = odAsc): TQuery; overload;
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
    function Where(const ACondition: TDeltaCondition): TQuery; overload;
    function AndWhere(const Value: string): TQuery; overload;
    function AndWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function AndWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery; overload;
    function AndWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery; overload;
    function AndWhere(const ACondition: TDeltaCondition): TQuery; overload;
    function OrWhere(const Value: string): TQuery; overload;
    function OrWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function OrWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TQuery; overload;
    function OrWhere(const AField: string; const AOpStr: string; const AValue: Variant): TQuery; overload;
    function OrWhere(const ACondition: TDeltaCondition): TQuery; overload;
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

    function ToSQL: string;
    property WhereClause: string read FFilter;
    property FilterText: string read FFilter;
    property OrderByClause: string read FOrderBy;

    function First: TDeltaModel;
    function All(ALimit: Integer = -1; AOffset: Integer = -1): TDeltaModelPaginatedList;
    function FindById(const AId: Variant): TDeltaModel;
    function Count: Int64;
    function Exists: Boolean;
    function Delete: Boolean;

    function AsJsonString: RawByteString;

    // Reusable cursor
    function Open: TQuery;
    function Next: Boolean;
    function EOF: Boolean;
    function Current: TDeltaModel;
    procedure Close;
    procedure SetCursorModel(AModel: TDeltaModel; AOwnsModel: Boolean = False);
    property IsOpen: Boolean read FIsOpen;
    property CurrentModel: TDeltaModel read FCursorModel;

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
    class function PatchObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static; overload;
    class function PatchObject(AConn: IDeltaORMEngine; AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel; static; overload;
  end;

  { TInsert }

  TInsert = class
  public
    class function InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel): Boolean; static; overload;
    class function InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel; static; overload;
    class function BulkInsertObjects(AConn: IDeltaORMEngine; AModels: array of TDeltaModel; ABatchSize: Integer = 500): Integer; static; overload;
    class function BulkInsertObjects(AConn: IDeltaORMEngine; AList: TDeltaModelList; ABatchSize: Integer = 500): Integer; static; overload;
  end;

  { TSave - Chooses between Insert and Update based on Primary Key }

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
  FCursorDS := nil;
  FCursorBindingMap := nil;
  FCursorSQLBuilder := nil;
  FCursorModel := nil;
  FCursorOwnsModel := False;
  FIsOpen := False;
  FBOF := False;
  FEOF := True;
end;

destructor TQuery.Destroy;
begin
  Close;
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

function TQuery.OrderBy(AField: TDeltaField; ADirection: TDeltaOrderDirection): TQuery;
var
  S: string;
begin
  if AField = nil then
    raise Exception.Create('TQuery.OrderBy: AField cannot be nil');
  S := AField.FieldName;
  if S.IsEmpty then
    raise Exception.Create('TQuery.OrderBy: AField has no FieldName configured');
  if ADirection = odDesc then
    S := S + ' DESC'
  else
    S := S + ' ASC';
  Result := OrderBy(S);
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
  if FFilter.IsEmpty then
    Result := Filter(Value)
  else
    Result := AndWhere(Value);
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

function TQuery.Where(const ACondition: TDeltaCondition): TQuery;
var
  Dialect: TDatabaseDialect;
begin
  if FConn <> nil then
    Dialect := FConn.Dialect
  else
    Dialect := ddPostgreSQL;
  Result := Where(TSQLCriteriaHelper.ConditionToSQL(ACondition, Dialect));
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

function TQuery.AndWhere(const ACondition: TDeltaCondition): TQuery;
var
  Dialect: TDatabaseDialect;
begin
  if FConn <> nil then
    Dialect := FConn.Dialect
  else
    Dialect := ddPostgreSQL;
  Result := AndWhere(TSQLCriteriaHelper.ConditionToSQL(ACondition, Dialect));
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

function TQuery.OrWhere(const ACondition: TDeltaCondition): TQuery;
var
  Dialect: TDatabaseDialect;
begin
  if FConn <> nil then
    Dialect := FConn.Dialect
  else
    Dialect := ddPostgreSQL;
  Result := OrWhere(TSQLCriteriaHelper.ConditionToSQL(ACondition, Dialect));
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
    try
      if DS.IsEmpty then
      begin
        Result.Free;
        Result := nil;
        Exit;
      end;

      FromDataSet(Result, DS);
    finally
      DS.Close;
    end;
  finally
    SQLBuilder.Free;
    DS.Free;
  end;
end;

function TQuery.All(ALimit: Integer; AOffset: Integer
  ): TDeltaModelPaginatedList;
var
  Obj, ObjTemp: TDeltaModel;
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
  BindingMap: TColumnBindingMap;
  I: Integer;
  EffLimit, EffOffset: Integer;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');

  EffLimit := ALimit;
  if EffLimit = -1 then EffLimit := FLimit;
  EffOffset := AOffset;
  if EffOffset = -1 then EffOffset := FOffset;

  Result := TDeltaModelPaginatedList.Create;
  Result.DeltaModelClass := FModelClass;

  ObjTemp := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(ObjTemp, FConn.Dialect);
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
      .Limit(EffLimit)
      .Offset(EffOffset)
      .Where(FFilter)
      .OrderBy(FOrderBy)
      .Build;
    BindParams(DS);

    DS.UniDirectional := False;
    DS.PacketRecords := -1;
    DS.Open;

    if DS.IsEmpty then
    begin
      DS.Close;
      Exit;
    end;

    BindingMap := TColumnBindingMap.Create(FModelClass, DS);
    try
      while not DS.EOF do
      begin
        Obj := FModelClass.Create;
        BindingMap.BindRecord(Obj);
        Result.Add(Obj);
        DS.Next;
      end;
    finally
      BindingMap.Free;
    end;
    DS.Close;

    if EffLimit > 0 then
    begin
      Result.page_size := EffLimit;
      if EffOffset >= 0 then
        Result.page := (EffOffset div EffLimit) + 1
      else
        Result.page := 1;
      Result.total_records := Result.Records.Count;
      Result.total_pages := (Result.total_records + EffLimit - 1) div EffLimit;
      if Result.total_pages < 1 then Result.total_pages := 1;
    end
    else
    begin
      Result.page := 1;
      Result.page_size := Result.Records.Count;
      if Result.page_size < 1 then Result.page_size := 20;
      Result.total_records := Result.Records.Count;
      Result.total_pages := 1;
    end;
  finally
    if DS.Active then
      DS.Close;
    SQLBuilder.Free;
    ObjTemp.Free;
    DS.Free;
  end;
end;

function TQuery.FindById(const AId: Variant): TDeltaModel;
var
  PKFieldName: string;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery.');

  PKFieldName := TRTTICache.GetModelMetadata(FModelClass).PKFieldName;
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
    try
      if not DS.IsEmpty then
        Result := DS.Fields[0].AsLargeInt;
    finally
      DS.Close;
    end;
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

function FastJSONEscape(const S: string): string;
const
  HexDigits: array[0..15] of Char = '0123456789abcdef';
var
  P, PEnd: PChar;
  Res: PChar;
  Len: Integer;
begin
  if S = '' then Exit('""');

  SetLength(Result, Length(S) * 6 + 2);
  Res := PChar(Result);
  Res^ := '"'; Inc(Res);

  P := PChar(S);
  PEnd := P + Length(S);

  while P < PEnd do
  begin
    case P^ of
      '"', '\':
        begin
          Res^ := '\'; Inc(Res);
          Res^ := P^;  Inc(Res);
        end;
      #8:  begin Res^ := '\'; Inc(Res); Res^ := 'b'; Inc(Res); end;
      #9:  begin Res^ := '\'; Inc(Res); Res^ := 't'; Inc(Res); end;
      #10: begin Res^ := '\'; Inc(Res); Res^ := 'n'; Inc(Res); end;
      #12: begin Res^ := '\'; Inc(Res); Res^ := 'f'; Inc(Res); end;
      #13: begin Res^ := '\'; Inc(Res); Res^ := 'r'; Inc(Res); end;
    else
      if Byte(P^) >= 32 then
      begin
        Res^ := P^;
        Inc(Res);
      end
      else
      begin
        Res^ := '\'; Inc(Res);
        Res^ := 'u'; Inc(Res);
        Res^ := '0'; Inc(Res);
        Res^ := '0'; Inc(Res);
        Res^ := HexDigits[Byte(P^) shr 4]; Inc(Res);
        Res^ := HexDigits[Byte(P^) and $F]; Inc(Res);
      end;
    end;
    Inc(P);
  end;

  Res^ := '"'; Inc(Res);
  Len := (PtrUInt(Res) - PtrUInt(PChar(Result)));
  SetLength(Result, Len);
end;

function TQuery.AsJsonString: RawByteString;
type
  TFieldCache = record
    Field: TField;
    DataType: TFieldType;
    JSONKey: string;
  end;
var
  DS: TSQLQuery;
  SQLBuilder: TDMSQLBuilder;
  SB: TStringBuilder;
  I, FCnt: Integer;
  StrValue: string;
  FieldsCache: array of TFieldCache;
  Obj: TDeltaModel;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery.');

  Obj := FModelClass.Create;
  DS := FConn.NewDataset;
  SQLBuilder := TDMSQLBuilder.Create(Obj, FConn.Dialect);
  SB := TStringBuilder.Create;
  try
    SQLBuilder.TableAlias(FTableAlias);
    for I := 0 to FJoins.Count - 1 do
      SQLBuilder.Join(FJoins[I]);

    if not FSelectFields.IsEmpty then
      SQLBuilder.Select(FSelectFields)
    else
      SQLBuilder.Select;

    DS.SQL.Text := SQLBuilder
      .Limit(FLimit)
      .Offset(FOffset)
      .Where(FFilter)
      .OrderBy(FOrderBy)
      .Build;

    BindParams(DS);

    DS.PacketRecords := -1;
    DS.UniDirectional := False;
    DS.Open;

    FCnt := DS.FieldCount;
    SetLength(FieldsCache, FCnt);
    for I := 0 to FCnt - 1 do
    begin
      FieldsCache[I].Field := DS.Fields[I];
      FieldsCache[I].DataType := DS.Fields[I].DataType;

      if I = 0 then
        FieldsCache[I].JSONKey := '"' + DS.Fields[I].FieldName + '":'
      else
        FieldsCache[I].JSONKey := ',"' + DS.Fields[I].FieldName + '":';
    end;

    SB.Capacity := 1024 * 1024;

    SB.Append('[');
    while not DS.EOF do
    begin
      if SB.Length > 1 then SB.Append(',');

      SB.Append('{');

      for I := 0 to FCnt - 1 do
      begin
        SB.Append(FieldsCache[I].JSONKey);

        if FieldsCache[I].Field.IsNull then
          SB.Append('null')
        else
        begin
          case FieldsCache[I].DataType of
            ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
              SB.Append(FieldsCache[I].Field.AsLargeInt);

            ftFloat, ftCurrency, ftBCD, ftFMTBcd:
              begin
                StrValue := FloatToStrF(FieldsCache[I].Field.AsFloat, ffFixed, 18, 4, DefaultFormatSettings);
                StrValue := StringReplace(StrValue, ',', '.', [rfReplaceAll]);
                SB.Append(StrValue);
              end;

            ftBoolean:
              if FieldsCache[I].Field.AsBoolean then SB.Append('true') else SB.Append('false');

          else
            SB.Append(FastJSONEscape(FieldsCache[I].Field.AsString));
          end;
        end;
      end;

      SB.Append('}');
      DS.Next;
    end;
    SB.Append(']');

    Result := SB.ToString;
  finally
    DS.Free;
    SQLBuilder.Free;
    SB.Free;
    Obj.Free;
  end;
end;

function TQuery.ToSQL: string;
var
  SQLBuilder: TDMSQLBuilder;
  ObjTemp: TDeltaModel;
  I: Integer;
  Dialect: TDatabaseDialect;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');

  if FConn <> nil then
    Dialect := FConn.Dialect
  else
    Dialect := ddPostgreSQL;

  ObjTemp := FModelClass.Create;
  try
    SQLBuilder := TDMSQLBuilder.Create(ObjTemp, Dialect);
    try
      SQLBuilder.TableAlias(FTableAlias);
      for I := 0 to FJoins.Count - 1 do
        SQLBuilder.Join(FJoins[I]);

      if not FSelectFields.IsEmpty then
        SQLBuilder.Select(FSelectFields)
      else
        SQLBuilder.Select;

      if FLimit > -1 then SQLBuilder.Limit(FLimit);
      if FOffset > -1 then SQLBuilder.Offset(FOffset);
      if not FOrderBy.IsEmpty then SQLBuilder.OrderBy(FOrderBy);
      if not FFilter.IsEmpty then SQLBuilder.Where(FFilter);

      Result := SQLBuilder.Build;
    finally
      SQLBuilder.Free;
    end;
  finally
    ObjTemp.Free;
  end;
end;

procedure TQuery.SetCursorModel(AModel: TDeltaModel; AOwnsModel: Boolean);
begin
  if FCursorOwnsModel and (FCursorModel <> nil) and (FCursorModel <> AModel) then
    FreeAndNil(FCursorModel);
  FCursorModel := AModel;
  FCursorOwnsModel := AOwnsModel;
end;

function TQuery.Open: TQuery;
var
  Dialect: TDatabaseDialect;
  I: Integer;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');

  if FIsOpen then
    Close;

  if FConn <> nil then
    Dialect := FConn.Dialect
  else
    Dialect := ddPostgreSQL;

  if FCursorModel = nil then
  begin
    FCursorModel := FModelClass.Create;
    FCursorOwnsModel := True;
  end;

  FCursorSQLBuilder := TDMSQLBuilder.Create(FCursorModel, Dialect);
  FCursorSQLBuilder.TableAlias(FTableAlias);
  for I := 0 to FJoins.Count - 1 do
    FCursorSQLBuilder.Join(FJoins[I]);

  if not FSelectFields.IsEmpty then
    FCursorSQLBuilder.Select(FSelectFields)
  else
    FCursorSQLBuilder.Select;

  FCursorDS := FConn.NewDataset;
  FCursorDS.SQL.Text :=
    FCursorSQLBuilder
    .Limit(FLimit)
    .Offset(FOffset)
    .Where(FFilter)
    .OrderBy(FOrderBy)
    .Build;
  BindParams(FCursorDS);

  FCursorDS.UniDirectional := False;
  FCursorDS.PacketRecords := -1;
  FCursorDS.Open;

  FCursorBindingMap := TColumnBindingMap.Create(FModelClass, FCursorDS);
  FIsOpen := True;
  FBOF := True;
  FEOF := FCursorDS.EOF;
  Result := Self;
end;

function TQuery.Next: Boolean;
begin
  if not FIsOpen then
    Open;

  if FCursorDS = nil then
  begin
    FEOF := True;
    Exit(False);
  end;

  if FBOF then
  begin
    FBOF := False;
    if FCursorDS.EOF then
    begin
      FEOF := True;
      Exit(False);
    end;
  end
  else
  begin
    FCursorDS.Next;
    if FCursorDS.EOF then
    begin
      FEOF := True;
      Exit(False);
    end;
  end;

  FCursorBindingMap.BindRecord(FCursorModel);
  Result := True;
end;

function TQuery.EOF: Boolean;
begin
  Result := FEOF;
end;

function TQuery.Current: TDeltaModel;
begin
  Result := FCursorModel;
end;

procedure TQuery.Close;
begin
  FreeAndNil(FCursorBindingMap);
  if Assigned(FCursorDS) then
  begin
    if FCursorDS.Active then
      FCursorDS.Close;
    FreeAndNil(FCursorDS);
  end;
  FreeAndNil(FCursorSQLBuilder);
  if FCursorOwnsModel then
    FreeAndNil(FCursorModel);
  FIsOpen := False;
  FBOF := False;
  FEOF := True;
end;

procedure TQuery.Clear;
begin
  Close;
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

class function TUpdate.PatchObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel): Boolean;
var
  DS: TSQLQuery;
  SQL: string;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit(False);

  SQL := TDMSQLBuilder.CreatePartialUpdate(
    AModel,
    AConn.Dialect,
    TDMSQLBuilder.WhereClausePK(AModel)
  );
  if SQL.IsEmpty then
    Exit(False);

  AModel.BeforeUpdate;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := SQL;
    ToDatasetParams(AModel, DS);

    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
    if Result then
    begin
      AModel.ClearAllDirty;
      AModel.AfterUpdate;
    end;
  finally
    DS.Free;
  end;
end;

class function TUpdate.PatchObject(AConn: IDeltaORMEngine;
  AModel: TDeltaModel; Return: TDeltaModelClass): TDeltaModel;
var
  DS: TSQLQuery;
  SQL: string;
begin
  if (AModel = nil) or (not AModel.HasDirtyFields) then
    Exit(nil);

  SQL := TDMSQLBuilder.CreatePartialUpdateReturning(
    AModel,
    AConn.Dialect,
    TDMSQLBuilder.WhereClausePK(AModel)
  );
  if SQL.IsEmpty then
    Exit(nil);

  AModel.BeforeUpdate;
  AModel.Validate;
  with AModel.Validator.Validate do
  begin
    if not OK then
      raise EDeltaValidation.Create(Message);
  end;

  DS := AConn.NewDataset;
  try
    DS.SQL.Text := SQL;
    ToDatasetParams(AModel, DS);

    DS.Open;
    if DS.IsEmpty then
      Exit(nil);

    Result := Return.Create;
    FromDataSet(Result, DS);
    DS.Close;
    if Assigned(Result) then
    begin
      AModel.ClearAllDirty;
      AModel.AfterUpdate;
    end;
  finally
    DS.Free;
  end;
end;

{ TInsert }

class function TInsert.InsertObject(AConn: IDeltaORMEngine; AModel: TDeltaModel
  ): Boolean;
var
  DS: TSQLQuery;
  Meta: TModelMetadata;
  I: Integer;
  Obj: DeltaModel.Fields.TDeltaField;
  NestedObj: TObject;
  LastId: Variant;
  ReturningFields: string;
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

    if AConn.Dialect = ddFirebird then
    begin
      ReturningFields := '';
      for I := 0 to AModel.FieldList.Count - 1 do
      begin
        Obj := AModel.FieldList[I];
        if Obj.IsVirtual or not (dboAutoInc in Obj.DBOptions) then Continue;
        if ReturningFields <> '' then ReturningFields := ReturningFields + ', ';
        ReturningFields := ReturningFields + Obj.FieldName;
      end;
      if ReturningFields <> '' then
      begin
        { Read the values from this INSERT, never the shared generator counter.
          Explicit column names work on both legacy and identity Firebird. }
        DS.SQL.Add('RETURNING ' + ReturningFields);
        ToDatasetParams(AModel, DS);
        DS.Open;
        Result := not DS.EOF;
        if Result then
          for I := 0 to AModel.FieldList.Count - 1 do
          begin
            Obj := AModel.FieldList[I];
            if not Obj.IsVirtual and (dboAutoInc in Obj.DBOptions) then
              Obj.Value := DS.FieldByName(Obj.FieldName).Value;
          end;
        DS.Close;
        if Result then AModel.AfterInsert;
        Exit;
      end;
    end;
    DS.ExecSQL;
    Result := DS.RowsAffected > 0;
    if Result then
    begin
      Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
      for I := 0 to Meta.FieldCount - 1 do
      begin
        if Meta.Fields[I].IsAutoInc then
        begin
          NestedObj := GetObjectProp(AModel, Meta.Fields[I].PropInfo);
          if NestedObj is TDeltaField then
          begin
            Obj := TDeltaField(NestedObj);
            if Obj.IsNull or (Obj.AsLargeInt = 0) then
            begin
              case AConn.Dialect of
                ddSQLite:
                  LastId := AConn.ExecuteScalar('SELECT last_insert_rowid()');
                ddPostgreSQL:
                  LastId := AConn.ExecuteScalar('SELECT lastval()');
                ddMySQL:
                  LastId := AConn.ExecuteScalar('SELECT LAST_INSERT_ID()');
                ddMSSQL:
                  LastId := AConn.ExecuteScalar('SELECT SCOPE_IDENTITY()');
              end;
              if not VarIsNull(LastId) then
                Obj.AsLargeInt := LastId;
              Break;
            end;
          end;
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
  I, J, TotalCount, BatchEnd, CurrentBatchSize: Integer;
  BatchModels: array of TDeltaModel;
  OwnsTransaction: Boolean;
begin
  Result := 0;
  TotalCount := Length(AModels);
  if TotalCount = 0 then Exit;

  if ABatchSize <= 0 then
    ABatchSize := 500;

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
    I := 0;
    while I < TotalCount do
    begin
      BatchEnd := I + ABatchSize - 1;
      if BatchEnd >= TotalCount then
        BatchEnd := TotalCount - 1;
      CurrentBatchSize := BatchEnd - I + 1;

      SetLength(BatchModels, CurrentBatchSize);
      for J := 0 to CurrentBatchSize - 1 do
        BatchModels[J] := AModels[I + J];

      DS := AConn.NewDataset;
      try
        if AConn.Dialect = ddFirebird then
        begin
          { SQLDB binds :parameters as DSQL placeholders. They cannot appear
            inside an EXECUTE BLOCK body without typed input declarations.
            Reuse an ordinary parameterized INSERT within the same transaction;
            retain bulk semantics (no generated IDs assigned to the models). }
          for J := 0 to CurrentBatchSize - 1 do
          begin
            if DS.SQL.Text <> TDMSQLBuilder.CreateInsert(BatchModels[J], AConn.Dialect) then
              DS.SQL.Text := TDMSQLBuilder.CreateInsert(BatchModels[J], AConn.Dialect);
            ToDatasetParams(BatchModels[J], DS);
            DS.ExecSQL;
          end;
        end
        else
        begin
          DS.SQL.Text := TDMSQLBuilder.CreateBulkInsert(BatchModels, AConn.Dialect);
          for J := 0 to CurrentBatchSize - 1 do
            ToDatasetParamsIndexed(BatchModels[J], DS, J);
          DS.ExecSQL;
        end;
        Result := Result + CurrentBatchSize;

        for J := 0 to CurrentBatchSize - 1 do
          BatchModels[J].AfterInsert;
      finally
        DS.Free;
      end;

      I := BatchEnd + 1;
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
  Meta: TModelMetadata;
  NestedObj: TObject;
  Obj: DeltaModel.Fields.TDeltaField;
begin
  Result := False;
  if not Assigned(AModel) then Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  if Meta.PKPropInfo = nil then Exit;

  NestedObj := GetObjectProp(AModel, Meta.PKPropInfo);
  if NestedObj is DeltaModel.Fields.TDeltaField then
  begin
    Obj := DeltaModel.Fields.TDeltaField(NestedObj);
    if not Obj.IsNull then
    begin
      if VarIsNumeric(Obj.Value) then
        Result := (Double(Obj.Value) > 0)
      else if VarIsStr(Obj.Value) then
        Result := not VarToStr(Obj.Value).Trim.IsEmpty
      else
        Result := True;
    end;
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
