unit DeltaModel.ORM.DML;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Criteria, DeltaModel.ORM.Registry,
  DeltaModel.SQLBuilder, DeltaModel.ORM.Hydrator, DeltaModel.ORM.Persister;

type
  TDelete = DeltaModel.ORM.Persister.TDelete;
  TUpdate = DeltaModel.ORM.Persister.TUpdate;
  TInsert = DeltaModel.ORM.Persister.TInsert;
  TSave   = DeltaModel.ORM.Persister.TSave;

  { TQuery: Dialect- and driver-agnostic fluent query builder and cursor }
  TQuery = class
  private
    FModelClass: TDeltaModelClass;
    FConn: IDeltaConnection;
    FFilter: string;
    FOrderBy: string;
    FSelectFields: string;
    FTableAlias: string;
    FJoins: TStringList;
    FLimit: Integer;
    FOffset: Integer;
    FParams: TDeltaParams;
    FCursorRS: IDeltaResultSet;
    FCursorBindingMap: TResultBindingMap;
    FCursorSQLBuilder: TDMSQLBuilder;
    FCursorModel: TDeltaModel;
    FCursorOwnsModel: Boolean;
    FIsOpen: Boolean;
    FEOF: Boolean;

    function CurrentSQLDialect: IDeltaSQLDialect;
    procedure RequireConnection;
    procedure RequireModelClass;
    function BuildSQL(AModel: TDeltaModel; AIsCount: Boolean;
      AEffLimit, AEffOffset: Integer; AForDelete: Boolean = False): string;
    procedure PrepareStatement(const AStmt: IDeltaStatement; const ASQL: string);
  public
    constructor Create(const AConn: IDeltaConnection);
    destructor Destroy; override;

    function OrderBy(const AField: string): TQuery; overload;
    function OrderBy(AField: TDeltaField;
      ADirection: TDeltaOrderDirection = odAsc): TQuery; overload;
    function Filter(const Value: string): TQuery;
    function TableAlias(const AAlias: string): TQuery;
    function Join(AJoinType: TJoinType; const ATable, AOnCondition: string;
      const AAlias: string = ''): TQuery; overload;
    function Join(const AJoinClause: string): TQuery; overload;
    function InnerJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TQuery;
    function LeftJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TQuery;
    function RightJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TQuery;
    function FullJoin(const ATable, AOnCondition: string;
      const AAlias: string = ''): TQuery;

    function Where(const Value: string): TQuery; overload;
    function Where(const AField: string; const AValue: Variant): TQuery; overload;
    function Where(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TQuery; overload;
    function Where(const AField, AOpStr: string;
      const AValue: Variant): TQuery; overload;
    function Where(const ACondition: TDeltaCondition): TQuery; overload;

    function AndWhere(const Value: string): TQuery; overload;
    function AndWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function AndWhere(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TQuery; overload;
    function AndWhere(const AField, AOpStr: string;
      const AValue: Variant): TQuery; overload;
    function AndWhere(const ACondition: TDeltaCondition): TQuery; overload;

    function OrWhere(const Value: string): TQuery; overload;
    function OrWhere(const AField: string; const AValue: Variant): TQuery; overload;
    function OrWhere(const AField: string; AOp: TComparisonOp;
      const AValue: Variant): TQuery; overload;
    function OrWhere(const AField, AOpStr: string;
      const AValue: Variant): TQuery; overload;
    function OrWhere(const ACondition: TDeltaCondition): TQuery; overload;

    function WhereBetween(const AField: string;
      const AVal1, AVal2: Variant): TQuery;
    function WhereIn(const AField: string;
      const AValues: array of Variant): TQuery;
    function WhereNotIn(const AField: string;
      const AValues: array of Variant): TQuery;
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
    property Params: TDeltaParams read FParams;
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

implementation

{ TQuery }

constructor TQuery.Create(const AConn: IDeltaConnection);
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
  FParams := TDeltaParams.Create;
  FCursorRS := nil;
  FCursorBindingMap := nil;
  FCursorSQLBuilder := nil;
  FCursorModel := nil;
  FCursorOwnsModel := False;
  FIsOpen := False;
  FEOF := True;
end;

destructor TQuery.Destroy;
begin
  Close;
  FJoins.Free;
  FParams.Free;
  inherited Destroy;
end;

function TQuery.CurrentSQLDialect: IDeltaSQLDialect;
var
  D: IDeltaDialect;
begin
  if (FConn <> nil) and (FConn.Dialect <> nil) then
    Exit(FConn.Dialect.SQL);
  D := TDeltaORMRegistry.FindDialect('postgresql');
  if D <> nil then
    Result := D.SQL
  else
    Result := nil;
end;

procedure TQuery.RequireConnection;
begin
  if FConn = nil then
    raise Exception.Create('No database connection assigned to TQuery.');
end;

procedure TQuery.RequireModelClass;
begin
  if FModelClass = nil then
    raise Exception.Create('Model class not defined for TQuery. Call SetModel first.');
end;

function TQuery.BuildSQL(AModel: TDeltaModel; AIsCount: Boolean;
  AEffLimit, AEffOffset: Integer; AForDelete: Boolean): string;
var
  Builder: TDMSQLBuilder;
  I: Integer;
begin
  Builder := TDMSQLBuilder.Create(AModel, CurrentSQLDialect);
  try
    if AForDelete then
      Exit(Builder.Delete.Where(FFilter).Build);

    Builder.TableAlias(FTableAlias);
    for I := 0 to FJoins.Count - 1 do
      Builder.Join(FJoins[I]);

    if AIsCount then
      Builder.Count
    else if not FSelectFields.IsEmpty then
      Builder.Select(FSelectFields)
    else
      Builder.Select;

    if AEffLimit > -1 then
      Builder.Limit(AEffLimit);
    if AEffOffset > -1 then
      Builder.Offset(AEffOffset);
    if not FFilter.IsEmpty then
      Builder.Where(FFilter);
    if (not AIsCount) and (not FOrderBy.IsEmpty) then
      Builder.OrderBy(FOrderBy);

    Result := Builder.Build;
  finally
    Builder.Free;
  end;
end;

procedure TQuery.PrepareStatement(const AStmt: IDeltaStatement; const ASQL: string);
begin
  AStmt.SQL := ASQL;
  AStmt.Params.Assign(FParams);
end;

function TQuery.Param(const AName: string; const AValue: Variant): TQuery;
begin
  FParams.SetParam(AName, AValue);
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

function TQuery.OrderBy(AField: TDeltaField;
  ADirection: TDeltaOrderDirection): TQuery;
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

function TQuery.Join(AJoinType: TJoinType; const ATable,
  AOnCondition: string; const AAlias: string): TQuery;
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

function TQuery.InnerJoin(const ATable, AOnCondition: string;
  const AAlias: string): TQuery;
begin
  Result := Join(jtInner, ATable, AOnCondition, AAlias);
end;

function TQuery.LeftJoin(const ATable, AOnCondition: string;
  const AAlias: string): TQuery;
begin
  Result := Join(jtLeft, ATable, AOnCondition, AAlias);
end;

function TQuery.RightJoin(const ATable, AOnCondition: string;
  const AAlias: string): TQuery;
begin
  Result := Join(jtRight, ATable, AOnCondition, AAlias);
end;

function TQuery.FullJoin(const ATable, AOnCondition: string;
  const AAlias: string): TQuery;
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
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.Where(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.Where(const AField, AOpStr: string;
  const AValue: Variant): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.Where(const ACondition: TDeltaCondition): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.ConditionToSQL(ACondition, FParams, CurrentSQLDialect));
end;

function TQuery.AndWhere(const Value: string): TQuery;
begin
  if Value.IsEmpty then
    Exit(Self);
  if FFilter.IsEmpty then
    FFilter := Value
  else
    FFilter := '(' + FFilter + ') AND (' + Value + ')';
  Result := Self;
end;

function TQuery.AndWhere(const AField: string; const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.AndWhere(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.AndWhere(const AField, AOpStr: string;
  const AValue: Variant): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.AndWhere(const ACondition: TDeltaCondition): TQuery;
begin
  Result := AndWhere(TSQLCriteriaHelper.ConditionToSQL(ACondition, FParams, CurrentSQLDialect));
end;

function TQuery.OrWhere(const Value: string): TQuery;
begin
  if Value.IsEmpty then
    Exit(Self);
  if FFilter.IsEmpty then
    FFilter := Value
  else
    FFilter := '(' + FFilter + ') OR (' + Value + ')';
  Result := Self;
end;

function TQuery.OrWhere(const AField: string; const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.OrWhere(const AField: string; AOp: TComparisonOp;
  const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.OrWhere(const AField, AOpStr: string;
  const AValue: Variant): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FParams, CurrentSQLDialect));
end;

function TQuery.OrWhere(const ACondition: TDeltaCondition): TQuery;
begin
  Result := OrWhere(TSQLCriteriaHelper.ConditionToSQL(ACondition, FParams, CurrentSQLDialect));
end;

function TQuery.WhereBetween(const AField: string;
  const AVal1, AVal2: Variant): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildBetween(AField, AVal1, AVal2, FParams, CurrentSQLDialect));
end;

function TQuery.WhereIn(const AField: string;
  const AValues: array of Variant): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FParams, CurrentSQLDialect, False));
end;

function TQuery.WhereNotIn(const AField: string;
  const AValues: array of Variant): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FParams, CurrentSQLDialect, True));
end;

function TQuery.WhereNull(const AField: string): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, True));
end;

function TQuery.WhereNotNull(const AField: string): TQuery;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, False));
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

function TQuery.ToSQL: string;
var
  ObjTemp: TDeltaModel;
begin
  RequireModelClass;
  ObjTemp := FModelClass.Create;
  try
    Result := BuildSQL(ObjTemp, False, FLimit, FOffset);
  finally
    ObjTemp.Free;
  end;
end;

function TQuery.First: TDeltaModel;
var
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
begin
  RequireModelClass;
  RequireConnection;

  Result := FModelClass.Create;
  try
    Stmt := FConn.CreateStatement;
    PrepareStatement(Stmt, BuildSQL(Result, False, 1, FOffset));
    RS := Stmt.ExecuteQuery;
    try
      if not RS.Next then
      begin
        FreeAndNil(Result);
        Exit;
      end;
      TDeltaHydrator.FromResultSet(Result, RS);
    finally
      RS.Close;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TQuery.All(ALimit: Integer; AOffset: Integer): TDeltaModelPaginatedList;
var
  Obj, ObjTemp: TDeltaModel;
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
  BindingMap: TResultBindingMap;
  EffLimit, EffOffset: Integer;
begin
  RequireModelClass;
  RequireConnection;

  EffLimit := ALimit;
  if EffLimit = -1 then
    EffLimit := FLimit;
  EffOffset := AOffset;
  if EffOffset = -1 then
    EffOffset := FOffset;

  Result := TDeltaModelPaginatedList.Create;
  Result.DeltaModelClass := FModelClass;

  ObjTemp := FModelClass.Create;
  try
    Stmt := FConn.CreateStatement;
    PrepareStatement(Stmt, BuildSQL(ObjTemp, False, EffLimit, EffOffset));
    RS := Stmt.ExecuteQuery;
    try
      BindingMap := TResultBindingMap.Create(FModelClass, RS);
      try
        while RS.Next do
        begin
          Obj := FModelClass.Create;
          BindingMap.BindRecord(Obj, RS);
          Result.Add(Obj);
        end;
      finally
        BindingMap.Free;
      end;
    finally
      RS.Close;
    end;

    if EffLimit > 0 then
    begin
      Result.page_size := EffLimit;
      if EffOffset >= 0 then
        Result.page := (EffOffset div EffLimit) + 1
      else
        Result.page := 1;
      Result.total_records := Result.Records.Count;
      Result.total_pages := (Result.total_records + EffLimit - 1) div EffLimit;
      if Result.total_pages < 1 then
        Result.total_pages := 1;
    end
    else
    begin
      Result.page := 1;
      Result.page_size := Result.Records.Count;
      if Result.page_size < 1 then
        Result.page_size := 20;
      Result.total_records := Result.Records.Count;
      Result.total_pages := 1;
    end;
  finally
    ObjTemp.Free;
  end;
end;

function TQuery.FindById(const AId: Variant): TDeltaModel;
var
  PKFieldName: string;
begin
  RequireModelClass;
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
  Stmt: IDeltaStatement;
  Val: Variant;
begin
  RequireModelClass;
  RequireConnection;

  Result := 0;
  ObjTemp := FModelClass.Create;
  try
    Stmt := FConn.CreateStatement;
    PrepareStatement(Stmt, BuildSQL(ObjTemp, True, -1, -1));
    Val := Stmt.ExecuteScalar;
    if not (VarIsNull(Val) or VarIsEmpty(Val)) then
      Result := Int64(Val);
  finally
    ObjTemp.Free;
  end;
end;

function TQuery.Exists: Boolean;
begin
  Result := Count > 0;
end;

function TQuery.Delete: Boolean;
var
  ObjTemp: TDeltaModel;
  Stmt: IDeltaStatement;
begin
  RequireModelClass;
  RequireConnection;

  ObjTemp := FModelClass.Create;
  try
    Stmt := FConn.CreateStatement;
    PrepareStatement(Stmt, BuildSQL(ObjTemp, False, -1, -1, True));
    Result := Stmt.Execute > 0;
  finally
    ObjTemp.Free;
  end;
end;

function TQuery.AsJsonString: RawByteString;
var
  Obj: TDeltaModel;
  Stmt: IDeltaStatement;
  RS: IDeltaResultSet;
begin
  RequireModelClass;
  RequireConnection;

  Obj := FModelClass.Create;
  try
    Stmt := FConn.CreateStatement;
    PrepareStatement(Stmt, BuildSQL(Obj, False, FLimit, FOffset));
    RS := Stmt.ExecuteQuery;
    try
      Result := TDeltaHydrator.ResultSetToJsonString(RS);
    finally
      RS.Close;
    end;
  finally
    Obj.Free;
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
  Stmt: IDeltaStatement;
begin
  RequireModelClass;
  RequireConnection;

  if FIsOpen then
    Close;

  if FCursorModel = nil then
  begin
    FCursorModel := FModelClass.Create;
    FCursorOwnsModel := True;
  end;

  Stmt := FConn.CreateStatement;
  PrepareStatement(Stmt, BuildSQL(FCursorModel, False, FLimit, FOffset));
  FCursorRS := Stmt.ExecuteQuery;
  FCursorBindingMap := TResultBindingMap.Create(FModelClass, FCursorRS);
  FIsOpen := True;
  FEOF := FCursorRS.IsEmpty;
  Result := Self;
end;

function TQuery.Next: Boolean;
begin
  if not FIsOpen then
    Open;

  if (FCursorRS = nil) or (not FCursorRS.Next) then
  begin
    FEOF := True;
    Exit(False);
  end;

  FCursorBindingMap.BindRecord(FCursorModel, FCursorRS);
  FEOF := False;
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
  if FCursorRS <> nil then
  begin
    FCursorRS.Close;
    FCursorRS := nil;
  end;
  FreeAndNil(FCursorSQLBuilder);
  if FCursorOwnsModel then
    FreeAndNil(FCursorModel);
  FIsOpen := False;
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

end.
