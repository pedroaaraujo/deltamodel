unit DeltaModel.SQLBuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, DeltaModel, DeltaModel.Fields, StrUtils,
  DeltaModel.ORM.Types;

type

  { TDMSQLBuilder }

  TDMSQLBuilder = class
  private
    FCommand: string;
    FModel: TDeltaModel;
    FFields: TStringList;
    FValues: TStringList;
    FWhereConditions: TStringList;
    FOrderBy: string;
    FLimit: Integer;
    FOffset: Integer;
    FDialect: TDatabaseDialect;
    FGroupBy: string;
    FUseQuotes: Boolean;
    FIsCount: Boolean;
    FTableAlias: string;
    FJoins: TStringList;

    function FieldAndValuesToSQL: string;
    function GetTableName: string;
    function GetTableNameWithAlias: string;
    function JoinsToSQL: string;
    function ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
    function FieldsToSQL: string;
    function ValuesToSQL: string;
    function WhereToSQL: string;
    function QuoteIdentifier(const AIdentifier: string): string;
    function BuildLimitOffset: string;
  public
    constructor Create(AModel: TDeltaModel; ADialect: TDatabaseDialect);
    destructor Destroy; override;

    function Select: TDMSQLBuilder; overload;
    function Select(const AColumns: string): TDMSQLBuilder; overload;
    function Select(const AColumns: array of string): TDMSQLBuilder; overload;
    function Count: TDMSQLBuilder;
    function Insert(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Update(UseNamedParams: Boolean = True): TDMSQLBuilder;
    function Delete: TDMSQLBuilder;
    function TableAlias(const AAlias: string): TDMSQLBuilder;
    function Join(AJoinType: TJoinType; const ATable, AOnCondition: string; const AAlias: string = ''): TDMSQLBuilder; overload;
    function Join(const AJoinClause: string): TDMSQLBuilder; overload;
    function InnerJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TDMSQLBuilder;
    function LeftJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TDMSQLBuilder;
    function RightJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TDMSQLBuilder;
    function FullJoin(const ATable, AOnCondition: string; const AAlias: string = ''): TDMSQLBuilder;
    function Where(const ACondition: string): TDMSQLBuilder; overload;
    function Where(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function Where(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder; overload;
    function Where(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const ACondition: string): TDMSQLBuilder; overload;
    function AndWhere(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder; overload;
    function AndWhere(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const ACondition: string): TDMSQLBuilder; overload;
    function OrWhere(const AField: string; const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder; overload;
    function OrWhere(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder; overload;
    function WhereBetween(const AField: string; const AVal1, AVal2: Variant): TDMSQLBuilder;
    function WhereIn(const AField: string; const AValues: array of Variant): TDMSQLBuilder;
    function WhereNotIn(const AField: string; const AValues: array of Variant): TDMSQLBuilder;
    function WhereNull(const AField: string): TDMSQLBuilder;
    function WhereNotNull(const AField: string): TDMSQLBuilder;
    function OrderBy(const AField: string): TDMSQLBuilder;
    function Limit(const ALimit: Integer): TDMSQLBuilder;
    function Offset(const AOffset: Integer): TDMSQLBuilder;
    function Page(const APageNumber, APageSize: Integer): TDMSQLBuilder;
    function GroupBy(const AField: string): TDMSQLBuilder;
    function UseQuotes(AValue: Boolean): TDMSQLBuilder;
    function Clear: TDMSQLBuilder;
    function Build: string;

    class function WhereClausePK(AModel: TDeltaModel): string;

    class function CreateInsert(AModel: TDeltaModel; ADialect: TDatabaseDialect; UseNamedParams: Boolean = True): string; static;
    class function CreateInsertReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; UseNamedParams: Boolean = True): string; static;
    class function CreateUpdate(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; static;
    class function CreateUpdateReturning(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''; UseNamedParams: Boolean = True): string; static;
    class function CreateDelete(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateSelect(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateCount(AModel: TDeltaModel; ADialect: TDatabaseDialect; const WhereClause: string = ''): string; static;
    class function CreateBulkInsert(AModels: array of TDeltaModel; ADialect: TDatabaseDialect): string; static;
    class function ExtractFieldNames(AModel: TDeltaModel; ADialect: TDatabaseDialect): TStringList; static;
    class function ExtractFieldValues(AModel: TDeltaModel; ADialect: TDatabaseDialect; ASuffix: string = ''): TStringList; static;
  end;

implementation

{ TDMSQLBuilder }

constructor TDMSQLBuilder.Create(AModel: TDeltaModel; ADialect: TDatabaseDialect);
begin
  FModel := AModel;
  FDialect := ADialect;
  FFields := TStringList.Create;
  FFields.Delimiter := ',';
  FFields.StrictDelimiter := True;
  FValues := TStringList.Create;
  FValues.Delimiter := ',';
  FValues.StrictDelimiter := True;
  FWhereConditions := TStringList.Create;
  FJoins := TStringList.Create;
  FTableAlias := '';
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
  FOrderBy := '';
  FUseQuotes := False;
  FIsCount := False;
end;

destructor TDMSQLBuilder.Destroy;
begin
  FFields.Free;
  FValues.Free;
  FWhereConditions.Free;
  FJoins.Free;
  inherited Destroy;
end;

function TDMSQLBuilder.UseQuotes(AValue: Boolean): TDMSQLBuilder;
begin
  FUseQuotes := AValue;
  Result := Self;
end;

function TDMSQLBuilder.QuoteIdentifier(const AIdentifier: string): string;
begin
  if FUseQuotes then
    Result := TDatabaseDialectHelper.QuoteIdentifier(AIdentifier, FDialect)
  else
    Result := AIdentifier;
end;

function TDMSQLBuilder.GetTableName: string;
begin
  Result := QuoteIdentifier(FModel.TableName);
end;

function TDMSQLBuilder.GetTableNameWithAlias: string;
begin
  Result := GetTableName;
  if not FTableAlias.IsEmpty then
    Result := Result + ' ' + QuoteIdentifier(FTableAlias);
end;

function TDMSQLBuilder.JoinsToSQL: string;
begin
  Result := '';
  if FJoins.Count > 0 then
    Result := ' ' + string.Join(' ', FJoins.ToStringArray);
end;

function TDMSQLBuilder.ExtractFieldsAndValues(UseNamedParams: Boolean): Boolean;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  PropValue: Variant;
  Obj: TDeltaField;
  NestedObj: TObject;
  FS: TFormatSettings;
begin
  Result := False;
  FS.DecimalSeparator := '.';
  FS.ThousandSeparator := ',';
  PropCount := GetPropList(FModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(FModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropValue := GetPropValue(FModel, PropInfo^.Name, False);

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(FModel, PropInfo^.Name);
        if NestedObj is TDeltaField then
        begin
          Obj := NestedObj as TDeltaField;

          if Obj.IsVirtual then Continue;

          if FCommand = 'INSERT INTO' then
          begin
            if Obj.IsNull or (dboAutoInc in Obj.DBOptions) or
               (not (dboInsert in Obj.DBOptions) and not (dboUpdate in Obj.DBOptions)) then Continue;
          end
          else
          begin
            if Obj.IsNull or not (dboUpdate in Obj.DBOptions) or (dboPrimaryKey in Obj.DBOptions) then Continue;
          end;

          if not Obj.FieldName.IsEmpty then
            FFields.Add(QuoteIdentifier(Obj.FieldName))
          else
            FFields.Add(QuoteIdentifier(LowerCase(PropInfo^.Name)));

          if UseNamedParams then
          begin
            if not Obj.FieldName.IsEmpty then
              FValues.Add(':' + Obj.FieldName)
            else
              FValues.Add(':' + LowerCase(PropInfo^.Name));
          end
          else
          if Obj.IsNull then
            FValues.Add('NULL')
          else
          if VarIsNumeric(Obj.Value) then
            FValues.Add(Obj.AsString.Replace(',', '', [rfReplaceAll]))
          else
            FValues.Add(Obj.AsString.QuotedString);
        end;
      end
      else
      begin
        if VarIsNull(PropValue) or (PropInfo^.SetProc = nil) then
          Continue;

        FFields.Add(QuoteIdentifier(PropInfo^.Name));

        if UseNamedParams then
          FValues.Add(':' + PropInfo^.Name)
        else
        if VarIsNumeric(PropValue) then
          FValues.Add(StrToFloat(VarToStr(PropValue), FS).ToString().Replace(',', '', [rfReplaceAll]))
        else
        if VarIsStr(PropValue) then
          FValues.Add(QuotedStr(PropValue))
        else
          FValues.Add(VarToStr(PropValue));
      end;
    end;
    Result := True;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function TDMSQLBuilder.FieldsToSQL: string;
begin
  Result := string.Join(', ', FFields.ToStringArray);
end;

function TDMSQLBuilder.ValuesToSQL: string;
begin
  Result := string.Join(', ', FValues.ToStringArray);
end;

function TDMSQLBuilder.WhereToSQL: string;
begin
  Result := '';
  if FWhereConditions.Count > 0 then
    Result := ' WHERE ' + string.Join(' AND ', FWhereConditions.ToStringArray);
end;

function TDMSQLBuilder.BuildLimitOffset: string;
var
  EffOffset, EffLimit: Integer;
begin
  Result := '';
  EffOffset := FOffset;
  EffLimit  := FLimit;

  case FDialect of
    ddPostgreSQL, ddSQLite, ddMySQL:
    begin
      if EffLimit > -1 then
        Result := Result + ' LIMIT ' + IntToStr(EffLimit);
      if EffOffset > -1 then
        Result := Result + ' OFFSET ' + IntToStr(EffOffset);
    end;

    ddFirebird:
    begin
      if EffLimit > -1 then
      begin
        if EffOffset > -1 then
          Result := Format(' ROWS %d TO %d', [EffOffset + 1, EffOffset + EffLimit])
        else
          Result := Format(' ROWS 1 TO %d', [EffLimit]);
      end
      else
      if EffOffset > -1 then
        Result := Format(' ROWS %d TO 2147483647', [EffOffset + 1]);
    end;

    ddMSSQL:
    begin
      if (EffOffset > -1) or (EffLimit > -1) then
      begin
        if EffOffset < 0 then EffOffset := 0;

        if FOrderBy.IsEmpty then
          Result := ' ORDER BY (SELECT NULL)';

        Result := Result + Format(' OFFSET %d ROWS', [EffOffset]);
        if EffLimit > -1 then
          Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [EffLimit]);
      end;
    end;

    ddOracle:
    begin
      if (EffOffset > -1) or (EffLimit > -1) then
      begin
        if EffOffset < 0 then EffOffset := 0;
        Result := Format(' OFFSET %d ROWS', [EffOffset]);
        if EffLimit > -1 then
          Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [EffLimit]);
      end;
    end;
  end;
end;

class function TDMSQLBuilder.WhereClausePK(AModel: TDeltaModel): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: TDeltaField;
  NestedObj: TObject;
  SL: TStringList;
begin
  PropCount := GetPropList(AModel.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit('');

  GetMem(PropList, PropCount * SizeOf(Pointer));
  SL := TStringList.Create;
  try
    GetPropList(AModel.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        NestedObj := GetObjectProp(AModel, PropInfo^.Name);
        if NestedObj is TDeltaField then
        begin
          Obj := NestedObj as TDeltaField;

          if Obj.IsVirtual then Continue;

          if Obj.IsNull or not (dboPrimaryKey in Obj.DBOptions) then
            Continue;

          if VarIsStr(Obj.Value) then
            SL.Add('(' + Obj.FieldName + ' = ' + QuotedStr(Obj.AsString) + ')')
          else
            SL.Add('(' + Obj.FieldName + ' = ' + VarToStr(Obj.Value) + ')');
        end;
      end;
    end;
    Result := string.Join(' AND ', SL.ToStringArray);
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
    SL.Free;
  end;
end;

function TDMSQLBuilder.Select: TDMSQLBuilder;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  Result := Self;
end;

function TDMSQLBuilder.Select(const AColumns: string): TDMSQLBuilder;
var
  Parts: TStringArray;
  I: Integer;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  Parts := AColumns.Split([',']);
  for I := 0 to High(Parts) do
    if not Parts[I].Trim.IsEmpty then
      FFields.Add(QuoteIdentifier(Parts[I].Trim));
  Result := Self;
end;

function TDMSQLBuilder.Select(const AColumns: array of string): TDMSQLBuilder;
var
  I: Integer;
begin
  FCommand := 'SELECT';
  FIsCount := False;
  FFields.Clear;
  for I := Low(AColumns) to High(AColumns) do
    if not AColumns[I].Trim.IsEmpty then
      FFields.Add(QuoteIdentifier(AColumns[I].Trim));
  Result := Self;
end;

function TDMSQLBuilder.Count: TDMSQLBuilder;
begin
  FCommand := 'SELECT';
  FIsCount := True;
  FFields.Clear;
  Result := Self;
end;

function TDMSQLBuilder.Insert(UseNamedParams: Boolean): TDMSQLBuilder;
begin
  FCommand := 'INSERT INTO';
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues(UseNamedParams);
  Result := Self;
end;

function TDMSQLBuilder.Update(UseNamedParams: Boolean): TDMSQLBuilder;
begin
  FCommand := 'UPDATE';
  FFields.Clear;
  FValues.Clear;
  ExtractFieldsAndValues(UseNamedParams);
  Result := Self;
end;

function TDMSQLBuilder.Delete: TDMSQLBuilder;
begin
  FCommand := 'DELETE FROM';
  Result := Self;
end;

function TDMSQLBuilder.TableAlias(const AAlias: string): TDMSQLBuilder;
begin
  FTableAlias := AAlias.Trim;
  Result := Self;
end;

function TDMSQLBuilder.Join(AJoinType: TJoinType; const ATable, AOnCondition: string; const AAlias: string): TDMSQLBuilder;
var
  Clause: string;
begin
  Clause := TSQLCriteriaHelper.JoinTypeToString(AJoinType) + ' ' + QuoteIdentifier(ATable);
  if not AAlias.Trim.IsEmpty then
    Clause := Clause + ' ' + QuoteIdentifier(AAlias.Trim);
  Clause := Clause + ' ON ' + AOnCondition;
  FJoins.Add(Clause);
  Result := Self;
end;

function TDMSQLBuilder.Join(const AJoinClause: string): TDMSQLBuilder;
begin
  if not AJoinClause.Trim.IsEmpty then
    FJoins.Add(AJoinClause.Trim);
  Result := Self;
end;

function TDMSQLBuilder.InnerJoin(const ATable, AOnCondition: string; const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtInner, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.LeftJoin(const ATable, AOnCondition: string; const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtLeft, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.RightJoin(const ATable, AOnCondition: string; const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtRight, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.FullJoin(const ATable, AOnCondition: string; const AAlias: string): TDMSQLBuilder;
begin
  Result := Join(jtFull, ATable, AOnCondition, AAlias);
end;

function TDMSQLBuilder.Where(const ACondition: string): TDMSQLBuilder;
begin
  if not ACondition.IsEmpty then
    FWhereConditions.Add(ACondition);
  Result := Self;
end;

function TDMSQLBuilder.Where(const AField: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AValue, FDialect));
end;

function TDMSQLBuilder.Where(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FDialect));
end;

function TDMSQLBuilder.Where(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FDialect));
end;

function TDMSQLBuilder.AndWhere(const ACondition: string): TDMSQLBuilder;
begin
  Result := Where(ACondition);
end;

function TDMSQLBuilder.AndWhere(const AField: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FDialect));
end;

function TDMSQLBuilder.AndWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FDialect));
end;

function TDMSQLBuilder.AndWhere(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := AndWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FDialect));
end;

function TDMSQLBuilder.OrWhere(const ACondition: string): TDMSQLBuilder;
var
  LastIdx: Integer;
  PrevCond: string;
begin
  if ACondition.IsEmpty then Exit(Self);

  if FWhereConditions.Count > 0 then
  begin
    LastIdx := FWhereConditions.Count - 1;
    PrevCond := FWhereConditions[LastIdx];
    FWhereConditions[LastIdx] := '(' + PrevCond + ' OR ' + ACondition + ')';
  end
  else
    FWhereConditions.Add(ACondition);

  Result := Self;
end;

function TDMSQLBuilder.OrWhere(const AField: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AValue, FDialect));
end;

function TDMSQLBuilder.OrWhere(const AField: string; AOp: TComparisonOp; const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOp, AValue, FDialect));
end;

function TDMSQLBuilder.OrWhere(const AField: string; const AOpStr: string; const AValue: Variant): TDMSQLBuilder;
begin
  Result := OrWhere(TSQLCriteriaHelper.BuildCondition(AField, AOpStr, AValue, FDialect));
end;

function TDMSQLBuilder.WhereBetween(const AField: string; const AVal1, AVal2: Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildBetween(AField, AVal1, AVal2, FDialect));
end;

function TDMSQLBuilder.WhereIn(const AField: string; const AValues: array of Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FDialect, False));
end;

function TDMSQLBuilder.WhereNotIn(const AField: string; const AValues: array of Variant): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildIn(AField, AValues, FDialect, True));
end;

function TDMSQLBuilder.WhereNull(const AField: string): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, True));
end;

function TDMSQLBuilder.WhereNotNull(const AField: string): TDMSQLBuilder;
begin
  Result := Where(TSQLCriteriaHelper.BuildNullCondition(AField, False));
end;

function TDMSQLBuilder.Clear: TDMSQLBuilder;
begin
  FFields.Clear;
  FValues.Clear;
  FWhereConditions.Clear;
  FJoins.Clear;
  FTableAlias := '';
  FLimit := -1;
  FOffset := -1;
  FGroupBy := '';
  FOrderBy := '';
  FIsCount := False;
  Result := Self;
end;

function TDMSQLBuilder.OrderBy(const AField: string): TDMSQLBuilder;
begin
  FOrderBy := AField;
  Result := Self;
end;

function TDMSQLBuilder.Limit(const ALimit: Integer): TDMSQLBuilder;
begin
  FLimit := ALimit;
  Result := Self;
end;

function TDMSQLBuilder.Offset(const AOffset: Integer): TDMSQLBuilder;
begin
  FOffset := AOffset;
  Result := Self;
end;

function TDMSQLBuilder.Page(const APageNumber, APageSize: Integer): TDMSQLBuilder;
begin
  if (APageNumber > 0) and (APageSize > 0) then
  begin
    FLimit  := APageSize;
    FOffset := (APageNumber - 1) * APageSize;
  end;
  Result := Self;
end;

function TDMSQLBuilder.GroupBy(const AField: string): TDMSQLBuilder;
begin
  FGroupBy := QuoteIdentifier(AField);
  Result := Self;
end;

function TDMSQLBuilder.Build: string;
var
  vFields, vOrderBy, vGroupBy, vLimitOffset: string;
begin
  case FCommand of
    'SELECT':
    begin
      if FIsCount then
        vFields := 'COUNT(*)'
      else
        vFields := IfThen(FFields.Count > 0, FieldsToSQL, '*');

      vGroupBy := IfThen(FGroupBy <> '', ' GROUP BY ' + FGroupBy, '');
      vOrderBy := IfThen(FOrderBy <> '', ' ORDER BY ' + FOrderBy, '');
      vLimitOffset := BuildLimitOffset;

      if (FDialect = ddMSSQL) and vLimitOffset.StartsWith(' ORDER BY') and (vOrderBy <> '') then
      begin
        vLimitOffset := Copy(vLimitOffset, Length(' ORDER BY (SELECT NULL)') + 1, Length(vLimitOffset));
      end;

      Result := Format('%s %s FROM %s%s%s%s%s%s',
        [FCommand, vFields, GetTableNameWithAlias, JoinsToSQL, WhereToSQL,
         vGroupBy, vOrderBy, vLimitOffset]);
    end;

    'INSERT INTO':
      Result := Format('%s %s (%s) VALUES (%s)',
        [FCommand, GetTableName, FieldsToSQL, ValuesToSQL]);

    'UPDATE':
      Result := Format('%s %s SET %s%s',
        [FCommand, GetTableName, FieldAndValuesToSQL, WhereToSQL]);

    'DELETE FROM':
      Result := Format('%s %s%s',
        [FCommand, GetTableName, WhereToSQL]);
  else
    raise Exception.Create('Invalid SQL command.');
  end;
end;

function TDMSQLBuilder.FieldAndValuesToSQL: string;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    for I := 0 to Pred(FFields.Count) do
    begin
      SL.Add(Format('%s = %s', [FFields[I], FValues[I]]));
    end;
    Result := string.Join(', ', SL.ToStringArray);
  finally
    SL.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsert(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Insert(UseNamedParams).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateInsertReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; UseNamedParams: Boolean): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Insert(UseNamedParams);
    case ADialect of
      ddMSSQL:
        Result := Format('INSERT INTO %s (%s) OUTPUT INSERTED.* VALUES (%s)',
          [Builder.GetTableName, Builder.FieldsToSQL, Builder.ValuesToSQL]);

      ddPostgreSQL, ddSQLite, ddFirebird:
        Result := Builder.Build + sLineBreak + 'RETURNING *';
    else
      Result := Builder.Build;
    end;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateUpdate(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string; UseNamedParams: Boolean
  ): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder
      .Update(UseNamedParams)
      .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
      .Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateUpdateReturning(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string; UseNamedParams: Boolean
  ): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := IfThen(WhereClause.IsEmpty, WhereClausePK(AModel), WhereClause);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Builder.Update(UseNamedParams).Where(WherePK);
    case ADialect of
      ddMSSQL:
        Result := Format('UPDATE %s SET %s OUTPUT INSERTED.*%s',
          [Builder.GetTableName, Builder.FieldAndValuesToSQL, Builder.WhereToSQL]);

      ddPostgreSQL, ddSQLite, ddFirebird:
        Result := Builder.Build + sLineBreak + 'RETURNING *';
    else
      Result := Builder.Build;
    end;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateDelete(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
  WherePK: string;
begin
  WherePK := WhereClausePK(AModel);
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder
      .Delete
      .Where(IfThen(WhereClause.IsEmpty, WherePK, WhereClause))
      .Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateSelect(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Select.Where(WhereClause).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.CreateCount(AModel: TDeltaModel;
  ADialect: TDatabaseDialect; const WhereClause: string): string;
var
  Builder: TDMSQLBuilder;
begin
  Builder := TDMSQLBuilder.Create(AModel, ADialect);
  try
    Result := Builder.Count.Where(WhereClause).Build;
  finally
    Builder.Free;
  end;
end;

class function TDMSQLBuilder.ExtractFieldNames(AModel: TDeltaModel; ADialect: TDatabaseDialect): TStringList;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  Obj: TDeltaField;
  NestedObj: TObject;
begin
  Result := TStringList.Create;
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
        if NestedObj is TDeltaField then
        begin
          Obj := NestedObj as TDeltaField;

          if Obj.IsVirtual then Continue;

          if not ((dboInsert in Obj.DBOptions) or (dboUpdate in Obj.DBOptions)) then
            Continue;

          if (dboAutoInc in Obj.DBOptions) and (Obj.IsNull or (VarIsNumeric(Obj.Value) and (Double(Obj.Value) = 0))) then
            Continue;

          if not Obj.FieldName.IsEmpty then
            Result.Add(Obj.FieldName)
          else
            Result.Add(PropInfo^.Name);
        end;
      end
      else
      begin
        if (PropInfo^.SetProc = nil) then
          Continue;
        Result.Add(PropInfo^.Name);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDMSQLBuilder.ExtractFieldValues(AModel: TDeltaModel; ADialect: TDatabaseDialect; ASuffix: string): TStringList;
var
  FieldNames: TStringList;
  I: Integer;
begin
  FieldNames := ExtractFieldNames(AModel, ADialect);
  try
    Result := TStringList.Create;
    for I := 0 to FieldNames.Count - 1 do
      Result.Add(':' + FieldNames[I] + ASuffix);
  finally
    FieldNames.Free;
  end;
end;

class function TDMSQLBuilder.CreateBulkInsert(AModels: array of TDeltaModel; ADialect: TDatabaseDialect): string;
var
  FieldNames: TStringList;
  TableName, FieldsSQL: string;
  I, J: Integer;
  ValuesBuilder: TStringBuilder;
begin
  if Length(AModels) = 0 then
    raise Exception.Create('BulkInsert requires at least one model.');

  TableName := AModels[0].TableName;
  FieldNames := ExtractFieldNames(AModels[0], ADialect);
  if FieldNames.Count = 0 then
  begin
    FieldNames.Free;
    raise Exception.Create('BulkInsert requires at least one insertable field.');
  end;

  ValuesBuilder := TStringBuilder.Create;
  try
    // Monta a lista de campos separada por vírgula uma única vez
    FieldsSQL := FieldNames[0];
    for J := 1 to FieldNames.Count - 1 do
      FieldsSQL := FieldsSQL + ', ' + FieldNames[J];

    for I := 0 to High(AModels) do
    begin
      if I > 0 then
      begin
        if ADialect in [ddFirebird, ddOracle] then
          ValuesBuilder.AppendLine
        else
          ValuesBuilder.Append(', ');
      end;

      if ADialect = ddOracle then
        ValuesBuilder.Append('  INTO ').Append(TableName).Append(' (').Append(FieldsSQL).Append(') VALUES (')
      else if ADialect = ddFirebird then
        ValuesBuilder.Append('  INSERT INTO ').Append(TableName).Append(' (').Append(FieldsSQL).Append(') VALUES (')
      else
        ValuesBuilder.Append('(');

      for J := 0 to FieldNames.Count - 1 do
      begin
        if J > 0 then ValuesBuilder.Append(', ');
        ValuesBuilder.Append(':').Append(FieldNames[J]).Append('_').Append(I);
      end;

      if ADialect = ddFirebird then
        ValuesBuilder.Append(');')
      else
        ValuesBuilder.Append(')');
    end;

    case ADialect of
      ddFirebird:
        Result := 'EXECUTE BLOCK AS' + sLineBreak + 'BEGIN' + sLineBreak +
                  ValuesBuilder.ToString + sLineBreak + 'END';
      ddOracle:
        Result := 'INSERT ALL' + sLineBreak +
                  ValuesBuilder.ToString + sLineBreak + 'SELECT 1 FROM DUAL';
      else
        Result := 'INSERT INTO ' + TableName + ' (' + FieldsSQL + ') VALUES ' + ValuesBuilder.ToString;
    end;
  finally
    ValuesBuilder.Free;
    FieldNames.Free;
  end;
end;

end.
