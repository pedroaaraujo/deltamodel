unit DeltaModel.ORM.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, StrUtils, DB, DeltaModel.Fields;

type
  TDatabaseDialect = (
    ddFirebird,
    ddPostgreSQL,
    ddSQLite,
    ddMySQL,
    ddMSSQL,
    ddOracle
  );

  TComparisonOp = (
    opEqual,              // =
    opNotEqual,           // <>
    opGreaterThan,        // >
    opGreaterThanOrEqual, // >=
    opLessThan,           // <
    opLessThanOrEqual,    // <=
    opLike,               // LIKE
    opILike,              // ILIKE
    opIn,                 // IN
    opNotIn,              // NOT IN
    opIsNull,             // IS NULL
    opIsNotNull,          // IS NOT NULL
    opBetween             // BETWEEN
  );

  TJoinType = (
    jtInner,
    jtLeft,
    jtRight,
    jtFull,
    jtCross
  );

  { TDatabaseDialectHelper }

  TDatabaseDialectHelper = class
    class function FromString(AStr: string): TDatabaseDialect; static;
    class function ToString(ADialect: TDatabaseDialect): string; reintroduce; static;
    class function QuoteIdentifier(const AIdentifier: string; ADialect: TDatabaseDialect): string; static;
    class function DefaultPort(ADialect: TDatabaseDialect): Integer; static;
    class function SupportsReturning(ADialect: TDatabaseDialect): Boolean; static;
  end;

  { TSQLCriteriaHelper }

  TSQLCriteriaHelper = class
  public
    class function OpToString(AOp: TComparisonOp; ADialect: TDatabaseDialect = ddPostgreSQL): string; static;
    class function StringToOp(const AOpStr: string): TComparisonOp; static;
    class function JoinTypeToString(AJoinType: TJoinType): string; static;
    class function QuoteString(const AValue: string; ADialect: TDatabaseDialect = ddPostgreSQL): string; static;
    class function FormatValue(const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static;
    class procedure AssignParamVariant(AParam: TParam; const AValue: Variant); static;
    class function AddParam(AParams: TParams; const AValue: Variant): string; static;
    class function BuildCondition(const AField: string; AOp: TComparisonOp; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AOpStr: string; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; AOp: TComparisonOp; const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AOpStr: string; const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildBetween(const AField: string; const AVal1, AVal2: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildBetween(const AField: string; const AVal1, AVal2: Variant; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildIn(const AField: string; const AValues: array of Variant; ADialect: TDatabaseDialect = ddPostgreSQL; ANotIn: Boolean = False): string; static; overload;
    class function BuildIn(const AField: string; const AValues: array of Variant; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL; ANotIn: Boolean = False): string; static; overload;
    class function BuildNullCondition(const AField: string; AIsNull: Boolean = True): string; static;
    class function ConditionToSQL(const ACond: TDeltaCondition; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function ConditionToSQL(const ACond: TDeltaCondition; AParams: TParams; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
  end;

implementation

{ TDatabaseDialectHelper }

class function TDatabaseDialectHelper.FromString(AStr: string): TDatabaseDialect;
var
  S: string;
begin
  S := LowerCase(Trim(AStr));
  case S of
    'firebird', 'fdb', 'interbase', 'ib':
      Result := ddFirebird;

    'postgresql', 'postgres', 'pgsql', 'pg':
      Result := ddPostgreSQL;

    'sqlite', 'sqlite3':
      Result := ddSQLite;

    'mysql', 'mysql 5.7', 'mysql 8.0', 'mariadb':
      Result := ddMySQL;

    'mssql', 'mssqlserver', 'sqlserver', 'ms-sql':
      Result := ddMSSQL;

    'oracle', 'ora':
      Result := ddOracle;
  else
    raise Exception.CreateFmt('"%s" isn''t a recognized Database Dialect.', [AStr]);
  end;
end;

class function TDatabaseDialectHelper.ToString(ADialect: TDatabaseDialect): string;
begin
  case ADialect of
    ddFirebird:   Result := 'Firebird';
    ddPostgreSQL: Result := 'PostgreSQL';
    ddSQLite:     Result := 'SQLite';
    ddMySQL:      Result := 'MySQL';
    ddMSSQL:      Result := 'MSSQL';
    ddOracle:     Result := 'Oracle';
  else
    Result := 'Unknown';
  end;
end;

class function TDatabaseDialectHelper.QuoteIdentifier(const AIdentifier: string;
  ADialect: TDatabaseDialect): string;
begin
  if AIdentifier.IsEmpty then Exit('');

  case ADialect of
    ddMySQL:
      Result := '`' + AIdentifier + '`';
    ddMSSQL:
      Result := '[' + AIdentifier + ']';
    ddPostgreSQL, ddOracle, ddFirebird, ddSQLite:
      Result := '"' + AIdentifier + '"';
  else
    Result := AIdentifier;
  end;
end;

class function TDatabaseDialectHelper.DefaultPort(ADialect: TDatabaseDialect): Integer;
begin
  case ADialect of
    ddFirebird:   Result := 3050;
    ddPostgreSQL: Result := 5432;
    ddMySQL:      Result := 3306;
    ddMSSQL:      Result := 1433;
    ddOracle:     Result := 1521;
    ddSQLite:     Result := 0;
  else
    Result := 0;
  end;
end;

class function TDatabaseDialectHelper.SupportsReturning(ADialect: TDatabaseDialect): Boolean;
begin
  // Dialects with native RETURNING clause support on INSERT/UPDATE
  Result := ADialect in [ddPostgreSQL, ddSQLite, ddFirebird];
end;

{ TSQLCriteriaHelper }

class function TSQLCriteriaHelper.OpToString(AOp: TComparisonOp;
  ADialect: TDatabaseDialect): string;
begin
  case AOp of
    opEqual:              Result := '=';
    opNotEqual:           Result := '<>';
    opGreaterThan:        Result := '>';
    opGreaterThanOrEqual: Result := '>=';
    opLessThan:           Result := '<';
    opLessThanOrEqual:    Result := '<=';
    opLike:               Result := 'LIKE';
    opILike:
      begin
        if ADialect = ddPostgreSQL then
          Result := 'ILIKE'
        else
          Result := 'LIKE';
      end;
    opIn:                 Result := 'IN';
    opNotIn:              Result := 'NOT IN';
    opIsNull:             Result := 'IS NULL';
    opIsNotNull:          Result := 'IS NOT NULL';
    opBetween:            Result := 'BETWEEN';
  else
    Result := '=';
  end;
end;

class function TSQLCriteriaHelper.StringToOp(const AOpStr: string): TComparisonOp;
var
  S: string;
begin
  S := UpperCase(Trim(AOpStr));
  if (S = '=') or (S = '==') or (S = 'EQ') then
    Result := opEqual
  else if (S = '<>') or (S = '!=') or (S = 'NE') then
    Result := opNotEqual
  else if (S = '>') or (S = 'GT') then
    Result := opGreaterThan
  else if (S = '>=') or (S = 'GTE') then
    Result := opGreaterThanOrEqual
  else if (S = '<') or (S = 'LT') then
    Result := opLessThan
  else if (S = '<=') or (S = 'LTE') then
    Result := opLessThanOrEqual
  else if S = 'LIKE' then
    Result := opLike
  else if S = 'ILIKE' then
    Result := opILike
  else if S = 'IN' then
    Result := opIn
  else if S = 'NOT IN' then
    Result := opNotIn
  else if S = 'IS NULL' then
    Result := opIsNull
  else if S = 'IS NOT NULL' then
    Result := opIsNotNull
  else if S = 'BETWEEN' then
    Result := opBetween
  else
    Result := opEqual;
end;

class function TSQLCriteriaHelper.JoinTypeToString(AJoinType: TJoinType): string;
begin
  case AJoinType of
    jtInner: Result := 'INNER JOIN';
    jtLeft:  Result := 'LEFT JOIN';
    jtRight: Result := 'RIGHT JOIN';
    jtFull:  Result := 'FULL JOIN';
    jtCross: Result := 'CROSS JOIN';
  else
    Result := 'INNER JOIN';
  end;
end;

class function TSQLCriteriaHelper.QuoteString(const AValue: string;
  ADialect: TDatabaseDialect): string;
begin
  { MySQL/MariaDB interpretam barra invertida como escape em literais (modo
    padrão, sem NO_BACKSLASH_ESCAPES). Sem duplicá-la, "\'" encerraria o literal. }
  if ADialect = ddMySQL then
    Result := QuotedStr(StringReplace(AValue, '\', '\\', [rfReplaceAll]))
  else
    Result := QuotedStr(AValue);
end;

class function TSQLCriteriaHelper.FormatValue(const AValue: Variant;
  ADialect: TDatabaseDialect): string;
var
  VType: TVarType;
  InvFS: TFormatSettings;
  DT: TDateTime;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Exit('NULL');

  VType := VarType(AValue);

  // Boolean
  if (VType = varBoolean) then
  begin
    case ADialect of
      ddMSSQL, ddOracle, ddSQLite, ddMySQL:
      begin
        if Boolean(AValue) then Exit('1') else Exit('0');
      end;
    else
      if Boolean(AValue) then Exit('TRUE') else Exit('FALSE');
    end;
  end;

  // Inteiros
  if VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varQWord] then
    Exit(VarToStr(AValue));

  // Ponto flutuante / Moeda
  if VType in [varSingle, varDouble, varCurrency] then
  begin
    InvFS.DecimalSeparator := '.';
    InvFS.ThousandSeparator := #0;
    Exit(FloatToStr(Double(AValue), InvFS));
  end;

  // Date / Time
  if (VType = varDate) then
  begin
    DT := VarToDateTime(AValue);
    if Frac(DT) = 0 then
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd', DT)))
    else
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', DT)));
  end;

  // Default: string with escaped quotes (dialect-aware)
  Result := QuoteString(VarToStr(AValue), ADialect);
end;

class procedure TSQLCriteriaHelper.AssignParamVariant(AParam: TParam; const AValue: Variant);
var
  VType: TVarType;
begin
  if AParam = nil then Exit;
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
  begin
    AParam.Clear;
    Exit;
  end;

  VType := VarType(AValue);
  if VType = varBoolean then
    AParam.AsBoolean := Boolean(AValue)
  else if VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varQWord] then
    AParam.AsLargeInt := AValue
  else if VType in [varSingle, varDouble, varCurrency] then
    AParam.AsFloat := Double(AValue)
  else if VType = varDate then
    AParam.AsDateTime := VarToDateTime(AValue)
  else
    AParam.AsString := VarToStr(AValue);
end;

class function TSQLCriteriaHelper.AddParam(AParams: TParams; const AValue: Variant): string;
var
  Idx: Integer;
  P: TParam;
begin
  Idx := AParams.Count + 1;
  while AParams.FindParam('p' + IntToStr(Idx)) <> nil do
    Inc(Idx);
  Result := 'p' + IntToStr(Idx);
  P := AParams.CreateParam(ftUnknown, Result, ptInput);
  AssignParamVariant(P, AValue);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  AOp: TComparisonOp; const AValue: Variant; ADialect: TDatabaseDialect): string;
begin
  case AOp of
    opIsNull:
      Result := Format('%s IS NULL', [AField]);
    opIsNotNull:
      Result := Format('%s IS NOT NULL', [AField]);
    opEqual:
      begin
        if VarIsNull(AValue) or VarIsEmpty(AValue) then
          Result := Format('%s IS NULL', [AField])
        else
          Result := Format('%s = %s', [AField, FormatValue(AValue, ADialect)]);
      end;
    opNotEqual:
      begin
        if VarIsNull(AValue) or VarIsEmpty(AValue) then
          Result := Format('%s IS NOT NULL', [AField])
        else
          Result := Format('%s <> %s', [AField, FormatValue(AValue, ADialect)]);
      end;
  else
    Result := Format('%s %s %s', [AField, OpToString(AOp, ADialect), FormatValue(AValue, ADialect)]);
  end;
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AOpStr: string; const AValue: Variant; ADialect: TDatabaseDialect): string;
begin
  Result := BuildCondition(AField, StringToOp(AOpStr), AValue, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AValue: Variant; ADialect: TDatabaseDialect): string;
begin
  Result := BuildCondition(AField, opEqual, AValue, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  AOp: TComparisonOp; const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect): string;
begin
  if AParams = nil then
    Exit(BuildCondition(AField, AOp, AValue, ADialect));

  case AOp of
    opIsNull:
      Result := Format('%s IS NULL', [AField]);
    opIsNotNull:
      Result := Format('%s IS NOT NULL', [AField]);
    opEqual:
      begin
        if VarIsNull(AValue) or VarIsEmpty(AValue) then
          Result := Format('%s IS NULL', [AField])
        else
          Result := Format('%s = :%s', [AField, AddParam(AParams, AValue)]);
      end;
    opNotEqual:
      begin
        if VarIsNull(AValue) or VarIsEmpty(AValue) then
          Result := Format('%s IS NOT NULL', [AField])
        else
          Result := Format('%s <> :%s', [AField, AddParam(AParams, AValue)]);
      end;
  else
    Result := Format('%s %s :%s', [AField, OpToString(AOp, ADialect), AddParam(AParams, AValue)]);
  end;
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AOpStr: string; const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect): string;
begin
  Result := BuildCondition(AField, StringToOp(AOpStr), AValue, AParams, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AValue: Variant; AParams: TParams; ADialect: TDatabaseDialect): string;
begin
  Result := BuildCondition(AField, opEqual, AValue, AParams, ADialect);
end;

class function TSQLCriteriaHelper.BuildBetween(const AField: string;
  const AVal1, AVal2: Variant; ADialect: TDatabaseDialect): string;
begin
  Result := Format('%s BETWEEN %s AND %s',
    [AField, FormatValue(AVal1, ADialect), FormatValue(AVal2, ADialect)]);
end;

class function TSQLCriteriaHelper.BuildBetween(const AField: string;
  const AVal1, AVal2: Variant; AParams: TParams; ADialect: TDatabaseDialect): string;
var
  P1, P2: string;
begin
  if AParams = nil then
    Exit(BuildBetween(AField, AVal1, AVal2, ADialect));
  P1 := AddParam(AParams, AVal1);
  P2 := AddParam(AParams, AVal2);
  Result := Format('%s BETWEEN :%s AND :%s', [AField, P1, P2]);
end;

class function TSQLCriteriaHelper.BuildIn(const AField: string;
  const AValues: array of Variant; ADialect: TDatabaseDialect;
  ANotIn: Boolean): string;
var
  I: Integer;
  OpStr: string;
begin
  if Length(AValues) = 0 then
  begin
    if ANotIn then
      Exit('1 = 1')
    else
      Exit('1 = 0');
  end;

  if ANotIn then
    OpStr := 'NOT IN'
  else
    OpStr := 'IN';

  Result := AField + ' ' + OpStr + ' (';
  for I := Low(AValues) to High(AValues) do
  begin
    if I > Low(AValues) then
      Result := Result + ', ';
    Result := Result + FormatValue(AValues[I], ADialect);
  end;
  Result := Result + ')';
end;

class function TSQLCriteriaHelper.BuildIn(const AField: string;
  const AValues: array of Variant; AParams: TParams; ADialect: TDatabaseDialect;
  ANotIn: Boolean): string;
var
  I: Integer;
  OpStr: string;
begin
  if AParams = nil then
    Exit(BuildIn(AField, AValues, ADialect, ANotIn));

  if Length(AValues) = 0 then
  begin
    if ANotIn then
      Exit('1 = 1')
    else
      Exit('1 = 0');
  end;

  if ANotIn then
    OpStr := 'NOT IN'
  else
    OpStr := 'IN';

  Result := AField + ' ' + OpStr + ' (';
  for I := Low(AValues) to High(AValues) do
  begin
    if I > Low(AValues) then
      Result := Result + ', ';
    Result := Result + ':' + AddParam(AParams, AValues[I]);
  end;
  Result := Result + ')';
end;

class function TSQLCriteriaHelper.BuildNullCondition(const AField: string;
  AIsNull: Boolean): string;
begin
  if AIsNull then
    Result := Format('%s IS NULL', [AField])
  else
    Result := Format('%s IS NOT NULL', [AField]);
end;

class function TSQLCriteriaHelper.ConditionToSQL(const ACond: TDeltaCondition;
  ADialect: TDatabaseDialect): string;
begin
  Result := ConditionToSQL(ACond, nil, ADialect);
end;

class function TSQLCriteriaHelper.ConditionToSQL(const ACond: TDeltaCondition;
  AParams: TParams; ADialect: TDatabaseDialect): string;
var
  FName: string;
begin
  if ACond.Field = nil then
    raise Exception.Create('TDeltaCondition: Field cannot be nil');

  FName := ACond.Field.FieldName;
  if FName.IsEmpty then
    raise Exception.Create('TDeltaCondition: Field has no FieldName configured');

  case ACond.&Operator of
    doEq:
      Result := BuildCondition(FName, opEqual, ACond.Value, AParams, ADialect);
    doNotEq:
      Result := BuildCondition(FName, opNotEqual, ACond.Value, AParams, ADialect);
    doGt:
      Result := BuildCondition(FName, opGreaterThan, ACond.Value, AParams, ADialect);
    doGte:
      Result := BuildCondition(FName, opGreaterThanOrEqual, ACond.Value, AParams, ADialect);
    doLt:
      Result := BuildCondition(FName, opLessThan, ACond.Value, AParams, ADialect);
    doLte:
      Result := BuildCondition(FName, opLessThanOrEqual, ACond.Value, AParams, ADialect);
    doLike:
      Result := BuildCondition(FName, opLike, ACond.Value, AParams, ADialect);
    doBetween:
      Result := BuildBetween(FName, ACond.Value, ACond.SecondaryValue, AParams, ADialect);
    doIn:
      Result := BuildIn(FName, ACond.Values, AParams, ADialect, False);
    doIsNull:
      Result := BuildNullCondition(FName, True);
    doIsNotNull:
      Result := BuildNullCondition(FName, False);
  else
    Result := BuildCondition(FName, opEqual, ACond.Value, AParams, ADialect);
  end;
end;

end.
