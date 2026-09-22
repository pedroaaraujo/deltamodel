unit DeltaModel.ORM.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, StrUtils;

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
    class function FormatValue(const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static;
    class function BuildCondition(const AField: string; AOp: TComparisonOp; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AOpStr: string; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildCondition(const AField: string; const AValue: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static; overload;
    class function BuildBetween(const AField: string; const AVal1, AVal2: Variant; ADialect: TDatabaseDialect = ddPostgreSQL): string; static;
    class function BuildIn(const AField: string; const AValues: array of Variant; ADialect: TDatabaseDialect = ddPostgreSQL; ANotIn: Boolean = False): string; static;
    class function BuildNullCondition(const AField: string; AIsNull: Boolean = True): string; static;
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
  // Dialetos com suporte nativo a cláusula RETURNING no INSERT/UPDATE
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

  // Booleano
  if (VType = varBoolean) then
  begin
    if Boolean(AValue) then
      Exit('TRUE')
    else
      Exit('FALSE');
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

  // Data / Hora
  if (VType = varDate) then
  begin
    DT := VarToDateTime(AValue);
    if Frac(DT) = 0 then
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd', DT)))
    else
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', DT)));
  end;

  // Default: string com aspas escapadas
  Result := QuotedStr(VarToStr(AValue));
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

class function TSQLCriteriaHelper.BuildBetween(const AField: string;
  const AVal1, AVal2: Variant; ADialect: TDatabaseDialect): string;
begin
  Result := Format('%s BETWEEN %s AND %s',
    [AField, FormatValue(AVal1, ADialect), FormatValue(AVal2, ADialect)]);
end;

class function TSQLCriteriaHelper.BuildIn(const AField: string;
  const AValues: array of Variant; ADialect: TDatabaseDialect;
  ANotIn: Boolean): string;
var
  I: Integer;
  SL: TStringList;
  OpStr: string;
begin
  if Length(AValues) = 0 then
  begin
    if ANotIn then
      Exit('1 = 1')
    else
      Exit('1 = 0');
  end;

  SL := TStringList.Create;
  try
    for I := Low(AValues) to High(AValues) do
      SL.Add(FormatValue(AValues[I], ADialect));

    if ANotIn then
      OpStr := 'NOT IN'
    else
      OpStr := 'IN';

    Result := Format('%s %s (%s)', [AField, OpStr, string.Join(', ', SL.ToStringArray)]);
  finally
    SL.Free;
  end;
end;

class function TSQLCriteriaHelper.BuildNullCondition(const AField: string;
  AIsNull: Boolean): string;
begin
  if AIsNull then
    Result := Format('%s IS NULL', [AField])
  else
    Result := Format('%s IS NOT NULL', [AField]);
end;

end.
