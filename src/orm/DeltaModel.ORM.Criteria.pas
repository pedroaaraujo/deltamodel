unit DeltaModel.ORM.Criteria;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces;

type
  { TSQLCriteriaHelper: Dialect-aware parameterized SQL predicate builder }
  TSQLCriteriaHelper = class
  public
    class function OpToString(AOp: TComparisonOp;
      const ADialect: IDeltaSQLDialect = nil): string; static;
    class function StringToOp(const AOpStr: string): TComparisonOp; static;
    class function JoinTypeToString(AJoinType: TJoinType): string; static;
    class function QuoteString(const AValue: string;
      const ADialect: IDeltaSQLDialect = nil): string; static;
    class function FormatValue(const AValue: Variant;
      const ADialect: IDeltaSQLDialect = nil): string; static;

    class function BuildCondition(const AField: string; AOp: TComparisonOp;
      const AValue: Variant; const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function BuildCondition(const AField, AOpStr: string;
      const AValue: Variant; const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function BuildCondition(const AField: string;
      const AValue: Variant; const ADialect: IDeltaSQLDialect = nil): string; static; overload;

    class function BuildCondition(const AField: string; AOp: TComparisonOp;
      const AValue: Variant; AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function BuildCondition(const AField, AOpStr: string;
      const AValue: Variant; AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function BuildCondition(const AField: string;
      const AValue: Variant; AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;

    class function BuildBetween(const AField: string;
      const AVal1, AVal2: Variant;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function BuildBetween(const AField: string;
      const AVal1, AVal2: Variant; AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;

    class function BuildIn(const AField: string;
      const AValues: array of Variant;
      const ADialect: IDeltaSQLDialect = nil;
      ANotIn: Boolean = False): string; static; overload;
    class function BuildIn(const AField: string;
      const AValues: array of Variant; AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil;
      ANotIn: Boolean = False): string; static; overload;

    class function BuildNullCondition(const AField: string;
      AIsNull: Boolean = True): string; static;

    class function ConditionToSQL(const ACond: TDeltaCondition;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;
    class function ConditionToSQL(const ACond: TDeltaCondition;
      AParams: TDeltaParams;
      const ADialect: IDeltaSQLDialect = nil): string; static; overload;
  end;

implementation

{ TSQLCriteriaHelper }

class function TSQLCriteriaHelper.OpToString(AOp: TComparisonOp;
  const ADialect: IDeltaSQLDialect): string;
begin
  if ADialect <> nil then
    Exit(ADialect.OpToSQL(AOp));

  case AOp of
    opEqual:              Result := '=';
    opNotEqual:           Result := '<>';
    opGreaterThan:        Result := '>';
    opGreaterThanOrEqual: Result := '>=';
    opLessThan:           Result := '<';
    opLessThanOrEqual:    Result := '<=';
    opLike:               Result := 'LIKE';
    opILike:              Result := 'LIKE';
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
  const ADialect: IDeltaSQLDialect): string;
begin
  if ADialect <> nil then
    Result := ADialect.QuoteStringLiteral(AValue)
  else
    Result := QuotedStr(AValue);
end;

class function TSQLCriteriaHelper.FormatValue(const AValue: Variant;
  const ADialect: IDeltaSQLDialect): string;
var
  VType: TVarType;
  InvFS: TFormatSettings;
  DT: TDateTime;
begin
  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Exit('NULL');

  VType := VarType(AValue);

  if VType = varBoolean then
  begin
    if ADialect <> nil then
      Exit(ADialect.FormatBooleanLiteral(Boolean(AValue)));
    if Boolean(AValue) then Exit('TRUE') else Exit('FALSE');
  end;

  if VType in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varQWord] then
    Exit(VarToStr(AValue));

  if VType in [varSingle, varDouble, varCurrency] then
  begin
    InvFS.DecimalSeparator := '.';
    InvFS.ThousandSeparator := #0;
    Exit(FloatToStr(Double(AValue), InvFS));
  end;

  if VType = varDate then
  begin
    DT := VarToDateTime(AValue);
    if Frac(DT) = 0 then
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd', DT)))
    else
      Exit(QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', DT)));
  end;

  Result := QuoteString(VarToStr(AValue), ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  AOp: TComparisonOp; const AValue: Variant;
  const ADialect: IDeltaSQLDialect): string;
begin
  case AOp of
    opIsNull:
      Result := Format('%s IS NULL', [AField]);
    opIsNotNull:
      Result := Format('%s IS NOT NULL', [AField]);
    opEqual:
      if VarIsNull(AValue) or VarIsEmpty(AValue) then
        Result := Format('%s IS NULL', [AField])
      else
        Result := Format('%s = %s', [AField, FormatValue(AValue, ADialect)]);
    opNotEqual:
      if VarIsNull(AValue) or VarIsEmpty(AValue) then
        Result := Format('%s IS NOT NULL', [AField])
      else
        Result := Format('%s <> %s', [AField, FormatValue(AValue, ADialect)]);
  else
    Result := Format('%s %s %s', [AField, OpToString(AOp, ADialect), FormatValue(AValue, ADialect)]);
  end;
end;

class function TSQLCriteriaHelper.BuildCondition(const AField, AOpStr: string;
  const AValue: Variant; const ADialect: IDeltaSQLDialect): string;
begin
  Result := BuildCondition(AField, StringToOp(AOpStr), AValue, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AValue: Variant; const ADialect: IDeltaSQLDialect): string;
begin
  Result := BuildCondition(AField, opEqual, AValue, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  AOp: TComparisonOp; const AValue: Variant; AParams: TDeltaParams;
  const ADialect: IDeltaSQLDialect): string;
begin
  if AParams = nil then
    Exit(BuildCondition(AField, AOp, AValue, ADialect));

  case AOp of
    opIsNull:
      Result := Format('%s IS NULL', [AField]);
    opIsNotNull:
      Result := Format('%s IS NOT NULL', [AField]);
    opEqual:
      if VarIsNull(AValue) or VarIsEmpty(AValue) then
        Result := Format('%s IS NULL', [AField])
      else
        Result := Format('%s = :%s', [AField, AParams.AddAnonymous(AValue)]);
    opNotEqual:
      if VarIsNull(AValue) or VarIsEmpty(AValue) then
        Result := Format('%s IS NOT NULL', [AField])
      else
        Result := Format('%s <> :%s', [AField, AParams.AddAnonymous(AValue)]);
  else
    Result := Format('%s %s :%s', [AField, OpToString(AOp, ADialect), AParams.AddAnonymous(AValue)]);
  end;
end;

class function TSQLCriteriaHelper.BuildCondition(const AField, AOpStr: string;
  const AValue: Variant; AParams: TDeltaParams;
  const ADialect: IDeltaSQLDialect): string;
begin
  Result := BuildCondition(AField, StringToOp(AOpStr), AValue, AParams, ADialect);
end;

class function TSQLCriteriaHelper.BuildCondition(const AField: string;
  const AValue: Variant; AParams: TDeltaParams;
  const ADialect: IDeltaSQLDialect): string;
begin
  Result := BuildCondition(AField, opEqual, AValue, AParams, ADialect);
end;

class function TSQLCriteriaHelper.BuildBetween(const AField: string;
  const AVal1, AVal2: Variant; const ADialect: IDeltaSQLDialect): string;
begin
  Result := Format('%s BETWEEN %s AND %s',
    [AField, FormatValue(AVal1, ADialect), FormatValue(AVal2, ADialect)]);
end;

class function TSQLCriteriaHelper.BuildBetween(const AField: string;
  const AVal1, AVal2: Variant; AParams: TDeltaParams;
  const ADialect: IDeltaSQLDialect): string;
var
  P1, P2: string;
begin
  if AParams = nil then
    Exit(BuildBetween(AField, AVal1, AVal2, ADialect));
  P1 := AParams.AddAnonymous(AVal1);
  P2 := AParams.AddAnonymous(AVal2);
  Result := Format('%s BETWEEN :%s AND :%s', [AField, P1, P2]);
end;

class function TSQLCriteriaHelper.BuildIn(const AField: string;
  const AValues: array of Variant; const ADialect: IDeltaSQLDialect;
  ANotIn: Boolean): string;
var
  I: Integer;
  OpStr: string;
begin
  if Length(AValues) = 0 then
  begin
    if ANotIn then Exit('1 = 1') else Exit('1 = 0');
  end;

  if ANotIn then OpStr := 'NOT IN' else OpStr := 'IN';
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
  const AValues: array of Variant; AParams: TDeltaParams;
  const ADialect: IDeltaSQLDialect; ANotIn: Boolean): string;
var
  I: Integer;
  OpStr: string;
begin
  if AParams = nil then
    Exit(BuildIn(AField, AValues, ADialect, ANotIn));

  if Length(AValues) = 0 then
  begin
    if ANotIn then Exit('1 = 1') else Exit('1 = 0');
  end;

  if ANotIn then OpStr := 'NOT IN' else OpStr := 'IN';
  Result := AField + ' ' + OpStr + ' (';
  for I := Low(AValues) to High(AValues) do
  begin
    if I > Low(AValues) then
      Result := Result + ', ';
    Result := Result + ':' + AParams.AddAnonymous(AValues[I]);
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
  const ADialect: IDeltaSQLDialect): string;
begin
  Result := ConditionToSQL(ACond, nil, ADialect);
end;

class function TSQLCriteriaHelper.ConditionToSQL(const ACond: TDeltaCondition;
  AParams: TDeltaParams; const ADialect: IDeltaSQLDialect): string;
var
  FName: string;
begin
  if ACond.Field = nil then
    raise Exception.Create('TDeltaCondition: Field cannot be nil');

  FName := ACond.Field.FieldName;
  if FName.IsEmpty then
    raise Exception.Create('TDeltaCondition: Field has no FieldName configured');

  case ACond.&Operator of
    doEq:        Result := BuildCondition(FName, opEqual, ACond.Value, AParams, ADialect);
    doNotEq:     Result := BuildCondition(FName, opNotEqual, ACond.Value, AParams, ADialect);
    doGt:        Result := BuildCondition(FName, opGreaterThan, ACond.Value, AParams, ADialect);
    doGte:       Result := BuildCondition(FName, opGreaterThanOrEqual, ACond.Value, AParams, ADialect);
    doLt:        Result := BuildCondition(FName, opLessThan, ACond.Value, AParams, ADialect);
    doLte:       Result := BuildCondition(FName, opLessThanOrEqual, ACond.Value, AParams, ADialect);
    doLike:      Result := BuildCondition(FName, opLike, ACond.Value, AParams, ADialect);
    doBetween:   Result := BuildBetween(FName, ACond.Value, ACond.SecondaryValue, AParams, ADialect);
    doIn:        Result := BuildIn(FName, ACond.Values, AParams, ADialect, False);
    doIsNull:    Result := BuildNullCondition(FName, True);
    doIsNotNull: Result := BuildNullCondition(FName, False);
  else
    Result := BuildCondition(FName, opEqual, ACond.Value, AParams, ADialect);
  end;
end;

end.
