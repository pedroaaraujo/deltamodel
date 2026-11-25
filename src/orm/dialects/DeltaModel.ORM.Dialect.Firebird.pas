unit DeltaModel.ORM.Dialect.Firebird;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TFirebirdSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function DefaultPort: Integer; override;
    function SupportsReturning: Boolean; override;
    function SupportsMultiRowBulkInsert: Boolean; override;
    function UseReturningForAutoInc: Boolean; override;
    function HealthCheckSQL: string; override;
    function BuildLimitOffset(ALimit, AOffset: Integer;
      const AOrderBySQL: string): string; override;
    function BuildBulkInsert(const ATable: string; AFieldNames: TStrings;
      ARowCount: Integer): string; override;
  end;

  TFirebirdDDLDialect = class(TBaseDDLDialect)
  private
    function EffectiveMajor(const AServerInfo: TDeltaServerInfo): Integer;
  public
    function OnRestrictClause: string; override;
    function RequiresCommitAfterInfraDDL: Boolean; override;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
    function FormatColumnDDL(AField: TDeltaField; const ASQLType: string;
      AIsPK, AIsAutoInc: Boolean;
      const AServerInfo: TDeltaServerInfo): string; override;
    procedure ExtraAutoIncrementStatements(const ATableName: string;
      AField: TDeltaField; const AServerInfo: TDeltaServerInfo;
      AStatements: TStrings); override;
  end;

  TFirebirdIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  public
    function DetectServerInfo(AConn: IDeltaConnection): TDeltaServerInfo; override;
  end;

  TFirebirdDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  public
    class function ParseMajorVersion(const AVersion: string): Integer; static;
    class function ObjectName(const APrefix, ATable, AField: string): string; static;
  end;

implementation

{ TFirebirdSQLDialect }

function TFirebirdSQLDialect.Name: string;
begin
  Result := 'Firebird';
end;

function TFirebirdSQLDialect.DefaultPort: Integer;
begin
  Result := 3050;
end;

function TFirebirdSQLDialect.SupportsReturning: Boolean;
begin
  Result := True;
end;

function TFirebirdSQLDialect.SupportsMultiRowBulkInsert: Boolean;
begin
  { SQLDB cannot bind :parameters inside an EXECUTE BLOCK body without typed inputs. }
  Result := False;
end;

function TFirebirdSQLDialect.UseReturningForAutoInc: Boolean;
begin
  Result := True;
end;

function TFirebirdSQLDialect.HealthCheckSQL: string;
begin
  Result := 'SELECT 1 FROM RDB$DATABASE';
end;

function TFirebirdSQLDialect.BuildLimitOffset(ALimit, AOffset: Integer;
  const AOrderBySQL: string): string;
begin
  Result := '';
  if ALimit > -1 then
  begin
    if AOffset > -1 then
      Result := Format(' ROWS %d TO %d', [AOffset + 1, AOffset + ALimit])
    else
      Result := Format(' ROWS 1 TO %d', [ALimit]);
  end
  else if AOffset > -1 then
    Result := Format(' ROWS %d TO 2147483647', [AOffset + 1]);
end;

function TFirebirdSQLDialect.BuildBulkInsert(const ATable: string;
  AFieldNames: TStrings; ARowCount: Integer): string;
var
  FieldsSQL: string;
  SB: TStringBuilder;
  I, J: Integer;
begin
  if (AFieldNames = nil) or (AFieldNames.Count = 0) or (ARowCount <= 0) then
    raise Exception.Create('BuildBulkInsert requires fields and at least one row.');

  FieldsSQL := AFieldNames[0];
  for J := 1 to AFieldNames.Count - 1 do
    FieldsSQL := FieldsSQL + ', ' + AFieldNames[J];

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('EXECUTE BLOCK AS').AppendLine('BEGIN');
    for I := 0 to ARowCount - 1 do
    begin
      SB.Append('  INSERT INTO ').Append(ATable).Append(' (').Append(FieldsSQL).Append(') VALUES (');
      for J := 0 to AFieldNames.Count - 1 do
      begin
        if J > 0 then
          SB.Append(', ');
        SB.Append(':').Append(AFieldNames[J]).Append('_').Append(I);
      end;
      SB.AppendLine(');');
    end;
    SB.Append('END');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TFirebirdDDLDialect }

function TFirebirdDDLDialect.EffectiveMajor(const AServerInfo: TDeltaServerInfo): Integer;
begin
  if AServerInfo.MajorVersion <= 0 then
    Result := 3
  else
    Result := AServerInfo.MajorVersion;
end;

function TFirebirdDDLDialect.OnRestrictClause: string;
begin
  Result := 'NO ACTION';
end;

function TFirebirdDDLDialect.RequiresCommitAfterInfraDDL: Boolean;
begin
  Result := True;
end;

function TFirebirdDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
var
  Size: Integer;
begin
  if (AField is TDFTextNull) or (AField is TDFTextRequired) then
    Result := 'BLOB SUB_TYPE TEXT'
  else if (AField is TDFStringNull) or (AField is TDFStringRequired) then
  begin
    if (AField is TDFUUIDNull) or (AField is TDFUUIDRequired) then
      Exit('VARCHAR(36)');
    Size := StringSize(AField);
    if Size > 8191 then
      Result := 'BLOB SUB_TYPE TEXT'
    else
      Result := Format('VARCHAR(%d)', [Size]);
  end
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TFirebirdDDLDialect.FormatColumnDDL(AField: TDeltaField;
  const ASQLType: string; AIsPK, AIsAutoInc: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if AIsAutoInc and not IsIntegerField(AField) then
    raise Exception.Create('Auto increment requires an integer field: ' + AField.FieldName);

  if AIsAutoInc then
  begin
    Result := AField.FieldName + ' ' + ASQLType;
    if EffectiveMajor(AServerInfo) >= 3 then
      Result := Result + ' GENERATED BY DEFAULT AS IDENTITY';
    if AIsPK then
      Result := Result + ' PRIMARY KEY';
    Result := Result + ' NOT NULL';
    Exit;
  end;

  Result := inherited FormatColumnDDL(AField, ASQLType, AIsPK, AIsAutoInc, AServerInfo);
end;

procedure TFirebirdDDLDialect.ExtraAutoIncrementStatements(
  const ATableName: string; AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo; AStatements: TStrings);
var
  GeneratorName, TriggerName: string;
begin
  if (EffectiveMajor(AServerInfo) >= 3) or not (dboAutoInc in AField.DBOptions) then
    Exit;
  if AStatements = nil then
    raise Exception.Create('Firebird legacy auto increment requires a statements list');

  GeneratorName := TFirebirdDialect.ObjectName('GEN', ATableName, AField.FieldName);
  TriggerName := TFirebirdDialect.ObjectName('BI', ATableName, AField.FieldName);
  AStatements.Add('CREATE GENERATOR ' + GeneratorName + ';');
  { A trigger is one DSQL statement, including its internal semicolon. }
  AStatements.Add('CREATE TRIGGER ' + TriggerName + ' FOR ' + ATableName +
    ' ACTIVE BEFORE INSERT POSITION 0 AS BEGIN IF (NEW.' + AField.FieldName +
    ' IS NULL) THEN NEW.' + AField.FieldName + ' = GEN_ID(' + GeneratorName + ', 1); END');
end;

{ TFirebirdIntrospector }

function TFirebirdIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT TRIM(RDB$INDEX_NAME) FROM RDB$INDICES WHERE RDB$RELATION_NAME=' +
    QuotedStr(UpperCase(ATable));
end;

function TFirebirdIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT TRIM(s.RDB$FIELD_NAME) AS source_column, ' +
    'TRIM(p.RDB$RELATION_NAME) AS target_table, TRIM(ps.RDB$FIELD_NAME) AS target_column ' +
    'FROM RDB$RELATION_CONSTRAINTS c ' +
    'JOIN RDB$REF_CONSTRAINTS r ON r.RDB$CONSTRAINT_NAME=c.RDB$CONSTRAINT_NAME ' +
    'JOIN RDB$RELATION_CONSTRAINTS p ON p.RDB$CONSTRAINT_NAME=r.RDB$CONST_NAME_UQ ' +
    'JOIN RDB$INDEX_SEGMENTS s ON s.RDB$INDEX_NAME=c.RDB$INDEX_NAME ' +
    'JOIN RDB$INDEX_SEGMENTS ps ON ps.RDB$INDEX_NAME=p.RDB$INDEX_NAME AND ps.RDB$FIELD_POSITION=s.RDB$FIELD_POSITION ' +
    'WHERE c.RDB$CONSTRAINT_TYPE=''FOREIGN KEY'' AND ' +
    '(SELECT COUNT(*) FROM RDB$INDEX_SEGMENTS s2 WHERE s2.RDB$INDEX_NAME=c.RDB$INDEX_NAME)=1 AND c.RDB$RELATION_NAME=' +
    QuotedStr(UpperCase(ATable));
end;

function TFirebirdIntrospector.DetectServerInfo(AConn: IDeltaConnection): TDeltaServerInfo;
var
  VerStr: string;
begin
  Result := DefaultServerInfo;
  if AConn = nil then
    Exit;
  VerStr := VarToStr(AConn.ExecuteScalar(
    'SELECT RDB$GET_CONTEXT(''SYSTEM'', ''ENGINE_VERSION'') FROM RDB$DATABASE'));
  Result.VersionString := Trim(VerStr);
  Result.MajorVersion := TFirebirdDialect.ParseMajorVersion(Result.VersionString);
end;

{ TFirebirdDialect }

class function TFirebirdDialect.ParseMajorVersion(const AVersion: string): Integer;
var
  S: string;
  P: Integer;
begin
  S := Trim(AVersion);
  P := Pos('.', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  if not TryStrToInt(S, Result) then
    raise Exception.Create('Cannot identify Firebird server version: ' + AVersion);
  if Result < 2 then
    raise Exception.Create('Unsupported Firebird server version: ' + AVersion);
end;

class function TFirebirdDialect.ObjectName(const APrefix, ATable, AField: string): string;
var
  I: Integer;
  Hash: QWord;
begin
  Result := UpperCase(APrefix + '_' + ATable + '_' + AField);
  if Length(Result) <= 31 then
    Exit;
  Hash := 2166136261;
  for I := 1 to Length(Result) do
    Hash := ((Hash xor Ord(Result[I])) * 16777619) and $FFFFFFFF;
  Result := Copy(Result, 1, 22) + '_' + IntToHex(Hash, 8);
end;

function TFirebirdDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TFirebirdSQLDialect.Create;
end;

function TFirebirdDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TFirebirdDDLDialect.Create;
end;

function TFirebirdDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TFirebirdIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(
    ['firebird', 'fdb', 'interbase', 'ib'],
    TFirebirdDialect.Create);

end.
