unit DeltaModel.ORM.Dialect.Oracle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TOracleSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function DefaultPort: Integer; override;
    function FormatBooleanLiteral(AValue: Boolean): string; override;
    function HealthCheckSQL: string; override;
    function BuildLimitOffset(ALimit, AOffset: Integer;
      const AOrderBySQL: string): string; override;
    function BuildBulkInsert(const ATable: string; AFieldNames: TStrings;
      ARowCount: Integer): string; override;
  end;

  TOracleDDLDialect = class(TBaseDDLDialect)
  protected
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; override;
  public
    function SupportsOnUpdateClause: Boolean; override;
    function SanitizeIdentifier(const AName: string): string; override;
    function MigrationVersionColumnType: string; override;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string; override;
  end;

  TOracleIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  end;

  TOracleDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  end;

implementation

{ TOracleSQLDialect }

function TOracleSQLDialect.Name: string;
begin
  Result := 'Oracle';
end;

function TOracleSQLDialect.DefaultPort: Integer;
begin
  Result := 1521;
end;

function TOracleSQLDialect.FormatBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TOracleSQLDialect.HealthCheckSQL: string;
begin
  Result := 'SELECT 1 FROM DUAL';
end;

function TOracleSQLDialect.BuildLimitOffset(ALimit, AOffset: Integer;
  const AOrderBySQL: string): string;
var
  EffOffset: Integer;
begin
  Result := '';
  if (AOffset > -1) or (ALimit > -1) then
  begin
    EffOffset := AOffset;
    if EffOffset < 0 then
      EffOffset := 0;
    Result := Format(' OFFSET %d ROWS', [EffOffset]);
    if ALimit > -1 then
      Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [ALimit]);
  end;
end;

function TOracleSQLDialect.BuildBulkInsert(const ATable: string;
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
    SB.AppendLine('INSERT ALL');
    for I := 0 to ARowCount - 1 do
    begin
      SB.Append('  INTO ').Append(ATable).Append(' (').Append(FieldsSQL).Append(') VALUES (');
      for J := 0 to AFieldNames.Count - 1 do
      begin
        if J > 0 then
          SB.Append(', ');
        SB.Append(':').Append(AFieldNames[J]).Append('_').Append(I);
      end;
      SB.AppendLine(')');
    end;
    SB.Append('SELECT 1 FROM DUAL');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TOracleDDLDialect }

function TOracleDDLDialect.SupportsOnUpdateClause: Boolean;
begin
  Result := False;
end;

function TOracleDDLDialect.SanitizeIdentifier(const AName: string): string;
begin
  Result := AName;
  if Length(Result) > 30 then
    Result := Copy(Result, 1, 30);
end;

function TOracleDDLDialect.MigrationVersionColumnType: string;
begin
  Result := 'NUMBER(19)';
end;

function TOracleDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
var
  Size: Integer;
begin
  if (AField is TDFIntNull) or (AField is TDFIntRequired) then
    Result := 'NUMBER(10)'
  else if (AField is TDFInt64Null) or (AField is TDFInt64Required) then
    Result := 'NUMBER(19)'
  else if (AField is TDFDoubleNull) or (AField is TDFDoubleRequired) then
    Result := 'BINARY_DOUBLE'
  else if (AField is TDFTimeNull) or (AField is TDFTimeRequired) then
    Result := 'VARCHAR2(8)'
  else if (AField is TDFBooleanNull) or (AField is TDFBooleanRequired) then
    Result := 'NUMBER(1)'
  else if (AField is TDFUUIDNull) or (AField is TDFUUIDRequired) then
    Result := 'VARCHAR2(36)'
  else if (AField is TDFTextNull) or (AField is TDFTextRequired) then
    Result := 'CLOB'
  else if (AField is TDFStringNull) or (AField is TDFStringRequired) then
  begin
    Size := StringSize(AField);
    if Size > 4000 then
      Result := 'CLOB'
    else
      Result := Format('VARCHAR2(%d)', [Size]);
  end
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TOracleDDLDialect.PrimitiveColumnTypeDDL(AKind: TTypeKind;
  const AServerInfo: TDeltaServerInfo): string;
begin
  case AKind of
    tkInteger:             Result := 'NUMBER(10)';
    tkInt64:               Result := 'NUMBER(19)';
    tkEnumeration, tkBool: Result := 'NUMBER(1)';
    tkFloat:               Result := 'BINARY_DOUBLE';
  else
    Result := 'VARCHAR2(255)';
  end;
end;

function TOracleDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  Result := Format('%s %s GENERATED ALWAYS AS IDENTITY PRIMARY KEY NOT NULL',
    [AField.FieldName, ASQLType]);
end;

{ TOracleIntrospector }

function TOracleIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT index_name FROM user_indexes WHERE table_name=' +
    QuotedStr(UpperCase(ATable));
end;

function TOracleIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT col.column_name AS source_column, ref.table_name AS target_table, ' +
    'rcol.column_name AS target_column FROM user_constraints c ' +
    'JOIN user_cons_columns col ON col.constraint_name=c.constraint_name ' +
    'JOIN all_constraints ref ON ref.owner=c.r_owner AND ref.constraint_name=c.r_constraint_name ' +
    'JOIN all_cons_columns rcol ON rcol.owner=ref.owner AND rcol.constraint_name=ref.constraint_name AND rcol.position=col.position ' +
    'WHERE c.constraint_type=''R'' AND ' +
    '(SELECT COUNT(*) FROM user_cons_columns c2 WHERE c2.constraint_name=c.constraint_name)=1 AND c.table_name=' +
    QuotedStr(UpperCase(ATable));
end;

{ TOracleDialect }

function TOracleDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TOracleSQLDialect.Create;
end;

function TOracleDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TOracleDDLDialect.Create;
end;

function TOracleDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TOracleIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(['oracle', 'ora'], TOracleDialect.Create);

end.
