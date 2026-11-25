unit DeltaModel.ORM.Dialect.Base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces;

type
  { TBaseSQLDialect: Shared ANSI SQL generation defaults }
  TBaseSQLDialect = class(TInterfacedObject, IDeltaSQLDialect)
  public
    function Name: string; virtual; abstract;
    function DefaultPort: Integer; virtual;
    function QuoteIdentifier(const AIdentifier: string): string; virtual;
    function QuoteStringLiteral(const AValue: string): string; virtual;
    function FormatBooleanLiteral(AValue: Boolean): string; virtual;
    function OpToSQL(AOp: TComparisonOp): string; virtual;
    function SupportsReturning: Boolean; virtual;
    function SupportsMultiRowBulkInsert: Boolean; virtual;
    function UseReturningForAutoInc: Boolean; virtual;
    function LastInsertIdSQL: string; virtual;
    function HealthCheckSQL: string; virtual;
    function BuildLimitOffset(ALimit, AOffset: Integer;
      const AOrderBySQL: string): string; virtual;
    function BuildSelect(const AFieldsSQL, ATableWithAlias, AJoinsSQL,
      AWhereSQL, AGroupBySQL, AOrderBySQL: string;
      ALimit, AOffset: Integer): string; virtual;
    function BuildInsertReturning(const ATable, AFieldsSQL, AValuesSQL: string;
      const AReturningCols: string = '*'): string; virtual;
    function BuildUpdateReturning(const ATable, ASetSQL, AWhereSQL: string): string; virtual;
    function BuildBulkInsert(const ATable: string; AFieldNames: TStrings;
      ARowCount: Integer): string; virtual;
  end;

  { TBaseDDLDialect: Shared ANSI DDL generation defaults }
  TBaseDDLDialect = class(TInterfacedObject, IDeltaDDLDialect)
  protected
    function IsIntegerField(AField: TDeltaField): Boolean;
    function StringSize(AField: TDeltaField): Integer;
    function OnDeleteActionToSQL(AAction: TForeignKeyEvent): string; virtual;
    function OnUpdateActionToSQL(AAction: TForeignKeyEvent): string; virtual;
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; virtual;
  public
    function CreateTablePrefix: string; virtual;
    function InlineConstraintsInCreateTable: Boolean; virtual;
    function SupportsAlterAddForeignKey: Boolean; virtual;
    function StripNotNullOnAlterAdd: Boolean; virtual;
    function CreateIndexPrefix(AUnique: Boolean): string; virtual;
    function SupportsOnUpdateClause: Boolean; virtual;
    function OnRestrictClause: string; virtual;
    function RequiresCommitAfterInfraDDL: Boolean; virtual;
    function SanitizeIdentifier(const AName: string): string; virtual;
    function MigrationVersionColumnType: string; virtual;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; virtual;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string; virtual;
    function FormatColumnDDL(AField: TDeltaField; const ASQLType: string;
      AIsPK, AIsAutoInc: Boolean;
      const AServerInfo: TDeltaServerInfo): string; virtual;
    function ForeignKeyDDL(const ATable, AColumn, ARefTable, ARefColumn: string;
      AOnDelete, AOnUpdate: TForeignKeyEvent): string; virtual;
    procedure ExtraAutoIncrementStatements(const ATableName: string;
      AField: TDeltaField; const AServerInfo: TDeltaServerInfo;
      AStatements: TStrings); virtual;
  end;

  { TBaseSchemaIntrospector: Shared catalog introspection helpers }
  TBaseSchemaIntrospector = class(TInterfacedObject, IDeltaSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; virtual;
    function ForeignKeysSQL(const ATable: string): string; virtual;
  public
    function DetectServerInfo(AConn: IDeltaConnection): TDeltaServerInfo; virtual;
    procedure GetIndexNames(AConn: IDeltaConnection; const ATable: string;
      ANames: TStrings); virtual;
    function ForeignKeyExists(AConn: IDeltaConnection; const ATable, AColumn,
      ARefTable, ARefColumn: string): Boolean; virtual;
  end;

  { TBaseDialect: Composes SQL + DDL + Introspector }
  TBaseDialect = class(TInterfacedObject, IDeltaDialect)
  private
    FSQL: IDeltaSQLDialect;
    FDDL: IDeltaDDLDialect;
    FIntrospector: IDeltaSchemaIntrospector;
  protected
    function CreateSQLDialect: IDeltaSQLDialect; virtual; abstract;
    function CreateDDLDialect: IDeltaDDLDialect; virtual; abstract;
    function CreateIntrospector: IDeltaSchemaIntrospector; virtual; abstract;
  public
    constructor Create;
    function Name: string; virtual;
    function SQL: IDeltaSQLDialect;
    function DDL: IDeltaDDLDialect;
    function Introspector: IDeltaSchemaIntrospector;
  end;

implementation

{ TBaseSQLDialect }

function TBaseSQLDialect.DefaultPort: Integer;
begin
  Result := 0;
end;

function TBaseSQLDialect.QuoteIdentifier(const AIdentifier: string): string;
begin
  if AIdentifier.IsEmpty then
    Exit('');
  Result := '"' + AIdentifier + '"';
end;

function TBaseSQLDialect.QuoteStringLiteral(const AValue: string): string;
begin
  Result := QuotedStr(AValue);
end;

function TBaseSQLDialect.FormatBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function TBaseSQLDialect.OpToSQL(AOp: TComparisonOp): string;
begin
  case AOp of
    opEqual:              Result := '=';
    opNotEqual:           Result := '<>';
    opGreaterThan:        Result := '>';
    opGreaterThanOrEqual: Result := '>=';
    opLessThan:           Result := '<';
    opLessThanOrEqual:    Result := '<=';
    opLike, opILike:      Result := 'LIKE';
    opIn:                 Result := 'IN';
    opNotIn:              Result := 'NOT IN';
    opIsNull:             Result := 'IS NULL';
    opIsNotNull:          Result := 'IS NOT NULL';
    opBetween:            Result := 'BETWEEN';
  else
    Result := '=';
  end;
end;

function TBaseSQLDialect.SupportsReturning: Boolean;
begin
  Result := False;
end;

function TBaseSQLDialect.SupportsMultiRowBulkInsert: Boolean;
begin
  Result := True;
end;

function TBaseSQLDialect.UseReturningForAutoInc: Boolean;
begin
  Result := False;
end;

function TBaseSQLDialect.LastInsertIdSQL: string;
begin
  Result := '';
end;

function TBaseSQLDialect.HealthCheckSQL: string;
begin
  Result := 'SELECT 1';
end;

function TBaseSQLDialect.BuildLimitOffset(ALimit, AOffset: Integer;
  const AOrderBySQL: string): string;
begin
  Result := '';
  if ALimit > -1 then
    Result := Result + ' LIMIT ' + IntToStr(ALimit);
  if AOffset > -1 then
    Result := Result + ' OFFSET ' + IntToStr(AOffset);
end;

function TBaseSQLDialect.BuildSelect(const AFieldsSQL, ATableWithAlias,
  AJoinsSQL, AWhereSQL, AGroupBySQL, AOrderBySQL: string;
  ALimit, AOffset: Integer): string;
var
  LimitSQL: string;
begin
  LimitSQL := BuildLimitOffset(ALimit, AOffset, AOrderBySQL);
  Result := Format('SELECT %s FROM %s%s%s%s%s%s',
    [AFieldsSQL, ATableWithAlias, AJoinsSQL, AWhereSQL,
     AGroupBySQL, AOrderBySQL, LimitSQL]);
end;

function TBaseSQLDialect.BuildInsertReturning(const ATable, AFieldsSQL,
  AValuesSQL, AReturningCols: string): string;
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s)',
    [ATable, AFieldsSQL, AValuesSQL]);
  if SupportsReturning then
    Result := Result + sLineBreak + 'RETURNING ' + AReturningCols;
end;

function TBaseSQLDialect.BuildUpdateReturning(const ATable, ASetSQL,
  AWhereSQL: string): string;
begin
  Result := Format('UPDATE %s SET %s%s', [ATable, ASetSQL, AWhereSQL]);
  if SupportsReturning then
    Result := Result + sLineBreak + 'RETURNING *';
end;

function TBaseSQLDialect.BuildBulkInsert(const ATable: string;
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
    SB.Append('INSERT INTO ').Append(ATable).Append(' (').Append(FieldsSQL).Append(') VALUES ');
    for I := 0 to ARowCount - 1 do
    begin
      if I > 0 then
        SB.Append(', ');
      SB.Append('(');
      for J := 0 to AFieldNames.Count - 1 do
      begin
        if J > 0 then
          SB.Append(', ');
        SB.Append(':').Append(AFieldNames[J]).Append('_').Append(I);
      end;
      SB.Append(')');
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TBaseDDLDialect }

function TBaseDDLDialect.IsIntegerField(AField: TDeltaField): Boolean;
begin
  Result := (AField is TDFIntNull) or (AField is TDFIntRequired) or
            (AField is TDFInt64Null) or (AField is TDFInt64Required);
end;

function TBaseDDLDialect.StringSize(AField: TDeltaField): Integer;
begin
  if AField is TDFStringNull then
    Result := TDFStringNull(AField).Size
  else if AField is TDFStringRequired then
    Result := TDFStringRequired(AField).Size
  else
    Result := 255;
end;

function TBaseDDLDialect.OnDeleteActionToSQL(AAction: TForeignKeyEvent): string;
begin
  case AAction of
    fkCascade:  Result := ' ON DELETE CASCADE';
    fkSetNull:  Result := ' ON DELETE SET NULL';
    fkRestrict: Result := ' ON DELETE ' + OnRestrictClause;
    fkNone:     Result := ' ON DELETE NO ACTION';
  else
    Result := '';
  end;
end;

function TBaseDDLDialect.OnUpdateActionToSQL(AAction: TForeignKeyEvent): string;
begin
  if not SupportsOnUpdateClause then
    Exit('');
  case AAction of
    fkCascade:  Result := ' ON UPDATE CASCADE';
    fkSetNull:  Result := ' ON UPDATE SET NULL';
    fkRestrict: Result := ' ON UPDATE ' + OnRestrictClause;
    fkNone:     Result := ' ON UPDATE NO ACTION';
  else
    Result := '';
  end;
end;

function TBaseDDLDialect.CreateTablePrefix: string;
begin
  Result := 'CREATE TABLE ';
end;

function TBaseDDLDialect.InlineConstraintsInCreateTable: Boolean;
begin
  Result := False;
end;

function TBaseDDLDialect.SupportsAlterAddForeignKey: Boolean;
begin
  Result := True;
end;

function TBaseDDLDialect.StripNotNullOnAlterAdd: Boolean;
begin
  Result := False;
end;

function TBaseDDLDialect.CreateIndexPrefix(AUnique: Boolean): string;
begin
  if AUnique then
    Result := 'CREATE UNIQUE INDEX '
  else
    Result := 'CREATE INDEX ';
end;

function TBaseDDLDialect.SupportsOnUpdateClause: Boolean;
begin
  Result := True;
end;

function TBaseDDLDialect.OnRestrictClause: string;
begin
  Result := 'RESTRICT';
end;

function TBaseDDLDialect.RequiresCommitAfterInfraDDL: Boolean;
begin
  Result := False;
end;

function TBaseDDLDialect.SanitizeIdentifier(const AName: string): string;
begin
  Result := AName;
end;

function TBaseDDLDialect.MigrationVersionColumnType: string;
begin
  Result := 'BIGINT';
end;

function TBaseDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
var
  Size: Integer;
begin
  if (AField is TDFIntNull) or (AField is TDFIntRequired) then
    Result := 'INTEGER'
  else if (AField is TDFInt64Null) or (AField is TDFInt64Required) then
    Result := 'BIGINT'
  else if (AField is TDFCurrencyNull) or (AField is TDFCurrencyRequired) then
    Result := 'DECIMAL(18,4)'
  else if (AField is TDFDoubleNull) or (AField is TDFDoubleRequired) then
    Result := 'DOUBLE PRECISION'
  else if (AField is TDFDateNull) or (AField is TDFDateRequired) then
    Result := 'DATE'
  else if (AField is TDFTimeNull) or (AField is TDFTimeRequired) then
    Result := 'TIME'
  else if (AField is TDFDateTimeNull) or (AField is TDFDateTimeRequired) then
    Result := 'TIMESTAMP'
  else if (AField is TDFBooleanNull) or (AField is TDFBooleanRequired) then
    Result := 'BOOLEAN'
  else if (AField is TDFUUIDNull) or (AField is TDFUUIDRequired) then
    Result := 'VARCHAR(36)'
  else if (AField is TDFTextNull) or (AField is TDFTextRequired) then
    Result := 'TEXT'
  else if (AField is TDFStringNull) or (AField is TDFStringRequired) then
  begin
    Size := StringSize(AField);
    if Size > 8191 then
      Result := 'TEXT'
    else
      Result := Format('VARCHAR(%d)', [Size]);
  end
  else
    raise Exception.CreateFmt('Field %s has an invalid datatype for DDL.', [AField.ClassName]);
end;

function TBaseDDLDialect.PrimitiveColumnTypeDDL(AKind: TTypeKind;
  const AServerInfo: TDeltaServerInfo): string;
begin
  case AKind of
    tkInteger:             Result := 'INTEGER';
    tkInt64:               Result := 'BIGINT';
    tkEnumeration, tkBool: Result := 'BOOLEAN';
    tkFloat:               Result := 'DOUBLE PRECISION';
  else
    Result := 'VARCHAR(255)';
  end;
end;

function TBaseDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if AIsPK then
    Result := Format('%s %s PRIMARY KEY NOT NULL', [AField.FieldName, ASQLType])
  else
    Result := Format('%s %s NOT NULL', [AField.FieldName, ASQLType]);
end;

function TBaseDDLDialect.FormatColumnDDL(AField: TDeltaField;
  const ASQLType: string; AIsPK, AIsAutoInc: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if AIsAutoInc and not IsIntegerField(AField) then
    raise Exception.Create('Auto increment requires an integer field: ' + AField.FieldName);

  if AIsPK and AIsAutoInc then
    Result := FormatAutoIncrementColumn(AField, ASQLType, True, AServerInfo)
  else if AIsPK then
    Result := Format('%s %s PRIMARY KEY NOT NULL', [AField.FieldName, ASQLType])
  else if AField is TDeltaFieldNullable then
    Result := Format('%s %s', [AField.FieldName, ASQLType])
  else
    Result := Format('%s %s NOT NULL', [AField.FieldName, ASQLType]);
end;

function TBaseDDLDialect.ForeignKeyDDL(const ATable, AColumn, ARefTable,
  ARefColumn: string; AOnDelete, AOnUpdate: TForeignKeyEvent): string;
var
  FKName, OnDel, OnUpd: string;
  Hash: QWord;
  I: Integer;
begin
  OnDel := OnDeleteActionToSQL(AOnDelete);
  OnUpd := OnUpdateActionToSQL(AOnUpdate);

  if InlineConstraintsInCreateTable then
    Exit(Format('FOREIGN KEY (%s) REFERENCES %s(%s)%s%s',
      [AColumn, ARefTable, ARefColumn, OnDel, OnUpd]));

  FKName := Format('FK_%s_%s_%s', [ATable, AColumn, ARefTable]);
  if Length(FKName) > 30 then
  begin
    Hash := 2166136261;
    for I := 1 to Length(FKName) do
      Hash := ((Hash xor Ord(FKName[I])) * 16777619) and $FFFFFFFF;
    FKName := Copy(FKName, 1, 21) + '_' + IntToHex(Hash, 8);
  end;

  Result := Format(
    'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s)%s%s',
    [ATable, FKName, AColumn, ARefTable, ARefColumn, OnDel, OnUpd]);
end;

procedure TBaseDDLDialect.ExtraAutoIncrementStatements(const ATableName: string;
  AField: TDeltaField; const AServerInfo: TDeltaServerInfo; AStatements: TStrings);
begin
  // Only legacy Firebird (< 3) emits extra generator + trigger statements.
end;

{ TBaseSchemaIntrospector }

function TBaseSchemaIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := '';
end;

function TBaseSchemaIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := '';
end;

function TBaseSchemaIntrospector.DetectServerInfo(AConn: IDeltaConnection): TDeltaServerInfo;
begin
  Result := DefaultServerInfo;
end;

procedure TBaseSchemaIntrospector.GetIndexNames(AConn: IDeltaConnection;
  const ATable: string; ANames: TStrings);
var
  SQLText: string;
  RS: IDeltaResultSet;
begin
  if ANames = nil then
    Exit;
  ANames.Clear;
  SQLText := IndexNamesSQL(ATable);
  if SQLText.IsEmpty or (AConn = nil) then
    Exit;

  RS := AConn.ExecuteQuery(SQLText);
  try
    while RS.Next do
      ANames.Add(RS.AsString(0));
  finally
    RS.Close;
  end;
end;

function TBaseSchemaIntrospector.ForeignKeyExists(AConn: IDeltaConnection;
  const ATable, AColumn, ARefTable, ARefColumn: string): Boolean;
var
  SQLText: string;
  RS: IDeltaResultSet;
begin
  Result := False;
  SQLText := ForeignKeysSQL(ATable);
  if SQLText.IsEmpty or (AConn = nil) then
    Exit;

  RS := AConn.ExecuteQuery(SQLText);
  try
    while RS.Next do
    begin
      if SameText(RS.AsString('source_column'), AColumn) and
         SameText(RS.AsString('target_table'), ARefTable) and
         SameText(RS.AsString('target_column'), ARefColumn) then
        Exit(True);
    end;
  finally
    RS.Close;
  end;
end;

{ TBaseDialect }

constructor TBaseDialect.Create;
begin
  inherited Create;
  FSQL := CreateSQLDialect;
  FDDL := CreateDDLDialect;
  FIntrospector := CreateIntrospector;
end;

function TBaseDialect.Name: string;
begin
  Result := FSQL.Name;
end;

function TBaseDialect.SQL: IDeltaSQLDialect;
begin
  Result := FSQL;
end;

function TBaseDialect.DDL: IDeltaDDLDialect;
begin
  Result := FDDL;
end;

function TBaseDialect.Introspector: IDeltaSchemaIntrospector;
begin
  Result := FIntrospector;
end;

end.
