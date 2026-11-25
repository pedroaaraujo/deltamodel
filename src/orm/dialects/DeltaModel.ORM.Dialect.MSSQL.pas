unit DeltaModel.ORM.Dialect.MSSQL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TMSSQLSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function DefaultPort: Integer; override;
    function QuoteIdentifier(const AIdentifier: string): string; override;
    function FormatBooleanLiteral(AValue: Boolean): string; override;
    function SupportsReturning: Boolean; override;
    function LastInsertIdSQL: string; override;
    function BuildLimitOffset(ALimit, AOffset: Integer;
      const AOrderBySQL: string): string; override;
    function BuildInsertReturning(const ATable, AFieldsSQL, AValuesSQL: string;
      const AReturningCols: string = '*'): string; override;
    function BuildUpdateReturning(const ATable, ASetSQL, AWhereSQL: string): string; override;
  end;

  TMSSQLDDLDialect = class(TBaseDDLDialect)
  protected
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; override;
  public
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string; override;
  end;

  TMSSQLIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  end;

  TMSSQLDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  end;

implementation

{ TMSSQLSQLDialect }

function TMSSQLSQLDialect.Name: string;
begin
  Result := 'MSSQL';
end;

function TMSSQLSQLDialect.DefaultPort: Integer;
begin
  Result := 1433;
end;

function TMSSQLSQLDialect.QuoteIdentifier(const AIdentifier: string): string;
begin
  if AIdentifier.IsEmpty then
    Exit('');
  Result := '[' + AIdentifier + ']';
end;

function TMSSQLSQLDialect.FormatBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TMSSQLSQLDialect.SupportsReturning: Boolean;
begin
  Result := True;
end;

function TMSSQLSQLDialect.LastInsertIdSQL: string;
begin
  Result := 'SELECT SCOPE_IDENTITY()';
end;

function TMSSQLSQLDialect.BuildLimitOffset(ALimit, AOffset: Integer;
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

    if Trim(AOrderBySQL).IsEmpty then
      Result := ' ORDER BY (SELECT NULL)';

    Result := Result + Format(' OFFSET %d ROWS', [EffOffset]);
    if ALimit > -1 then
      Result := Result + Format(' FETCH NEXT %d ROWS ONLY', [ALimit]);
  end;
end;

function TMSSQLSQLDialect.BuildInsertReturning(const ATable, AFieldsSQL,
  AValuesSQL, AReturningCols: string): string;
var
  OutputExpr: string;
begin
  if Trim(AReturningCols) = '*' then
    OutputExpr := 'INSERTED.*'
  else
    OutputExpr := 'INSERTED.' + Trim(AReturningCols);

  Result := Format('INSERT INTO %s (%s) OUTPUT %s VALUES (%s)',
    [ATable, AFieldsSQL, OutputExpr, AValuesSQL]);
end;

function TMSSQLSQLDialect.BuildUpdateReturning(const ATable, ASetSQL,
  AWhereSQL: string): string;
begin
  Result := Format('UPDATE %s SET %s OUTPUT INSERTED.*%s',
    [ATable, ASetSQL, AWhereSQL]);
end;

{ TMSSQLDDLDialect }

function TMSSQLDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if (AField is TDFDoubleNull) or (AField is TDFDoubleRequired) then
    Result := 'FLOAT'
  else if (AField is TDFDateTimeNull) or (AField is TDFDateTimeRequired) then
    Result := 'DATETIME2'
  else if (AField is TDFBooleanNull) or (AField is TDFBooleanRequired) then
    Result := 'BIT'
  else if (AField is TDFTextNull) or (AField is TDFTextRequired) then
    Result := 'VARCHAR(MAX)'
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TMSSQLDDLDialect.PrimitiveColumnTypeDDL(AKind: TTypeKind;
  const AServerInfo: TDeltaServerInfo): string;
begin
  case AKind of
    tkEnumeration, tkBool: Result := 'BIT';
    tkFloat:               Result := 'FLOAT';
  else
    Result := inherited PrimitiveColumnTypeDDL(AKind, AServerInfo);
  end;
end;

function TMSSQLDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  Result := Format('%s %s IDENTITY(1,1) PRIMARY KEY NOT NULL',
    [AField.FieldName, ASQLType]);
end;

{ TMSSQLIntrospector }

function TMSSQLIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT name FROM sys.indexes WHERE object_id=OBJECT_ID(' + QuotedStr(ATable) + ')';
end;

function TMSSQLIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT COL_NAME(f.parent_object_id,f.parent_column_id) AS source_column, ' +
    'OBJECT_NAME(f.referenced_object_id) AS target_table, ' +
    'COL_NAME(f.referenced_object_id,f.referenced_column_id) AS target_column ' +
    'FROM sys.foreign_key_columns f WHERE ' +
    '(SELECT COUNT(*) FROM sys.foreign_key_columns f2 WHERE f2.constraint_object_id=f.constraint_object_id)=1 ' +
    'AND f.parent_object_id=OBJECT_ID(' + QuotedStr(ATable) + ')';
end;

{ TMSSQLDialect }

function TMSSQLDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TMSSQLSQLDialect.Create;
end;

function TMSSQLDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TMSSQLDDLDialect.Create;
end;

function TMSSQLDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TMSSQLIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(
    ['mssql', 'mssqlserver', 'sqlserver', 'ms-sql'],
    TMSSQLDialect.Create);

end.
