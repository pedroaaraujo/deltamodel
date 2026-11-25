unit DeltaModel.ORM.Dialect.MySQL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TMySQLSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function DefaultPort: Integer; override;
    function QuoteIdentifier(const AIdentifier: string): string; override;
    function QuoteStringLiteral(const AValue: string): string; override;
    function FormatBooleanLiteral(AValue: Boolean): string; override;
    function LastInsertIdSQL: string; override;
  end;

  TMySQLDDLDialect = class(TBaseDDLDialect)
  protected
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; override;
  public
    function CreateTablePrefix: string; override;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string; override;
  end;

  TMySQLIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  end;

  TMySQLDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  end;

implementation

{ TMySQLSQLDialect }

function TMySQLSQLDialect.Name: string;
begin
  Result := 'MySQL';
end;

function TMySQLSQLDialect.DefaultPort: Integer;
begin
  Result := 3306;
end;

function TMySQLSQLDialect.QuoteIdentifier(const AIdentifier: string): string;
begin
  if AIdentifier.IsEmpty then
    Exit('');
  Result := '`' + AIdentifier + '`';
end;

function TMySQLSQLDialect.QuoteStringLiteral(const AValue: string): string;
begin
  { MySQL/MariaDB interpret backslash as an escape character in string literals. }
  Result := QuotedStr(StringReplace(AValue, '\', '\\', [rfReplaceAll]));
end;

function TMySQLSQLDialect.FormatBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TMySQLSQLDialect.LastInsertIdSQL: string;
begin
  Result := 'SELECT LAST_INSERT_ID()';
end;

{ TMySQLDDLDialect }

function TMySQLDDLDialect.CreateTablePrefix: string;
begin
  Result := 'CREATE TABLE IF NOT EXISTS ';
end;

function TMySQLDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if (AField is TDFDoubleNull) or (AField is TDFDoubleRequired) then
    Result := 'DOUBLE'
  else if (AField is TDFDateTimeNull) or (AField is TDFDateTimeRequired) then
    Result := 'DATETIME'
  else if (AField is TDFBooleanNull) or (AField is TDFBooleanRequired) then
    Result := 'TINYINT(1)'
  else if (AField is TDFTextNull) or (AField is TDFTextRequired) then
    Result := 'LONGTEXT'
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TMySQLDDLDialect.PrimitiveColumnTypeDDL(AKind: TTypeKind;
  const AServerInfo: TDeltaServerInfo): string;
begin
  case AKind of
    tkEnumeration, tkBool: Result := 'TINYINT(1)';
    tkFloat:               Result := 'DOUBLE';
  else
    Result := inherited PrimitiveColumnTypeDDL(AKind, AServerInfo);
  end;
end;

function TMySQLDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  Result := Format('%s %s NOT NULL AUTO_INCREMENT PRIMARY KEY',
    [AField.FieldName, ASQLType]);
end;

{ TMySQLIntrospector }

function TMySQLIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT DISTINCT INDEX_NAME FROM information_schema.STATISTICS ' +
    'WHERE TABLE_SCHEMA=DATABASE() AND TABLE_NAME=' + QuotedStr(ATable);
end;

function TMySQLIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT COLUMN_NAME AS source_column, REFERENCED_TABLE_NAME AS target_table, ' +
    'REFERENCED_COLUMN_NAME AS target_column FROM information_schema.KEY_COLUMN_USAGE k ' +
    'WHERE TABLE_SCHEMA=DATABASE() AND REFERENCED_TABLE_NAME IS NOT NULL ' +
    'AND (SELECT COUNT(*) FROM information_schema.KEY_COLUMN_USAGE k2 ' +
    'WHERE k2.CONSTRAINT_SCHEMA=k.CONSTRAINT_SCHEMA AND k2.TABLE_NAME=k.TABLE_NAME ' +
    'AND k2.CONSTRAINT_NAME=k.CONSTRAINT_NAME)=1 AND TABLE_NAME=' + QuotedStr(ATable);
end;

{ TMySQLDialect }

function TMySQLDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TMySQLSQLDialect.Create;
end;

function TMySQLDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TMySQLDDLDialect.Create;
end;

function TMySQLDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TMySQLIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(
    ['mysql', 'mysql 5.7', 'mysql 8.0', 'mysql5.7', 'mysql8.0', 'mariadb'],
    TMySQLDialect.Create);

end.
