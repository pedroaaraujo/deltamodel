unit DeltaModel.ORM.Dialect.SQLite;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TSQLiteSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function FormatBooleanLiteral(AValue: Boolean): string; override;
    function SupportsReturning: Boolean; override;
    function LastInsertIdSQL: string; override;
  end;

  TSQLiteDDLDialect = class(TBaseDDLDialect)
  protected
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; override;
  public
    function CreateTablePrefix: string; override;
    function InlineConstraintsInCreateTable: Boolean; override;
    function SupportsAlterAddForeignKey: Boolean; override;
    function StripNotNullOnAlterAdd: Boolean; override;
    function CreateIndexPrefix(AUnique: Boolean): string; override;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
    function PrimitiveColumnTypeDDL(AKind: TTypeKind;
      const AServerInfo: TDeltaServerInfo): string; override;
  end;

  TSQLiteIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  end;

  TSQLiteDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  end;

implementation

{ TSQLiteSQLDialect }

function TSQLiteSQLDialect.Name: string;
begin
  Result := 'SQLite';
end;

function TSQLiteSQLDialect.FormatBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TSQLiteSQLDialect.SupportsReturning: Boolean;
begin
  Result := True;
end;

function TSQLiteSQLDialect.LastInsertIdSQL: string;
begin
  Result := 'SELECT last_insert_rowid()';
end;

{ TSQLiteDDLDialect }

function TSQLiteDDLDialect.CreateTablePrefix: string;
begin
  Result := 'CREATE TABLE IF NOT EXISTS ';
end;

function TSQLiteDDLDialect.InlineConstraintsInCreateTable: Boolean;
begin
  Result := True;
end;

function TSQLiteDDLDialect.SupportsAlterAddForeignKey: Boolean;
begin
  Result := False;
end;

function TSQLiteDDLDialect.StripNotNullOnAlterAdd: Boolean;
begin
  Result := True;
end;

function TSQLiteDDLDialect.CreateIndexPrefix(AUnique: Boolean): string;
begin
  if AUnique then
    Result := 'CREATE UNIQUE INDEX IF NOT EXISTS '
  else
    Result := 'CREATE INDEX IF NOT EXISTS ';
end;

function TSQLiteDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if (AField is TDFCurrencyNull) or (AField is TDFCurrencyRequired) or
     (AField is TDFDoubleNull) or (AField is TDFDoubleRequired) then
    Result := 'REAL'
  else if (AField is TDFDateTimeNull) or (AField is TDFDateTimeRequired) then
    Result := 'DATETIME'
  else if (AField is TDFBooleanNull) or (AField is TDFBooleanRequired) then
    Result := 'INTEGER'
  else if (AField is TDFStringNull) or (AField is TDFStringRequired) then
  begin
    if (AField is TDFUUIDNull) or (AField is TDFUUIDRequired) then
      Result := 'VARCHAR(36)'
    else
      Result := 'TEXT';
  end
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TSQLiteDDLDialect.PrimitiveColumnTypeDDL(AKind: TTypeKind;
  const AServerInfo: TDeltaServerInfo): string;
begin
  case AKind of
    tkEnumeration, tkBool: Result := 'INTEGER';
    tkFloat:               Result := 'REAL';
    tkInteger:             Result := 'INTEGER';
    tkInt64:               Result := 'BIGINT';
  else
    Result := 'TEXT';
  end;
end;

function TSQLiteDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
begin
  Result := Format('%s INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL', [AField.FieldName]);
end;

{ TSQLiteIntrospector }

function TSQLiteIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT name FROM pragma_index_list(' + QuotedStr(ATable) + ')';
end;

function TSQLiteIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT "from" AS source_column, "table" AS target_table, ' +
    '"to" AS target_column FROM pragma_foreign_key_list(' + QuotedStr(ATable) + ') ' +
    'WHERE id IN (SELECT id FROM pragma_foreign_key_list(' + QuotedStr(ATable) + ') GROUP BY id HAVING COUNT(*)=1)';
end;

{ TSQLiteDialect }

function TSQLiteDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TSQLiteSQLDialect.Create;
end;

function TSQLiteDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TSQLiteDDLDialect.Create;
end;

function TSQLiteDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TSQLiteIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(['sqlite', 'sqlite3'], TSQLiteDialect.Create);

end.
