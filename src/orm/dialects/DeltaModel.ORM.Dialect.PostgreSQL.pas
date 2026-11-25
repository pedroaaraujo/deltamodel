unit DeltaModel.ORM.Dialect.PostgreSQL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  DeltaModel.Fields, DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.Registry, DeltaModel.ORM.Dialect.Base;

type
  TPostgreSQLSQLDialect = class(TBaseSQLDialect)
  public
    function Name: string; override;
    function DefaultPort: Integer; override;
    function OpToSQL(AOp: TComparisonOp): string; override;
    function SupportsReturning: Boolean; override;
    function LastInsertIdSQL: string; override;
  end;

  TPostgreSQLDDLDialect = class(TBaseDDLDialect)
  protected
    function FormatAutoIncrementColumn(AField: TDeltaField; const ASQLType: string;
      AIsPK: Boolean; const AServerInfo: TDeltaServerInfo): string; override;
  public
    function CreateTablePrefix: string; override;
    function CreateIndexPrefix(AUnique: Boolean): string; override;
    function ColumnTypeDDL(AField: TDeltaField;
      const AServerInfo: TDeltaServerInfo): string; override;
  end;

  TPostgreSQLIntrospector = class(TBaseSchemaIntrospector)
  protected
    function IndexNamesSQL(const ATable: string): string; override;
    function ForeignKeysSQL(const ATable: string): string; override;
  end;

  TPostgreSQLDialect = class(TBaseDialect)
  protected
    function CreateSQLDialect: IDeltaSQLDialect; override;
    function CreateDDLDialect: IDeltaDDLDialect; override;
    function CreateIntrospector: IDeltaSchemaIntrospector; override;
  end;

implementation

{ TPostgreSQLSQLDialect }

function TPostgreSQLSQLDialect.Name: string;
begin
  Result := 'PostgreSQL';
end;

function TPostgreSQLSQLDialect.DefaultPort: Integer;
begin
  Result := 5432;
end;

function TPostgreSQLSQLDialect.OpToSQL(AOp: TComparisonOp): string;
begin
  if AOp = opILike then
    Result := 'ILIKE'
  else
    Result := inherited OpToSQL(AOp);
end;

function TPostgreSQLSQLDialect.SupportsReturning: Boolean;
begin
  Result := True;
end;

function TPostgreSQLSQLDialect.LastInsertIdSQL: string;
begin
  Result := 'SELECT lastval()';
end;

{ TPostgreSQLDDLDialect }

function TPostgreSQLDDLDialect.CreateTablePrefix: string;
begin
  Result := 'CREATE TABLE IF NOT EXISTS ';
end;

function TPostgreSQLDDLDialect.CreateIndexPrefix(AUnique: Boolean): string;
begin
  if AUnique then
    Result := 'CREATE UNIQUE INDEX IF NOT EXISTS '
  else
    Result := 'CREATE INDEX IF NOT EXISTS ';
end;

function TPostgreSQLDDLDialect.ColumnTypeDDL(AField: TDeltaField;
  const AServerInfo: TDeltaServerInfo): string;
begin
  if (AField is TDFUUIDNull) or (AField is TDFUUIDRequired) then
    Result := 'UUID'
  else
    Result := inherited ColumnTypeDDL(AField, AServerInfo);
end;

function TPostgreSQLDDLDialect.FormatAutoIncrementColumn(AField: TDeltaField;
  const ASQLType: string; AIsPK: Boolean;
  const AServerInfo: TDeltaServerInfo): string;
var
  SerialType: string;
begin
  if (AField is TDFInt64Null) or (AField is TDFInt64Required) then
    SerialType := 'BIGSERIAL'
  else
    SerialType := 'SERIAL';
  Result := Format('%s %s PRIMARY KEY NOT NULL', [AField.FieldName, SerialType]);
end;

{ TPostgreSQLIntrospector }

function TPostgreSQLIntrospector.IndexNamesSQL(const ATable: string): string;
begin
  Result := 'SELECT indexname FROM pg_indexes WHERE schemaname=current_schema() AND tablename=' +
    QuotedStr(ATable);
end;

function TPostgreSQLIntrospector.ForeignKeysSQL(const ATable: string): string;
begin
  Result := 'SELECT a.attname AS source_column, rt.relname AS target_table, ' +
    'ra.attname AS target_column FROM pg_constraint c ' +
    'JOIN pg_class t ON t.oid=c.conrelid ' +
    'JOIN pg_class rt ON rt.oid=c.confrelid ' +
    'JOIN pg_attribute a ON a.attrelid=t.oid AND a.attnum=c.conkey[1] ' +
    'JOIN pg_attribute ra ON ra.attrelid=rt.oid AND ra.attnum=c.confkey[1] ' +
    'WHERE c.contype=''f'' AND array_length(c.conkey,1)=1 ' +
    'AND t.relnamespace=current_schema()::regnamespace AND t.relname=' + QuotedStr(ATable);
end;

{ TPostgreSQLDialect }

function TPostgreSQLDialect.CreateSQLDialect: IDeltaSQLDialect;
begin
  Result := TPostgreSQLSQLDialect.Create;
end;

function TPostgreSQLDialect.CreateDDLDialect: IDeltaDDLDialect;
begin
  Result := TPostgreSQLDDLDialect.Create;
end;

function TPostgreSQLDialect.CreateIntrospector: IDeltaSchemaIntrospector;
begin
  Result := TPostgreSQLIntrospector.Create;
end;

initialization
  TDeltaORMRegistry.RegisterDialect(
    ['postgresql', 'postgres', 'pgsql', 'pg'],
    TPostgreSQLDialect.Create);

end.
