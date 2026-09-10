unit DeltaModel.ORM.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDatabaseDialect = (
    ddFirebird,
    ddPostgreSQL,
    ddSQLite,
    ddMySQL,
    ddMSSQL,
    ddOracle
  );

  { TDatabaseDialectHelper }

  TDatabaseDialectHelper = class
    class function FromString(AStr: string): TDatabaseDialect; static;
    class function ToString(ADialect: TDatabaseDialect): string; reintroduce; static;
    class function QuoteIdentifier(const AIdentifier: string; ADialect: TDatabaseDialect): string; static;
    class function DefaultPort(ADialect: TDatabaseDialect): Integer; static;
    class function SupportsReturning(ADialect: TDatabaseDialect): Boolean; static;
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

end.
