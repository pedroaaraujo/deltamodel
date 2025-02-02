unit DeltaModel.ORM.Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDatabaseDialect = (ddFirebird, ddPostgreSQL, ddSQLite);

  { TDatabaseDialectHelper }

  TDatabaseDialectHelper = class
    class function FromString(AStr: string): TDatabaseDialect;
  end;

implementation


{ TDatabaseDialectHelper }

class function TDatabaseDialectHelper.FromString(AStr: string
  ): TDatabaseDialect;
begin
  case AStr.ToLower of
    'firebird': Result := ddFirebird;
    'postgresql': Result := ddPostgreSQL; // Corrigido de ddFirebird para ddPostgreSQL
    'sqlite', 'sqlite3': Result := ddSQLite;
  else
    raise Exception.CreateFmt('%s isn''t a valid Database Dialect', [AStr]);
  end;
end;

end.

