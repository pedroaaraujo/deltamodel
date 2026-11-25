program test_autoincrement_integration;
{$mode ObjFPC}{$H+}
uses
  cthreads, cwstring, Classes, SysUtils, Variants, SQLDB,
  sqlite3conn, pqconnection, ibconnection, mysql57conn, mysql57dyn,
  DeltaModel, DeltaModel.Fields,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry,
  DeltaModel.ORM.Dialects, DeltaModel.ORM.Dialect.Firebird,
  DeltaModel.ORM.Driver.SQLDB, DeltaModel.ORM.Persister,
  DeltaModel.ORM.DML, DeltaModel.ORM.Schema;

type
  TConnectorAccess = class(TSQLConnector)
    procedure ConfigureMariaDB;
  end;

  TItem = class(TDeltaModel)
  private
    FId: TDFInt64Null;
    FCode: TDFIntNull;
  public
    procedure AfterConstruction; override;
  published
    property Id: TDFInt64Null read FId write FId;
    property Code: TDFIntNull read FCode write FCode;
  end;

var
  AlterCase: Boolean;
  Checks: Integer;
  Conn: IDeltaConnection;

procedure TItem.AfterConstruction;
begin
  inherited;
  if AlterCase then TableName := 'dm_auto_alter' else TableName := 'dm_auto_fresh';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Code.FieldName := 'code';
end;

procedure TConnectorAccess.ConfigureMariaDB;
begin
  CheckProxy;
  TMySQL57Connection(Proxy).SkipLibraryVersionCheck := True;
end;

procedure Check(Value: Boolean; const Msg: string);
begin
  if not Value then raise Exception.Create(Msg);
  Inc(Checks); WriteLn('[PASS] ', Msg);
end;

function CountItems(const AConn: IDeltaConnection): Int64;
var
  Q: TQuery;
begin
  Q := TQuery.Create(AConn);
  try
    Q.SetModel(TItem);
    Result := Q.Count;
  finally
    Q.Free;
  end;
end;

procedure RunAutoIncrement(Adding: Boolean);
var
  Schema: TDeltaORMSchema;
  Item: TItem;
  Table, GeneratorName: string;
  FirstId, SecondId, Version: Int64;
  Major: Integer;
begin
  AlterCase := Adding;
  Item := TItem.Create;
  Table := Item.TableName;
  Schema := nil;
  try
    if Adding then
    begin
      Conn.ExecuteDirect('CREATE TABLE ' + Table + ' (code INTEGER)');
      Conn.Commit;
    end;
    Schema := TDeltaORMSchema.Create(Conn);
    Schema.RegisterModel(TItem);
    Schema.PrepareDB(False);
    if SameText(Conn.Dialect.Name, 'Firebird') then
    begin
      Major := TFirebirdDialect.ParseMajorVersion(VarToStr(Conn.ExecuteScalar(
        'SELECT RDB$GET_CONTEXT(''SYSTEM'', ''ENGINE_VERSION'') FROM RDB$DATABASE')));
      Check((Pos('IDENTITY', Schema.SQL.Text) > 0) = (Major >= 3), 'plan reflects real Firebird server version');
    end;
    Schema.PrepareDB(True);
    Version := Schema.DatabaseVersion;
    Item.Code.Value := 101;
    Check(TDeltaPersister.Insert(Conn, Item), 'ORM insert succeeds');
    FirstId := Item.Id.Value;
    Check((not Item.Id.IsNull) and (FirstId > 0), 'ORM returns generated Int64 key');
    Conn.Commit;
    Item.Id.Clear;
    Item.Code.Value := 102;
    Check(TDeltaPersister.Insert(Conn, Item), 'second ORM insert succeeds');
    SecondId := Item.Id.Value;
    Check(SecondId > FirstId, 'generated IDs increase');
    Conn.Commit;
    Conn.ExecuteDirect('INSERT INTO ' + Table + ' (id,code) VALUES (9000,103)');
    Conn.Commit;
    Check(Int64(Conn.ExecuteScalar('SELECT id FROM ' + Table + ' WHERE code=103')) = 9000, 'explicit key preserved');
    if SameText(Conn.Dialect.Name, 'Firebird') then
    begin
      if Major >= 3 then
      begin
        GeneratorName := Trim(VarToStr(Conn.ExecuteScalar(
          'SELECT RDB$GENERATOR_NAME FROM RDB$RELATION_FIELDS WHERE RDB$RELATION_NAME=' +
          QuotedStr(UpperCase(Table)) + ' AND RDB$FIELD_NAME=''ID''')));
        Check(GeneratorName <> '', 'identity has an internal generator');
        Check(Int64(Conn.ExecuteScalar('SELECT COUNT(*) FROM RDB$TRIGGERS WHERE RDB$RELATION_NAME=' +
          QuotedStr(UpperCase(Table)) + ' AND COALESCE(RDB$SYSTEM_FLAG,0)=0')) = 0, 'identity creates no user trigger');
      end
      else
      begin
        GeneratorName := TFirebirdDialect.ObjectName('GEN', Table, 'id');
        Check(Int64(Conn.ExecuteScalar('SELECT COUNT(*) FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME=' +
          QuotedStr(GeneratorName) + ' AND COALESCE(RDB$SYSTEM_FLAG,0)=0')) = 1, 'legacy user generator exists');
        Check(Int64(Conn.ExecuteScalar('SELECT COUNT(*) FROM RDB$TRIGGERS WHERE RDB$TRIGGER_NAME=' +
          QuotedStr(TFirebirdDialect.ObjectName('BI', Table, 'id')))) = 1, 'legacy trigger exists');
        Conn.ExecuteDirect('INSERT INTO ' + Table + ' (id,code) VALUES (NULL,104)');
        Conn.Commit;
        Check(Int64(Conn.ExecuteScalar('SELECT id FROM ' + Table + ' WHERE code=104')) > SecondId, 'legacy trigger fills explicit NULL');
      end;
    end;
    Schema.PrepareDB(True);
    Check(Schema.SQL.Count = 0, 'repeat migration does not recreate auto increment');
    Check(Schema.DatabaseVersion = Version, 'repeat migration keeps version');
    Item.Id.Clear;
    Item.Code.Value := 105;
    Check(TDeltaPersister.Insert(Conn, Item), 'insert after repeat migration');
    Check(Item.Id.Value > SecondId, 'repeat migration does not reset sequence');
    Conn.Commit;
  finally Schema.Free; Item.Free; end;
end;

procedure RunBulkInsert;
var
  Items: array[0..2] of TDeltaModel;
  N: Integer;
  BeforeCount: Int64;
  Failed: Boolean;
begin
  AlterCase := False;
  BeforeCount := CountItems(Conn);
  Conn.Commit;
  for N := 0 to High(Items) do
  begin
    Items[N] := TItem.Create;
    TItem(Items[N]).Code.Value := 700 + N;
  end;
  try
    Check(TDeltaPersister.BulkInsert(Conn, Items, 2) = 3, 'bulk inserts all rows across batches');
    Check(CountItems(Conn) = BeforeCount + 3, 'bulk persists complete batch');
    Check(TItem(Items[0]).Id.IsNull, 'bulk does not populate generated IDs');
    Conn.Commit;
    Conn.StartTransaction;
    Check(TDeltaPersister.BulkInsert(Conn, Items, 2) = 3, 'bulk joins caller transaction');
    Check(Conn.TransactionActive, 'bulk leaves caller transaction active');
    Conn.Rollback;
    Check(CountItems(Conn) = BeforeCount + 3, 'caller rollback removes entire bulk');
    Conn.Commit;
    TItem(Items[1]).Id.Value := 880001;
    TItem(Items[2]).Id.Value := 880001;
    Failed := False;
    try TDeltaPersister.BulkInsert(Conn, Items, 1);
    except Failed := True; end;
    Check(Failed, 'duplicate key fails bulk');
    Check(CountItems(Conn) = BeforeCount + 3, 'owned transaction rolls back earlier batches');
    Conn.Commit;
  finally
    for N := 0 to High(Items) do Items[N].Free;
  end;
end;

var
  URL, VersionSQL: string;
  Attempts: Integer;
  SQLDBAccess: ISQLDBConnectionAccess;
begin
  try
    URL := GetEnvironmentVariable('DELTAMODEL_TEST_URL');
    if URL = '' then raise Exception.Create('DELTAMODEL_TEST_URL must point to a disposable test database');
    if Pos('mysql:', URL) = 1 then mysql57dyn.InitialiseMysql('libmariadb.so.3');
    Conn := TDeltaORM.CreateConnection(URL);
    try
      if Supports(Conn, ISQLDBConnectionAccess, SQLDBAccess) then
      begin
        if SameText(Conn.Dialect.Name, 'SQLite') then
          SQLDBAccess.Connector.Params.Values['foreign_keys'] := 'ON';
        if SameText(Conn.Dialect.Name, 'MySQL') then
          TConnectorAccess(SQLDBAccess.Connector).ConfigureMariaDB;
      end;
      for Attempts := 1 to 60 do
      begin
        try Conn.Connect; Break;
        except if Attempts = 60 then raise; Sleep(500); end;
      end;
      if SameText(Conn.Dialect.Name, 'SQLite') then
        VersionSQL := 'SELECT sqlite_version()'
      else if SameText(Conn.Dialect.Name, 'Firebird') then
        VersionSQL := 'SELECT rdb$get_context(''SYSTEM'',''ENGINE_VERSION'') FROM rdb$database'
      else
        VersionSQL := 'SELECT version()';
      WriteLn('SERVER ', VarToStr(Conn.ExecuteScalar(VersionSQL)));
      Conn.Commit;
      RunAutoIncrement(False);
      RunBulkInsert;
      if not SameText(Conn.Dialect.Name, 'SQLite') then RunAutoIncrement(True);
      WriteLn('RESULT: ', Checks, ' integration checks passed');
    finally Conn := nil; end;
  except
    on E: Exception do begin WriteLn('[FAIL] ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
