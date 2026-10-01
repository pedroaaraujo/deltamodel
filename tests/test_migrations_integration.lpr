program test_migrations_integration;
{$mode ObjFPC}{$H+}
uses
  cthreads, cwstring, Classes, SysUtils, Variants, DB, SQLDB,
  sqlite3conn, pqconnection, ibconnection, mysql57conn, mysql57dyn,
  DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Connection,
  DeltaModel.ORM.Schema, DeltaModel.ORM.Types;

type
  TConnectorAccess = class(TSQLConnector)
    procedure ConfigureMariaDB;
  end;
  TParent = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FCode: TDFIntNull;
  public
    procedure AfterConstruction; override;
  published
    property Id: TDFIntRequired read FId write FId;
    property Code: TDFIntNull read FCode write FCode;
  end;
  TChild = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FParentId: TDFIntNull;
    FOtherId: TDFIntNull;
  public
    procedure AfterConstruction; override;
  published
    property Id: TDFIntRequired read FId write FId;
    property ParentId: TDFIntNull read FParentId write FParentId;
    property OtherId: TDFIntNull read FOtherId write FOtherId;
  end;
var
  Scenario: string;
  Checks: Integer;
  Engine: TDeltaORMEngine;

procedure TConnectorAccess.ConfigureMariaDB;
begin
  CheckProxy;
  TMySQL57Connection(Proxy).SkipLibraryVersionCheck := True;
end;
procedure Check(Condition: Boolean; const Msg: string);
begin
  if not Condition then raise Exception.Create(Scenario + ': ' + Msg);
  Inc(Checks);
  WriteLn('[PASS] ', Scenario, ': ', Msg);
end;
procedure TParent.AfterConstruction;
begin
  inherited;
  TableName := 'dm_' + Scenario + '_parent';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey];
  Code.FieldName := 'code';
  Code.DBOptions := [dboUnique];
  if Scenario = 'cycle' then Code.ForeignKey.References(TChild, 'id', fkNone, fkNone);
end;
procedure TChild.AfterConstruction;
begin
  inherited;
  TableName := 'dm_' + Scenario + '_child';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey];
  ParentId.FieldName := 'parent_id';
  ParentId.ForeignKey.References(TParent, 'code');
  OtherId.FieldName := 'other_id';
  OtherId.ForeignKey.References(TParent, 'code');
  if Scenario = 'indexed' then ParentId.IsIndexed := True;
end;

procedure RunScenario(const Name: string);
var
  Schema: TDeltaORMSchema;
  ParentName, ChildName, Extra, Plan: string;
  Failed: Boolean;
  Version: Int64;
  DS: TDataSet;
begin
  Scenario := Name;
  WriteLn('SCENARIO ', Name);
  ParentName := 'dm_' + Name + '_parent';
  ChildName := 'dm_' + Name + '_child';
  if Name = 'alter' then
  begin
    Engine.ExecuteDirect('CREATE TABLE ' + ParentName + ' (id INTEGER NOT NULL PRIMARY KEY)');
    Engine.ExecuteDirect('CREATE TABLE ' + ChildName + ' (id INTEGER NOT NULL PRIMARY KEY)');
    Engine.Commit;
    Engine.ExecuteDirect('INSERT INTO ' + ParentName + ' (id) VALUES (7)');
    Engine.Commit;
  end;
  if (Name = 'existing') or (Name = 'legacy') or (Name = 'orphan') then
  begin
    Engine.ExecuteDirect('CREATE TABLE ' + ParentName + ' (id INTEGER NOT NULL PRIMARY KEY, code INTEGER UNIQUE)');
    Extra := '';
    if Name = 'legacy' then Extra := ', CONSTRAINT legacy_fk FOREIGN KEY (parent_id) REFERENCES ' + ParentName + '(code)';
    Engine.ExecuteDirect('CREATE TABLE ' + ChildName + ' (id INTEGER NOT NULL PRIMARY KEY, parent_id INTEGER, other_id INTEGER' + Extra + ')');
    Engine.Commit;
    if Name = 'orphan' then Engine.ExecuteDirect('INSERT INTO ' + ChildName + ' (id,parent_id) VALUES (1,999)');
    Engine.Commit;
  end;
  Schema := TDeltaORMSchema.Create(Engine);
  try
    try
      Schema.RegisterModel(TChild);
      Schema.RegisterModel(TParent);
      if (Engine.Dialect = ddSQLite) and ((Name = 'alter') or (Name = 'existing') or (Name = 'legacy') or (Name = 'orphan')) then
      begin
        Failed := False;
        try Schema.PrepareDB(True);
        except on E: Exception do Failed := Pos('requires rebuilding', E.Message) > 0; end;
        Check(Failed, 'unsupported FK alteration rejected before execution');
        if Name = 'alter' then
        begin
          DS := Engine.ExecuteQuery('SELECT * FROM ' + ParentName);
          try
            Check(DS.FieldCount = 1, 'planning failure left original structure intact');
            Check(DS.Fields[0].AsInteger = 7, 'planning failure preserved existing data');
          finally DS.Free; end;
        end;
        Exit;
      end;
      if Name = 'orphan' then
      begin
        Version := Engine.ExecuteScalar('SELECT MAX(version) FROM deltamodel_schema_migrations');
        Failed := False;
        try Schema.PrepareDB(True);
        except on E: Exception do begin Failed := Pos('FOREIGN KEY', UpperCase(E.Message)) > 0; WriteLn('Expected FK error: ', E.Message); end; end;
        Check(Failed, 'orphan data prevents FK creation');
        Check(Int64(Engine.ExecuteScalar('SELECT MAX(version) FROM deltamodel_schema_migrations')) = Version, 'failed migration does not record success');
        Exit;
      end;
      Schema.PrepareDB(False);
      Plan := Schema.SQL.Text;
      if Engine.Dialect <> ddSQLite then
      begin
        Check(Pos('FOREIGN KEY', Plan) > 0, 'pending FKs included in plan');
        if Name = 'alter' then
          Check(Pos('ALTER TABLE ' + ParentName + ' ADD ', Plan) < Pos('FOREIGN KEY', Plan), 'new target column precedes FK');
        if Name = 'legacy' then
          Check(Pos('FOREIGN KEY (parent_id)', Plan) = 0, 'legacy FK not duplicated');
      end;
      Schema.PrepareDB(True);
      Check(Schema.DatabaseVersion > 0, 'migration recorded after successful DDL');
      if Name <> 'cycle' then
      begin
        Engine.ExecuteDirect('INSERT INTO ' + ParentName + ' (id,code) VALUES (1,100)');
        Engine.ExecuteDirect('INSERT INTO ' + ChildName + ' (id,parent_id,other_id) VALUES (1,100,100)');
        Engine.Commit;
        Failed := False;
        try
          Engine.ExecuteDirect('INSERT INTO ' + ChildName + ' (id,parent_id,other_id) VALUES (2,999,100)');
          Engine.Commit;
        except
          on E: Exception do begin
            Failed := Pos('FOREIGN KEY', UpperCase(E.Message)) > 0;
            if Engine.TransactionActive then Engine.Rollback;
            WriteLn('Expected FK error: ', E.Message);
          end;
        end;
        Check(Failed, 'database rejects orphan FK value');
        Check(Int64(Engine.ExecuteScalar('SELECT COUNT(*) FROM ' + ChildName)) = 1, 'valid row preserved after rejected insert');
      end;
      Version := Schema.DatabaseVersion;
      Schema.PrepareDB(True);
      Check(Schema.SQL.Count = 0, 'second execution has no DDL');
      Check(Schema.DatabaseVersion = Version, 'second execution adds no history');
    except
      on E: Exception do begin
        WriteLn('Generated plan:', LineEnding, Schema.SQL.Text);
        raise;
      end;
    end;
  finally
    Schema.Free;
  end;
end;
var
  URL, VersionSQL: string;
  Attempts: Integer;
begin
  try
    URL := GetEnvironmentVariable('DELTAMODEL_TEST_URL');
    if URL = '' then raise Exception.Create('DELTAMODEL_TEST_URL must point to a disposable test database');
    if Pos('mysql:', URL) = 1 then mysql57dyn.InitialiseMysql('libmariadb.so.3');
    Engine := TDeltaORMEngine.Create(URL);
    try
      if Engine.Dialect = ddSQLite then Engine.Connection.Params.Values['foreign_keys'] := 'ON';
      if Engine.Dialect = ddMySQL then TConnectorAccess(Engine.Connection).ConfigureMariaDB;
      for Attempts := 1 to 60 do
      begin
        try Engine.Connection.Connected := True; Break;
        except if Attempts = 60 then raise; Sleep(500); end;
      end;
      if Engine.Dialect = ddSQLite then
      begin
        VersionSQL := 'SELECT sqlite_version()';
      end
      else if Engine.Dialect = ddFirebird then
        VersionSQL := 'SELECT rdb$get_context(''SYSTEM'',''ENGINE_VERSION'') FROM rdb$database'
      else VersionSQL := 'SELECT version()';
      WriteLn('SERVER ', VarToStr(Engine.ExecuteScalar(VersionSQL)));
      Engine.Commit;
      RunScenario('fresh');
      RunScenario('alter');
      RunScenario('existing');
      RunScenario('legacy');
      RunScenario('cycle');
      RunScenario('indexed');
      RunScenario('orphan');
      WriteLn('RESULT: ', Checks, ' integration checks passed');
    finally Engine.Free; end;
  except
    on E: Exception do begin WriteLn('[FAIL] ', E.ClassName, ': ', E.Message); Halt(1); end;
  end;
end.
