program test_migrations;
{$mode ObjFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes, SysUtils, DB, SQLDB, sqlite3conn,
  DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Types,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Connection,
  DeltaModel.ORM.DDL, DeltaModel.ORM.Schema;

type
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
    FOtherParentId: TDFIntNull;
  public
    procedure AfterConstruction; override;
  published
    property Id: TDFIntRequired read FId write FId;
    property ParentId: TDFIntNull read FParentId write FParentId;
    property OtherParentId: TDFIntNull read FOtherParentId write FOtherParentId;
  end;
  { Real SQLite metadata, PostgreSQL DDL only. Never persist this plan. }
  TPlanningEngine = class(TDeltaORMEngine, IDeltaORMEngine)
  private
    procedure ReadTestCatalog(DataSet: TDataSet);
  public
    function NewDataset: TSQLQuery;
    function Dialect: TDatabaseDialect;
    procedure ExecuteDirect(const ASQL: string);
  end;

var Passed: Integer;
procedure Check(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
  Inc(Passed);
  WriteLn('[PASS] ', Message);
end;

function TPlanningEngine.Dialect: TDatabaseDialect;
begin
  Result := ddPostgreSQL;
end;
function TPlanningEngine.NewDataset: TSQLQuery;
begin
  Result := inherited NewDataset;
  Result.BeforeOpen := @ReadTestCatalog;
end;
procedure TPlanningEngine.ReadTestCatalog(DataSet: TDataSet);
var
  CatalogQuery: TSQLQuery;
begin
  CatalogQuery := TSQLQuery(DataSet);
  { Simulate the catalog's normalized result. This does not validate PostgreSQL SQL. }
  if Pos('FROM pg_indexes', CatalogQuery.SQL.Text) > 0 then
    CatalogQuery.SQL.Text := 'SELECT name FROM sqlite_master WHERE type=''index'' AND 1=0';
  if Pos('FROM pg_constraint', CatalogQuery.SQL.Text) > 0 then
    CatalogQuery.SQL.Text := 'SELECT "from" AS source_column, "table" AS target_table, ' +
      '"to" AS target_column FROM pragma_foreign_key_list(''migration_children'')';
end;
procedure TPlanningEngine.ExecuteDirect(const ASQL: string);
begin
  raise Exception.Create('Planning engine must not execute DDL: ' + ASQL);
end;
procedure TParent.AfterConstruction;
begin
  inherited;
  TableName := 'migration_parents';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey];
  Code.FieldName := 'code';
  Code.DBOptions := [dboUnique];
end;
procedure TChild.AfterConstruction;
begin
  inherited;
  TableName := 'migration_children';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey];
  ParentId.FieldName := 'parent_id';
  ParentId.ForeignKey.References(TParent, 'code');
  OtherParentId.FieldName := 'other_parent_id';
  OtherParentId.ForeignKey.References(TParent, 'code');
end;

procedure TestPlan;
var
  Engine: TPlanningEngine;
  Schema: TDeltaORMSchema;
  Parent: TParent;
  Child: TChild;
  Plan, FK1, FK2: string;
  Cycle: Boolean;
begin
  for Cycle := False to True do
  begin
    Engine := TPlanningEngine.Create('sqlite:///:memory:');
    try
      { Existing table requires ALTER ADD code before the child's FK. }
      if not Cycle then
        TDeltaORMEngine(Engine).ExecuteDirect('CREATE TABLE migration_parents (id INTEGER PRIMARY KEY)');
      Schema := TDeltaORMSchema.Create(Engine);
      try
        Child := TChild.Create;
        Parent := TParent.Create;
        Parent.Code.IsIndexed := True;
        if Cycle then Parent.Code.ForeignKey.References(TChild, 'id');
        Schema.RegisterModel(Child);
        Schema.RegisterModel(Parent);
        Schema.PrepareDB(False);
        Plan := Schema.SQL.Text;
        Check(Pos('ALTER TABLE migration_children ADD CONSTRAINT FK_', Plan) > 0, 'FK deferred to ALTER TABLE');
        Check(Pos('REFERENCES migration_parents(code)', Plan) > 0, 'FK uses custom TableName');
        Check((Pos('CREATE INDEX', Plan) > 0) and (Pos('CREATE INDEX', Plan) < Pos('FOREIGN KEY', Plan)), 'Indexes precede all FKs');
        Check(Pos('UNIQUE (code)', Plan) < Pos('FOREIGN KEY', Plan), 'Referenced UNIQUE precedes all FKs');
        if not Cycle then
          Check(Pos('ALTER TABLE migration_parents ADD', Plan) < Pos('FOREIGN KEY', Plan), 'Existing table columns precede all FKs')
        else
          Check(Pos('CREATE TABLE IF NOT EXISTS migration_parents', Plan) < Pos('FOREIGN KEY', Plan), 'Circular references: all tables precede FKs');
        FK1 := TDDLBuilder.ForeignKeyDDL(Child.TableName, Child.ParentId, ddOracle);
        FK2 := TDDLBuilder.ForeignKeyDDL(Child.TableName, Child.OtherParentId, ddOracle);
        Check(FK1 <> FK2, 'Two references to the same table have distinct DDL');
        Check(Copy(FK1, 1, Pos(' FOREIGN KEY', FK1)) <> Copy(FK2, 1, Pos(' FOREIGN KEY', FK2)), 'Long constraint names retain distinct hash suffixes');
      finally
        Schema.Free;
      end;
    finally
      Engine.Free;
    end;
  end;
end;

procedure TestExistingColumnFK;
var
  Engine: TPlanningEngine;
  Schema: TDeltaORMSchema;
  Legacy: Boolean;
  Extra, Plan: string;
begin
  for Legacy := False to True do
  begin
    Engine := TPlanningEngine.Create('sqlite:///:memory:');
    try
      TDeltaORMEngine(Engine).ExecuteDirect('CREATE TABLE migration_parents (id INTEGER PRIMARY KEY, code INTEGER UNIQUE)');
      Extra := '';
      if Legacy then Extra := ', CONSTRAINT old_fk_name FOREIGN KEY(parent_id) REFERENCES migration_parents(code)';
      TDeltaORMEngine(Engine).ExecuteDirect('CREATE TABLE migration_children (id INTEGER PRIMARY KEY, parent_id INTEGER, other_parent_id INTEGER' + Extra + ')');
      Schema := TDeltaORMSchema.Create(Engine);
      try
        Schema.RegisterModel(TChild);
        Schema.RegisterModel(TParent);
        Schema.PrepareDB(False);
        Plan := Schema.SQL.Text;
        Check(Pos('FOREIGN KEY (other_parent_id)', Plan) > 0, 'FK added to a column that already exists');
        Check((Pos('FOREIGN KEY (parent_id)', Plan) > 0) <> Legacy, 'Existing FK recognized independently of constraint name');
        Check(Pos(' ADD   ', Plan) = 0, 'Existing columns are not recreated');
      finally
        Schema.Free;
      end;
    finally
      Engine.Free;
    end;
  end;
end;

procedure TestSQLite;
var
  Engine: TDeltaORMEngine;
  Schema: TDeltaORMSchema;
  Failed: Boolean;
  DS: TDataSet;
  Version: Int64;
  Child: TChild;
begin
  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Schema := TDeltaORMSchema.Create(Engine);
    try
      Schema.RegisterModel(TChild);
      Schema.RegisterModel(TParent);
      Schema.PrepareDB(True);
      Version := Schema.DatabaseVersion;
      DS := Engine.ExecuteQuery('PRAGMA foreign_key_list(migration_children)');
      try
        Check(not DS.EOF, 'SQLite stores inline foreign keys');
        Check(DS.FieldByName('table').AsString = 'migration_parents', 'SQLite references custom table name');
      finally
        DS.Free;
      end;
      Schema.PrepareDB(True);
      Check(Schema.SQL.Count = 0, 'Repeated migration has no DDL');
      Check(Schema.DatabaseVersion = Version, 'Repeated migration does not add history');
    finally
      Schema.Free;
    end;
  finally
    Engine.Free;
  end;

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.ExecuteDirect('CREATE TABLE migration_children (id INTEGER PRIMARY KEY, parent_id INTEGER)');
    Engine.Commit;
    Schema := TDeltaORMSchema.Create(Engine);
    try
      Schema.RegisterModel(TChild);
      Schema.RegisterModel(TParent);
      Failed := False;
      try
        Schema.PrepareDB(True);
      except
        on E: Exception do Failed := Pos('requires rebuilding the table', E.Message) > 0;
      end;
      Check(Failed, 'SQLite missing FK is rejected during planning');
      DS := Engine.ExecuteQuery('SELECT name FROM sqlite_master WHERE name=''migration_parents''');
      try
        Check(DS.EOF, 'Unsupported SQLite migration executes no partial DDL');
      finally
        DS.Free;
      end;
    finally
      Schema.Free;
    end;
  finally
    Engine.Free;
  end;

  Engine := TDeltaORMEngine.Create('sqlite:///:memory:');
  try
    Engine.ExecuteDirect('CREATE TABLE migration_children (id INTEGER PRIMARY KEY)');
    Engine.ExecuteDirect('INSERT INTO migration_children (id) VALUES (7)');
    Engine.Commit;
    Schema := TDeltaORMSchema.Create(Engine);
    try
      Child := TChild.Create;
      Child.ParentId.ForeignKey.ReferencesTable := nil;
      Child.OtherParentId.ForeignKey.ReferencesTable := nil;
      Schema.RegisterModel(Child);
      Schema.PrepareDB(True);
      DS := Engine.ExecuteQuery('SELECT id, parent_id, other_parent_id FROM migration_children');
      try
        Check(DS.FieldCount = 3, 'Every new column is executed as a separate statement');
        Check(DS.FieldByName('id').AsInteger = 7, 'ALTER migration preserves existing data');
      finally
        DS.Free;
      end;
      Version := Schema.DatabaseVersion;
      Schema.PrepareDB(True);
      Check(Schema.DatabaseVersion = Version, 'ALTER migration can be rerun without new history');
    finally
      Schema.Free;
    end;
  finally
    Engine.Free;
  end;

end;

begin
  TestPlan;
  TestExistingColumnFK;
  TestSQLite;
  WriteLn(Passed, ' migration checks passed.');
end.
