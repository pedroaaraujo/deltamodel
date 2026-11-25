program test_migrations;
{$mode ObjFPC}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes, SysUtils, Variants, sqlite3conn,
  DeltaModel, DeltaModel.Fields,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry,
  DeltaModel.ORM.Connection, DeltaModel.ORM.DDL, DeltaModel.ORM.Schema,
  DeltaModel.ORM.Dialects, DeltaModel.ORM.Driver.SQLDB;

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
  TPlanningConnection = class(TBaseDeltaConnection)
  private
    FInner: IDeltaConnection;
    function RewriteCatalogSQL(const ASQL: string): string;
  public
    constructor Create(const AInner: IDeltaConnection);
    function DriverName: string; override;
    function Connected: Boolean; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function TransactionActive: Boolean; override;
    function CreateStatement(const ASQL: string = ''): IDeltaStatement; override;
    procedure ExecuteDirect(const ASQL: string); override;
    function ExecuteQuery(const ASQL: string): IDeltaResultSet; override;
    procedure GetTableNames(ATables: TStrings); override;
    procedure SeedSQL(const ASQL: string);
  end;

var
  Passed: Integer;

procedure Check(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
  Inc(Passed);
  WriteLn('[PASS] ', Message);
end;

constructor TPlanningConnection.Create(const AInner: IDeltaConnection);
begin
  inherited Create(TDeltaORM.Dialect('postgresql'));
  FInner := AInner;
end;

function TPlanningConnection.DriverName: string;
begin
  Result := FInner.DriverName;
end;

function TPlanningConnection.Connected: Boolean;
begin
  Result := FInner.Connected;
end;

procedure TPlanningConnection.Connect;
begin
  FInner.Connect;
end;

procedure TPlanningConnection.Disconnect;
begin
  FInner.Disconnect;
end;

procedure TPlanningConnection.StartTransaction;
begin
  FInner.StartTransaction;
end;

procedure TPlanningConnection.Commit;
begin
  FInner.Commit;
end;

procedure TPlanningConnection.Rollback;
begin
  FInner.Rollback;
end;

function TPlanningConnection.TransactionActive: Boolean;
begin
  Result := FInner.TransactionActive;
end;

function TPlanningConnection.RewriteCatalogSQL(const ASQL: string): string;
begin
  Result := ASQL;
  if Pos('FROM pg_indexes', ASQL) > 0 then
    Result := 'SELECT name FROM sqlite_master WHERE type=''index'' AND 1=0';
  if Pos('FROM pg_constraint', ASQL) > 0 then
    Result := 'SELECT "from" AS source_column, "table" AS target_table, ' +
      '"to" AS target_column FROM pragma_foreign_key_list(''migration_children'')';
end;

function TPlanningConnection.CreateStatement(const ASQL: string): IDeltaStatement;
begin
  Result := FInner.CreateStatement(RewriteCatalogSQL(ASQL));
end;

procedure TPlanningConnection.ExecuteDirect(const ASQL: string);
begin
  raise Exception.Create('Planning engine must not execute DDL: ' + ASQL);
end;

function TPlanningConnection.ExecuteQuery(const ASQL: string): IDeltaResultSet;
begin
  Result := FInner.ExecuteQuery(RewriteCatalogSQL(ASQL));
end;

procedure TPlanningConnection.GetTableNames(ATables: TStrings);
begin
  FInner.GetTableNames(ATables);
end;

procedure TPlanningConnection.SeedSQL(const ASQL: string);
begin
  FInner.ExecuteDirect(ASQL);
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
  PlanObj: TPlanningConnection;
  Conn: IDeltaConnection;
  Schema: TDeltaORMSchema;
  Parent: TParent;
  Child: TChild;
  Plan, FK1, FK2: string;
  Cycle: Boolean;
begin
  for Cycle := False to True do
  begin
    PlanObj := TPlanningConnection.Create(TDeltaORM.Connect('sqlite:///:memory:'));
    Conn := PlanObj;
    try
      if not Cycle then
        PlanObj.SeedSQL('CREATE TABLE migration_parents (id INTEGER PRIMARY KEY)');
      Schema := TDeltaORMSchema.Create(Conn);
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
        FK1 := TDDLBuilder.ForeignKeyDDL(Child.TableName, Child.ParentId, 'oracle');
        FK2 := TDDLBuilder.ForeignKeyDDL(Child.TableName, Child.OtherParentId, 'oracle');
        Check(FK1 <> FK2, 'Two references to the same table have distinct DDL');
        Check(Copy(FK1, 1, Pos(' FOREIGN KEY', FK1)) <> Copy(FK2, 1, Pos(' FOREIGN KEY', FK2)), 'Long constraint names retain distinct hash suffixes');
      finally
        Schema.Free;
      end;
    finally
      Conn := nil;
    end;
  end;
end;

procedure TestExistingColumnFK;
var
  PlanObj: TPlanningConnection;
  Conn: IDeltaConnection;
  Schema: TDeltaORMSchema;
  Legacy: Boolean;
  Extra, Plan: string;
begin
  for Legacy := False to True do
  begin
    PlanObj := TPlanningConnection.Create(TDeltaORM.Connect('sqlite:///:memory:'));
    Conn := PlanObj;
    try
      PlanObj.SeedSQL('CREATE TABLE migration_parents (id INTEGER PRIMARY KEY, code INTEGER UNIQUE)');
      Extra := '';
      if Legacy then Extra := ', CONSTRAINT old_fk_name FOREIGN KEY(parent_id) REFERENCES migration_parents(code)';
      PlanObj.SeedSQL('CREATE TABLE migration_children (id INTEGER PRIMARY KEY, parent_id INTEGER, other_parent_id INTEGER' + Extra + ')');
      Schema := TDeltaORMSchema.Create(Conn);
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
      Conn := nil;
    end;
  end;
end;

procedure TestSQLite;
var
  Conn: IDeltaConnection;
  Schema: TDeltaORMSchema;
  Failed: Boolean;
  RS: IDeltaResultSet;
  Version: Int64;
  Child: TChild;
begin
  Conn := TDeltaORM.Connect('sqlite:///:memory:');
  try
    Schema := TDeltaORMSchema.Create(Conn);
    try
      Schema.RegisterModel(TChild);
      Schema.RegisterModel(TParent);
      Schema.PrepareDB(True);
      Version := Schema.DatabaseVersion;
      RS := Conn.ExecuteQuery('PRAGMA foreign_key_list(migration_children)');
      try
        Check(RS.Next, 'SQLite stores inline foreign keys');
        Check(RS.AsString('table') = 'migration_parents', 'SQLite references custom table name');
      finally
        RS.Close;
      end;
      Schema.PrepareDB(True);
      Check(Schema.SQL.Count = 0, 'Repeated migration has no DDL');
      Check(Schema.DatabaseVersion = Version, 'Repeated migration does not add history');
    finally
      Schema.Free;
    end;
  finally
    Conn := nil;
  end;

  Conn := TDeltaORM.Connect('sqlite:///:memory:');
  try
    Conn.ExecuteDirect('CREATE TABLE migration_children (id INTEGER PRIMARY KEY, parent_id INTEGER)');
    if Conn.TransactionActive then Conn.Commit;
    Schema := TDeltaORMSchema.Create(Conn);
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
      RS := Conn.ExecuteQuery('SELECT name FROM sqlite_master WHERE name=''migration_parents''');
      try
        Check(not RS.Next, 'Unsupported SQLite migration executes no partial DDL');
      finally
        RS.Close;
      end;
    finally
      Schema.Free;
    end;
  finally
    Conn := nil;
  end;

  Conn := TDeltaORM.Connect('sqlite:///:memory:');
  try
    Conn.ExecuteDirect('CREATE TABLE migration_children (id INTEGER PRIMARY KEY)');
    Conn.ExecuteDirect('INSERT INTO migration_children (id) VALUES (7)');
    if Conn.TransactionActive then Conn.Commit;
    Schema := TDeltaORMSchema.Create(Conn);
    try
      Child := TChild.Create;
      Child.ParentId.ForeignKey.ReferencesTable := nil;
      Child.OtherParentId.ForeignKey.ReferencesTable := nil;
      Schema.RegisterModel(Child);
      Schema.PrepareDB(True);
      RS := Conn.ExecuteQuery('SELECT id, parent_id, other_parent_id FROM migration_children');
      try
        Check(RS.Next, 'Row exists after ALTER migration');
        Check(RS.FieldCount = 3, 'Every new column is executed as a separate statement');
        Check(RS.AsInteger('id') = 7, 'ALTER migration preserves existing data');
      finally
        RS.Close;
      end;
      Version := Schema.DatabaseVersion;
      Schema.PrepareDB(True);
      Check(Schema.DatabaseVersion = Version, 'ALTER migration can be rerun without new history');
    finally
      Schema.Free;
    end;
  finally
    Conn := nil;
  end;
end;

begin
  TestPlan;
  TestExistingColumnFK;
  TestSQLite;
  WriteLn(Passed, ' migration checks passed.');
end.
