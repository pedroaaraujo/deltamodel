unit DeltaModel.ORM.Schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, TypInfo, fgl, DB, SQLDB, DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Interfaces,
  DeltaModel.ORM.DDL, DeltaModel.ORM.Types;

type

  { TDeltaORMSchema }

  TDeltaORMSchema = class
  private type TTableList = specialize TFPGObjectList<TDeltaModel>;
  private
    FConnection: IDeltaORMEngine;
    FModels: TTableList;
    FDBTables: TStringList;
    FSQL: TStringList;
    FConstraints: TStringList;
    FForeignKeys: TStringList;
    FDatabaseVersion: Int64;
    FFirebirdMajor: Integer;
    procedure CreateTables;
    procedure AlterTables;
    procedure CreateConstraints;
    procedure CreateForeignKeys;
    function ForeignKeyExists(const Table: string; Field: TDeltaField): Boolean;
    procedure CreateIndexes;
    procedure ReadIndexNames(const Table: string; Names: TStrings);
    procedure EnsureMigrationTable;
    function ReadDatabaseVersion: Int64;
    function MigrationChecksum(ACount: Integer): string;
  public
    property SQL: TStringList read FSQL;
    { Version of the last successfully applied PrepareDB migration. }
    property DatabaseVersion: Int64 read FDatabaseVersion;
    procedure RegisterModel(Model: TDeltaModel); overload;
    procedure RegisterModel(ModelClass: TDeltaModelClass); overload;
    procedure PrepareDB(Persist: Boolean);
    constructor Create(AConnection: IDeltaORMEngine);
    destructor Destroy; override;
  end;

implementation

const
  MIGRATION_TABLE = 'deltamodel_schema_migrations';

{ TDeltaORMSchema }

procedure TDeltaORMSchema.CreateTables;
var
  I: Integer;
  Obj: TDeltaModel;
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    if (FDBTables.IndexOf(Obj.TableName) > -1) then
      Continue;

    FSQL.Add(TDDLBuilder.CreateTableAndFields(Obj, FConstraints, FConnection.Dialect, False, FFirebirdMajor));
  end;
end;

procedure TDeltaORMSchema.AlterTables;
var
  I, F: Integer;
  Obj: TDeltaModel;
  FieldList: TStringList;
  DS: TSQLQuery;
begin
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    FieldList := TStringList.Create;
    try
      if (FDBTables.IndexOf(Obj.TableName) = -1) then
        Continue;

      DS := FConnection.NewDataset;
      try
        DS.SQL.Text :=
          'SELECT * FROM ' + Obj.TableName + sLineBreak +
          'WHERE 1 = 0';
        DS.Open;
        for F := 0 to Pred(DS.FieldCount) do
          FieldList.Add(DS.Fields[F].FieldName);
        DS.Close;
      finally
        DS.Free;
      end;

      TDDLBuilder.AddFieldStatements(Obj, FConnection.Dialect, FieldList,
        FConstraints, FSQL, False, FFirebirdMajor);
    finally
      FieldList.Free;
    end;
  end;
end;

procedure TDeltaORMSchema.CreateConstraints;
var
  I: Integer;
  S: string;
begin
  for I := 0 to Pred(FConstraints.Count) do
  begin
    S := FConstraints[I];
    if not S.IsEmpty then
    begin
      FSQL.Add(S);
    end;
  end;
end;

function TDeltaORMSchema.ForeignKeyExists(const Table: string;
  Field: TDeltaField): Boolean;
var
  DS: TSQLQuery;
  Query, RefTable: string;
begin
  Result := False;
  if FDBTables.IndexOf(Table) = -1 then Exit;
  RefTable := TDDLBuilder.ReferencedTableName(Field);
  case FConnection.Dialect of
    ddSQLite:
      Query := 'SELECT "from" AS source_column, "table" AS target_table, ' +
        '"to" AS target_column FROM pragma_foreign_key_list(' + QuotedStr(Table) + ') ' +
        'WHERE id IN (SELECT id FROM pragma_foreign_key_list(' + QuotedStr(Table) + ') GROUP BY id HAVING COUNT(*)=1)';
    ddPostgreSQL:
      Query := 'SELECT a.attname AS source_column, rt.relname AS target_table, ' +
        'ra.attname AS target_column FROM pg_constraint c ' +
        'JOIN pg_class t ON t.oid=c.conrelid ' +
        'JOIN pg_class rt ON rt.oid=c.confrelid ' +
        'JOIN pg_attribute a ON a.attrelid=t.oid AND a.attnum=c.conkey[1] ' +
        'JOIN pg_attribute ra ON ra.attrelid=rt.oid AND ra.attnum=c.confkey[1] ' +
        'WHERE c.contype=''f'' AND array_length(c.conkey,1)=1 ' +
        'AND t.relnamespace=current_schema()::regnamespace AND t.relname=' + QuotedStr(Table);
    ddMySQL:
      Query := 'SELECT COLUMN_NAME AS source_column, REFERENCED_TABLE_NAME AS target_table, ' +
        'REFERENCED_COLUMN_NAME AS target_column FROM information_schema.KEY_COLUMN_USAGE k ' +
        'WHERE TABLE_SCHEMA=DATABASE() AND REFERENCED_TABLE_NAME IS NOT NULL ' +
        'AND (SELECT COUNT(*) FROM information_schema.KEY_COLUMN_USAGE k2 WHERE k2.CONSTRAINT_SCHEMA=k.CONSTRAINT_SCHEMA AND k2.TABLE_NAME=k.TABLE_NAME AND k2.CONSTRAINT_NAME=k.CONSTRAINT_NAME)=1 AND TABLE_NAME=' + QuotedStr(Table);
    ddMSSQL:
      Query := 'SELECT COL_NAME(f.parent_object_id,f.parent_column_id) AS source_column, ' +
        'OBJECT_NAME(f.referenced_object_id) AS target_table, ' +
        'COL_NAME(f.referenced_object_id,f.referenced_column_id) AS target_column ' +
        'FROM sys.foreign_key_columns f WHERE ' +
        '(SELECT COUNT(*) FROM sys.foreign_key_columns f2 WHERE f2.constraint_object_id=f.constraint_object_id)=1 AND f.parent_object_id=OBJECT_ID(' + QuotedStr(Table) + ')';
    ddOracle:
      Query := 'SELECT col.column_name AS source_column, ref.table_name AS target_table, ' +
        'rcol.column_name AS target_column FROM user_constraints c ' +
        'JOIN user_cons_columns col ON col.constraint_name=c.constraint_name ' +
        'JOIN all_constraints ref ON ref.owner=c.r_owner AND ref.constraint_name=c.r_constraint_name ' +
        'JOIN all_cons_columns rcol ON rcol.owner=ref.owner AND rcol.constraint_name=ref.constraint_name AND rcol.position=col.position ' +
        'WHERE c.constraint_type=''R'' AND ' +
        '(SELECT COUNT(*) FROM user_cons_columns c2 WHERE c2.constraint_name=c.constraint_name)=1 AND c.table_name=' + QuotedStr(UpperCase(Table));
    ddFirebird:
      Query := 'SELECT TRIM(s.RDB$FIELD_NAME) AS source_column, ' +
        'TRIM(p.RDB$RELATION_NAME) AS target_table, TRIM(ps.RDB$FIELD_NAME) AS target_column ' +
        'FROM RDB$RELATION_CONSTRAINTS c ' +
        'JOIN RDB$REF_CONSTRAINTS r ON r.RDB$CONSTRAINT_NAME=c.RDB$CONSTRAINT_NAME ' +
        'JOIN RDB$RELATION_CONSTRAINTS p ON p.RDB$CONSTRAINT_NAME=r.RDB$CONST_NAME_UQ ' +
        'JOIN RDB$INDEX_SEGMENTS s ON s.RDB$INDEX_NAME=c.RDB$INDEX_NAME ' +
        'JOIN RDB$INDEX_SEGMENTS ps ON ps.RDB$INDEX_NAME=p.RDB$INDEX_NAME AND ps.RDB$FIELD_POSITION=s.RDB$FIELD_POSITION ' +
        'WHERE c.RDB$CONSTRAINT_TYPE=''FOREIGN KEY'' AND ' +
        '(SELECT COUNT(*) FROM RDB$INDEX_SEGMENTS s2 WHERE s2.RDB$INDEX_NAME=c.RDB$INDEX_NAME)=1 AND c.RDB$RELATION_NAME=' + QuotedStr(UpperCase(Table));
  end;
  DS := FConnection.NewDataset;
  try
    DS.SQL.Text := Query;
    DS.Open;
    while not DS.EOF do
    begin
      if SameText(DS.FieldByName('source_column').AsString, Field.FieldName) and
         SameText(DS.FieldByName('target_table').AsString, RefTable) and
         SameText(DS.FieldByName('target_column').AsString, Field.ForeignKey.ReferencesField) then
        Exit(True);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

procedure TDeltaORMSchema.CreateForeignKeys;
var
  I, J, Count: Integer;
  Props: PPropList;
  Obj: TDeltaModel;
  Value: TObject;
  Field: TDeltaField;
begin
  for I := 0 to FModels.Count - 1 do
  begin
    Obj := FModels[I];
    { SQLite includes constraints in CREATE TABLE, never as standalone SQL. }
    if (FConnection.Dialect = ddSQLite) and (FDBTables.IndexOf(Obj.TableName) = -1) then
      Continue;
    Count := GetPropList(Obj.ClassInfo, tkProperties, nil);
    GetMem(Props, Count * SizeOf(Pointer));
    try
      GetPropList(Obj.ClassInfo, tkProperties, Props, False);
      for J := 0 to Count - 1 do
      begin
        if (Props^[J]^.PropType^.Kind <> tkClass) or (Props^[J]^.GetProc = nil) then Continue;
        Value := GetObjectProp(Obj, Props^[J]);
        if not (Value is TDeltaField) then Continue;
        Field := TDeltaField(Value);
        if Field.IsVirtual or (Field.ForeignKey.ReferencesTable = nil) then Continue;
        if ForeignKeyExists(Obj.TableName, Field) then Continue;
        if FConnection.Dialect = ddSQLite then
          raise Exception.CreateFmt('SQLite: adding foreign key %s.%s requires rebuilding the table. No migration was executed.',
            [Obj.TableName, Field.FieldName]);
        FForeignKeys.Add(TDDLBuilder.ForeignKeyDDL(Obj.TableName, Field, FConnection.Dialect));
      end;
    finally
      FreeMem(Props);
    end;
  end;
end;

procedure TDeltaORMSchema.ReadIndexNames(const Table: string; Names: TStrings);
var
  DS: TSQLQuery;
  Query: string;
begin
  Names.Clear;
  if FDBTables.IndexOf(Table) = -1 then Exit;
  case FConnection.Dialect of
    ddSQLite:
      Query := 'SELECT name FROM pragma_index_list(' + QuotedStr(Table) + ')';
    ddPostgreSQL:
      Query := 'SELECT indexname FROM pg_indexes WHERE schemaname=current_schema() AND tablename=' + QuotedStr(Table);
    ddMySQL:
      Query := 'SELECT DISTINCT INDEX_NAME FROM information_schema.STATISTICS WHERE TABLE_SCHEMA=DATABASE() AND TABLE_NAME=' + QuotedStr(Table);
    ddFirebird:
      Query := 'SELECT TRIM(RDB$INDEX_NAME) FROM RDB$INDICES WHERE RDB$RELATION_NAME=' + QuotedStr(UpperCase(Table));
    ddMSSQL:
      Query := 'SELECT name FROM sys.indexes WHERE object_id=OBJECT_ID(' + QuotedStr(Table) + ')';
    ddOracle:
      Query := 'SELECT index_name FROM user_indexes WHERE table_name=' + QuotedStr(UpperCase(Table));
  end;
  DS := FConnection.NewDataset;
  try
    DS.SQL.Text := Query;
    DS.Open;
    while not DS.EOF do
    begin
      Names.Add(DS.Fields[0].AsString);
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

procedure TDeltaORMSchema.CreateIndexes;
var
  I, J: Integer;
  Obj: TDeltaModel;
  IdxList, ExistingNames: TStringList;
begin
  IdxList := TStringList.Create;
  ExistingNames := TStringList.Create;
  try
    for I := 0 to Pred(FModels.Count) do
    begin
      Obj := FModels.Items[I];
      ReadIndexNames(Obj.TableName, ExistingNames);
      TDDLBuilder.GetIndexes(Obj, FConnection.Dialect, IdxList, ExistingNames);
    end;

    for J := 0 to Pred(IdxList.Count) do
    begin
      if not IdxList[J].IsEmpty then
        FSQL.Add(IdxList[J]);
    end;
  finally
    ExistingNames.Free;
    IdxList.Free;
  end;
end;

procedure TDeltaORMSchema.EnsureMigrationTable;
var
  VersionType: string;
  DS: TSQLQuery;
  HasName, HasChecksum: Boolean;
  I: Integer;
begin
  if FDBTables.IndexOf(MIGRATION_TABLE) = -1 then
  begin
    if FConnection.Dialect = ddOracle then
      VersionType := 'NUMBER(19)'
    else
      VersionType := 'BIGINT';
    FSQL.Insert(0,
      'CREATE TABLE ' + MIGRATION_TABLE + ' (' +
      'version ' + VersionType + ' NOT NULL PRIMARY KEY, ' +
      'migration_name VARCHAR(255) NOT NULL, ' +
      'checksum VARCHAR(64) NOT NULL, ' +
      'applied_at TIMESTAMP NOT NULL)');
    Exit;
  end;

  HasName := False;
  HasChecksum := False;
  DS := FConnection.NewDataset;
  try
    DS.SQL.Text := 'SELECT * FROM ' + MIGRATION_TABLE + ' WHERE 1 = 0';
    DS.Open;
    for I := 0 to Pred(DS.FieldCount) do
    begin
      if SameText(DS.Fields[I].FieldName, 'migration_name') then
        HasName := True
      else if SameText(DS.Fields[I].FieldName, 'checksum') then
        HasChecksum := True;
    end;
    DS.Close;
  finally
    DS.Free;
  end;

  { Upgrade history tables created by earlier DeltaModel versions. }
  if not HasName then
    FSQL.Insert(0, 'ALTER TABLE ' + MIGRATION_TABLE +
      ' ADD migration_name VARCHAR(255)');
  if not HasChecksum then
    FSQL.Insert(0, 'ALTER TABLE ' + MIGRATION_TABLE +
      ' ADD checksum VARCHAR(64)');
end;

function TDeltaORMSchema.ReadDatabaseVersion: Int64;
var
  DS: TSQLQuery;
begin
  Result := 0;
  DS := FConnection.NewDataset;
  try
    DS.SQL.Text := 'SELECT MAX(version) AS version FROM ' + MIGRATION_TABLE;
    DS.Open;
    if not DS.FieldByName('version').IsNull then
      Result := DS.FieldByName('version').AsLargeInt;
    DS.Close;
  finally
    DS.Free;
  end;
end;

function TDeltaORMSchema.MigrationChecksum(ACount: Integer): string;
var
  I, J: Integer;
  C: Byte;
  HashA, HashB: QWord;
  S: string;
begin
  { Two independent 32-bit rolling hashes provide a stable 16-hex-digit
    fingerprint without adding a cryptography package dependency. }
  HashA := 2166136261;
  HashB := 5381;
  for I := 0 to ACount - 1 do
  begin
    S := FSQL[I] + #10;
    for J := 1 to Length(S) do
    begin
      C := Ord(S[J]);
      HashA := ((HashA and $FFFFFFFF) xor C) * 16777619;
      HashA := HashA and $FFFFFFFF;
      HashB := ((HashB and $FFFFFFFF) * 33) xor C;
      HashB := HashB and $FFFFFFFF;
    end;
  end;
  Result := IntToHex(Int64(HashA), 8) + IntToHex(Int64(HashB), 8);
end;

procedure TDeltaORMSchema.RegisterModel(Model: TDeltaModel);
begin
  FModels.Add(Model);
end;

procedure TDeltaORMSchema.RegisterModel(ModelClass: TDeltaModelClass);
begin
  RegisterModel(ModelClass.Create);
end;

procedure TDeltaORMSchema.PrepareDB(Persist: Boolean);
var
  I: Integer;
  S: string;
  HasMigration: Boolean;
  ModelDDLCount, InfrastructureDDLCount: Integer;
  Checksum, MigrationName: string;
begin
  FSQL.Clear;
  FConstraints.Clear;
  FForeignKeys.Clear;
  FDBTables.Clear;
  FConnection.Connection.GetTableNames(FDBTables);

  FFirebirdMajor := 3;
  if FConnection.Dialect = ddFirebird then
    FFirebirdMajor := TDDLBuilder.ParseFirebirdMajor(VarToStr(
      FConnection.ExecuteScalar('SELECT RDB$GET_CONTEXT(''SYSTEM'', ''ENGINE_VERSION'') FROM RDB$DATABASE')));

  CreateTables;

  AlterTables;

  CreateConstraints;

  CreateIndexes;

  CreateForeignKeys;
  FSQL.AddStrings(FForeignKeys);

  ModelDDLCount := FSQL.Count;
  Checksum := MigrationChecksum(ModelDDLCount);
  HasMigration := FDBTables.IndexOf(MIGRATION_TABLE) = -1;
  EnsureMigrationTable;
  InfrastructureDDLCount := FSQL.Count - ModelDDLCount;

  if Persist and (FSQL.Count > 0) then
  begin
    try
      for I := 0 to Pred(FSQL.Count) do
      begin
        S := FSQL.Strings[I];
        if S.Trim.IsEmpty then Continue;
        FConnection.ExecuteDirect(S);
        { Firebird DML cannot prepare against uncommitted history metadata.
          Commit only history infrastructure here; model DDL and its history
          entry remain together in the following transaction. }
        if (FConnection.Dialect = ddFirebird) and
           (I = InfrastructureDDLCount - 1) and FConnection.TransactionActive then
          FConnection.Commit;
      end;

      if HasMigration then
        FDatabaseVersion := 0
      else
        FDatabaseVersion := ReadDatabaseVersion;

      { The migration table itself is infrastructure. Record one version for
        the complete set of model DDL generated by this PrepareDB call. }
      if ModelDDLCount > 0 then
      begin
        Inc(FDatabaseVersion);
        MigrationName := Format('preparedb_%6.6d', [FDatabaseVersion]);
        FConnection.ExecuteDirect(
          'INSERT INTO ' + MIGRATION_TABLE +
          ' (version, migration_name, checksum, applied_at) VALUES (' +
          IntToStr(FDatabaseVersion) + ', ' + QuotedStr(MigrationName) + ', ' +
          QuotedStr(Checksum) + ', CURRENT_TIMESTAMP)');
      end;

      if FConnection.TransactionActive then
        FConnection.Commit;

      { Keep this schema object reusable for another PrepareDB call. }
      FDBTables.Clear;
      FConnection.Connection.GetTableNames(FDBTables);
    except
      if FConnection.TransactionActive then
        FConnection.Rollback;
      raise;
    end;
  end;

  if Persist and (FSQL.Count = 0) then
    FDatabaseVersion := ReadDatabaseVersion;
end;

constructor TDeltaORMSchema.Create(AConnection: IDeltaORMEngine);
begin
  FConnection := AConnection;
  FModels := TTableList.Create;
  FDBTables := TStringList.Create;
  FConstraints := TStringList.Create;
  FForeignKeys := TStringList.Create;
  FSQL := TStringList.Create;
  FSQL.Delimiter := ';';
  FSQL.StrictDelimiter := True;

  FConnection.Connection.GetTableNames(FDBTables);
end;

destructor TDeltaORMSchema.Destroy;
begin
  FModels.Free;
  FDBTables.Free;
  FSQL.Free;
  FConstraints.Free;
  FForeignKeys.Free;
  inherited Destroy;
end;

end.
