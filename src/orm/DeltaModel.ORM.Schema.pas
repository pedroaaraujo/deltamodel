unit DeltaModel.ORM.Schema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, TypInfo, fgl,
  DeltaModel, DeltaModel.Fields,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DeltaModel.ORM.DDL;

type
  { TDeltaORMSchema: Dialect-agnostic database migration planner and runner }
  TDeltaORMSchema = class
  private
    type TTableList = specialize TFPGObjectList<TDeltaModel>;
  private
    FConnection: IDeltaConnection;
    FModels: TTableList;
    FDBTables: TStringList;
    FSQL: TStringList;
    FConstraints: TStringList;
    FForeignKeys: TStringList;
    FDatabaseVersion: Int64;
    FServerInfo: TDeltaServerInfo;
    procedure CreateTables;
    procedure AlterTables;
    procedure CreateConstraints;
    procedure CreateForeignKeys;
    procedure CreateIndexes;
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
    constructor Create(const AConnection: IDeltaConnection);
    destructor Destroy; override;
  end;

implementation

const
  MIGRATION_TABLE = 'deltamodel_schema_migrations';

{ TDeltaORMSchema }

constructor TDeltaORMSchema.Create(const AConnection: IDeltaConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FModels := TTableList.Create;
  FDBTables := TStringList.Create;
  FConstraints := TStringList.Create;
  FForeignKeys := TStringList.Create;
  FSQL := TStringList.Create;
  FSQL.Delimiter := ';';
  FSQL.StrictDelimiter := True;
  FServerInfo := DefaultServerInfo;

  if FConnection <> nil then
    FConnection.GetTableNames(FDBTables);
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

procedure TDeltaORMSchema.CreateTables;
var
  I: Integer;
  Obj: TDeltaModel;
  DDL: IDeltaDDLDialect;
begin
  DDL := FConnection.Dialect.DDL;
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    if FDBTables.IndexOf(Obj.TableName) > -1 then
      Continue;

    FSQL.Add(TDDLBuilder.CreateTableAndFields(Obj, FConstraints, DDL, False, FServerInfo));
  end;
end;

procedure TDeltaORMSchema.AlterTables;
var
  I, F: Integer;
  Obj: TDeltaModel;
  FieldList: TStringList;
  RS: IDeltaResultSet;
  DDL: IDeltaDDLDialect;
begin
  DDL := FConnection.Dialect.DDL;
  for I := 0 to Pred(FModels.Count) do
  begin
    Obj := FModels.Items[I];
    if FDBTables.IndexOf(Obj.TableName) = -1 then
      Continue;

    FieldList := TStringList.Create;
    try
      RS := FConnection.ExecuteQuery(
        'SELECT * FROM ' + Obj.TableName + sLineBreak + 'WHERE 1 = 0');
      try
        for F := 0 to RS.FieldCount - 1 do
          FieldList.Add(RS.FieldName(F));
      finally
        RS.Close;
      end;

      TDDLBuilder.AddFieldStatements(Obj, DDL, FieldList, FConstraints, FSQL, False, FServerInfo);
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
      FSQL.Add(S);
  end;
end;

procedure TDeltaORMSchema.CreateForeignKeys;
var
  I, J, Count: Integer;
  Props: PPropList;
  Obj: TDeltaModel;
  Value: TObject;
  Field: TDeltaField;
  DDL: IDeltaDDLDialect;
  Intro: IDeltaSchemaIntrospector;
  RefTable: string;
begin
  DDL := FConnection.Dialect.DDL;
  Intro := FConnection.Dialect.Introspector;

  for I := 0 to FModels.Count - 1 do
  begin
    Obj := FModels[I];
    if DDL.InlineConstraintsInCreateTable and (FDBTables.IndexOf(Obj.TableName) = -1) then
      Continue;

    Count := GetPropList(Obj.ClassInfo, tkProperties, nil);
    if Count = 0 then
      Continue;

    GetMem(Props, Count * SizeOf(Pointer));
    try
      GetPropList(Obj.ClassInfo, tkProperties, Props, False);
      for J := 0 to Count - 1 do
      begin
        if (Props^[J]^.PropType^.Kind <> tkClass) or (Props^[J]^.GetProc = nil) then
          Continue;
        Value := GetObjectProp(Obj, Props^[J]);
        if not (Value is TDeltaField) then
          Continue;
        Field := TDeltaField(Value);
        if Field.IsVirtual or (Field.ForeignKey.ReferencesTable = nil) then
          Continue;

        if FDBTables.IndexOf(Obj.TableName) > -1 then
        begin
          RefTable := TDDLBuilder.ReferencedTableName(Field);
          if Intro.ForeignKeyExists(FConnection, Obj.TableName, Field.FieldName,
               RefTable, Field.ForeignKey.ReferencesField) then
            Continue;
        end;

        if not DDL.SupportsAlterAddForeignKey then
          raise Exception.CreateFmt(
            '%s: adding foreign key %s.%s requires rebuilding the table. No migration was executed.',
            [FConnection.Dialect.Name, Obj.TableName, Field.FieldName]);

        FForeignKeys.Add(TDDLBuilder.ForeignKeyDDL(Obj.TableName, Field, DDL));
      end;
    finally
      FreeMem(Props);
    end;
  end;
end;

procedure TDeltaORMSchema.CreateIndexes;
var
  I, J: Integer;
  Obj: TDeltaModel;
  IdxList, ExistingNames: TStringList;
  DDL: IDeltaDDLDialect;
  Intro: IDeltaSchemaIntrospector;
begin
  DDL := FConnection.Dialect.DDL;
  Intro := FConnection.Dialect.Introspector;
  IdxList := TStringList.Create;
  ExistingNames := TStringList.Create;
  try
    for I := 0 to Pred(FModels.Count) do
    begin
      Obj := FModels.Items[I];
      if FDBTables.IndexOf(Obj.TableName) > -1 then
        Intro.GetIndexNames(FConnection, Obj.TableName, ExistingNames)
      else
        ExistingNames.Clear;

      TDDLBuilder.GetIndexes(Obj, DDL, IdxList, ExistingNames);
    end;

    for J := 0 to Pred(IdxList.Count) do
      if not IdxList[J].IsEmpty then
        FSQL.Add(IdxList[J]);
  finally
    ExistingNames.Free;
    IdxList.Free;
  end;
end;

procedure TDeltaORMSchema.EnsureMigrationTable;
var
  VersionType: string;
  RS: IDeltaResultSet;
  HasName, HasChecksum: Boolean;
  I: Integer;
begin
  if FDBTables.IndexOf(MIGRATION_TABLE) = -1 then
  begin
    VersionType := FConnection.Dialect.DDL.MigrationVersionColumnType;
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
  RS := FConnection.ExecuteQuery('SELECT * FROM ' + MIGRATION_TABLE + ' WHERE 1 = 0');
  try
    for I := 0 to RS.FieldCount - 1 do
    begin
      if SameText(RS.FieldName(I), 'migration_name') then
        HasName := True
      else if SameText(RS.FieldName(I), 'checksum') then
        HasChecksum := True;
    end;
  finally
    RS.Close;
  end;

  if not HasName then
    FSQL.Insert(0, 'ALTER TABLE ' + MIGRATION_TABLE + ' ADD migration_name VARCHAR(255)');
  if not HasChecksum then
    FSQL.Insert(0, 'ALTER TABLE ' + MIGRATION_TABLE + ' ADD checksum VARCHAR(64)');
end;

function TDeltaORMSchema.ReadDatabaseVersion: Int64;
var
  Val: Variant;
begin
  Result := 0;
  Val := FConnection.ExecuteScalar('SELECT MAX(version) AS version FROM ' + MIGRATION_TABLE);
  if not (VarIsNull(Val) or VarIsEmpty(Val)) then
    Result := Int64(Val);
end;

function TDeltaORMSchema.MigrationChecksum(ACount: Integer): string;
var
  I, J: Integer;
  C: Byte;
  HashA, HashB: QWord;
  S: string;
begin
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
  DDL: IDeltaDDLDialect;
begin
  DDL := FConnection.Dialect.DDL;

  FSQL.Clear;
  FConstraints.Clear;
  FForeignKeys.Clear;
  FDBTables.Clear;
  FConnection.GetTableNames(FDBTables);
  FServerInfo := FConnection.Dialect.Introspector.DetectServerInfo(FConnection);

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
        if S.Trim.IsEmpty then
          Continue;
        FConnection.ExecuteDirect(S);
        if DDL.RequiresCommitAfterInfraDDL and
           (I = InfrastructureDDLCount - 1) and
           FConnection.TransactionActive then
          FConnection.Commit;
      end;

      if HasMigration then
        FDatabaseVersion := 0
      else
        FDatabaseVersion := ReadDatabaseVersion;

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

      FDBTables.Clear;
      FConnection.GetTableNames(FDBTables);
    except
      if FConnection.TransactionActive then
        FConnection.Rollback;
      raise;
    end;
  end;

  if Persist and (FSQL.Count = 0) then
    FDatabaseVersion := ReadDatabaseVersion;
end;

end.
