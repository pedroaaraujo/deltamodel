unit DeltaModel.ORM.DDL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Types;

type

  { TDDLBuilder }

  TDDLBuilder = class
  protected
    class function FieldDDL(DeltaField: TDeltaField; ADialect: TDatabaseDialect): string;
    class function ForeignKeyDDL(const Table: string; DeltaField: TDeltaField;
      ADialect: TDatabaseDialect): string;
    class function PrimitiveFieldDDL(Name: string; Kind: TTypeKind; ADialect: TDatabaseDialect): string;
    class procedure GetFieldsCT(Obj: TDeltaModel; ADialect: TDatabaseDialect; List: TStrings);
    class procedure GetConstraintsCT(Obj: TDeltaModel; ADialect: TDatabaseDialect; List: TStrings);
    class procedure GetFieldsAT(Obj: TDeltaModel; ADialect: TDatabaseDialect; List, ActualFieldList, Constraints: TStrings);
  public
    class function CreateTableAndFields(Obj: TDeltaModel; Constraints: TStrings; ADialect: TDatabaseDialect): string;
    class function CreateFields(Obj: TDeltaModel; ADialect: TDatabaseDialect; ActualFieldList, Constraints: TStrings): string;
  end;

implementation

{ TDDLBuilder }

class function TDDLBuilder.FieldDDL(DeltaField: TDeltaField;
  ADialect: TDatabaseDialect): string;
var
  SQLType, AutoIncClause, NotNullClause: string;
  Size: Integer;
  IsAutoInc, IsPK: Boolean;
begin
  SQLType := EmptyStr;
  AutoIncClause := EmptyStr;
  IsPK := dboPrimaryKey in DeltaField.DBOptions;
  IsAutoInc := dboAutoInc in DeltaField.DBOptions;

  // Inteiro 32 bits
  if (DeltaField is TDFIntNull) or (DeltaField is TDFIntRequired) then
  begin
    case ADialect of
      ddOracle: SQLType := 'NUMBER(10)';
    else
      SQLType := 'INTEGER';
    end;
  end
  else
  // Inteiro 64 bits
  if (DeltaField is TDFInt64Null) or (DeltaField is TDFInt64Required) then
  begin
    case ADialect of
      ddOracle: SQLType := 'NUMBER(19)';
    else
      SQLType := 'BIGINT';
    end;
  end
  else
  // Moeda / Decimal
  if (DeltaField is TDFCurrencyNull) or (DeltaField is TDFCurrencyRequired) then
  begin
    case ADialect of
      ddMySQL, ddMSSQL, ddOracle:
        SQLType := 'DECIMAL(18,4)';
      ddSQLite:
        SQLType := 'REAL';
      ddFirebird, ddPostgreSQL:
        SQLType := 'DOUBLE PRECISION';
    end;
  end
  else
  // Ponto flutuante
  if (DeltaField is TDFDoubleNull) or (DeltaField is TDFDoubleRequired) then
  begin
    case ADialect of
      ddMySQL:
        SQLType := 'DOUBLE';
      ddMSSQL:
        SQLType := 'FLOAT';
      ddOracle:
        SQLType := 'BINARY_DOUBLE';
      ddSQLite:
        SQLType := 'REAL';
      ddFirebird, ddPostgreSQL:
        SQLType := 'DOUBLE PRECISION';
    end;
  end
  else
  // Data
  if (DeltaField is TDFDateNull) or (DeltaField is TDFDateRequired) then
  begin
    SQLType := 'DATE';
  end
  else
  // Hora
  if (DeltaField is TDFTimeNull) or (DeltaField is TDFTimeRequired) then
  begin
    case ADialect of
      ddOracle: SQLType := 'VARCHAR2(8)';
    else
      SQLType := 'TIME';
    end;
  end
  else
  // Data e Hora
  if (DeltaField is TDFDateTimeNull) or (DeltaField is TDFDateTimeRequired) then
  begin
    case ADialect of
      ddFirebird, ddPostgreSQL, ddOracle:
        SQLType := 'TIMESTAMP';
      ddMSSQL:
        SQLType := 'DATETIME2';
      ddSQLite, ddMySQL:
        SQLType := 'DATETIME';
    end;
  end
  else
  // Booleano
  if (DeltaField is TDFBooleanNull) or (DeltaField is TDFBooleanRequired) then
  begin
    case ADialect of
      ddPostgreSQL, ddFirebird:
        SQLType := 'BOOLEAN';
      ddMySQL:
        SQLType := 'TINYINT(1)';
      ddMSSQL:
        SQLType := 'BIT';
      ddOracle:
        SQLType := 'NUMBER(1)';
      ddSQLite:
        SQLType := 'INTEGER';
    end;
  end
  else
  // UUID
  if (DeltaField is TDFUUIDNull) or (DeltaField is TDFUUIDRequired) then
  begin
    case ADialect of
      ddPostgreSQL:
        SQLType := 'UUID';
      ddOracle:
        SQLType := 'VARCHAR2(36)';
    else
      SQLType := 'VARCHAR(36)';
    end;
  end
  else
  // String
  if (DeltaField is TDFStringNull) or (DeltaField is TDFStringRequired) then
  begin
    if (DeltaField is TDFStringNull) then
      Size := (DeltaField as TDFStringNull).Size
    else
      Size := (DeltaField as TDFStringRequired).Size;

    case ADialect of
      ddSQLite:
        SQLType := 'TEXT';
      ddOracle:
        SQLType := Format('VARCHAR2(%d)', [Size]);
    else
      SQLType := Format('VARCHAR(%d)', [Size]);
    end;
  end;

  if SQLType.IsEmpty then
    raise Exception.CreateFmt('Field %s has an invalid datatype for DDL.', [DeltaField.ClassName]);

  // Tratamento de Chave Primária e Auto-Incremento por dialeto
  if IsPK and IsAutoInc then
  begin
    case ADialect of
      ddPostgreSQL:
      begin
        if (DeltaField is TDFInt64Null) or (DeltaField is TDFInt64Required) then
          SQLType := 'BIGSERIAL'
        else
          SQLType := 'SERIAL';
        AutoIncClause := 'PRIMARY KEY NOT NULL';
      end;
      ddMySQL:
        AutoIncClause := 'NOT NULL AUTO_INCREMENT PRIMARY KEY';
      ddMSSQL:
        AutoIncClause := 'IDENTITY(1,1) PRIMARY KEY NOT NULL';
      ddSQLite:
      begin
        SQLType := 'INTEGER';
        AutoIncClause := 'PRIMARY KEY AUTOINCREMENT NOT NULL';
      end;
      ddFirebird:
        AutoIncClause := 'GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY NOT NULL';
      ddOracle:
        AutoIncClause := 'GENERATED ALWAYS AS IDENTITY PRIMARY KEY NOT NULL';
    end;

    Result := Format('%s %s %s', [DeltaField.FieldName, SQLType, AutoIncClause]);
  end
  else
  if IsPK then
  begin
    Result := Format('%s %s PRIMARY KEY NOT NULL', [DeltaField.FieldName, SQLType]);
  end
  else
  if (DeltaField is TDeltaFieldNullable) then
  begin
    Result := Format('%s %s', [DeltaField.FieldName, SQLType]);
  end
  else
  begin
    NotNullClause := 'NOT NULL';
    Result := Format('%s %s %s', [DeltaField.FieldName, SQLType, NotNullClause]);
  end;
end;

class function TDDLBuilder.ForeignKeyDDL(const Table: string; DeltaField: TDeltaField;
  ADialect: TDatabaseDialect): string;
var
  FKName, RefTable, RefField: string;
  OnDeleteClause, OnUpdateClause: string;
  Obj: TObject;
begin
  if ADialect in [ddSQLite] then
  begin
    // SQLite não suporta ALTER TABLE ADD CONSTRAINT FOREIGN KEY
    Exit('');
  end;

  RefField := DeltaField.ForeignKey.ReferencesField;
  if DeltaField.ForeignKey.ReferencesTable = nil then Exit('');

  Obj := DeltaField.ForeignKey.ReferencesTable.Create;
  try
    RefTable := (Obj as TDeltaModel).TableName;
  finally
    Obj.Free;
  end;

  case DeltaField.ForeignKey.OnDelete of
    fkCascade:  OnDeleteClause := ' ON DELETE CASCADE';
    fkSetNull:  OnDeleteClause := ' ON DELETE SET NULL';
    fkRestrict: OnDeleteClause := ' ON DELETE RESTRICT';
    fkNone:     OnDeleteClause := ' ON DELETE NO ACTION';
  else
    OnDeleteClause := '';
  end;

  // Oracle não suporta ON UPDATE em Foreign Keys
  if ADialect = ddOracle then
    OnUpdateClause := ''
  else
  begin
    case DeltaField.ForeignKey.OnUpdate of
      fkCascade:  OnUpdateClause := ' ON UPDATE CASCADE';
      fkSetNull:  OnUpdateClause := ' ON UPDATE SET NULL';
      fkRestrict: OnUpdateClause := ' ON UPDATE RESTRICT';
      fkNone:     OnUpdateClause := ' ON UPDATE NO ACTION';
    else
      OnUpdateClause := '';
    end;
  end;

  FKName := Format('FK_%s_%s', [Table, RefTable]);
  FKName := Copy(FKName, 1, 30); // Limite de 30 caracteres para Oracle e compatibilidade

  Result := Format(
    'ALTER TABLE %s ADD CONSTRAINT %s ' +
    'FOREIGN KEY (%s) REFERENCES %s(%s)%s%s',
    [Table, FKName, DeltaField.FieldName, RefTable, RefField, OnDeleteClause, OnUpdateClause]
  );
end;

class function TDDLBuilder.PrimitiveFieldDDL(Name: string; Kind: TTypeKind;
  ADialect: TDatabaseDialect): string;
var
  SQLType: string;
begin
  case Kind of
    tkInteger:
    begin
      case ADialect of
        ddOracle: SQLType := 'NUMBER(10)';
      else
        SQLType := 'INTEGER';
      end;
    end;

    tkInt64:
    begin
      case ADialect of
        ddOracle: SQLType := 'NUMBER(19)';
      else
        SQLType := 'BIGINT';
      end;
    end;

    tkEnumeration, tkBool:
    begin
      case ADialect of
        ddPostgreSQL, ddFirebird: SQLType := 'BOOLEAN';
        ddMySQL: SQLType := 'TINYINT(1)';
        ddMSSQL: SQLType := 'BIT';
        ddOracle: SQLType := 'NUMBER(1)';
        ddSQLite: SQLType := 'INTEGER';
      end;
    end;

    tkFloat:
    begin
      case ADialect of
        ddMySQL: SQLType := 'DOUBLE';
        ddMSSQL: SQLType := 'FLOAT';
        ddOracle: SQLType := 'BINARY_DOUBLE';
        ddSQLite: SQLType := 'REAL';
        ddFirebird, ddPostgreSQL: SQLType := 'DOUBLE PRECISION';
      end;
    end;
  else
    case ADialect of
      ddSQLite: SQLType := 'TEXT';
      ddOracle: SQLType := 'VARCHAR2(255)';
    else
      SQLType := 'VARCHAR(255)';
    end;
  end;

  Result := Format('%s %s NOT NULL', [Name, SQLType]);
end;

class procedure TDDLBuilder.GetFieldsCT(Obj: TDeltaModel;
  ADialect: TDatabaseDialect; List: TStrings);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  DeltaField: TDeltaField;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if (PropInfo^.PropType^.Kind = tkClass) and
         (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
      begin
        DeltaField := TDeltaField(GetObjectProp(Obj, PropInfo));
        List.Add(sLineBreak + '  ' + FieldDDL(DeltaField, ADialect));
      end
      else
      begin
        List.Add(sLineBreak + '  ' + PrimitiveFieldDDL(PropInfo^.Name, PropInfo^.PropType^.Kind, ADialect));
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class procedure TDDLBuilder.GetConstraintsCT(Obj: TDeltaModel;
  ADialect: TDatabaseDialect; List: TStrings);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  DeltaField: TDeltaField;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if not (PropInfo^.PropType^.Kind = tkClass) then
        Continue;

      if not (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
        Continue;

      DeltaField := TDeltaField(GetObjectProp(Obj, PropInfo));

      if (DeltaField.ForeignKey.ReferencesTable <> nil) then
      begin
        if not DeltaField.ForeignKey.ReferencesTable.InheritsFrom(TDeltaModel) then
          raise Exception.CreateFmt('%s foreign key references a non TDeltaModel class.', [DeltaField.ClassName]);

        List.Add(ForeignKeyDDL(Obj.TableName, DeltaField, ADialect));
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class procedure TDDLBuilder.GetFieldsAT(Obj: TDeltaModel;
  ADialect: TDatabaseDialect; List, ActualFieldList, Constraints: TStrings);
const
  COLUMN = sLineBreak + '  COLUMN ';
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  DeltaField: TDeltaField;
  FieldDef: string;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount = 0 then Exit;
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      if (PropInfo^.PropType^.Kind = tkClass) and
         (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
      begin
        DeltaField := TDeltaField(GetObjectProp(Obj, PropInfo));
        if ActualFieldList.IndexOf(DeltaField.FieldName) = -1 then
        begin
          FieldDef := FieldDDL(DeltaField, ADialect);
          if (ADialect = ddSQLite) and (Pos(' NOT NULL', FieldDef) > 0) then
            FieldDef := StringReplace(FieldDef, ' NOT NULL', '', [rfReplaceAll]);
          List.Add(COLUMN + FieldDef);

          if (DeltaField.ForeignKey.ReferencesTable <> nil) then
          begin
            if DeltaField.ForeignKey.ReferencesTable.InheritsFrom(TDeltaModel) then
              Constraints.Add(ForeignKeyDDL(Obj.TableName, DeltaField, ADialect))
            else
              raise Exception.CreateFmt('%s foreign key references a non TDeltaModel class.', [DeltaField.ClassName]);
          end;
        end;
      end
      else
      begin
        if ActualFieldList.IndexOf(PropInfo^.Name) = -1 then
        begin
          FieldDef := PrimitiveFieldDDL(PropInfo^.Name, PropInfo^.PropType^.Kind, ADialect);
          if (ADialect = ddSQLite) and (Pos(' NOT NULL', FieldDef) > 0) then
            FieldDef := StringReplace(FieldDef, ' NOT NULL', '', [rfReplaceAll]);
          List.Add(COLUMN + FieldDef);
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDDLBuilder.CreateTableAndFields(Obj: TDeltaModel;
  Constraints: TStrings; ADialect: TDatabaseDialect): string;
var
  SQL, Fields: TStringList;
  Option: string;
begin
  SQL := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Delimiter := ',';
    Fields.StrictDelimiter := True;
    GetFieldsCT(Obj, ADialect, Fields);

    case ADialect of
      ddSQLite, ddPostgreSQL, ddMySQL:
        Option := 'IF NOT EXISTS ';
    else
      Option := EmptyStr;
    end;

    SQL.Add(
      'CREATE TABLE ' + Option + Obj.TableName + ' (' +
      Fields.DelimitedText + sLineBreak +
      ');'
    );

    // Se fornecido Constraints, adiciona as FKs
    if Assigned(Constraints) then
      GetConstraintsCT(Obj, ADialect, Constraints);

    Result := SQL.Text;
  finally
    SQL.Free;
    Fields.Free;
  end;
end;

class function TDDLBuilder.CreateFields(Obj: TDeltaModel;
  ADialect: TDatabaseDialect; ActualFieldList, Constraints: TStrings): string;
var
  SQL, Fields: TStringList;
  I: Integer;
begin
  Result := EmptyStr;

  SQL := TStringList.Create;
  Fields := TStringList.Create;
  try
    GetFieldsAT(Obj, ADialect, Fields, ActualFieldList, Constraints);

    for I := 0 to Pred(Fields.Count) do
    begin
      SQL.Add(
        'ALTER TABLE ' + Obj.TableName +
        ' ADD ' + Fields[I] + ';'
      );
    end;

    if Fields.Count > 0 then
      Result := SQL.Text;
  finally
    SQL.Free;
    Fields.Free;
  end;
end;

end.
