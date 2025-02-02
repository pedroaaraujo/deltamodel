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
    class function PrimitiveFieldDDL(Name: string; Kind: TTypeKind; ADialect: TDatabaseDialect): string;
    class procedure GetFieldsCT(Obj: TDeltaModel; ADialect: TDatabaseDialect; List: TStrings);
    class procedure GetFieldsAT(Obj: TDeltaModel; ADialect: TDatabaseDialect; List: TStrings);
  public
    class function CreateTableAndFields(Obj: TDeltaModel; ADialect: TDatabaseDialect): string;
    class function CreateFields(Obj: TDeltaModel; ADialect: TDatabaseDialect): string;
  end;

implementation

{ TDDLBuilder }

class function TDDLBuilder.FieldDDL(DeltaField: TDeltaField;
  ADialect: TDatabaseDialect): string;
var
  SQLType: string;
  Size: Integer;
begin
  SQLType := EmptyStr;

  if (DeltaField is TDFIntNull) or (DeltaField is TDFIntRequired) then
  begin
    SQLType := 'INTEGER';
  end
  else
  if (DeltaField is TDFCurrencyNull) or (DeltaField is TDFCurrencyRequired) or
     (DeltaField is TDFDoubleNull) or (DeltaField is TDFDoubleRequired) then
  begin
    case ADialect of
      ddFirebird: SQLType   := 'DOUBLE PRECISION';
      ddSQLite: SQLType     := 'REAL';
      ddPostgreSQL: SQLType := 'DOUBLE PRECISION';
    end;
  end
  else
  if (DeltaField is TDFStringNull) or (DeltaField is TDFStringRequired) then
  begin
    if (DeltaField is TDFStringNull) then
    begin
      Size := (DeltaField as TDFStringNull).Size;
    end
    else
    begin
      Size := (DeltaField as TDFStringRequired).Size;
    end;

    case ADialect of
      ddSQLite: SQLType := 'TEXT';
    else
      SQLType := Format('VARCHAR(%d)', [Size]);
    end;
  end;

  if SQLType.IsEmpty then
  begin
    raise Exception.CreateFmt('Field %s has an invalid datatype for DDL.', [DeltaField.ClassName]);
  end;

  if (dboPrimaryKey in DeltaField.DBOptions) then
  begin
    SQLType := Format('%s PRIMARY KEY', [SQLType]);
    Result := Format('%s %s NOT NULL', [DeltaField.FieldName, SQLType]);
  end
  else
  if (DeltaField is TDeltaFieldNullable) then
  begin
    Result := Format('%s %s', [DeltaField.FieldName, SQLType]);
  end
  else
  begin
    Result := Format('%s %s NOT NULL', [DeltaField.FieldName, SQLType]);
  end;
end;

class function TDDLBuilder.PrimitiveFieldDDL(Name: string; Kind: TTypeKind;
  ADialect: TDatabaseDialect): string;
var
  SQLType: string;
begin
  case Kind of
    tkInteger, tkInt64, tkEnumeration:
    begin
      SQLType := 'INTEGER';
    end;

    tkBool:
    begin
      SQLType := 'BOOLEAN';
    end;

    tkFloat:
    begin
      case ADialect of
        ddFirebird: SQLType   := 'DOUBLE PRECISION';
        ddSQLite: SQLType     := 'REAL';
        ddPostgreSQL: SQLType := 'DOUBLE PRECISION';
      end;
    end;
  else
    case ADialect of
      ddSQLite: SQLType := 'TEXT';
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
      if (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
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

class procedure TDDLBuilder.GetFieldsAT(Obj: TDeltaModel;
  ADialect: TDatabaseDialect; List: TStrings);
const
  ADD_COLUMN = sLineBreak + '  ADD COLUMN ';
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
      if (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
      begin
        DeltaField := TDeltaField(GetObjectProp(Obj, PropInfo));
        List.Add(ADD_COLUMN + FieldDDL(DeltaField, ADialect));
      end
      else
      begin
        List.Add(ADD_COLUMN + PrimitiveFieldDDL(PropInfo^.Name, PropInfo^.PropType^.Kind, ADialect));
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDDLBuilder.CreateTableAndFields(Obj: TDeltaModel;
  ADialect: TDatabaseDialect): string;
var
  SQL, Fields: TStringList;
begin
  SQL := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Delimiter := ',';
    Fields.StrictDelimiter := True;
    GetFieldsCT(Obj, ADialect, Fields);

    SQL.Add(
      'CREATE TABLE ' + Obj.TableName + ' (' +
      Fields.DelimitedText +
      ');'
    );

    Result := SQL.Text;
  finally
    SQL.Free;
    Fields.Free;
  end;
end;

class function TDDLBuilder.CreateFields(Obj: TDeltaModel;
  ADialect: TDatabaseDialect): string;
var
  SQL, Fields: TStringList;
begin
  SQL := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Delimiter := ',';
    Fields.StrictDelimiter := True;
    GetFieldsCT(Obj, ADialect, Fields);

    SQL.Add('ALTER TABLE ' + Obj.TableName);
    SQL.Add(Fields.DelimitedText);
    SQL.Add(';');

    Result := SQL.Text;
  finally
    SQL.Free;
    Fields.Free;
  end;
end;

end.

