unit DeltaModel.ORM.DDL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Types,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry;

type
  { TDDLBuilder: Dialect-agnostic DDL generator delegating rules to IDeltaDDLDialect }
  TDDLBuilder = class
  protected
    class function FieldDDL(DeltaField: TDeltaField;
      const ADialect: IDeltaDDLDialect;
      const AServerInfo: TDeltaServerInfo): string; static;
    class function PrimitiveFieldDDL(const Name: string; Kind: TTypeKind;
      const ADialect: IDeltaDDLDialect;
      const AServerInfo: TDeltaServerInfo): string; static;
    class procedure GetFieldsCT(Obj: TDeltaModel;
      const ADialect: IDeltaDDLDialect; List: TStrings;
      const AServerInfo: TDeltaServerInfo); static;
    class procedure GetConstraintsCT(Obj: TDeltaModel;
      const ADialect: IDeltaDDLDialect; List: TStrings;
      IncludeForeignKeys: Boolean); static;
    class procedure GetFieldsAT(Obj: TDeltaModel;
      const ADialect: IDeltaDDLDialect;
      List, ActualFieldList, Constraints: TStrings;
      IncludeForeignKeys: Boolean;
      const AServerInfo: TDeltaServerInfo); static;
  public
    class function ServerInfo(AMajorVersion: Integer = 0): TDeltaServerInfo; static;
    class function ReferencedTableName(DeltaField: TDeltaField): string; static;

    class function ForeignKeyDDL(const Table: string; DeltaField: TDeltaField;
      const ADialect: IDeltaDDLDialect): string; overload; static;
    class function ForeignKeyDDL(const Table: string; DeltaField: TDeltaField;
      const ADialectName: string): string; overload; static;

    class function SanitizeIdentifier(const AName: string;
      const ADialect: IDeltaDDLDialect): string; overload; static;
    class function SanitizeIdentifier(const AName, ADialectName: string): string; overload; static;

    class function IndexName(const Table: string; IndexDef: TDeltaIndex;
      const ADialect: IDeltaDDLDialect): string; overload; static;
    class function IndexName(const Table: string; IndexDef: TDeltaIndex;
      const ADialectName: string): string; overload; static;

    class function IndexDDL(const Table: string; IndexDef: TDeltaIndex;
      const ADialect: IDeltaDDLDialect): string; overload; static;
    class function IndexDDL(const Table: string; IndexDef: TDeltaIndex;
      const ADialectName: string): string; overload; static;

    class procedure GetIndexes(Obj: TDeltaModel; const ADialect: IDeltaDDLDialect;
      List: TStrings; ExistingNames: TStrings = nil); overload; static;
    class procedure GetIndexes(Obj: TDeltaModel; const ADialectName: string;
      List: TStrings; ExistingNames: TStrings = nil); overload; static;

    class function CreateTableAndFields(Obj: TDeltaModel; Constraints: TStrings;
      const ADialect: IDeltaDDLDialect; IncludeForeignKeys: Boolean;
      const AServerInfo: TDeltaServerInfo): string; overload; static;
    class function CreateTableAndFields(Obj: TDeltaModel; Constraints: TStrings;
      const ADialectName: string; IncludeForeignKeys: Boolean = True;
      AMajorVersion: Integer = 0): string; overload; static;

    class procedure AddFieldStatements(Obj: TDeltaModel;
      const ADialect: IDeltaDDLDialect;
      ActualFieldList, Constraints, Statements: TStrings;
      IncludeForeignKeys: Boolean;
      const AServerInfo: TDeltaServerInfo); overload; static;
    class procedure AddFieldStatements(Obj: TDeltaModel;
      const ADialectName: string;
      ActualFieldList, Constraints, Statements: TStrings;
      IncludeForeignKeys: Boolean = True;
      AMajorVersion: Integer = 0); overload; static;

    class function CreateFields(Obj: TDeltaModel;
      const ADialect: IDeltaDDLDialect;
      ActualFieldList, Constraints: TStrings;
      IncludeForeignKeys: Boolean;
      const AServerInfo: TDeltaServerInfo): string; overload; static;
    class function CreateFields(Obj: TDeltaModel; const ADialectName: string;
      ActualFieldList, Constraints: TStrings;
      IncludeForeignKeys: Boolean = True;
      AMajorVersion: Integer = 0): string; overload; static;
  end;

implementation

{ TDDLBuilder }

class function TDDLBuilder.ServerInfo(AMajorVersion: Integer): TDeltaServerInfo;
begin
  Result := DefaultServerInfo;
  Result.MajorVersion := AMajorVersion;
end;

class function TDDLBuilder.FieldDDL(DeltaField: TDeltaField;
  const ADialect: IDeltaDDLDialect; const AServerInfo: TDeltaServerInfo): string;
var
  SQLType: string;
  IsPK, IsAutoInc: Boolean;
begin
  IsPK := dboPrimaryKey in DeltaField.DBOptions;
  IsAutoInc := dboAutoInc in DeltaField.DBOptions;
  SQLType := ADialect.ColumnTypeDDL(DeltaField, AServerInfo);
  if SQLType.IsEmpty then
    raise Exception.CreateFmt('Field %s has an invalid datatype for DDL.', [DeltaField.ClassName]);
  Result := ADialect.FormatColumnDDL(DeltaField, SQLType, IsPK, IsAutoInc, AServerInfo);
end;

class function TDDLBuilder.PrimitiveFieldDDL(const Name: string; Kind: TTypeKind;
  const ADialect: IDeltaDDLDialect; const AServerInfo: TDeltaServerInfo): string;
begin
  Result := Format('%s %s NOT NULL', [Name, ADialect.PrimitiveColumnTypeDDL(Kind, AServerInfo)]);
end;

class function TDDLBuilder.ReferencedTableName(DeltaField: TDeltaField): string;
var
  Model: TDeltaModel;
begin
  Result := '';
  if DeltaField.ForeignKey.ReferencesTable = nil then
    Exit;
  if not DeltaField.ForeignKey.ReferencesTable.InheritsFrom(TDeltaModel) then
    raise Exception.Create('Foreign key must reference a TDeltaModel class');
  Model := TDeltaModelClass(DeltaField.ForeignKey.ReferencesTable).Create;
  try
    Result := Model.TableName;
  finally
    Model.Free;
  end;
end;

class function TDDLBuilder.ForeignKeyDDL(const Table: string;
  DeltaField: TDeltaField; const ADialect: IDeltaDDLDialect): string;
var
  RefTable: string;
begin
  if DeltaField.ForeignKey.ReferencesTable = nil then
    Exit('');
  RefTable := ReferencedTableName(DeltaField);
  Result := ADialect.ForeignKeyDDL(
    Table,
    DeltaField.FieldName,
    RefTable,
    DeltaField.ForeignKey.ReferencesField,
    DeltaField.ForeignKey.OnDelete,
    DeltaField.ForeignKey.OnUpdate);
end;

class function TDDLBuilder.ForeignKeyDDL(const Table: string;
  DeltaField: TDeltaField; const ADialectName: string): string;
begin
  Result := ForeignKeyDDL(Table, DeltaField, TDeltaORMRegistry.ResolveDialect(ADialectName).DDL);
end;

class procedure TDDLBuilder.GetFieldsCT(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect; List: TStrings;
  const AServerInfo: TDeltaServerInfo);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  DeltaField: TDeltaField;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount = 0 then
    Exit;
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
        if DeltaField.IsVirtual then
          Continue;
        List.Add(sLineBreak + '  ' + FieldDDL(DeltaField, ADialect, AServerInfo));
      end
      else
        List.Add(sLineBreak + '  ' + PrimitiveFieldDDL(PropInfo^.Name, PropInfo^.PropType^.Kind, ADialect, AServerInfo));
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDDLBuilder.SanitizeIdentifier(const AName: string;
  const ADialect: IDeltaDDLDialect): string;
begin
  Result := ADialect.SanitizeIdentifier(AName);
end;

class function TDDLBuilder.SanitizeIdentifier(const AName,
  ADialectName: string): string;
begin
  Result := SanitizeIdentifier(AName, TDeltaORMRegistry.ResolveDialect(ADialectName).DDL);
end;

class function TDDLBuilder.IndexName(const Table: string; IndexDef: TDeltaIndex;
  const ADialect: IDeltaDDLDialect): string;
var
  IdxName, S: string;
  I: Integer;
begin
  IdxName := IndexDef.Name;
  if IdxName.IsEmpty then
  begin
    S := '';
    for I := 0 to Pred(IndexDef.Fields.Count) do
    begin
      if not S.IsEmpty then
        S := S + '_';
      S := S + IndexDef.Fields[I];
    end;
    if IndexDef.IsUnique then
      IdxName := 'UQ_' + Table + '_' + S
    else
      IdxName := 'IX_' + Table + '_' + S;
  end;
  Result := SanitizeIdentifier(IdxName, ADialect);
end;

class function TDDLBuilder.IndexName(const Table: string; IndexDef: TDeltaIndex;
  const ADialectName: string): string;
begin
  Result := IndexName(Table, IndexDef, TDeltaORMRegistry.ResolveDialect(ADialectName).DDL);
end;

class function TDDLBuilder.IndexDDL(const Table: string; IndexDef: TDeltaIndex;
  const ADialect: IDeltaDDLDialect): string;
var
  IdxName, FieldsList, Prefix: string;
begin
  IdxName := IndexName(Table, IndexDef, ADialect);
  FieldsList := IndexDef.GetFieldsSQL;
  Prefix := ADialect.CreateIndexPrefix(IndexDef.IsUnique);
  Result := Format('%s%s ON %s (%s);', [Prefix, IdxName, Table, FieldsList]);
end;

class function TDDLBuilder.IndexDDL(const Table: string; IndexDef: TDeltaIndex;
  const ADialectName: string): string;
begin
  Result := IndexDDL(Table, IndexDef, TDeltaORMRegistry.ResolveDialect(ADialectName).DDL);
end;

class procedure TDDLBuilder.GetIndexes(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect; List: TStrings; ExistingNames: TStrings);
var
  I: Integer;
  DeltaField: TDeltaField;
  TmpIdx: TDeltaIndex;
begin
  for I := 0 to Pred(Obj.FieldList.Count) do
  begin
    DeltaField := Obj.FieldList[I];
    if DeltaField.IsVirtual then
      Continue;

    if dboIndex in DeltaField.DBOptions then
    begin
      TmpIdx := TDeltaIndex.Create('IX_' + Obj.TableName + '_' + DeltaField.FieldName, [DeltaField.FieldName], False);
      try
        if (ExistingNames = nil) or
           (ExistingNames.IndexOf(IndexName(Obj.TableName, TmpIdx, ADialect)) = -1) then
          List.Add(IndexDDL(Obj.TableName, TmpIdx, ADialect));
      finally
        TmpIdx.Free;
      end;
    end;
  end;

  for I := 0 to Pred(Obj.Indexes.Count) do
  begin
    if (ExistingNames = nil) or
       (ExistingNames.IndexOf(IndexName(Obj.TableName, Obj.Indexes[I], ADialect)) = -1) then
      List.Add(IndexDDL(Obj.TableName, Obj.Indexes[I], ADialect));
  end;
end;

class procedure TDDLBuilder.GetIndexes(Obj: TDeltaModel;
  const ADialectName: string; List: TStrings; ExistingNames: TStrings);
begin
  GetIndexes(Obj, TDeltaORMRegistry.ResolveDialect(ADialectName).DDL, List, ExistingNames);
end;

class procedure TDDLBuilder.GetConstraintsCT(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect; List: TStrings;
  IncludeForeignKeys: Boolean);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I, J: Integer;
  DeltaField: TDeltaField;
  CName, S: string;
  C: TDeltaConstraint;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount > 0 then
  begin
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
        if DeltaField.IsVirtual then
          Continue;

        if IncludeForeignKeys and (DeltaField.ForeignKey.ReferencesTable <> nil) then
        begin
          if not DeltaField.ForeignKey.ReferencesTable.InheritsFrom(TDeltaModel) then
            raise Exception.CreateFmt('%s foreign key references a non TDeltaModel class.', [DeltaField.ClassName]);
          List.Add(ForeignKeyDDL(Obj.TableName, DeltaField, ADialect));
        end;

        if dboUnique in DeltaField.DBOptions then
        begin
          CName := SanitizeIdentifier('UQ_' + Obj.TableName + '_' + DeltaField.FieldName, ADialect);
          if ADialect.InlineConstraintsInCreateTable then
            List.Add(Format('CONSTRAINT %s UNIQUE (%s)', [CName, DeltaField.FieldName]))
          else
            List.Add(Format('ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s);', [Obj.TableName, CName, DeltaField.FieldName]));
        end;
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;

  for I := 0 to Pred(Obj.Constraints.Count) do
  begin
    C := Obj.Constraints[I];
    CName := C.Name;
    if C.Kind = ckUnique then
    begin
      if CName.IsEmpty then
      begin
        S := '';
        for J := 0 to Pred(C.Fields.Count) do
        begin
          if not S.IsEmpty then
            S := S + '_';
          S := S + C.Fields[J];
        end;
        CName := 'UQ_' + Obj.TableName + '_' + S;
      end;
      CName := SanitizeIdentifier(CName, ADialect);

      if ADialect.InlineConstraintsInCreateTable then
        List.Add(Format('CONSTRAINT %s UNIQUE (%s)', [CName, C.GetFieldsSQL]))
      else
        List.Add(Format('ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s);', [Obj.TableName, CName, C.GetFieldsSQL]));
    end
    else if C.Kind = ckCheck then
    begin
      if CName.IsEmpty then
        CName := 'CHK_' + Obj.TableName + '_' + IntToStr(I + 1);
      CName := SanitizeIdentifier(CName, ADialect);

      if ADialect.InlineConstraintsInCreateTable then
        List.Add(Format('CONSTRAINT %s CHECK (%s)', [CName, C.CheckExpression]))
      else
        List.Add(Format('ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s);', [Obj.TableName, CName, C.CheckExpression]));
    end;
  end;
end;

class procedure TDDLBuilder.GetFieldsAT(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect;
  List, ActualFieldList, Constraints: TStrings;
  IncludeForeignKeys: Boolean; const AServerInfo: TDeltaServerInfo);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropCount, I: Integer;
  DeltaField: TDeltaField;
  FieldDef, UniqueName: string;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount = 0 then
    Exit;
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.GetProc = nil then
        Continue;

      if (PropInfo^.PropType^.Kind = tkClass) and
         (TObject(GetObjectProp(Obj, PropInfo)) is TDeltaField) then
      begin
        DeltaField := TDeltaField(GetObjectProp(Obj, PropInfo));
        if DeltaField.IsVirtual then
          Continue;

        if ActualFieldList.IndexOf(DeltaField.FieldName) = -1 then
        begin
          FieldDef := FieldDDL(DeltaField, ADialect, AServerInfo);
          if ADialect.StripNotNullOnAlterAdd and (Pos(' NOT NULL', FieldDef) > 0) then
            FieldDef := StringReplace(FieldDef, ' NOT NULL', '', [rfReplaceAll]);

          List.Add('  ' + FieldDef);
          ADialect.ExtraAutoIncrementStatements(Obj.TableName, DeltaField, AServerInfo, Constraints);

          if IncludeForeignKeys and (DeltaField.ForeignKey.ReferencesTable <> nil) then
          begin
            if DeltaField.ForeignKey.ReferencesTable.InheritsFrom(TDeltaModel) then
              Constraints.Add(ForeignKeyDDL(Obj.TableName, DeltaField, ADialect))
            else
              raise Exception.CreateFmt('%s foreign key references a non TDeltaModel class.', [DeltaField.ClassName]);
          end;

          if dboUnique in DeltaField.DBOptions then
          begin
            UniqueName := SanitizeIdentifier('UQ_' + Obj.TableName + '_' + DeltaField.FieldName, ADialect);
            if ADialect.InlineConstraintsInCreateTable then
              Constraints.Add(Format('%s%s ON %s (%s);',
                [ADialect.CreateIndexPrefix(True), UniqueName, Obj.TableName, DeltaField.FieldName]))
            else
              Constraints.Add(Format('ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s);',
                [Obj.TableName, UniqueName, DeltaField.FieldName]));
          end;
        end;
      end
      else if ActualFieldList.IndexOf(PropInfo^.Name) = -1 then
      begin
        FieldDef := PrimitiveFieldDDL(PropInfo^.Name, PropInfo^.PropType^.Kind, ADialect, AServerInfo);
        if ADialect.StripNotNullOnAlterAdd and (Pos(' NOT NULL', FieldDef) > 0) then
          FieldDef := StringReplace(FieldDef, ' NOT NULL', '', [rfReplaceAll]);
        List.Add('  ' + FieldDef);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

class function TDDLBuilder.CreateTableAndFields(Obj: TDeltaModel;
  Constraints: TStrings; const ADialect: IDeltaDDLDialect;
  IncludeForeignKeys: Boolean; const AServerInfo: TDeltaServerInfo): string;
var
  SQL, Fields: TStringList;
  FieldsBlock, S: string;
  I: Integer;
begin
  SQL := TStringList.Create;
  Fields := TStringList.Create;
  try
    GetFieldsCT(Obj, ADialect, Fields, AServerInfo);

    if ADialect.InlineConstraintsInCreateTable then
      GetConstraintsCT(Obj, ADialect, Fields, True);

    FieldsBlock := '';
    for I := 0 to Fields.Count - 1 do
    begin
      S := Trim(Fields[I]);
      if S.IsEmpty then
        Continue;
      if not FieldsBlock.IsEmpty then
        FieldsBlock := FieldsBlock + ',' + sLineBreak;
      FieldsBlock := FieldsBlock + '  ' + S;
    end;

    SQL.Add(
      ADialect.CreateTablePrefix + Obj.TableName + ' (' + sLineBreak +
      FieldsBlock + sLineBreak +
      ');'
    );

    if Assigned(Constraints) and (not ADialect.InlineConstraintsInCreateTable) then
      GetConstraintsCT(Obj, ADialect, Constraints, IncludeForeignKeys);

    if Assigned(Constraints) then
      for I := 0 to Obj.FieldList.Count - 1 do
        if not Obj.FieldList[I].IsVirtual then
          ADialect.ExtraAutoIncrementStatements(Obj.TableName, Obj.FieldList[I], AServerInfo, Constraints);

    Result := SQL.Text;
  finally
    SQL.Free;
    Fields.Free;
  end;
end;

class function TDDLBuilder.CreateTableAndFields(Obj: TDeltaModel;
  Constraints: TStrings; const ADialectName: string;
  IncludeForeignKeys: Boolean; AMajorVersion: Integer): string;
begin
  Result := CreateTableAndFields(Obj, Constraints,
    TDeltaORMRegistry.ResolveDialect(ADialectName).DDL,
    IncludeForeignKeys, ServerInfo(AMajorVersion));
end;

class procedure TDDLBuilder.AddFieldStatements(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect;
  ActualFieldList, Constraints, Statements: TStrings;
  IncludeForeignKeys: Boolean; const AServerInfo: TDeltaServerInfo);
var
  Fields: TStringList;
  I: Integer;
begin
  Fields := TStringList.Create;
  try
    GetFieldsAT(Obj, ADialect, Fields, ActualFieldList, Constraints, IncludeForeignKeys, AServerInfo);
    for I := 0 to Fields.Count - 1 do
      Statements.Add('ALTER TABLE ' + Obj.TableName + ' ADD ' + Fields[I] + ';');
  finally
    Fields.Free;
  end;
end;

class procedure TDDLBuilder.AddFieldStatements(Obj: TDeltaModel;
  const ADialectName: string;
  ActualFieldList, Constraints, Statements: TStrings;
  IncludeForeignKeys: Boolean; AMajorVersion: Integer);
begin
  AddFieldStatements(Obj,
    TDeltaORMRegistry.ResolveDialect(ADialectName).DDL,
    ActualFieldList, Constraints, Statements,
    IncludeForeignKeys, ServerInfo(AMajorVersion));
end;

class function TDDLBuilder.CreateFields(Obj: TDeltaModel;
  const ADialect: IDeltaDDLDialect;
  ActualFieldList, Constraints: TStrings;
  IncludeForeignKeys: Boolean; const AServerInfo: TDeltaServerInfo): string;
var
  SQL: TStringList;
begin
  SQL := TStringList.Create;
  try
    AddFieldStatements(Obj, ADialect, ActualFieldList, Constraints, SQL, IncludeForeignKeys, AServerInfo);
    Result := SQL.Text;
  finally
    SQL.Free;
  end;
end;

class function TDDLBuilder.CreateFields(Obj: TDeltaModel;
  const ADialectName: string;
  ActualFieldList, Constraints: TStrings;
  IncludeForeignKeys: Boolean; AMajorVersion: Integer): string;
begin
  Result := CreateFields(Obj,
    TDeltaORMRegistry.ResolveDialect(ADialectName).DDL,
    ActualFieldList, Constraints, IncludeForeignKeys, ServerInfo(AMajorVersion));
end;

end.
