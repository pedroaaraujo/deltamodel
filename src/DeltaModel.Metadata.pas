unit DeltaModel.Metadata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SyncObjs, DeltaModel.Fields;

type
  { Representa os metadados de uma propriedade RTTI em cache }
  PDeltaPropItem = ^TDeltaPropItem;
  TDeltaPropItem = record
    PropInfo: PPropInfo;
    Name: string;
    PropKind: TTypeKind;
    HasGetProc: Boolean;
    HasSetProc: Boolean;
    IsClass: Boolean;
    ClassType: TClass;
    IsDeltaField: Boolean;
    IsDeltaModelList: Boolean;
  end;
  TDeltaPropItemArray = array of TDeltaPropItem;

  { Cache de propriedades RTTI de uma classe }
  TRTTIClassInfo = class
  private
    FClassType: TClass;
    FProps: TDeltaPropItemArray;
    FPropCount: Integer;
    procedure BuildProps;
  public
    constructor Create(AClass: TClass);
    function FindProp(const AName: string): PDeltaPropItem;
    property ClassType: TClass read FClassType;
    property Props: TDeltaPropItemArray read FProps;
    property PropCount: Integer read FPropCount;
  end;

  { Metadados específicos de um campo TDeltaField dentro de TDeltaModel }
  PDeltaFieldMeta = ^TDeltaFieldMeta;
  TDeltaFieldMeta = record
    PropInfo: PPropInfo;
    PropName: string;
    FieldName: string;
    IsPrimaryKey: Boolean;
    IsAutoInc: Boolean;
    IsRequired: Boolean;
    FieldClass: TClass;
    FieldKind: TDeltaFieldKind;
    IsVirtual: Boolean;
  end;
  TDeltaFieldMetaArray = array of TDeltaFieldMeta;

  { Metadados de persistência de um TDeltaModel }
  TModelMetadata = class
  private
    FModelClass: TClass;
    FTableName: string;
    FPKFieldName: string;
    FPKPropName: string;
    FPKPropInfo: PPropInfo;
    FFields: TDeltaFieldMetaArray;
    FFieldCount: Integer;
    FRTTIInfo: TRTTIClassInfo;
    procedure ExtractMetadata;
  public
    constructor Create(AModelClass: TClass; ARTTIInfo: TRTTIClassInfo);
    function FindField(const AFieldName: string): PDeltaFieldMeta;
    function FindFieldByProp(const APropName: string): PDeltaFieldMeta;
    property ModelClass: TClass read FModelClass;
    property TableName: string read FTableName;
    property PKFieldName: string read FPKFieldName;
    property PKPropName: string read FPKPropName;
    property PKPropInfo: PPropInfo read FPKPropInfo;
    property Fields: TDeltaFieldMetaArray read FFields;
    property FieldCount: Integer read FFieldCount;
    property RTTIInfo: TRTTIClassInfo read FRTTIInfo;
  end;

  { Cache global thread-safe de RTTI e Metadados }
  TRTTICache = class
  private
    class var FLock: TCriticalSection;
    class var FClassMap: TFPList;
    class var FClassInfoList: TFPList;
    class var FModelMetaClassMap: TFPList;
    class var FModelMetaList: TFPList;
  public
    class function GetClassProps(AClass: TClass): TRTTIClassInfo;
    class function GetModelMetadata(AClass: TClass): TModelMetadata;
    class procedure Clear;
  end;

implementation

uses
  DeltaModel, DeltaModel.List;

{ TRTTIClassInfo }

constructor TRTTIClassInfo.Create(AClass: TClass);
begin
  inherited Create;
  FClassType := AClass;
  BuildProps;
end;

procedure TRTTIClassInfo.BuildProps;
var
  RawPropList: PPropList;
  RawCount, I: Integer;
  PInfo: PPropInfo;
  TypeD: PTypeData;
begin
  if FClassType = nil then
  begin
    FPropCount := 0;
    SetLength(FProps, 0);
    Exit;
  end;

  RawCount := GetPropList(FClassType.ClassInfo, tkProperties, nil);
  FPropCount := RawCount;
  SetLength(FProps, RawCount);

  if RawCount > 0 then
  begin
    GetMem(RawPropList, RawCount * SizeOf(Pointer));
    try
      GetPropList(FClassType.ClassInfo, tkProperties, RawPropList, False);
      for I := 0 to RawCount - 1 do
      begin
        PInfo := RawPropList^[I];
        FProps[I].PropInfo := PInfo;
        FProps[I].Name := PInfo^.Name;
        FProps[I].PropKind := PInfo^.PropType^.Kind;
        FProps[I].HasGetProc := (PInfo^.GetProc <> nil);
        FProps[I].HasSetProc := (PInfo^.SetProc <> nil);
        FProps[I].IsClass := (PInfo^.PropType^.Kind = tkClass);
        FProps[I].ClassType := nil;
        FProps[I].IsDeltaField := False;
        FProps[I].IsDeltaModelList := False;

        if FProps[I].IsClass then
        begin
          TypeD := GetTypeData(PInfo^.PropType);
          if Assigned(TypeD) then
          begin
            FProps[I].ClassType := TypeD^.ClassType;
            if Assigned(FProps[I].ClassType) then
            begin
              FProps[I].IsDeltaField := FProps[I].ClassType.InheritsFrom(TDeltaField);
              FProps[I].IsDeltaModelList := FProps[I].ClassType.InheritsFrom(TCustomDeltaModelList);
            end;
          end;
        end;
      end;
    finally
      FreeMem(RawPropList, RawCount * SizeOf(Pointer));
    end;
  end;
end;

function TRTTIClassInfo.FindProp(const AName: string): PDeltaPropItem;
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
  begin
    if SameText(FProps[I].Name, AName) then
      Exit(@FProps[I]);
  end;
  Result := nil;
end;

{ TModelMetadata }

constructor TModelMetadata.Create(AModelClass: TClass; ARTTIInfo: TRTTIClassInfo);
begin
  inherited Create;
  FModelClass := AModelClass;
  FRTTIInfo := ARTTIInfo;
  ExtractMetadata;
end;

procedure TModelMetadata.ExtractMetadata;
var
  TempInstance: TObject;
  I: Integer;
  PropItem: PDeltaPropItem;
  NestedObj: TObject;
  DField: TDeltaField;
  Meta: TDeltaFieldMeta;
begin
  FTableName := '';
  FPKFieldName := '';
  FPKPropName := '';
  FPKPropInfo := nil;
  FFieldCount := 0;
  SetLength(FFields, 0);

  if not Assigned(FModelClass) then
    Exit;

  // Extrai nome padrão da tabela por convenção caso não consiga instanciar
  FTableName := AnsiLowerCase(Copy(FModelClass.ClassName, 2, MaxInt));

  // Instancia uma única vez para ler propriedades configuradas em TDeltaModel.Configure
  if FModelClass.InheritsFrom(TDeltaModel) then
  begin
    try
      TempInstance := TDeltaModelClass(FModelClass).Create;
      try
        FTableName := TDeltaModel(TempInstance).TableName;

        for I := 0 to FRTTIInfo.PropCount - 1 do
        begin
          PropItem := @FRTTIInfo.Props[I];
          if PropItem^.IsClass and PropItem^.HasGetProc then
          begin
            NestedObj := GetObjectProp(TempInstance, PropItem^.PropInfo);
            if NestedObj is TDeltaField then
            begin
              DField := TDeltaField(NestedObj);
              Meta.PropInfo := PropItem^.PropInfo;
              Meta.PropName := PropItem^.Name;
              Meta.FieldName := DField.FieldName;
              if Meta.FieldName.IsEmpty then
                Meta.FieldName := PropItem^.Name;
              Meta.IsPrimaryKey := (dboPrimaryKey in DField.DBOptions);
              Meta.IsAutoInc := (dboAutoInc in DField.DBOptions);
              Meta.IsRequired := (DField.IsRequired);
              Meta.FieldClass := DField.ClassType;
              Meta.FieldKind := DField.FieldKind;
              Meta.IsVirtual := DField.IsVirtual;

              SetLength(FFields, FFieldCount + 1);
              FFields[FFieldCount] := Meta;
              Inc(FFieldCount);

              if Meta.IsPrimaryKey and (FPKFieldName.IsEmpty) then
              begin
                FPKFieldName := Meta.FieldName;
                FPKPropName := Meta.PropName;
                FPKPropInfo := Meta.PropInfo;
              end;
            end;
          end;
        end;
      finally
        TempInstance.Free;
      end;
    except
      // Fallback gracioso
    end;
  end;

  if FPKFieldName.IsEmpty then
  begin
    FPKFieldName := 'id';
    FPKPropName := 'id';
    PropItem := FRTTIInfo.FindProp('id');
    if Assigned(PropItem) then
      FPKPropInfo := PropItem^.PropInfo;
  end;
end;

function TModelMetadata.FindField(const AFieldName: string): PDeltaFieldMeta;
var
  I: Integer;
begin
  for I := 0 to FFieldCount - 1 do
  begin
    if SameText(FFields[I].FieldName, AFieldName) then
      Exit(@FFields[I]);
  end;
  Result := nil;
end;

function TModelMetadata.FindFieldByProp(const APropName: string): PDeltaFieldMeta;
var
  I: Integer;
begin
  for I := 0 to FFieldCount - 1 do
  begin
    if SameText(FFields[I].PropName, APropName) then
      Exit(@FFields[I]);
  end;
  Result := nil;
end;

{ TRTTICache }

class function TRTTICache.GetClassProps(AClass: TClass): TRTTIClassInfo;
var
  Idx: Integer;
  Info: TRTTIClassInfo;
begin
  if not Assigned(AClass) then
    Exit(nil);

  FLock.Enter;
  try
    Idx := FClassMap.IndexOf(Pointer(AClass));
    if Idx >= 0 then
      Exit(TRTTIClassInfo(FClassInfoList[Idx]));

    Info := TRTTIClassInfo.Create(AClass);
    FClassMap.Add(Pointer(AClass));
    FClassInfoList.Add(Info);
    Result := Info;
  finally
    FLock.Leave;
  end;
end;

class function TRTTICache.GetModelMetadata(AClass: TClass): TModelMetadata;
var
  Idx: Integer;
  Meta: TModelMetadata;
  RttiInfo: TRTTIClassInfo;
begin
  if not Assigned(AClass) then
    Exit(nil);

  FLock.Enter;
  try
    Idx := FModelMetaClassMap.IndexOf(Pointer(AClass));
    if Idx >= 0 then
      Exit(TModelMetadata(FModelMetaList[Idx]));

    RttiInfo := GetClassProps(AClass);
    Meta := TModelMetadata.Create(AClass, RttiInfo);
    FModelMetaClassMap.Add(Pointer(AClass));
    FModelMetaList.Add(Meta);
    Result := Meta;
  finally
    FLock.Leave;
  end;
end;

class procedure TRTTICache.Clear;
var
  I: Integer;
begin
  if not Assigned(FLock) then Exit;

  FLock.Enter;
  try
    if Assigned(FModelMetaList) then
    begin
      for I := 0 to FModelMetaList.Count - 1 do
        TObject(FModelMetaList[I]).Free;
      FModelMetaList.Clear;
      FModelMetaClassMap.Clear;
    end;

    if Assigned(FClassInfoList) then
    begin
      for I := 0 to FClassInfoList.Count - 1 do
        TObject(FClassInfoList[I]).Free;
      FClassInfoList.Clear;
      FClassMap.Clear;
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  TRTTICache.FLock := TCriticalSection.Create;
  TRTTICache.FClassMap := TFPList.Create;
  TRTTICache.FClassInfoList := TFPList.Create;
  TRTTICache.FModelMetaClassMap := TFPList.Create;
  TRTTICache.FModelMetaList := TFPList.Create;

finalization
  TRTTICache.Clear;
  FreeAndNil(TRTTICache.FModelMetaList);
  FreeAndNil(TRTTICache.FModelMetaClassMap);
  FreeAndNil(TRTTICache.FClassInfoList);
  FreeAndNil(TRTTICache.FClassMap);
  FreeAndNil(TRTTICache.FLock);

end.
