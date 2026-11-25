unit DeltaModel.Metadata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SyncObjs, DeltaModel.Fields;

type
  { Represents cached metadata for a single RTTI property }
  PDeltaPropItem = ^TDeltaPropItem;
  TDeltaPropItem = record
    Index: Integer;
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

  { RTTI property cache for a class }
  TRTTIClassInfo = class
  private
    FClassType: TClass;
    FProps: TDeltaPropItemArray;
    FPropCount: Integer;
    procedure BuildProps;
  public
    constructor Create(AClass: TClass);
    function FindProp(const AName: string): PDeltaPropItem;
    function FindPropIndex(const AName: string): Integer;
    property ClassType: TClass read FClassType;
    property Props: TDeltaPropItemArray read FProps;
    property PropCount: Integer read FPropCount;
  end;

  { Field metadata for a TDeltaField or primitive property inside TDeltaModel }
  PDeltaFieldMeta = ^TDeltaFieldMeta;
  TDeltaFieldMeta = record
    PropInfo: PPropInfo;
    PropName: string;
    FieldName: string;
    PropIndex: Integer;
    IsPrimaryKey: Boolean;
    IsAutoInc: Boolean;
    IsRequired: Boolean;
    FieldClass: TClass;
    FieldKind: TDeltaFieldKind;
    IsVirtual: Boolean;
    IsDeltaField: Boolean;
    DBOptions: TDBOptionsSet;
    FieldSize: Integer;
    SchemaField: TDeltaSchemaField;
  end;
  TDeltaFieldMetaArray = array of TDeltaFieldMeta;

  { Plano pré-calculado para TDeltaModel.CreateFields: evita GetPropList,
    alocação e busca por nome no schema a cada instância. }
  TDeltaCreateFieldItem = record
    PropInfo: PPropInfo;
    PropName: string;
    FieldClass: TClass;
    SchemaField: TDeltaSchemaField;
  end;
  TDeltaCreateFieldPlan = array of TDeltaCreateFieldItem;

  { Persistence metadata for a TDeltaModel }
  TModelMetadata = class
  private
    FCreatePlan: TDeltaCreateFieldPlan;
    FCreatePlanReady: Boolean;
    FDefaultTableName: string;
    FModelClass: TClass;
    FTableName: string;
    FPKFieldName: string;
    FPKPropName: string;
    FPKPropInfo: PPropInfo;
    FFields: TDeltaFieldMetaArray;
    FFieldCount: Integer;
    FRTTIInfo: TRTTIClassInfo;
    FSchema: TDeltaModelSchema;
    procedure BuildCreatePlan;
  public
    constructor Create(AModelClass: TClass; ARTTIInfo: TRTTIClassInfo);
    destructor Destroy; override;
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
    property Schema: TDeltaModelSchema read FSchema;
    { Plano de criação dos TDF*: pronto ao final de ExtractMetadata. Durante a
      própria extração (instância temporária) CreateFields usa o caminho completo. }
    property CreatePlan: TDeltaCreateFieldPlan read FCreatePlan;
    property CreatePlanReady: Boolean read FCreatePlanReady;
    property DefaultTableName: string read FDefaultTableName;
    procedure ExtractMetadata;
  end;

  { Cache global thread-safe de RTTI e Metadados }
  TRTTICache = class
  private
    class var FLock: TCriticalSection;
    class var FClassMap: TFPList;
    class var FClassInfoList: TFPList;
    class var FModelMetaClassMap: TFPList;
    class var FModelMetaList: TFPList;
    class function GetClassPropsLocked(AClass: TClass): TRTTIClassInfo;
  public
    { Leituras usam cache por thread sem lock; a primeira consulta de cada
      classe (por thread) passa pelo lock global. }
    class function GetClassProps(AClass: TClass): TRTTIClassInfo;
    class function GetModelMetadata(AClass: TClass): TModelMetadata;
    class procedure Clear;
  end;

implementation

uses
  DeltaModel, DeltaModel.List;

const
  TL_CACHE_SLOTS = 8;

var
  { Incrementada em TRTTICache.Clear: invalida os caches por thread. }
  GCacheGeneration: Cardinal = 1;

threadvar
  { Cache por thread (sem lock) das últimas classes consultadas. Os objetos
    só são liberados em TRTTICache.Clear, que muda a geração. }
  TLGeneration: Cardinal;
  TLNextSlot: Integer;
  TLMetaClass: array[0..TL_CACHE_SLOTS - 1] of TClass;
  TLMetaValue: array[0..TL_CACHE_SLOTS - 1] of TModelMetadata;
  TLPropsClass: array[0..TL_CACHE_SLOTS - 1] of TClass;
  TLPropsValue: array[0..TL_CACHE_SLOTS - 1] of TRTTIClassInfo;

procedure EnsureThreadCacheGeneration; inline;
var
  Gen: Cardinal;
begin
  Gen := GCacheGeneration;
  if TLGeneration <> Gen then
  begin
    FillChar(TLMetaClass, SizeOf(TLMetaClass), 0);
    FillChar(TLMetaValue, SizeOf(TLMetaValue), 0);
    FillChar(TLPropsClass, SizeOf(TLPropsClass), 0);
    FillChar(TLPropsValue, SizeOf(TLPropsValue), 0);
    TLNextSlot := 0;
    TLGeneration := Gen;
  end;
end;

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
        FProps[I].Index := I;
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

function TRTTIClassInfo.FindPropIndex(const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
  begin
    if SameText(FProps[I].Name, AName) then
      Exit(I);
  end;
  Result := -1;
end;

{ TModelMetadata }

constructor TModelMetadata.Create(AModelClass: TClass; ARTTIInfo: TRTTIClassInfo);
begin
  inherited Create;
  FModelClass := AModelClass;
  FRTTIInfo := ARTTIInfo;
  FSchema := nil;
  FFieldCount := 0;
  SetLength(FFields, 0);
  if Assigned(FModelClass) then
    FTableName := AnsiLowerCase(Copy(FModelClass.ClassName, 2, MaxInt))
  else
    FTableName := '';
  // ExtractMetadata is invoked by TRTTICache.GetModelMetadata after caching to guard against recursion
end;

destructor TModelMetadata.Destroy;
begin
  FSchema.Free;
  inherited Destroy;
end;

procedure TModelMetadata.ExtractMetadata;
var
  TempInstance: TObject;
  I: Integer;
  PropItem: PDeltaPropItem;
  NestedObj: TObject;
  DField: TDeltaField;
  Meta: TDeltaFieldMeta;
  SchemaField: TDeltaSchemaField;
begin
  FTableName := '';
  FPKFieldName := '';
  FPKPropName := '';
  FPKPropInfo := nil;
  FFieldCount := 0;
  SetLength(FFields, 0);

  if not Assigned(FModelClass) then
    Exit;

  // Derive default table name by convention
  FTableName := AnsiLowerCase(Copy(FModelClass.ClassName, 2, MaxInt));

  if FModelClass.InheritsFrom(TDeltaModel) then
  begin
    // 1. Process class fluent Schema (Approach B: Primitive Properties)
    FSchema := TDeltaModelSchema.Create;
    TDeltaModelClass(FModelClass).ConfigureSchema(FSchema);
    if not FSchema.TableName.IsEmpty then
      FTableName := FSchema.TableName;

    for I := 0 to FSchema.FieldCount - 1 do
    begin
      SchemaField := FSchema.Fields[I];
      PropItem := FRTTIInfo.FindProp(SchemaField.PropName);
      { Propriedades de classe (TDF*) também podem ser citadas no schema fluente
        (ex.: Field('id').AsPK), mas são processadas no passo 2 a partir da
        instância; tratá-las aqui como primitivas faria o streamer e o binding
        de INSERT/UPDATE lerem o objeto como string vazia. }
      if (PropItem <> nil) and not PropItem^.IsClass then
      begin
        Meta.PropInfo := PropItem^.PropInfo;
        Meta.PropName := PropItem^.Name;
        Meta.FieldName := SchemaField.FieldName;
        if Meta.FieldName.IsEmpty then
          Meta.FieldName := LowerCase(PropItem^.Name);
        Meta.PropIndex := PropItem^.Index;
        Meta.IsPrimaryKey := (dboPrimaryKey in SchemaField.DBOptions);
        Meta.IsAutoInc := (dboAutoInc in SchemaField.DBOptions);
        Meta.IsRequired := SchemaField.IsRequired;
        Meta.FieldClass := nil;
        Meta.IsVirtual := False;
        Meta.IsDeltaField := False;
        Meta.DBOptions := SchemaField.DBOptions;
        Meta.FieldSize := SchemaField.FieldSize;
        Meta.SchemaField := SchemaField;

        case PropItem^.PropKind of
          tkInteger, tkInt64:
            Meta.FieldKind := dfkInteger;
          tkFloat:
            Meta.FieldKind := dfkFloat;
          tkBool:
            Meta.FieldKind := dfkBoolean;
          tkString, tkLString, tkAString, tkWString, tkUString:
            Meta.FieldKind := dfkString;
        else
          Meta.FieldKind := dfkString;
        end;

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

    // 2. Process additional fields or TDeltaField (including TDFHasOne, TDFHasMany)
    try
      TempInstance := TDeltaModelClass(FModelClass).Create;
      try
        if FTableName = AnsiLowerCase(Copy(FModelClass.ClassName, 2, MaxInt)) then
        begin
          if not TDeltaModel(TempInstance).TableName.IsEmpty then
            FTableName := TDeltaModel(TempInstance).TableName;
        end;

        for I := 0 to FRTTIInfo.PropCount - 1 do
        begin
          PropItem := @FRTTIInfo.Props[I];
          if (FindFieldByProp(PropItem^.Name) = nil) and PropItem^.IsClass and PropItem^.HasGetProc then
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
              Meta.PropIndex := PropItem^.Index;
              Meta.IsPrimaryKey := (dboPrimaryKey in DField.DBOptions);
              Meta.IsAutoInc := (dboAutoInc in DField.DBOptions);
              Meta.IsRequired := (DField.IsRequired);
              Meta.FieldClass := DField.ClassType;
              Meta.FieldKind := DField.FieldKind;
              Meta.IsVirtual := DField.IsVirtual;
              Meta.IsDeltaField := True;
              Meta.DBOptions := DField.DBOptions;
              Meta.FieldSize := 0;
              if DField is TDFStringNull then
                Meta.FieldSize := TDFStringNull(DField).Size
              else if DField is TDFStringRequired then
                Meta.FieldSize := TDFStringRequired(DField).Size;

              SchemaField := FSchema.FindField(PropItem^.Name);
              if SchemaField = nil then
                SchemaField := FSchema.Field(PropItem^.Name);

              if SchemaField.FieldName.IsEmpty or (SchemaField.FieldName = LowerCase(PropItem^.Name)) then
                SchemaField.Column(Meta.FieldName)
              else
                Meta.FieldName := SchemaField.FieldName;
              { O schema fluente só acrescenta marcas estruturais (AsPK, AutoInc,
                Unique, Index); permissões de escrita vêm do campo. }
              Meta.DBOptions := Meta.DBOptions + (SchemaField.DBOptions * DELTA_STRUCTURAL_DB_OPTIONS);
              SchemaField.DBOptions := Meta.DBOptions;
              Meta.IsPrimaryKey := dboPrimaryKey in Meta.DBOptions;
              Meta.IsAutoInc := dboAutoInc in Meta.DBOptions;
              Meta.IsRequired := Meta.IsRequired or SchemaField.IsRequired;
              if Meta.IsRequired then
                SchemaField.Required;
              SchemaField.FieldKind := Meta.FieldKind;
              if Meta.FieldSize > 0 then
                SchemaField.Size(Meta.FieldSize);
              Meta.SchemaField := SchemaField;
              DField.SchemaField := SchemaField;

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

  BuildCreatePlan;
end;

procedure TModelMetadata.BuildCreatePlan;
var
  PropList: PPropList;
  PropCount, I, N: Integer;
  PInfo: PPropInfo;
  PClass: TClass;
begin
  SetLength(FCreatePlan, 0);
  FDefaultTableName := AnsiLowerCase(Copy(FModelClass.ClassName, 2, MaxInt));
  PropCount := GetPropList(FModelClass.ClassInfo, tkProperties, nil);
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      { Mesma ordem (ordenada) usada historicamente por CreateFields. }
      GetPropList(FModelClass.ClassInfo, tkProperties, PropList);
      N := 0;
      SetLength(FCreatePlan, PropCount);
      for I := 0 to PropCount - 1 do
      begin
        PInfo := PropList^[I];
        if (PInfo^.SetProc = nil) or (PInfo^.PropType^.Kind <> tkClass) then
          Continue;
        PClass := GetTypeData(PInfo^.PropType)^.ClassType;
        if (PClass = nil) or not PClass.InheritsFrom(TDeltaField) then
          Continue;
        FCreatePlan[N].PropInfo := PInfo;
        FCreatePlan[N].PropName := PInfo^.Name;
        FCreatePlan[N].FieldClass := PClass;
        if Assigned(FSchema) then
          FCreatePlan[N].SchemaField := FSchema.FindField(PInfo^.Name)
        else
          FCreatePlan[N].SchemaField := nil;
        Inc(N);
      end;
      SetLength(FCreatePlan, N);
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;
  FCreatePlanReady := True;
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
begin
  if not Assigned(AClass) then
    Exit(nil);

  EnsureThreadCacheGeneration;
  for Idx := 0 to TL_CACHE_SLOTS - 1 do
    if TLPropsClass[Idx] = AClass then
      Exit(TLPropsValue[Idx]);

  Result := GetClassPropsLocked(AClass);
  TLPropsClass[TLNextSlot] := AClass;
  TLPropsValue[TLNextSlot] := Result;
  TLNextSlot := (TLNextSlot + 1) mod TL_CACHE_SLOTS;
end;

class function TRTTICache.GetClassPropsLocked(AClass: TClass): TRTTIClassInfo;
var
  Idx: Integer;
  Info: TRTTIClassInfo;
begin
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

  EnsureThreadCacheGeneration;
  for Idx := 0 to TL_CACHE_SLOTS - 1 do
    if TLMetaClass[Idx] = AClass then
      Exit(TLMetaValue[Idx]);

  FLock.Enter;
  try
    Idx := FModelMetaClassMap.IndexOf(Pointer(AClass));
    if Idx >= 0 then
      Result := TModelMetadata(FModelMetaList[Idx])
    else
    begin
      RttiInfo := GetClassPropsLocked(AClass);
      Meta := TModelMetadata.Create(AClass, RttiInfo);
      FModelMetaClassMap.Add(Pointer(AClass));
      FModelMetaList.Add(Meta);
      Meta.ExtractMetadata;
      Result := Meta;
    end;
  finally
    FLock.Leave;
  end;

  { Metadados ainda em extração (chamada reentrante da instância temporária)
    não entram no cache por thread: só objetos completos. }
  if Result.CreatePlanReady then
  begin
    TLMetaClass[TLNextSlot] := AClass;
    TLMetaValue[TLNextSlot] := Result;
    TLNextSlot := (TLNextSlot + 1) mod TL_CACHE_SLOTS;
  end;
end;

class procedure TRTTICache.Clear;
var
  I: Integer;
begin
  if not Assigned(FLock) then Exit;

  FLock.Enter;
  try
    InterLockedIncrement(LongInt(GCacheGeneration));
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
