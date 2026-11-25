unit DeltaModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, TypInfo,
  DeltaAPISchema, DeltaSerialization, DeltaModelMessages, DeltaValidator,
  DeltaModel.Fields, DeltaModel.List, DeltaModel.ApiDoc;

type

  TFieldList = specialize TFPGObjectList<TDeltaField>;

  TConstraintKind = (ckUnique, ckCheck);

  { TDeltaConstraint }

  TDeltaConstraint = class
  private
    FName: string;
    FKind: TConstraintKind;
    FFields: TStringList;
    FCheckExpression: string;
  public
    constructor CreateUnique(const AName: string; const AFields: array of string);
    constructor CreateCheck(const AName: string; const AExpression: string);
    destructor Destroy; override;
    function GetFieldsSQL: string;
    property Name: string read FName write FName;
    property Kind: TConstraintKind read FKind write FKind;
    property Fields: TStringList read FFields;
    property CheckExpression: string read FCheckExpression write FCheckExpression;
  end;

  { TDeltaIndex }

  TDeltaIndex = class
  private
    FName: string;
    FFields: TStringList;
    FIsUnique: Boolean;
  public
    constructor Create(const AName: string; const AFields: array of string; AIsUnique: Boolean = False);
    destructor Destroy; override;
    function GetFieldsSQL: string;
    property Name: string read FName write FName;
    property Fields: TStringList read FFields;
    property IsUnique: Boolean read FIsUnique write FIsUnique;
  end;

  TConstraintList = specialize TFPGObjectList<TDeltaConstraint>;
  TIndexList = specialize TFPGObjectList<TDeltaIndex>;

  TDeltaModelRegisterSchemaProc = procedure(const AName, ASchemaJson: string);

procedure SetDeltaModelRegisterSchemaProc(AProc: TDeltaModelRegisterSchemaProc);
function GetDeltaModelRegisterSchemaProc: TDeltaModelRegisterSchemaProc;

type

  { TDeltaModel }

  TDeltaModel = class
  private
    FTableName: string;
    FValidator: TValidator;
    FFieldList: TFieldList;
    FConstraints: TConstraintList;
    FIndexes: TIndexList;
    FNullMask: UInt64;
    FDirtyMask: UInt64;
    procedure CreateFields;
    function GetTableName: string;
    procedure SetTableName(AValue: string);
  public
    class function Schema: TDeltaModelSchema;
    class function GetSchema: TDeltaModelSchema; inline;
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); virtual;
    { Documentation-only metadata for OpenAPI (descriptions and enums).
      Read by GenerateSchema; does not affect ORM, validation or serialization. }
    class procedure DescribeApiSchema(ADoc: TDeltaApiDoc); virtual;

    function IsNull(const APropName: string): Boolean; overload;
    function IsNull(APropIndex: Integer): Boolean; overload; inline;
    procedure SetNull(const APropName: string); overload;
    procedure SetNull(APropIndex: Integer); overload; inline;
    procedure ClearNull(const APropName: string); overload;
    procedure ClearNull(APropIndex: Integer); overload; inline;
    procedure ClearAllNulls; inline;

    function IsDirty(const APropName: string): Boolean; overload;
    function IsDirty(APropIndex: Integer): Boolean; overload; inline;
    procedure MarkDirty(const APropName: string); overload;
    procedure MarkDirty(APropIndex: Integer); overload; inline;
    procedure ClearDirty(const APropName: string); overload;
    procedure ClearDirty(APropIndex: Integer); overload; inline;
    procedure ClearAllDirty;
    function HasDirtyFields: Boolean;
    function DirtyFieldCount: Integer;
    function GetDirtyFieldNames: TStringArray;

    property TableName: string read GetTableName write SetTableName;
    property FieldList: TFieldList read FFieldList;
    property Constraints: TConstraintList read FConstraints;
    property Indexes: TIndexList read FIndexes;
    procedure FromJson(JsonStr: string); virtual;
    procedure Validate; virtual;
    procedure BeforeDestruction; override;
    procedure Configure; virtual;
    procedure CopyObject(Obj: TDeltaModel);
    function Clone: TDeltaModel;
    function IsEmpty: Boolean;
    function ToJson: RawByteString;
    function ToJsonObj: TJSONObject;
    class function SwaggerSchema(IsArray: Boolean = False; IsPaginated: Boolean = False): string; deprecated 'Use SwaggerSchemaObject, SwaggerSchemaArray, or SwaggerSchemaPaginated instead';
    class function SwaggerSchemaObject(AddExamples: Boolean = False): string;
    class function SwaggerSchemaArray(AddExamples: Boolean = False): string;
    class function SwaggerSchemaPaginated(AddExamples: Boolean = False): string;
    class function SchemaName: string; virtual;
    class function RegisterSwaggerSchema(AddExamples: Boolean = False): string;
    class function SwaggerSchemaRef(IsArray: Boolean = False; IsPaginated: Boolean = False): string;
    class function SwaggerSchemaRefObject: string;
    class function SwaggerSchemaRefArray: string;
    class function SwaggerSchemaRefPaginated: string;
    constructor Create; virtual;

    function AddUniqueConstraint(const AName: string; const AFields: array of string): TDeltaConstraint; overload;
    function AddUniqueConstraint(const AFields: array of string): TDeltaConstraint; overload;
    function AddCheckConstraint(const AName: string; const AExpression: string): TDeltaConstraint;

    function AddIndex(const AName: string; const AFields: array of string; AIsUnique: Boolean = False): TDeltaIndex; overload;
    function AddIndex(const AFields: array of string; AIsUnique: Boolean = False): TDeltaIndex; overload;
    function AddUniqueIndex(const AName: string; const AFields: array of string): TDeltaIndex; overload;
    function AddUniqueIndex(const AFields: array of string): TDeltaIndex; overload;

    // Lifecycle hooks
    procedure BeforeInsert; virtual;
    procedure AfterInsert; virtual;
    procedure BeforeUpdate; virtual;
    procedure AfterUpdate; virtual;
    procedure BeforeDelete; virtual;
    procedure AfterDelete; virtual;
    procedure BeforeSave; virtual;
    procedure AfterSave; virtual;
  public
    property Validator: TValidator read FValidator;
  end;

  TDeltaModelClass = class of TDeltaModel;

  { TDeltaModelRecords }

  TDeltaModelRecords = class(specialize TFPGObjectList<TDeltaModel>)
  public
    function Add(const AItem: TDeltaModel): Integer; deprecated 'Use TDeltaQuery.Cursor or StreamJsonTo instead';
    function AddInternal(const AItem: TDeltaModel): Integer; inline;
  end;

  { TDeltaModelList }

  TDeltaModelList = class(TCustomDeltaModelList)
  private
    FDeltaModelClass: TDeltaModelClass;
    FRecords: TDeltaModelRecords;
    FPage: Integer;
    FPageSize: Integer;
    FTotalRecords: Int64;
    FTotalPages: Integer;
    function GetTotal: Int64;
    procedure SetTotal(AValue: Int64);
    function GetLimit: Integer;
    procedure SetLimit(AValue: Integer);
  public
    function GetPage: Integer; override;
    procedure SetPage(AValue: Integer); override;
    function GetPageSize: Integer; override;
    procedure SetPageSize(AValue: Integer); override;
    function GetTotalRecords: Int64; override;
    procedure SetTotalRecords(AValue: Int64); override;
    function GetTotalPages: Integer; override;
    procedure SetTotalPages(AValue: Integer); override;

    property DeltaModelClass: TDeltaModelClass read FDeltaModelClass write FDeltaModelClass;
    property Records: TDeltaModelRecords read FRecords;
    property items: TDeltaModelRecords read FRecords;
    property page: Integer read GetPage write SetPage;
    property page_size: Integer read GetPageSize write SetPageSize;
    property total_records: Int64 read GetTotalRecords write SetTotalRecords;
    property total_pages: Integer read GetTotalPages write SetTotalPages;

    // Backward-compatibility aliases
    property Total: Int64 read GetTotal write SetTotal;
    property Limit: Integer read GetLimit write SetLimit;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property TotalRecords: Int64 read GetTotalRecords write SetTotalRecords;
    property TotalPages: Integer read GetTotalPages write SetTotalPages;

    procedure FromJson(JsonStr: string); override;
    function ToJson: RawByteString; override;
    function ToPaginatedJson: RawByteString;
    function ToJsonObj: TJSONArray; override;
    function ToPaginatedJsonObj: TJSONObject; override;
    function SwaggerSchema(AddExamples: Boolean = False): TJSONObject; override; deprecated 'Use SwaggerSchemaArray or SwaggerSchemaPaginated instead';
    function SwaggerSchemaArray(AddExamples: Boolean = False): TJSONObject; override;
    function SwaggerSchemaPaginated(AddExamples: Boolean = False): TJSONObject; override;
    function SwaggerSchemaRefArray: string;
    function SwaggerSchemaRefPaginated: string;
    function SetDeltaModelClass(AClass: TDeltaModelClass): TDeltaModelList;
    function Add(AModel: TDeltaModel): Integer;
    function Count: Integer; override;
    function GetItem(AIndex: Integer): TDeltaModel;
    property Item[AIndex: Integer]: TDeltaModel read GetItem; default;

    procedure FromCSV(const CSVStr: string; ADelimiter: Char = ';');
    function ToCSV(ADelimiter: Char = ';'): string;

    function GetItemObj(AIndex: Integer): TObject; override;
    procedure ClearList; override;
    procedure AddObj(AObj: TObject); override;
    function GetModelClass: TClass; override;
    function NewItem: TObject; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  generic TGDeltaModelListEnumerator<T: TDeltaModel> = class
  private
    FList: TDeltaModelList;
    FIndex: Integer;
  public
    constructor Create(AList: TDeltaModelList);
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  generic TGDeltaModelList<T: TDeltaModel> = class(TDeltaModelList)
  public
    type
      TEnumerator = specialize TGDeltaModelListEnumerator<T>;
    constructor Create;
    function Add(AModel: T): Integer;
    function GetTypedItem(AIndex: Integer): T;
    property Item[AIndex: Integer]: T read GetTypedItem; default;
    function First: T;
    function Last: T;
    function GetEnumerator: TEnumerator;
  end;

  DeltaModelList = TDeltaModelList;

  { TDeltaModelPaginatedList }

  TDeltaModelPaginatedList = class(TDeltaModelList)
  public
    function ToJson: RawByteString; override;
    function SwaggerSchema(AddExamples: Boolean = False): TJSONObject; override; deprecated 'Use SwaggerSchemaPaginated instead';
  end;

  generic TGDeltaModelPaginatedList<T: TDeltaModel> = class(TDeltaModelPaginatedList)
  public
    type
      TEnumerator = specialize TGDeltaModelListEnumerator<T>;
    constructor Create;
    function Add(AModel: T): Integer;
    function GetTypedItem(AIndex: Integer): T;
    property Item[AIndex: Integer]: T read GetTypedItem; default;
    function First: T;
    function Last: T;
    function GetEnumerator: TEnumerator;
  end;

implementation

uses
  DeltaModel.JsonStreamer, DeltaModel.Metadata, DeltaModel.Messages, DeltaModel.Domains;

var
  GDeltaModelRegisterSchemaProc: TDeltaModelRegisterSchemaProc = nil;

procedure SetDeltaModelRegisterSchemaProc(AProc: TDeltaModelRegisterSchemaProc);
begin
  GDeltaModelRegisterSchemaProc := AProc;
end;

function GetDeltaModelRegisterSchemaProc: TDeltaModelRegisterSchemaProc;
begin
  Result := GDeltaModelRegisterSchemaProc;
end;

{ TDeltaConstraint }

constructor TDeltaConstraint.CreateUnique(const AName: string;
  const AFields: array of string);
var
  I: Integer;
begin
  inherited Create;
  FName := AName;
  FKind := ckUnique;
  FFields := TStringList.Create;
  for I := Low(AFields) to High(AFields) do
    FFields.Add(Trim(AFields[I]));
end;

constructor TDeltaConstraint.CreateCheck(const AName: string;
  const AExpression: string);
begin
  inherited Create;
  FName := AName;
  FKind := ckCheck;
  FFields := TStringList.Create;
  FCheckExpression := AExpression;
end;

destructor TDeltaConstraint.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TDeltaConstraint.GetFieldsSQL: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FFields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + FFields[I];
  end;
end;

{ TDeltaIndex }

constructor TDeltaIndex.Create(const AName: string;
  const AFields: array of string; AIsUnique: Boolean);
var
  I: Integer;
begin
  inherited Create;
  FName := AName;
  FIsUnique := AIsUnique;
  FFields := TStringList.Create;
  for I := Low(AFields) to High(AFields) do
    FFields.Add(Trim(AFields[I]));
end;

destructor TDeltaIndex.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TDeltaIndex.GetFieldsSQL: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FFields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ', ';
    Result := Result + FFields[I];
  end;
end;

{ TDeltaModelRecords }

function TDeltaModelRecords.Add(const AItem: TDeltaModel): Integer;
begin
  Result := inherited Add(AItem);
end;

function TDeltaModelRecords.AddInternal(const AItem: TDeltaModel): Integer;
begin
  Result := inherited Add(AItem);
end;

{ TDeltaModel }

procedure TDeltaModel.CreateFields;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
  I, PropCount: integer;
  PropObj: TObject;
  PropClass: TClass;
  DeltaField: TDeltaField;
  ASchema: TDeltaModelSchema;
begin
  PropCount := GetPropList(Self.ClassInfo, tkProperties, nil);

  if PropCount = 0 then Exit;

  ASchema := Self.GetSchema;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Self.ClassInfo, tkProperties, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      if PropInfo^.SetProc = nil then Continue;

      PropType := PropInfo^.PropType;

      if (PropType^.Kind = tkClass) then
      begin
        PropClass := GetTypeData(PropType)^.ClassType;

        if PropClass.InheritsFrom(TDeltaField) then
        begin
          PropObj := GetObjectProp(Self, PropInfo);

          if (PropObj = nil) then
          begin
            PropObj := TDeltaFieldClass(PropClass).Create;

            DeltaField := TDeltaField(PropObj);

            if ASchema <> nil then
              DeltaField.SchemaField := ASchema.FindField(PropInfo^.Name);

            DeltaField.FieldName := PropInfo^.Name;
            DeltaField.Visible   := True;

            SetObjectProp(Self, PropInfo, PropObj);
            FFieldList.Add(DeltaField);
          end
          else
          begin
            DeltaField := TDeltaField(PropObj);
            if ASchema <> nil then
              DeltaField.SchemaField := ASchema.FindField(PropInfo^.Name);
            if FFieldList.IndexOf(DeltaField) < 0 then
              FFieldList.Add(DeltaField);
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure TDeltaModel.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FValidator.Free;
  FFieldList.Free;
  FConstraints.Free;
  FIndexes.Free;
end;

procedure TDeltaModel.Configure;
begin

end;

class function TDeltaModel.Schema: TDeltaModelSchema;
begin
  Result := TRTTICache.GetModelMetadata(Self).Schema;
end;

class function TDeltaModel.GetSchema: TDeltaModelSchema;
begin
  Result := Schema;
end;

class procedure TDeltaModel.ConfigureSchema(ASchema: TDeltaModelSchema);
begin

end;

class procedure TDeltaModel.DescribeApiSchema(ADoc: TDeltaApiDoc);
begin

end;

function TDeltaModel.IsNull(APropIndex: Integer): Boolean;
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    Result := (FNullMask and (UInt64(1) shl APropIndex)) <> 0
  else
    Result := False;
end;

procedure TDeltaModel.SetNull(APropIndex: Integer);
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    FNullMask := FNullMask or (UInt64(1) shl APropIndex);
end;

procedure TDeltaModel.ClearNull(APropIndex: Integer);
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    FNullMask := FNullMask and not (UInt64(1) shl APropIndex);
end;

procedure TDeltaModel.ClearAllNulls;
begin
  FNullMask := 0;
end;

function TDeltaModel.IsNull(const APropName: string): Boolean;
var
  FieldObj: TDeltaField;
  PropInfo: PPropInfo;
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  PropInfo := GetPropInfo(Self.ClassInfo, APropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    FieldObj := TDeltaField(GetObjectProp(Self, PropInfo));
    if FieldObj <> nil then
      Exit(FieldObj.IsNull);
  end;

  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    Exit(IsNull(PropItem^.Index));

  Result := False;
end;

procedure TDeltaModel.SetNull(const APropName: string);
var
  FieldObj: TDeltaField;
  PropInfo: PPropInfo;
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  PropInfo := GetPropInfo(Self.ClassInfo, APropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    FieldObj := TDeltaField(GetObjectProp(Self, PropInfo));
    if FieldObj <> nil then
    begin
      FieldObj.Clear;
      Exit;
    end;
  end;

  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    SetNull(PropItem^.Index);
end;

procedure TDeltaModel.ClearNull(const APropName: string);
var
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    ClearNull(PropItem^.Index);
end;

function TDeltaModel.IsDirty(APropIndex: Integer): Boolean;
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    Result := (FDirtyMask and (UInt64(1) shl APropIndex)) <> 0
  else
    Result := False;
end;

procedure TDeltaModel.MarkDirty(APropIndex: Integer);
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    FDirtyMask := FDirtyMask or (UInt64(1) shl APropIndex);
end;

procedure TDeltaModel.ClearDirty(APropIndex: Integer);
begin
  if (APropIndex >= 0) and (APropIndex < 64) then
    FDirtyMask := FDirtyMask and not (UInt64(1) shl APropIndex);
end;

procedure TDeltaModel.ClearAllDirty;
var
  I: Integer;
begin
  FDirtyMask := 0;
  if FFieldList <> nil then
    for I := 0 to FFieldList.Count - 1 do
      FFieldList[I].Dirty := False;
end;

function TDeltaModel.IsDirty(const APropName: string): Boolean;
var
  FieldObj: TDeltaField;
  PropInfo: PPropInfo;
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  PropInfo := GetPropInfo(Self.ClassInfo, APropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    FieldObj := TDeltaField(GetObjectProp(Self, PropInfo));
    if (FieldObj <> nil) and FieldObj.Dirty then
      Exit(True);
  end;

  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    Exit(IsDirty(PropItem^.Index));

  Result := False;
end;

procedure TDeltaModel.MarkDirty(const APropName: string);
var
  FieldObj: TDeltaField;
  PropInfo: PPropInfo;
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  PropInfo := GetPropInfo(Self.ClassInfo, APropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    FieldObj := TDeltaField(GetObjectProp(Self, PropInfo));
    if FieldObj <> nil then
      FieldObj.Dirty := True;
  end;

  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    MarkDirty(PropItem^.Index);
end;

procedure TDeltaModel.ClearDirty(const APropName: string);
var
  FieldObj: TDeltaField;
  PropInfo: PPropInfo;
  PropItem: PDeltaPropItem;
  ClassProps: TRTTIClassInfo;
begin
  PropInfo := GetPropInfo(Self.ClassInfo, APropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    FieldObj := TDeltaField(GetObjectProp(Self, PropInfo));
    if FieldObj <> nil then
      FieldObj.Dirty := False;
  end;

  ClassProps := TRTTICache.GetClassProps(Self.ClassType);
  PropItem := ClassProps.FindProp(APropName);
  if PropItem <> nil then
    ClearDirty(PropItem^.Index);
end;

function TDeltaModel.HasDirtyFields: Boolean;
var
  I: Integer;
begin
  if FDirtyMask <> 0 then
    Exit(True);
  if FFieldList <> nil then
    for I := 0 to FFieldList.Count - 1 do
      if FFieldList[I].Dirty then
        Exit(True);
  Result := False;
end;

function TDeltaModel.DirtyFieldCount: Integer;
var
  Meta: TModelMetadata;
  I: Integer;
  FieldMeta: PDeltaFieldMeta;
  NestedObj: TObject;
begin
  Result := 0;
  Meta := TRTTICache.GetModelMetadata(Self.ClassType);
  if (Meta <> nil) and (Meta.FieldCount > 0) then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FieldMeta := @Meta.Fields[I];
      if FieldMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(Self, FieldMeta^.PropInfo);
        if (NestedObj is TDeltaField) and (TDeltaField(NestedObj).Dirty or IsDirty(FieldMeta^.PropIndex)) then
          Inc(Result);
      end
      else if IsDirty(FieldMeta^.PropIndex) then
        Inc(Result);
    end;
  end
  else if FFieldList <> nil then
  begin
    for I := 0 to FFieldList.Count - 1 do
      if FFieldList[I].Dirty then
        Inc(Result);
  end;
end;

function TDeltaModel.GetDirtyFieldNames: TStringArray;
var
  Meta: TModelMetadata;
  I, Cnt: Integer;
  FieldMeta: PDeltaFieldMeta;
  NestedObj: TObject;
begin
  Result := nil;
  Cnt := 0;
  Meta := TRTTICache.GetModelMetadata(Self.ClassType);
  if (Meta <> nil) and (Meta.FieldCount > 0) then
  begin
    SetLength(Result, Meta.FieldCount);
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FieldMeta := @Meta.Fields[I];
      if FieldMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(Self, FieldMeta^.PropInfo);
        if (NestedObj is TDeltaField) and (TDeltaField(NestedObj).Dirty or IsDirty(FieldMeta^.PropIndex)) then
        begin
          Result[Cnt] := FieldMeta^.FieldName;
          Inc(Cnt);
        end;
      end
      else if IsDirty(FieldMeta^.PropIndex) then
      begin
        Result[Cnt] := FieldMeta^.FieldName;
        Inc(Cnt);
      end;
    end;
    SetLength(Result, Cnt);
  end;
end;

procedure TDeltaModel.CopyObject(Obj: TDeltaModel);
begin
  DeltaSerialization.CopyObject(Obj, Self);
end;

function TDeltaModel.Clone: TDeltaModel;
begin
  Result := TDeltaModelClass(Self.ClassType).Create;
  DeltaSerialization.CopyObject(Self, Result);
end;

function TDeltaModel.IsEmpty: Boolean;
var
  I: Integer;
  Field: TDeltaField;
  Meta: TModelMetadata;
begin
  Result := True;
  if (FFieldList <> nil) and (FFieldList.Count > 0) then
  begin
    for I := 0 to Pred(Self.FFieldList.Count) do
    begin
      Field := Self.FFieldList.Items[I];
      if not Field.IsNull then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    Meta := TRTTICache.GetModelMetadata(Self.ClassType);
    for I := 0 to Meta.FieldCount - 1 do
    begin
      if not IsNull(Meta.Fields[I].PropIndex) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TDeltaModel.GetTableName: string;
var
  Meta: TModelMetadata;
begin
  if (FTableName = AnsiLowerCase(Copy(Self.ClassName, 2, MaxInt))) or FTableName.IsEmpty then
  begin
    Meta := TRTTICache.GetModelMetadata(Self.ClassType);
    if (Meta <> nil) and not Meta.TableName.IsEmpty then
      Exit(Meta.TableName);
  end;
  Result := FTableName;
end;

procedure TDeltaModel.SetTableName(AValue: string);
begin
  if FTableName = AValue then Exit;
  FTableName := AValue;
end;

procedure TDeltaModel.FromJson(JsonStr: string);
begin
  Deserialize(Self, JsonStr);
end;

procedure TDeltaModel.Validate;
var
  I: Integer;
  Field: TDeltaField;
  Req: TDeltaFieldRequired;
  Meta: TModelMetadata;
  FMeta: TDeltaFieldMeta;
  PropInfo: PPropInfo;
  PropValueStr: string;
  PropValueFloat: Double;
  PropValueInt: Int64;
  VVal: TValid;
begin
  // 1. Validate traditional model fields (TDF*)
  for I := 0 to Pred(Self.FFieldList.Count) do
  begin
    Field := Self.FFieldList.Items[I];

    if (Field is TDeltaFieldRequired) then
    begin
      // Auto-increment primary key will be generated by the database
      if (dboAutoInc in Field.DBOptions) and Field.IsNull then
        Continue;

      Req := (Field as TDeltaFieldRequired);

      if (Req.IsNull) or
         ((Req is TDFStringRequired) and ((Req as TDFStringRequired).AsString.Trim.IsEmpty)) then
      begin
        raise EDeltaValidation.CreateFmt(
          'Field %s.%s is required',
          [Self.ClassName, Req.FieldName]
        );
      end;
    end;

    // Domain and format validation (TDFEmail, TDFCPF, TDFCNPJ, TDFDocumento, TDFTelefone, TDFCEP, TDFUUID*)
    if (not Field.IsNull) and (not Field.IsValid) then
    begin
      raise EDeltaValidation.CreateFmt(
        'Validation failed for field %s.%s: invalid %s value',
        [Self.ClassName, Field.FieldName, Field.SwaggerFormat]
      );
    end;
  end;

  // 2. Declarative Fluent Schema validation (Approach B)
  Meta := TRTTICache.GetModelMetadata(Self.ClassType);
  if (Meta <> nil) and (Meta.FieldCount > 0) then
  begin
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FMeta := Meta.Fields[I];
      if FMeta.IsDeltaField then
        Continue;

      // If auto-increment PK is null, it will be generated by the database
      if (dboAutoInc in FMeta.DBOptions) and IsNull(FMeta.PropIndex) then
        Continue;

      // Required
      if (FMeta.IsRequired) and IsNull(FMeta.PropIndex) then
        raise EDeltaValidation.CreateFmt('Field %s.%s is required', [Self.ClassName, FMeta.PropName]);

      // If not null, validate declarative rules
      if not IsNull(FMeta.PropIndex) then
      begin
        PropInfo := GetPropInfo(Self.ClassInfo, FMeta.PropName);
        if PropInfo <> nil then
        begin
          case PropInfo^.PropType^.Kind of
            tkAString, tkUString, tkString:
            begin
              PropValueStr := GetStrProp(Self, PropInfo);
              if (FMeta.IsRequired) and (PropValueStr.Trim.IsEmpty) then
                raise EDeltaValidation.CreateFmt('Field %s.%s is required', [Self.ClassName, FMeta.PropName]);

              if FMeta.SchemaField <> nil then
              begin
                if FMeta.SchemaField.IsNotEmpty and PropValueStr.Trim.IsEmpty then
                  raise EDeltaValidation.Create(MsgFieldCannotBeEmpty(Self.ClassName, FMeta.PropName));

                if FMeta.SchemaField.HasMinLength and (Length(PropValueStr) < FMeta.SchemaField.MinLengthValue) then
                  raise EDeltaValidation.Create(MsgFieldMinLength(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MinLengthValue));

                if FMeta.SchemaField.HasMaxLength and (Length(PropValueStr) > FMeta.SchemaField.MaxLengthValue) then
                  raise EDeltaValidation.Create(MsgFieldMaxLength(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MaxLengthValue));

                if FMeta.SchemaField.IsEmail then
                begin
                  if not IsValidEmailAddress(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidEmail(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsCPF then
                begin
                  if not IsValidCPF(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidCPF(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsCNPJ then
                begin
                  if not IsValidCNPJ(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidCNPJ(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsCPF_CNPJ then
                begin
                  if not IsValidDocumento(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidCPF_CNPJ(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsURL then
                begin
                  VVal := TValidatorItemUrl.Create.Validate(PropValueStr);
                  if not VVal.OK then
                    raise EDeltaValidation.Create(MsgFieldInvalidUrl(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsPhone then
                begin
                  if not IsValidTelefone(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidPhone(Self.ClassName, FMeta.PropName));
                end;

                if FMeta.SchemaField.IsCEP then
                begin
                  if not IsValidCEP(PropValueStr) then
                    raise EDeltaValidation.Create(MsgFieldInvalidCEP(Self.ClassName, FMeta.PropName));
                end;
              end;
            end;
            tkInteger, tkInt64:
            begin
              PropValueInt := GetOrdProp(Self, PropInfo);
              if FMeta.SchemaField <> nil then
              begin
                if FMeta.SchemaField.IsPositive and (PropValueInt <= 0) then
                  raise EDeltaValidation.Create(MsgFieldMustBePositive(Self.ClassName, FMeta.PropName));
                if FMeta.SchemaField.HasMin and (PropValueInt < FMeta.SchemaField.MinValue) then
                  raise EDeltaValidation.Create(MsgFieldMinValue(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MinValue));
                if FMeta.SchemaField.HasMax and (PropValueInt > FMeta.SchemaField.MaxValue) then
                  raise EDeltaValidation.Create(MsgFieldMaxValue(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MaxValue));
              end;
            end;
            tkFloat:
            begin
              PropValueFloat := GetFloatProp(Self, PropInfo);
              if FMeta.SchemaField <> nil then
              begin
                if FMeta.SchemaField.IsPositive and (PropValueFloat <= 0) then
                  raise EDeltaValidation.Create(MsgFieldMustBePositive(Self.ClassName, FMeta.PropName));
                if FMeta.SchemaField.HasMin and (PropValueFloat < FMeta.SchemaField.MinValue) then
                  raise EDeltaValidation.Create(MsgFieldMinValue(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MinValue));
                if FMeta.SchemaField.HasMax and (PropValueFloat > FMeta.SchemaField.MaxValue) then
                  raise EDeltaValidation.Create(MsgFieldMaxValue(Self.ClassName, FMeta.PropName, FMeta.SchemaField.MaxValue));
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TDeltaModel.ToJson: RawByteString;
begin
  Result := TDeltaFastJsonStreamer.ModelToJson(Self);
end;

function TDeltaModel.ToJsonObj: TJSONObject;
begin
  Result := SerializeToJsonObj(Self);
end;

class function TDeltaModel.SwaggerSchema(IsArray: Boolean; IsPaginated: Boolean): string;
begin
  if IsPaginated or IsArray then
    Result := SwaggerSchemaPaginated(False)
  else
    Result := SwaggerSchemaObject(False);
end;

class function TDeltaModel.SwaggerSchemaObject(AddExamples: Boolean): string;
var
  Obj: TDeltaModel;
begin
  Obj := Create;
  try
    Result := GenerateSchemaStr(Obj, AddExamples, False);
  finally
    Obj.Free;
  end;
end;

class function TDeltaModel.SwaggerSchemaArray(AddExamples: Boolean): string;
var
  Obj: TDeltaModel;
begin
  Obj := Create;
  try
    Result := GenerateSchemaStr(Obj, AddExamples, True);
  finally
    Obj.Free;
  end;
end;

class function TDeltaModel.SwaggerSchemaPaginated(AddExamples: Boolean): string;
var
  Obj: TDeltaModel;
begin
  Obj := Create;
  try
    Result := GeneratePaginatedSchemaStr(Obj, AddExamples);
  finally
    Obj.Free;
  end;
end;

class function TDeltaModel.SchemaName: string;
var
  N: string;
begin
  N := ClassName;
  if (Length(N) > 1) and (N[1] = 'T') and (N[2] in ['A'..'Z']) then
    Result := Copy(N, 2, Length(N) - 1)
  else
    Result := N;
end;

class function TDeltaModel.RegisterSwaggerSchema(AddExamples: Boolean): string;
var
  SName, SJson: string;
  Proc: TDeltaModelRegisterSchemaProc;
begin
  SName := SchemaName;
  Result := SName;
  Proc := GetDeltaModelRegisterSchemaProc;
  if Assigned(Proc) then
  begin
    SJson := SwaggerSchemaObject(AddExamples);
    Proc(SName, SJson);
  end;
end;

class function TDeltaModel.SwaggerSchemaRef(IsArray: Boolean; IsPaginated: Boolean): string;
var
  SName: string;
begin
  SName := RegisterSwaggerSchema(False);
  if IsPaginated then
    Result := GeneratePaginatedSchemaRefStr(SName)
  else if IsArray then
    Result := GenerateSchemaRefArrayStr(SName)
  else
    Result := GenerateSchemaRefStr(SName);
end;

class function TDeltaModel.SwaggerSchemaRefObject: string;
begin
  Result := SwaggerSchemaRef(False, False);
end;

class function TDeltaModel.SwaggerSchemaRefArray: string;
begin
  Result := SwaggerSchemaRef(True, False);
end;

class function TDeltaModel.SwaggerSchemaRefPaginated: string;
begin
  Result := SwaggerSchemaRef(False, True);
end;

constructor TDeltaModel.Create;
begin
  FValidator := TValidator.Create;
  FFieldList := TFieldList.Create();
  FConstraints := TConstraintList.Create();
  FIndexes := TIndexList.Create();
  FNullMask := 0;
  FDirtyMask := 0;

  CreateFields();

  FTableName := AnsiLowerCase(Copy(Self.ClassName, 2, MaxInt));

  Configure();
end;

function TDeltaModel.AddUniqueConstraint(const AName: string;
  const AFields: array of string): TDeltaConstraint;
begin
  Result := TDeltaConstraint.CreateUnique(AName, AFields);
  FConstraints.Add(Result);
end;

function TDeltaModel.AddUniqueConstraint(
  const AFields: array of string): TDeltaConstraint;
begin
  Result := AddUniqueConstraint('', AFields);
end;

function TDeltaModel.AddCheckConstraint(const AName: string;
  const AExpression: string): TDeltaConstraint;
begin
  Result := TDeltaConstraint.CreateCheck(AName, AExpression);
  FConstraints.Add(Result);
end;

function TDeltaModel.AddIndex(const AName: string;
  const AFields: array of string; AIsUnique: Boolean): TDeltaIndex;
begin
  Result := TDeltaIndex.Create(AName, AFields, AIsUnique);
  FIndexes.Add(Result);
end;

function TDeltaModel.AddIndex(const AFields: array of string;
  AIsUnique: Boolean): TDeltaIndex;
begin
  Result := AddIndex('', AFields, AIsUnique);
end;

function TDeltaModel.AddUniqueIndex(const AName: string;
  const AFields: array of string): TDeltaIndex;
begin
  Result := AddIndex(AName, AFields, True);
end;

function TDeltaModel.AddUniqueIndex(
  const AFields: array of string): TDeltaIndex;
begin
  Result := AddIndex('', AFields, True);
end;

procedure TDeltaModel.BeforeInsert;
begin
end;

procedure TDeltaModel.AfterInsert;
begin
end;

procedure TDeltaModel.BeforeUpdate;
begin
end;

procedure TDeltaModel.AfterUpdate;
begin
end;

procedure TDeltaModel.BeforeDelete;
begin
end;

procedure TDeltaModel.AfterDelete;
begin
end;

procedure TDeltaModel.BeforeSave;
begin
end;

procedure TDeltaModel.AfterSave;
begin
end;

{ TDeltaModelList }

function TDeltaModelList.GetPage: Integer;
begin
  Result := FPage;
end;

procedure TDeltaModelList.SetPage(AValue: Integer);
begin
  FPage := AValue;
end;

function TDeltaModelList.GetPageSize: Integer;
begin
  Result := FPageSize;
end;

procedure TDeltaModelList.SetPageSize(AValue: Integer);
begin
  FPageSize := AValue;
end;

function TDeltaModelList.GetTotalRecords: Int64;
begin
  Result := FTotalRecords;
end;

procedure TDeltaModelList.SetTotalRecords(AValue: Int64);
begin
  FTotalRecords := AValue;
end;

function TDeltaModelList.GetTotalPages: Integer;
begin
  Result := FTotalPages;
end;

procedure TDeltaModelList.SetTotalPages(AValue: Integer);
begin
  FTotalPages := AValue;
end;

function TDeltaModelList.GetTotal: Int64;
begin
  Result := FTotalRecords;
end;

procedure TDeltaModelList.SetTotal(AValue: Int64);
begin
  FTotalRecords := AValue;
end;

function TDeltaModelList.GetLimit: Integer;
begin
  Result := FPageSize;
end;

procedure TDeltaModelList.SetLimit(AValue: Integer);
begin
  FPageSize := AValue;
end;

procedure TDeltaModelList.FromJson(JsonStr: string);
var
  JsonData: TJSONData;
  Arr: TJSONArray;
  Element: TJSONObject;
  Obj: TDeltaModel;
  I: Integer;
  MetaObj, JObj: TJSONObject;
  ItemsData: TJSONData;
begin
  if FDeltaModelClass = nil then
  begin
    raise Exception.Create(DeltaModelClassNotAssigned);
  end;

  JsonData := GetJSON(JsonStr);
  try
    Self.Records.Clear;
    if JsonData is TJSONObject then
    begin
      JObj := TJSONObject(JsonData);

      FPage := JObj.Get('page', 1);
      FPageSize := JObj.Get('page_size', 20);
      FTotalRecords := JObj.Get('total_records', Int64(0));
      FTotalPages := JObj.Get('total_pages', 1);

      Arr := nil;
      ItemsData := JObj.Find('items');
      if ItemsData = nil then
        ItemsData := JObj.Find('data');
      if (ItemsData <> nil) and (ItemsData is TJSONArray) then
        Arr := TJSONArray(ItemsData);

      if Assigned(Arr) then
      begin
        for I := 0 to Pred(Arr.Count) do
        begin
          Obj := FDeltaModelClass.Create;
          Self.Add(Obj);
          Element := Arr.Items[I] as TJSONObject;
          DeserializeObj(Obj, Element);
        end;
        if (FTotalRecords = 0) and (Arr.Count > 0) then
          FTotalRecords := Arr.Count;
      end;
    end
    else if JsonData is TJSONArray then
    begin
      Arr := TJSONArray(JsonData);
      FTotalRecords := Arr.Count;
      FPage := 1;
      FPageSize := Arr.Count;
      if FPageSize < 1 then FPageSize := 20;
      FTotalPages := 1;

      for I := 0 to Pred(Arr.Count) do
      begin
        Obj := FDeltaModelClass.Create;
        Self.Add(Obj);
        Element := Arr.Items[I] as TJSONObject;
        DeserializeObj(Obj, Element);
      end;
    end
    else
      raise Exception.CreateFmt('Expected a JSON Array or Paginated Object with items/data array, but got %s', [JsonData.ClassName]);
  finally
    JsonData.Free;
  end;
end;

function TDeltaModelList.ToPaginatedJsonObj: TJSONObject;
var
  JObj: TJSONObject;
  CalcTotalPages: Integer;
  CurLimit: Integer;
  CurTotal: Int64;
begin
  CurLimit := FPageSize;
  if CurLimit <= 0 then CurLimit := 20;
  FPageSize := CurLimit;

  if FPage <= 0 then FPage := 1;

  CurTotal := FTotalRecords;
  if (CurTotal = 0) and (FRecords.Count > 0) then
    CurTotal := FRecords.Count;
  FTotalRecords := CurTotal;

  CalcTotalPages := (CurTotal + CurLimit - 1) div CurLimit;
  if CalcTotalPages < 1 then CalcTotalPages := 1;
  FTotalPages := CalcTotalPages;

  JObj := TJSONObject.Create;
  // Canonical DeltaModelList structure:
  // items, page, page_size, total_records, total_pages
  JObj.Add('items', Self.ToJsonObj);
  JObj.Add('page', FPage);
  JObj.Add('page_size', FPageSize);
  JObj.Add('total_records', FTotalRecords);
  JObj.Add('total_pages', FTotalPages);

  Result := JObj;
end;

function TDeltaModelList.ToJson: RawByteString;
begin
  Result := TDeltaFastJsonStreamer.ListToJson(Self);
end;

function TDeltaModelList.ToPaginatedJson: RawByteString;
begin
  Result := TDeltaFastJsonStreamer.PaginatedListToJson(Self);
end;

function TDeltaModelList.ToJsonObj: TJSONArray;
var
  Arr: TJSONArray;
  I: Integer;
begin
  Arr := TJSONArray.Create();
  for I := 0 to Pred(Self.Records.Count) do
  begin
    Arr.Add(SerializeToJsonObj(Self.Records.Items[I]));
  end;
  Result := Arr;
end;

function TDeltaModelList.SwaggerSchema(AddExamples: Boolean): TJSONObject;
begin
  Result := SwaggerSchemaPaginated(AddExamples);
end;

function TDeltaModelList.SwaggerSchemaArray(AddExamples: Boolean): TJSONObject;
var
  Obj: TDeltaModel;
begin
  if FDeltaModelClass = nil then
    raise Exception.Create(DeltaModelClassNotAssigned);
  Obj := FDeltaModelClass.Create;
  try
    Result := GenerateSchema(Obj, AddExamples, True);
  finally
    Obj.Free;
  end;
end;

function TDeltaModelList.SwaggerSchemaPaginated(AddExamples: Boolean): TJSONObject;
var
  Obj: TDeltaModel;
begin
  if FDeltaModelClass = nil then
    raise Exception.Create(DeltaModelClassNotAssigned);
  Obj := FDeltaModelClass.Create;
  try
    Result := GeneratePaginatedSchema(Obj, AddExamples);
  finally
    Obj.Free;
  end;
end;

function TDeltaModelList.SwaggerSchemaRefArray: string;
begin
  if FDeltaModelClass = nil then
    raise Exception.Create(DeltaModelClassNotAssigned);
  Result := FDeltaModelClass.SwaggerSchemaRefArray;
end;

function TDeltaModelList.SwaggerSchemaRefPaginated: string;
begin
  if FDeltaModelClass = nil then
    raise Exception.Create(DeltaModelClassNotAssigned);
  Result := FDeltaModelClass.SwaggerSchemaRefPaginated;
end;

function TDeltaModelList.SetDeltaModelClass(AClass: TDeltaModelClass): TDeltaModelList;
begin
  Result := Self;
  Self.FDeltaModelClass := AClass;
end;

procedure TDeltaModelList.AfterConstruction;
begin
  inherited AfterConstruction;
  FRecords := TDeltaModelRecords.Create;
  FPage := 1;
  FPageSize := 20;
  FTotalRecords := 0;
  FTotalPages := 1;
end;

function TDeltaModelList.Add(AModel: TDeltaModel): Integer;
begin
  Result := FRecords.AddInternal(AModel);
end;

function TDeltaModelList.Count: Integer;
begin
  Result := FRecords.Count;
end;

function TDeltaModelList.GetItem(AIndex: Integer): TDeltaModel;
begin
  Result := FRecords[AIndex];
end;

procedure TDeltaModelList.FromCSV(const CSVStr: string; ADelimiter: Char);
begin
  DeltaSerialization.DeserializeCSVToList(Self, CSVStr, ADelimiter);
end;

function TDeltaModelList.ToCSV(ADelimiter: Char): string;
begin
  Result := DeltaSerialization.SerializeListToCSV(Self, ADelimiter);
end;

function TDeltaModelList.GetItemObj(AIndex: Integer): TObject;
begin
  Result := FRecords[AIndex];
end;

procedure TDeltaModelList.ClearList;
begin
  FRecords.Clear;
  FTotalRecords := 0;
  FPage := 1;
  FTotalPages := 1;
end;

procedure TDeltaModelList.AddObj(AObj: TObject);
begin
  FRecords.AddInternal(AObj as TDeltaModel);
end;

function TDeltaModelList.GetModelClass: TClass;
begin
  Result := FDeltaModelClass;
end;

function TDeltaModelList.NewItem: TObject;
begin
  Result := FDeltaModelClass.Create;
end;

procedure TDeltaModelList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FRecords.Free;
end;

{ TGDeltaModelListEnumerator }

constructor TGDeltaModelListEnumerator.Create(AList: TDeltaModelList);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGDeltaModelListEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FList <> nil) and (FIndex < FList.Count);
end;

function TGDeltaModelListEnumerator.GetCurrent: T;
begin
  Result := T(FList[FIndex]);
end;

{ TGDeltaModelList }

constructor TGDeltaModelList.Create;
begin
  inherited Create;
  FDeltaModelClass := T;
end;

function TGDeltaModelList.Add(AModel: T): Integer;
begin
  Result := FRecords.AddInternal(AModel);
end;

function TGDeltaModelList.GetTypedItem(AIndex: Integer): T;
begin
  Result := T(FRecords[AIndex]);
end;

function TGDeltaModelList.First: T;
begin
  if FRecords.Count > 0 then
    Result := T(FRecords[0])
  else
    Result := nil;
end;

function TGDeltaModelList.Last: T;
begin
  if FRecords.Count > 0 then
    Result := T(FRecords[FRecords.Count - 1])
  else
    Result := nil;
end;

function TGDeltaModelList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TDeltaModelPaginatedList }

function TDeltaModelPaginatedList.ToJson: RawByteString;
begin
  Result := TDeltaFastJsonStreamer.PaginatedListToJson(Self);
end;

function TDeltaModelPaginatedList.SwaggerSchema(AddExamples: Boolean): TJSONObject;
begin
  Result := SwaggerSchemaPaginated(AddExamples);
end;

{ TGDeltaModelPaginatedList }

constructor TGDeltaModelPaginatedList.Create;
begin
  inherited Create;
  FDeltaModelClass := T;
end;

function TGDeltaModelPaginatedList.Add(AModel: T): Integer;
begin
  Result := FRecords.AddInternal(AModel);
end;

function TGDeltaModelPaginatedList.GetTypedItem(AIndex: Integer): T;
begin
  Result := T(FRecords[AIndex]);
end;

function TGDeltaModelPaginatedList.First: T;
begin
  if FRecords.Count > 0 then
    Result := T(FRecords[0])
  else
    Result := nil;
end;

function TGDeltaModelPaginatedList.Last: T;
begin
  if FRecords.Count > 0 then
    Result := T(FRecords[FRecords.Count - 1])
  else
    Result := nil;
end;

function TGDeltaModelPaginatedList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

end.
