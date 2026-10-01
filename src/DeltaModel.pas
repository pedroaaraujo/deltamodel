unit DeltaModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, TypInfo,
  DeltaAPISchema, DeltaSerialization, DeltaModelMessages, DeltaValidator,
  DeltaModel.Fields, DeltaModel.List;

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

  { TDeltaModel }

  TDeltaModel = class
  private
    FTableName: string;
    FValidator: TValidator;
    FFieldList: TFieldList;
    FConstraints: TConstraintList;
    FIndexes: TIndexList;
    procedure CreateFields;
    procedure SetTableName(AValue: string);
  public
    property TableName: string read FTableName write SetTableName;
    property FieldList: TFieldList read FFieldList;
    property Constraints: TConstraintList read FConstraints;
    property Indexes: TIndexList read FIndexes;
    procedure FromJson(JsonStr: string);
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

  TDeltaModelRecords = specialize TFPGObjectList<TDeltaModel>;

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

    // Aliases para retrocompatibilidade
    property Total: Int64 read GetTotal write SetTotal;
    property Limit: Integer read GetLimit write SetLimit;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property TotalRecords: Int64 read GetTotalRecords write SetTotalRecords;
    property TotalPages: Integer read GetTotalPages write SetTotalPages;

    procedure FromJson(JsonStr: string); override;
    function ToJson: RawByteString; override;
    function ToJsonObj: TJSONArray; override;
    function ToPaginatedJsonObj: TJSONObject; override;
    function SwaggerSchema(AddExamples: Boolean = False): TJSONObject; override; deprecated 'Use SwaggerSchemaArray or SwaggerSchemaPaginated instead';
    function SwaggerSchemaArray(AddExamples: Boolean = False): TJSONObject; override;
    function SwaggerSchemaPaginated(AddExamples: Boolean = False): TJSONObject; override;
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

  generic TGDeltaModelList<T: TDeltaModel> = class(TDeltaModelList)
  public
    constructor Create;
    function Add(AModel: T): Integer;
    function GetTypedItem(AIndex: Integer): T;
    property Item[AIndex: Integer]: T read GetTypedItem; default;
    function First: T;
    function Last: T;
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
    constructor Create;
    function Add(AModel: T): Integer;
    function GetTypedItem(AIndex: Integer): T;
    property Item[AIndex: Integer]: T read GetTypedItem; default;
    function First: T;
    function Last: T;
  end;

implementation

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
begin
  PropCount := GetPropList(Self.ClassInfo, tkProperties, nil);

  if PropCount = 0 then Exit;

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

            DeltaField.FieldName := PropInfo^.Name;
            DeltaField.Visible   := True;

            SetObjectProp(Self, PropInfo, PropObj);
            FFieldList.Add(DeltaField);
          end
          else
          begin
            DeltaField := TDeltaField(PropObj);
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
begin
  Result := True;
  for I := 0 to Pred(Self.FFieldList.Count) do
  begin
    Field := Self.FFieldList.Items[I];
    if not Field.IsNull then
    begin
      Result := False;
      Exit;
    end;
  end;
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
begin
  for I := 0 to Pred(Self.FFieldList.Count) do
  begin
    Field := Self.FFieldList.Items[I];

    if not (Field is TDeltaFieldRequired) then
    begin
      Continue;
    end;

    // Chave primária auto-incremento não preenchida será gerada pelo banco
    if (dboAutoInc in Field.DBOptions) and Field.IsNull then
      Continue;

    Req := (Field as TDeltaFieldRequired);

    if (Req.IsNull) or
       ((Req is TDFStringRequired) and ((Req as TDFStringRequired).AsString.Trim.IsEmpty)) then
    begin
      raise EDeltaValidation.CreateFmt(
        'Field %s.%s is required',
        [
          Self.ClassName,
          Req.FieldName
        ]
      );
    end;
  end;

  // [Sugestão Arquitetural] No futuro, delegue este laço acima diretamente para:
  // FValidator.CheckRequiredFields(Self.FFieldList);
end;

function TDeltaModel.ToJson: RawByteString;
begin
  Result := Serialize(Self);
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

constructor TDeltaModel.Create;
begin
  FValidator := TValidator.Create;
  FFieldList := TFieldList.Create();
  FConstraints := TConstraintList.Create();
  FIndexes := TIndexList.Create();

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
      if (ItemsData <> nil) and (ItemsData is TJSONArray) then
        Arr := TJSONArray(ItemsData);

      if Assigned(Arr) then
      begin
        for I := 0 to Pred(Arr.Count) do
        begin
          Obj := FDeltaModelClass.Create;
          Self.Records.Add(Obj);
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
        Self.Records.Add(Obj);
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
  // Estrutura canônica DeltaModelList:
  // items, page, page_size, total_records, total_pages
  JObj.Add('items', Self.ToJsonObj);
  JObj.Add('page', FPage);
  JObj.Add('page_size', FPageSize);
  JObj.Add('total_records', FTotalRecords);
  JObj.Add('total_pages', FTotalPages);

  Result := JObj;
end;

function TDeltaModelList.ToJson: RawByteString;
var
  JObj: TJSONObject;
begin
  JObj := ToPaginatedJsonObj;
  try
    Result := JObj.AsJSON;
  finally
    JObj.Free;
  end;
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
  Result := FRecords.Add(AModel);
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
  FRecords.Add(AObj as TDeltaModel);
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

{ TGDeltaModelList }

constructor TGDeltaModelList.Create;
begin
  inherited Create;
  FDeltaModelClass := T;
end;

function TGDeltaModelList.Add(AModel: T): Integer;
begin
  Result := FRecords.Add(AModel);
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

{ TDeltaModelPaginatedList }

function TDeltaModelPaginatedList.ToJson: RawByteString;
var
  JObj: TJSONObject;
begin
  JObj := ToPaginatedJsonObj;
  try
    Result := JObj.AsJSON;
  finally
    JObj.Free;
  end;
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
  Result := FRecords.Add(AModel);
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

end.
