unit DeltaModel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, TypInfo,
  DeltaAPISchema, DeltaSerialization, DeltaModelMessages, DeltaValidator,
  DeltaModel.Fields, DeltaModel.List;

type

  TFieldList = specialize TFPGObjectList<TDeltaField>;

  { TDeltaModel }

  TDeltaModel = class
  private
    FTableName: string;
    FValidator: TValidator;
    FFieldList: TFieldList;
    procedure CreateFields;
    procedure SetTableName(AValue: string);
  public
    property TableName: string read FTableName write SetTableName;
    procedure FromJson(JsonStr: string);
    procedure Validate; virtual;
    procedure BeforeDestruction; override;
    procedure Configure; virtual;
    procedure CopyObject(Obj: TDeltaModel);
    function Clone: TDeltaModel;
    function IsEmpty: Boolean;
    function ToJson: RawByteString;
    function ToJsonObj: TJSONObject;
    class function SwaggerSchema(IsArray: Boolean = False): string;
    constructor Create; virtual;

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
  public
    property DeltaModelClass: TDeltaModelClass read FDeltaModelClass write FDeltaModelClass;
    property Records: TDeltaModelRecords read FRecords;
    procedure FromJson(JsonStr: string); override;
    function ToJson: RawByteString; override;
    function ToJsonObj: TJSONArray; override;
    function SwaggerSchema(AddExamples: Boolean): TJSONObject; override;
    function SetDeltaModelClass(AClass: TDeltaModelClass): TDeltaModelList;
    function Add(AModel: TDeltaModel): Integer;
    function Count: Integer;
    function GetItem(AIndex: Integer): TDeltaModel;
    property Items[AIndex: Integer]: TDeltaModel read GetItem; default;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TDeltaModel }

procedure TDeltaModel.CreateFields;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
  I, PropCount: integer;
  PropObj: TObject;
  PropClass: TClass;
begin
  PropCount := GetPropList(Self.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Self.ClassInfo, tkProperties, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropType := PropInfo^.PropType;

      if (PropType^.Kind = tkClass) then
      begin
        PropClass := GetTypeData(PropType)^.ClassType;

        if PropClass.InheritsFrom(TDeltaField) then
        begin
          PropObj := GetObjectProp(Self, PropInfo);

          if (PropObj = nil) then
          begin
            PropObj := PropClass.Create;
            (PropObj as TDeltaField).FieldName := PropInfo^.Name;
            (PropObj as TDeltaField).Visible   := True;

            SetObjectProp(Self, PropInfo, PropObj);

            FFieldList.Add(PropObj as TDeltaField);
          end
          else
          begin
             if FFieldList.IndexOf(PropObj as TDeltaField) < 0 then
               FFieldList.Add(PropObj as TDeltaField);
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

    Req := (Field as TDeltaFieldRequired);

    // [Otimização] Uso de .Trim.IsEmpty para não forçar a varredura e cálculo de length desnecessários
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

class function TDeltaModel.SwaggerSchema(IsArray: Boolean): string;
var
  Obj: TDeltaModel;
begin
  Obj := Create;
  try
    Result := GenerateSchemaStr(Obj, False, IsArray);
  finally
    Obj.Free;
  end;
end;

constructor TDeltaModel.Create;
begin
  FValidator := TValidator.Create;
  FFieldList := TFieldList.Create();

  CreateFields();

  FTableName := AnsiLowerCase(Copy(Self.ClassName, 2, MaxInt));

  Configure();
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

procedure TDeltaModelList.FromJson(JsonStr: string);
var
  JsonData: TJSONData;
  Arr: TJSONArray;
  Element: TJSONObject;
  Obj: TDeltaModel;
  I: Integer;
begin
  if FDeltaModelClass = nil then
  begin
    raise Exception.Create(DeltaModelClassNotAssigned);
  end;

  // [Correção de Memory Leak]
  // O JsonData é obtido genericamente. Se for um objeto e não um array, ele é destruído corretamente no Finally.
  JsonData := GetJSON(JsonStr);
  try
    if not (JsonData is TJSONArray) then
      raise Exception.CreateFmt('Expected a JSON Array but got %s', [JsonData.ClassName]);

    Arr := TJSONArray(JsonData);
    Self.Records.Clear;
    for I := 0 to Pred(Arr.Count) do
    begin
      Obj := FDeltaModelClass.Create;
      Self.Records.Add(Obj);
      Element := Arr.Items[I] as TJSONObject;
      DeserializeObj(Obj, Element);
    end;
  finally
    JsonData.Free;
  end;
end;

function TDeltaModelList.ToJson: RawByteString;
var
  JsonArr: TJSONArray;
  I: Integer;
begin
  JsonArr := TJSONArray.Create();
  try
    for I := 0 to Pred(Self.Records.Count) do
    begin
      JsonArr.Add(
        SerializeToJsonObj(
          Self.Records.Items[I]
        )
      );
    end;
    Result := JsonArr.AsJSON;
  finally
    JsonArr.Free;
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
var
  Obj: TDeltaModel;
begin
  Obj := Self.DeltaModelClass.Create;
  try
    Result := GenerateSchema(Obj, AddExamples);
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

procedure TDeltaModelList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FRecords.Free;
end;

end.
