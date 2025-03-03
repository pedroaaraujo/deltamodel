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
   function ToJson: RawByteString;
   function ToJsonObj: TJSONObject;
   class function SwaggerSchema(IsArray: Boolean = False): string;
   constructor Create; virtual;
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
        PropObj := GetObjectProp(Self, PropInfo^.Name);
        if (PropObj = nil) then
        begin
          PropClass := GetTypeData(PropInfo^.PropType)^.ClassType;
          if PropClass.InheritsFrom(TDeltaField) then
          begin
            PropObj := PropClass.Create;
            (PropObj as TDeltaField).FieldName := PropInfo^.Name;
            SetObjectProp(Self, PropInfo, PropObj);
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
var
  Json: TJSONObject;
begin
  Json := SerializeToJsonObj(Obj);
  try
    DeserializeObj(Self, Json);
  finally
    Json.Free;
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
begin
  for I := 0 to Pred(Self.FFieldList.Count) do
  begin
    if (Self.FFieldList.Items[I] is TDeltaFieldRequired) and
       ((Self.FFieldList.Items[I] as TDeltaFieldRequired).IsNull) then
    begin
      raise EDeltaValidation.CreateFmt(
        'Field %s.%s id required',
        [
          Self.ClassName,
          (Self.FFieldList.Items[I] as TDFStringRequired).FieldName
        ]
      );
    end;

    if (Self.FFieldList.Items[I] is TDFStringRequired) and
       ((Self.FFieldList.Items[I] as TDFStringRequired).AsString.Trim.Length = 0) then
    begin
      raise EDeltaValidation.CreateFmt(
        'Invalid value for %s.%s',
        [
          Self.ClassName,
          (Self.FFieldList.Items[I] as TDFStringRequired).FieldName
        ]
      );
    end;
  end;
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
  FTableName := Copy(Self.ToString, 2, Length(Self.ToString));
  FTableName := FTableName.ToLower;

  AfterConstruction;

  Configure();
end;

{ TDeltaModelList }

procedure TDeltaModelList.FromJson(JsonStr: string);
var
  Arr: TJSONArray;
  Element: TJSONObject;
  Obj: TDeltaModel;
  I: Integer;
begin
  if FDeltaModelClass = nil then
  begin
    raise Exception.Create(DeltaModelClassNotAssigned);
  end;

  Arr := GetJSON(JsonStr) as TJSONArray;
  try
    Self.Records.Clear;
    for I := 0 to Pred(Arr.Count) do
    begin
      Obj := FDeltaModelClass.Create;
      Self.Records.Add(Obj);
      Element := Arr.Items[I] as TJSONObject;
      DeserializeObj(Obj, Element);
    end;
  finally
    Arr.Free;
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

procedure TDeltaModelList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FRecords.Free;
end;                                
end.
