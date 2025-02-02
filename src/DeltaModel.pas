unit DeltaModel;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, TypInfo,
  DeltaAPISchema, DeltaSerialization, DeltaModelMessages, DeltaValidator, DeltaModel.Fields;

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
 protected
   property Validator: TValidator read FValidator;
 public
   property TableName: string read FTableName write SetTableName;
   procedure FromJson(JsonStr: string);
   procedure Validate; virtual; abstract;
   procedure BeforeDestruction; override;
   function ToJson: string;
   function ToJsonObj: TJSONObject;
   class function SwaggerSchema(IsArray: Boolean = False): string;
   constructor Create; virtual;
 end;

 TDeltaModelClass = class of TDeltaModel;

 TCustomDeltaModelList = specialize TFPGObjectList<TDeltaModel>;

 IDeltaModelList = Interface
 procedure FromJson(JsonStr: string);
 function ToJson: string;
 function ToJsonObj: TJSONObject;
 end;

 { TDeltaModelList }

 TDeltaModelList = class
 private
   FDeltaModelClass: TDeltaModelClass;
   FRecords: TCustomDeltaModelList;
 public
   property DeltaModelClass: TDeltaModelClass read FDeltaModelClass write FDeltaModelClass;
   property Records: TCustomDeltaModelList read FRecords;
   procedure FromJson(JsonStr: string); virtual;
   function ToJson: string;
   function ToJsonObj: TJSONObject;
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

procedure TDeltaModel.SetTableName(AValue: string);
begin
  if FTableName = AValue then Exit;
  FTableName := AValue;
end;

procedure TDeltaModel.FromJson(JsonStr: string);
begin
  Deserialize(Self, JsonStr);
end;

function TDeltaModel.ToJson: string;
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

function TDeltaModelList.ToJson: string;
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

function TDeltaModelList.ToJsonObj: TJSONObject;
begin
  Result := SerializeToJsonObj(Self);
end;

function TDeltaModelList.SetDeltaModelClass(AClass: TDeltaModelClass): TDeltaModelList;
begin
  Result := Self;
  Self.FDeltaModelClass := AClass;
end;

procedure TDeltaModelList.AfterConstruction;
begin
  inherited AfterConstruction;
  FRecords := TCustomDeltaModelList.Create;
end;

procedure TDeltaModelList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FRecords.Free;
end;                                
end.
