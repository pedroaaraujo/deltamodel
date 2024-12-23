unit DeltaModel;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, TypInfo,
  DeltaAPISchema, DeltaSerialization, DeltaModelMessages, DeltaValidator, DeltaModel.Types;

type

 { TDeltaModel }

 TDeltaModel = class
 private
   FValidator: TValidator;
   procedure CreateFields;
   procedure AfterConstruction; override;
   procedure BeforeDestruction; override;
 protected
   property Validator: TValidator read FValidator;
 public
   procedure FromJson(JsonStr: string);
   function ToJson: string;
   function ToJsonObj: TJSONObject;
   class function SwaggerSchema(IsArray: Boolean = False): string;
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
          if Supports(PropClass, INullableValue) then
          begin
            PropObj := PropClass.Create;
            SetObjectProp(Self, PropInfo, PropObj);
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

procedure TDeltaModel.AfterConstruction;
begin
  inherited AfterConstruction;
  FValidator := TValidator.Create;
  CreateFields();
end;

procedure TDeltaModel.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FValidator.Free;
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
