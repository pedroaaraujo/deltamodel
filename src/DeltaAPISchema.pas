unit DeltaAPISchema;

interface

uses
  classes, sysutils, fpjson, jsonparser, TypInfo, Variants, fgl,
  DeltaModel.Fields, DeltaModel.List;

function GenerateSchema(Obj: TObject; AddExamples: Boolean = False; IsArray: Boolean = False): TJSONObject;
function GenerateSchemaStr(Obj: TObject; AddExamples: Boolean = False; IsArray: Boolean = False): string;

implementation

function GenerateSchema(Obj: TObject; AddExamples: Boolean; IsArray: Boolean): TJSONObject;
var
  JsonData: TJSONObject;
  JsonArray: TJSONObject;
  PropList: PPropList;
  PropInfo: PPropInfo;
  I, PropCount: integer;
  NestedObj: TObject;
  PropName: string;
  SchemaObj: TJSONObject;
  PropValue: Variant;
  ListObj: TFPSList;
  FirstItem: TObject;
begin
  if not Assigned(Obj) then Exit(nil);

  JsonData := TJSONObject.Create;

  JsonData.Add('type', 'object');
  JsonData.Add('properties', TJSONObject.Create);

  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropName := PropInfo^.Name;
      PropValue := GetPropValue(Obj, PropName, False);

      SchemaObj := TJSONObject.Create;
      case PropInfo^.PropType^.Kind of
        tkInteger:
          begin
            SchemaObj.Add('type', 'integer');
            if AddExamples then
              SchemaObj.Add('example', Integer(PropValue));
          end;
        tkChar, tkWChar, tkString, tkLString, tkAString, tkWString, tkUString:
          begin
            SchemaObj.Add('type', 'string');
            if AddExamples then
              SchemaObj.Add('example', string(PropValue));
          end;
        tkBool:
          begin
            SchemaObj.Add('type', 'boolean');
            if AddExamples then
              SchemaObj.Add('example', Boolean(PropValue));
          end;
        tkInt64:
          begin
            SchemaObj.Add('type', 'integer');
            if AddExamples then
              SchemaObj.Add('example', Int64(PropValue));
          end;
        tkEnumeration:
          begin
            SchemaObj.Add('type', 'string');
            if AddExamples then
              SchemaObj.Add('example', GetEnumName(PropInfo^.PropType, Integer(PropValue)));
          end;
        tkFloat:
          begin
            SchemaObj.Add('type', 'number');
            if AddExamples then
              SchemaObj.Add('example', Double(PropValue));
          end;
        tkVariant:
          begin
            SchemaObj.Add('type', 'string');
            if AddExamples then
              SchemaObj.Add('example', VarToStr(PropValue));
          end;
        tkClass:
          begin
            NestedObj := GetObjectProp(Obj, PropInfo^.Name);
            if Assigned(NestedObj) then
            begin
              if NestedObj is TDeltaField then
              begin
                SchemaObj.Add('type', (NestedObj as TDeltaField).SwaggerDataType);
              end
              else
              if NestedObj is TCustomDeltaModelList then
              begin
                SchemaObj.Add('type', 'array');
                SchemaObj.Add('items', (NestedObj as TCustomDeltaModelList).SwaggerSchema(AddExamples));
              end
              else
              if NestedObj is TFPSList then
              begin
                SchemaObj.Add('type', 'array');
                ListObj := TFPSList(NestedObj);
                if ListObj.Count > 0 then
                begin
                  FirstItem := TObject(ListObj.Items[0]^);
                  if Assigned(FirstItem) then
                  begin
                    SchemaObj.Add('items', GenerateSchema(FirstItem, AddExamples, False));
                  end;
                end
                else
                begin
                  SchemaObj.Add('items', TJSONObject.Create);
                end;
              end
              else
              begin
                SchemaObj.Add('type', 'object');
                SchemaObj.Add('properties', GenerateSchema(NestedObj, AddExamples, False).Find('properties'));
              end;
            end
            else
              SchemaObj.Add('type', 'null');
          end;
      end;

      TJSONObject(JsonData.Objects['properties']).Add(PropName, SchemaObj);
    end;
    Result := JsonData;

    if IsArray then
    begin
      JsonArray := TJSONObject.Create;
      JsonArray.Add('type', 'array');
      JsonArray.Add('items', JsonData);
      Result := JsonArray;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;


function GenerateSchemaStr(Obj: TObject; AddExamples: Boolean; IsArray: Boolean
  ): string;
var
  Json: TJSONObject;
begin
  Json := GenerateSchema(Obj, AddExamples, IsArray);
  try
    Result := Json.AsJSON;
  finally
    Json.Free;
  end;
end;

end.
