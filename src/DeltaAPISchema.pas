unit DeltaAPISchema;

{$mode ObjFPC}{$H+}

interface

uses
  classes, sysutils, fpjson, jsonparser, TypInfo, Variants, fgl,
  DeltaModel.Fields, DeltaModel.List;

function GenerateSchema(Obj: TObject; AddExamples: Boolean = False; IsArray: Boolean = False): TJSONObject;
function GenerateSchemaStr(Obj: TObject; AddExamples: Boolean = False; IsArray: Boolean = False): string;
function GeneratePaginatedSchema(Obj: TObject; AddExamples: Boolean = False): TJSONObject;
function GeneratePaginatedSchemaStr(Obj: TObject; AddExamples: Boolean = False): string;

implementation

uses
  DeltaModel;

function CreateModelInstance(AClass: TClass): TObject;
begin
  if AClass = nil then
    Exit(nil);
  if AClass.InheritsFrom(TDeltaModel) then
    Result := TDeltaModelClass(AClass).Create
  else
    Result := AClass.Create;
end;

function GenerateSchema(Obj: TObject; AddExamples: Boolean; IsArray: Boolean): TJSONObject;
var
  JsonData: TJSONObject;
  JsonArray: TJSONObject;
  PropList: PPropList;
  PropInfo: PPropInfo;
  I, PropCount: integer;
  NestedObj: TObject;
  NestedSchema: TJSONObject;
  PropsData: TJSONData;
  PropName: string;
  SchemaObj: TJSONObject;
  PropValue: Variant;
  ListObj: TFPSList;
  FirstItem: TObject;
  SwaggerFmt: string;
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
            SchemaObj.Add('format', 'int64');
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
            SchemaObj.Add('format', 'double');
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
              if (NestedObj is TDeltaField) then
              begin
                if not (NestedObj as TDeltaField).Visible then
                begin
                  SchemaObj.Free;
                  Continue;
                end;

                if NestedObj is TDFHasOne then
                begin
                  SchemaObj.Add('type', 'object');
                  if ((NestedObj as TDFHasOne).RelationClass <> nil) and
                     (NestedObj as TDFHasOne).RelationClass.InheritsFrom(TObject) then
                  begin
                    FirstItem := CreateModelInstance((NestedObj as TDFHasOne).RelationClass);
                    try
                      NestedSchema := GenerateSchema(FirstItem, AddExamples, False);
                      try
                        PropsData := NestedSchema.Extract('properties');
                        if Assigned(PropsData) then
                          SchemaObj.Add('properties', PropsData)
                        else
                          SchemaObj.Add('properties', TJSONObject.Create);
                      finally
                        NestedSchema.Free;
                      end;
                    finally
                      FirstItem.Free;
                    end;
                  end
                  else
                    SchemaObj.Add('properties', TJSONObject.Create);
                end
                else
                begin
                  SchemaObj.Add('type', (NestedObj as TDeltaField).SwaggerDataType);

                  // Adiciona format quando disponível
                  SwaggerFmt := (NestedObj as TDeltaField).SwaggerFormat;
                  if not SwaggerFmt.IsEmpty then
                    SchemaObj.Add('format', SwaggerFmt);

                  // Adiciona nullable para campos opcionais
                  if NestedObj is TDeltaFieldNullable then
                  begin
                    {$IF FPC_FULLVERSION >= 30200}
                    SchemaObj.Add('nullable', True);
                    {$ENDIF}
                  end;

                  if AddExamples and not (NestedObj as TDeltaField).IsNull then
                    SchemaObj.Add('example', (NestedObj as TDeltaField).AsString);
                end;
              end
              else
              if NestedObj is TCustomDeltaModelList then
              begin
                SchemaObj.Add('type', 'array');
                if (NestedObj as TCustomDeltaModelList).GetModelClass <> nil then
                begin
                  FirstItem := CreateModelInstance((NestedObj as TCustomDeltaModelList).GetModelClass);
                  try
                    SchemaObj.Add('items', GenerateSchema(FirstItem, AddExamples, False));
                  finally
                    FirstItem.Free;
                  end;
                end
                else
                  SchemaObj.Add('items', TJSONObject.Create);
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
                    SchemaObj.Add('items', GenerateSchema(FirstItem, AddExamples, False))
                  else
                    SchemaObj.Add('items', TJSONObject.Create);
                end
                else
                  SchemaObj.Add('items', TJSONObject.Create);
              end
              else
              begin
                SchemaObj.Add('type', 'object');
                NestedSchema := GenerateSchema(NestedObj, AddExamples, False);
                try
                  PropsData := NestedSchema.Extract('properties');
                  if Assigned(PropsData) then
                    SchemaObj.Add('properties', PropsData)
                  else
                    SchemaObj.Add('properties', TJSONObject.Create);
                finally
                  NestedSchema.Free;
                end;
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

function GeneratePaginatedSchema(Obj: TObject; AddExamples: Boolean): TJSONObject;
var
  RootObj, PropsObj, ItemsArray, ItemSchema: TJSONObject;
  TotalProp, PageProp, PageSizeProp, TotalPagesProp: TJSONObject;
begin
  RootObj := TJSONObject.Create;
  RootObj.Add('type', 'object');

  PropsObj := TJSONObject.Create;

  // 1. items (lista genérica)
  ItemSchema := GenerateSchema(Obj, AddExamples, False);
  if not Assigned(ItemSchema) then
    ItemSchema := TJSONObject.Create;

  ItemsArray := TJSONObject.Create;
  ItemsArray.Add('type', 'array');
  ItemsArray.Add('items', ItemSchema);
  PropsObj.Add('items', ItemsArray);

  // 2. page (inteiro)
  PageProp := TJSONObject.Create;
  PageProp.Add('type', 'integer');
  if AddExamples then PageProp.Add('example', 1);
  PropsObj.Add('page', PageProp);

  // 3. page_size (inteiro)
  PageSizeProp := TJSONObject.Create;
  PageSizeProp.Add('type', 'integer');
  if AddExamples then PageSizeProp.Add('example', 20);
  PropsObj.Add('page_size', PageSizeProp);

  // 4. total_records (inteiro)
  TotalProp := TJSONObject.Create;
  TotalProp.Add('type', 'integer');
  TotalProp.Add('format', 'int64');
  if AddExamples then TotalProp.Add('example', 100);
  PropsObj.Add('total_records', TotalProp);

  // 5. total_pages (inteiro)
  TotalPagesProp := TJSONObject.Create;
  TotalPagesProp.Add('type', 'integer');
  if AddExamples then TotalPagesProp.Add('example', 5);
  PropsObj.Add('total_pages', TotalPagesProp);

  RootObj.Add('properties', PropsObj);
  Result := RootObj;
end;

function GeneratePaginatedSchemaStr(Obj: TObject; AddExamples: Boolean): string;
var
  Json: TJSONObject;
begin
  Json := GeneratePaginatedSchema(Obj, AddExamples);
  try
    Result := Json.AsJSON;
  finally
    Json.Free;
  end;
end;

end.
