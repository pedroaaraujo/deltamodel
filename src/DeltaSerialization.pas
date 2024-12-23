unit DeltaSerialization;

interface

uses
  classes, sysutils, fpjson, jsonparser, TypInfo, Variants, fgl, DeltaModel.Types;

procedure Deserialize(Obj: TObject; JsonString: string);
procedure DeserializeObj(Obj: TObject; JsonData: TJSONObject);
function Serialize(Obj: TObject): string;
function SerializeToJsonObj(Obj: TObject): TJSONObject;

implementation

procedure Deserialize(Obj: TObject; JsonString: string);
var
  JsonData: TJSONObject;
begin
  if JsonString.Trim.IsEmpty then 
    Exit;

  JsonData := TJSONObject(GetJSON(JsonString, False));
  try
    DeserializeObj(Obj, JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure DeserializeObj(Obj: TObject; JsonData: TJSONObject);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: PTypeInfo;
  I, PropCount: integer;
  PropValue: TJSONData;
  PropObj: TObject;
  Value: string;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropType := PropInfo^.PropType;
      case PropType^.Kind of
        tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString:
        begin
          if JsonData.Find(PropInfo^.Name, PropValue) or
             JsonData.Find(LowerCase(PropInfo^.Name), PropValue) then
          begin
            {$IFDEF MSWINDOWS}
            Value := Utf8ToAnsi(UTF8Encode(PropValue.AsString));
            {$ELSE}
            Value := PropValue.AsString;
            {$ENDIF}
            SetPropValue(Obj, PropInfo^.Name, Value);
          end;
        end;

        tkInteger, tkInt64, tkEnumeration, tkFloat, tkVariant, tkBool:
        begin
          if JsonData.Find(PropInfo^.Name, PropValue) or
             JsonData.Find(LowerCase(PropInfo^.Name), PropValue) then
          begin
            Value := PropValue.Value;
            SetPropValue(Obj, PropInfo^.Name, Value);
          end;
        end;
        tkClass:
        begin
          if JsonData.Find(PropInfo^.Name, PropValue) or
             JsonData.Find(LowerCase(PropInfo^.Name), PropValue) then
          begin
            PropObj := GetObjectProp(Obj, PropInfo^.Name);

            if PropObj is TIntNull then
            begin
              (PropObj as TIntNull).Value := PropValue.Value;
            end
            else
            if Assigned(PropObj) then
            begin
              DeserializeObj(PropObj, TJSONObject(PropValue));
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function Serialize(Obj: TObject): string;
var
  JsonData: TJSONObject;
begin
  if Obj = nil then 
    Exit('{}');

  JsonData := SerializeToJsonObj(Obj);
  try
    Result := JsonData.AsJSON;
  finally
    JsonData.Free;
  end;
end;

function SerializeToJsonObj(Obj: TObject): TJSONObject;
var
  JsonData: TJSONObject;
  JsonArr: TJSONArray;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropType: TTypeInfo;
  PropValue: Variant;
  I, PropCount, Item: integer;
  NestedObj: TObject;
  ObjectItem: TObject;
  PropName: string;
begin
  if (not Assigned(@Obj)) or (Obj = nil) then 
    Exit(nil);

  JsonData := TJSONObject.Create;
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      PropType := PropInfo^.PropType^;
      PropName := PropInfo^.Name;
      try
        PropValue := GetPropValue(Obj, PropName);
      except
        Continue;
      end;

      case PropType.Kind of
        tkInteger:
          JsonData.Add(PropName, Integer(PropValue));
        tkString, tkLString, tkAString:
          JsonData.Add(PropName, string(PropValue));
        tkBool:
          JsonData.Add(PropName, Boolean(PropValue));
        tkInt64:
          JsonData.Add(PropName, Int64(PropValue));
        tkEnumeration:
          JsonData.Add(PropName, VarToStr(PropValue));
        tkFloat:
          JsonData.Add(PropName, Double(PropValue));
        tkChar:
          JsonData.Add(PropName, Char(PropValue));
        tkWChar:
          JsonData.Add(PropName, WideChar(PropValue));
        tkWString:
          JsonData.Add(PropName, WideString(PropValue));
        tkVariant:
          JsonData.Add(PropName, VarToStr(PropValue));
        tkClass:
          begin
            NestedObj := GetObjectProp(Obj, PropInfo^.Name);
            if Assigned(NestedObj) then
            begin
              if NestedObj is TFPSList then
              begin
                JsonArr := TJSONArray.Create();
                JsonData.Add(PropInfo^.Name, JsonArr);

                for Item := 0 to Pred((NestedObj as TFPSList).Count) do
                begin
                  ObjectItem := TObject(TFPSList(NestedObj).Items[Item]^);
                  JsonArr.Add(SerializeToJsonObj(ObjectItem));
                end;
              end
              else
              if NestedObj is TIntNull then
              begin
                try
                  JsonData.Add(PropName, Int64(TIntNull(NestedObj).Value));
                except
                  JsonData.Add(PropInfo^.Name, TJSONNull.Create);
                end;
              end
              else
              begin
                JsonData.Add(PropInfo^.Name, SerializeToJsonObj(NestedObj));
              end;
            end
            else
            begin
              JsonData.Add(PropInfo^.Name, TJSONNull.Create);
            end;
          end;
      end;
    end;
    Result := JsonData;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

end.
