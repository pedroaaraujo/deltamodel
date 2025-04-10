unit DeltaSerialization;

interface

uses
  classes, sysutils, fpjson, jsonparser, TypInfo, Variants, fgl,
  DeltaModel.Fields, DeltaModel.List;

procedure Deserialize(Obj: TObject; JsonString: string);
procedure DeserializeObj(Obj: TObject; JsonData: TJSONObject);
function Serialize(Obj: TObject): RawByteString;
function SerializeToJsonObj(Obj: TObject): TJSONObject;

procedure CopyObject(AFrom, ATo: TObject);

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
          if not (JsonData.Find(PropInfo^.Name, PropValue) or JsonData.Find(LowerCase(PropInfo^.Name), PropValue)) then
          begin
            Continue;
          end;

          PropObj := GetObjectProp(Obj, PropInfo^.Name);

          if (PropObj is TDeltaField) then
          begin
            try
              if not (PropObj as TDeltaField).Visible then
                Continue;

              if (PropObj is TDFDateRequired) or
                 (PropObj is TDFDateNull) or
                 (PropObj is TDFTimeRequired) or
                 (PropObj is TDFTimeNull) or
                 (PropObj is TDFDateTimeRequired) or
                 (PropObj is TDFDateTimeNull) then
              begin
                if (PropValue.JSONType = jtNull) then
                begin
                  (PropObj as TDeltaField).Clear;
                end
                else
                begin
                  DateTimeToField((PropObj as TDeltaField), PropValue.AsString);
                end;
              end
              else
              begin
                case PropValue.JSONType of
                  jtNumber:
                    (PropObj as TDeltaField).Value := PropValue.AsFloat;
                  jtString:
                    (PropObj as TDeltaField).Value := PropValue.AsString;
                  jtBoolean:
                    (PropObj as TDeltaField).Value := PropValue.AsBoolean;
                  jtNull:
                    (PropObj as TDeltaField).Value := Null;
                else
                  raise Exception.Create('Incompatible type');
                end;
              end;
            except
              on E: Exception do
              begin
                raise Exception.CreateFmt('Invalid value for "%s.%s". %s', [Obj.ClassName, PropInfo^.Name, E.Message]);
              end;
            end;
          end
          else
          if Assigned(PropObj) then
          begin
            if PropValue is TJSONObject then
              DeserializeObj(PropObj, TJSONObject(PropValue));
          end
          else
          begin
            PropObj := GetTypeData(PropInfo^.PropType)^.ClassType.Create;
            SetObjectProp(Obj, PropInfo^.Name, PropObj);
            if PropValue is TJSONObject then
              DeserializeObj(PropObj, TJSONObject(PropValue));
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;
end;

function Serialize(Obj: TObject): RawByteString;
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
  I, PropCount, Item: Integer;
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
              if NestedObj is TDeltaField then
              begin
                try
                  if (NestedObj as TDeltaField).IsNull then
                  begin
                    JsonData.Add(PropName, TJSONNull.Create);
                  end
                  else
                  if (NestedObj is TDFDateRequired) or
                     (NestedObj is TDFDateNull) or
                     (NestedObj is TDFTimeRequired) or
                     (NestedObj is TDFTimeNull) or
                     (NestedObj is TDFDateTimeRequired) or
                     (NestedObj is TDFDateTimeNull) then
                  begin
                    JsonData.Add(PropName, (NestedObj as TDeltaField).AsString);
                  end
                  else
                  begin
                    if not (NestedObj as TDeltaField).Visible then
                      Continue;

                    PropValue := (NestedObj as TDeltaField).Value;
                    case VarType(PropValue) of
                      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
                        JsonData.Add(PropName, Integer(PropValue));
                      varSingle, varDouble, varCurrency:
                        JsonData.Add(PropName, Double(PropValue));
                      varUString, varString, varOleStr:
                        JsonData.Add(PropName, string(PropValue));
                      varBoolean:
                        JsonData.Add(PropName, Boolean(PropValue));
                      varNull, varEmpty:
                        JsonData.Add(PropName, TJSONNull.Create);
                    else
                      raise Exception.CreateFmt('Unsupported type for property "%s".', [PropName]);
                    end;
                  end;
                except
                  JsonData.Add(PropName, TJSONNull.Create);
                end;
              end
              else
              if NestedObj is TCustomDeltaModelList then
              begin
                JsonData.Add(PropInfo^.Name, (NestedObj as TCustomDeltaModelList).ToJsonObj);
              end
              else
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

procedure CopyObject(AFrom, ATo: TObject);
var
  PropListFrom, PropListTo: PPropList;
  PropCountFrom, PropCountTo: Integer;
  PropInfo: PPropInfo;
  PropType: TTypeInfo;
  PropValue: Variant;
  ObjFrom, ObjTo: TObject;
  I: Integer;
  PropName: string;
begin
  PropCountFrom := GetPropList(AFrom.ClassInfo, tkProperties, nil);
  PropCountTo := GetPropList(ATo.ClassInfo, tkProperties, nil);
  GetMem(PropListFrom, PropCountFrom * SizeOf(Pointer));
  GetMem(PropListTo, PropCountTo * SizeOf(Pointer));
  try
    GetPropList(AFrom.ClassInfo, tkProperties, PropListFrom);
    GetPropList(ATo.ClassInfo, tkProperties, PropListTo);

    for I := 0 to Pred(PropCountFrom) do
    begin
      PropInfo := PropListFrom^[I];
      PropType := PropInfo^.PropType^;
      PropName := PropInfo^.Name;
      try
        PropValue := GetPropValue(AFrom, PropName);
      except
        Continue;
      end;

      case PropType.Kind of
        tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString,
        tkInteger, tkInt64, tkEnumeration, tkFloat, tkVariant, tkBool:
        begin
          SetPropValue(ATo, PropInfo^.Name, PropValue);
        end;

        tkClass:
        begin
          ObjFrom := GetObjectProp(AFrom, PropInfo^.Name);
          ObjTo   := GetObjectProp(ATo, PropInfo^.Name);

          if (ObjFrom = nil) or (ObjTo = nil) then
          begin
            Exit;
          end;

          if (ObjFrom is TDeltaField) then
          begin
            if not (ObjFrom as TDeltaField).Visible then
              Continue;

            (ObjTo as TDeltaField).Value := (ObjFrom as TDeltaField).Value;
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropListTo, PropCountTo * SizeOf(Pointer));
    FreeMem(PropListFrom, PropCountFrom * SizeOf(Pointer));
  end;
end;

end.
