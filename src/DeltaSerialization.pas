unit DeltaSerialization;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, TypInfo, Variants, fgl,
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
  I, PropCount: Integer;
  PropValue: TJSONData;
  PropObj: TObject;
  PropName, ValueStr: string;
begin
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      if PropInfo^.SetProc = nil then Continue;

      PropName := PropInfo^.Name;

      if not JsonData.Find(PropName, PropValue) then
      begin
        if not JsonData.Find(LowerCase(PropName), PropValue) then
          Continue;
      end;

      PropType := PropInfo^.PropType;
      case PropType^.Kind of
        tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString:
        begin
          {$IFDEF MSWINDOWS}
          ValueStr := Utf8ToAnsi(UTF8Encode(PropValue.AsString));
          {$ELSE}
          ValueStr := PropValue.AsString;
          {$ENDIF}
          SetStrProp(Obj, PropInfo, ValueStr);
        end;

        tkInteger, tkInt64:
          SetOrdProp(Obj, PropInfo, PropValue.AsInt64);

        tkBool:
          SetOrdProp(Obj, PropInfo, Ord(PropValue.AsBoolean));

        tkEnumeration:
        begin
          if PropValue.JSONType = jtString then
            SetOrdProp(Obj, PropInfo, GetEnumValue(PropType, PropValue.AsString))
          else
            SetOrdProp(Obj, PropInfo, PropValue.AsInteger);
        end;

        tkFloat:
          SetFloatProp(Obj, PropInfo, PropValue.AsFloat);

        tkVariant:
          SetVariantProp(Obj, PropInfo, PropValue.Value);

        tkClass:
        begin
          PropObj := GetObjectProp(Obj, PropInfo);

          if (PropObj is TDeltaField) then
          begin
            try
              if not (PropObj as TDeltaField).Visible then
                Continue;

              if (PropObj is TDFDateRequired) or (PropObj is TDFDateNull) or
                 (PropObj is TDFTimeRequired) or (PropObj is TDFTimeNull) or
                 (PropObj is TDFDateTimeRequired) or (PropObj is TDFDateTimeNull) then
              begin
                if (PropValue.JSONType = jtNull) then
                  (PropObj as TDeltaField).Clear
                else
                  DateTimeToField((PropObj as TDeltaField), PropValue.AsString);
              end
              else
              begin
                case PropValue.JSONType of
                  jtNumber:  (PropObj as TDeltaField).Value := PropValue.AsFloat;
                  jtString:  (PropObj as TDeltaField).Value := PropValue.AsString;
                  jtBoolean: (PropObj as TDeltaField).Value := PropValue.AsBoolean;
                  jtNull:    (PropObj as TDeltaField).Value := Null;
                else
                  raise Exception.Create('Incompatible type');
                end;
              end;
            except
              on E: Exception do
                raise Exception.CreateFmt('Invalid value for "%s.%s". %s', [Obj.ClassName, PropName, E.Message]);
            end;
          end
          else if Assigned(PropObj) then
          begin
            if PropValue is TJSONObject then
              DeserializeObj(PropObj, TJSONObject(PropValue));
          end
          else
          begin
            PropObj := GetTypeData(PropInfo^.PropType)^.ClassType.Create;
            SetObjectProp(Obj, PropInfo, PropObj);

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
  I, PropCount, Item: Integer;
  NestedObj: TObject;
  ObjectItem: TObject;
  PropName: string;
  VariantVal: Variant;
begin
  if (not Assigned(Obj)) or (Obj = nil) then
    Exit(nil);

  JsonData := TJSONObject.Create;
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Obj.ClassInfo, tkProperties, PropList, False);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];

      if PropInfo^.GetProc = nil then Continue;

      PropType := PropInfo^.PropType^;
      PropName := PropInfo^.Name;

      case PropType.Kind of
        tkInteger, tkInt64:
          JsonData.Add(PropName, GetOrdProp(Obj, PropInfo));

        tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
          JsonData.Add(PropName, GetStrProp(Obj, PropInfo));

        tkBool:
          JsonData.Add(PropName, Boolean(GetOrdProp(Obj, PropInfo)));

        tkEnumeration:
          JsonData.Add(PropName, GetEnumName(PropInfo^.PropType, GetOrdProp(Obj, PropInfo)));

        tkFloat:
          JsonData.Add(PropName, GetFloatProp(Obj, PropInfo));

        tkVariant:
          JsonData.Add(PropName, VarToStr(GetVariantProp(Obj, PropInfo)));

        tkClass:
        begin
          NestedObj := GetObjectProp(Obj, PropInfo);

          if Assigned(NestedObj) then
          begin
            if NestedObj is TDeltaField then
            begin
              try
                if (NestedObj as TDeltaField).IsNull then
                begin
                  JsonData.Add(PropName, TJSONNull.Create);
                end
                else if (NestedObj is TDFDateRequired) or (NestedObj is TDFDateNull) or
                        (NestedObj is TDFTimeRequired) or (NestedObj is TDFTimeNull) or
                        (NestedObj is TDFDateTimeRequired) or (NestedObj is TDFDateTimeNull) then
                begin
                  JsonData.Add(PropName, (NestedObj as TDeltaField).AsString);
                end
                else
                begin
                  if not (NestedObj as TDeltaField).Visible then
                    Continue;

                  VariantVal := (NestedObj as TDeltaField).Value;
                  case VarType(VariantVal) of
                    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
                      JsonData.Add(PropName, Integer(VariantVal));
                    varSingle, varDouble, varCurrency:
                      JsonData.Add(PropName, Double(VariantVal));
                    varUString, varString, varOleStr:
                      JsonData.Add(PropName, string(VariantVal));
                    varBoolean:
                      JsonData.Add(PropName, Boolean(VariantVal));
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
            else if NestedObj is TCustomDeltaModelList then
            begin
              JsonData.Add(PropInfo^.Name, (NestedObj as TCustomDeltaModelList).ToJsonObj);
            end
            else if NestedObj is TFPSList then
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
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfoFrom, PropInfoTo: PPropInfo;
  ObjFrom, ObjTo: TObject;
begin
  if (AFrom = nil) or (ATo = nil) then Exit;

  if AFrom.ClassType = ATo.ClassType then
  begin
    PropCount := GetPropList(AFrom.ClassInfo, tkProperties, nil);
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropList(AFrom.ClassInfo, tkProperties, PropList, False);
      for I := 0 to PropCount - 1 do
      begin
        PropInfoFrom := PropList^[I];

        if (PropInfoFrom^.GetProc = nil) or (PropInfoFrom^.SetProc = nil) then
          Continue;

        case PropInfoFrom^.PropType^.Kind of
          tkInteger, tkInt64, tkBool, tkEnumeration:
            SetOrdProp(ATo, PropInfoFrom, GetOrdProp(AFrom, PropInfoFrom));

          tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString:
            SetStrProp(ATo, PropInfoFrom, GetStrProp(AFrom, PropInfoFrom));

          tkFloat:
            SetFloatProp(ATo, PropInfoFrom, GetFloatProp(AFrom, PropInfoFrom));

          tkVariant:
            SetVariantProp(ATo, PropInfoFrom, GetVariantProp(AFrom, PropInfoFrom));

          tkClass:
          begin
            ObjFrom := GetObjectProp(AFrom, PropInfoFrom);
            ObjTo   := GetObjectProp(ATo, PropInfoFrom);

            if (ObjFrom = nil) or (ObjTo = nil) then Continue;

            if ObjFrom is TDeltaField then
            begin
              if (ObjFrom as TDeltaField).Visible then
                (ObjTo as TDeltaField).Value := (ObjFrom as TDeltaField).Value;
            end
            else
              CopyObject(ObjFrom, ObjTo);
          end;
        end;
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end
  else
  begin
    PropCount := GetPropList(AFrom.ClassInfo, tkProperties, nil);
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropList(AFrom.ClassInfo, tkProperties, PropList, False);
      for I := 0 to PropCount - 1 do
      begin
        PropInfoFrom := PropList^[I];
        if PropInfoFrom^.GetProc = nil then Continue;

        PropInfoTo := GetPropInfo(ATo.ClassInfo, PropInfoFrom^.Name);
        if (PropInfoTo = nil) or (PropInfoTo^.SetProc = nil) then Continue;
        if PropInfoFrom^.PropType^.Kind <> PropInfoTo^.PropType^.Kind then Continue;

        case PropInfoFrom^.PropType^.Kind of
          tkInteger, tkInt64, tkBool, tkEnumeration:
            SetOrdProp(ATo, PropInfoTo, GetOrdProp(AFrom, PropInfoFrom));

          tkString, tkWString, tkLString, tkAString, tkChar, tkWChar, tkUnicodeString:
            SetStrProp(ATo, PropInfoTo, GetStrProp(AFrom, PropInfoFrom));

          tkFloat:
            SetFloatProp(ATo, PropInfoTo, GetFloatProp(AFrom, PropInfoFrom));

          tkVariant:
            SetVariantProp(ATo, PropInfoTo, GetVariantProp(AFrom, PropInfoFrom));

          tkClass:
          begin
            ObjFrom := GetObjectProp(AFrom, PropInfoFrom);
            ObjTo   := GetObjectProp(ATo, PropInfoTo);

            if (ObjFrom = nil) or (ObjTo = nil) then Continue;

            if ObjFrom is TDeltaField then
            begin
              if (ObjFrom as TDeltaField).Visible then
                (ObjTo as TDeltaField).Value := (ObjFrom as TDeltaField).Value;
            end
            else
              CopyObject(ObjFrom, ObjTo);
          end;
        end;
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;
end;

end.
