unit DeltaModel.ORM.Hydrator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces;

type
  PResultBindingItem = ^TResultBindingItem;
  TResultBindingItem = record
    ColIndex: Integer;
    PropInfo: PPropInfo;
    FieldKind: TDeltaFieldKind;
    IsDeltaField: Boolean;
    RegularPropKind: TTypeKind;
    PropIndex: Integer;
  end;
  TResultBindingItemArray = array of TResultBindingItem;

  { TResultBindingMap: Pre-allocated column-to-property mapping for zero-alloc cursor hydration }
  TResultBindingMap = class
  private
    FBindings: TResultBindingItemArray;
    FBindingCount: Integer;
    FModelClass: TClass;
    procedure AddBinding(const AItem: TResultBindingItem);
    procedure BuildBindings(const ARS: IDeltaResultSet);
  public
    constructor Create(AModelClass: TClass; const ARS: IDeltaResultSet);
    procedure BindRecord(AModel: TDeltaModel; const ARS: IDeltaResultSet);
    property Bindings: TResultBindingItemArray read FBindings;
    property BindingCount: Integer read FBindingCount;
    property ModelClass: TClass read FModelClass;
  end;

  { TDeltaHydrator: ResultSet hydration, JSON export and parameter binding }
  TDeltaHydrator = class
  private
    class procedure AssignDeltaFieldParam(AParam: TDeltaParam;
      ADeltaField: TDeltaField; AFieldKind: TDeltaFieldKind); static;
    class procedure AssignPrimitiveFieldParam(AParam: TDeltaParam;
      AModel: TDeltaModel; AFieldMeta: PDeltaFieldMeta); static;
    class procedure AssignRegularPropParam(AParam: TDeltaParam;
      AModel: TDeltaModel; APropItem: PDeltaPropItem); static;
  public
    class procedure FromResultSet(AModel: TDeltaModel;
      const ARS: IDeltaResultSet); static;
    class procedure BindModelParams(AModel: TDeltaModel; AParams: TDeltaParams;
      const ASuffix: string = ''); static;
    class function ResultSetToJsonString(const ARS: IDeltaResultSet): RawByteString; static;
  end;

implementation

{ TResultBindingMap }

constructor TResultBindingMap.Create(AModelClass: TClass;
  const ARS: IDeltaResultSet);
begin
  inherited Create;
  FModelClass := AModelClass;
  BuildBindings(ARS);
end;

procedure TResultBindingMap.AddBinding(const AItem: TResultBindingItem);
begin
  SetLength(FBindings, FBindingCount + 1);
  FBindings[FBindingCount] := AItem;
  Inc(FBindingCount);
end;

procedure TResultBindingMap.BuildBindings(const ARS: IDeltaResultSet);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, ColIdx: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  Item: TResultBindingItem;
begin
  FBindingCount := 0;
  SetLength(FBindings, 0);
  if (ARS = nil) or (FModelClass = nil) then
    Exit;

  Meta := TRTTICache.GetModelMetadata(FModelClass);
  ClassProps := TRTTICache.GetClassProps(FModelClass);

  // 1. Fields registered in TModelMetadata (TDeltaField and fluent primitives)
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsVirtual then
      Continue;

    ColIdx := ARS.FindField(FieldMeta^.FieldName);
    if ColIdx < 0 then
      ColIdx := ARS.FindField(FieldMeta^.PropName);

    if ColIdx >= 0 then
    begin
      Item.ColIndex := ColIdx;
      Item.PropInfo := FieldMeta^.PropInfo;
      Item.FieldKind := FieldMeta^.FieldKind;
      Item.IsDeltaField := FieldMeta^.IsDeltaField;
      Item.PropIndex := FieldMeta^.PropIndex;
      if not FieldMeta^.IsDeltaField then
        Item.RegularPropKind := FieldMeta^.PropInfo^.PropType^.Kind
      else
        Item.RegularPropKind := tkUnknown;
      AddBinding(Item);
    end;
  end;

  // 2. Remaining published properties not registered in Schema
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasSetProc) then
      Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then
      Continue;

    ColIdx := ARS.FindField(PropItem^.Name);
    if ColIdx >= 0 then
    begin
      Item.ColIndex := ColIdx;
      Item.PropInfo := PropItem^.PropInfo;
      Item.FieldKind := dfkString;
      Item.IsDeltaField := False;
      Item.RegularPropKind := PropItem^.PropKind;
      Item.PropIndex := PropItem^.Index;
      AddBinding(Item);
    end;
  end;
end;

procedure TResultBindingMap.BindRecord(AModel: TDeltaModel;
  const ARS: IDeltaResultSet);
var
  I, ColIdx: Integer;
  Item: PResultBindingItem;
  DeltaField: TDeltaField;
  PropObj: TObject;
begin
  if (AModel = nil) or (ARS = nil) then
    Exit;
  AModel.ClearAllDirty;

  for I := 0 to FBindingCount - 1 do
  begin
    Item := @FBindings[I];
    ColIdx := Item^.ColIndex;

    if Item^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, Item^.PropInfo);
      if (PropObj = nil) or not (PropObj is TDeltaField) then
        Continue;
      DeltaField := TDeltaField(PropObj);

      if ARS.IsNull(ColIdx) then
      begin
        DeltaField.Clear;
        Continue;
      end;

      case Item^.FieldKind of
        dfkInteger:  DeltaField.AsLargeInt := ARS.AsLargeInt(ColIdx);
        dfkFloat:    DeltaField.AsFloat := ARS.AsFloat(ColIdx);
        dfkString:   DeltaField.SetAsString(ARS.AsString(ColIdx));
        dfkBoolean:  DeltaField.AsBoolean := ARS.AsBoolean(ColIdx);
        dfkDateTime: DeltaField.AsDateTime := ARS.AsDateTime(ColIdx);
      else
        DeltaField.Value := ARS.AsVariant(ColIdx);
      end;
    end
    else
    begin
      if ARS.IsNull(ColIdx) then
      begin
        AModel.SetNull(Item^.PropIndex);
        Continue;
      end;

      AModel.ClearNull(Item^.PropIndex);
      try
        case Item^.RegularPropKind of
          tkInteger, tkInt64:
            SetOrdProp(AModel, Item^.PropInfo, ARS.AsLargeInt(ColIdx));
          tkString, tkLString, tkAString, tkWString, tkUString:
            SetStrProp(AModel, Item^.PropInfo, ARS.AsString(ColIdx));
          tkFloat:
            SetFloatProp(AModel, Item^.PropInfo, ARS.AsFloat(ColIdx));
          tkBool, tkEnumeration:
            SetOrdProp(AModel, Item^.PropInfo, Ord(ARS.AsBoolean(ColIdx)));
        else
          SetPropValue(AModel, Item^.PropInfo^.Name, ARS.AsVariant(ColIdx));
        end;
      except
      end;
    end;
  end;
end;

{ TDeltaHydrator }

class procedure TDeltaHydrator.FromResultSet(AModel: TDeltaModel;
  const ARS: IDeltaResultSet);
var
  Map: TResultBindingMap;
begin
  if (AModel = nil) or (ARS = nil) or ARS.EOF then
    Exit;
  Map := TResultBindingMap.Create(AModel.ClassType, ARS);
  try
    Map.BindRecord(AModel, ARS);
  finally
    Map.Free;
  end;
end;

class procedure TDeltaHydrator.AssignDeltaFieldParam(AParam: TDeltaParam;
  ADeltaField: TDeltaField; AFieldKind: TDeltaFieldKind);
begin
  if ADeltaField.IsNull then
    AParam.Clear
  else if (ADeltaField is TDFTextNull) or (ADeltaField is TDFTextRequired) then
    AParam.AsMemo := ADeltaField.AsString
  else
  begin
    case AFieldKind of
      dfkInteger:  AParam.AsLargeInt := ADeltaField.AsLargeInt;
      dfkFloat:    AParam.AsFloat := ADeltaField.AsFloat;
      dfkString:   AParam.AsString := ADeltaField.AsString;
      dfkBoolean:  AParam.AsBoolean := ADeltaField.AsBoolean;
      dfkDateTime: AParam.AsDateTime := ADeltaField.AsDateTime;
    else
      AParam.AssignVariant(ADeltaField.Value);
    end;
  end;
end;

class procedure TDeltaHydrator.AssignPrimitiveFieldParam(AParam: TDeltaParam;
  AModel: TDeltaModel; AFieldMeta: PDeltaFieldMeta);
begin
  if AModel.IsNull(AFieldMeta^.PropIndex) then
  begin
    AParam.Clear;
    Exit;
  end;

  case AFieldMeta^.FieldKind of
    dfkInteger:  AParam.AsLargeInt := GetOrdProp(AModel, AFieldMeta^.PropInfo);
    dfkFloat:    AParam.AsFloat := GetFloatProp(AModel, AFieldMeta^.PropInfo);
    dfkBoolean:  AParam.AsBoolean := Boolean(GetOrdProp(AModel, AFieldMeta^.PropInfo));
    dfkDateTime: AParam.AsDateTime := GetFloatProp(AModel, AFieldMeta^.PropInfo);
    dfkString:   AParam.AsString := GetStrProp(AModel, AFieldMeta^.PropInfo);
  else
    AParam.AssignVariant(GetPropValue(AModel, AFieldMeta^.PropName));
  end;
end;

class procedure TDeltaHydrator.AssignRegularPropParam(AParam: TDeltaParam;
  AModel: TDeltaModel; APropItem: PDeltaPropItem);
begin
  try
    case APropItem^.PropKind of
      tkInteger, tkInt64:
        AParam.AsLargeInt := GetOrdProp(AModel, APropItem^.PropInfo);
      tkString, tkLString, tkAString, tkWString, tkUString:
        AParam.AsString := GetStrProp(AModel, APropItem^.PropInfo);
      tkFloat:
        AParam.AsFloat := GetFloatProp(AModel, APropItem^.PropInfo);
      tkBool, tkEnumeration:
        AParam.AsBoolean := Boolean(GetOrdProp(AModel, APropItem^.PropInfo));
    else
      AParam.AssignVariant(GetPropValue(AModel, APropItem^.Name));
    end;
  except
  end;
end;

class procedure TDeltaHydrator.BindModelParams(AModel: TDeltaModel;
  AParams: TDeltaParams; const ASuffix: string);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
  Param: TDeltaParam;
  ParamName: string;
begin
  if (AModel = nil) or (AParams = nil) then
    Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Metadata fields (TDeltaField and fluent primitives)
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsVirtual or FieldMeta^.FieldName.IsEmpty then
      Continue;

    ParamName := FieldMeta^.FieldName + ASuffix;
    Param := AParams.AddParam(ParamName);

    if FieldMeta^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
      if PropObj is TDeltaField then
        AssignDeltaFieldParam(Param, TDeltaField(PropObj), FieldMeta^.FieldKind);
    end
    else
      AssignPrimitiveFieldParam(Param, AModel, FieldMeta);

    if (not FieldMeta^.PropName.IsEmpty) and
       (not SameText(FieldMeta^.PropName, FieldMeta^.FieldName)) then
    begin
      Param := AParams.AddParam(FieldMeta^.PropName + ASuffix);
      Param.Assign(AParams.FindParam(ParamName));
      Param.Name := FieldMeta^.PropName + ASuffix;
    end;
  end;

  // 2. Remaining published properties
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then
      Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then
      Continue;

    Param := AParams.AddParam(PropItem^.Name + ASuffix);
    AssignRegularPropParam(Param, AModel, PropItem);
  end;
end;

function FastJSONEscape(const S: string): string;
const
  HexDigits: array[0..15] of Char = '0123456789abcdef';
var
  P, PEnd: PChar;
  Res: PChar;
  Len: PtrUInt;
begin
  if S = '' then
    Exit('""');

  SetLength(Result, Length(S) * 6 + 2);
  Res := PChar(Result);
  Res^ := '"';
  Inc(Res);

  P := PChar(S);
  PEnd := P + Length(S);

  while P < PEnd do
  begin
    case P^ of
      '"', '\':
        begin
          Res^ := '\'; Inc(Res);
          Res^ := P^;  Inc(Res);
        end;
      #8:  begin Res^ := '\'; Inc(Res); Res^ := 'b'; Inc(Res); end;
      #9:  begin Res^ := '\'; Inc(Res); Res^ := 't'; Inc(Res); end;
      #10: begin Res^ := '\'; Inc(Res); Res^ := 'n'; Inc(Res); end;
      #12: begin Res^ := '\'; Inc(Res); Res^ := 'f'; Inc(Res); end;
      #13: begin Res^ := '\'; Inc(Res); Res^ := 'r'; Inc(Res); end;
    else
      if Byte(P^) >= 32 then
      begin
        Res^ := P^;
        Inc(Res);
      end
      else
      begin
        Res^ := '\'; Inc(Res);
        Res^ := 'u'; Inc(Res);
        Res^ := '0'; Inc(Res);
        Res^ := '0'; Inc(Res);
        Res^ := HexDigits[Byte(P^) shr 4]; Inc(Res);
        Res^ := HexDigits[Byte(P^) and $F]; Inc(Res);
      end;
    end;
    Inc(P);
  end;

  Res^ := '"';
  Inc(Res);
  Len := PtrUInt(Res) - PtrUInt(PChar(Result));
  SetLength(Result, Len);
end;

class function TDeltaHydrator.ResultSetToJsonString(
  const ARS: IDeltaResultSet): RawByteString;
type
  TFieldCache = record
    DataType: TDeltaParamType;
    JSONKey: string;
  end;
var
  SB: TStringBuilder;
  I, FCnt: Integer;
  StrValue: string;
  InvFS: TFormatSettings;
  FieldsCache: array of TFieldCache;
begin
  if ARS = nil then
    Exit('[]');

  InvFS := DefaultFormatSettings;
  InvFS.DecimalSeparator := '.';
  InvFS.ThousandSeparator := #0;

  FCnt := ARS.FieldCount;
  SetLength(FieldsCache, FCnt);
  for I := 0 to FCnt - 1 do
  begin
    FieldsCache[I].DataType := ARS.FieldType(I);
    if I = 0 then
      FieldsCache[I].JSONKey := '"' + ARS.FieldName(I) + '":'
    else
      FieldsCache[I].JSONKey := ',"' + ARS.FieldName(I) + '":';
  end;

  SB := TStringBuilder.Create;
  try
    SB.Capacity := 64 * 1024;
    SB.Append('[');
    while ARS.Next do
    begin
      if SB.Length > 1 then
        SB.Append(',');
      SB.Append('{');

      for I := 0 to FCnt - 1 do
      begin
        SB.Append(FieldsCache[I].JSONKey);
        if ARS.IsNull(I) then
          SB.Append('null')
        else
        begin
          case FieldsCache[I].DataType of
            dptInteger, dptLargeInt:
              SB.Append(ARS.AsLargeInt(I));
            dptFloat:
              begin
                StrValue := FloatToStrF(ARS.AsFloat(I), ffFixed, 18, 4, InvFS);
                SB.Append(StrValue);
              end;
            dptBoolean:
              if ARS.AsBoolean(I) then
                SB.Append('true')
              else
                SB.Append('false');
          else
            SB.Append(FastJSONEscape(ARS.AsString(I)));
          end;
        end;
      end;

      SB.Append('}');
    end;
    SB.Append(']');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
