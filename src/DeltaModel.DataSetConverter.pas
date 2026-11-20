unit DeltaModel.DataSetConverter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, DeltaModel, DeltaModel.Fields,
  DeltaModel.Metadata, DB, SQLDB;

type
  { Pre-allocated binding entry between a TDataSet column and a TDeltaModel field }
  PColumnBindingItem = ^TColumnBindingItem;
  TColumnBindingItem = record
    Field: TField;
    PropInfo: PPropInfo;
    FieldKind: TDeltaFieldKind;
    IsDeltaField: Boolean;
    RegularPropKind: TTypeKind;
    PropIndex: Integer;
  end;
  TColumnBindingItemArray = array of TColumnBindingItem;

  { Pre-allocated column mapping eliminating DS.FindField and Variant boxing inside loops }
  TColumnBindingMap = class
  private
    FBindings: TColumnBindingItemArray;
    FBindingCount: Integer;
    FModelClass: TClass;
    procedure BuildBindings(ADS: TDataSet);
  public
    constructor Create(AModelClass: TClass; ADS: TDataSet);
    procedure BindRecord(AModel: TDeltaModel);
    property Bindings: TColumnBindingItemArray read FBindings;
    property BindingCount: Integer read FBindingCount;
    property ModelClass: TClass read FModelClass;
  end;

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet); overload;
procedure FromDataSet(AModel: TDeltaModel; ABindingMap: TColumnBindingMap); overload;
procedure ToDatasetParams(AModel: TDeltaModel; DS: TSQLQuery);
procedure ToDatasetParamsIndexed(AModel: TDeltaModel; DS: TSQLQuery; AIndex: Integer);

implementation

{ TColumnBindingMap }

constructor TColumnBindingMap.Create(AModelClass: TClass; ADS: TDataSet);
begin
  inherited Create;
  FModelClass := AModelClass;
  BuildBindings(ADS);
end;

procedure TColumnBindingMap.BuildBindings(ADS: TDataSet);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I: Integer;
  F: TField;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  Item: TColumnBindingItem;
begin
  FBindingCount := 0;
  SetLength(FBindings, 0);
  if (ADS = nil) or (FModelClass = nil) then Exit;

  Meta := TRTTICache.GetModelMetadata(FModelClass);
  ClassProps := TRTTICache.GetClassProps(FModelClass);

  // 1. Bind fields via TModelMetadata (TDeltaField and Schema primitive properties)
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsVirtual then Continue;

    F := ADS.FindField(FieldMeta^.FieldName);
    if F = nil then
      F := ADS.FindField(FieldMeta^.PropName);

    if F <> nil then
    begin
      Item.Field := F;
      Item.PropInfo := FieldMeta^.PropInfo;
      Item.FieldKind := FieldMeta^.FieldKind;
      Item.IsDeltaField := FieldMeta^.IsDeltaField;
      Item.PropIndex := FieldMeta^.PropIndex;
      if not FieldMeta^.IsDeltaField then
        Item.RegularPropKind := FieldMeta^.PropInfo^.PropType^.Kind
      else
        Item.RegularPropKind := tkUnknown;

      SetLength(FBindings, FBindingCount + 1);
      FBindings[FBindingCount] := Item;
      Inc(FBindingCount);
    end;
  end;

  // 2. Bind regular published properties not declared in Schema
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasSetProc) then Continue;

    // Skip if already bound by Schema
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    F := ADS.FindField(PropItem^.Name);
    if F <> nil then
    begin
      Item.Field := F;
      Item.PropInfo := PropItem^.PropInfo;
      Item.FieldKind := dfkString;
      Item.IsDeltaField := False;
      Item.RegularPropKind := PropItem^.PropKind;
      Item.PropIndex := PropItem^.Index;

      SetLength(FBindings, FBindingCount + 1);
      FBindings[FBindingCount] := Item;
      Inc(FBindingCount);
    end;
  end;
end;

procedure TColumnBindingMap.BindRecord(AModel: TDeltaModel);
var
  I: Integer;
  Item: PColumnBindingItem;
  DSField: TField;
  DeltaField: TDeltaField;
  PropObj: TObject;
begin
  if AModel = nil then Exit;
  AModel.ClearAllDirty;

  for I := 0 to FBindingCount - 1 do
  begin
    Item := @FBindings[I];
    DSField := Item^.Field;

    if Item^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, Item^.PropInfo);
      if (PropObj = nil) or not (PropObj is TDeltaField) then Continue;
      DeltaField := TDeltaField(PropObj);

      if DSField.IsNull then
      begin
        DeltaField.Clear;
        Continue;
      end;

      case Item^.FieldKind of
        dfkInteger:
          DeltaField.AsLargeInt := DSField.AsLargeInt;

        dfkFloat:
          DeltaField.AsFloat := DSField.AsFloat;

        dfkString:
          DeltaField.SetAsString(DSField.AsString);

        dfkBoolean:
          DeltaField.AsBoolean := DSField.AsBoolean;

        dfkDateTime:
          DeltaField.AsDateTime := DSField.AsDateTime;
      else
        DeltaField.Value := DSField.Value;
      end;
    end
    else
    begin
      if DSField.IsNull then
      begin
        AModel.SetNull(Item^.PropIndex);
        Continue;
      end;

      AModel.ClearNull(Item^.PropIndex);
      try
        case Item^.RegularPropKind of
          tkInteger, tkInt64:
            SetOrdProp(AModel, Item^.PropInfo, DSField.AsLargeInt);

          tkString, tkLString, tkAString, tkWString, tkUString:
            SetStrProp(AModel, Item^.PropInfo, DSField.AsString);

          tkFloat:
            SetFloatProp(AModel, Item^.PropInfo, DSField.AsFloat);

          tkBool, tkEnumeration:
            SetOrdProp(AModel, Item^.PropInfo, Ord(DSField.AsBoolean));
        else
          SetPropValue(AModel, Item^.PropInfo^.Name, DSField.Value);
        end;
      except
        // Safely ignore incompatible assignments
      end;
    end;
  end;
end;

{ Helper procedures }

procedure FromDataSet(AModel: TDeltaModel; DS: TDataSet);
var
  BindingMap: TColumnBindingMap;
begin
  if (AModel = nil) or (DS = nil) or DS.IsEmpty then Exit;

  BindingMap := TColumnBindingMap.Create(AModel.ClassType, DS);
  try
    BindingMap.BindRecord(AModel);
  finally
    BindingMap.Free;
  end;
end;

procedure FromDataSet(AModel: TDeltaModel; ABindingMap: TColumnBindingMap);
begin
  if (AModel = nil) or (ABindingMap = nil) then Exit;
  ABindingMap.BindRecord(AModel);
end;

procedure ToDatasetParams(AModel: TDeltaModel; DS: TSQLQuery);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  Param: TParam;
  PropObj: TObject;
  DeltaField: TDeltaField;
begin
  if (AModel = nil) or (DS = nil) then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Fields mapped via TModelMetadata (TDeltaField and primitives)
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsVirtual or FieldMeta^.FieldName.IsEmpty then Continue;

    Param := DS.Params.FindParam(FieldMeta^.FieldName);
    if Param = nil then
      Param := DS.Params.FindParam(FieldMeta^.PropName);

    if Param <> nil then
    begin
      if FieldMeta^.IsDeltaField then
      begin
        PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
        if PropObj is TDeltaField then
        begin
          DeltaField := TDeltaField(PropObj);
          if DeltaField.IsNull then
            Param.Clear
          else
          if (DeltaField is TDFTextNull) or (DeltaField is TDFTextRequired) then
          begin
            Param.DataType := ftMemo;
            Param.AsMemo := DeltaField.AsString;
          end
          else
          begin
            case FieldMeta^.FieldKind of
              dfkInteger:
                Param.AsLargeInt := DeltaField.AsLargeInt;
              dfkFloat:
                Param.AsFloat := DeltaField.AsFloat;
              dfkString:
                Param.AsString := DeltaField.AsString;
              dfkBoolean:
                Param.AsBoolean := DeltaField.AsBoolean;
              dfkDateTime:
                Param.AsDateTime := DeltaField.AsDateTime;
            else
              Param.Value := DeltaField.Value;
            end;
          end;
        end;
      end
      else
      begin
        // Primitive property
        if AModel.IsNull(FieldMeta^.PropIndex) then
          Param.Clear
        else
        begin
          case FieldMeta^.FieldKind of
            dfkInteger:
              Param.AsLargeInt := GetOrdProp(AModel, FieldMeta^.PropInfo);
            dfkFloat:
              Param.AsFloat := GetFloatProp(AModel, FieldMeta^.PropInfo);
            dfkBoolean:
              Param.AsBoolean := Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo));
            dfkDateTime:
              Param.AsDateTime := GetFloatProp(AModel, FieldMeta^.PropInfo);
            dfkString:
              Param.AsString := GetStrProp(AModel, FieldMeta^.PropInfo);
          else
            Param.Value := GetPropValue(AModel, FieldMeta^.PropName);
          end;
        end;
      end;
    end;
  end;

  // 2. Regular published properties (not declared in Schema)
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    Param := DS.Params.FindParam(PropItem^.Name);
    if Param <> nil then
    begin
      try
        case PropItem^.PropKind of
          tkInteger, tkInt64:
            Param.AsLargeInt := GetOrdProp(AModel, PropItem^.PropInfo);
          tkString, tkLString, tkAString, tkWString, tkUString:
            Param.AsString := GetStrProp(AModel, PropItem^.PropInfo);
          tkFloat:
            Param.AsFloat := GetFloatProp(AModel, PropItem^.PropInfo);
          tkBool, tkEnumeration:
            Param.AsBoolean := Boolean(GetOrdProp(AModel, PropItem^.PropInfo));
        else
          Param.Value := GetPropValue(AModel, PropItem^.Name);
        end;
      except
        // Ignore non-mappable properties
      end;
    end;
  end;
end;

procedure ToDatasetParamsIndexed(AModel: TDeltaModel; DS: TSQLQuery; AIndex: Integer);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  Param: TParam;
  PropObj: TObject;
  DeltaField: TDeltaField;
  ParamName: string;
begin
  if (AModel = nil) or (DS = nil) then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Fields mapped via TModelMetadata (TDeltaField and primitives)
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsVirtual or FieldMeta^.FieldName.IsEmpty then Continue;

    ParamName := FieldMeta^.FieldName + '_' + IntToStr(AIndex);
    Param := DS.Params.FindParam(ParamName);
    if Param = nil then
    begin
      ParamName := FieldMeta^.PropName + '_' + IntToStr(AIndex);
      Param := DS.Params.FindParam(ParamName);
    end;

    if Param <> nil then
    begin
      if FieldMeta^.IsDeltaField then
      begin
        PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
        if PropObj is TDeltaField then
        begin
          DeltaField := TDeltaField(PropObj);
          if DeltaField.IsNull then
            Param.Clear
          else
          if (DeltaField is TDFTextNull) or (DeltaField is TDFTextRequired) then
          begin
            Param.DataType := ftMemo;
            Param.AsMemo := DeltaField.AsString;
          end
          else
          begin
            case FieldMeta^.FieldKind of
              dfkInteger:
                Param.AsLargeInt := DeltaField.AsLargeInt;
              dfkFloat:
                Param.AsFloat := DeltaField.AsFloat;
              dfkString:
                Param.AsString := DeltaField.AsString;
              dfkBoolean:
                Param.AsBoolean := DeltaField.AsBoolean;
              dfkDateTime:
                Param.AsDateTime := DeltaField.AsDateTime;
            else
              Param.Value := DeltaField.Value;
            end;
          end;
        end;
      end
      else
      begin
        // Primitive property
        if AModel.IsNull(FieldMeta^.PropIndex) then
          Param.Clear
        else
        begin
          case FieldMeta^.FieldKind of
            dfkInteger:
              Param.AsLargeInt := GetOrdProp(AModel, FieldMeta^.PropInfo);
            dfkFloat:
              Param.AsFloat := GetFloatProp(AModel, FieldMeta^.PropInfo);
            dfkBoolean:
              Param.AsBoolean := Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo));
            dfkDateTime:
              Param.AsDateTime := GetFloatProp(AModel, FieldMeta^.PropInfo);
            dfkString:
              Param.AsString := GetStrProp(AModel, FieldMeta^.PropInfo);
          else
            Param.Value := GetPropValue(AModel, FieldMeta^.PropName);
          end;
        end;
      end;
    end;
  end;

  // 2. Regular published properties
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    ParamName := PropItem^.Name + '_' + IntToStr(AIndex);
    Param := DS.Params.FindParam(ParamName);
    if Param <> nil then
    begin
      try
        case PropItem^.PropKind of
          tkInteger, tkInt64:
            Param.AsLargeInt := GetOrdProp(AModel, PropItem^.PropInfo);
          tkString, tkLString, tkAString, tkWString, tkUString:
            Param.AsString := GetStrProp(AModel, PropItem^.PropInfo);
          tkFloat:
            Param.AsFloat := GetFloatProp(AModel, PropItem^.PropInfo);
          tkBool, tkEnumeration:
            Param.AsBoolean := Boolean(GetOrdProp(AModel, PropItem^.PropInfo));
        else
          Param.Value := GetPropValue(AModel, PropItem^.Name);
        end;
      except
        // Ignore non-mappable properties
      end;
    end;
  end;
end;

end.
