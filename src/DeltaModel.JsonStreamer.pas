{
    DeltaModel - High-Performance Direct JSON Streamer

    Streams TDeltaModel instances and collections directly into a pre-allocated
    RawByteString buffer without intermediate TJSONObject/TJSONArray DOM trees.
    Guarantees pure fixed-point decimal serialization without scientific notation
    and locale-independent '.' decimal separator.
}

unit DeltaModel.JsonStreamer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, Math,
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaModel.Metadata;

type
  { High-performance direct JSON serializer with fixed-point decimal formatting }
  TDeltaFastJsonStreamer = class
  private
    FBuffer: RawByteString;
    FLen: Integer;
    FCapacity: Integer;
    procedure EnsureCapacity(AAdditional: Integer);
    procedure AppendChar(C: AnsiChar); inline;
    procedure AppendRaw(const S: RawByteString); inline;
    procedure AppendString(const S: string);
    procedure AppendFloat(const AValue: Double; AMaxDecimals: Integer = 8);
    procedure AppendCurrency(const AValue: Currency); inline;
    procedure AppendInt64(const AValue: Int64); inline;
    procedure AppendBoolean(const AValue: Boolean); inline;
    procedure AppendNull; inline;
    procedure WriteFloatProperty(AObj: TObject; APropInfo: PPropInfo);
  public
    constructor Create(AInitialCapacity: Integer = 4096);
    procedure Clear;

    procedure WriteModel(AModel: TDeltaModel);
    procedure WriteList(AList: TCustomDeltaModelList);
    procedure WritePaginatedList(AList: TCustomDeltaModelList);
    procedure WriteObject(AObj: TObject);

    function AsJSON: RawByteString;

    { Static convenience helpers }
    class function ModelToJson(AModel: TDeltaModel): RawByteString;
    class function ListToJson(AList: TCustomDeltaModelList): RawByteString;
    class function PaginatedListToJson(AList: TCustomDeltaModelList): RawByteString;
    class function ObjectToJson(AObj: TObject): RawByteString;
    class function FormatFloatFixed(const AValue: Double; AMaxDecimals: Integer = 8): string;
    class function FormatCurrencyFixed(const AValue: Currency): string;
  end;

implementation

{ TDeltaFastJsonStreamer }

constructor TDeltaFastJsonStreamer.Create(AInitialCapacity: Integer = 4096);
begin
  inherited Create;
  if AInitialCapacity < 256 then
    AInitialCapacity := 256;
  FCapacity := AInitialCapacity;
  SetLength(FBuffer, FCapacity);
  FLen := 0;
end;

procedure TDeltaFastJsonStreamer.Clear;
begin
  FLen := 0;
end;

procedure TDeltaFastJsonStreamer.EnsureCapacity(AAdditional: Integer);
var
  NewCapacity: Integer;
begin
  if FLen + AAdditional > FCapacity then
  begin
    NewCapacity := FCapacity * 2;
    if NewCapacity < FLen + AAdditional then
      NewCapacity := (FLen + AAdditional) + 4096;
    FCapacity := NewCapacity;
    SetLength(FBuffer, FCapacity);
  end;
end;

procedure TDeltaFastJsonStreamer.AppendChar(C: AnsiChar);
begin
  if FLen + 1 > FCapacity then
    EnsureCapacity(1);
  Inc(FLen);
  FBuffer[FLen] := C;
end;

procedure TDeltaFastJsonStreamer.AppendRaw(const S: RawByteString);
var
  L: Integer;
begin
  L := Length(S);
  if L = 0 then Exit;
  if FLen + L > FCapacity then
    EnsureCapacity(L);
  Move(S[1], FBuffer[FLen + 1], L);
  Inc(FLen, L);
end;

procedure TDeltaFastJsonStreamer.AppendString(const S: string);
var
  I, L: Integer;
  C: Char;
begin
  L := Length(S);
  EnsureCapacity(L + 8);
  AppendChar('"');
  for I := 1 to L do
  begin
    C := S[I];
    case C of
      '"':  AppendRaw('\"');
      '\':  AppendRaw('\\');
      #8:   AppendRaw('\b');
      #9:   AppendRaw('\t');
      #10:  AppendRaw('\n');
      #12:  AppendRaw('\f');
      #13:  AppendRaw('\r');
      #0..#7, #11, #14..#31:
        AppendRaw('\u' + IntToHex(Ord(C), 4));
    else
      AppendChar(C);
    end;
  end;
  AppendChar('"');
end;

class function TDeltaFastJsonStreamer.FormatFloatFixed(const AValue: Double; AMaxDecimals: Integer = 8): string;
begin
  Result := DeltaFormatFloatFixed(AValue, AMaxDecimals);
end;

class function TDeltaFastJsonStreamer.FormatCurrencyFixed(const AValue: Currency): string;
begin
  Result := DeltaFormatCurrencyFixed(AValue);
end;

procedure TDeltaFastJsonStreamer.AppendFloat(const AValue: Double; AMaxDecimals: Integer = 8);
begin
  AppendRaw(DeltaFormatFloatFixed(AValue, AMaxDecimals));
end;

procedure TDeltaFastJsonStreamer.AppendCurrency(const AValue: Currency);
begin
  AppendRaw(DeltaFormatCurrencyFixed(AValue));
end;

procedure TDeltaFastJsonStreamer.AppendInt64(const AValue: Int64);
begin
  AppendRaw(IntToStr(AValue));
end;

procedure TDeltaFastJsonStreamer.AppendBoolean(const AValue: Boolean);
begin
  if AValue then
    AppendRaw('true')
  else
    AppendRaw('false');
end;

procedure TDeltaFastJsonStreamer.AppendNull;
begin
  AppendRaw('null');
end;

procedure TDeltaFastJsonStreamer.WriteFloatProperty(AObj: TObject; APropInfo: PPropInfo);
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(APropInfo^.PropType);
  if Assigned(TypeData) and (TypeData^.FloatType = ftCurr) then
    AppendCurrency(Currency(GetFloatProp(AObj, APropInfo)))
  else
    AppendFloat(GetFloatProp(AObj, APropInfo), 8);
end;

procedure TDeltaFastJsonStreamer.WriteModel(AModel: TDeltaModel);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, J: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
  DeltaField: TDeltaField;
  HasWrittenField: Boolean;
begin
  if AModel = nil then
  begin
    AppendNull;
    Exit;
  end;

  AppendChar('{');
  HasWrittenField := False;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  { 1. Managed schema fields (TDeltaField wrappers and primitive properties) }
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];

    if FieldMeta^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
      if (PropObj = nil) or not (PropObj is TDeltaField) then Continue;
      DeltaField := TDeltaField(PropObj);

      if not DeltaField.Visible then Continue;

      if HasWrittenField then AppendChar(',');
      HasWrittenField := True;

      AppendString(FieldMeta^.PropName);
      AppendChar(':');

      if DeltaField is TDFHasOne then
      begin
        if Assigned(TDFHasOne(DeltaField).Model) then
          WriteModel(TDeltaModel(TDFHasOne(DeltaField).Model))
        else
          AppendNull;
      end
      else if DeltaField is TDFHasMany then
      begin
        AppendChar('[');
        for J := 0 to TDFHasMany(DeltaField).Count - 1 do
        begin
          if J > 0 then AppendChar(',');
          if TDFHasMany(DeltaField)[J] is TDeltaModel then
            WriteModel(TDeltaModel(TDFHasMany(DeltaField)[J]))
          else
            AppendNull;
        end;
        AppendChar(']');
      end
      else if DeltaField.IsNull then
      begin
        AppendNull;
      end
      else if (DeltaField is TDFCurrencyNull) or (DeltaField is TDFCurrencyRequired) then
      begin
        AppendCurrency(DeltaField.AsCurrency);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            AppendInt64(DeltaField.AsLargeInt);

          dfkFloat:
            AppendFloat(DeltaField.AsFloat, 8);

          dfkBoolean:
            AppendBoolean(DeltaField.AsBoolean);

          dfkDateTime:
            AppendString(DeltaField.AsString);

          dfkString:
            AppendString(DeltaField.AsString);
        else
          AppendString(DeltaField.AsString);
        end;
      end;
    end
    else
    begin
      { Primitive property (Style B with bitmask nullability) }
      if HasWrittenField then AppendChar(',');
      HasWrittenField := True;

      AppendString(FieldMeta^.PropName);
      AppendChar(':');

      if AModel.IsNull(FieldMeta^.PropIndex) then
      begin
        AppendNull;
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            if FieldMeta^.PropInfo^.PropType^.Kind in [tkInt64, tkQWord] then
              AppendInt64(GetInt64Prop(AModel, FieldMeta^.PropInfo))
            else
              AppendInt64(GetOrdProp(AModel, FieldMeta^.PropInfo));
          dfkFloat:
            WriteFloatProperty(AModel, FieldMeta^.PropInfo);
          dfkBoolean:
            AppendBoolean(Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo)));
          dfkDateTime:
            AppendString(FormatDateTime('yyyy-mm-dd hh:nn:ss', GetFloatProp(AModel, FieldMeta^.PropInfo)));
          dfkString:
            AppendString(GetStrProp(AModel, FieldMeta^.PropInfo));
        else
          AppendString(GetStrProp(AModel, FieldMeta^.PropInfo));
        end;
      end;
    end;
  end;

  { 2. Additional published properties (not declared in Schema and non-TDeltaField) }
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    if HasWrittenField then AppendChar(',');
    HasWrittenField := True;

    AppendString(PropItem^.Name);
    AppendChar(':');

    if AModel.IsNull(PropItem^.Index) then
    begin
      AppendNull;
      Continue;
    end;

    case PropItem^.PropKind of
      tkInteger:
        AppendInt64(GetOrdProp(AModel, PropItem^.PropInfo));

      tkInt64, tkQWord:
        AppendInt64(GetInt64Prop(AModel, PropItem^.PropInfo));

      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
        AppendString(GetStrProp(AModel, PropItem^.PropInfo));

      tkBool:
        AppendBoolean(Boolean(GetOrdProp(AModel, PropItem^.PropInfo)));

      tkEnumeration:
        AppendString(GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AModel, PropItem^.PropInfo)));

      tkFloat:
        WriteFloatProperty(AModel, PropItem^.PropInfo);

      tkClass:
        begin
          PropObj := GetObjectProp(AModel, PropItem^.PropInfo);
          if PropObj = nil then
            AppendNull
          else if PropObj is TDeltaModel then
            WriteModel(TDeltaModel(PropObj))
          else if PropObj is TCustomDeltaModelList then
            WriteList(TCustomDeltaModelList(PropObj))
          else
            WriteObject(PropObj);
        end;
    else
      AppendNull;
    end;
  end;

  AppendChar('}');
end;

procedure TDeltaFastJsonStreamer.WriteObject(AObj: TObject);
var
  ClassProps: TRTTIClassInfo;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
  I: Integer;
  HasWrittenField: Boolean;
begin
  if AObj = nil then
  begin
    AppendNull;
    Exit;
  end;

  if AObj is TDeltaModel then
  begin
    WriteModel(TDeltaModel(AObj));
    Exit;
  end;

  if AObj is TCustomDeltaModelList then
  begin
    WriteList(TCustomDeltaModelList(AObj));
    Exit;
  end;

  AppendChar('{');
  HasWrittenField := False;
  ClassProps := TRTTICache.GetClassProps(AObj.ClassType);

  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if not PropItem^.HasGetProc then Continue;

    if HasWrittenField then AppendChar(',');
    HasWrittenField := True;

    AppendString(PropItem^.Name);
    AppendChar(':');

    case PropItem^.PropKind of
      tkInteger:
        AppendInt64(GetOrdProp(AObj, PropItem^.PropInfo));
      tkInt64, tkQWord:
        AppendInt64(GetInt64Prop(AObj, PropItem^.PropInfo));
      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
        AppendString(GetStrProp(AObj, PropItem^.PropInfo));
      tkBool:
        AppendBoolean(Boolean(GetOrdProp(AObj, PropItem^.PropInfo)));
      tkEnumeration:
        AppendString(GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AObj, PropItem^.PropInfo)));
      tkFloat:
        WriteFloatProperty(AObj, PropItem^.PropInfo);
      tkClass:
        begin
          PropObj := GetObjectProp(AObj, PropItem^.PropInfo);
          WriteObject(PropObj);
        end;
    else
      AppendNull;
    end;
  end;

  AppendChar('}');
end;

procedure TDeltaFastJsonStreamer.WriteList(AList: TCustomDeltaModelList);
var
  I: Integer;
begin
  if AList = nil then
  begin
    AppendNull;
    Exit;
  end;

  AppendChar('[');
  for I := 0 to AList.Count - 1 do
  begin
    if I > 0 then AppendChar(',');
    WriteModel(TDeltaModel(AList.GetItemObj(I)));
  end;
  AppendChar(']');
end;

procedure TDeltaFastJsonStreamer.WritePaginatedList(AList: TCustomDeltaModelList);
var
  I: Integer;
  CurLimit: Integer;
  CurPage: Integer;
  CurTotal: Int64;
  CalcTotalPages: Integer;
begin
  if AList = nil then
  begin
    AppendNull;
    Exit;
  end;

  CurLimit := AList.page_size;
  if CurLimit <= 0 then CurLimit := 20;
  CurPage := AList.page;
  if CurPage <= 0 then CurPage := 1;

  CurTotal := AList.total_records;
  if (CurTotal = 0) and (AList.Count > 0) then
    CurTotal := AList.Count;

  CalcTotalPages := (CurTotal + CurLimit - 1) div CurLimit;
  if CalcTotalPages < 1 then CalcTotalPages := 1;

  AList.page_size := CurLimit;
  AList.page := CurPage;
  AList.total_records := CurTotal;
  AList.total_pages := CalcTotalPages;

  AppendRaw('{"items":[');
  for I := 0 to AList.Count - 1 do
  begin
    if I > 0 then AppendChar(',');
    WriteModel(TDeltaModel(AList.GetItemObj(I)));
  end;
  AppendRaw('],"total_records":');
  AppendInt64(CurTotal);
  AppendRaw(',"page":');
  AppendInt64(CurPage);
  AppendRaw(',"page_size":');
  AppendInt64(CurLimit);
  AppendRaw(',"total_pages":');
  AppendInt64(CalcTotalPages);
  AppendChar('}');
end;

function TDeltaFastJsonStreamer.AsJSON: RawByteString;
begin
  Result := Copy(FBuffer, 1, FLen);
end;

class function TDeltaFastJsonStreamer.ModelToJson(AModel: TDeltaModel): RawByteString;
var
  Streamer: TDeltaFastJsonStreamer;
begin
  Streamer := TDeltaFastJsonStreamer.Create(1024);
  try
    Streamer.WriteModel(AModel);
    Result := Streamer.AsJSON;
  finally
    Streamer.Free;
  end;
end;

class function TDeltaFastJsonStreamer.ListToJson(AList: TCustomDeltaModelList): RawByteString;
var
  Streamer: TDeltaFastJsonStreamer;
begin
  Streamer := TDeltaFastJsonStreamer.Create(4096);
  try
    Streamer.WriteList(AList);
    Result := Streamer.AsJSON;
  finally
    Streamer.Free;
  end;
end;

class function TDeltaFastJsonStreamer.PaginatedListToJson(AList: TCustomDeltaModelList): RawByteString;
var
  Streamer: TDeltaFastJsonStreamer;
begin
  Streamer := TDeltaFastJsonStreamer.Create(4096);
  try
    Streamer.WritePaginatedList(AList);
    Result := Streamer.AsJSON;
  finally
    Streamer.Free;
  end;
end;

class function TDeltaFastJsonStreamer.ObjectToJson(AObj: TObject): RawByteString;
var
  Streamer: TDeltaFastJsonStreamer;
begin
  Streamer := TDeltaFastJsonStreamer.Create(1024);
  try
    Streamer.WriteObject(AObj);
    Result := Streamer.AsJSON;
  finally
    Streamer.Free;
  end;
end;

end.
