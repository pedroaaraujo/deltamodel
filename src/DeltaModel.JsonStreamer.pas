unit DeltaModel.JsonStreamer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants, Math,
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaModel.Metadata;

type
  { TDeltaFastJsonStreamer: Serializador JSON de alta performance direto para RawByteString,
    eliminando nós DOM intermediários e formatando números decimais em ponto fixo
    sem notação científica (ffFixed). }
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
    procedure AppendInt64(const AValue: Int64); inline;
    procedure AppendBoolean(const AValue: Boolean); inline;
    procedure AppendNull; inline;
  public
    constructor Create(AInitialCapacity: Integer = 4096);
    procedure Clear;

    procedure WriteModel(AModel: TDeltaModel);
    procedure WriteList(AList: TCustomDeltaModelList);
    procedure WritePaginatedList(AList: TCustomDeltaModelList);

    function AsJSON: RawByteString;

    // Métodos utilitários e estáticos de conveniência
    class function ModelToJson(AModel: TDeltaModel): RawByteString;
    class function ListToJson(AList: TCustomDeltaModelList): RawByteString;
    class function PaginatedListToJson(AList: TCustomDeltaModelList): RawByteString;
    class function FormatFloatFixed(const AValue: Double; AMaxDecimals: Integer = 8): string;
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
var
  FS: TFormatSettings;
  S: string;
  I: Integer;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit('0');

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  if AMaxDecimals < 0 then AMaxDecimals := 2;
  if AMaxDecimals > 18 then AMaxDecimals := 18;

  // ffFixed NUNCA gera notação científica (E+ ou E-)
  S := FloatToStrF(AValue, ffFixed, 18, AMaxDecimals, FS);

  // Remove zeros supérfluos à direita após o separador decimal
  if Pos('.', S) > 0 then
  begin
    I := Length(S);
    while (I > 0) and (S[I] = '0') do
      Dec(I);
    if (I > 0) and (S[I] = '.') then
      Dec(I);
    S := Copy(S, 1, I);
  end;

  if (S = '') or (S = '-0') then
    S := '0';

  Result := S;
end;

procedure TDeltaFastJsonStreamer.AppendFloat(const AValue: Double; AMaxDecimals: Integer = 8);
begin
  AppendRaw(FormatFloatFixed(AValue, AMaxDecimals));
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

procedure TDeltaFastJsonStreamer.WriteModel(AModel: TDeltaModel);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I: Integer;
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

  // 1. Campos gerenciados TDeltaField
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
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
    else if DeltaField.IsNull then
    begin
      AppendNull;
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
  end;

  // 2. Propriedades publicadas normais (não-TDeltaField)
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;

    if HasWrittenField then AppendChar(',');
    HasWrittenField := True;

    AppendString(PropItem^.Name);
    AppendChar(':');

    case PropItem^.PropKind of
      tkInteger, tkInt64:
        AppendInt64(GetOrdProp(AModel, PropItem^.PropInfo));

      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
        AppendString(GetStrProp(AModel, PropItem^.PropInfo));

      tkBool:
        AppendBoolean(Boolean(GetOrdProp(AModel, PropItem^.PropInfo)));

      tkEnumeration:
        AppendString(GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AModel, PropItem^.PropInfo)));

      tkFloat:
        AppendFloat(GetFloatProp(AModel, PropItem^.PropInfo), 8);
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

end.
