unit DeltaModel.Codec.Csv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  Variants,
  Math,
  DateUtils,
  DeltaModel,
  DeltaModel.Fields,
  DeltaModel.List,
  DeltaModel.Metadata,
  DeltaModel.Codec;

type
  { TDeltaCsvCodec: High-Performance RFC 4180 CSV Codec (Zero-DOM Streamer)
    RFC 4180 Specification:
    - First line: header with comma-separated column names.
    - Data lines with comma-separated fields.
    - Fields containing commas, double quotes, or line breaks are enclosed in double quotes ("...").
    - Embedded double quotes are escaped by doubling them ("").
    - Decimal point '.' for floating-point numbers without scientific notation.
    - Dates formatted in canonical ISO-8601. }
  TDeltaCsvCodec = class(TInterfacedObject, IDeltaCodec)
  private
    function CsvEscape(const S: string): string;
    function FloatToCsv(const V: Double): string;
    function CsvToFloatDef(const S: string; ADefault: Double): Double;
    function ParseCsvRow(P: PChar; out NextP: PChar; out Fields: TStringArray): Boolean;
    function GetFieldStringValue(AModel: TDeltaModel; FieldMeta: PDeltaFieldMeta): string;
    procedure SetFieldStringValue(AModel: TDeltaModel; FieldMeta: PDeltaFieldMeta; const AVal: string);
  public
    function GetFormatName: string;
    function GetMimeType: string;
    function CanHandleMimeType(const AMimeType: string): Boolean;
    procedure EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
    procedure EncodeList(AList: TObject; AOutputStream: TStream);
    procedure DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
    procedure DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
  end;

implementation

{ Utility Functions }

function StreamToRawString(AStream: TStream): string;
var
  Raw: RawByteString;
  Len: Int64;
begin
  Result := '';
  if AStream = nil then Exit;
  Len := AStream.Size - AStream.Position;
  if Len <= 0 then Exit;
  SetLength(Raw, Len);
  AStream.ReadBuffer(Raw[1], Len);
  Result := UTF8ToString(Raw);
end;

procedure StringToStream(const S: string; AStream: TStream);
var
  Raw: RawByteString;
begin
  if (AStream = nil) or (Length(S) = 0) then Exit;
  Raw := UTF8Encode(S);
  AStream.WriteBuffer(Raw[1], Length(Raw));
end;

{ TDeltaCsvCodec }

function TDeltaCsvCodec.GetFormatName: string;
begin
  Result := 'csv';
end;

function TDeltaCsvCodec.GetMimeType: string;
begin
  Result := 'text/csv';
end;

function TDeltaCsvCodec.CanHandleMimeType(const AMimeType: string): Boolean;
var
  M: string;
  P: Integer;
begin
  M := LowerCase(Trim(AMimeType));
  P := Pos(';', M);
  if P > 0 then
    M := Trim(Copy(M, 1, P - 1));

  Result := (M = 'text/csv') or
            (M = 'application/csv') or
            (M = 'csv');
end;

function TDeltaCsvCodec.CsvEscape(const S: string): string;
var
  NeedsQuotes: Boolean;
  I: Integer;
begin
  NeedsQuotes := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in [',', '"', #10, #13] then
    begin
      NeedsQuotes := True;
      Break;
    end;
  end;

  if not NeedsQuotes then
    Exit(S);

  Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
end;

function TDeltaCsvCodec.FloatToCsv(const V: Double): string;
var
  FS: TFormatSettings;
  I: Integer;
begin
  if IsNan(V) or IsInfinite(V) then
    Exit('0');

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := FloatToStrF(V, ffFixed, 18, 8, FS);

  if Pos('.', Result) > 0 then
  begin
    I := Length(Result);
    while (I > 0) and (Result[I] = '0') do
      Dec(I);
    if (I > 0) and (Result[I] = '.') then
      Dec(I);
    Result := Copy(Result, 1, I);
  end;

  if (Result = '') or (Result = '-0') then
    Result := '0';
end;

function TDeltaCsvCodec.CsvToFloatDef(const S: string; ADefault: Double): Double;
var
  FS: TFormatSettings;
  CleanStr: string;
begin
  CleanStr := Trim(S);
  if CleanStr = '' then Exit(ADefault);

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  if not TryStrToFloat(StringReplace(CleanStr, ',', '.', [rfReplaceAll]), Result, FS) then
    Result := ADefault;
end;

function TDeltaCsvCodec.ParseCsvRow(P: PChar; out NextP: PChar; out Fields: TStringArray): Boolean;
var
  CurField: string;
  InQuotes: Boolean;
  Len: Integer;
begin
  Fields := nil;
  Len := 0;
  NextP := P;
  if (P = nil) or (P^ = #0) then Exit(False);

  SetLength(Fields, 8);
  CurField := '';
  InQuotes := False;

  while P^ <> #0 do
  begin
    if InQuotes then
    begin
      if P^ = '"' then
      begin
        if (P + 1)^ = '"' then
        begin
          CurField := CurField + '"';
          Inc(P, 2);
        end
        else
        begin
          InQuotes := False;
          Inc(P);
        end;
      end
      else
      begin
        CurField := CurField + P^;
        Inc(P);
      end;
    end
    else
    begin
      if P^ = '"' then
      begin
        InQuotes := True;
        Inc(P);
      end
      else if P^ = ',' then
      begin
        if Len >= Length(Fields) then
          SetLength(Fields, Length(Fields) * 2);
        Fields[Len] := CurField;
        Inc(Len);
        CurField := '';
        Inc(P);
      end
      else if P^ in [#10, #13] then
      begin
        if (P^ = #13) and ((P + 1)^ = #10) then
          Inc(P, 2)
        else
          Inc(P);
        Break;
      end
      else
      begin
        CurField := CurField + P^;
        Inc(P);
      end;
    end;
  end;

  if Len >= Length(Fields) then
    SetLength(Fields, Len + 1);
  Fields[Len] := CurField;
  Inc(Len);
  SetLength(Fields, Len);
  NextP := P;
  Result := True;
end;

function TDeltaCsvCodec.GetFieldStringValue(AModel: TDeltaModel; FieldMeta: PDeltaFieldMeta): string;
var
  PropObj: TObject;
  DeltaField: TDeltaField;
begin
  Result := '';
  if (AModel = nil) or (FieldMeta = nil) then Exit;

  if FieldMeta^.IsDeltaField then
  begin
    PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
    if (PropObj = nil) or not (PropObj is TDeltaField) then Exit;
    DeltaField := TDeltaField(PropObj);
    if not DeltaField.Visible or DeltaField.IsNull then Exit;

    case FieldMeta^.FieldKind of
      dfkInteger:
        Result := IntToStr(DeltaField.AsLargeInt);
      dfkFloat:
        Result := FloatToCsv(DeltaField.AsFloat);
      dfkBoolean:
        if DeltaField.AsBoolean then Result := 'true' else Result := 'false';
      dfkDateTime:
        Result := DeltaField.AsString;
      dfkString:
        Result := DeltaField.AsString;
    else
      Result := DeltaField.AsString;
    end;
  end
  else
  begin
    if AModel.IsNull(FieldMeta^.PropIndex) then Exit;

    case FieldMeta^.FieldKind of
      dfkInteger:
        Result := IntToStr(GetOrdProp(AModel, FieldMeta^.PropInfo));
      dfkFloat:
        Result := FloatToCsv(GetFloatProp(AModel, FieldMeta^.PropInfo));
      dfkBoolean:
        if Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo)) then Result := 'true' else Result := 'false';
      dfkDateTime:
        Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', GetFloatProp(AModel, FieldMeta^.PropInfo));
      dfkString:
        Result := GetStrProp(AModel, FieldMeta^.PropInfo);
    end;
  end;
end;

procedure TDeltaCsvCodec.SetFieldStringValue(AModel: TDeltaModel; FieldMeta: PDeltaFieldMeta; const AVal: string);
var
  PropObj: TObject;
  DeltaField: TDeltaField;
  CleanVal: string;
begin
  if (AModel = nil) or (FieldMeta = nil) then Exit;
  CleanVal := Trim(AVal);

  if FieldMeta^.IsDeltaField then
  begin
    PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
    if (PropObj = nil) or not (PropObj is TDeltaField) then Exit;
    DeltaField := TDeltaField(PropObj);

    if AVal = '' then
    begin
      if DeltaField is TDeltaFieldNullable then
        DeltaField.Clear
      else
        DeltaField.SetAsString('');
    end
    else
    begin
      case FieldMeta^.FieldKind of
        dfkInteger:
          DeltaField.AsInt64 := StrToInt64Def(CleanVal, 0);
        dfkFloat:
          DeltaField.AsFloat := CsvToFloatDef(CleanVal, 0.0);
        dfkBoolean:
          DeltaField.AsBoolean := SameText(CleanVal, 'true') or (CleanVal = '1');
        dfkDateTime:
          DeltaField.SetAsString(CleanVal);
        dfkString:
          DeltaField.SetAsString(AVal);
      else
        DeltaField.SetAsString(AVal);
      end;
    end;
  end
  else
  begin
    if AVal = '' then
      AModel.SetNull(FieldMeta^.PropIndex)
    else
    begin
      case FieldMeta^.FieldKind of
        dfkInteger:
          SetOrdProp(AModel, FieldMeta^.PropInfo, StrToInt64Def(CleanVal, 0));
        dfkFloat:
          SetFloatProp(AModel, FieldMeta^.PropInfo, CsvToFloatDef(CleanVal, 0.0));
        dfkBoolean:
          SetOrdProp(AModel, FieldMeta^.PropInfo, Ord(SameText(CleanVal, 'true') or (CleanVal = '1')));
        dfkDateTime:
          SetFloatProp(AModel, FieldMeta^.PropInfo, ISO8601ToDateDef(CleanVal, 0.0));
        dfkString:
          SetStrProp(AModel, FieldMeta^.PropInfo, AVal);
      end;
      AModel.ClearNull(FieldMeta^.PropIndex);
    end;
  end;
end;

procedure TDeltaCsvCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
var
  Meta: TModelMetadata;
  I: Integer;
  FieldMeta: PDeltaFieldMeta;
  HeaderLine, DataLine, ValStr: string;
begin
  if (AModel = nil) or (AOutputStream = nil) then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);

  HeaderLine := '';
  DataLine := '';

  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if I > 0 then
    begin
      HeaderLine := HeaderLine + ',';
      DataLine := DataLine + ',';
    end;

    HeaderLine := HeaderLine + CsvEscape(FieldMeta^.PropName);
    ValStr := GetFieldStringValue(AModel, FieldMeta);
    DataLine := DataLine + CsvEscape(ValStr);
  end;

  StringToStream(HeaderLine + sLineBreak + DataLine + sLineBreak, AOutputStream);
end;

procedure TDeltaCsvCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  CustomList: TCustomDeltaModelList;
  ItemObj: TObject;
  Meta: TModelMetadata;
  I, J, Count: Integer;
  FieldMeta: PDeltaFieldMeta;
  HeaderLine, DataLine, ValStr: string;
begin
  if (AList = nil) or (AOutputStream = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  Count := CustomList.Count;
  if Count = 0 then
  begin
    if CustomList.GetModelClass <> nil then
    begin
      Meta := TRTTICache.GetModelMetadata(CustomList.GetModelClass);
      HeaderLine := '';
      for I := 0 to Meta.FieldCount - 1 do
      begin
        if I > 0 then HeaderLine := HeaderLine + ',';
        HeaderLine := HeaderLine + CsvEscape(Meta.Fields[I].PropName);
      end;
      StringToStream(HeaderLine + sLineBreak, AOutputStream);
    end;
    Exit;
  end;

  // Use the first item to generate the header row
  ItemObj := CustomList.GetItemObj(0);
  if not (ItemObj is TDeltaModel) then Exit;
  Meta := TRTTICache.GetModelMetadata(ItemObj.ClassType);

  HeaderLine := '';
  for I := 0 to Meta.FieldCount - 1 do
  begin
    if I > 0 then HeaderLine := HeaderLine + ',';
    HeaderLine := HeaderLine + CsvEscape(Meta.Fields[I].PropName);
  end;
  StringToStream(HeaderLine + sLineBreak, AOutputStream);

  // Emit data rows
  for J := 0 to Count - 1 do
  begin
    ItemObj := CustomList.GetItemObj(J);
    if not (ItemObj is TDeltaModel) then Continue;

    DataLine := '';
    for I := 0 to Meta.FieldCount - 1 do
    begin
      FieldMeta := @Meta.Fields[I];
      if I > 0 then DataLine := DataLine + ',';
      ValStr := GetFieldStringValue(TDeltaModel(ItemObj), FieldMeta);
      DataLine := DataLine + CsvEscape(ValStr);
    end;
    StringToStream(DataLine + sLineBreak, AOutputStream);
  end;
end;

procedure TDeltaCsvCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
var
  CsvContent: string;
  P, NextP: PChar;
  HeaderRow, DataRow: TStringArray;
  Meta: TModelMetadata;
  FMeta: PDeltaFieldMeta;
  I, Limit: Integer;
begin
  if (AInputStream = nil) or (AModel = nil) then Exit;
  CsvContent := StreamToRawString(AInputStream);
  if Trim(CsvContent) = '' then Exit;

  P := PChar(CsvContent);
  if not ParseCsvRow(P, NextP, HeaderRow) then Exit;
  P := NextP;

  if not ParseCsvRow(P, NextP, DataRow) then Exit;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  Limit := Length(HeaderRow);
  if Length(DataRow) < Limit then
    Limit := Length(DataRow);

  for I := 0 to Limit - 1 do
  begin
    FMeta := Meta.FindFieldByProp(HeaderRow[I]);
    if FMeta = nil then
      FMeta := Meta.FindField(HeaderRow[I]);

    if FMeta <> nil then
      SetFieldStringValue(AModel, FMeta, DataRow[I]);
  end;
end;

procedure TDeltaCsvCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
var
  CsvContent: string;
  P, NextP: PChar;
  HeaderRow, DataRow: TStringArray;
  Meta: TModelMetadata;
  FMeta: PDeltaFieldMeta;
  CustomList: TCustomDeltaModelList;
  NewObj: TObject;
  I, Limit: Integer;
  TargetClass: TClass;
begin
  if (AInputStream = nil) or (AList = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  if (CustomList.GetModelClass = nil) and (AModelClass <> nil) and
     (AList is TDeltaModelList) and AModelClass.InheritsFrom(TDeltaModel) then
    TDeltaModelList(AList).DeltaModelClass := TDeltaModelClass(AModelClass);

  CsvContent := StreamToRawString(AInputStream);
  if Trim(CsvContent) = '' then Exit;

  P := PChar(CsvContent);
  if not ParseCsvRow(P, NextP, HeaderRow) then Exit;
  P := NextP;

  TargetClass := AModelClass;
  if TargetClass = nil then
    TargetClass := CustomList.GetModelClass;

  if (TargetClass = nil) or not TargetClass.InheritsFrom(TDeltaModel) then Exit;
  Meta := TRTTICache.GetModelMetadata(TargetClass);

  while (P <> nil) and (P^ <> #0) do
  begin
    if not ParseCsvRow(P, NextP, DataRow) then Break;
    P := NextP;

    // Blank line at end of file
    if (Length(DataRow) = 0) or ((Length(DataRow) = 1) and (DataRow[0] = '')) then
      Continue;

    NewObj := nil;
    if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
      NewObj := TDeltaModelClass(AModelClass).Create
    else
      NewObj := CustomList.NewItem;

    if NewObj is TDeltaModel then
    begin
      Limit := Length(HeaderRow);
      if Length(DataRow) < Limit then
        Limit := Length(DataRow);

      for I := 0 to Limit - 1 do
      begin
        FMeta := Meta.FindFieldByProp(HeaderRow[I]);
        if FMeta = nil then
          FMeta := Meta.FindField(HeaderRow[I]);

        if FMeta <> nil then
          SetFieldStringValue(TDeltaModel(NewObj), FMeta, DataRow[I]);
      end;

      CustomList.AddObj(NewObj);
    end
    else
      NewObj.Free;
  end;
end;

initialization
  TDeltaCodecRegistry.RegisterCodec(TDeltaCsvCodec.Create);

end.
