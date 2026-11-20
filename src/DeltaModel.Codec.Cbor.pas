unit DeltaModel.Codec.Cbor;

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

const
  { RFC 8949 Section 10.1 Security Guards (Anti-OOM / DoS) }
  MAX_CBOR_NESTING_DEPTH = 64;
  MAX_CBOR_STRING_LENGTH = 16 * 1024 * 1024; // 16 MB

type
  { TDeltaCborCodec: High-Performance Binary CBOR (RFC 8949) Codec (Zero-DOM)
    RFC 8949 Specification:
    - Major 0: Unsigned Integer (0..23 inline, 24..27 uint8/16/32/64 big-endian)
    - Major 1: Negative Integer (-1 - N)
    - Major 2: Byte String
    - Major 3: UTF-8 Text String
    - Major 4: Array of items
    - Major 5: Map of key/value pairs
    - Major 6: Semantic tag (e.g., tag 0 = RFC 3339 text string)
    - Major 7: Simple values & Floats (0xF4 false, 0xF5 true, 0xF6 null, 0xFB float64 BE) }
  TDeltaCborCodec = class(TInterfacedObject, IDeltaCodec)
  private
    procedure WriteHeader(AStream: TStream; AMajor: Byte; ALength: UInt64);
    procedure WriteInt64(AStream: TStream; AValue: Int64);
    procedure WriteDouble(AStream: TStream; AValue: Double);
    procedure WriteBoolean(AStream: TStream; AValue: Boolean);
    procedure WriteNull(AStream: TStream);
    procedure WriteTextString(AStream: TStream; const S: string);

    function ReadHeader(AStream: TStream; out AMajor: Byte; out AVal: UInt64; out AAddInfo: Byte; out AIsIndefinite: Boolean): Boolean;
    function ReadTextString(AStream: TStream; ALength: UInt64): string;
    procedure SkipCborValue(AStream: TStream; ADepth: Integer = 0);

    function CountModelPairs(AModel: TDeltaModel): Integer;
    procedure WriteModelMap(AModel: TDeltaModel; AStream: TStream);
    procedure ReadModelMap(AStream: TStream; AModel: TDeltaModel; APairCount: UInt64; AIsIndefinite: Boolean; ADepth: Integer = 0);
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

{ TDeltaCborCodec }

function TDeltaCborCodec.GetFormatName: string;
begin
  Result := 'cbor';
end;

function TDeltaCborCodec.GetMimeType: string;
begin
  Result := 'application/cbor';
end;

function TDeltaCborCodec.CanHandleMimeType(const AMimeType: string): Boolean;
var
  M: string;
  P: Integer;
begin
  M := LowerCase(Trim(AMimeType));
  P := Pos(';', M);
  if P > 0 then
    M := Trim(Copy(M, 1, P - 1));

  Result := (M = 'application/cbor') or
            (M = 'cbor');
end;

procedure TDeltaCborCodec.WriteHeader(AStream: TStream; AMajor: Byte; ALength: UInt64);
var
  InitialByte: Byte;
  U8: UInt8;
  U16: Word;
  U32: UInt32;
  U64: UInt64;
begin
  if ALength < 24 then
  begin
    InitialByte := (AMajor shl 5) or Byte(ALength);
    AStream.WriteBuffer(InitialByte, 1);
  end
  else if ALength <= $FF then
  begin
    InitialByte := (AMajor shl 5) or 24;
    AStream.WriteBuffer(InitialByte, 1);
    U8 := UInt8(ALength);
    AStream.WriteBuffer(U8, 1);
  end
  else if ALength <= $FFFF then
  begin
    InitialByte := (AMajor shl 5) or 25;
    AStream.WriteBuffer(InitialByte, 1);
    U16 := NtoBE(Word(ALength));
    AStream.WriteBuffer(U16, 2);
  end
  else if ALength <= $FFFFFFFF then
  begin
    InitialByte := (AMajor shl 5) or 26;
    AStream.WriteBuffer(InitialByte, 1);
    U32 := NtoBE(UInt32(ALength));
    AStream.WriteBuffer(U32, 4);
  end
  else
  begin
    InitialByte := (AMajor shl 5) or 27;
    AStream.WriteBuffer(InitialByte, 1);
    U64 := NtoBE(ALength);
    AStream.WriteBuffer(U64, 8);
  end;
end;

procedure TDeltaCborCodec.WriteInt64(AStream: TStream; AValue: Int64);
var
  N: UInt64;
begin
  if AValue >= 0 then
    WriteHeader(AStream, 0, UInt64(AValue))
  else
  begin
    // Major 1: -1 - AValue
    N := UInt64(-1 - AValue);
    WriteHeader(AStream, 1, N);
  end;
end;

procedure TDeltaCborCodec.WriteDouble(AStream: TStream; AValue: Double);
var
  InitialByte: Byte;
  U64, BE: UInt64;
begin
  InitialByte := $FB; // Major 7, 27
  AStream.WriteBuffer(InitialByte, 1);
  Move(AValue, U64, 8);
  BE := NtoBE(U64);
  AStream.WriteBuffer(BE, 8);
end;

procedure TDeltaCborCodec.WriteBoolean(AStream: TStream; AValue: Boolean);
var
  B: Byte;
begin
  if AValue then
    B := $F5
  else
    B := $F4;
  AStream.WriteBuffer(B, 1);
end;

procedure TDeltaCborCodec.WriteNull(AStream: TStream);
var
  B: Byte;
begin
  B := $F6;
  AStream.WriteBuffer(B, 1);
end;

procedure TDeltaCborCodec.WriteTextString(AStream: TStream; const S: string);
var
  Raw: RawByteString;
begin
  Raw := UTF8Encode(S);
  WriteHeader(AStream, 3, Length(Raw));
  if Length(Raw) > 0 then
    AStream.WriteBuffer(Raw[1], Length(Raw));
end;

function TDeltaCborCodec.ReadHeader(AStream: TStream; out AMajor: Byte; out AVal: UInt64; out AAddInfo: Byte; out AIsIndefinite: Boolean): Boolean;
var
  InitialByte: Byte;
  U8: UInt8;
  U16: Word;
  U32: UInt32;
  U64: UInt64;
begin
  Result := False;
  AIsIndefinite := False;
  AVal := 0;
  AAddInfo := 0;
  AMajor := 0;
  if AStream.Position >= AStream.Size then Exit;

  AStream.ReadBuffer(InitialByte, 1);
  AMajor := InitialByte shr 5;
  AAddInfo := InitialByte and $1F;

  if AAddInfo < 24 then
  begin
    AVal := AAddInfo;
    Result := True;
  end
  else if AAddInfo = 24 then
  begin
    if AStream.Position + 1 > AStream.Size then
      raise EDeltaDeserializationException.Create('CBOR payload truncated: expected 1 byte for additional info (RFC 8949 Section 10.1)');
    AStream.ReadBuffer(U8, 1);
    AVal := U8;
    Result := True;
  end
  else if AAddInfo = 25 then
  begin
    if AStream.Position + 2 > AStream.Size then
      raise EDeltaDeserializationException.Create('CBOR payload truncated: expected 2 bytes for additional info (RFC 8949 Section 10.1)');
    AStream.ReadBuffer(U16, 2);
    AVal := BEtoN(U16);
    Result := True;
  end
  else if AAddInfo = 26 then
  begin
    if AStream.Position + 4 > AStream.Size then
      raise EDeltaDeserializationException.Create('CBOR payload truncated: expected 4 bytes for additional info (RFC 8949 Section 10.1)');
    AStream.ReadBuffer(U32, 4);
    AVal := BEtoN(U32);
    Result := True;
  end
  else if AAddInfo = 27 then
  begin
    if AStream.Position + 8 > AStream.Size then
      raise EDeltaDeserializationException.Create('CBOR payload truncated: expected 8 bytes for additional info (RFC 8949 Section 10.1)');
    AStream.ReadBuffer(U64, 8);
    AVal := BEtoN(U64);
    Result := True;
  end
  else if AAddInfo = 31 then
  begin
    AIsIndefinite := True;
    Result := True;
  end
  else
  begin
    raise EDeltaDeserializationException.CreateFmt('Invalid CBOR additional info value %d (RFC 8949 Section 3)', [AAddInfo]);
  end;

  // RFC 8949 Section 10.1 - Resource Exhaustion & Truncation Checks:
  if (AMajor in [2, 3]) and not AIsIndefinite then
  begin
    if AVal > MAX_CBOR_STRING_LENGTH then
      raise EDeltaDeserializationException.CreateFmt('CBOR string length (%u bytes) exceeds maximum limit (RFC 8949 Section 10.1)', [AVal]);
    if AVal > UInt64(AStream.Size - AStream.Position) then
      raise EDeltaDeserializationException.Create('CBOR payload truncated: declared string length exceeds available bytes in stream (RFC 8949 Section 10.1)');
  end
  else if (AMajor = 4) and not AIsIndefinite then
  begin
    if AVal > UInt64(AStream.Size - AStream.Position) then
      raise EDeltaDeserializationException.Create('CBOR array truncated: declared item count exceeds available stream bytes (RFC 8949 Section 10.1)');
  end
  else if (AMajor = 5) and not AIsIndefinite then
  begin
    if AVal > UInt64((AStream.Size - AStream.Position) div 2) then
      raise EDeltaDeserializationException.Create('CBOR map truncated: declared pair count exceeds available stream bytes (RFC 8949 Section 10.1)');
  end;
end;

function TDeltaCborCodec.ReadTextString(AStream: TStream; ALength: UInt64): string;
var
  Raw: RawByteString;
begin
  if ALength = 0 then Exit('');
  if ALength > MAX_CBOR_STRING_LENGTH then
    raise EDeltaDeserializationException.CreateFmt('CBOR string length (%u bytes) exceeds maximum limit (RFC 8949 Section 10.1)', [ALength]);
  if ALength > UInt64(AStream.Size - AStream.Position) then
    raise EDeltaDeserializationException.Create('CBOR payload truncated: declared string length exceeds available bytes in stream (RFC 8949 Section 10.1)');

  SetLength(Raw, ALength);
  AStream.ReadBuffer(Raw[1], ALength);
  Result := UTF8ToString(Raw);
end;

procedure TDeltaCborCodec.SkipCborValue(AStream: TStream; ADepth: Integer = 0);
var
  Major, AddInfo: Byte;
  Val: UInt64;
  IsIndef: Boolean;
  I: UInt64;
  PeekB: Byte;
begin
  if ADepth > MAX_CBOR_NESTING_DEPTH then
    raise EDeltaDeserializationException.Create('Maximum CBOR nesting depth exceeded (RFC 8949 Section 10.1)');

  if AStream.Position >= AStream.Size then Exit;
  if not ReadHeader(AStream, Major, Val, AddInfo, IsIndef) then Exit;

  case Major of
    0, 1: // Simple integer: value already consumed
      ;

    2, 3: // Byte string, Text string
    begin
      if IsIndef then
      begin
        while AStream.Position < AStream.Size do
        begin
          AStream.ReadBuffer(PeekB, 1);
          if PeekB = $FF then Break;
          AStream.Seek(-1, soCurrent);
          SkipCborValue(AStream, ADepth + 1);
        end;
      end
      else
        AStream.Seek(Val, soCurrent);
    end;

    4: // Array
    begin
      if IsIndef then
      begin
        while AStream.Position < AStream.Size do
        begin
          AStream.ReadBuffer(PeekB, 1);
          if PeekB = $FF then Break;
          AStream.Seek(-1, soCurrent);
          SkipCborValue(AStream, ADepth + 1);
        end;
      end
      else
      begin
        for I := 1 to Val do
          SkipCborValue(AStream, ADepth + 1);
      end;
    end;

    5: // Map
    begin
      if IsIndef then
      begin
        while AStream.Position < AStream.Size do
        begin
          AStream.ReadBuffer(PeekB, 1);
          if PeekB = $FF then Break;
          AStream.Seek(-1, soCurrent);
          SkipCborValue(AStream, ADepth + 1); // key
          SkipCborValue(AStream, ADepth + 1); // value
        end;
      end
      else
      begin
        for I := 1 to Val * 2 do
          SkipCborValue(AStream, ADepth + 1);
      end;
    end;

    6: // Tag
    begin
      SkipCborValue(AStream, ADepth + 1);
    end;

    7: // Floats and simple values (bytes already consumed in ReadHeader)
      ;
  end;
end;

function TDeltaCborCodec.CountModelPairs(AModel: TDeltaModel): Integer;
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, Count: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
begin
  Count := 0;
  if AModel = nil then Exit(0);
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    if FieldMeta^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
      if (PropObj <> nil) and (PropObj is TDeltaField) and TDeltaField(PropObj).Visible then
        Inc(Count);
    end
    else
      Inc(Count);
  end;

  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;
    Inc(Count);
  end;

  Result := Count;
end;

procedure TDeltaCborCodec.WriteModelMap(AModel: TDeltaModel; AStream: TStream);
var
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, J, PairCount: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj, ChildObj: TObject;
  DeltaField: TDeltaField;
  Key: string;
begin
  if AModel = nil then
  begin
    WriteNull(AStream);
    Exit;
  end;

  PairCount := CountModelPairs(AModel);
  WriteHeader(AStream, 5, PairCount); // Major 5: Map

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Managed fields
  for I := 0 to Meta.FieldCount - 1 do
  begin
    FieldMeta := @Meta.Fields[I];
    Key := FieldMeta^.PropName;

    if FieldMeta^.IsDeltaField then
    begin
      PropObj := GetObjectProp(AModel, FieldMeta^.PropInfo);
      if (PropObj = nil) or not (PropObj is TDeltaField) then Continue;
      DeltaField := TDeltaField(PropObj);
      if not DeltaField.Visible then Continue;

      WriteTextString(AStream, Key);

      if DeltaField is TDFHasOne then
      begin
        if Assigned(TDFHasOne(DeltaField).Model) and (TDFHasOne(DeltaField).Model is TDeltaModel) then
          WriteModelMap(TDeltaModel(TDFHasOne(DeltaField).Model), AStream)
        else
          WriteNull(AStream);
      end
      else if DeltaField is TDFHasMany then
      begin
        WriteHeader(AStream, 4, TDFHasMany(DeltaField).Count);
        for J := 0 to TDFHasMany(DeltaField).Count - 1 do
        begin
          ChildObj := TDFHasMany(DeltaField)[J];
          if ChildObj is TDeltaModel then
            WriteModelMap(TDeltaModel(ChildObj), AStream)
          else
            WriteNull(AStream);
        end;
      end
      else if DeltaField.IsNull then
      begin
        WriteNull(AStream);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            WriteInt64(AStream, DeltaField.AsLargeInt);

          dfkFloat:
            WriteDouble(AStream, DeltaField.AsFloat);

          dfkBoolean:
            WriteBoolean(AStream, DeltaField.AsBoolean);

          dfkDateTime:
            WriteTextString(AStream, DeltaField.AsString);

          dfkString:
            WriteTextString(AStream, DeltaField.AsString);
        else
          WriteTextString(AStream, DeltaField.AsString);
        end;
      end;
    end
    else
    begin
      // Mapped primitive property
      WriteTextString(AStream, Key);
      if AModel.IsNull(FieldMeta^.PropIndex) then
      begin
        WriteNull(AStream);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            WriteInt64(AStream, GetOrdProp(AModel, FieldMeta^.PropInfo));

          dfkFloat:
            WriteDouble(AStream, GetFloatProp(AModel, FieldMeta^.PropInfo));

          dfkBoolean:
            WriteBoolean(AStream, Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo)));

          dfkDateTime:
            WriteTextString(AStream, FormatDateTime('yyyy-mm-dd hh:nn:ss', GetFloatProp(AModel, FieldMeta^.PropInfo)));

          dfkString:
            WriteTextString(AStream, GetStrProp(AModel, FieldMeta^.PropInfo));
        end;
      end;
    end;
  end;

  // 2. Additional published properties
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    Key := PropItem^.Name;
    WriteTextString(AStream, Key);

    case PropItem^.PropKind of
      tkInteger, tkInt64:
        WriteInt64(AStream, GetOrdProp(AModel, PropItem^.PropInfo));

      tkFloat:
        WriteDouble(AStream, GetFloatProp(AModel, PropItem^.PropInfo));

      tkBool:
        WriteBoolean(AStream, Boolean(GetOrdProp(AModel, PropItem^.PropInfo)));

      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
        WriteTextString(AStream, GetStrProp(AModel, PropItem^.PropInfo));

      tkEnumeration:
        WriteTextString(AStream, GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AModel, PropItem^.PropInfo)));
    end;
  end;
end;

procedure TDeltaCborCodec.ReadModelMap(AStream: TStream; AModel: TDeltaModel; APairCount: UInt64; AIsIndefinite: Boolean; ADepth: Integer = 0);
var
  Meta: TModelMetadata;
  FMeta: PDeltaFieldMeta;
  KeyMajor, ValMajor, KeyAddInfo, ValAddInfo: Byte;
  KeyVal, ValVal: UInt64;
  KeyIndef, ValIndef: Boolean;
  KeyName: string;
  NestedObj, ChildObj: TObject;

  // Data values
  IntVal: Int64;
  DblVal: Double;
  StrVal: string;
  SngVal: Single;
  PeekB: Byte;
  I, ArrCount: UInt64;
  ArrIndef: Boolean;
  ArrMajor, ArrAddInfo: Byte;
begin
  if ADepth > MAX_CBOR_NESTING_DEPTH then
    raise EDeltaDeserializationException.Create('Maximum CBOR nesting depth exceeded (RFC 8949 Section 10.1)');

  if AModel = nil then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);

  while (AStream.Position < AStream.Size) do
  begin
    if AIsIndefinite then
    begin
      AStream.ReadBuffer(PeekB, 1);
      if PeekB = $FF then Break; // Indefinite-length collection break stop code
      AStream.Seek(-1, soCurrent);
    end
    else
    begin
      if APairCount = 0 then Break;
      Dec(APairCount);
    end;

    // 1. Read key (UTF-8 string)
    if not ReadHeader(AStream, KeyMajor, KeyVal, KeyAddInfo, KeyIndef) then Break;
    if KeyMajor = 3 then
      KeyName := ReadTextString(AStream, KeyVal)
    else
    begin
      SkipCborValue(AStream, ADepth + 1);
      Continue;
    end;

    FMeta := Meta.FindFieldByProp(KeyName);
    if FMeta = nil then
      FMeta := Meta.FindField(KeyName);

    // 2. Read value
    if not ReadHeader(AStream, ValMajor, ValVal, ValAddInfo, ValIndef) then Break;

    case ValMajor of
      0: // Unsigned Integer
      begin
        IntVal := Int64(ValVal);
        if FMeta <> nil then
        begin
          if FMeta^.IsDeltaField then
          begin
            NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
            if NestedObj is TDeltaField then
            begin
              if FMeta^.FieldKind = dfkFloat then
                TDeltaField(NestedObj).AsFloat := IntVal
              else
                TDeltaField(NestedObj).AsInt64 := IntVal;
            end;
          end
          else
          begin
            if FMeta^.FieldKind = dfkFloat then
              SetFloatProp(AModel, FMeta^.PropInfo, IntVal)
            else
              SetOrdProp(AModel, FMeta^.PropInfo, IntVal);
            AModel.ClearNull(FMeta^.PropIndex);
          end;
        end;
      end;

      1: // Negative Integer
      begin
        IntVal := -1 - Int64(ValVal);
        if FMeta <> nil then
        begin
          if FMeta^.IsDeltaField then
          begin
            NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
            if NestedObj is TDeltaField then
            begin
              if FMeta^.FieldKind = dfkFloat then
                TDeltaField(NestedObj).AsFloat := IntVal
              else
                TDeltaField(NestedObj).AsInt64 := IntVal;
            end;
          end
          else
          begin
            if FMeta^.FieldKind = dfkFloat then
              SetFloatProp(AModel, FMeta^.PropInfo, IntVal)
            else
              SetOrdProp(AModel, FMeta^.PropInfo, IntVal);
            AModel.ClearNull(FMeta^.PropIndex);
          end;
        end;
      end;

      2: // Byte String
      begin
        AStream.Seek(ValVal, soCurrent);
      end;

      3: // UTF-8 Text String
      begin
        StrVal := ReadTextString(AStream, ValVal);
        if FMeta <> nil then
        begin
          if FMeta^.IsDeltaField then
          begin
            NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
            if NestedObj is TDeltaField then
            begin
              if FMeta^.FieldKind = dfkDateTime then
                TDeltaField(NestedObj).SetAsString(StrVal)
              else if FMeta^.FieldKind = dfkInteger then
                TDeltaField(NestedObj).AsInt64 := StrToInt64Def(StrVal, 0)
              else if FMeta^.FieldKind = dfkFloat then
                TDeltaField(NestedObj).AsFloat := StrToFloatDef(StrVal, 0)
              else
                TDeltaField(NestedObj).SetAsString(StrVal);
            end;
          end
          else
          begin
            SetStrProp(AModel, FMeta^.PropInfo, StrVal);
            AModel.ClearNull(FMeta^.PropIndex);
          end;
        end;
      end;

      4: // Array (HasMany)
      begin
        ArrCount := ValVal;
        ArrIndef := ValIndef;
        if (FMeta <> nil) and FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDFHasMany then
          begin
            TDFHasMany(NestedObj).Clear;
            while True do
            begin
              if ArrIndef then
              begin
                if AStream.Position >= AStream.Size then Break;
                AStream.ReadBuffer(PeekB, 1);
                if PeekB = $FF then Break;
                AStream.Seek(-1, soCurrent);
              end
              else
              begin
                if ArrCount = 0 then Break;
                Dec(ArrCount);
              end;

              if not ReadHeader(AStream, ArrMajor, ValVal, ArrAddInfo, ValIndef) then Break;
              if ArrMajor = 5 then
              begin
                ChildObj := nil;
                if TDFHasMany(NestedObj).RelationClass <> nil then
                  ChildObj := TDeltaModelClass(TDFHasMany(NestedObj).RelationClass).Create;
                if ChildObj is TDeltaModel then
                begin
                  ReadModelMap(AStream, TDeltaModel(ChildObj), ValVal, ValIndef, ADepth + 1);
                  TDFHasMany(NestedObj).Add(ChildObj);
                end
                else
                  SkipCborValue(AStream, ADepth + 1);
              end
              else
                SkipCborValue(AStream, ADepth + 1);
            end;
          end
          else
          begin
            for I := 1 to ArrCount do
              SkipCborValue(AStream, ADepth + 1);
          end;
        end
        else
        begin
          for I := 1 to ArrCount do
            SkipCborValue(AStream, ADepth + 1);
        end;
      end;

      5: // Map (HasOne / subdocumento)
      begin
        if (FMeta <> nil) and FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDFHasOne then
          begin
            if (TDFHasOne(NestedObj).Model = nil) and (TDFHasOne(NestedObj).RelationClass <> nil) then
              TDFHasOne(NestedObj).Model := TDeltaModelClass(TDFHasOne(NestedObj).RelationClass).Create;
            if TDFHasOne(NestedObj).Model is TDeltaModel then
              ReadModelMap(AStream, TDeltaModel(TDFHasOne(NestedObj).Model), ValVal, ValIndef, ADepth + 1)
            else
            begin
              if ValIndef then
              begin
                while AStream.Position < AStream.Size do
                begin
                  AStream.ReadBuffer(PeekB, 1);
                  if PeekB = $FF then Break;
                  AStream.Seek(-1, soCurrent);
                  SkipCborValue(AStream, ADepth + 1);
                  SkipCborValue(AStream, ADepth + 1);
                end;
              end
              else
              begin
                for I := 1 to ValVal * 2 do
                  SkipCborValue(AStream, ADepth + 1);
              end;
            end;
          end
          else
          begin
            if ValIndef then
            begin
              while AStream.Position < AStream.Size do
              begin
                AStream.ReadBuffer(PeekB, 1);
                if PeekB = $FF then Break;
                AStream.Seek(-1, soCurrent);
                SkipCborValue(AStream, ADepth + 1);
                SkipCborValue(AStream, ADepth + 1);
              end;
            end
            else
            begin
              for I := 1 to ValVal * 2 do
                SkipCborValue(AStream, ADepth + 1);
            end;
          end;
        end
        else
        begin
          if ValIndef then
          begin
            while AStream.Position < AStream.Size do
            begin
              AStream.ReadBuffer(PeekB, 1);
              if PeekB = $FF then Break;
              AStream.Seek(-1, soCurrent);
              SkipCborValue(AStream, ADepth + 1);
              SkipCborValue(AStream, ADepth + 1);
            end;
          end
          else
          begin
            for I := 1 to ValVal * 2 do
              SkipCborValue(AStream, ADepth + 1);
          end;
        end;
      end;

      6: // Semantic tag (e.g., tag 0 = RFC 3339 date, tag 1 = epoch)
      begin
        // Read tagged value recursively
        if ReadHeader(AStream, ValMajor, ValVal, ValAddInfo, ValIndef) then
        begin
          if ValMajor = 3 then
          begin
            StrVal := ReadTextString(AStream, ValVal);
            if (FMeta <> nil) and FMeta^.IsDeltaField then
            begin
              NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
              if NestedObj is TDeltaField then
                TDeltaField(NestedObj).SetAsString(StrVal);
            end;
          end
          else
            SkipCborValue(AStream, ADepth + 1);
        end;
      end;

      7: // Simple values and Floats
      begin
        case ValAddInfo of
          20: // False
          begin
            if FMeta <> nil then
            begin
              if FMeta^.IsDeltaField then
              begin
                NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
                if NestedObj is TDeltaField then
                  TDeltaField(NestedObj).AsBoolean := False;
              end
              else
              begin
                SetOrdProp(AModel, FMeta^.PropInfo, 0);
                AModel.ClearNull(FMeta^.PropIndex);
              end;
            end;
          end;

          21: // True
          begin
            if FMeta <> nil then
            begin
              if FMeta^.IsDeltaField then
              begin
                NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
                if NestedObj is TDeltaField then
                  TDeltaField(NestedObj).AsBoolean := True;
              end
              else
              begin
                SetOrdProp(AModel, FMeta^.PropInfo, 1);
                AModel.ClearNull(FMeta^.PropIndex);
              end;
            end;
          end;

          22, 23: // Null, Undefined
          begin
            if FMeta <> nil then
            begin
              if FMeta^.IsDeltaField then
              begin
                NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
                if NestedObj is TDeltaField then
                  TDeltaField(NestedObj).Clear;
              end
              else
                AModel.SetNull(FMeta^.PropIndex);
            end;
          end;

          26: // Float 32-bit (Single)
          begin
            Move(ValVal, SngVal, 4);
            DblVal := SngVal;
            if FMeta <> nil then
            begin
              if FMeta^.IsDeltaField then
              begin
                NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
                if NestedObj is TDeltaField then
                  TDeltaField(NestedObj).AsFloat := DblVal;
              end
              else
              begin
                SetFloatProp(AModel, FMeta^.PropInfo, DblVal);
                AModel.ClearNull(FMeta^.PropIndex);
              end;
            end;
          end;

          27: // Float 64-bit (Double)
          begin
            Move(ValVal, DblVal, 8);
            if FMeta <> nil then
            begin
              if FMeta^.IsDeltaField then
              begin
                NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
                if NestedObj is TDeltaField then
                  TDeltaField(NestedObj).AsFloat := DblVal;
              end
              else
              begin
                SetFloatProp(AModel, FMeta^.PropInfo, DblVal);
                AModel.ClearNull(FMeta^.PropIndex);
              end;
            end;
          end;
        end;
      end;
    else
      SkipCborValue(AStream);
    end;
  end;
end;

procedure TDeltaCborCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
begin
  if (AModel = nil) or (AOutputStream = nil) then Exit;
  WriteModelMap(AModel, AOutputStream);
end;

procedure TDeltaCborCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  CustomList: TCustomDeltaModelList;
  I, Count: Integer;
  ItemObj: TObject;
begin
  if (AList = nil) or (AOutputStream = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  Count := CustomList.Count;
  WriteHeader(AOutputStream, 4, Count); // Major 4: Array

  for I := 0 to Count - 1 do
  begin
    ItemObj := CustomList.GetItemObj(I);
    if ItemObj is TDeltaModel then
      WriteModelMap(TDeltaModel(ItemObj), AOutputStream)
    else
      WriteNull(AOutputStream);
  end;
end;

procedure TDeltaCborCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
var
  Major, AddInfo: Byte;
  Val: UInt64;
  IsIndef: Boolean;
begin
  if (AInputStream = nil) or (AModel = nil) then Exit;
  if not ReadHeader(AInputStream, Major, Val, AddInfo, IsIndef) then
    raise EDeltaDeserializationException.Create('CBOR payload is empty or invalid (RFC 8949)');
  if Major <> 5 then
    raise EDeltaDeserializationException.CreateFmt('Expected CBOR Map (Major type 5) at root, found Major %d (RFC 8949)', [Major]);
  ReadModelMap(AInputStream, AModel, Val, IsIndef, 0);
end;

procedure TDeltaCborCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
var
  Major, AddInfo, KeyMajor, KeyAddInfo, ValMajor, ValAddInfo: Byte;
  Val, KeyVal, ValVal, ArrCount: UInt64;
  IsIndef, KeyIndef, ValIndef, ArrIndef: Boolean;
  CustomList: TCustomDeltaModelList;
  NewObj: TObject;
  KeyName: string;
  PeekB: Byte;
begin
  if (AInputStream = nil) or (AList = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  if (CustomList.GetModelClass = nil) and (AModelClass <> nil) and
     (AList is TDeltaModelList) and AModelClass.InheritsFrom(TDeltaModel) then
    TDeltaModelList(AList).DeltaModelClass := TDeltaModelClass(AModelClass);

  if not ReadHeader(AInputStream, Major, Val, AddInfo, IsIndef) then
    raise EDeltaDeserializationException.Create('CBOR payload is empty or invalid (RFC 8949)');

  // 1. Direct model array: [ {..}, {..} ]
  if Major = 4 then
  begin
    ArrCount := Val;
    ArrIndef := IsIndef;

    while True do
    begin
      if ArrIndef then
      begin
        if AInputStream.Position >= AInputStream.Size then Break;
        AInputStream.ReadBuffer(PeekB, 1);
        if PeekB = $FF then Break;
        AInputStream.Seek(-1, soCurrent);
      end
      else
      begin
        if ArrCount = 0 then Break;
        Dec(ArrCount);
      end;

      if not ReadHeader(AInputStream, ValMajor, ValVal, ValAddInfo, ValIndef) then Break;
      if ValMajor = 5 then
      begin
        NewObj := nil;
        if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
          NewObj := TDeltaModelClass(AModelClass).Create
        else
          NewObj := CustomList.NewItem;

        if NewObj is TDeltaModel then
        begin
          ReadModelMap(AInputStream, TDeltaModel(NewObj), ValVal, ValIndef, 1);
          CustomList.AddObj(NewObj);
        end
        else
        begin
          NewObj.Free;
          SkipCborValue(AInputStream, 1);
        end;
      end
      else
        SkipCborValue(AInputStream, 1);
    end;
    Exit;
  end;

  // 2. Map envelope: {"items": [..], "total_records": N, ...}
  if Major = 5 then
  begin
    while True do
    begin
      if IsIndef then
      begin
        if AInputStream.Position >= AInputStream.Size then Break;
        AInputStream.ReadBuffer(PeekB, 1);
        if PeekB = $FF then Break;
        AInputStream.Seek(-1, soCurrent);
      end
      else
      begin
        if Val = 0 then Break;
        Dec(Val);
      end;

      if not ReadHeader(AInputStream, KeyMajor, KeyVal, KeyAddInfo, KeyIndef) then Break;
      if KeyMajor = 3 then
        KeyName := ReadTextString(AInputStream, KeyVal)
      else
      begin
        SkipCborValue(AInputStream, 1);
        Continue;
      end;

      if not ReadHeader(AInputStream, ValMajor, ValVal, ValAddInfo, ValIndef) then Break;

      if (SameText(KeyName, 'items') or SameText(KeyName, 'data')) and (ValMajor = 4) then
      begin
        ArrCount := ValVal;
        ArrIndef := ValIndef;

        while True do
        begin
          if ArrIndef then
          begin
            if AInputStream.Position >= AInputStream.Size then Break;
            AInputStream.ReadBuffer(PeekB, 1);
            if PeekB = $FF then Break;
            AInputStream.Seek(-1, soCurrent);
          end
          else
          begin
            if ArrCount = 0 then Break;
            Dec(ArrCount);
          end;

          if not ReadHeader(AInputStream, Major, ValVal, AddInfo, ValIndef) then Break;
          if Major = 5 then
          begin
            NewObj := nil;
            if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
              NewObj := TDeltaModelClass(AModelClass).Create
            else
              NewObj := CustomList.NewItem;

            if NewObj is TDeltaModel then
            begin
              ReadModelMap(AInputStream, TDeltaModel(NewObj), ValVal, ValIndef, 2);
              CustomList.AddObj(NewObj);
            end
            else
            begin
              NewObj.Free;
              SkipCborValue(AInputStream, 2);
            end;
          end
          else
            SkipCborValue(AInputStream, 2);
        end;
      end
      else if SameText(KeyName, 'total_records') and (ValMajor in [0, 1]) then
      begin
        CustomList.total_records := Int64(ValVal);
      end
      else if SameText(KeyName, 'page') and (ValMajor in [0, 1]) then
      begin
        CustomList.page := Integer(ValVal);
      end
      else if SameText(KeyName, 'page_size') and (ValMajor in [0, 1]) then
      begin
        CustomList.page_size := Integer(ValVal);
      end
      else
        SkipCborValue(AInputStream, 1);
    end;
  end
  else
    raise EDeltaDeserializationException.CreateFmt('Expected CBOR Array or Map envelope at list root, found Major %d (RFC 8949)', [Major]);
end;

initialization
  TDeltaCodecRegistry.RegisterCodec(TDeltaCborCodec.Create);

end.
