unit DeltaModel.Codec.Bson;

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
  { TDeltaBsonCodec: Codec BSON v1.1 Binário de Alta Performance (Zero-DOM)
    Especificação BSON v1.1 (bsonspec.org):
    - Documento: int32 tamanho total em bytes (little-endian), elementos, 0x00 terminal.
    - Elemento: [type: 1 byte][cstring name: null-terminated][payload].
    - Tipos suportados:
      * 0x01: Double (IEEE 754 float64, 8 bytes little-endian)
      * 0x02: UTF-8 String (int32 length incluindo \0 + bytes + \0)
      * 0x03: Documento Embutido (HasOne / sub-modelo)
      * 0x04: Array (HasMany / lista indexada "0", "1", ...)
      * 0x05: Binary BLOB (int32 len + subtype byte + bytes)
      * 0x08: Boolean (1 byte: 0x00 false, 0x01 true)
      * 0x09: UTC Datetime (int64 ms desde 1970-01-01 UTC)
      * 0x0A: Null (sem payload)
      * 0x10: Int32 (4 bytes little-endian)
      * 0x12: Int64 (8 bytes little-endian) }
  TDeltaBsonCodec = class(TInterfacedObject, IDeltaCodec)
  private
    procedure WriteCString(AStream: TStream; const S: string);
    procedure WriteBsonString(AStream: TStream; const S: string);
    function ReadCString(AStream: TStream): string;
    function ReadBsonString(AStream: TStream): string;
    procedure SkipBsonElementPayload(AStream: TStream; AType: Byte);

    procedure WriteModelDoc(AModel: TDeltaModel; AStream: TStream);
    procedure ReadModelDoc(AStream: TStream; AModel: TDeltaModel);
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

{ Funções Auxiliares de Conversão Little-Endian }

function LEToHostDbl(const ABytes: UInt64): Double;
var
  U: UInt64;
begin
  Result := 0.0;
  U := LEtoN(ABytes);
  Move(U, Result, 8);
end;

function HostToLEDbl(const AValue: Double): UInt64;
var
  U: UInt64;
begin
  U := 0;
  Move(AValue, U, 8);
  Result := NtoLE(U);
end;

{ TDeltaBsonCodec }

function TDeltaBsonCodec.GetFormatName: string;
begin
  Result := 'bson';
end;

function TDeltaBsonCodec.GetMimeType: string;
begin
  Result := 'application/bson';
end;

function TDeltaBsonCodec.CanHandleMimeType(const AMimeType: string): Boolean;
var
  M: string;
  P: Integer;
begin
  M := LowerCase(Trim(AMimeType));
  P := Pos(';', M);
  if P > 0 then
    M := Trim(Copy(M, 1, P - 1));

  Result := (M = 'application/bson') or
            (M = 'application/x-bson') or
            (M = 'bson');
end;

procedure TDeltaBsonCodec.WriteCString(AStream: TStream; const S: string);
var
  Raw: RawByteString;
  Zero: Byte;
begin
  Raw := UTF8Encode(S);
  if Length(Raw) > 0 then
    AStream.WriteBuffer(Raw[1], Length(Raw));
  Zero := 0;
  AStream.WriteBuffer(Zero, 1);
end;

procedure TDeltaBsonCodec.WriteBsonString(AStream: TStream; const S: string);
var
  Raw: RawByteString;
  Len: Int32;
  Zero: Byte;
begin
  Raw := UTF8Encode(S);
  Len := NtoLE(Int32(Length(Raw) + 1));
  AStream.WriteBuffer(Len, 4);
  if Length(Raw) > 0 then
    AStream.WriteBuffer(Raw[1], Length(Raw));
  Zero := 0;
  AStream.WriteBuffer(Zero, 1);
end;

function TDeltaBsonCodec.ReadCString(AStream: TStream): string;
var
  B: Byte;
  Raw: RawByteString;
  Len: Integer;
begin
  Raw := '';
  Len := 0;
  while AStream.Position < AStream.Size do
  begin
    AStream.ReadBuffer(B, 1);
    if B = 0 then Break;
    Inc(Len);
    SetLength(Raw, Len);
    Raw[Len] := AnsiChar(B);
  end;
  Result := UTF8ToString(Raw);
end;

function TDeltaBsonCodec.ReadBsonString(AStream: TStream): string;
var
  LenLE, StrLen: Int32;
  Raw: RawByteString;
  Zero: Byte;
begin
  Result := '';
  if AStream.Position + 4 > AStream.Size then Exit;
  AStream.ReadBuffer(LenLE, 4);
  StrLen := LEtoN(LenLE);
  if StrLen <= 0 then Exit;

  if StrLen > 1 then
  begin
    SetLength(Raw, StrLen - 1);
    AStream.ReadBuffer(Raw[1], StrLen - 1);
    Result := UTF8ToString(Raw);
  end;
  // Lê o byte nulo terminal \0
  if AStream.Position < AStream.Size then
    AStream.ReadBuffer(Zero, 1);
end;

procedure TDeltaBsonCodec.SkipBsonElementPayload(AStream: TStream; AType: Byte);
var
  DocSizeLE, StrLenLE, BinLenLE: Int32;
  DocSize, StrLen, BinLen: Int32;
  Subtype: Byte;
begin
  case AType of
    $01: // Double: 8 bytes
      AStream.Seek(8, soCurrent);

    $02, $0D, $0E: // String, JS, Symbol: int32 len + bytes
    begin
      if AStream.Position + 4 <= AStream.Size then
      begin
        AStream.ReadBuffer(StrLenLE, 4);
        StrLen := LEtoN(StrLenLE);
        if StrLen > 0 then
          AStream.Seek(StrLen, soCurrent);
      end;
    end;

    $03, $04: // Embedded document or Array: int32 size + ...
    begin
      if AStream.Position + 4 <= AStream.Size then
      begin
        AStream.ReadBuffer(DocSizeLE, 4);
        DocSize := LEtoN(DocSizeLE);
        if DocSize > 4 then
          AStream.Seek(DocSize - 4, soCurrent);
      end;
    end;

    $05: // Binary BLOB: int32 len + subtype (1 byte) + bytes
    begin
      if AStream.Position + 5 <= AStream.Size then
      begin
        AStream.ReadBuffer(BinLenLE, 4);
        AStream.ReadBuffer(Subtype, 1);
        BinLen := LEtoN(BinLenLE);
        if BinLen > 0 then
          AStream.Seek(BinLen, soCurrent);
      end;
    end;

    $07: // ObjectId: 12 bytes
      AStream.Seek(12, soCurrent);

    $08: // Boolean: 1 byte
      AStream.Seek(1, soCurrent);

    $09: // UTC Datetime: 8 bytes
      AStream.Seek(8, soCurrent);

    $0A: // Null: 0 bytes
      ;

    $0B: // RegEx: cstring pattern + cstring options
    begin
      ReadCString(AStream);
      ReadCString(AStream);
    end;

    $10: // Int32: 4 bytes
      AStream.Seek(4, soCurrent);

    $11: // Timestamp: 8 bytes
      AStream.Seek(8, soCurrent);

    $12: // Int64: 8 bytes
      AStream.Seek(8, soCurrent);

    $13: // Decimal128: 16 bytes
      AStream.Seek(16, soCurrent);
  else
    // Tipo desconhecido; não avança para evitar corrupção
  end;
end;

procedure TDeltaBsonCodec.WriteModelDoc(AModel: TDeltaModel; AStream: TStream);
var
  StartPos, EndPos: Int64;
  DocLen, Placeholder: Int32;
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, J: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj: TObject;
  DeltaField: TDeltaField;
  Key: string;
  IntVal: Int64;
  DblVal: Double;
  DTVal: TDateTime;
  MsVal: Int64;
  SVal: string;
  BVal, ElemType, Zero: Byte;
  I32: Int32;
  I64: Int64;
  U64: UInt64;
  ArrStartPos, ArrEndPos: Int64;
  ArrLen: Int32;
  ChildObj: TObject;
begin
  if AModel = nil then Exit;

  StartPos := AStream.Position;
  Placeholder := 0;
  AStream.WriteBuffer(Placeholder, 4);

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Campos gerenciados (TDeltaField e primitivos com mapeamento Schema)
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

      if DeltaField is TDFHasOne then
      begin
        if Assigned(TDFHasOne(DeltaField).Model) and (TDFHasOne(DeltaField).Model is TDeltaModel) then
        begin
          ElemType := $03; // Documento embutido
          AStream.WriteBuffer(ElemType, 1);
          WriteCString(AStream, Key);
          WriteModelDoc(TDeltaModel(TDFHasOne(DeltaField).Model), AStream);
        end
        else
        begin
          ElemType := $0A; // Null
          AStream.WriteBuffer(ElemType, 1);
          WriteCString(AStream, Key);
        end;
      end
      else if DeltaField is TDFHasMany then
      begin
        ElemType := $04; // Array BSON
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);

        ArrStartPos := AStream.Position;
        AStream.WriteBuffer(Placeholder, 4);

        for J := 0 to TDFHasMany(DeltaField).Count - 1 do
        begin
          ChildObj := TDFHasMany(DeltaField)[J];
          if ChildObj is TDeltaModel then
          begin
            ElemType := $03;
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, IntToStr(J));
            WriteModelDoc(TDeltaModel(ChildObj), AStream);
          end;
        end;

        Zero := 0;
        AStream.WriteBuffer(Zero, 1);
        ArrEndPos := AStream.Position;
        ArrLen := NtoLE(Int32(ArrEndPos - ArrStartPos));
        AStream.Position := ArrStartPos;
        AStream.WriteBuffer(ArrLen, 4);
        AStream.Position := ArrEndPos;
      end
      else if DeltaField.IsNull then
      begin
        ElemType := $0A; // Null
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
          begin
            IntVal := DeltaField.AsLargeInt;
            if (IntVal >= -2147483648) and (IntVal <= 2147483647) then
            begin
              ElemType := $10; // Int32
              AStream.WriteBuffer(ElemType, 1);
              WriteCString(AStream, Key);
              I32 := NtoLE(Int32(IntVal));
              AStream.WriteBuffer(I32, 4);
            end
            else
            begin
              ElemType := $12; // Int64
              AStream.WriteBuffer(ElemType, 1);
              WriteCString(AStream, Key);
              I64 := NtoLE(IntVal);
              AStream.WriteBuffer(I64, 8);
            end;
          end;

          dfkFloat:
          begin
            ElemType := $01; // Double
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            DblVal := DeltaField.AsFloat;
            U64 := HostToLEDbl(DblVal);
            AStream.WriteBuffer(U64, 8);
          end;

          dfkBoolean:
          begin
            ElemType := $08; // Boolean
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            if DeltaField.AsBoolean then BVal := 1 else BVal := 0;
            AStream.WriteBuffer(BVal, 1);
          end;

          dfkDateTime:
          begin
            ElemType := $09; // UTC Datetime ms
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            DTVal := DeltaField.AsDateTime;
            if DTVal = 0 then
              MsVal := 0
            else
              MsVal := Round((DTVal - 25569.0) * 86400000.0);
            I64 := NtoLE(MsVal);
            AStream.WriteBuffer(I64, 8);
          end;

          dfkString:
          begin
            ElemType := $02; // String UTF-8
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            SVal := DeltaField.AsString;
            WriteBsonString(AStream, SVal);
          end;
        else
          ElemType := $02;
          AStream.WriteBuffer(ElemType, 1);
          WriteCString(AStream, Key);
          SVal := DeltaField.AsString;
          WriteBsonString(AStream, SVal);
        end;
      end;
    end
    else
    begin
      // Propriedade Primitiva mapeada
      if AModel.IsNull(FieldMeta^.PropIndex) then
      begin
        ElemType := $0A; // Null
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
          begin
            IntVal := GetOrdProp(AModel, FieldMeta^.PropInfo);
            ElemType := $12; // Int64
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            I64 := NtoLE(IntVal);
            AStream.WriteBuffer(I64, 8);
          end;

          dfkFloat:
          begin
            ElemType := $01; // Double
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            DblVal := GetFloatProp(AModel, FieldMeta^.PropInfo);
            U64 := HostToLEDbl(DblVal);
            AStream.WriteBuffer(U64, 8);
          end;

          dfkBoolean:
          begin
            ElemType := $08;
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            if Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo)) then BVal := 1 else BVal := 0;
            AStream.WriteBuffer(BVal, 1);
          end;

          dfkDateTime:
          begin
            ElemType := $09;
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            DTVal := GetFloatProp(AModel, FieldMeta^.PropInfo);
            if DTVal = 0 then
              MsVal := 0
            else
              MsVal := Round((DTVal - 25569.0) * 86400000.0);
            I64 := NtoLE(MsVal);
            AStream.WriteBuffer(I64, 8);
          end;

          dfkString:
          begin
            ElemType := $02;
            AStream.WriteBuffer(ElemType, 1);
            WriteCString(AStream, Key);
            SVal := GetStrProp(AModel, FieldMeta^.PropInfo);
            WriteBsonString(AStream, SVal);
          end;
        end;
      end;
    end;
  end;

  // 2. Propriedades publicadas adicionais (fora do Schema)
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    Key := PropItem^.Name;
    case PropItem^.PropKind of
      tkInteger, tkInt64:
      begin
        IntVal := GetOrdProp(AModel, PropItem^.PropInfo);
        ElemType := $12;
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
        I64 := NtoLE(IntVal);
        AStream.WriteBuffer(I64, 8);
      end;

      tkFloat:
      begin
        ElemType := $01;
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
        DblVal := GetFloatProp(AModel, PropItem^.PropInfo);
        U64 := HostToLEDbl(DblVal);
        AStream.WriteBuffer(U64, 8);
      end;

      tkBool:
      begin
        ElemType := $08;
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
        if Boolean(GetOrdProp(AModel, PropItem^.PropInfo)) then BVal := 1 else BVal := 0;
        AStream.WriteBuffer(BVal, 1);
      end;

      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
      begin
        ElemType := $02;
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
        SVal := GetStrProp(AModel, PropItem^.PropInfo);
        WriteBsonString(AStream, SVal);
      end;

      tkEnumeration:
      begin
        ElemType := $02;
        AStream.WriteBuffer(ElemType, 1);
        WriteCString(AStream, Key);
        SVal := GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AModel, PropItem^.PropInfo));
        WriteBsonString(AStream, SVal);
      end;
    end;
  end;

  // Terminal de documento \0
  Zero := 0;
  AStream.WriteBuffer(Zero, 1);

  // Atualiza tamanho total do documento no cabeçalho
  EndPos := AStream.Position;
  DocLen := NtoLE(Int32(EndPos - StartPos));
  AStream.Position := StartPos;
  AStream.WriteBuffer(DocLen, 4);
  AStream.Position := EndPos;
end;

procedure TDeltaBsonCodec.ReadModelDoc(AStream: TStream; AModel: TDeltaModel);
var
  DocStartPos, DocEndPos: Int64;
  DocLenLE, DocLen: Int32;
  ElemType, Zero: Byte;
  ElemName: string;
  Meta: TModelMetadata;
  FMeta: PDeltaFieldMeta;
  NestedObj, ChildObj: TObject;

  // Valores lidos
  IntVal: Int64;
  DblVal: Double;
  BoolVal: Boolean;
  DTVal: TDateTime;
  StrVal: string;

  I32: Int32;
  I64: Int64;
  U64: UInt64;
  BVal: Byte;
  ArrStartPos, ArrEndPos: Int64;
  ArrDocLenLE, ArrDocLen: Int32;
  ArrElemType: Byte;
  ArrElemIdx: string;
begin
  if (AModel = nil) or (AStream.Position + 4 > AStream.Size) then Exit;

  DocStartPos := AStream.Position;
  AStream.ReadBuffer(DocLenLE, 4);
  DocLen := LEtoN(DocLenLE);
  if DocLen < 5 then Exit;
  DocEndPos := DocStartPos + DocLen;
  if DocEndPos > AStream.Size then
    DocEndPos := AStream.Size;

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);

  while AStream.Position < DocEndPos - 1 do
  begin
    AStream.ReadBuffer(ElemType, 1);
    if ElemType = 0 then Break; // Terminal do documento

    ElemName := ReadCString(AStream);

    FMeta := Meta.FindFieldByProp(ElemName);
    if FMeta = nil then
      FMeta := Meta.FindField(ElemName);

    case ElemType of
      $01: // Double
      begin
        AStream.ReadBuffer(U64, 8);
        DblVal := LEToHostDbl(U64);
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

      $02: // String UTF-8
      begin
        StrVal := ReadBsonString(AStream);
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

      $03: // Documento Embutido (HasOne)
      begin
        if (FMeta <> nil) and FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDFHasOne then
          begin
            if (TDFHasOne(NestedObj).Model = nil) and (TDFHasOne(NestedObj).RelationClass <> nil) then
              TDFHasOne(NestedObj).Model := TDeltaModelClass(TDFHasOne(NestedObj).RelationClass).Create;
            if TDFHasOne(NestedObj).Model is TDeltaModel then
              ReadModelDoc(AStream, TDeltaModel(TDFHasOne(NestedObj).Model))
            else
              SkipBsonElementPayload(AStream, $03);
          end
          else
            SkipBsonElementPayload(AStream, $03);
        end
        else
          SkipBsonElementPayload(AStream, $03);
      end;

      $04: // Array BSON (HasMany)
      begin
        if (FMeta <> nil) and FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDFHasMany then
          begin
            TDFHasMany(NestedObj).Clear;
            ArrStartPos := AStream.Position;
            AStream.ReadBuffer(ArrDocLenLE, 4);
            ArrDocLen := LEtoN(ArrDocLenLE);
            ArrEndPos := ArrStartPos + ArrDocLen;
            if ArrEndPos > DocEndPos then ArrEndPos := DocEndPos;

            while AStream.Position < ArrEndPos - 1 do
            begin
              AStream.ReadBuffer(ArrElemType, 1);
              if ArrElemType = 0 then Break;
              ArrElemIdx := ReadCString(AStream);

              if ArrElemType = $03 then
              begin
                ChildObj := nil;
                if TDFHasMany(NestedObj).RelationClass <> nil then
                  ChildObj := TDeltaModelClass(TDFHasMany(NestedObj).RelationClass).Create;
                if ChildObj is TDeltaModel then
                begin
                  ReadModelDoc(AStream, TDeltaModel(ChildObj));
                  TDFHasMany(NestedObj).Add(ChildObj);
                end
                else
                  SkipBsonElementPayload(AStream, $03);
              end
              else
                SkipBsonElementPayload(AStream, ArrElemType);
            end;

            if AStream.Position < ArrEndPos then
              AStream.Position := ArrEndPos;
          end
          else
            SkipBsonElementPayload(AStream, $04);
        end
        else
          SkipBsonElementPayload(AStream, $04);
      end;

      $08: // Boolean
      begin
        AStream.ReadBuffer(BVal, 1);
        BoolVal := (BVal <> 0);
        if FMeta <> nil then
        begin
          if FMeta^.IsDeltaField then
          begin
            NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
            if NestedObj is TDeltaField then
              TDeltaField(NestedObj).AsBoolean := BoolVal;
          end
          else
          begin
            SetOrdProp(AModel, FMeta^.PropInfo, Ord(BoolVal));
            AModel.ClearNull(FMeta^.PropIndex);
          end;
        end;
      end;

      $09: // UTC Datetime ms
      begin
        AStream.ReadBuffer(I64, 8);
        IntVal := LEtoN(I64);
        if IntVal = 0 then
          DTVal := 0
        else
          DTVal := (Double(IntVal) / 86400000.0) + 25569.0;

        if FMeta <> nil then
        begin
          if FMeta^.IsDeltaField then
          begin
            NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
            if NestedObj is TDeltaField then
              TDeltaField(NestedObj).AsDateTime := DTVal;
          end
          else
          begin
            SetFloatProp(AModel, FMeta^.PropInfo, DTVal);
            AModel.ClearNull(FMeta^.PropIndex);
          end;
        end;
      end;

      $0A: // Null
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

      $10: // Int32
      begin
        AStream.ReadBuffer(I32, 4);
        IntVal := LEtoN(I32);
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

      $12: // Int64
      begin
        AStream.ReadBuffer(I64, 8);
        IntVal := LEtoN(I64);
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
    else
      SkipBsonElementPayload(AStream, ElemType);
    end;
  end;

  if (AStream.Position < DocEndPos) and (AStream.Position = DocEndPos - 1) then
    AStream.ReadBuffer(Zero, 1)
  else if AStream.Position < DocEndPos then
    AStream.Position := DocEndPos;
end;

procedure TDeltaBsonCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
begin
  if (AModel = nil) or (AOutputStream = nil) then Exit;
  WriteModelDoc(AModel, AOutputStream);
end;

procedure TDeltaBsonCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  CustomList: TCustomDeltaModelList;
  StartPos, EndPos, ArrStartPos, ArrEndPos: Int64;
  DocLen, ArrLen, Placeholder: Int32;
  I, Count: Integer;
  ItemObj: TObject;
  ElemType, Zero: Byte;
  I64: Int64;
  I32: Int32;
begin
  if (AList = nil) or (AOutputStream = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  StartPos := AOutputStream.Position;
  Placeholder := 0;
  AOutputStream.WriteBuffer(Placeholder, 4);

  // Metadados de paginação
  ElemType := $12; // Int64 total_records
  AOutputStream.WriteBuffer(ElemType, 1);
  WriteCString(AOutputStream, 'total_records');
  I64 := NtoLE(CustomList.total_records);
  AOutputStream.WriteBuffer(I64, 8);

  ElemType := $10; // Int32 page
  AOutputStream.WriteBuffer(ElemType, 1);
  WriteCString(AOutputStream, 'page');
  I32 := NtoLE(Int32(CustomList.page));
  AOutputStream.WriteBuffer(I32, 4);

  ElemType := $10; // Int32 page_size
  AOutputStream.WriteBuffer(ElemType, 1);
  WriteCString(AOutputStream, 'page_size');
  I32 := NtoLE(Int32(CustomList.page_size));
  AOutputStream.WriteBuffer(I32, 4);

  // Array BSON 'items'
  ElemType := $04;
  AOutputStream.WriteBuffer(ElemType, 1);
  WriteCString(AOutputStream, 'items');

  ArrStartPos := AOutputStream.Position;
  AOutputStream.WriteBuffer(Placeholder, 4);

  Count := CustomList.Count;
  for I := 0 to Count - 1 do
  begin
    ItemObj := CustomList.GetItemObj(I);
    if ItemObj is TDeltaModel then
    begin
      ElemType := $03; // Documento embutido
      AOutputStream.WriteBuffer(ElemType, 1);
      WriteCString(AOutputStream, IntToStr(I));
      WriteModelDoc(TDeltaModel(ItemObj), AOutputStream);
    end;
  end;

  Zero := 0;
  AOutputStream.WriteBuffer(Zero, 1);
  ArrEndPos := AOutputStream.Position;
  ArrLen := NtoLE(Int32(ArrEndPos - ArrStartPos));
  AOutputStream.Position := ArrStartPos;
  AOutputStream.WriteBuffer(ArrLen, 4);
  AOutputStream.Position := ArrEndPos;

  // Finaliza documento envelope
  AOutputStream.WriteBuffer(Zero, 1);
  EndPos := AOutputStream.Position;
  DocLen := NtoLE(Int32(EndPos - StartPos));
  AOutputStream.Position := StartPos;
  AOutputStream.WriteBuffer(DocLen, 4);
  AOutputStream.Position := EndPos;
end;

procedure TDeltaBsonCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
begin
  if (AInputStream = nil) or (AModel = nil) then Exit;
  ReadModelDoc(AInputStream, AModel);
end;

procedure TDeltaBsonCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
var
  DocStartPos, DocEndPos: Int64;
  DocLenLE, DocLen: Int32;
  ElemType, Zero: Byte;
  ElemName: string;
  ArrStartPos, ArrEndPos: Int64;
  ArrDocLenLE, ArrDocLen: Int32;
  ArrElemType: Byte;
  ArrElemIdx: string;
  NewObj: TObject;
  CustomList: TCustomDeltaModelList;
  I32: Int32;
  I64: Int64;
begin
  if (AInputStream = nil) or (AList = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  if (CustomList.GetModelClass = nil) and (AModelClass <> nil) and
     (AList is TDeltaModelList) and AModelClass.InheritsFrom(TDeltaModel) then
    TDeltaModelList(AList).DeltaModelClass := TDeltaModelClass(AModelClass);

  if AInputStream.Position + 4 > AInputStream.Size then Exit;

  DocStartPos := AInputStream.Position;
  AInputStream.ReadBuffer(DocLenLE, 4);
  DocLen := LEtoN(DocLenLE);
  if DocLen < 5 then Exit;
  DocEndPos := DocStartPos + DocLen;
  if DocEndPos > AInputStream.Size then
    DocEndPos := AInputStream.Size;

  while AInputStream.Position < DocEndPos - 1 do
  begin
    AInputStream.ReadBuffer(ElemType, 1);
    if ElemType = 0 then Break;
    ElemName := ReadCString(AInputStream);

    if (SameText(ElemName, 'items') or SameText(ElemName, 'data')) and (ElemType in [$03, $04]) then
    begin
      // Array ou documento com subdocumentos
      ArrStartPos := AInputStream.Position;
      AInputStream.ReadBuffer(ArrDocLenLE, 4);
      ArrDocLen := LEtoN(ArrDocLenLE);
      ArrEndPos := ArrStartPos + ArrDocLen;
      if ArrEndPos > DocEndPos then ArrEndPos := DocEndPos;

      while AInputStream.Position < ArrEndPos - 1 do
      begin
        AInputStream.ReadBuffer(ArrElemType, 1);
        if ArrElemType = 0 then Break;
        ArrElemIdx := ReadCString(AInputStream);

        if ArrElemType = $03 then
        begin
          NewObj := nil;
          if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
            NewObj := TDeltaModelClass(AModelClass).Create
          else
            NewObj := CustomList.NewItem;

          if NewObj is TDeltaModel then
          begin
            ReadModelDoc(AInputStream, TDeltaModel(NewObj));
            CustomList.AddObj(NewObj);
          end
          else
          begin
            NewObj.Free;
            SkipBsonElementPayload(AInputStream, $03);
          end;
        end
        else
          SkipBsonElementPayload(AInputStream, ArrElemType);
      end;

      if AInputStream.Position < ArrEndPos then
        AInputStream.Position := ArrEndPos;
    end
    else if (ElemType = $03) and ((ElemName = '0') or (StrToIntDef(ElemName, -1) >= 0)) then
    begin
      // Documento com itens diretamente na raiz: {"0": {...}, "1": {...}}
      NewObj := nil;
      if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
        NewObj := TDeltaModelClass(AModelClass).Create
      else
        NewObj := CustomList.NewItem;

      if NewObj is TDeltaModel then
      begin
        ReadModelDoc(AInputStream, TDeltaModel(NewObj));
        CustomList.AddObj(NewObj);
      end
      else
      begin
        NewObj.Free;
        SkipBsonElementPayload(AInputStream, $03);
      end;
    end
    else if SameText(ElemName, 'total_records') and (ElemType in [$10, $12]) then
    begin
      if ElemType = $10 then
      begin
        AInputStream.ReadBuffer(I32, 4);
        CustomList.total_records := LEtoN(I32);
      end
      else
      begin
        AInputStream.ReadBuffer(I64, 8);
        CustomList.total_records := LEtoN(I64);
      end;
    end
    else if SameText(ElemName, 'page') and (ElemType in [$10, $12]) then
    begin
      if ElemType = $10 then
      begin
        AInputStream.ReadBuffer(I32, 4);
        CustomList.page := LEtoN(I32);
      end
      else
      begin
        AInputStream.ReadBuffer(I64, 8);
        CustomList.page := LEtoN(I64);
      end;
    end
    else if SameText(ElemName, 'page_size') and (ElemType in [$10, $12]) then
    begin
      if ElemType = $10 then
      begin
        AInputStream.ReadBuffer(I32, 4);
        CustomList.page_size := LEtoN(I32);
      end
      else
      begin
        AInputStream.ReadBuffer(I64, 8);
        CustomList.page_size := LEtoN(I64);
      end;
    end
    else
      SkipBsonElementPayload(AInputStream, ElemType);
  end;

  if (AInputStream.Position < DocEndPos) and (AInputStream.Position = DocEndPos - 1) then
    AInputStream.ReadBuffer(Zero, 1)
  else if AInputStream.Position < DocEndPos then
    AInputStream.Position := DocEndPos;
end;

initialization
  TDeltaCodecRegistry.RegisterCodec(TDeltaBsonCodec.Create);

end.
