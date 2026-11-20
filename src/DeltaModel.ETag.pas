{
    DeltaModel - RFC 7232 / RFC 9110 Entity Tag (ETag) Engine

    Provides:
    - Deterministic ETag calculation for TDeltaModel (both Style A TDeltaField
      and Style B primitive properties with NullMask bitmask).
    - Support for SHA-256 (default) and ultra-fast zero-allocation FNV-1a 64-bit.
    - RFC 7232 Section 2.3 grammar validation and canonical formatting ("<tag>" / W/"<tag>").
    - RFC 7232 Section 2.3.2 Strong and Weak comparison functions.
    - Quote-aware ETag list parser (handles commas inside opaque-tags and '*' wildcard).
    - Class helper for TDeltaModel to avoid intrusive edits to DeltaModel.pas.
}

unit DeltaModel.ETag;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, TypInfo, fpsha256,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata, DeltaModel.List;

type
  { Hash algorithm used to compute deterministic model entity tags }
  TETagHashAlgorithm = (
    ehaSHA256,
    ehaFNV1a64
  );

  { Parsed representation of an RFC 7232 Section 2.3 entity-tag }
  TEntityTag = record
    OpaqueTag: string;
    Weak: Boolean;
    IsWildcard: Boolean;
    IsValid: Boolean;
  end;

  TEntityTagArray = array of TEntityTag;

  { Class helper exposing ETag calculation directly on TDeltaModel instances }
  TDeltaModelETagHelper = class helper for TDeltaModel
  public
    function CalculateETag(AWeak: Boolean = False;
      AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
    function CalculateWeakETag(AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
  end;

{ Formats an opaque-tag into canonical RFC 7232 format: "<tag>" or W/"<tag>" }
function FormatETag(const AOpaqueTag: string; AWeak: Boolean = False): string;

{ Validates that AOpaqueTag contains only RFC 7232 Section 2.3 etagc octets }
function IsValidOpaqueTag(const AOpaqueTag: string): Boolean;

{ Parses a single entity-tag ("abc", W/"abc", or *) according to RFC 7232 Section 2.3 }
function TryParseETag(const ARaw: string; out ATag: TEntityTag): Boolean;

{ Parses a comma-separated list of entity-tags or '*' (quote-aware per RFC 7232) }
function ParseETagList(const AHeaderValue: string): TEntityTagArray;

{ RFC 7232 Section 2.3.2 Strong Comparison:
  Both tags must be strong (not weak) and have identical opaque-tags. }
function StrongCompareETag(const A, B: TEntityTag): Boolean; overload;
function StrongCompareETag(const A, B: string): Boolean; overload;

{ RFC 7232 Section 2.3.2 Weak Comparison:
  Both tags have identical opaque-tags, regardless of weakness indicator. }
function WeakCompareETag(const A, B: TEntityTag): Boolean; overload;
function WeakCompareETag(const A, B: string): Boolean; overload;

{ Evaluates whether ACurrentETag matches any member of AHeaderList (including '*').
  When AUseStrongComparison is True, uses StrongCompareETag (If-Match semantics).
  When AUseStrongComparison is False, uses WeakCompareETag (If-None-Match semantics). }
function MatchesETagList(const ACurrentETag, AHeaderList: string;
  AUseStrongComparison: Boolean; AResourceExists: Boolean = True): Boolean;

{ Calculates a deterministic RFC 7232 ETag over a TDeltaModel instance }
function CalculateETag(AModel: TDeltaModel; AWeak: Boolean = False;
  AAlgorithm: TETagHashAlgorithm = ehaSHA256): string; overload;

{ Calculates a deterministic RFC 7232 ETag over raw bytes }
function CalculateETagFromBytes(const AData: RawByteString; AWeak: Boolean = False;
  AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;

{ Wraps an existing version token (e.g. row version / revision number) as an ETag }
function CalculateETagFromVersion(const AVersionToken: string; AWeak: Boolean = False): string;

implementation

const
  FNV1A_64_OFFSET_BASIS: QWord = QWord($CBF29CE484222325);
  FNV1A_64_PRIME: QWord = QWord($00000100000001B3);
  HEX_CHARS_LOWER: array[0..15] of Char = '0123456789abcdef';

type
  { Internal streaming accumulator supporting SHA-256 and zero-alloc FNV-1a 64 }
  TETagAccumulator = record
    Algorithm: TETagHashAlgorithm;
    FnvHash: QWord;
    Buffer: TBytes;
    BufLen: SizeInt;
    BufCap: SizeInt;
    procedure Init(AAlgorithm: TETagHashAlgorithm);
    procedure FeedRaw(AData: Pointer; ALen: SizeInt); inline;
    procedure FeedByte(AByte: Byte); inline;
    procedure FeedString(const AStr: string); inline;
    procedure FeedInt64(AVal: Int64); inline;
    procedure FeedDouble(AVal: Double); inline;
    function FinalizeHex: string;
  end;

{ TETagAccumulator }

procedure TETagAccumulator.Init(AAlgorithm: TETagHashAlgorithm);
begin
  Algorithm := AAlgorithm;
  FnvHash := FNV1A_64_OFFSET_BASIS;
  Buffer := nil;
  BufLen := 0;
  BufCap := 0;
  if Algorithm = ehaSHA256 then
  begin
    BufCap := 512;
    SetLength(Buffer, BufCap);
  end;
end;

procedure TETagAccumulator.FeedRaw(AData: Pointer; ALen: SizeInt);
var
  P: PByte;
  I: SizeInt;
  NewCap: SizeInt;
begin
  if (AData = nil) or (ALen <= 0) then Exit;

  if Algorithm = ehaFNV1a64 then
  begin
    {$IFOPT Q+}{$DEFINE Q_WAS_ON}{$Q-}{$ENDIF}
    {$IFOPT R+}{$DEFINE R_WAS_ON}{$R-}{$ENDIF}
    P := PByte(AData);
    for I := 0 to ALen - 1 do
    begin
      FnvHash := FnvHash xor P[I];
      FnvHash := FnvHash * FNV1A_64_PRIME;
    end;
    {$IFDEF R_WAS_ON}{$UNDEF R_WAS_ON}{$R+}{$ENDIF}
    {$IFDEF Q_WAS_ON}{$UNDEF Q_WAS_ON}{$Q+}{$ENDIF}
  end
  else
  begin
    if BufLen + ALen > BufCap then
    begin
      NewCap := BufCap;
      if NewCap < 512 then
        NewCap := 512;
      while NewCap < BufLen + ALen do
        NewCap := NewCap * 2;
      SetLength(Buffer, NewCap);
      BufCap := NewCap;
    end;
    Move(AData^, Buffer[BufLen], ALen);
    Inc(BufLen, ALen);
  end;
end;

procedure TETagAccumulator.FeedByte(AByte: Byte);
begin
  FeedRaw(@AByte, 1);
end;

procedure TETagAccumulator.FeedString(const AStr: string);
var
  L: SizeInt;
begin
  L := Length(AStr);
  FeedInt64(L);
  if L > 0 then
    FeedRaw(@AStr[1], L);
end;

procedure TETagAccumulator.FeedInt64(AVal: Int64);
begin
  FeedRaw(@AVal, SizeOf(Int64));
end;

procedure TETagAccumulator.FeedDouble(AVal: Double);
var
  NormVal: Double;
begin
  { Normalize -0.0 to +0.0 for deterministic hashing }
  if AVal = 0.0 then
    NormVal := 0.0
  else
    NormVal := AVal;
  FeedRaw(@NormVal, SizeOf(Double));
end;

function TETagAccumulator.FinalizeHex: string;
var
  HexOut: AnsiString;
  FinalBytes: TBytes;
  I: Integer;
  V: QWord;
begin
  if Algorithm = ehaFNV1a64 then
  begin
    SetLength(Result, 16);
    V := FnvHash;
    for I := 16 downto 1 do
    begin
      Result[I] := HEX_CHARS_LOWER[V and $0F];
      V := V shr 4;
    end;
  end
  else
  begin
    if BufLen = 0 then
      SetLength(FinalBytes, 0)
    else if BufLen = BufCap then
      FinalBytes := Buffer
    else
    begin
      SetLength(FinalBytes, BufLen);
      Move(Buffer[0], FinalBytes[0], BufLen);
    end;
    TSHA256.DigestHexa(FinalBytes, HexOut);
    Result := LowerCase(string(HexOut));
  end;
end;

{ Internal recursive model feeder }

procedure FeedModelToAccumulator(AModel: TDeltaModel; var AAcc: TETagAccumulator);
var
  ClassInfo: TRTTIClassInfo;
  PropItem: PDeltaPropItem;
  I, J: Integer;
  ObjVal: TObject;
  FieldObj: TDeltaField;
  ListObj: TCustomDeltaModelList;
  ChildObj: TObject;
  BoolByte: Byte;
begin
  if AModel = nil then
  begin
    AAcc.FeedByte($00);
    Exit;
  end;

  AAcc.FeedByte($01);
  AAcc.FeedString(AModel.ClassName);

  ClassInfo := TRTTICache.GetClassProps(AModel.ClassType);
  for I := 0 to ClassInfo.PropCount - 1 do
  begin
    PropItem := @ClassInfo.Props[I];
    if not PropItem^.HasGetProc then
      Continue;

    if PropItem^.IsDeltaField then
    begin
      ObjVal := GetObjectProp(AModel, PropItem^.PropInfo);
      if (ObjVal = nil) or not (ObjVal is TDeltaField) then
        Continue;

      FieldObj := TDeltaField(ObjVal);
      { Skip invisible fields (e.g. sensitive secrets not part of representation) }
      if not FieldObj.Visible then
        Continue;

      AAcc.FeedByte($10);
      AAcc.FeedString(PropItem^.Name);

      if FieldObj.IsNull then
      begin
        AAcc.FeedByte($00);
      end
      else
      begin
        AAcc.FeedByte($01);
        case FieldObj.FieldKind of
          dfkInteger:
            AAcc.FeedInt64(FieldObj.AsLargeInt);
          dfkFloat:
            AAcc.FeedDouble(FieldObj.AsFloat);
          dfkBoolean:
            begin
              if FieldObj.AsBoolean then BoolByte := 1 else BoolByte := 0;
              AAcc.FeedByte(BoolByte);
            end;
          dfkDateTime:
            AAcc.FeedDouble(FieldObj.AsDateTime);
        else
          AAcc.FeedString(FieldObj.AsString);
        end;
      end;
    end
    else if PropItem^.IsDeltaModelList then
    begin
      AAcc.FeedByte($20);
      AAcc.FeedString(PropItem^.Name);
      ObjVal := GetObjectProp(AModel, PropItem^.PropInfo);
      if (ObjVal <> nil) and (ObjVal is TCustomDeltaModelList) then
      begin
        ListObj := TCustomDeltaModelList(ObjVal);
        AAcc.FeedInt64(ListObj.Count);
        for J := 0 to ListObj.Count - 1 do
        begin
          ChildObj := ListObj.GetItemObj(J);
          if ChildObj is TDeltaModel then
            FeedModelToAccumulator(TDeltaModel(ChildObj), AAcc)
          else
            AAcc.FeedByte($00);
        end;
      end
      else
        AAcc.FeedInt64(-1);
    end
    else if PropItem^.IsClass then
    begin
      ObjVal := GetObjectProp(AModel, PropItem^.PropInfo);
      if (ObjVal <> nil) and (ObjVal is TDeltaModel) then
      begin
        AAcc.FeedByte($30);
        AAcc.FeedString(PropItem^.Name);
        FeedModelToAccumulator(TDeltaModel(ObjVal), AAcc);
      end;
    end
    else
    begin
      { Style B: Primitive published property with bitmask nullability }
      AAcc.FeedByte($40);
      AAcc.FeedString(PropItem^.Name);

      if AModel.IsNull(PropItem^.Index) then
      begin
        AAcc.FeedByte($00);
      end
      else
      begin
        AAcc.FeedByte($01);
        case PropItem^.PropKind of
          tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkBool:
            AAcc.FeedInt64(GetOrdProp(AModel, PropItem^.PropInfo));
          tkInt64, tkQWord:
            AAcc.FeedInt64(GetInt64Prop(AModel, PropItem^.PropInfo));
          tkFloat:
            AAcc.FeedDouble(GetFloatProp(AModel, PropItem^.PropInfo));
          tkSString, tkLString, tkAString, tkWString, tkUString:
            AAcc.FeedString(GetStrProp(AModel, PropItem^.PropInfo));
        else
          AAcc.FeedString(GetPropValue(AModel, PropItem^.Name, True));
        end;
      end;
    end;
  end;
end;

{ Public functions }

function FormatETag(const AOpaqueTag: string; AWeak: Boolean = False): string;
begin
  if AWeak then
    Result := 'W/"' + AOpaqueTag + '"'
  else
    Result := '"' + AOpaqueTag + '"';
end;

function IsValidOpaqueTag(const AOpaqueTag: string): Boolean;
var
  I: SizeInt;
  B: Byte;
begin
  { RFC 7232 Section 2.3:
    opaque-tag = DQUOTE *etagc DQUOTE
    etagc      = %x21 / %x23-7E / obs-text (%x80-FF)
    Excludes CTLs (0x00-0x1F, 0x7F), SP (0x20), and DQUOTE (0x22). }
  for I := 1 to Length(AOpaqueTag) do
  begin
    B := Ord(AOpaqueTag[I]);
    if (B < $21) or (B = $22) or (B = $7F) then
      Exit(False);
  end;
  Result := True;
end;

function TryParseETag(const ARaw: string; out ATag: TEntityTag): Boolean;
var
  S: string;
  StartIdx, Len: SizeInt;
  Opaque: string;
begin
  ATag.OpaqueTag := '';
  ATag.Weak := False;
  ATag.IsWildcard := False;
  ATag.IsValid := False;
  Result := False;

  S := Trim(ARaw);
  Len := Length(S);
  if Len = 0 then
    Exit;

  if S = '*' then
  begin
    ATag.IsWildcard := True;
    ATag.IsValid := True;
    Exit(True);
  end;

  StartIdx := 1;
  { Weak indicator is strictly case-sensitive "W/" per RFC 7232 Section 2.3 }
  if (Len >= 2) and (S[1] = 'W') and (S[2] = '/') then
  begin
    ATag.Weak := True;
    StartIdx := 3;
  end;

  { Must have opening and closing DQUOTE }
  if (Len - StartIdx + 1) < 2 then
    Exit;
  if (S[StartIdx] <> '"') or (S[Len] <> '"') then
    Exit;

  Opaque := Copy(S, StartIdx + 1, Len - StartIdx - 1);
  if not IsValidOpaqueTag(Opaque) then
    Exit;

  ATag.OpaqueTag := Opaque;
  ATag.IsValid := True;
  Result := True;
end;

function ParseETagList(const AHeaderValue: string): TEntityTagArray;
var
  S, Token: string;
  I, Len, TokenStart, Count: SizeInt;
  InQuotes: Boolean;
  Tag: TEntityTag;
begin
  Result := nil;
  S := Trim(AHeaderValue);
  Len := Length(S);
  if Len = 0 then
    Exit;

  if S = '*' then
  begin
    SetLength(Result, 1);
    Result[0].OpaqueTag := '';
    Result[0].Weak := False;
    Result[0].IsWildcard := True;
    Result[0].IsValid := True;
    Exit;
  end;

  Count := 0;
  InQuotes := False;
  TokenStart := 1;

  for I := 1 to Len + 1 do
  begin
    if (I <= Len) and (S[I] = '"') then
      InQuotes := not InQuotes
    else if (I > Len) or ((S[I] = ',') and not InQuotes) then
    begin
      Token := Trim(Copy(S, TokenStart, I - TokenStart));
      if Token <> '' then
      begin
        if TryParseETag(Token, Tag) then
        begin
          SetLength(Result, Count + 1);
          Result[Count] := Tag;
          Inc(Count);
        end;
      end;
      TokenStart := I + 1;
    end;
  end;
end;

function StrongCompareETag(const A, B: TEntityTag): Boolean;
begin
  { RFC 7232 Section 2.3.2:
    Strong comparison: two entity-tags are equivalent if both are not weak
    and their opaque-tags match character-by-character. }
  if (not A.IsValid) or (not B.IsValid) then
    Exit(False);
  if A.IsWildcard or B.IsWildcard then
    Exit(False);
  if A.Weak or B.Weak then
    Exit(False);
  Result := (A.OpaqueTag = B.OpaqueTag);
end;

function StrongCompareETag(const A, B: string): Boolean;
var
  TagA, TagB: TEntityTag;
begin
  if (not TryParseETag(A, TagA)) or (not TryParseETag(B, TagB)) then
    Exit(False);
  Result := StrongCompareETag(TagA, TagB);
end;

function WeakCompareETag(const A, B: TEntityTag): Boolean;
begin
  { RFC 7232 Section 2.3.2:
    Weak comparison: two entity-tags are equivalent if their opaque-tags
    match character-by-character, regardless of either or both being tagged
    as "weak". }
  if (not A.IsValid) or (not B.IsValid) then
    Exit(False);
  if A.IsWildcard or B.IsWildcard then
    Exit(False);
  Result := (A.OpaqueTag = B.OpaqueTag);
end;

function WeakCompareETag(const A, B: string): Boolean;
var
  TagA, TagB: TEntityTag;
begin
  if (not TryParseETag(A, TagA)) or (not TryParseETag(B, TagB)) then
    Exit(False);
  Result := WeakCompareETag(TagA, TagB);
end;

function MatchesETagList(const ACurrentETag, AHeaderList: string;
  AUseStrongComparison: Boolean; AResourceExists: Boolean = True): Boolean;
var
  Tags: TEntityTagArray;
  CurrentTag: TEntityTag;
  HasCurrentTag: Boolean;
  I: SizeInt;
begin
  Result := False;
  Tags := ParseETagList(AHeaderList);
  if Length(Tags) = 0 then
    Exit(False);

  { Check wildcard '*' first }
  for I := 0 to High(Tags) do
  begin
    if Tags[I].IsWildcard then
      Exit(AResourceExists);
  end;

  if not AResourceExists then
    Exit(False);

  HasCurrentTag := TryParseETag(ACurrentETag, CurrentTag);
  if not HasCurrentTag then
    Exit(False);

  for I := 0 to High(Tags) do
  begin
    if AUseStrongComparison then
    begin
      if StrongCompareETag(CurrentTag, Tags[I]) then
        Exit(True);
    end
    else
    begin
      if WeakCompareETag(CurrentTag, Tags[I]) then
        Exit(True);
    end;
  end;
end;

function CalculateETag(AModel: TDeltaModel; AWeak: Boolean = False;
  AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
var
  Acc: TETagAccumulator;
begin
  Acc.Init(AAlgorithm);
  FeedModelToAccumulator(AModel, Acc);
  Result := FormatETag(Acc.FinalizeHex, AWeak);
end;

function CalculateETagFromBytes(const AData: RawByteString; AWeak: Boolean = False;
  AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
var
  Acc: TETagAccumulator;
begin
  Acc.Init(AAlgorithm);
  if Length(AData) > 0 then
    Acc.FeedRaw(@AData[1], Length(AData));
  Result := FormatETag(Acc.FinalizeHex, AWeak);
end;

function CalculateETagFromVersion(const AVersionToken: string; AWeak: Boolean = False): string;
begin
  Result := FormatETag(AVersionToken, AWeak);
end;

{ TDeltaModelETagHelper }

function TDeltaModelETagHelper.CalculateETag(AWeak: Boolean = False;
  AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
begin
  Result := DeltaModel.ETag.CalculateETag(Self, AWeak, AAlgorithm);
end;

function TDeltaModelETagHelper.CalculateWeakETag(AAlgorithm: TETagHashAlgorithm = ehaSHA256): string;
begin
  Result := DeltaModel.ETag.CalculateETag(Self, True, AAlgorithm);
end;

end.
