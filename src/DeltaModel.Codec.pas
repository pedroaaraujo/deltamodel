unit DeltaModel.Codec;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  fpjson,
  jsonparser,
  DeltaModel,
  DeltaModel.List,
  DeltaModel.JsonStreamer;

type
  { Codec Architecture Exceptions }
  EDeltaCodecException = class(Exception)
  public
    constructor CreateFmt(const AFormat: string; const AArgs: array of const);
  end;
  EDeltaCodecNotFound = class(EDeltaCodecException);
  EDeltaSerializationException = class(EDeltaCodecException);
  EDeltaDeserializationException = class(EDeltaCodecException);

  { Universal Serialization / Deserialization Codec Contract }
  IDeltaCodec = interface
    ['{A5F8E2D1-B73C-4D90-8E14-6F39A1C54E20}']
    function GetFormatName: string;
    function GetMimeType: string;
    function CanHandleMimeType(const AMimeType: string): Boolean;
    procedure EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
    procedure EncodeList(AList: TObject; AOutputStream: TStream);
    procedure DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
    procedure DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
  end;

  { Thread-Safe Central Codec Registry for DeltaModel }
  TDeltaCodecRegistry = class
  private
    class var FLock: TCriticalSection;
    class var FCodecs: TInterfaceList;
    class procedure EnsureInitialized;
    class function NormalizeKey(const AKey: string): string;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;

    class procedure RegisterCodec(ACodec: IDeltaCodec);
    class procedure UnregisterCodec(const AFormatName: string);
    class function GetCodec(const AMimeTypeOrFormat: string): IDeltaCodec;
    class function HasCodec(const AMimeTypeOrFormat: string): Boolean;
    class function GetSupportedMimeTypes: TStringArray;
    class function GetSupportedFormats: TStringArray;
    class procedure Clear;

    { Direct String Convenience Helpers }
    class function EncodeModelToString(AModel: TDeltaModel; const AMimeTypeOrFormat: string = 'json'): string;
    class procedure DecodeModelFromString(const AContent: string; AModel: TDeltaModel; const AMimeTypeOrFormat: string = 'json');
    class function EncodeListToString(AList: TObject; const AMimeTypeOrFormat: string = 'json'): string;
    class procedure DecodeListFromString(const AContent: string; AList: TObject; AModelClass: TClass; const AMimeTypeOrFormat: string = 'json');
  end;

  { Default High-Performance JSON Codec (Zero-DOM Streamer) }
  TDeltaJsonCodec = class(TInterfacedObject, IDeltaCodec)
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

{ EDeltaCodecException }

constructor EDeltaCodecException.CreateFmt(const AFormat: string; const AArgs: array of const);
begin
  inherited Create(Format(AFormat, AArgs));
end;

{ Local Utility Functions }

function StreamToRawBytes(AStream: TStream): RawByteString;
var
  Len: Int64;
begin
  Result := '';
  if AStream = nil then Exit;
  Len := AStream.Size - AStream.Position;
  if Len <= 0 then Exit;
  SetLength(Result, Len);
  AStream.ReadBuffer(Result[1], Len);
end;

procedure RawBytesToStream(const S: RawByteString; AStream: TStream);
begin
  if (AStream = nil) or (Length(S) = 0) then Exit;
  AStream.WriteBuffer(S[1], Length(S));
end;

{ TDeltaCodecRegistry }

class constructor TDeltaCodecRegistry.ClassCreate;
begin
  FLock := TCriticalSection.Create;
  FCodecs := TInterfaceList.Create;
end;

class destructor TDeltaCodecRegistry.ClassDestroy;
begin
  FreeAndNil(FCodecs);
  FreeAndNil(FLock);
end;

class procedure TDeltaCodecRegistry.EnsureInitialized;
begin
  if FLock = nil then
    FLock := TCriticalSection.Create;
  if FCodecs = nil then
    FCodecs := TInterfaceList.Create;
end;

class function TDeltaCodecRegistry.NormalizeKey(const AKey: string): string;
var
  P: Integer;
begin
  Result := LowerCase(Trim(AKey));
  P := Pos(';', Result);
  if P > 0 then
    Result := Trim(Copy(Result, 1, P - 1));
end;

class procedure TDeltaCodecRegistry.RegisterCodec(ACodec: IDeltaCodec);
var
  I: Integer;
  Existing: IDeltaCodec;
begin
  if ACodec = nil then Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    // Replace if a codec with the same FormatName is already registered
    for I := FCodecs.Count - 1 downto 0 do
    begin
      Existing := FCodecs[I] as IDeltaCodec;
      if SameText(Existing.GetFormatName, ACodec.GetFormatName) then
      begin
        FCodecs.Delete(I);
        Break;
      end;
    end;
    FCodecs.Add(ACodec);
  finally
    FLock.Leave;
  end;
end;

class procedure TDeltaCodecRegistry.UnregisterCodec(const AFormatName: string);
var
  I: Integer;
  Existing: IDeltaCodec;
begin
  EnsureInitialized;
  FLock.Enter;
  try
    for I := FCodecs.Count - 1 downto 0 do
    begin
      Existing := FCodecs[I] as IDeltaCodec;
      if SameText(Existing.GetFormatName, AFormatName) then
      begin
        FCodecs.Delete(I);
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

class function TDeltaCodecRegistry.GetCodec(const AMimeTypeOrFormat: string): IDeltaCodec;
var
  NormKey: string;
  I: Integer;
  Codec: IDeltaCodec;
begin
  Result := nil;
  EnsureInitialized;
  NormKey := NormalizeKey(AMimeTypeOrFormat);
  if NormKey.IsEmpty then
    raise EDeltaCodecNotFound.Create('Identificador de codec ou MIME type não informado.');

  FLock.Enter;
  try
    // 1. Lookup by exact FormatName (e.g., 'json', 'cbor', 'xml')
    for I := 0 to FCodecs.Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      if SameText(Codec.GetFormatName, NormKey) then
        Exit(Codec);
    end;

    // 2. Lookup by exact MimeType (e.g., 'application/json')
    for I := 0 to FCodecs.Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      if SameText(NormalizeKey(Codec.GetMimeType), NormKey) then
        Exit(Codec);
    end;

    // 3. Lookup via CanHandleMimeType
    for I := 0 to FCodecs.Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      if Codec.CanHandleMimeType(NormKey) then
        Exit(Codec);
    end;
  finally
    FLock.Leave;
  end;

  raise EDeltaCodecNotFound.CreateFmt('Nenhum codec registrado para o formato ou MIME type "%s".', [AMimeTypeOrFormat]);
end;

class function TDeltaCodecRegistry.HasCodec(const AMimeTypeOrFormat: string): Boolean;
var
  NormKey: string;
  I: Integer;
  Codec: IDeltaCodec;
begin
  Result := False;
  EnsureInitialized;
  NormKey := NormalizeKey(AMimeTypeOrFormat);
  if NormKey.IsEmpty then Exit(False);

  FLock.Enter;
  try
    for I := 0 to FCodecs.Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      if SameText(Codec.GetFormatName, NormKey) or
         SameText(NormalizeKey(Codec.GetMimeType), NormKey) or
         Codec.CanHandleMimeType(NormKey) then
        Exit(True);
    end;
  finally
    FLock.Leave;
  end;
end;

class function TDeltaCodecRegistry.GetSupportedMimeTypes: TStringArray;
var
  I, Count: Integer;
  Codec: IDeltaCodec;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try
    Count := FCodecs.Count;
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      Result[I] := Codec.GetMimeType;
    end;
  finally
    FLock.Leave;
  end;
end;

class function TDeltaCodecRegistry.GetSupportedFormats: TStringArray;
var
  I, Count: Integer;
  Codec: IDeltaCodec;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try
    Count := FCodecs.Count;
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
    begin
      Codec := FCodecs[I] as IDeltaCodec;
      Result[I] := Codec.GetFormatName;
    end;
  finally
    FLock.Leave;
  end;
end;

class procedure TDeltaCodecRegistry.Clear;
begin
  EnsureInitialized;
  FLock.Enter;
  try
    FCodecs.Clear;
  finally
    FLock.Leave;
  end;
end;

class function TDeltaCodecRegistry.EncodeModelToString(AModel: TDeltaModel; const AMimeTypeOrFormat: string): string;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
begin
  Codec := GetCodec(AMimeTypeOrFormat);
  MS := TMemoryStream.Create;
  try
    Codec.EncodeModel(AModel, MS);
    MS.Position := 0;
    Result := StreamToRawBytes(MS);
  finally
    MS.Free;
  end;
end;

class procedure TDeltaCodecRegistry.DecodeModelFromString(const AContent: string; AModel: TDeltaModel; const AMimeTypeOrFormat: string);
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
begin
  if AModel = nil then Exit;
  Codec := GetCodec(AMimeTypeOrFormat);
  MS := TMemoryStream.Create;
  try
    RawBytesToStream(RawByteString(AContent), MS);
    MS.Position := 0;
    Codec.DecodeModel(MS, AModel);
  finally
    MS.Free;
  end;
end;

class function TDeltaCodecRegistry.EncodeListToString(AList: TObject; const AMimeTypeOrFormat: string): string;
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
begin
  Codec := GetCodec(AMimeTypeOrFormat);
  MS := TMemoryStream.Create;
  try
    Codec.EncodeList(AList, MS);
    MS.Position := 0;
    Result := StreamToRawBytes(MS);
  finally
    MS.Free;
  end;
end;

class procedure TDeltaCodecRegistry.DecodeListFromString(const AContent: string; AList: TObject; AModelClass: TClass; const AMimeTypeOrFormat: string);
var
  Codec: IDeltaCodec;
  MS: TMemoryStream;
begin
  if AList = nil then Exit;
  Codec := GetCodec(AMimeTypeOrFormat);
  MS := TMemoryStream.Create;
  try
    RawBytesToStream(RawByteString(AContent), MS);
    MS.Position := 0;
    Codec.DecodeList(MS, AList, AModelClass);
  finally
    MS.Free;
  end;
end;

{ TDeltaJsonCodec }

function TDeltaJsonCodec.GetFormatName: string;
begin
  Result := 'json';
end;

function TDeltaJsonCodec.GetMimeType: string;
begin
  Result := 'application/json';
end;

function TDeltaJsonCodec.CanHandleMimeType(const AMimeType: string): Boolean;
var
  M: string;
  P: Integer;
begin
  M := LowerCase(Trim(AMimeType));
  P := Pos(';', M);
  if P > 0 then
    M := Trim(Copy(M, 1, P - 1));

  Result := (M = 'application/json') or
            (M = 'text/json') or
            (M = 'json') or
            (M = 'application/problem+json') or
            (M = 'application/vnd.api+json');
end;

procedure TDeltaJsonCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
var
  Raw: RawByteString;
begin
  if (AModel = nil) or (AOutputStream = nil) then Exit;
  Raw := TDeltaFastJsonStreamer.ModelToJson(AModel);
  RawBytesToStream(Raw, AOutputStream);
end;

procedure TDeltaJsonCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  Raw: RawByteString;
begin
  if (AList = nil) or (AOutputStream = nil) then Exit;
  if AList is TCustomDeltaModelList then
    Raw := TDeltaFastJsonStreamer.ListToJson(TCustomDeltaModelList(AList))
  else
    Raw := '[]';

  RawBytesToStream(Raw, AOutputStream);
end;

procedure TDeltaJsonCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
var
  Raw: RawByteString;
begin
  if (AModel = nil) or (AInputStream = nil) then Exit;
  Raw := StreamToRawBytes(AInputStream);
  if Trim(string(Raw)) <> '' then
    AModel.FromJson(string(Raw));
end;

procedure TDeltaJsonCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
var
  Raw: RawByteString;
begin
  if (AList = nil) or (AInputStream = nil) then Exit;
  Raw := StreamToRawBytes(AInputStream);
  if Trim(string(Raw)) = '' then Exit;

  if AList is TDeltaModelList then
  begin
    if (TDeltaModelList(AList).DeltaModelClass = nil) and (AModelClass <> nil) and
       AModelClass.InheritsFrom(TDeltaModel) then
      TDeltaModelList(AList).DeltaModelClass := TDeltaModelClass(AModelClass);
    TDeltaModelList(AList).FromJson(string(Raw));
  end;
end;

initialization
  TDeltaCodecRegistry.RegisterCodec(TDeltaJsonCodec.Create);

end.
