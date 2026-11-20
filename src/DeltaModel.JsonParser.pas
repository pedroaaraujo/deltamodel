unit DeltaModel.JsonParser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.List, DeltaModel.Metadata;

type
  { TDeltaFastJsonParser: Parser JSON de passagem única sem construção de árvore DOM,
    populando diretamente instâncias de TDeltaModel e coleções TCustomDeltaModelList. }
  TDeltaFastJsonParser = class
  private
    FJson: RawByteString;
    FPtr: PAnsiChar;
    FEnd: PAnsiChar;

    procedure SkipWhitespace; inline;
    function ReadString(out AStr: string): Boolean;
    function ReadNumber(out AIsFloat: Boolean; out AIntVal: Int64; out AFloatVal: Double): Boolean;
    function ReadBoolean(out AVal: Boolean): Boolean;
    function IsNullToken: Boolean;
    procedure SkipValue;

    procedure ParseObject(AModel: TDeltaModel; AMeta: TModelMetadata);
  public
    constructor Create(const AJson: RawByteString);
    procedure ParseIntoModel(AModel: TDeltaModel);
    procedure ParseIntoList(AList: TCustomDeltaModelList);

    class procedure DeserializeModel(AModel: TDeltaModel; const AJson: RawByteString);
    class procedure DeserializeList(AList: TCustomDeltaModelList; const AJson: RawByteString);
  end;

implementation

{ TDeltaFastJsonParser }

constructor TDeltaFastJsonParser.Create(const AJson: RawByteString);
begin
  inherited Create;
  FJson := AJson;
  FPtr := PAnsiChar(FJson);
  FEnd := FPtr + Length(FJson);
end;

procedure TDeltaFastJsonParser.SkipWhitespace;
begin
  while (FPtr < FEnd) and (FPtr^ in [#9, #10, #13, ' ']) do
    Inc(FPtr);
end;

function TDeltaFastJsonParser.ReadString(out AStr: string): Boolean;
var
  StartP: PAnsiChar;
  C: AnsiChar;
  Res: string;
begin
  AStr := '';
  if (FPtr >= FEnd) or (FPtr^ <> '"') then
    Exit(False);

  Inc(FPtr); // pula aspas de abertura
  StartP := FPtr;
  Res := '';

  while (FPtr < FEnd) and (FPtr^ <> '"') do
  begin
    if FPtr^ = '\' then
    begin
      // Copia o trecho acumulado antes do escape
      if FPtr > StartP then
        Res := Res + Copy(string(StartP), 1, FPtr - StartP);
      Inc(FPtr);
      if FPtr >= FEnd then Break;
      case FPtr^ of
        '"': Res := Res + '"';
        '\': Res := Res + '\';
        '/': Res := Res + '/';
        'b': Res := Res + #8;
        'f': Res := Res + #12;
        'n': Res := Res + #10;
        'r': Res := Res + #13;
        't': Res := Res + #9;
        'u':
        begin
          // Unicode \uXXXX simples
          Inc(FPtr, 4); // consome 4 hex dígitos
          Res := Res + '?'; // representação segura básica
        end;
      else
        Res := Res + FPtr^;
      end;
      Inc(FPtr);
      StartP := FPtr;
    end
    else
      Inc(FPtr);
  end;

  if FPtr > StartP then
    Res := Res + Copy(string(StartP), 1, FPtr - StartP);

  if (FPtr < FEnd) and (FPtr^ = '"') then
    Inc(FPtr); // consome aspas de fechamento

  AStr := Res;
  Result := True;
end;

function TDeltaFastJsonParser.ReadNumber(out AIsFloat: Boolean; out AIntVal: Int64; out AFloatVal: Double): Boolean;
var
  StartP: PAnsiChar;
  NumStr: string;
  FS: TFormatSettings;
begin
  AIsFloat := False;
  AIntVal := 0;
  AFloatVal := 0.0;
  StartP := FPtr;

  if (FPtr < FEnd) and (FPtr^ = '-') then
    Inc(FPtr);

  while (FPtr < FEnd) and (FPtr^ in ['0'..'9']) do
    Inc(FPtr);

  if (FPtr < FEnd) and (FPtr^ = '.') then
  begin
    AIsFloat := True;
    Inc(FPtr);
    while (FPtr < FEnd) and (FPtr^ in ['0'..'9']) do
      Inc(FPtr);
  end;

  if (FPtr < FEnd) and (FPtr^ in ['e', 'E']) then
  begin
    AIsFloat := True;
    Inc(FPtr);
    if (FPtr < FEnd) and (FPtr^ in ['+', '-']) then
      Inc(FPtr);
    while (FPtr < FEnd) and (FPtr^ in ['0'..'9']) do
      Inc(FPtr);
  end;

  SetString(NumStr, StartP, FPtr - StartP);
  if NumStr.IsEmpty then
    Exit(False);

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  if AIsFloat then
    Result := TryStrToFloat(NumStr, AFloatVal, FS)
  else
  begin
    Result := TryStrToInt64(NumStr, AIntVal);
    if not Result then
    begin
      AIsFloat := True;
      Result := TryStrToFloat(NumStr, AFloatVal, FS);
    end;
  end;
end;

function TDeltaFastJsonParser.ReadBoolean(out AVal: Boolean): Boolean;
begin
  AVal := False;
  if (FEnd - FPtr >= 4) and (FPtr[0] = 't') and (FPtr[1] = 'r') and (FPtr[2] = 'u') and (FPtr[3] = 'e') then
  begin
    AVal := True;
    Inc(FPtr, 4);
    Exit(True);
  end;

  if (FEnd - FPtr >= 5) and (FPtr[0] = 'f') and (FPtr[1] = 'a') and (FPtr[2] = 'l') and (FPtr[3] = 's') and (FPtr[4] = 'e') then
  begin
    AVal := False;
    Inc(FPtr, 5);
    Exit(True);
  end;

  Result := False;
end;

function TDeltaFastJsonParser.IsNullToken: Boolean;
begin
  if (FEnd - FPtr >= 4) and (FPtr[0] = 'n') and (FPtr[1] = 'u') and (FPtr[2] = 'l') and (FPtr[3] = 'l') then
  begin
    Inc(FPtr, 4);
    Exit(True);
  end;
  Result := False;
end;

procedure TDeltaFastJsonParser.SkipValue;
var
  Depth: Integer;
begin
  SkipWhitespace;
  if FPtr >= FEnd then Exit;

  if FPtr^ = '"' then
  begin
    Inc(FPtr);
    while (FPtr < FEnd) and (FPtr^ <> '"') do
    begin
      if FPtr^ = '\' then Inc(FPtr);
      if FPtr < FEnd then Inc(FPtr);
    end;
    if (FPtr < FEnd) and (FPtr^ = '"') then Inc(FPtr);
  end
  else if FPtr^ in ['{', '['] then
  begin
    Depth := 0;
    while FPtr < FEnd do
    begin
      if FPtr^ in ['{', '['] then Inc(Depth)
      else if FPtr^ in ['}', ']'] then
      begin
        Dec(Depth);
        if Depth = 0 then
        begin
          Inc(FPtr);
          Break;
        end;
      end
      else if FPtr^ = '"' then
      begin
        Inc(FPtr);
        while (FPtr < FEnd) and (FPtr^ <> '"') do
        begin
          if FPtr^ = '\' then Inc(FPtr);
          if FPtr < FEnd then Inc(FPtr);
        end;
      end;
      Inc(FPtr);
    end;
  end
  else
  begin
    while (FPtr < FEnd) and not (FPtr^ in [',', '}', ']', #9, #10, #13, ' ']) do
      Inc(FPtr);
  end;
end;

procedure TDeltaFastJsonParser.ParseObject(AModel: TDeltaModel; AMeta: TModelMetadata);
var
  KeyName: string;
  FMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  NestedObj, ChildObj: TObject;
  StrVal: string;
  IsFloat: Boolean;
  IntVal: Int64;
  FloatVal: Double;
  BoolVal: Boolean;
begin
  SkipWhitespace;
  if (FPtr >= FEnd) or (FPtr^ <> '{') then Exit;
  Inc(FPtr); // consome '{'

  while FPtr < FEnd do
  begin
    SkipWhitespace;
    if (FPtr < FEnd) and (FPtr^ = '}') then
    begin
      Inc(FPtr);
      Break;
    end;

    // Lê a chave (nome da propriedade)
    if not ReadString(KeyName) then Break;

    SkipWhitespace;
    if (FPtr < FEnd) and (FPtr^ = ':') then
      Inc(FPtr); // consome ':'
    SkipWhitespace;

    FMeta := AMeta.FindFieldByProp(KeyName);
    if FMeta = nil then
      FMeta := AMeta.FindField(KeyName);

    if IsNullToken then
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
    end
    else if FPtr^ = '"' then
    begin
      ReadString(StrVal);
      if FMeta <> nil then
      begin
        if FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDeltaField then
            TDeltaField(NestedObj).SetAsString(StrVal);
        end
        else
        begin
          SetStrProp(AModel, FMeta^.PropInfo, StrVal);
          AModel.ClearNull(FMeta^.PropIndex);
        end;
      end;
    end
    else if FPtr^ in ['t', 'f'] then
    begin
      ReadBoolean(BoolVal);
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
    end
    else if FPtr^ in ['-', '0'..'9'] then
    begin
      ReadNumber(IsFloat, IntVal, FloatVal);
      if FMeta <> nil then
      begin
        if FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDeltaField then
          begin
            if IsFloat then
              TDeltaField(NestedObj).AsFloat := FloatVal
            else
              TDeltaField(NestedObj).AsInt64 := IntVal;
          end;
        end
        else
        begin
          if IsFloat then
            SetFloatProp(AModel, FMeta^.PropInfo, FloatVal)
          else
            SetOrdProp(AModel, FMeta^.PropInfo, IntVal);
          AModel.ClearNull(FMeta^.PropIndex);
        end;
      end;
    end
    else if FPtr^ = '{' then
    begin
      // Objeto aninhado (ex: HasOne)
      if (FMeta <> nil) and FMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
        if (NestedObj is TDFHasOne) then
        begin
          if (TDFHasOne(NestedObj).Model = nil) and (TDFHasOne(NestedObj).RelationClass <> nil) then
            TDFHasOne(NestedObj).Model := TDeltaModelClass(TDFHasOne(NestedObj).RelationClass).Create;
          if TDFHasOne(NestedObj).Model is TDeltaModel then
            ParseObject(TDeltaModel(TDFHasOne(NestedObj).Model), TRTTICache.GetModelMetadata(TDFHasOne(NestedObj).Model.ClassType))
          else
            SkipValue;
        end
        else
          SkipValue;
      end
      else
        SkipValue;
    end
    else if FPtr^ = '[' then
    begin
      // Array aninhado (ex: HasMany)
      if (FMeta <> nil) and FMeta^.IsDeltaField then
      begin
        NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
        if NestedObj is TDFHasMany then
        begin
          TDFHasMany(NestedObj).Clear;
          Inc(FPtr); // pula '['
          SkipWhitespace;
          while (FPtr < FEnd) and (FPtr^ <> ']') do
          begin
            if FPtr^ = '{' then
            begin
              ChildObj := nil;
              if TDFHasMany(NestedObj).RelationClass <> nil then
                ChildObj := TDeltaModelClass(TDFHasMany(NestedObj).RelationClass).Create;
              if ChildObj is TDeltaModel then
              begin
                ParseObject(TDeltaModel(ChildObj), TRTTICache.GetModelMetadata(ChildObj.ClassType));
                TDFHasMany(NestedObj).Add(ChildObj);
              end
              else
                SkipValue;
            end
            else
              SkipValue;
            SkipWhitespace;
            if (FPtr < FEnd) and (FPtr^ = ',') then
              Inc(FPtr);
            SkipWhitespace;
          end;
          if (FPtr < FEnd) and (FPtr^ = ']') then
            Inc(FPtr);
        end
        else
          SkipValue;
      end
      else
        SkipValue;
    end
    else
      SkipValue;

    SkipWhitespace;
    if (FPtr < FEnd) and (FPtr^ = ',') then
      Inc(FPtr);
  end;
end;

procedure TDeltaFastJsonParser.ParseIntoModel(AModel: TDeltaModel);
var
  Meta: TModelMetadata;
begin
  if AModel = nil then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ParseObject(AModel, Meta);
end;

procedure TDeltaFastJsonParser.ParseIntoList(AList: TCustomDeltaModelList);
var
  NewObj: TObject;
  Meta: TModelMetadata;
  ItemClass: TClass;
  Key: string;
  IsFloat: Boolean;
  IntVal: Int64;
  FloatVal: Double;
begin
  if AList = nil then Exit;
  ItemClass := AList.GetModelClass;
  if ItemClass = nil then Exit;

  Meta := TRTTICache.GetModelMetadata(ItemClass);

  SkipWhitespace;
  if FPtr >= FEnd then Exit;

  // 1. Suporta envelope paginado: {"items": [...], "page": 1, "page_size": 10, ...}
  if FPtr^ = '{' then
  begin
    Inc(FPtr); // consome '{'
    while FPtr < FEnd do
    begin
      SkipWhitespace;
      if (FPtr < FEnd) and (FPtr^ = '}') then
      begin
        Inc(FPtr);
        Break;
      end;

      if not ReadString(Key) then Break;
      SkipWhitespace;
      if (FPtr < FEnd) and (FPtr^ = ':') then Inc(FPtr);
      SkipWhitespace;

      if (SameText(Key, 'items') or SameText(Key, 'data')) and (FPtr < FEnd) and (FPtr^ = '[') then
      begin
        Inc(FPtr); // consome '['
        while FPtr < FEnd do
        begin
          SkipWhitespace;
          if (FPtr < FEnd) and (FPtr^ = ']') then
          begin
            Inc(FPtr);
            Break;
          end;

          if FPtr^ = '{' then
          begin
            NewObj := AList.NewItem;
            if NewObj is TDeltaModel then
            begin
              ParseObject(TDeltaModel(NewObj), Meta);
              AList.AddObj(NewObj);
            end
            else
              NewObj.Free;
          end
          else
            SkipValue;

          SkipWhitespace;
          if (FPtr < FEnd) and (FPtr^ = ',') then
            Inc(FPtr);
        end;
      end
      else if SameText(Key, 'page') then
      begin
        if ReadNumber(IsFloat, IntVal, FloatVal) then
          AList.page := IntVal;
      end
      else if SameText(Key, 'page_size') then
      begin
        if ReadNumber(IsFloat, IntVal, FloatVal) then
          AList.page_size := IntVal;
      end
      else if SameText(Key, 'total_records') then
      begin
        if ReadNumber(IsFloat, IntVal, FloatVal) then
          AList.total_records := IntVal;
      end
      else if SameText(Key, 'total_pages') then
      begin
        if ReadNumber(IsFloat, IntVal, FloatVal) then
          AList.total_pages := IntVal;
      end
      else
        SkipValue;

      SkipWhitespace;
      if (FPtr < FEnd) and (FPtr^ = ',') then
        Inc(FPtr);
    end;
    Exit;
  end;

  // 2. Suporta array direto: [ {...}, {...} ]
  if FPtr^ = '[' then
  begin
    Inc(FPtr); // consome '['
    while FPtr < FEnd do
    begin
      SkipWhitespace;
      if (FPtr < FEnd) and (FPtr^ = ']') then
      begin
        Inc(FPtr);
        Break;
      end;

      if FPtr^ = '{' then
      begin
        NewObj := AList.NewItem;
        if NewObj is TDeltaModel then
        begin
          ParseObject(TDeltaModel(NewObj), Meta);
          AList.AddObj(NewObj);
        end
        else
          NewObj.Free;
      end
      else
        SkipValue;

      SkipWhitespace;
      if (FPtr < FEnd) and (FPtr^ = ',') then
        Inc(FPtr);
    end;
  end;
end;

class procedure TDeltaFastJsonParser.DeserializeModel(AModel: TDeltaModel; const AJson: RawByteString);
var
  Parser: TDeltaFastJsonParser;
begin
  if Length(AJson) = 0 then Exit;
  Parser := TDeltaFastJsonParser.Create(AJson);
  try
    Parser.ParseIntoModel(AModel);
  finally
    Parser.Free;
  end;
end;

class procedure TDeltaFastJsonParser.DeserializeList(AList: TCustomDeltaModelList; const AJson: RawByteString);
var
  Parser: TDeltaFastJsonParser;
begin
  if Length(AJson) = 0 then Exit;
  Parser := TDeltaFastJsonParser.Create(AJson);
  try
    Parser.ParseIntoList(AList);
  finally
    Parser.Free;
  end;
end;

end.
