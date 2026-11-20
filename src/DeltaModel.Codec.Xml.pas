unit DeltaModel.Codec.Xml;

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
  { TDeltaXmlCodec: Codec XML de Alta Performance (Zero-DOM Streamer)
    Contrato Universal de Serialização e Desserialização XML:
    - Declaração XML padrão UTF-8.
    - Modelo: <entity><field1>valor</field1>...</entity>
    - Lista: <items><item>...</item></items>
    - Tratamento de entidades XML (&amp;, &lt;, &gt;, &quot;, &apos;).
    - Suporte a tags auto-fechadas <field/> para valores nulos. }
  TDeltaXmlCodec = class(TInterfacedObject, IDeltaCodec)
  private
    function XmlEscape(const S: string): string;
    function XmlUnescape(const S: string): string;
    function FloatToXml(const V: Double): string;
    function XmlToFloatDef(const S: string; ADefault: Double): Double;
    function GetEntityTag(AModel: TDeltaModel): string;

    procedure WriteModelXml(AModel: TDeltaModel; AStream: TStream; const ATagName: string = '');
    procedure ParseXmlTags(P: PChar; out NextP: PChar; out TagName, Content: string; out IsClosing, IsSelfClosing: Boolean);
    procedure ReadModelXml(const AXml: string; AModel: TDeltaModel);
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

{ Funções Utilitárias }

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

{ TDeltaXmlCodec }

function TDeltaXmlCodec.GetFormatName: string;
begin
  Result := 'xml';
end;

function TDeltaXmlCodec.GetMimeType: string;
begin
  Result := 'application/xml';
end;

function TDeltaXmlCodec.CanHandleMimeType(const AMimeType: string): Boolean;
var
  M: string;
  P: Integer;
begin
  M := LowerCase(Trim(AMimeType));
  P := Pos(';', M);
  if P > 0 then
    M := Trim(Copy(M, 1, P - 1));

  Result := (M = 'application/xml') or
            (M = 'text/xml') or
            (M = 'xml');
end;

function TDeltaXmlCodec.XmlEscape(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
    else
      Result := Result + C;
    end;
  end;
end;

function TDeltaXmlCodec.XmlUnescape(const S: string): string;
begin
  Result := S;
  if Pos('&', Result) = 0 then Exit;
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function TDeltaXmlCodec.FloatToXml(const V: Double): string;
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

function TDeltaXmlCodec.XmlToFloatDef(const S: string; ADefault: Double): Double;
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

function TDeltaXmlCodec.GetEntityTag(AModel: TDeltaModel): string;
begin
  if AModel = nil then Exit('item');
  if AModel.TableName <> '' then
    Result := AModel.TableName
  else
  begin
    Result := AModel.ClassName;
    if (Length(Result) > 1) and (Result[1] in ['T', 't']) then
      Result := Copy(Result, 2, Length(Result) - 1);
  end;
  if Result = '' then Result := 'item';
end;

procedure TDeltaXmlCodec.WriteModelXml(AModel: TDeltaModel; AStream: TStream; const ATagName: string);
var
  RootTag: string;
  Meta: TModelMetadata;
  ClassProps: TRTTIClassInfo;
  I, J: Integer;
  FieldMeta: PDeltaFieldMeta;
  PropItem: PDeltaPropItem;
  PropObj, ChildObj: TObject;
  DeltaField: TDeltaField;
  Key, ValStr: string;
begin
  if AModel = nil then
  begin
    StringToStream('<null/>', AStream);
    Exit;
  end;

  if ATagName <> '' then
    RootTag := ATagName
  else
    RootTag := GetEntityTag(AModel);

  StringToStream('<' + RootTag + '>', AStream);

  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);
  ClassProps := TRTTICache.GetClassProps(AModel.ClassType);

  // 1. Campos mapeados (TDeltaField e primitivos)
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
          StringToStream('<' + Key + '>', AStream);
          WriteModelXml(TDeltaModel(TDFHasOne(DeltaField).Model), AStream, GetEntityTag(TDeltaModel(TDFHasOne(DeltaField).Model)));
          StringToStream('</' + Key + '>', AStream);
        end
        else
          StringToStream('<' + Key + ' xsi:nil="true"/>', AStream);
      end
      else if DeltaField is TDFHasMany then
      begin
        StringToStream('<' + Key + '>', AStream);
        for J := 0 to TDFHasMany(DeltaField).Count - 1 do
        begin
          ChildObj := TDFHasMany(DeltaField)[J];
          if ChildObj is TDeltaModel then
            WriteModelXml(TDeltaModel(ChildObj), AStream, 'item');
        end;
        StringToStream('</' + Key + '>', AStream);
      end
      else if DeltaField.IsNull then
      begin
        StringToStream('<' + Key + ' xsi:nil="true"/>', AStream);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            ValStr := IntToStr(DeltaField.AsLargeInt);

          dfkFloat:
            ValStr := FloatToXml(DeltaField.AsFloat);

          dfkBoolean:
            if DeltaField.AsBoolean then ValStr := 'true' else ValStr := 'false';

          dfkDateTime:
            ValStr := DeltaField.AsString;

          dfkString:
            ValStr := XmlEscape(DeltaField.AsString);
        else
          ValStr := XmlEscape(DeltaField.AsString);
        end;

        StringToStream('<' + Key + '>' + ValStr + '</' + Key + '>', AStream);
      end;
    end
    else
    begin
      // Propriedade Primitiva
      if AModel.IsNull(FieldMeta^.PropIndex) then
      begin
        StringToStream('<' + Key + ' xsi:nil="true"/>', AStream);
      end
      else
      begin
        case FieldMeta^.FieldKind of
          dfkInteger:
            ValStr := IntToStr(GetOrdProp(AModel, FieldMeta^.PropInfo));

          dfkFloat:
            ValStr := FloatToXml(GetFloatProp(AModel, FieldMeta^.PropInfo));

          dfkBoolean:
            if Boolean(GetOrdProp(AModel, FieldMeta^.PropInfo)) then ValStr := 'true' else ValStr := 'false';

          dfkDateTime:
            ValStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', GetFloatProp(AModel, FieldMeta^.PropInfo));

          dfkString:
            ValStr := XmlEscape(GetStrProp(AModel, FieldMeta^.PropInfo));
        end;

        StringToStream('<' + Key + '>' + ValStr + '</' + Key + '>', AStream);
      end;
    end;
  end;

  // 2. Propriedades publicadas adicionais
  for I := 0 to ClassProps.PropCount - 1 do
  begin
    PropItem := @ClassProps.Props[I];
    if PropItem^.IsDeltaField or (not PropItem^.HasGetProc) then Continue;
    if Meta.FindFieldByProp(PropItem^.Name) <> nil then Continue;

    Key := PropItem^.Name;
    case PropItem^.PropKind of
      tkInteger, tkInt64:
        ValStr := IntToStr(GetOrdProp(AModel, PropItem^.PropInfo));

      tkFloat:
        ValStr := FloatToXml(GetFloatProp(AModel, PropItem^.PropInfo));

      tkBool:
        if Boolean(GetOrdProp(AModel, PropItem^.PropInfo)) then ValStr := 'true' else ValStr := 'false';

      tkString, tkLString, tkAString, tkWString, tkUString, tkChar, tkWChar:
        ValStr := XmlEscape(GetStrProp(AModel, PropItem^.PropInfo));

      tkEnumeration:
        ValStr := XmlEscape(GetEnumName(PropItem^.PropInfo^.PropType, GetOrdProp(AModel, PropItem^.PropInfo)));
    else
      Continue;
    end;

    StringToStream('<' + Key + '>' + ValStr + '</' + Key + '>', AStream);
  end;

  StringToStream('</' + RootTag + '>', AStream);
end;

procedure TDeltaXmlCodec.ParseXmlTags(P: PChar; out NextP: PChar; out TagName, Content: string; out IsClosing, IsSelfClosing: Boolean);
var
  TagStart, ContentStart: PChar;
  InQuote: Char;
begin
  TagName := '';
  Content := '';
  IsClosing := False;
  IsSelfClosing := False;
  NextP := P;
  if (P = nil) or (P^ = #0) then Exit;

  // Localiza abertura '<'
  while (P^ <> #0) and (P^ <> '<') do Inc(P);
  if P^ = #0 then Exit;

  Inc(P); // Pula '<'

  // Pula comentários <!-- ... -->
  if (P[0] = '!') and (P[1] = '-') and (P[2] = '-') then
  begin
    Inc(P, 3);
    while (P^ <> #0) and not ((P[0] = '-') and (P[1] = '-') and (P[2] = '>')) do Inc(P);
    if P^ <> #0 then Inc(P, 3);
    NextP := P;
    Exit;
  end;

  // Pula declaração <?xml ... ?>
  if P^ = '?' then
  begin
    while (P^ <> #0) and not ((P[0] = '?') and (P[1] = '>')) do Inc(P);
    if P^ <> #0 then Inc(P, 2);
    NextP := P;
    Exit;
  end;

  // Tag de fechamento </tag>
  if P^ = '/' then
  begin
    IsClosing := True;
    Inc(P);
    TagStart := P;
    while (P^ <> #0) and not (P^ in [' ', '>', '/', #9, #10, #13]) do Inc(P);
    SetString(TagName, TagStart, P - TagStart);
    while (P^ <> #0) and (P^ <> '>') do Inc(P);
    if P^ = '>' then Inc(P);
    NextP := P;
    Exit;
  end;

  // Tag de abertura <tag ...>
  TagStart := P;
  while (P^ <> #0) and not (P^ in [' ', '>', '/', #9, #10, #13]) do Inc(P);
  SetString(TagName, TagStart, P - TagStart);

  // Consome atributos até '>' ou '/>'
  InQuote := #0;
  while P^ <> #0 do
  begin
    if InQuote <> #0 then
    begin
      if P^ = InQuote then InQuote := #0;
      Inc(P);
    end
    else
    begin
      if P^ in ['"', ''''] then
      begin
        InQuote := P^;
        Inc(P);
      end
      else if (P[0] = '/') and (P[1] = '>') then
      begin
        IsSelfClosing := True;
        Inc(P, 2);
        NextP := P;
        Exit;
      end
      else if P^ = '>' then
      begin
        Inc(P);
        Break;
      end
      else
        Inc(P);
    end;
  end;

  // Lê texto de conteúdo até o próximo '<'
  ContentStart := P;
  while (P^ <> #0) and (P^ <> '<') do Inc(P);
  SetString(Content, ContentStart, P - ContentStart);
  Content := XmlUnescape(Trim(Content));
  NextP := P;
end;

procedure TDeltaXmlCodec.ReadModelXml(const AXml: string; AModel: TDeltaModel);
var
  P, NextP: PChar;
  TagName, Content: string;
  IsClosing, IsSelfClosing: Boolean;
  Meta: TModelMetadata;
  FMeta: PDeltaFieldMeta;
  NestedObj: TObject;
begin
  if (AModel = nil) or (Length(AXml) = 0) then Exit;
  Meta := TRTTICache.GetModelMetadata(AModel.ClassType);

  P := PChar(AXml);
  while (P <> nil) and (P^ <> #0) do
  begin
    ParseXmlTags(P, NextP, TagName, Content, IsClosing, IsSelfClosing);
    if P = NextP then Break;
    P := NextP;

    if (TagName = '') or IsClosing then Continue;

    FMeta := Meta.FindFieldByProp(TagName);
    if FMeta = nil then
      FMeta := Meta.FindField(TagName);

    if FMeta <> nil then
    begin
      if IsSelfClosing or (Content = '') then
      begin
        if FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDeltaField then
          begin
            if (FMeta^.FieldKind = dfkString) and not IsSelfClosing then
              TDeltaField(NestedObj).SetAsString('')
            else
              TDeltaField(NestedObj).Clear;
          end;
        end
        else
        begin
          if (FMeta^.FieldKind = dfkString) and not IsSelfClosing then
          begin
            SetStrProp(AModel, FMeta^.PropInfo, '');
            AModel.ClearNull(FMeta^.PropIndex);
          end
          else
            AModel.SetNull(FMeta^.PropIndex);
        end;
      end
      else
      begin
        if FMeta^.IsDeltaField then
        begin
          NestedObj := GetObjectProp(AModel, FMeta^.PropInfo);
          if NestedObj is TDeltaField then
          begin
            case FMeta^.FieldKind of
              dfkInteger:
                TDeltaField(NestedObj).AsInt64 := StrToInt64Def(Content, 0);

              dfkFloat:
                TDeltaField(NestedObj).AsFloat := XmlToFloatDef(Content, 0.0);

              dfkBoolean:
                TDeltaField(NestedObj).AsBoolean := SameText(Content, 'true') or (Content = '1');

              dfkDateTime:
                TDeltaField(NestedObj).SetAsString(Content);

              dfkString:
                TDeltaField(NestedObj).SetAsString(Content);
            else
              TDeltaField(NestedObj).SetAsString(Content);
            end;
          end;
        end
        else
        begin
          case FMeta^.FieldKind of
            dfkInteger:
              SetOrdProp(AModel, FMeta^.PropInfo, StrToInt64Def(Content, 0));

            dfkFloat:
              SetFloatProp(AModel, FMeta^.PropInfo, XmlToFloatDef(Content, 0.0));

            dfkBoolean:
              SetOrdProp(AModel, FMeta^.PropInfo, Ord(SameText(Content, 'true') or (Content = '1')));

            dfkDateTime:
              SetFloatProp(AModel, FMeta^.PropInfo, ISO8601ToDateDef(Content, 0.0));

            dfkString:
              SetStrProp(AModel, FMeta^.PropInfo, Content);
          end;
          AModel.ClearNull(FMeta^.PropIndex);
        end;
      end;
    end;
  end;
end;

procedure TDeltaXmlCodec.EncodeModel(AModel: TDeltaModel; AOutputStream: TStream);
begin
  if (AModel = nil) or (AOutputStream = nil) then Exit;
  StringToStream('<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak, AOutputStream);
  WriteModelXml(AModel, AOutputStream);
end;

procedure TDeltaXmlCodec.EncodeList(AList: TObject; AOutputStream: TStream);
var
  CustomList: TCustomDeltaModelList;
  I, Count: Integer;
  ItemObj: TObject;
begin
  if (AList = nil) or (AOutputStream = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  StringToStream('<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak, AOutputStream);
  StringToStream(Format('<items total_records="%d" page="%d" page_size="%d">' + sLineBreak,
    [CustomList.total_records, CustomList.page, CustomList.page_size]), AOutputStream);

  Count := CustomList.Count;
  for I := 0 to Count - 1 do
  begin
    ItemObj := CustomList.GetItemObj(I);
    if ItemObj is TDeltaModel then
    begin
      WriteModelXml(TDeltaModel(ItemObj), AOutputStream, 'item');
      StringToStream(sLineBreak, AOutputStream);
    end;
  end;

  StringToStream('</items>' + sLineBreak, AOutputStream);
end;

procedure TDeltaXmlCodec.DecodeModel(AInputStream: TStream; AModel: TDeltaModel);
var
  XmlContent: string;
begin
  if (AInputStream = nil) or (AModel = nil) then Exit;
  XmlContent := StreamToRawString(AInputStream);
  if Trim(XmlContent) = '' then Exit;
  ReadModelXml(XmlContent, AModel);
end;

procedure TDeltaXmlCodec.DecodeList(AInputStream: TStream; AList: TObject; AModelClass: TClass);
var
  XmlContent: string;
  CustomList: TCustomDeltaModelList;
  P, NextP, ItemStart: PChar;
  TagName, Content, ItemXml: string;
  IsClosing, IsSelfClosing: Boolean;
  NewObj: TObject;
  InsideItem: Boolean;
  ItemTag: string;
begin
  if (AInputStream = nil) or (AList = nil) then Exit;
  if not (AList is TCustomDeltaModelList) then Exit;
  CustomList := TCustomDeltaModelList(AList);

  if (CustomList.GetModelClass = nil) and (AModelClass <> nil) and
     (AList is TDeltaModelList) and AModelClass.InheritsFrom(TDeltaModel) then
    TDeltaModelList(AList).DeltaModelClass := TDeltaModelClass(AModelClass);

  XmlContent := StreamToRawString(AInputStream);
  if Trim(XmlContent) = '' then Exit;

  P := PChar(XmlContent);
  InsideItem := False;
  ItemStart := nil;
  ItemTag := '';

  while (P <> nil) and (P^ <> #0) do
  begin
    if not InsideItem then
    begin
      ItemStart := P;
      ParseXmlTags(P, NextP, TagName, Content, IsClosing, IsSelfClosing);
      if P = NextP then Break;
      P := NextP;

      // Detecta abertura de um item de dados (ex: <item>, <produto>, ou entidade filha de <items>)
      if not IsClosing and (TagName <> '') and not SameText(TagName, 'items') and
         not SameText(TagName, 'list') and not SameText(TagName, 'root') then
      begin
        InsideItem := True;
        ItemTag := TagName;
      end;
    end
    else
    begin
      ParseXmlTags(P, NextP, TagName, Content, IsClosing, IsSelfClosing);
      if P = NextP then Break;
      P := NextP;

      // Detecta fechamento do item atual
      if IsClosing and SameText(TagName, ItemTag) then
      begin
        SetString(ItemXml, ItemStart, P - ItemStart);
        InsideItem := False;

        NewObj := nil;
        if (AModelClass <> nil) and AModelClass.InheritsFrom(TDeltaModel) then
          NewObj := TDeltaModelClass(AModelClass).Create
        else
          NewObj := CustomList.NewItem;

        if NewObj is TDeltaModel then
        begin
          ReadModelXml(ItemXml, TDeltaModel(NewObj));
          CustomList.AddObj(NewObj);
        end
        else
          NewObj.Free;
      end;
    end;
  end;
end;

initialization
  TDeltaCodecRegistry.RegisterCodec(TDeltaXmlCodec.Create);

end.
