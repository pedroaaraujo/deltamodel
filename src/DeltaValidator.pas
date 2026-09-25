unit DeltaValidator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, Variants, RegExpr, fpjson,
  DeltaModelMessages;

type
  EDeltaValidation = class(Exception);

  TValid = record
    OK: Boolean;
    Message: UTF8String;
  end;

  IDeltaValidatorItem = interface ['{B89C6AA3-6762-4E24-8217-9240F162702F}']
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorField }

  TValidatorField = class(specialize TFPGInterfacedObjectList<IDeltaValidatorItem>)
  private
    FName: string;
    FValue: Variant;
  public
    property Name: string read FName write FName;
    property Value: Variant read FValue write FValue;
    function AddValidator(Item: IDeltaValidatorItem): TValidatorField;
    constructor Create(_Name: string; _Value: Variant);
    function Validate: TValid;
    function ValidateToJson: TValid;
  end;

  // Alias TDeltaField removido para evitar shadowing com DeltaModel.Fields.TDeltaField
  // TDeltaField = TValidatorField;

  { TValidator }

  TValidator = class
  private
    FFields: specialize TFPGObjectList<TValidatorField>;
  public
    property Fields: specialize TFPGObjectList<TValidatorField> read FFields;
    function AddField(Name: string; Value: Variant): TValidatorField;
    function Validate: TValid;
    function ValidateToJson: TValid;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  { Text Validations }

  TValidatorItemNotEmpty = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMinLength = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMinLength: Integer;
  public
    constructor Create(MinLength: Integer);
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMaxLength = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMaxLength: Integer;
  public
    constructor Create(MaxLength: Integer);
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemRegex = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FPattern: string;
  public
    constructor Create(Pattern: string);
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorEmail }

  TValidatorEmail = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemEmail = TValidatorEmail;

  { TValidatorItemUrl - Valida URL com http/https }

  TValidatorItemUrl = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorItemCPF - Valida CPF brasileiro (apenas dígitos, sem pontuação) }

  TValidatorItemCPF = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorItemCNPJ - Valida CNPJ brasileiro (apenas dígitos, sem pontuação) }

  TValidatorItemCNPJ = class(TInterfacedObject, IDeltaValidatorItem)
  public
    function Validate(Value: Variant): TValid;
  end;

  { Numeric Validations }

  TValidatorItemMinValue = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMinValue: Double;
  public
    constructor Create(MinValue: Double);
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorItemGreaterThanZero }

  TValidatorItemGreaterThanZero = class(TInterfacedObject, IDeltaValidatorItem)
  public
    constructor Create;
    function Validate(Value: Variant): TValid;
  end;

  TValidatorItemMaxValue = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMaxValue: Double;
  public
    constructor Create(MaxValue: Double);
    function Validate(Value: Variant): TValid;
  end;

  { TValidatorItemBetween - Valida se o valor está entre Min e Max (inclusivo) }

  TValidatorItemBetween = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FMin: Double;
    FMax: Double;
  public
    constructor Create(AMin, AMax: Double);
    function Validate(Value: Variant): TValid;
  end;

  { Date Validations }

  TValidatorItemPeriod = class(TInterfacedObject, IDeltaValidatorItem)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  public
    constructor Create(StartDate, EndDate: TDateTime);
    function Validate(Value: Variant): TValid;
  end;

function IsValidEmail(const AEmail: string): Boolean;
function IsValidCnpj(const ACnpj: string): Boolean;

implementation

{ Helpers internos }

function OnlyDigits(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Result := Result + S[I];
end;

function ValidateCPFDigits(const CPF: string): Boolean;
var
  Sum, Remainder, I: Integer;
begin
  Result := False;
  if Length(CPF) <> 11 then Exit;

  // Rejeita sequências com todos os dígitos iguais
  if (CPF = StringOfChar(CPF[1], 11)) then Exit;

  // Primeiro dígito verificador
  Sum := 0;
  for I := 1 to 9 do
    Sum := Sum + StrToInt(CPF[I]) * (11 - I);
  Remainder := (Sum * 10) mod 11;
  if Remainder = 10 then Remainder := 0;
  if Remainder <> StrToInt(CPF[10]) then Exit;

  // Segundo dígito verificador
  Sum := 0;
  for I := 1 to 10 do
    Sum := Sum + StrToInt(CPF[I]) * (12 - I);
  Remainder := (Sum * 10) mod 11;
  if Remainder = 10 then Remainder := 0;
  if Remainder <> StrToInt(CPF[11]) then Exit;

  Result := True;
end;

function IsValidCnpj(const ACnpj: string): Boolean;

  function OnlyAlphaNumbers(const S: string): string;
  var
    I: Integer;
    C: Char;
  begin
    Result := '';
    for I := 1 to Length(S) do
    begin
      C := UpCase(S[I]);
      if CharInSet(C, ['0'..'9', 'A'..'Z']) then
        Result := Result + C;
    end;
  end;

const
  Multiplier1: array[1..12] of Integer = (5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2);
  Multiplier2: array[1..13] of Integer = (6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2);
  BaseSub: Integer = Ord('0');
var
  Cnpj: string;
  I, Sum, Remainder, Digit1, Digit2: Integer;
  AllEqual: Boolean;
begin
  Cnpj := OnlyAlphaNumbers(ACnpj);

  if Length(Cnpj) <> 14 then
    Exit(False);

  // Verifica se todos os dígitos são iguais
  AllEqual := True;
  for I := 2 to 14 do
    if Cnpj[I] <> Cnpj[1] then
    begin
      AllEqual := False;
      Break;
    end;
  if AllEqual then
    Exit(False);

  // Cálculo do 1º digito verificador
  Sum := 0;
  for I := 1 to 12 do
    Sum := Sum + (Ord(Cnpj[I]) - BaseSub) * Multiplier1[I];
  Remainder := Sum mod 11;
  if Remainder < 2 then
    Digit1 := 0
  else
    Digit1 := 11 - Remainder;

  // Cálculo do 2º digito verificador
  Sum := 0;
  for I := 1 to 13 do
    Sum := Sum + (Ord(Cnpj[I]) - BaseSub) * Multiplier2[I];
  Remainder := Sum mod 11;
  if Remainder < 2 then
    Digit2 := 0
  else
    Digit2 := 11 - Remainder;

  Result := (Ord(Cnpj[13]) - BaseSub = Digit1) and
            (Ord(Cnpj[14]) - BaseSub = Digit2);
end;

function ValidateCNPJDigits(const CNPJ: string): Boolean;
begin
  Result := IsValidCnpj(CNPJ);
end;

{ TValidatorField }

function TValidatorField.AddValidator(Item: IDeltaValidatorItem): TValidatorField;
begin
  Self.Add(Item);
  Result := Self;
end;

constructor TValidatorField.Create(_Name: string; _Value: Variant);
begin
  inherited Create;
  FName := _Name;
  FValue := _Value;
end;

function TValidatorField.Validate: TValid;
var
  i: Integer;
  ValidationResult: TValid;
begin
  Result.OK := True;
  Result.Message := '';

  for i := 0 to Count - 1 do
  begin
    ValidationResult := Items[i].Validate(FValue);
    if not ValidationResult.OK then
    begin
      Result := ValidationResult;
      Exit;
    end;
  end;
end;

function TValidatorField.ValidateToJson: TValid;
var
  Json: TJSONArray;
  I: Integer;
  ValidationResult: TValid;
begin
  Json := TJSONArray.Create();
  try
    for I := 0 to Count - 1 do
    begin
      ValidationResult := Items[i].Validate(FValue);
      if not ValidationResult.OK then
        Json.Add(ValidationResult.Message);
    end;

    Result.Message := Json.AsJSON;
    Result.OK := Json.Count = 0;
  finally
    Json.Free;
  end;
end;

{ TValidator }

function TValidator.AddField(Name: string; Value: Variant): TValidatorField;
var
  Field: TValidatorField;
begin
  Field := TValidatorField.Create(Name, Value);
  FFields.Add(Field);
  Result := Field;
end;

function TValidator.Validate: TValid;
var
  i: Integer;
  ValidationResult: TValid;
begin
  Result.OK := True;
  Result.Message := '';

  for i := 0 to FFields.Count - 1 do
  begin
    ValidationResult := FFields[i].Validate;
    if not ValidationResult.OK then
    begin
      Result.Ok := False;
      Result.Message := Format(
        ValidationFailedForField,
        [FFields[I].Name, ValidationResult.Message]
      );
      Exit;
    end;
  end;
end;

function TValidator.ValidateToJson: TValid;
var
  I: Integer;
  ValidationResult: TValid;
  JsonItem: TJSONObject;
  JsonArr: TJSONArray;
begin
  JsonArr := TJSONArray.Create();
  try
    for I := 0 to FFields.Count - 1 do
    begin
      ValidationResult := FFields[I].ValidateToJson;
      if not ValidationResult.OK then
      begin
        JsonItem := TJSONObject.Create;
        JsonItem.Add('field', FFields[I].Name);
        JsonItem.Add('issues', GetJson(ValidationResult.Message, False));
        JsonArr.Add(JsonItem)
      end;
    end;

    Result.Message := JsonArr.AsJSON;
    Result.OK := JsonArr.Count = 0;
  finally
    JsonArr.Free;
  end;
end;

procedure TValidator.Clear;
begin
  FFields.Clear;
end;

constructor TValidator.Create;
begin
  FFields := specialize TFPGObjectList<TValidatorField>.Create(True);
end;

destructor TValidator.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

{ Text Validations }

function TValidatorItemNotEmpty.Validate(Value: Variant): TValid;
begin
  Result.OK := not VarIsEmpty(Value) and (VarToStr(Value) <> '');
  if not Result.OK then
    Result.Message := ValueCannotBeEmpty;
end;

function UTF8CharLength(const S: string): Integer;
var
  P, PEnd: PChar;
begin
  Result := 0;
  if S = '' then Exit;
  P := PChar(S);
  PEnd := P + Length(S);
  while P < PEnd do
  begin
    if (Byte(P^) and $C0) <> $80 then
      Inc(Result);
    Inc(P);
  end;
end;

constructor TValidatorItemMinLength.Create(MinLength: Integer);
begin
  FMinLength := MinLength;
end;

function TValidatorItemMinLength.Validate(Value: Variant): TValid;
var
  Str: string;
  ActualLenght: Integer;
begin
  Str := VarToStr(Value);
  ActualLenght := UTF8CharLength(Str);

  Result.OK := ActualLenght >= FMinLength;
  if not Result.OK then
    Result.Message := Format(MinimumLenght, [FMinLength]);
end;

constructor TValidatorItemMaxLength.Create(MaxLength: Integer);
begin
  FMaxLength := MaxLength;
end;

function TValidatorItemMaxLength.Validate(Value: Variant): TValid;
var
  Str: string;
  ActualLenght: Integer;
begin
  Str := VarToStr(Value);
  ActualLenght := UTF8CharLength(Str);

  Result.OK := ActualLenght <= FMaxLength;
  if not Result.OK then
    Result.Message := Format(MaximumLenght, [FMaxLength]);
end;

constructor TValidatorItemRegex.Create(Pattern: string);
begin
  FPattern := Pattern;
end;

function TValidatorItemRegex.Validate(Value: Variant): TValid;
var
  RegEx: TRegExpr;
begin
  RegEx := TRegExpr.Create(FPattern);
  try
    Result.OK := RegEx.Exec(VarToStr(Value));
    if not Result.OK then
      Result.Message := ValueDoesNotMatchREGEX;
  finally
    RegEx.Free;
  end;
end;

const
  EmailPattern =
    '^[a-zA-Z0-9_%+\-]+(\.[a-zA-Z0-9_%+\-]+)*@([a-zA-Z0-9\-]+\.)+[a-zA-Z]{2,}$';

function IsValidEmail(const AEmail: string): Boolean;
var
  S: string;
  Regex: TRegExpr;
  AtPos, DotPos, I, Len: Integer;
  LocalPart, DomainPart: string;
begin
  Result := False;
  S := Trim(AEmail);
  Len := Length(S);
  if Len < 6 then Exit; // Mínimo: a@b.co

  // 1. Validação via Regex com hífens devidamente escapados (\-) para total compatibilidade x86_64, AArch64 e ARM
  try
    Regex := TRegExpr.Create(EmailPattern);
    try
      Regex.ModifierI := True;
      if Regex.Exec(S) then
        Exit(True);
    finally
      Regex.Free;
    end;
  except
    // Fallback de segurança caso a engine TRegExpr lance exceção de alinhamento/memória em AArch64/ARM
  end;

  // 2. Validação algorítmica pura como garantia de robustez em todas as arquiteturas
  AtPos := Pos('@', S);
  if (AtPos <= 1) or (AtPos = Len) then Exit;

  // Não pode conter múltiplos '@'
  if Pos('@', Copy(S, AtPos + 1, Len)) > 0 then Exit;

  LocalPart := Copy(S, 1, AtPos - 1);
  DomainPart := Copy(S, AtPos + 1, Len);

  if (Length(LocalPart) < 1) or (Length(LocalPart) > 64) then Exit;
  for I := 1 to Length(LocalPart) do
  begin
    if not (LocalPart[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_', '%', '+', '-']) then
      Exit;
  end;
  if (LocalPart[1] = '.') or (LocalPart[Length(LocalPart)] = '.') or (Pos('..', LocalPart) > 0) then
    Exit;

  if (Length(DomainPart) < 4) or (Length(DomainPart) > 255) then Exit;
  if (DomainPart[1] = '.') or (DomainPart[Length(DomainPart)] = '.') or (Pos('..', DomainPart) > 0) then
    Exit;
  if (DomainPart[1] = '-') or (DomainPart[Length(DomainPart)] = '-') then
    Exit;

  DotPos := 0;
  for I := Length(DomainPart) downto 1 do
  begin
    if DomainPart[I] = '.' then
    begin
      DotPos := I;
      Break;
    end;
  end;
  if (DotPos = 0) or ((Length(DomainPart) - DotPos) < 2) then Exit;

  for I := DotPos + 1 to Length(DomainPart) do
  begin
    if not (DomainPart[I] in ['a'..'z', 'A'..'Z']) then
      Exit;
  end;

  for I := 1 to Length(DomainPart) do
  begin
    if not (DomainPart[I] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-']) then
      Exit;
  end;

  Result := True;
end;

{ TValidatorEmail }

function TValidatorEmail.Validate(Value: Variant): TValid;
begin
  Result.OK := IsValidEmail(VarToStr(Value));
  if not Result.OK then
    Result.Message := InvalidEmail;
end;

{ TValidatorItemUrl }

function TValidatorItemUrl.Validate(Value: Variant): TValid;
const
  UrlPattern = '^https?://[^\s/$.?#].[^\s]*$';
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create(UrlPattern);
  try
    Result.OK := Regex.Exec(VarToStr(Value));
    if not Result.OK then
      Result.Message := InvalidUrl;
  finally
    Regex.Free;
  end;
end;

{ TValidatorItemCPF }

function TValidatorItemCPF.Validate(Value: Variant): TValid;
var
  Digits: string;
begin
  Digits := OnlyDigits(VarToStr(Value));
  Result.OK := ValidateCPFDigits(Digits);
  if not Result.OK then
    Result.Message := InvalidCPF;
end;

{ TValidatorItemCNPJ }

function TValidatorItemCNPJ.Validate(Value: Variant): TValid;
begin
  Result.OK := IsValidCnpj(VarToStr(Value));
  if not Result.OK then
    Result.Message := InvalidCNPJ;
end;

{ Numeric Validations }

constructor TValidatorItemMinValue.Create(MinValue: Double);
begin
  FMinValue := MinValue;
end;

function TValidatorItemMinValue.Validate(Value: Variant): TValid;
begin
  Result.OK := Value >= FMinValue;
  if not Result.OK then
    Result.Message := Format(MinimumAllowedValue, [FMinValue]);
end;

constructor TValidatorItemGreaterThanZero.Create;
begin
  inherited Create;
end;

function TValidatorItemGreaterThanZero.Validate(Value: Variant): TValid;
begin
  Result.OK := Value > 0;
  if not Result.OK then
    Result.Message := ValueMustBeGreaterThanZero;
end;

constructor TValidatorItemMaxValue.Create(MaxValue: Double);
begin
  FMaxValue := MaxValue;
end;

function TValidatorItemMaxValue.Validate(Value: Variant): TValid;
begin
  Result.OK := Value <= FMaxValue;
  if not Result.OK then
    Result.Message := Format(MaximumAllowedValue, [FMaxValue]);
end;

{ TValidatorItemBetween }

constructor TValidatorItemBetween.Create(AMin, AMax: Double);
begin
  FMin := AMin;
  FMax := AMax;
end;

function TValidatorItemBetween.Validate(Value: Variant): TValid;
begin
  Result.OK := (Value >= FMin) and (Value <= FMax);
  if not Result.OK then
    Result.Message := Format(AllowedRange, [FloatToStr(FMin), FloatToStr(FMax)]);
end;

constructor TValidatorItemPeriod.Create(StartDate, EndDate: TDateTime);
begin
  FStartDate := StartDate;
  FEndDate := EndDate;
end;

function TValidatorItemPeriod.Validate(Value: Variant): TValid;
var
  ValueDate: TDateTime;
begin
  Result.OK := False;
  if VarIsStr(Value) then
  begin
    ValueDate := StrToDate(Value);
    if (ValueDate >= FStartDate) and (ValueDate <= FEndDate) then
      Result.OK := True;
  end;

  if not Result.OK then
    Result.Message := Format(
      AllowedRange,
      [DateToStr(FStartDate), DateToStr(FEndDate)]
    );
end;

end.
