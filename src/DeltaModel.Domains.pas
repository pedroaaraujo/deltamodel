unit DeltaModel.Domains;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants,
  DeltaModel.Fields, DeltaValidator;

type
  TDocumentoKind = (dkUnknown, dkCPF, dkCNPJ);

{ ============================================================================ }
{ Pure Sanitization, Validation & Formatting Functions (Zero-RegExpr, Fast)    }
{ ============================================================================ }

function SomenteDigitos(const AValue: string): string;
function SomenteAlfanumerico(const AValue: string): string;

// Email (RFC 5322 simplified/strict)
function SanitizeEmail(const AValue: string): string;
function IsValidEmailAddress(const AValue: string): Boolean;
function FormatEmail(const AValue: string): string;

// CPF (11 digits, check digits, rejects repeated sequences)
function SanitizeCPF(const AValue: string): string;
function IsValidCPF(const AValue: string): Boolean;
function FormatCPF(const AValue: string): string;

// CNPJ (14 chars, supports numeric & alphanumeric RFB IN 2.229/2024)
function SanitizeCNPJ(const AValue: string): string;
function IsValidCNPJ(const AValue: string): Boolean;
function IsAlphanumericCNPJ(const AValue: string): Boolean;
function FormatCNPJ(const AValue: string): string;

// Polymorphic Document (CPF or numeric/alphanumeric CNPJ)
function SanitizeDocumento(const AValue: string): string;
function DetectDocumentoKind(const AValue: string): TDocumentoKind;
function IsValidDocumento(const AValue: string): Boolean;
function FormatDocumento(const AValue: string): string;

// Phone (E.164 international & BR 10/11-digit national)
function SanitizeTelefone(const AValue: string): string;
function IsValidTelefone(const AValue: string): Boolean;
function FormatTelefone(const AValue: string): string;
function TelefoneToE164(const AValue: string; const ADefaultCountryCode: string = '55'): string;

// CEP (Brazilian 8-digit postal code)
function SanitizeCEP(const AValue: string): string;
function IsValidCEP(const AValue: string): Boolean;
function FormatCEP(const AValue: string): string;

{ ============================================================================ }
{ Lightweight Domain Field Value Objects (Zero Extra Instance Fields)          }
{ ============================================================================ }

type
  { TDFEmail (Nullable) }

  TDFEmail = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFEmailNull = TDFEmail;

  { TDFEmailRequired }

  TDFEmailRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  { TDFCPF (Nullable) }

  TDFCPF = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFCPFNull = TDFCPF;

  { TDFCPFRequired }

  TDFCPFRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  { TDFCNPJ (Nullable - Numeric & Alphanumeric RFB IN 2.229/2024) }

  TDFCNPJ = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    function IsAlphanumeric: Boolean;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFCNPJNull = TDFCNPJ;

  { TDFCNPJRequired }

  TDFCNPJRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    function IsAlphanumeric: Boolean;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  { TDFDocumento (Nullable - Polymorphic CPF or Numeric/Alphanumeric CNPJ) }

  TDFDocumento = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Kind: TDocumentoKind;
    function IsCPF: Boolean;
    function IsCNPJ: Boolean;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFDocumentoNull = TDFDocumento;

  { TDFDocumentoRequired }

  TDFDocumentoRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Kind: TDocumentoKind;
    function IsCPF: Boolean;
    function IsCNPJ: Boolean;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  { TDFTelefone (Nullable - E.164 & BR 10/11-digit) }

  TDFTelefone = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    function ToE164(const ADefaultCountryCode: string = '55'): string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFTelefoneNull = TDFTelefone;

  { TDFTelefoneRequired }

  TDFTelefoneRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    function ToE164(const ADefaultCountryCode: string = '55'): string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  { TDFCEP (Nullable - 8-digit Brazilian postal code) }

  TDFCEP = class(TDFStringNull)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

  TDFCEPNull = TDFCEP;

  { TDFCEPRequired }

  TDFCEPRequired = class(TDFStringRequired)
  protected
    procedure SetValue(AValue: Variant); override;
  public
    constructor Create; override;
    procedure AfterConstruction; override;
    procedure SetAsString(const AValue: string); override;
    function IsValid: Boolean; override;
    function SwaggerDataType: string; override;
    function SwaggerFormat: string; override;
    function Formatted: string;
    procedure ValidateOrRaise;
    class function ValidateValue(const AValue: string): Boolean; static;
    class function Sanitize(const AValue: string): string; static;
    class function FormatValue(const AValue: string): string; static;
  end;

implementation

const
  EMAIL_MAX_SIZE     = 254;
  CPF_SIZE           = 11;
  CNPJ_SIZE          = 14;
  DOCUMENTO_MAX_SIZE = 14;
  TELEFONE_MAX_SIZE  = 20;
  CEP_SIZE           = 8;

  CNPJ_WEIGHTS_1: array[1..12] of Integer = (5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2);
  CNPJ_WEIGHTS_2: array[1..13] of Integer = (6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2);

{ ============================================================================ }
{ Internal Fast Helpers                                                        }
{ ============================================================================ }

function HasOnlyAllowedChars(const AValue: string; const AAllowed: TSysCharSet): Boolean; inline;
var
  I: Integer;
begin
  for I := 1 to Length(AValue) do
    if not (AValue[I] in AAllowed) then
      Exit(False);
  Result := True;
end;

function AllCharsEqual(const S: string): Boolean; inline;
var
  I: Integer;
  FirstCh: Char;
begin
  if Length(S) <= 1 then
    Exit(True);
  FirstCh := S[1];
  for I := 2 to Length(S) do
    if S[I] <> FirstCh then
      Exit(False);
  Result := True;
end;

function PrepareCPFForStorage(const AValue: string): string;
begin
  if HasOnlyAllowedChars(AValue, ['0'..'9', '.', '-', ' ', #9]) then
    Result := SomenteDigitos(AValue)
  else
    Result := Trim(AValue);
end;

function PrepareCNPJForStorage(const AValue: string): string;
begin
  if HasOnlyAllowedChars(AValue, ['0'..'9', 'A'..'Z', 'a'..'z', '.', '/', '-', ' ', #9]) then
    Result := SomenteAlfanumerico(AValue)
  else
    Result := UpperCase(Trim(AValue));
end;

function PrepareDocumentoForStorage(const AValue: string): string;
begin
  if HasOnlyAllowedChars(AValue, ['0'..'9', 'A'..'Z', 'a'..'z', '.', '/', '-', ' ', #9]) then
    Result := SomenteAlfanumerico(AValue)
  else
    Result := UpperCase(Trim(AValue));
end;

function PrepareTelefoneForStorage(const AValue: string): string;
begin
  if HasOnlyAllowedChars(AValue, ['0'..'9', '+', '(', ')', '-', '.', ' ', #9]) then
    Result := SanitizeTelefone(AValue)
  else
    Result := Trim(AValue);
end;

function PrepareCEPForStorage(const AValue: string): string;
begin
  if HasOnlyAllowedChars(AValue, ['0'..'9', '-', '.', ' ', #9]) then
    Result := SomenteDigitos(AValue)
  else
    Result := Trim(AValue);
end;

{ ============================================================================ }
{ Core Sanitization Functions                                                  }
{ ============================================================================ }

function SomenteDigitos(const AValue: string): string;
var
  I, Len, OutIdx: Integer;
  C: Char;
begin
  Len := Length(AValue);
  if Len = 0 then
    Exit('');
  SetLength(Result, Len);
  OutIdx := 0;
  for I := 1 to Len do
  begin
    C := AValue[I];
    if (C >= '0') and (C <= '9') then
    begin
      Inc(OutIdx);
      Result[OutIdx] := C;
    end;
  end;
  SetLength(Result, OutIdx);
end;

function SomenteAlfanumerico(const AValue: string): string;
var
  I, Len, OutIdx: Integer;
  C: Char;
begin
  Len := Length(AValue);
  if Len = 0 then
    Exit('');
  SetLength(Result, Len);
  OutIdx := 0;
  for I := 1 to Len do
  begin
    C := AValue[I];
    if (C >= 'a') and (C <= 'z') then
      C := Chr(Ord(C) - 32);
    if ((C >= '0') and (C <= '9')) or ((C >= 'A') and (C <= 'Z')) then
    begin
      Inc(OutIdx);
      Result[OutIdx] := C;
    end;
  end;
  SetLength(Result, OutIdx);
end;

{ ============================================================================ }
{ Email                                                                        }
{ ============================================================================ }

function SanitizeEmail(const AValue: string): string;
var
  I, Len: Integer;
  C: Char;
begin
  Result := Trim(AValue);
  Len := Length(Result);
  for I := 1 to Len do
  begin
    C := Result[I];
    if (C >= 'A') and (C <= 'Z') then
      Result[I] := Chr(Ord(C) + 32);
  end;
end;

function IsValidEmailAddress(const AValue: string): Boolean;
const
  LOCAL_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '.', '!', '#', '$', '%', '&',
                 '''', '*', '+', '-', '/', '=', '?', '^', '_', '`', '{', '|', '}', '~'];
  DOMAIN_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '-', '.'];
var
  S: string;
  Len, AtPos, I, LabelLen, LastDotPos, DotCount: Integer;
  C: Char;
begin
  Result := False;
  S := Trim(AValue);
  Len := Length(S);
  if (Len < 5) or (Len > EMAIL_MAX_SIZE) then
    Exit;

  // Locate single '@'
  AtPos := 0;
  for I := 1 to Len do
  begin
    if S[I] = '@' then
    begin
      if AtPos > 0 then
        Exit; // Multiple '@'
      AtPos := I;
    end;
  end;

  // Local part: 1..64 chars; Domain part: at least 3 chars (e.g. a.co)
  if (AtPos <= 1) or (AtPos > 65) or (Len - AtPos < 3) then
    Exit;

  // Validate local part
  if (S[1] = '.') or (S[AtPos - 1] = '.') then
    Exit;

  for I := 1 to AtPos - 1 do
  begin
    C := S[I];
    if not (C in LOCAL_CHARS) then
      Exit;
    if (C = '.') and (I > 1) and (S[I - 1] = '.') then
      Exit; // Consecutive dots
  end;

  // Validate domain part
  if (S[AtPos + 1] = '.') or (S[AtPos + 1] = '-') or
     (S[Len] = '.') or (S[Len] = '-') then
    Exit;

  LabelLen := 0;
  DotCount := 0;
  LastDotPos := AtPos;

  for I := AtPos + 1 to Len do
  begin
    C := S[I];
    if not (C in DOMAIN_CHARS) then
      Exit;

    if C = '.' then
    begin
      if (LabelLen = 0) or (LabelLen > 63) then
        Exit;
      if S[I - 1] = '-' then
        Exit; // Label cannot end with '-'
      Inc(DotCount);
      LastDotPos := I;
      LabelLen := 0;
    end
    else
    begin
      if (LabelLen = 0) and (C = '-') then
        Exit; // Label cannot start with '-'
      Inc(LabelLen);
      if LabelLen > 63 then
        Exit;
    end;
  end;

  if (DotCount = 0) or (LabelLen < 2) or (LabelLen > 63) then
    Exit;

  // TLD must be alphabetic (RFC 1123 / ICANN TLD rule, >= 2 letters)
  for I := LastDotPos + 1 to Len do
  begin
    C := S[I];
    if not (((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'))) then
      Exit;
  end;

  Result := True;
end;

function FormatEmail(const AValue: string): string;
begin
  Result := SanitizeEmail(AValue);
end;

{ ============================================================================ }
{ CPF                                                                          }
{ ============================================================================ }

function SanitizeCPF(const AValue: string): string;
begin
  Result := SomenteDigitos(AValue);
end;

function IsValidCPF(const AValue: string): Boolean;
var
  Digits: array[1..11] of Integer;
  I, Len, Count, Sum, Rem: Integer;
  C: Char;
  AllEqual: Boolean;
begin
  Result := False;
  Len := Length(AValue);
  if Len < 11 then
    Exit;

  Count := 0;
  for I := 1 to Len do
  begin
    C := AValue[I];
    if (C >= '0') and (C <= '9') then
    begin
      Inc(Count);
      if Count > 11 then
        Exit;
      Digits[Count] := Ord(C) - Ord('0');
    end
    else if not (C in ['.', '-', ' ', #9]) then
      Exit; // Reject letters or unexpected characters
  end;

  if Count <> 11 then
    Exit;

  // Reject repeated sequences (00000000000 .. 99999999999)
  AllEqual := True;
  for I := 2 to 11 do
    if Digits[I] <> Digits[1] then
    begin
      AllEqual := False;
      Break;
    end;
  if AllEqual then
    Exit;

  // 1st check digit
  Sum := 0;
  for I := 1 to 9 do
    Sum := Sum + Digits[I] * (11 - I);
  Rem := (Sum * 10) mod 11;
  if Rem = 10 then
    Rem := 0;
  if Rem <> Digits[10] then
    Exit;

  // 2nd check digit
  Sum := 0;
  for I := 1 to 10 do
    Sum := Sum + Digits[I] * (12 - I);
  Rem := (Sum * 10) mod 11;
  if Rem = 10 then
    Rem := 0;
  if Rem <> Digits[11] then
    Exit;

  Result := True;
end;

function FormatCPF(const AValue: string): string;
var
  Clean: string;
begin
  Clean := SanitizeCPF(AValue);
  if Length(Clean) <> 11 then
    Exit(Clean);
  Result := Copy(Clean, 1, 3) + '.' +
            Copy(Clean, 4, 3) + '.' +
            Copy(Clean, 7, 3) + '-' +
            Copy(Clean, 10, 2);
end;

{ ============================================================================ }
{ CNPJ (Numeric & Alphanumeric RFB IN 2.229/2024)                              }
{ ============================================================================ }

function SanitizeCNPJ(const AValue: string): string;
begin
  Result := SomenteAlfanumerico(AValue);
end;

function IsValidCNPJ(const AValue: string): Boolean;
var
  Chars: array[1..14] of Char;
  I, Len, Count, Sum, Rem, Digit1, Digit2: Integer;
  C: Char;
  AllEqual: Boolean;
begin
  Result := False;
  Len := Length(AValue);
  if Len < 14 then
    Exit;

  Count := 0;
  for I := 1 to Len do
  begin
    C := AValue[I];
    if (C >= 'a') and (C <= 'z') then
      C := Chr(Ord(C) - 32);

    if ((C >= '0') and (C <= '9')) or ((C >= 'A') and (C <= 'Z')) then
    begin
      Inc(Count);
      if Count > 14 then
        Exit;
      Chars[Count] := C;
    end
    else if not (C in ['.', '/', '-', ' ', #9]) then
      Exit; // Reject unexpected symbols
  end;

  if Count <> 14 then
    Exit;

  // RFB IN 2.229/2024: positions 13 and 14 (check digits) MUST be numeric [0-9]
  if not ((Chars[13] >= '0') and (Chars[13] <= '9') and
          (Chars[14] >= '0') and (Chars[14] <= '9')) then
    Exit;

  // Reject repeated sequences (e.g. 00000000000000, 11111111111111)
  AllEqual := True;
  for I := 2 to 14 do
    if Chars[I] <> Chars[1] then
    begin
      AllEqual := False;
      Break;
    end;
  if AllEqual then
    Exit;

  // 1st check digit using canonical ASCII - 48 values
  Sum := 0;
  for I := 1 to 12 do
    Sum := Sum + (Ord(Chars[I]) - 48) * CNPJ_WEIGHTS_1[I];
  Rem := Sum mod 11;
  if Rem < 2 then
    Digit1 := 0
  else
    Digit1 := 11 - Rem;

  if (Ord(Chars[13]) - 48) <> Digit1 then
    Exit;

  // 2nd check digit using canonical ASCII - 48 values
  Sum := 0;
  for I := 1 to 13 do
    Sum := Sum + (Ord(Chars[I]) - 48) * CNPJ_WEIGHTS_2[I];
  Rem := Sum mod 11;
  if Rem < 2 then
    Digit2 := 0
  else
    Digit2 := 11 - Rem;

  Result := (Ord(Chars[14]) - 48) = Digit2;
end;

function IsAlphanumericCNPJ(const AValue: string): Boolean;
var
  Clean: string;
  I: Integer;
begin
  Result := False;
  if not IsValidCNPJ(AValue) then
    Exit;
  Clean := SanitizeCNPJ(AValue);
  for I := 1 to 12 do
    if (Clean[I] >= 'A') and (Clean[I] <= 'Z') then
      Exit(True);
end;

function FormatCNPJ(const AValue: string): string;
var
  Clean: string;
begin
  Clean := SanitizeCNPJ(AValue);
  if Length(Clean) <> 14 then
    Exit(Clean);
  Result := Copy(Clean, 1, 2) + '.' +
            Copy(Clean, 3, 3) + '.' +
            Copy(Clean, 6, 3) + '/' +
            Copy(Clean, 9, 4) + '-' +
            Copy(Clean, 13, 2);
end;

{ ============================================================================ }
{ Polymorphic Document (CPF or CNPJ)                                           }
{ ============================================================================ }

function SanitizeDocumento(const AValue: string): string;
begin
  Result := SomenteAlfanumerico(AValue);
end;

function DetectDocumentoKind(const AValue: string): TDocumentoKind;
var
  Clean: string;
  I: Integer;
begin
  Clean := SanitizeDocumento(AValue);
  if Length(Clean) = 11 then
  begin
    for I := 1 to 11 do
      if not ((Clean[I] >= '0') and (Clean[I] <= '9')) then
        Exit(dkUnknown);
    Result := dkCPF;
  end
  else if Length(Clean) = 14 then
    Result := dkCNPJ
  else
    Result := dkUnknown;
end;

function IsValidDocumento(const AValue: string): Boolean;
var
  Clean: string;
begin
  if not HasOnlyAllowedChars(AValue, ['0'..'9', 'A'..'Z', 'a'..'z', '.', '/', '-', ' ', #9]) then
    Exit(False);
  Clean := SanitizeDocumento(AValue);
  if Length(Clean) = 11 then
    Result := IsValidCPF(AValue)
  else if Length(Clean) = 14 then
    Result := IsValidCNPJ(AValue)
  else
    Result := False;
end;

function FormatDocumento(const AValue: string): string;
var
  Clean: string;
begin
  Clean := SanitizeDocumento(AValue);
  if Length(Clean) = 11 then
    Result := FormatCPF(Clean)
  else if Length(Clean) = 14 then
    Result := FormatCNPJ(Clean)
  else
    Result := Clean;
end;

{ ============================================================================ }
{ Telefone (E.164 & BR 10/11 digits)                                           }
{ ============================================================================ }

function SanitizeTelefone(const AValue: string): string;
var
  S: string;
  I, Len, OutIdx: Integer;
  C: Char;
begin
  S := Trim(AValue);
  Len := Length(S);
  if Len = 0 then
    Exit('');

  SetLength(Result, Len);
  OutIdx := 0;

  if S[1] = '+' then
  begin
    Inc(OutIdx);
    Result[OutIdx] := '+';
    I := 2;
  end
  else
    I := 1;

  while I <= Len do
  begin
    C := S[I];
    if (C >= '0') and (C <= '9') then
    begin
      Inc(OutIdx);
      Result[OutIdx] := C;
    end;
    Inc(I);
  end;
  SetLength(Result, OutIdx);
end;

function IsValidBRSubscriber(const ADigits: string; AOffset, ASubLen: Integer): Boolean;
var
  D1, D2, FirstSub: Char;
  SubStr: string;
begin
  // Validate Brazilian DDD: two non-zero digits (11..99 without 0)
  D1 := ADigits[AOffset];
  D2 := ADigits[AOffset + 1];
  if (D1 = '0') or (D2 = '0') then
    Exit(False);

  FirstSub := ADigits[AOffset + 2];
  SubStr := Copy(ADigits, AOffset, ASubLen);
  if AllCharsEqual(SubStr) then
    Exit(False);

  if ASubLen = 11 then
    // Mobile: 9-digit subscriber must start with '9'
    Result := (FirstSub = '9')
  else if ASubLen = 10 then
    // Landline: 8-digit subscriber starts with '2'..'9'
    Result := (FirstSub >= '2') and (FirstSub <= '9')
  else
    Result := False;
end;

function IsValidTelefone(const AValue: string): Boolean;
var
  S, Digits: string;
  I, Len, DLen: Integer;
  HasPlus: Boolean;
  C: Char;
begin
  Result := False;
  S := Trim(AValue);
  Len := Length(S);
  if Len = 0 then
    Exit;

  HasPlus := (S[1] = '+');
  SetLength(Digits, Len);
  DLen := 0;

  for I := 1 to Len do
  begin
    C := S[I];
    if (C >= '0') and (C <= '9') then
    begin
      Inc(DLen);
      Digits[DLen] := C;
    end
    else if C = '+' then
    begin
      if I <> 1 then
        Exit; // '+' only permitted at first position
    end
    else if not (C in ['(', ')', '-', '.', ' ', #9]) then
      Exit; // Reject letters and invalid symbols
  end;
  SetLength(Digits, DLen);

  if DLen = 0 then
    Exit;

  if HasPlus then
  begin
    // E.164: 8 to 15 digits, country code cannot start with '0'
    if (DLen < 8) or (DLen > 15) or (Digits[1] = '0') then
      Exit;
    if AllCharsEqual(Digits) then
      Exit;
    // Specific validation for Brazil (+55)
    if (DLen >= 2) and (Digits[1] = '5') and (Digits[2] = '5') then
    begin
      if (DLen <> 12) and (DLen <> 13) then
        Exit;
      Result := IsValidBRSubscriber(Digits, 3, DLen - 2);
    end
    else
      Result := True;
  end
  else
  begin
    // National BR: 10 (landline) or 11 (mobile), or 12/13 with country code 55
    if (DLen = 10) or (DLen = 11) then
      Result := IsValidBRSubscriber(Digits, 1, DLen)
    else if ((DLen = 12) or (DLen = 13)) and (Digits[1] = '5') and (Digits[2] = '5') then
      Result := IsValidBRSubscriber(Digits, 3, DLen - 2)
    else
      Result := False;
  end;
end;

function FormatTelefone(const AValue: string): string;
var
  Clean, Digits: string;
  DLen: Integer;
begin
  Clean := SanitizeTelefone(AValue);
  if Clean = '' then
    Exit('');

  if Clean[1] = '+' then
  begin
    Digits := Copy(Clean, 2, MaxInt);
    DLen := Length(Digits);
    if (DLen = 12) and (Digits[1] = '5') and (Digits[2] = '5') then
      Result := '+55 (' + Copy(Digits, 3, 2) + ') ' + Copy(Digits, 5, 4) + '-' + Copy(Digits, 9, 4)
    else if (DLen = 13) and (Digits[1] = '5') and (Digits[2] = '5') then
      Result := '+55 (' + Copy(Digits, 3, 2) + ') ' + Copy(Digits, 5, 5) + '-' + Copy(Digits, 10, 4)
    else
      Result := Clean;
  end
  else
  begin
    DLen := Length(Clean);
    if DLen = 10 then
      Result := '(' + Copy(Clean, 1, 2) + ') ' + Copy(Clean, 3, 4) + '-' + Copy(Clean, 7, 4)
    else if DLen = 11 then
      Result := '(' + Copy(Clean, 1, 2) + ') ' + Copy(Clean, 3, 5) + '-' + Copy(Clean, 8, 4)
    else if (DLen = 12) and (Clean[1] = '5') and (Clean[2] = '5') then
      Result := '+55 (' + Copy(Clean, 3, 2) + ') ' + Copy(Clean, 5, 4) + '-' + Copy(Clean, 9, 4)
    else if (DLen = 13) and (Clean[1] = '5') and (Clean[2] = '5') then
      Result := '+55 (' + Copy(Clean, 3, 2) + ') ' + Copy(Clean, 5, 5) + '-' + Copy(Clean, 10, 4)
    else
      Result := Clean;
  end;
end;

function TelefoneToE164(const AValue: string; const ADefaultCountryCode: string): string;
var
  Clean, CC: string;
begin
  Clean := SanitizeTelefone(AValue);
  if Clean = '' then
    Exit('');
  if Clean[1] = '+' then
    Exit(Clean);

  CC := SomenteDigitos(ADefaultCountryCode);
  if CC = '' then
    CC := '55';

  if ((Length(Clean) = 12) or (Length(Clean) = 13)) and
     (Copy(Clean, 1, Length(CC)) = CC) then
    Result := '+' + Clean
  else
    Result := '+' + CC + Clean;
end;

{ ============================================================================ }
{ CEP                                                                          }
{ ============================================================================ }

function SanitizeCEP(const AValue: string): string;
begin
  Result := SomenteDigitos(AValue);
end;

function IsValidCEP(const AValue: string): Boolean;
var
  I, Len, Count: Integer;
  C: Char;
  AllZero: Boolean;
begin
  Result := False;
  Len := Length(AValue);
  if Len < 8 then
    Exit;

  Count := 0;
  AllZero := True;
  for I := 1 to Len do
  begin
    C := AValue[I];
    if (C >= '0') and (C <= '9') then
    begin
      Inc(Count);
      if Count > 8 then
        Exit;
      if C <> '0' then
        AllZero := False;
    end
    else if not (C in ['-', '.', ' ', #9]) then
      Exit; // Reject letters or invalid characters
  end;

  Result := (Count = 8) and (not AllZero);
end;

function FormatCEP(const AValue: string): string;
var
  Clean: string;
begin
  Clean := SanitizeCEP(AValue);
  if Length(Clean) <> 8 then
    Exit(Clean);
  Result := Copy(Clean, 1, 5) + '-' + Copy(Clean, 6, 3);
end;

{ ============================================================================ }
{ TDFEmail & TDFEmailRequired                                                  }
{ ============================================================================ }

constructor TDFEmail.Create;
begin
  inherited Create;
  Size := EMAIL_MAX_SIZE;
end;

procedure TDFEmail.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := EMAIL_MAX_SIZE;
end;

procedure TDFEmail.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(SanitizeEmail(string(AValue)));
end;

procedure TDFEmail.SetAsString(const AValue: string);
begin
  inherited SetAsString(SanitizeEmail(AValue));
end;

function TDFEmail.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidEmailAddress(AsString);
end;

function TDFEmail.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFEmail.SwaggerFormat: string;
begin
  Result := 'email';
end;

function TDFEmail.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatEmail(AsString);
end;

procedure TDFEmail.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid email address for field "%s".', [FieldName]);
end;

class function TDFEmail.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidEmailAddress(AValue);
end;

class function TDFEmail.Sanitize(const AValue: string): string;
begin
  Result := SanitizeEmail(AValue);
end;

class function TDFEmail.FormatValue(const AValue: string): string;
begin
  Result := FormatEmail(AValue);
end;

{ TDFEmailRequired }

constructor TDFEmailRequired.Create;
begin
  inherited Create;
  Size := EMAIL_MAX_SIZE;
end;

procedure TDFEmailRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := EMAIL_MAX_SIZE;
end;

procedure TDFEmailRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(SanitizeEmail(string(AValue)));
end;

procedure TDFEmailRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(SanitizeEmail(AValue));
end;

function TDFEmailRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidEmailAddress(AsString);
end;

function TDFEmailRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFEmailRequired.SwaggerFormat: string;
begin
  Result := 'email';
end;

function TDFEmailRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatEmail(AsString);
end;

procedure TDFEmailRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid email address for field "%s".', [FieldName]);
end;

class function TDFEmailRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidEmailAddress(AValue);
end;

class function TDFEmailRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeEmail(AValue);
end;

class function TDFEmailRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatEmail(AValue);
end;

{ ============================================================================ }
{ TDFCPF & TDFCPFRequired                                                      }
{ ============================================================================ }

constructor TDFCPF.Create;
begin
  inherited Create;
  Size := CPF_SIZE;
end;

procedure TDFCPF.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CPF_SIZE;
end;

procedure TDFCPF.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCPFForStorage(string(AValue)));
end;

procedure TDFCPF.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCPFForStorage(AValue));
end;

function TDFCPF.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidCPF(AsString);
end;

function TDFCPF.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCPF.SwaggerFormat: string;
begin
  Result := 'cpf';
end;

function TDFCPF.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCPF(AsString);
end;

procedure TDFCPF.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CPF for field "%s".', [FieldName]);
end;

class function TDFCPF.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCPF(AValue);
end;

class function TDFCPF.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCPF(AValue);
end;

class function TDFCPF.FormatValue(const AValue: string): string;
begin
  Result := FormatCPF(AValue);
end;

{ TDFCPFRequired }

constructor TDFCPFRequired.Create;
begin
  inherited Create;
  Size := CPF_SIZE;
end;

procedure TDFCPFRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CPF_SIZE;
end;

procedure TDFCPFRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCPFForStorage(string(AValue)));
end;

procedure TDFCPFRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCPFForStorage(AValue));
end;

function TDFCPFRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidCPF(AsString);
end;

function TDFCPFRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCPFRequired.SwaggerFormat: string;
begin
  Result := 'cpf';
end;

function TDFCPFRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCPF(AsString);
end;

procedure TDFCPFRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CPF for field "%s".', [FieldName]);
end;

class function TDFCPFRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCPF(AValue);
end;

class function TDFCPFRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCPF(AValue);
end;

class function TDFCPFRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatCPF(AValue);
end;

{ ============================================================================ }
{ TDFCNPJ & TDFCNPJRequired                                                    }
{ ============================================================================ }

constructor TDFCNPJ.Create;
begin
  inherited Create;
  Size := CNPJ_SIZE;
end;

procedure TDFCNPJ.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CNPJ_SIZE;
end;

procedure TDFCNPJ.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCNPJForStorage(string(AValue)));
end;

procedure TDFCNPJ.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCNPJForStorage(AValue));
end;

function TDFCNPJ.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidCNPJ(AsString);
end;

function TDFCNPJ.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCNPJ.SwaggerFormat: string;
begin
  Result := 'cnpj';
end;

function TDFCNPJ.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCNPJ(AsString);
end;

function TDFCNPJ.IsAlphanumeric: Boolean;
begin
  if IsNull then
    Result := False
  else
    Result := IsAlphanumericCNPJ(AsString);
end;

procedure TDFCNPJ.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CNPJ for field "%s".', [FieldName]);
end;

class function TDFCNPJ.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCNPJ(AValue);
end;

class function TDFCNPJ.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCNPJ(AValue);
end;

class function TDFCNPJ.FormatValue(const AValue: string): string;
begin
  Result := FormatCNPJ(AValue);
end;

{ TDFCNPJRequired }

constructor TDFCNPJRequired.Create;
begin
  inherited Create;
  Size := CNPJ_SIZE;
end;

procedure TDFCNPJRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CNPJ_SIZE;
end;

procedure TDFCNPJRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCNPJForStorage(string(AValue)));
end;

procedure TDFCNPJRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCNPJForStorage(AValue));
end;

function TDFCNPJRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidCNPJ(AsString);
end;

function TDFCNPJRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCNPJRequired.SwaggerFormat: string;
begin
  Result := 'cnpj';
end;

function TDFCNPJRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCNPJ(AsString);
end;

function TDFCNPJRequired.IsAlphanumeric: Boolean;
begin
  if IsNull then
    Result := False
  else
    Result := IsAlphanumericCNPJ(AsString);
end;

procedure TDFCNPJRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CNPJ for field "%s".', [FieldName]);
end;

class function TDFCNPJRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCNPJ(AValue);
end;

class function TDFCNPJRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCNPJ(AValue);
end;

class function TDFCNPJRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatCNPJ(AValue);
end;

{ ============================================================================ }
{ TDFDocumento & TDFDocumentoRequired                                          }
{ ============================================================================ }

constructor TDFDocumento.Create;
begin
  inherited Create;
  Size := DOCUMENTO_MAX_SIZE;
end;

procedure TDFDocumento.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DOCUMENTO_MAX_SIZE;
end;

procedure TDFDocumento.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareDocumentoForStorage(string(AValue)));
end;

procedure TDFDocumento.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareDocumentoForStorage(AValue));
end;

function TDFDocumento.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidDocumento(AsString);
end;

function TDFDocumento.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDocumento.SwaggerFormat: string;
begin
  Result := 'cpf-cnpj';
end;

function TDFDocumento.Kind: TDocumentoKind;
begin
  if IsNull then
    Result := dkUnknown
  else
    Result := DetectDocumentoKind(AsString);
end;

function TDFDocumento.IsCPF: Boolean;
begin
  Result := (not IsNull) and (Kind = dkCPF) and IsValidCPF(AsString);
end;

function TDFDocumento.IsCNPJ: Boolean;
begin
  Result := (not IsNull) and (Kind = dkCNPJ) and IsValidCNPJ(AsString);
end;

function TDFDocumento.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatDocumento(AsString);
end;

procedure TDFDocumento.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid document (CPF/CNPJ) for field "%s".', [FieldName]);
end;

class function TDFDocumento.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidDocumento(AValue);
end;

class function TDFDocumento.Sanitize(const AValue: string): string;
begin
  Result := SanitizeDocumento(AValue);
end;

class function TDFDocumento.FormatValue(const AValue: string): string;
begin
  Result := FormatDocumento(AValue);
end;

{ TDFDocumentoRequired }

constructor TDFDocumentoRequired.Create;
begin
  inherited Create;
  Size := DOCUMENTO_MAX_SIZE;
end;

procedure TDFDocumentoRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := DOCUMENTO_MAX_SIZE;
end;

procedure TDFDocumentoRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareDocumentoForStorage(string(AValue)));
end;

procedure TDFDocumentoRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareDocumentoForStorage(AValue));
end;

function TDFDocumentoRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidDocumento(AsString);
end;

function TDFDocumentoRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFDocumentoRequired.SwaggerFormat: string;
begin
  Result := 'cpf-cnpj';
end;

function TDFDocumentoRequired.Kind: TDocumentoKind;
begin
  if IsNull then
    Result := dkUnknown
  else
    Result := DetectDocumentoKind(AsString);
end;

function TDFDocumentoRequired.IsCPF: Boolean;
begin
  Result := (not IsNull) and (Kind = dkCPF) and IsValidCPF(AsString);
end;

function TDFDocumentoRequired.IsCNPJ: Boolean;
begin
  Result := (not IsNull) and (Kind = dkCNPJ) and IsValidCNPJ(AsString);
end;

function TDFDocumentoRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatDocumento(AsString);
end;

procedure TDFDocumentoRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid document (CPF/CNPJ) for field "%s".', [FieldName]);
end;

class function TDFDocumentoRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidDocumento(AValue);
end;

class function TDFDocumentoRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeDocumento(AValue);
end;

class function TDFDocumentoRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatDocumento(AValue);
end;

{ ============================================================================ }
{ TDFTelefone & TDFTelefoneRequired                                            }
{ ============================================================================ }

constructor TDFTelefone.Create;
begin
  inherited Create;
  Size := TELEFONE_MAX_SIZE;
end;

procedure TDFTelefone.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := TELEFONE_MAX_SIZE;
end;

procedure TDFTelefone.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareTelefoneForStorage(string(AValue)));
end;

procedure TDFTelefone.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareTelefoneForStorage(AValue));
end;

function TDFTelefone.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidTelefone(AsString);
end;

function TDFTelefone.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFTelefone.SwaggerFormat: string;
begin
  Result := 'phone';
end;

function TDFTelefone.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatTelefone(AsString);
end;

function TDFTelefone.ToE164(const ADefaultCountryCode: string): string;
begin
  if IsNull then
    Result := ''
  else
    Result := TelefoneToE164(AsString, ADefaultCountryCode);
end;

procedure TDFTelefone.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid phone number for field "%s".', [FieldName]);
end;

class function TDFTelefone.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidTelefone(AValue);
end;

class function TDFTelefone.Sanitize(const AValue: string): string;
begin
  Result := SanitizeTelefone(AValue);
end;

class function TDFTelefone.FormatValue(const AValue: string): string;
begin
  Result := FormatTelefone(AValue);
end;

{ TDFTelefoneRequired }

constructor TDFTelefoneRequired.Create;
begin
  inherited Create;
  Size := TELEFONE_MAX_SIZE;
end;

procedure TDFTelefoneRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := TELEFONE_MAX_SIZE;
end;

procedure TDFTelefoneRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareTelefoneForStorage(string(AValue)));
end;

procedure TDFTelefoneRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareTelefoneForStorage(AValue));
end;

function TDFTelefoneRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidTelefone(AsString);
end;

function TDFTelefoneRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFTelefoneRequired.SwaggerFormat: string;
begin
  Result := 'phone';
end;

function TDFTelefoneRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatTelefone(AsString);
end;

function TDFTelefoneRequired.ToE164(const ADefaultCountryCode: string): string;
begin
  if IsNull then
    Result := ''
  else
    Result := TelefoneToE164(AsString, ADefaultCountryCode);
end;

procedure TDFTelefoneRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid phone number for field "%s".', [FieldName]);
end;

class function TDFTelefoneRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidTelefone(AValue);
end;

class function TDFTelefoneRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeTelefone(AValue);
end;

class function TDFTelefoneRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatTelefone(AValue);
end;

{ ============================================================================ }
{ TDFCEP & TDFCEPRequired                                                      }
{ ============================================================================ }

constructor TDFCEP.Create;
begin
  inherited Create;
  Size := CEP_SIZE;
end;

procedure TDFCEP.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CEP_SIZE;
end;

procedure TDFCEP.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCEPForStorage(string(AValue)));
end;

procedure TDFCEP.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCEPForStorage(AValue));
end;

function TDFCEP.IsValid: Boolean;
begin
  if IsNull then
    Result := True
  else
    Result := IsValidCEP(AsString);
end;

function TDFCEP.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCEP.SwaggerFormat: string;
begin
  Result := 'cep';
end;

function TDFCEP.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCEP(AsString);
end;

procedure TDFCEP.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CEP for field "%s".', [FieldName]);
end;

class function TDFCEP.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCEP(AValue);
end;

class function TDFCEP.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCEP(AValue);
end;

class function TDFCEP.FormatValue(const AValue: string): string;
begin
  Result := FormatCEP(AValue);
end;

{ TDFCEPRequired }

constructor TDFCEPRequired.Create;
begin
  inherited Create;
  Size := CEP_SIZE;
end;

procedure TDFCEPRequired.AfterConstruction;
begin
  inherited AfterConstruction;
  Size := CEP_SIZE;
end;

procedure TDFCEPRequired.SetValue(AValue: Variant);
begin
  if VarIsNull(AValue) then
    inherited SetValue(AValue)
  else
    inherited SetValue(PrepareCEPForStorage(string(AValue)));
end;

procedure TDFCEPRequired.SetAsString(const AValue: string);
begin
  inherited SetAsString(PrepareCEPForStorage(AValue));
end;

function TDFCEPRequired.IsValid: Boolean;
begin
  Result := (not IsNull) and IsValidCEP(AsString);
end;

function TDFCEPRequired.SwaggerDataType: string;
begin
  Result := 'string';
end;

function TDFCEPRequired.SwaggerFormat: string;
begin
  Result := 'cep';
end;

function TDFCEPRequired.Formatted: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FormatCEP(AsString);
end;

procedure TDFCEPRequired.ValidateOrRaise;
begin
  if not IsValid then
    raise EDeltaValidation.CreateFmt('Invalid CEP for field "%s".', [FieldName]);
end;

class function TDFCEPRequired.ValidateValue(const AValue: string): Boolean;
begin
  Result := IsValidCEP(AValue);
end;

class function TDFCEPRequired.Sanitize(const AValue: string): string;
begin
  Result := SanitizeCEP(AValue);
end;

class function TDFCEPRequired.FormatValue(const AValue: string): string;
begin
  Result := FormatCEP(AValue);
end;

end.
