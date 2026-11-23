unit DeltaModelMessages;

{$mode ObjFPC}{$H+}

interface

uses
  DeltaModel.Messages;

type
  TDeltaLocale = DeltaModel.Messages.TDeltaLocale;

resourcestring
  // Legacy string constants (kept for backward compatibility)
  DeltaModelClassNotAssigned = 'DeltaModelClass not assigned to DeltaModelList.';
  ValidationFailedForField = 'Validation failed for field %s: %s';
  ValueCannotBeEmpty = 'Value cannot be empty.';
  MinimumLength = 'Minimum length: %d characters.';
  MaximumLength = 'Maximum length: %d characters.';
  MinimumLenght = 'Minimum length: %d characters.';
  MaximumLenght = 'Maximum length: %d characters.';
  ValueDoesNotMatchREGEX = 'Value does not match the expected pattern.';
  InvalidEmail = 'Invalid email address.';
  InvalidUrl = 'Invalid URL. Must start with http:// or https://.';
  InvalidCPF = 'Invalid CPF.';
  InvalidCNPJ = 'Invalid CNPJ.';
  MinimumAllowedValue = 'Minimum allowed value: %.2f.';
  MaximumAllowedValue = 'Maximum allowed value: %.2f.';
  ValueMustBeGreaterThanZero = 'Value must be greater than zero.';
  AllowedRange = 'Allowed range: %s to %s.';
  FieldIsRequired = 'Field %s is required.';

procedure SetDeltaLocale(ALocale: TDeltaLocale); inline;
function GetDeltaLocale: TDeltaLocale; inline;
procedure SetDeltaLocaleByName(const AName: string); inline;
function GetDeltaLocaleName: string; inline;

implementation

procedure SetDeltaLocale(ALocale: TDeltaLocale);
begin
  DeltaModel.Messages.SetDeltaLocale(ALocale);
end;

function GetDeltaLocale: TDeltaLocale;
begin
  Result := DeltaModel.Messages.GetDeltaLocale;
end;

procedure SetDeltaLocaleByName(const AName: string);
begin
  DeltaModel.Messages.SetDeltaLocaleByName(AName);
end;

function GetDeltaLocaleName: string;
begin
  Result := DeltaModel.Messages.GetDeltaLocaleName;
end;

end.
