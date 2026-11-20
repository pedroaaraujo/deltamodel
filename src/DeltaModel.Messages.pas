unit DeltaModel.Messages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Supported locales for DeltaModel messages }
  TDeltaLocale = (dlEnUS, dlPtBR, dlEsES);

procedure SetDeltaLocale(ALocale: TDeltaLocale);
function GetDeltaLocale: TDeltaLocale;
procedure SetDeltaLocaleByName(const AName: string);
function GetDeltaLocaleName: string;

// Model validation messages
function MsgModelClassNotAssigned: string;
function MsgFieldRequired(const AClassName, AFieldName: string): string;
function MsgFieldCannotBeEmpty(const AClassName, AFieldName: string): string;
function MsgFieldMinLength(const AClassName, AFieldName: string; ALen: Integer): string;
function MsgFieldMaxLength(const AClassName, AFieldName: string; ALen: Integer): string;
function MsgFieldInvalidEmail(const AClassName, AFieldName: string): string;
function MsgFieldInvalidCPF(const AClassName, AFieldName: string): string;
function MsgFieldInvalidCNPJ(const AClassName, AFieldName: string): string;
function MsgFieldInvalidCPF_CNPJ(const AClassName, AFieldName: string): string;
function MsgFieldInvalidUrl(const AClassName, AFieldName: string): string;
function MsgFieldInvalidPhone(const AClassName, AFieldName: string): string;
function MsgFieldInvalidCEP(const AClassName, AFieldName: string): string;
function MsgFieldMustBePositive(const AClassName, AFieldName: string): string;
function MsgFieldMinValue(const AClassName, AFieldName: string; AVal: Double): string;
function MsgFieldMaxValue(const AClassName, AFieldName: string; AVal: Double): string;

// Generic validator messages
function MsgValidationFailed(const AField, ADetail: string): string;
function MsgValueCannotBeEmpty: string;
function MsgMinLength(ALen: Integer): string;
function MsgMaxLength(ALen: Integer): string;
function MsgValueDoesNotMatchRegex: string;
function MsgInvalidEmail: string;
function MsgInvalidUrl: string;
function MsgInvalidCPF: string;
function MsgInvalidCNPJ: string;
function MsgMinValue(AVal: Double): string;
function MsgMaxValue(AVal: Double): string;
function MsgValueMustBeGreaterThanZero: string;
function MsgAllowedRange(const AMin, AMax: string): string;

// ORM messages
function MsgBulkInsertRequiresModels: string;
function MsgBulkInsertRequiresInsertableFields: string;
function MsgModelClassNotFound: string;

implementation

var
  GCurrentLocale: TDeltaLocale = dlEnUS;

const
  STR_LOCALE_NAMES: array[TDeltaLocale] of string = ('en-US', 'pt-BR', 'es-ES');

  STR_MODEL_CLASS_NOT_ASSIGNED: array[TDeltaLocale] of string = (
    'DeltaModelClass not assigned to DeltaModelList.',
    'DeltaModelClass não atribuída para DeltaModelList.',
    'DeltaModelClass no asignada a DeltaModelList.'
  );

  STR_FIELD_REQUIRED: array[TDeltaLocale] of string = (
    'Field %s.%s is required.',
    'O campo %s.%s é obrigatório.',
    'El campo %s.%s es obligatorio.'
  );

  STR_FIELD_CANNOT_BE_EMPTY: array[TDeltaLocale] of string = (
    'Field %s.%s cannot be empty.',
    'O campo %s.%s não pode ser vazio.',
    'El campo %s.%s no puede estar vacío.'
  );

  STR_FIELD_MIN_LENGTH: array[TDeltaLocale] of string = (
    'Field %s.%s minimum length is %d characters.',
    'O campo %s.%s deve ter no mínimo %d caracteres.',
    'El campo %s.%s debe tener un mínimo de %d caracteres.'
  );

  STR_FIELD_MAX_LENGTH: array[TDeltaLocale] of string = (
    'Field %s.%s maximum length is %d characters.',
    'O campo %s.%s deve ter no máximo %d caracteres.',
    'El campo %s.%s debe tener un máximo de %d caracteres.'
  );

  STR_FIELD_INVALID_EMAIL: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid email address.',
    'O campo %s.%s não é um e-mail válido.',
    'El campo %s.%s no es un correo electrónico válido.'
  );

  STR_FIELD_INVALID_CPF: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid CPF.',
    'O campo %s.%s não é um CPF válido.',
    'El campo %s.%s no es un CPF válido.'
  );

  STR_FIELD_INVALID_CNPJ: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid CNPJ.',
    'O campo %s.%s não é um CNPJ válido.',
    'El campo %s.%s no es un CNPJ válido.'
  );

  STR_FIELD_INVALID_CPF_CNPJ: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid CPF or CNPJ.',
    'O campo %s.%s não é um CPF ou CNPJ válido.',
    'El campo %s.%s no es un CPF o CNPJ válido.'
  );

  STR_FIELD_INVALID_URL: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid URL (must start with http:// or https://).',
    'O campo %s.%s não é uma URL válida (deve iniciar com http:// ou https://).',
    'El campo %s.%s no es una URL válida (debe comenzar con http:// o https://).'
  );

  STR_FIELD_INVALID_PHONE: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid phone number.',
    'O campo %s.%s não é um telefone válido.',
    'El campo %s.%s no es un número de teléfono válido.'
  );

  STR_FIELD_INVALID_CEP: array[TDeltaLocale] of string = (
    'Field %s.%s is not a valid CEP.',
    'O campo %s.%s não é um CEP válido.',
    'El campo %s.%s no es un CEP válido.'
  );

  STR_FIELD_MUST_BE_POSITIVE: array[TDeltaLocale] of string = (
    'Field %s.%s must be greater than zero.',
    'O campo %s.%s deve ser maior que zero.',
    'El campo %s.%s debe ser mayor que cero.'
  );

  STR_FIELD_MIN_VALUE: array[TDeltaLocale] of string = (
    'Field %s.%s minimum value is %.2f.',
    'O campo %s.%s deve ter valor mínimo %.2f.',
    'El campo %s.%s debe tener un valor mínimo de %.2f.'
  );

  STR_FIELD_MAX_VALUE: array[TDeltaLocale] of string = (
    'Field %s.%s maximum value is %.2f.',
    'O campo %s.%s deve ter valor máximo %.2f.',
    'El campo %s.%s debe tener un valor máximo de %.2f.'
  );

  STR_VALIDATION_FAILED: array[TDeltaLocale] of string = (
    'Validation failed for field %s: %s',
    'Falha na validação do campo %s: %s',
    'Error de validación para el campo %s: %s'
  );

  STR_VALUE_CANNOT_BE_EMPTY: array[TDeltaLocale] of string = (
    'Value cannot be empty.',
    'O valor não pode ser vazio.',
    'El valor no puede estar vacío.'
  );

  STR_MIN_LENGTH: array[TDeltaLocale] of string = (
    'Minimum length: %d characters.',
    'Tamanho mínimo: %d caracteres.',
    'Longitud mínima: %d caracteres.'
  );

  STR_MAX_LENGTH: array[TDeltaLocale] of string = (
    'Maximum length: %d characters.',
    'Tamanho máximo: %d caracteres.',
    'Longitud máxima: %d caracteres.'
  );

  STR_VALUE_DOES_NOT_MATCH_REGEX: array[TDeltaLocale] of string = (
    'Value does not match the expected pattern.',
    'O valor não corresponde ao padrão esperado.',
    'El valor no coincide con el patrón esperado.'
  );

  STR_INVALID_EMAIL: array[TDeltaLocale] of string = (
    'Invalid email address.',
    'Endereço de e-mail inválido.',
    'Dirección de correo electrónico inválida.'
  );

  STR_INVALID_URL: array[TDeltaLocale] of string = (
    'Invalid URL. Must start with http:// or https://.',
    'URL inválida. Deve iniciar com http:// ou https://.',
    'URL inválida. Debe comenzar con http:// o https://.'
  );

  STR_INVALID_CPF: array[TDeltaLocale] of string = (
    'Invalid CPF document.',
    'CPF inválido.',
    'Documento CPF inválido.'
  );

  STR_INVALID_CNPJ: array[TDeltaLocale] of string = (
    'Invalid CNPJ document.',
    'CNPJ inválido.',
    'Documento CNPJ inválido.'
  );

  STR_MIN_VALUE: array[TDeltaLocale] of string = (
    'Minimum allowed value: %.2f.',
    'Valor mínimo permitido: %.2f.',
    'Valor mínimo permitido: %.2f.'
  );

  STR_MAX_VALUE: array[TDeltaLocale] of string = (
    'Maximum allowed value: %.2f.',
    'Valor máximo permitido: %.2f.',
    'Valor máximo permitido: %.2f.'
  );

  STR_VALUE_MUST_BE_GREATER_THAN_ZERO: array[TDeltaLocale] of string = (
    'Value must be greater than zero.',
    'O valor deve ser maior que zero.',
    'El valor debe ser mayor que cero.'
  );

  STR_ALLOWED_RANGE: array[TDeltaLocale] of string = (
    'Allowed range: %s to %s.',
    'Intervalo permitido: de %s a %s.',
    'Rango permitido: de %s a %s.'
  );

  STR_BULK_INSERT_REQUIRES_MODELS: array[TDeltaLocale] of string = (
    'BulkInsert requires at least one model.',
    'BulkInsert requer pelo menos um modelo.',
    'BulkInsert requiere al menos un modelo.'
  );

  STR_BULK_INSERT_REQUIRES_INSERTABLE_FIELDS: array[TDeltaLocale] of string = (
    'BulkInsert requires at least one insertable field.',
    'BulkInsert requer pelo menos um campo inserível.',
    'BulkInsert requiere al menos un campo insertable.'
  );

  STR_MODEL_CLASS_NOT_FOUND: array[TDeltaLocale] of string = (
    'Model class not defined for TQuery.',
    'Classe de modelo não definida para TQuery.',
    'Clase de modelo no definida para TQuery.'
  );

procedure SetDeltaLocale(ALocale: TDeltaLocale);
begin
  GCurrentLocale := ALocale;
end;

function GetDeltaLocale: TDeltaLocale;
begin
  Result := GCurrentLocale;
end;

procedure SetDeltaLocaleByName(const AName: string);
var
  S: string;
begin
  S := LowerCase(Trim(AName));
  if (S = 'pt') or (S = 'pt-br') or (S = 'pt_br') or (S = 'portuguese') then
    GCurrentLocale := dlPtBR
  else if (S = 'es') or (S = 'es-es') or (S = 'es_es') or (S = 'spanish') then
    GCurrentLocale := dlEsES
  else
    GCurrentLocale := dlEnUS;
end;

function GetDeltaLocaleName: string;
begin
  Result := STR_LOCALE_NAMES[GCurrentLocale];
end;

function MsgModelClassNotAssigned: string;
begin
  Result := STR_MODEL_CLASS_NOT_ASSIGNED[GCurrentLocale];
end;

function MsgFieldRequired(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_REQUIRED[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldCannotBeEmpty(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_CANNOT_BE_EMPTY[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldMinLength(const AClassName, AFieldName: string; ALen: Integer): string;
begin
  Result := Format(STR_FIELD_MIN_LENGTH[GCurrentLocale], [AClassName, AFieldName, ALen]);
end;

function MsgFieldMaxLength(const AClassName, AFieldName: string; ALen: Integer): string;
begin
  Result := Format(STR_FIELD_MAX_LENGTH[GCurrentLocale], [AClassName, AFieldName, ALen]);
end;

function MsgFieldInvalidEmail(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_EMAIL[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidCPF(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_CPF[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidCNPJ(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_CNPJ[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidCPF_CNPJ(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_CPF_CNPJ[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidUrl(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_URL[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidPhone(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_PHONE[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldInvalidCEP(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_INVALID_CEP[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldMustBePositive(const AClassName, AFieldName: string): string;
begin
  Result := Format(STR_FIELD_MUST_BE_POSITIVE[GCurrentLocale], [AClassName, AFieldName]);
end;

function MsgFieldMinValue(const AClassName, AFieldName: string; AVal: Double): string;
begin
  Result := Format(STR_FIELD_MIN_VALUE[GCurrentLocale], [AClassName, AFieldName, AVal]);
end;

function MsgFieldMaxValue(const AClassName, AFieldName: string; AVal: Double): string;
begin
  Result := Format(STR_FIELD_MAX_VALUE[GCurrentLocale], [AClassName, AFieldName, AVal]);
end;

function MsgValidationFailed(const AField, ADetail: string): string;
begin
  Result := Format(STR_VALIDATION_FAILED[GCurrentLocale], [AField, ADetail]);
end;

function MsgValueCannotBeEmpty: string;
begin
  Result := STR_VALUE_CANNOT_BE_EMPTY[GCurrentLocale];
end;

function MsgMinLength(ALen: Integer): string;
begin
  Result := Format(STR_MIN_LENGTH[GCurrentLocale], [ALen]);
end;

function MsgMaxLength(ALen: Integer): string;
begin
  Result := Format(STR_MAX_LENGTH[GCurrentLocale], [ALen]);
end;

function MsgValueDoesNotMatchRegex: string;
begin
  Result := STR_VALUE_DOES_NOT_MATCH_REGEX[GCurrentLocale];
end;

function MsgInvalidEmail: string;
begin
  Result := STR_INVALID_EMAIL[GCurrentLocale];
end;

function MsgInvalidUrl: string;
begin
  Result := STR_INVALID_URL[GCurrentLocale];
end;

function MsgInvalidCPF: string;
begin
  Result := STR_INVALID_CPF[GCurrentLocale];
end;

function MsgInvalidCNPJ: string;
begin
  Result := STR_INVALID_CNPJ[GCurrentLocale];
end;

function MsgMinValue(AVal: Double): string;
begin
  Result := Format(STR_MIN_VALUE[GCurrentLocale], [AVal]);
end;

function MsgMaxValue(AVal: Double): string;
begin
  Result := Format(STR_MAX_VALUE[GCurrentLocale], [AVal]);
end;

function MsgValueMustBeGreaterThanZero: string;
begin
  Result := STR_VALUE_MUST_BE_GREATER_THAN_ZERO[GCurrentLocale];
end;

function MsgAllowedRange(const AMin, AMax: string): string;
begin
  Result := Format(STR_ALLOWED_RANGE[GCurrentLocale], [AMin, AMax]);
end;

function MsgBulkInsertRequiresModels: string;
begin
  Result := STR_BULK_INSERT_REQUIRES_MODELS[GCurrentLocale];
end;

function MsgBulkInsertRequiresInsertableFields: string;
begin
  Result := STR_BULK_INSERT_REQUIRES_INSERTABLE_FIELDS[GCurrentLocale];
end;

function MsgModelClassNotFound: string;
begin
  Result := STR_MODEL_CLASS_NOT_FOUND[GCurrentLocale];
end;

initialization
  // Auto-detect locale from environment variable ZENITH_LOCALE or DELTAMODEL_LOCALE
  if GetEnvironmentVariable('ZENITH_LOCALE') <> '' then
    SetDeltaLocaleByName(GetEnvironmentVariable('ZENITH_LOCALE'))
  else if GetEnvironmentVariable('DELTAMODEL_LOCALE') <> '' then
    SetDeltaLocaleByName(GetEnvironmentVariable('DELTAMODEL_LOCALE'))
  else
    GCurrentLocale := dlEnUS;

end.
