unit DeltaModelMessages;

{$mode ObjFPC}{$H+}

interface

resourcestring
  //Model
  DeltaModelClassNotAssigned = 'DeltaModelClass not assigned to DeltaModelList.';

  //Validator
  ValidationFailedForField = 'Validation failed for field %s: %s';
  ValueCannotBeEmpty = 'Value cannot be empty.';
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

  //ORM
  FieldIsRequired = 'Field %s is required.';

implementation

end.
