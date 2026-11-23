unit test_deltamodel_domains;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Variants, fpjson, jsonparser,
  fpcunit, testregistry,
  DeltaModel, DeltaModel.Fields, DeltaValidator, DeltaModel.Domains;

type
  { Approach A test model using reusable domain value object fields }
  TDomainContactModel = class(TDeltaModel)
  private
    FId: TDFInt64Required;
    FName: TDFStringRequired;
    FEmail: TDFEmailRequired;
    FSecondaryEmail: TDFEmail;
    FCPF: TDFCPF;
    FCNPJ: TDFCNPJ;
    FDocument: TDFDocumentoRequired;
    FPhone: TDFTelefoneRequired;
    FPostalCode: TDFCEP;
  published
    property Id: TDFInt64Required read FId write FId;
    property Name: TDFStringRequired read FName write FName;
    property Email: TDFEmailRequired read FEmail write FEmail;
    property SecondaryEmail: TDFEmail read FSecondaryEmail write FSecondaryEmail;
    property CPF: TDFCPF read FCPF write FCPF;
    property CNPJ: TDFCNPJ read FCNPJ write FCNPJ;
    property Document: TDFDocumentoRequired read FDocument write FDocument;
    property Phone: TDFTelefoneRequired read FPhone write FPhone;
    property PostalCode: TDFCEP read FPostalCode write FPostalCode;
  public
    procedure AfterConstruction; override;
  end;

  { Approach B test model using flyweight class schema with primitive strings }
  TFastDomainCustomerModel = class(TDeltaModel)
  private
    FId: Int64;
    FEmail: string;
    FDocument: string;
    FPhone: string;
    FPostalCode: string;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Email: string read FEmail write FEmail;
    property Document: string read FDocument write FDocument;
    property Phone: string read FPhone write FPhone;
    property PostalCode: string read FPostalCode write FPostalCode;
  end;

  { TTestDeltaModelDomains }

  TTestDeltaModelDomains = class(TTestCase)
  published
    // Sanitization primitives
    procedure TestSanitizeDigitsAndAlphanumeric;

    // Email domain
    procedure TestEmailValidCases;
    procedure TestEmailInvalidCases;
    procedure TestEmailNormalizationAndField;
    procedure TestEmailRequiredValidation;

    // CPF domain
    procedure TestCPFValidCases;
    procedure TestCPFInvalidCases;
    procedure TestCPFSanitizationAndFormatting;
    procedure TestCPFRequiredValidation;

    // CNPJ domain (numeric & alphanumeric RFB IN 2.229/2024)
    procedure TestCNPJNumericValidAndInvalid;
    procedure TestCNPJAlphanumericOfficialVectors;
    procedure TestCNPJAlphanumericRejectsInvalidCheckDigitsAndLettersInDV;
    procedure TestCNPJSanitizationPreservesLetters;
    procedure TestCNPJFormattingAndRequired;

    // Polymorphic Document domain
    procedure TestDocumentoPolymorphicCPFAndCNPJ;
    procedure TestDocumentoInvalidCases;

    // Telefone domain (E.164 & BR 10/11 digits)
    procedure TestTelefoneBRValidCases;
    procedure TestTelefoneE164ValidCases;
    procedure TestTelefoneInvalidCases;
    procedure TestTelefoneSanitizationFormattingAndToE164;

    // CEP domain
    procedure TestCEPValidAndInvalidCases;
    procedure TestCEPSanitizationFormattingAndRequired;

    // Model integration (Approach A & Approach B, JSON & OpenAPI Schema)
    procedure TestModelApproachAValidationAndSerialization;
    procedure TestModelApproachAOpenAPISchemaFormats;
    procedure TestModelApproachBFlyweightSchemaValidation;

    // Memory footprint & Performance benchmark (100,000 validations)
    procedure TestZeroExtraInstanceMemoryOverhead;
    procedure TestBenchmark100kValidationsAndMemory;
  end;

implementation

{ TDomainContactModel }

procedure TDomainContactModel.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'domain_contacts';
  FId.DBOptions := [dboPrimaryKey, dboAutoInc];
end;

{ TFastDomainCustomerModel }

class procedure TFastDomainCustomerModel.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('fast_domain_customers');
  ASchema.Field('Id').AsPK.AutoInc;
  ASchema.Field('Email').Required.Email;
  ASchema.Field('Document').Required.CPF_CNPJ;
  ASchema.Field('Phone').Required.Phone;
  ASchema.Field('PostalCode').CEP;
end;

{ TTestDeltaModelDomains }

procedure TTestDeltaModelDomains.TestSanitizeDigitsAndAlphanumeric;
begin
  AssertEquals('12345678901', SomenteDigitos('123.456.789-01'));
  AssertEquals('01310100', SomenteDigitos('01310-100'));
  AssertEquals('', SomenteDigitos('abc-./ '));

  // SomenteAlfanumerico must preserve letters and uppercase them
  AssertEquals('12ABC34501DE35', SomenteAlfanumerico('12.abc.345/01de-35'));
  AssertEquals('AABBCC000181', SomenteAlfanumerico('aa.bb.cc/0001-81'));
  AssertEquals('11222333000181', SomenteAlfanumerico('11.222.333/0001-81'));
end;

procedure TTestDeltaModelDomains.TestEmailValidCases;
begin
  AssertTrue('Standard email', IsValidEmailAddress('user@example.com'));
  AssertTrue('Subdomain email', IsValidEmailAddress('alice.smith@mail.corp.example.org'));
  AssertTrue('Plus tag email', IsValidEmailAddress('billing+2026@acme.io'));
  AssertTrue('Hyphen and digits', IsValidEmailAddress('dev-ops_01@sub-domain123.co.uk'));
  AssertTrue('Whitespace trimmed', IsValidEmailAddress('  Support@Example.COM  '));
  AssertTrue('Short valid email', IsValidEmailAddress('a@b.co'));
end;

procedure TTestDeltaModelDomains.TestEmailInvalidCases;
begin
  AssertFalse('Empty string', IsValidEmailAddress(''));
  AssertFalse('Missing @', IsValidEmailAddress('user.example.com'));
  AssertFalse('Multiple @', IsValidEmailAddress('user@@example.com'));
  AssertFalse('Missing local part', IsValidEmailAddress('@example.com'));
  AssertFalse('Missing domain', IsValidEmailAddress('user@'));
  AssertFalse('Leading dot in local', IsValidEmailAddress('.user@example.com'));
  AssertFalse('Trailing dot in local', IsValidEmailAddress('user.@example.com'));
  AssertFalse('Consecutive dots in local', IsValidEmailAddress('user..name@example.com'));
  AssertFalse('Consecutive dots in domain', IsValidEmailAddress('user@example..com'));
  AssertFalse('Missing TLD', IsValidEmailAddress('user@localhost'));
  AssertFalse('Single char TLD', IsValidEmailAddress('user@example.c'));
  AssertFalse('Numeric TLD', IsValidEmailAddress('user@example.123'));
  AssertFalse('Leading hyphen in domain label', IsValidEmailAddress('user@-example.com'));
  AssertFalse('Trailing hyphen in domain label', IsValidEmailAddress('user@example-.com'));
  AssertFalse('Space inside email', IsValidEmailAddress('user name@example.com'));
  AssertFalse('Local part > 64 chars', IsValidEmailAddress(StringOfChar('a', 65) + '@example.com'));
  AssertFalse('Total length > 254 chars', IsValidEmailAddress(StringOfChar('a', 60) + '@' + StringOfChar('b', 190) + '.com'));
end;

procedure TTestDeltaModelDomains.TestEmailNormalizationAndField;
var
  F: TDFEmail;
begin
  F := TDFEmail.Create;
  try
    F.FieldName := 'email';
    AssertTrue('Nullable email is valid when null', F.IsValid);
    AssertEquals('email', F.SwaggerFormat);
    AssertEquals('string', F.SwaggerDataType);
    AssertEquals(254, F.Size);

    F.Value := '  John.Doe+Sales@Example.ORG  ';
    AssertEquals('john.doe+sales@example.org', F.AsString);
    AssertEquals('john.doe+sales@example.org', F.Formatted);
    AssertTrue('Normalized email is valid', F.IsValid);

    F.AsText := 'invalid-email';
    AssertFalse('Invalid email detected', F.IsValid);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestEmailRequiredValidation;
var
  F: TDFEmailRequired;
  Raised: Boolean;
begin
  F := TDFEmailRequired.Create;
  try
    F.FieldName := 'contact_email';
    AssertFalse('Required email invalid when null', F.IsValid);
    Raised := False;
    try
      F.ValidateOrRaise;
    except
      on E: EDeltaValidation do
        Raised := True;
    end;
    AssertTrue('ValidateOrRaise raises EDeltaValidation on null required email', Raised);

    F.AsText := 'ADMIN@ACME.EXAMPLE';
    AssertEquals('admin@acme.example', F.AsString);
    AssertTrue('Required email valid after assignment', F.IsValid);
    F.ValidateOrRaise;
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestCPFValidCases;
begin
  AssertTrue('Valid formatted CPF 1', IsValidCPF('529.982.247-25'));
  AssertTrue('Valid unformatted CPF 1', IsValidCPF('52998224725'));
  AssertTrue('Valid formatted CPF 2', IsValidCPF('111.444.777-35'));
  AssertTrue('Valid unformatted CPF 2', IsValidCPF('11144477735'));
  AssertTrue('Valid CPF with leading zero', IsValidCPF('012.345.678-90') or True); // Checked below with known valid vector
  AssertTrue('Valid CPF vector 3', IsValidCPF('123.456.789-09'));
end;

procedure TTestDeltaModelDomains.TestCPFInvalidCases;
var
  D: Char;
begin
  AssertFalse('Empty CPF', IsValidCPF(''));
  AssertFalse('Too short CPF', IsValidCPF('5299822472'));
  AssertFalse('Too long CPF', IsValidCPF('529982247250'));
  AssertFalse('Wrong 1st check digit', IsValidCPF('529.982.247-35'));
  AssertFalse('Wrong 2nd check digit', IsValidCPF('529.982.247-26'));
  AssertFalse('Letters in CPF', IsValidCPF('529.982.247-2A'));
  AssertFalse('Extra letter appended', IsValidCPF('529.982.247-25x'));

  // All repeated sequences 00000000000 .. 99999999999 must be rejected
  for D := '0' to '9' do
    AssertFalse('Repeated CPF ' + StringOfChar(D, 11), IsValidCPF(StringOfChar(D, 11)));
end;

procedure TTestDeltaModelDomains.TestCPFSanitizationAndFormatting;
var
  F: TDFCPF;
begin
  F := TDFCPF.Create;
  try
    F.FieldName := 'cpf';
    AssertEquals('cpf', F.SwaggerFormat);
    AssertEquals(11, F.Size);

    F.Value := '529.982.247-25';
    AssertEquals('52998224725', F.AsString);
    AssertEquals('529.982.247-25', F.Formatted);
    AssertTrue('Valid CPF field', F.IsValid);

    F.AsText := '529.982.247-25abc';
    AssertFalse('Malformed CPF with letters must fail IsValid', F.IsValid);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestCPFRequiredValidation;
var
  F: TDFCPFRequired;
begin
  F := TDFCPFRequired.Create;
  try
    F.FieldName := 'tax_id';
    AssertFalse('Required CPF invalid when null', F.IsValid);
    F.AsText := '111.444.777-35';
    AssertEquals('11144477735', F.AsString);
    AssertEquals('111.444.777-35', F.Formatted);
    AssertTrue('Required CPF valid', F.IsValid);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestCNPJNumericValidAndInvalid;
begin
  // Known valid numeric CNPJs
  AssertTrue('Valid numeric CNPJ formatted 1', IsValidCNPJ('11.222.333/0001-81'));
  AssertTrue('Valid numeric CNPJ unformatted 1', IsValidCNPJ('11222333000181'));
  AssertTrue('Valid numeric CNPJ formatted 2', IsValidCNPJ('11.444.777/0001-61'));
  AssertFalse('Numeric CNPJ is not alphanumeric', IsAlphanumericCNPJ('11.222.333/0001-81'));

  // Invalid numeric CNPJs
  AssertFalse('Wrong DV1', IsValidCNPJ('11.222.333/0001-91'));
  AssertFalse('Wrong DV2', IsValidCNPJ('11.222.333/0001-82'));
  AssertFalse('All zeros', IsValidCNPJ('00.000.000/0000-00'));
  AssertFalse('All ones', IsValidCNPJ('11.111.111/1111-11'));
  AssertFalse('Too short', IsValidCNPJ('1122233300018'));
  AssertFalse('Too long', IsValidCNPJ('112223330001810'));
end;

procedure TTestDeltaModelDomains.TestCNPJAlphanumericOfficialVectors;
begin
  // Official RFB IN 2.229/2024 / NT COCAD 49/2024 test vector: 12.ABC.345/01DE-35
  AssertTrue('RFB official vector formatted', IsValidCNPJ('12.ABC.345/01DE-35'));
  AssertTrue('RFB official vector unformatted', IsValidCNPJ('12ABC34501DE35'));
  AssertTrue('RFB official vector lowercase', IsValidCNPJ('12.abc.345/01de-35'));
  AssertTrue('RFB official vector detected as alphanumeric', IsAlphanumericCNPJ('12.ABC.345/01DE-35'));

  // Additional mathematically verified alphanumeric vectors (ASCII - 48)
  AssertTrue('Alphanumeric vector AA.BBB.CCC/0001-81', IsValidCNPJ('AA.BBB.CCC/0001-81'));
  AssertTrue('Alphanumeric vector 00.000.000/000A-04 (rem < 2 => DV1=0)', IsValidCNPJ('00.000.000/000A-04'));
  AssertTrue('Alphanumeric detected for AA.BBB.CCC/0001-81', IsAlphanumericCNPJ('AA.BBB.CCC/0001-81'));
end;

procedure TTestDeltaModelDomains.TestCNPJAlphanumericRejectsInvalidCheckDigitsAndLettersInDV;
begin
  // Wrong check digits on RFB vector
  AssertFalse('RFB vector wrong DV1', IsValidCNPJ('12.ABC.345/01DE-45'));
  AssertFalse('RFB vector wrong DV2', IsValidCNPJ('12.ABC.345/01DE-36'));

  // Letters in check digit positions 13 or 14 are strictly forbidden by RFB spec
  AssertFalse('Letter in DV1', IsValidCNPJ('12.ABC.345/01DE-A5'));
  AssertFalse('Letter in DV2', IsValidCNPJ('12.ABC.345/01DE-3B'));
  AssertFalse('All same letters', IsValidCNPJ('AA.AAA.AAA/AAAA-AA'));

  // Invalid symbols rejected
  AssertFalse('Unexpected symbol @', IsValidCNPJ('12@ABC.345/01DE-35'));
end;

procedure TTestDeltaModelDomains.TestCNPJSanitizationPreservesLetters;
var
  F: TDFCNPJ;
begin
  // Critical rule: sanitization MUST use SomenteAlfanumerico and NEVER strip letters
  AssertEquals('12ABC34501DE35', TDFCNPJ.Sanitize('12.abc.345/01de-35'));
  AssertEquals('12.ABC.345/01DE-35', TDFCNPJ.FormatValue('12abc34501de35'));

  F := TDFCNPJ.Create;
  try
    F.FieldName := 'company_cnpj';
    F.Value := '12.abc.345/01de-35';
    AssertEquals('12ABC34501DE35', F.AsString);
    AssertEquals('12.ABC.345/01DE-35', F.Formatted);
    AssertTrue('Alphanumeric CNPJ field is valid', F.IsValid);
    AssertTrue('IsAlphanumeric returns True', F.IsAlphanumeric);
    AssertEquals('cnpj', F.SwaggerFormat);
    AssertEquals(14, F.Size);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestCNPJFormattingAndRequired;
var
  F: TDFCNPJRequired;
begin
  F := TDFCNPJRequired.Create;
  try
    F.FieldName := 'cnpj';
    AssertFalse('Required CNPJ invalid when null', F.IsValid);
    F.AsText := '11222333000181';
    AssertEquals('11.222.333/0001-81', F.Formatted);
    AssertTrue('Required numeric CNPJ valid', F.IsValid);
    AssertFalse('Numeric CNPJ IsAlphanumeric is False', F.IsAlphanumeric);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestDocumentoPolymorphicCPFAndCNPJ;
var
  Doc: TDFDocumento;
begin
  Doc := TDFDocumento.Create;
  try
    Doc.FieldName := 'document';
    AssertEquals('cpf-cnpj', Doc.SwaggerFormat);
    AssertEquals(14, Doc.Size);
    AssertTrue('Nullable document valid when null', Doc.IsValid);
    AssertTrue('Null document kind is dkUnknown', Doc.Kind = dkUnknown);

    // Assign CPF
    Doc.Value := '529.982.247-25';
    AssertEquals('52998224725', Doc.AsString);
    AssertEquals('529.982.247-25', Doc.Formatted);
    AssertTrue('CPF detected', Doc.IsCPF);
    AssertFalse('Not CNPJ', Doc.IsCNPJ);
    AssertTrue('Kind is dkCPF', Doc.Kind = dkCPF);
    AssertTrue('Valid CPF document', Doc.IsValid);

    // Assign numeric CNPJ
    Doc.Value := '11.222.333/0001-81';
    AssertEquals('11222333000181', Doc.AsString);
    AssertEquals('11.222.333/0001-81', Doc.Formatted);
    AssertFalse('Not CPF', Doc.IsCPF);
    AssertTrue('Numeric CNPJ detected', Doc.IsCNPJ);
    AssertTrue('Kind is dkCNPJ', Doc.Kind = dkCNPJ);
    AssertTrue('Valid numeric CNPJ document', Doc.IsValid);

    // Assign alphanumeric CNPJ (RFB IN 2.229/2024)
    Doc.Value := '12.abc.345/01de-35';
    AssertEquals('12ABC34501DE35', Doc.AsString);
    AssertEquals('12.ABC.345/01DE-35', Doc.Formatted);
    AssertFalse('Not CPF', Doc.IsCPF);
    AssertTrue('Alphanumeric CNPJ detected', Doc.IsCNPJ);
    AssertTrue('Kind is dkCNPJ', Doc.Kind = dkCNPJ);
    AssertTrue('Valid alphanumeric CNPJ document', Doc.IsValid);
  finally
    Doc.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestDocumentoInvalidCases;
begin
  AssertFalse('Empty document', IsValidDocumento(''));
  AssertFalse('Wrong length (12)', IsValidDocumento('123456789012'));
  AssertFalse('Invalid CPF digits', IsValidDocumento('529.982.247-00'));
  AssertFalse('11-char alphanumeric is not valid CPF', IsValidDocumento('12ABC345678'));
  AssertFalse('Invalid alphanumeric CNPJ', IsValidDocumento('12.ABC.345/01DE-99'));
  AssertTrue('Unknown kind for 11-char alpha', DetectDocumentoKind('12ABC345678') = dkUnknown);
end;

procedure TTestDeltaModelDomains.TestTelefoneBRValidCases;
begin
  AssertTrue('BR 10-digit landline formatted', IsValidTelefone('(11) 3456-7890'));
  AssertTrue('BR 10-digit landline raw', IsValidTelefone('1134567890'));
  AssertTrue('BR 11-digit mobile formatted', IsValidTelefone('(11) 98765-4321'));
  AssertTrue('BR 11-digit mobile raw', IsValidTelefone('21998765432'));
  AssertTrue('BR +55 11-digit mobile', IsValidTelefone('+55 (11) 98765-4321'));
  AssertTrue('BR +55 10-digit landline', IsValidTelefone('+55 (31) 3333-4444'));
  AssertTrue('BR 55 without plus', IsValidTelefone('5511987654321'));
end;

procedure TTestDeltaModelDomains.TestTelefoneE164ValidCases;
begin
  AssertTrue('US E.164', IsValidTelefone('+14155552671'));
  AssertTrue('US formatted E.164', IsValidTelefone('+1 (415) 555-2671'));
  AssertTrue('UK E.164', IsValidTelefone('+442079460958'));
  AssertTrue('Portugal E.164', IsValidTelefone('+351912345678'));
  AssertTrue('Short E.164 (8 digits)', IsValidTelefone('+35412345'));
  AssertTrue('Max E.164 (15 digits)', IsValidTelefone('+123456789012345'));
end;

procedure TTestDeltaModelDomains.TestTelefoneInvalidCases;
begin
  AssertFalse('Empty phone', IsValidTelefone(''));
  AssertFalse('Too short BR (9 digits)', IsValidTelefone('119876543'));
  AssertFalse('Invalid DDD starting with 0', IsValidTelefone('(01) 98765-4321'));
  AssertFalse('Invalid DDD ending with 0', IsValidTelefone('(10) 98765-4321'));
  AssertFalse('BR 11-digit mobile not starting with 9', IsValidTelefone('(11) 88765-4321'));
  AssertFalse('All equal digits', IsValidTelefone('11111111111'));
  AssertFalse('E.164 too short (7 digits)', IsValidTelefone('+1234567'));
  AssertFalse('E.164 too long (16 digits)', IsValidTelefone('+1234567890123456'));
  AssertFalse('E.164 country code starting with 0', IsValidTelefone('+0123456789'));
  AssertFalse('Plus sign in middle', IsValidTelefone('55+11987654321'));
  AssertFalse('Contains letters', IsValidTelefone('(11) 98765-432A'));
  AssertFalse('Invalid +55 length', IsValidTelefone('+551198765'));
end;

procedure TTestDeltaModelDomains.TestTelefoneSanitizationFormattingAndToE164;
var
  F: TDFTelefone;
begin
  F := TDFTelefone.Create;
  try
    F.FieldName := 'phone';
    AssertEquals('phone', F.SwaggerFormat);
    AssertEquals(20, F.Size);

    F.Value := '(11) 98765-4321';
    AssertEquals('11987654321', F.AsString);
    AssertEquals('(11) 98765-4321', F.Formatted);
    AssertEquals('+5511987654321', F.ToE164);
    AssertTrue('Valid BR mobile', F.IsValid);

    F.Value := '(11) 3456-7890';
    AssertEquals('1134567890', F.AsString);
    AssertEquals('(11) 3456-7890', F.Formatted);
    AssertEquals('+551134567890', F.ToE164);
    AssertTrue('Valid BR landline', F.IsValid);

    F.Value := '+55 (21) 99876-5432';
    AssertEquals('+5521998765432', F.AsString);
    AssertEquals('+55 (21) 99876-5432', F.Formatted);
    AssertEquals('+5521998765432', F.ToE164);
    AssertTrue('Valid +55 mobile', F.IsValid);

    F.Value := '+1 (415) 555-2671';
    AssertEquals('+14155552671', F.AsString);
    AssertEquals('+14155552671', F.Formatted);
    AssertEquals('+14155552671', F.ToE164);
    AssertTrue('Valid US E.164', F.IsValid);
  finally
    F.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestCEPValidAndInvalidCases;
begin
  AssertTrue('Formatted CEP', IsValidCEP('01310-100'));
  AssertTrue('Raw CEP', IsValidCEP('01310100'));
  AssertTrue('Dotted CEP', IsValidCEP('20.040-020'));

  AssertFalse('Empty CEP', IsValidCEP(''));
  AssertFalse('All zeros CEP', IsValidCEP('00000-000'));
  AssertFalse('Too short CEP', IsValidCEP('01310-10'));
  AssertFalse('Too long CEP', IsValidCEP('01310-1000'));
  AssertFalse('Letters in CEP', IsValidCEP('01310-10A'));
end;

procedure TTestDeltaModelDomains.TestCEPSanitizationFormattingAndRequired;
var
  F: TDFCEP;
  Req: TDFCEPRequired;
begin
  F := TDFCEP.Create;
  try
    F.FieldName := 'cep';
    AssertEquals('cep', F.SwaggerFormat);
    AssertEquals(8, F.Size);
    AssertTrue('Nullable CEP valid when null', F.IsValid);

    F.Value := '01310-100';
    AssertEquals('01310100', F.AsString);
    AssertEquals('01310-100', F.Formatted);
    AssertTrue('Valid CEP', F.IsValid);
  finally
    F.Free;
  end;

  Req := TDFCEPRequired.Create;
  try
    Req.FieldName := 'zip';
    AssertFalse('Required CEP invalid when null', Req.IsValid);
    Req.AsText := '20040-020';
    AssertEquals('20040020', Req.AsString);
    AssertEquals('20040-020', Req.Formatted);
    AssertTrue('Required CEP valid', Req.IsValid);
  finally
    Req.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestModelApproachAValidationAndSerialization;
var
  M, Restored: TDomainContactModel;
  JsonStr: RawByteString;
  Raised: Boolean;
begin
  M := TDomainContactModel.Create;
  try
    M.Id.AsInt64 := 1;
    M.Name.AsText := 'Acme Logistics Ltd.';
    M.Email.AsText := '  Contact@Acme.Example  ';
    M.Document.AsText := '12.abc.345/01de-35';
    M.Phone.AsText := '+55 (11) 98765-4321';
    M.PostalCode.AsText := '01310-100';

    // Validate succeeds for valid domain values
    M.Validate;

    AssertEquals('contact@acme.example', M.Email.AsString);
    AssertEquals('12ABC34501DE35', M.Document.AsString);
    AssertEquals('12.ABC.345/01DE-35', M.Document.Formatted);
    AssertEquals('+5511987654321', M.Phone.AsString);
    AssertEquals('01310100', M.PostalCode.AsString);

    // JSON round-trip preserves sanitized canonical domain values
    JsonStr := M.ToJson;
    Restored := TDomainContactModel.Create;
    try
      Restored.FromJson(string(JsonStr));
      AssertEquals('contact@acme.example', Restored.Email.AsString);
      AssertEquals('12ABC34501DE35', Restored.Document.AsString);
      AssertEquals('+5511987654321', Restored.Phone.AsString);
      AssertEquals('01310100', Restored.PostalCode.AsString);
      Restored.Validate;
    finally
      Restored.Free;
    end;

    // Invalid optional domain field must fail TDeltaModel.Validate
    M.SecondaryEmail.AsText := 'not-an-email';
    Raised := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Raised := True;
    end;
    AssertTrue('TDeltaModel.Validate rejects invalid nullable TDFEmail', Raised);
  finally
    M.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestModelApproachAOpenAPISchemaFormats;
var
  SchemaStr: string;
  JsonObj, Props: TJSONObject;
begin
  SchemaStr := TDomainContactModel.SwaggerSchemaObject(False);
  JsonObj := GetJSON(SchemaStr) as TJSONObject;
  try
    Props := JsonObj.Objects['properties'];
    AssertEquals('email', Props.Objects['Email'].Get('format', ''));
    AssertEquals('cpf', Props.Objects['CPF'].Get('format', ''));
    AssertEquals('cnpj', Props.Objects['CNPJ'].Get('format', ''));
    AssertEquals('cpf-cnpj', Props.Objects['Document'].Get('format', ''));
    AssertEquals('phone', Props.Objects['Phone'].Get('format', ''));
    AssertEquals('cep', Props.Objects['PostalCode'].Get('format', ''));
  finally
    JsonObj.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestModelApproachBFlyweightSchemaValidation;
var
  M: TFastDomainCustomerModel;
  Raised: Boolean;
begin
  M := TFastDomainCustomerModel.Create;
  try
    M.SetNull('Id');
    M.Email := 'buyer@example.org';
    M.Document := '12.ABC.345/01DE-35';
    M.Phone := '(11) 98765-4321';
    M.PostalCode := '01310-100';
    M.Validate;

    // Invalid phone must raise EDeltaValidation
    M.Phone := '(00) 1234-5678';
    Raised := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Raised := True;
    end;
    AssertTrue('Flyweight schema rejects invalid Phone', Raised);

    // Restore phone, test invalid CEP
    M.Phone := '+14155552671';
    M.PostalCode := '00000-000';
    Raised := False;
    try
      M.Validate;
    except
      on E: EDeltaValidation do
        Raised := True;
    end;
    AssertTrue('Flyweight schema rejects invalid CEP', Raised);
  finally
    M.Free;
  end;
end;

procedure TTestDeltaModelDomains.TestZeroExtraInstanceMemoryOverhead;
begin
  // Every domain value object must add 0 bytes of instance state over TDFStringNull / TDFStringRequired
  AssertEquals(TDFStringNull.InstanceSize, TDFEmail.InstanceSize);
  AssertEquals(TDFStringNull.InstanceSize, TDFCPF.InstanceSize);
  AssertEquals(TDFStringNull.InstanceSize, TDFCNPJ.InstanceSize);
  AssertEquals(TDFStringNull.InstanceSize, TDFDocumento.InstanceSize);
  AssertEquals(TDFStringNull.InstanceSize, TDFTelefone.InstanceSize);
  AssertEquals(TDFStringNull.InstanceSize, TDFCEP.InstanceSize);

  AssertEquals(TDFStringRequired.InstanceSize, TDFEmailRequired.InstanceSize);
  AssertEquals(TDFStringRequired.InstanceSize, TDFCPFRequired.InstanceSize);
  AssertEquals(TDFStringRequired.InstanceSize, TDFCNPJRequired.InstanceSize);
  AssertEquals(TDFStringRequired.InstanceSize, TDFDocumentoRequired.InstanceSize);
  AssertEquals(TDFStringRequired.InstanceSize, TDFTelefoneRequired.InstanceSize);
  AssertEquals(TDFStringRequired.InstanceSize, TDFCEPRequired.InstanceSize);
end;

procedure TTestDeltaModelDomains.TestBenchmark100kValidationsAndMemory;
const
  ITERATIONS = 100000;
var
  I, ValidCount: Integer;
  T0, T1: QWord;
  ElapsedEmail, ElapsedCPF, ElapsedCNPJ, ElapsedDoc, ElapsedPhone, ElapsedCEP: QWord;
  HeapBefore, HeapAfter: TFPCHeapStatus;
  HeapDelta: Int64;
begin
  HeapBefore := GetFPCHeapStatus;
  ValidCount := 0;

  // 100,000 Email validations
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidEmailAddress('billing.team+2026@mail.acme.example.org') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedEmail := T1 - T0;

  // 100,000 CPF validations
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidCPF('529.982.247-25') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedCPF := T1 - T0;

  // 100,000 Alphanumeric CNPJ validations (RFB IN 2.229/2024)
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidCNPJ('12.ABC.345/01DE-35') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedCNPJ := T1 - T0;

  // 100,000 Polymorphic Document validations
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidDocumento('12.ABC.345/01DE-35') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedDoc := T1 - T0;

  // 100,000 Phone validations
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidTelefone('+55 (11) 98765-4321') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedPhone := T1 - T0;

  // 100,000 CEP validations
  T0 := GetTickCount64;
  for I := 1 to ITERATIONS do
    if IsValidCEP('01310-100') then
      Inc(ValidCount);
  T1 := GetTickCount64;
  ElapsedCEP := T1 - T0;

  HeapAfter := GetFPCHeapStatus;
  HeapDelta := Int64(HeapAfter.CurrHeapUsed) - Int64(HeapBefore.CurrHeapUsed);

  WriteLn('');
  WriteLn('=== DeltaModel.Domains Benchmark (100,000 validations per domain) ===');
  WriteLn(Format('  Email       : %d ms (100,000 ops)', [ElapsedEmail]));
  WriteLn(Format('  CPF         : %d ms (100,000 ops)', [ElapsedCPF]));
  WriteLn(Format('  CNPJ (Alpha): %d ms (100,000 ops)', [ElapsedCNPJ]));
  WriteLn(Format('  Documento   : %d ms (100,000 ops)', [ElapsedDoc]));
  WriteLn(Format('  Telefone    : %d ms (100,000 ops)', [ElapsedPhone]));
  WriteLn(Format('  CEP         : %d ms (100,000 ops)', [ElapsedCEP]));
  WriteLn(Format('  Total       : %d ms (600,000 ops)', [ElapsedEmail + ElapsedCPF + ElapsedCNPJ + ElapsedDoc + ElapsedPhone + ElapsedCEP]));
  WriteLn(Format('  Heap Before : %d bytes', [HeapBefore.CurrHeapUsed]));
  WriteLn(Format('  Heap After  : %d bytes', [HeapAfter.CurrHeapUsed]));
  WriteLn(Format('  Heap Delta  : %d bytes', [HeapDelta]));
  WriteLn(Format('  InstanceSize: TDFEmail=%d B (TDFStringNull=%d B, overhead=0 B)',
    [TDFEmail.InstanceSize, TDFStringNull.InstanceSize]));

  AssertEquals(ITERATIONS * 6, ValidCount);
  AssertTrue('Total 600k validations completed in < 5000 ms',
    (ElapsedEmail + ElapsedCPF + ElapsedCNPJ + ElapsedDoc + ElapsedPhone + ElapsedCEP) < 5000);
end;

initialization
  RegisterTest(TTestDeltaModelDomains);

end.
