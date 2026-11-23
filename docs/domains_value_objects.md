# Domain Types & Reusable Value Objects (`DeltaModel.Domains`)

The `DeltaModel.Domains` unit provides a lightweight, zero-overhead architecture of **Domain Value Objects** and pure validation/sanitization/formatting functions for common business identifiers and contact fields:

- **Email** (`TDFEmail`, `TDFEmailRequired`)
- **CPF** (`TDFCPF`, `TDFCPFRequired`)
- **CNPJ — Numeric & Alphanumeric RFB IN 2.229/2024** (`TDFCNPJ`, `TDFCNPJRequired`)
- **Polymorphic Document (CPF / CNPJ)** (`TDFDocumento`, `TDFDocumentoRequired`)
- **Phone (E.164 & Brazilian 10/11-digit)** (`TDFTelefone`, `TDFTelefoneRequired`)
- **CEP (Brazilian 8-digit Postal Code)** (`TDFCEP`, `TDFCEPRequired`)

---

## 1. Architectural Principles

1. **Zero Extra Instance Memory Overhead**:
   Every `TDF*` domain class inherits directly from `TDFStringNull` (nullable) or `TDFStringRequired` (required) without introducing any extra instance fields.
   `TDFEmail.InstanceSize = TDFStringNull.InstanceSize` (80 bytes on x86_64; **0 bytes overhead**).
2. **Zero-RegExpr Hot Path**:
   All validators use single-pass character-scanning algorithms in pure Free Pascal, eliminating `TRegExpr` allocation overhead and achieving **0 bytes heap delta** across 600,000 validations.
3. **Automatic Canonical Sanitization on Assignment**:
   Assigning a value via `.Value := ...` or `.AsText := ...` automatically sanitizes valid formatted input into its canonical persistence representation while preserving malformed inputs containing illegal characters so `.IsValid` and `TDeltaModel.Validate` reliably reject them.
4. **Dual-Style Support (Approach A & Approach B)**:
   - **Approach A (Typed Field Properties)**: Use `TDFEmail`, `TDFCPF`, `TDFCNPJ`, `TDFDocumento`, `TDFTelefone`, `TDFCEP` (and their `*Required` counterparts) directly as published model properties. `TDeltaModel.Validate` automatically checks `.IsValid` on all non-null domain fields, and `DeltaAPISchema` emits OpenAPI `format` metadata automatically.
   - **Approach B (Flyweight Class Schema + Native `string` Properties)**: Declare `.Email`, `.CPF`, `.CNPJ`, `.CPF_CNPJ`, `.Phone` / `.Telefone`, and `.CEP` in `ConfigureSchema(ASchema: TDeltaModelSchema)` for zero-wrapper, bitmask-nullability models.

---

## 2. Value Objects Reference

| Nullable Class | Required Class | Alias | Default Size | OpenAPI `format` | Canonical Storage | Formatted Output |
| :--- | :--- | :--- | :---: | :--- | :--- | :--- |
| `TDFEmail` | `TDFEmailRequired` | `TDFEmailNull` | `254` | `email` | `user@example.org` (trimmed, lowercase) | `user@example.org` |
| `TDFCPF` | `TDFCPFRequired` | `TDFCPFNull` | `11` | `cpf` | `52998224725` (11 digits) | `529.982.247-25` |
| `TDFCNPJ` | `TDFCNPJRequired` | `TDFCNPJNull` | `14` | `cnpj` | `12ABC34501DE35` (14 alnum uppercase) | `12.ABC.345/01DE-35` |
| `TDFDocumento` | `TDFDocumentoRequired` | `TDFDocumentoNull` | `14` | `cpf-cnpj` | 11 digits (CPF) or 14 alnum (CNPJ) | `000.000.000-00` or `XX.XXX.XXX/XXXX-XX` |
| `TDFTelefone` | `TDFTelefoneRequired` | `TDFTelefoneNull` | `20` | `phone` | `+5511987654321` or `11987654321` | `+55 (11) 98765-4321` / `(11) 98765-4321` |
| `TDFCEP` | `TDFCEPRequired` | `TDFCEPNull` | `8` | `cep` | `01310100` (8 digits) | `01310-100` |

### Common Methods on All Domain Fields

- `IsValid: Boolean` — Nullable variants return `True` when `IsNull`; required variants return `False` when `IsNull`. Both validate the domain rules when populated.
- `Formatted: string` — Returns the human-readable formatted mask (or empty string if `IsNull`).
- `ValidateOrRaise` — Raises `EDeltaValidation` if `not IsValid`.
- `class function ValidateValue(const AValue: string): Boolean; static;`
- `class function Sanitize(const AValue: string): string; static;`
- `class function FormatValue(const AValue: string): string; static;`

### Domain-Specific Helpers

- `TDFCNPJ` / `TDFCNPJRequired`:
  - `IsAlphanumeric: Boolean` — Returns `True` when the CNPJ is valid and contains at least one letter (`A`–`Z`) in positions 1–12.
- `TDFDocumento` / `TDFDocumentoRequired`:
  - `Kind: TDocumentoKind` — Returns `dkCPF`, `dkCNPJ`, or `dkUnknown`.
  - `IsCPF: Boolean` / `IsCNPJ: Boolean` — Checks both kind and mathematical validity.
- `TDFTelefone` / `TDFTelefoneRequired`:
  - `ToE164(const ADefaultCountryCode: string = '55'): string` — Converts national numbers to E.164 format (e.g., `(11) 98765-4321` -> `+5511987654321`).

---

## 3. Alphanumeric CNPJ Specification (RFB IN 2.229/2024)

`TDFCNPJ`, `TDFDocumento`, and `IsValidCNPJ` natively support both traditional numeric CNPJs and **Alphanumeric CNPJs** defined by **RFB IN 2.229/2024 / NT COCAD 49/2024**:

- **Structure**: 14 characters total (`XX.XXX.XXX/XXXX-DV`):
  - Positions **1–12** (8-character root + 4-character branch): alphanumeric `[0-9A-Z]` (lowercase input is normalized to uppercase).
  - Positions **13–14** (check digits): strictly numeric `[0-9]`. Letters in check digit positions are rejected.
- **Sanitization**:
  - Uses `SomenteAlfanumerico` (`[0-9A-Z]`). **Letters are never stripped.**
- **Check Digit Algorithm (Modulo 11 with ASCII − 48)**:
  - Each character `C` maps to `Ord(C) - 48` (`'0'` = 0 .. `'9'` = 9, `'A'` = 17 .. `'Z'` = 42).
  - 1st check digit weights over positions 1–12: `[5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2]`.
  - 2nd check digit weights over positions 1–13: `[6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2]`.
  - `Remainder := Sum mod 11; if Remainder < 2 then Digit := 0 else Digit := 11 - Remainder;`
- **Verified Official Vector**:
  - Input: `12.ABC.345/01DE-35` (or `12.abc.345/01de-35`)
  - Sanitized: `12ABC34501DE35`
  - Valid: `True` (`IsAlphanumericCNPJ = True`)

---

## 4. Usage Examples

### Approach A: Typed Domain Fields on `TDeltaModel`

```pascal
uses
  DeltaModel, DeltaModel.Fields, DeltaModel.Domains;

type
  TCustomer = class(TDeltaModel)
  private
    FId: TDFInt64Required;
    FName: TDFStringRequired;
    FEmail: TDFEmailRequired;
    FDocument: TDFDocumentoRequired;
    FPhone: TDFTelefoneRequired;
    FPostalCode: TDFCEP;
  published
    property Id: TDFInt64Required read FId write FId;
    property Name: TDFStringRequired read FName write FName;
    property Email: TDFEmailRequired read FEmail write FEmail;
    property Document: TDFDocumentoRequired read FDocument write FDocument;
    property Phone: TDFTelefoneRequired read FPhone write FPhone;
    property PostalCode: TDFCEP read FPostalCode write FPostalCode;
  public
    procedure AfterConstruction; override;
  end;

procedure TCustomer.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'customers';
  FId.DBOptions := [dboPrimaryKey, dboAutoInc];
end;
```

```pascal
var
  C: TCustomer;
begin
  C := TCustomer.Create;
  try
    C.Name.AsText := 'Acme Logistics Ltd.';
    C.Email.AsText := '  Billing@Acme.Example  ';   // Stored as 'billing@acme.example'
    C.Document.AsText := '12.abc.345/01de-35';      // Stored as '12ABC34501DE35'
    C.Phone.AsText := '+55 (11) 98765-4321';        // Stored as '+5511987654321'
    C.PostalCode.AsText := '01310-100';             // Stored as '01310100'

    C.Validate; // Validates required fields + all domain rules automatically

    WriteLn(C.Document.Formatted); // '12.ABC.345/01DE-35'
    WriteLn(C.Phone.ToE164);       // '+5511987654321'
    WriteLn(C.PostalCode.Formatted); // '01310-100'
  finally
    C.Free;
  end;
end;
```

### Approach B: Flyweight Class Schema with Native Primitives

```pascal
type
  TFastSupplier = class(TDeltaModel)
  private
    FId: Int64;
    FEmail: string;
    FTaxId: string;
    FPhone: string;
    FZip: string;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Email: string read FEmail write FEmail;
    property TaxId: string read FTaxId write FTaxId;
    property Phone: string read FPhone write FPhone;
    property Zip: string read FZip write FZip;
  end;

class procedure TFastSupplier.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('suppliers');
  ASchema.Field('Id').AsPK.AutoInc;
  ASchema.Field('Email').Required.Email;
  ASchema.Field('TaxId').Required.CPF_CNPJ;
  ASchema.Field('Phone').Required.Phone;
  ASchema.Field('Zip').CEP;
end;
```

### Standalone Pure Functions

```pascal
if IsValidCNPJ('12.ABC.345/01DE-35') then
  WriteLn(FormatCNPJ('12abc34501de35')); // '12.ABC.345/01DE-35'

if IsValidTelefone('(11) 98765-4321') then
  WriteLn(TelefoneToE164('(11) 98765-4321')); // '+5511987654321'
```

---

## 5. Benchmarks & Resource Metrics

Measured with FPC 3.2.3 (`-O2`, x86_64 Linux) via `tests/test_domains_runner.lpr` (100,000 validations per domain, 600,000 total):

| Domain | Operations | Elapsed Time | Throughput |
| :--- | :---: | :---: | :---: |
| `IsValidEmailAddress` | 100,000 | 20 ms | ~5.00M ops/sec |
| `IsValidCPF` | 100,000 | 4 ms | ~25.00M ops/sec |
| `IsValidCNPJ` (Alphanumeric) | 100,000 | 6 ms | ~16.67M ops/sec |
| `IsValidDocumento` (Polymorphic) | 100,000 | 22 ms | ~4.55M ops/sec |
| `IsValidTelefone` (E.164 / BR) | 100,000 | 25 ms | ~4.00M ops/sec |
| `IsValidCEP` | 100,000 | 1 ms | ~100.00M ops/sec |
| **Total** | **600,000** | **78 ms** | **~7.69M ops/sec** |

- **Heap Consumption Delta** (`GetFPCHeapStatus.CurrHeapUsed`): **0 bytes** (`26,848 B` before -> `26,848 B` after).
- **Instance Memory Overhead**: **0 bytes** (`TDF*` = `80 B`, identical to `TDFStringNull` = `80 B`).

---

## 6. Running the Test Suite

```bash
mkdir -p /tmp/dm_domains_build
fpc -O2 -FE/tmp/dm_domains_build -FU/tmp/dm_domains_build \
    -Fu./src -Fu./tests/src -Fi./src \
    tests/test_domains_runner.lpr
/tmp/dm_domains_build/test_domains_runner -a --format=plain
```
