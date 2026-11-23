# DeltaModel ORM — Example 02: Domain Validation & Typed Model Collections (`02-validation`)

An interactive Lazarus LCL application demonstrating **DeltaValidator** domain rules (including tax document validation for CPF and CNPJ, email, URL, numeric ranges, and string length constraints) along with 1:N typed model lists (`TDeltaModelList`).

---

## 🚀 Key Features Demonstrated

- **Comprehensive Field Validators (`TEmployee` & `TCompany`)**:
  - `TValidatorItemNotEmpty`, `TValidatorItemMinLength`, `TValidatorItemMaxLength` for strings.
  - `TValidatorEmail` for RFC-compliant email addresses.
  - `TValidatorItemCPF` and `TValidatorItemCNPJ` for tax ID checksum verification.
  - `TValidatorItemUrl` for company website validation.
  - `TValidatorItemGreaterThanZero`, `TValidatorItemMaxValue`, and `TValidatorItemBetween` for numeric and currency fields (`salary`, `age`, `performanceScore`).
- **Hierarchical Collection Validation**:
  - `TCompany` contains `employees: TEmployeeList` (`TDeltaModelList` configured with `SetDeltaModelClass(TEmployee)`).
  - Calling `Company.Validate` validates both the parent `TCompany` rules and recursively validates every `TEmployee` item in `Company.employees`.
- **Collection JSON Serialization & Deserialization**:
  - Serializes a `TCompany` along with its `employees` array to JSON (`Co.ToJson`) and reconstructs the entire object graph from JSON (`Co.FromJson`).

---

## 🛠️ Build & Run

### Prerequisites
- Free Pascal Compiler (`fpc` 3.2.2+) and Lazarus LCL development libraries.

### Option 1: Compile with `fpc` (Clean Temporary Build)

```bash
mkdir -p /tmp/deltamodel_ex02
cd example/02-validation

fpc -MObjFPC -Scghi -O2 \
  -FE/tmp/deltamodel_ex02 \
  -FU/tmp/deltamodel_ex02 \
  -Fu../../src \
  -Fi../../src \
  -Fu/usr/lib64/lazarus/lcl/units/x86_64-linux \
  -Fu/usr/lib64/lazarus/lcl/units/x86_64-linux/gtk2 \
  -Fu/usr/lib64/lazarus/components/lazutils/lib/x86_64-linux \
  -Fu/usr/lib64/lazarus/components/freetype/lib/x86_64-linux \
  -Fu/usr/lib64/lazarus/packager/units/x86_64-linux \
  project1.lpr
```

### Option 2: Compile with Lazarus (`lazbuild`)

```bash
lazbuild --bm=Default example/02-validation/project1.lpi
```

### Running the Application

```bash
/tmp/deltamodel_ex02/project1
```

---

## 📖 Usage Guide

1. **Employee Tab**: Edit the employee name, email, and CPF, then click **Validate Employee** to test `TEmployee.Validate`.
2. **Company Tab**: Edit the company name, CNPJ, and website URL, then click **Validate Company** to validate the company and its nested employee list, or click **Serialize List** to generate the nested JSON payload.
3. **JSON Tab**: Inspect or edit the serialized JSON for `TCompany` + `employees` and click **Deserialize List** to parse the JSON array back into `TCompany.employees`.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
