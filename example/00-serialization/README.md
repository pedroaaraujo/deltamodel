# DeltaModel ORM — Example 00: Serialization, Validation & OpenAPI Schema (`00-serialization`)

An interactive Lazarus LCL desktop application demonstrating **DeltaModel** JSON serialization (`ToJson`), deserialization (`FromJson`), declarative field validation (`DeltaValidator`), nested models, 1:1 `TDFHasOne` relationships, and OpenAPI 3.1 schema generation (`SwaggerSchemaObject`).

---

## 🚀 Key Features Demonstrated

- **Complete Data Type Coverage (`TUser`)**:
  - `TDFUUIDNull` (`id`), `TDFStringRequired` (`name`, `email`), `TDFIntNull` (`age`), `TDFInt64Null` (`bigCode`), `TDFDoubleNull` (`score`), `TDFCurrencyRequired` (`salary`), `TDFDateNull` (`birthDate`), `TDFTimeNull` (`loginTime`), `TDFBooleanRequired` (`active`), `TDFDateTimeNull` (`created`), and `TDFTextNull` (`bio`).
- **Nested & Relational Models**:
  - Embedded value object (`address: TAddress` with `city`, `state`, `zipcode`).
  - 1:1 `TDFHasOne` relation (`profile.References(TUserProfile)` with `title` and `department`).
- **Declarative Validation (`TUser.Validate`)**:
  - Built-in required field validation (`inherited Validate`).
  - Rule composition with `TValidatorItemNotEmpty`, `TValidatorItemMinLength`, `TValidatorItemMaxLength`, `TValidatorEmail`, `TValidatorItemMinValue`, `TValidatorItemMaxValue`, and `TValidatorItemBetween`.
- **OpenAPI 3.1 Schema Introspection**:
  - One-click generation of the complete JSON Schema via `TUser.SwaggerSchemaObject`.

---

## 🛠️ Build & Run

### Prerequisites
- Free Pascal Compiler (`fpc` 3.2.2+) and Lazarus LCL development libraries (`lazbuild`).

### Option 1: Compile with `fpc` (Clean Temporary Build)

```bash
mkdir -p /tmp/deltamodel_ex00
cd example/00-serialization

fpc -MObjFPC -Scghi -O2 \
  -FE/tmp/deltamodel_ex00 \
  -FU/tmp/deltamodel_ex00 \
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
lazbuild --bm=Default example/00-serialization/project1.lpi
```

### Running the Application

```bash
/tmp/deltamodel_ex00/project1
```

---

## 📖 Usage Guide

The application window provides four interactive tabs:
1. **Serialization**: Populates a `TUser` instance with all data types, nested `TAddress`, and `TUserProfile`, then renders the serialized JSON payload.
2. **Deserialization**: Parses a JSON payload from the text memo into a `TUser` instance and populates the UI controls.
3. **Validation**: Runs `User.Validate` against the form inputs and displays validation success or detailed `EDeltaValidation` error messages.
4. **Schema**: Calls `TUser.SwaggerSchemaObject` to output the OpenAPI 3.1 JSON Schema representation of the model hierarchy.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
