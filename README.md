<p align="right">
  <b>Language / Idioma:</b>
  <a href="README.md">English</a> |
  <a href="README.pt-BR.md">Português (Brasil)</a> |
  <a href="README.es-ES.md">Español</a>
</p>

# DeltaModel

[![Lazarus](https://img.shields.io/badge/Lazarus-2.2%2B-blue.svg)](https://www.lazarus-ide.org/)
[![FreePascal](https://img.shields.io/badge/FPC-3.2.2%2B-green.svg)](https://www.freepascal.org/)
[![Multi--Database](https://img.shields.io/badge/Databases-6%20DBMS-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**DeltaModel** is a lightweight, modern, and extensible micro-ORM and data-modeling library for **Lazarus / Free Pascal (FPC)** that unites in a single framework:

- 🧱 **Data Modeling**: Strongly-typed field descriptors (`TDF*`) or pure native primitive properties with Fluent Class Schema (`ConfigureSchema`).
- ✅ **Fluent Validation Engine**: Declarative validation rules in schema (`Min`, `Max`, `Range`, `MinLength`, `MaxLength`, `Positive`, `NotEmpty`, `Email`, `CPF`, `CNPJ`, `URL`) and custom validation hooks (`Validate`).
- 🔄 **High-Performance JSON Serialization**: Zero-DOM serializer `TDeltaFastJsonStreamer` and lexical streaming deserializer `TDeltaFastJsonParser`.
- 🌐 **Internationalization (i18n)**: Multi-language validation messages and internal exceptions (`en-US`, `pt-BR`, `es-ES`), configurable programmatically (`SetDeltaLocale`) or via environment variable (`DELTAMODEL_LOCALE`).
- 📋 **OpenAPI / Swagger Schemas**: Automatic generation of OpenAPI 3.1 compliant schemas with canonical `$ref` component resolution for API documentation.
- 🗄️ **Multi-DBMS Micro-ORM with Generics**:
  - Automated DDL migration engine that inspects and creates/updates tables, columns, primary keys, foreign keys, constraints (`UNIQUE`, `CHECK`), and database indexes.
  - Fluent, type-safe SQL query builder `TDeltaQuery<T>` with Generics and collections `TGDeltaModelList<T>`.
  - Automated 1:N Eager Loading via `Include('RelationName')`.
  - HTTP Chunked Streaming Response (`StreamJsonTo`) with constant, low heap footprint.
  - RAII-scoped atomic transaction manager with auto-rollback (`IDeltaTransaction`, `BeginTransaction`).
  - Intelligent CRUD operations and lifecycle hooks.

---

## 🚀 Supported DBMS

DeltaModel's ORM module provides database-agnostic support across the primary enterprise relational database engines:

| DBMS | Dialect | Auto-Increment | Pagination Syntax | Returning Syntax |
| :--- | :--- | :--- | :--- | :--- |
| **PostgreSQL** | `ddPostgreSQL` | `SERIAL / BIGSERIAL` | `LIMIT n OFFSET m` | `RETURNING *` |
| **MySQL / MariaDB** | `ddMySQL` | `AUTO_INCREMENT` | `LIMIT n OFFSET m` | Standard INSERT |
| **SQLite3** | `ddSQLite` | `AUTOINCREMENT` | `LIMIT n OFFSET m` | `RETURNING *` |
| **Firebird** | `ddFirebird` | Generator + trigger (2.5); `IDENTITY` (3+) | `ROWS m TO n` | `RETURNING *` |
| **SQL Server (MSSQL)** | `ddMSSQL` | `IDENTITY(1,1)` | `OFFSET-FETCH` | `OUTPUT INSERTED.*` |
| **Oracle Database** | `ddOracle` | `IDENTITY` | `OFFSET-FETCH` | Standard INSERT |

---

## 📦 Standard Connection URL

Initialize the ORM engine using a standardized connection URL:

```pascal
uses DeltaModel.ORM.Connection;

var
  Con: TDeltaORMEngine;
begin
  // SQLite in-memory
  Con := TDeltaORMEngine.Create('sqlite://:memory:');

  // SQLite local file
  Con := TDeltaORMEngine.Create('sqlite:///var/data/app.db');

  // PostgreSQL
  Con := TDeltaORMEngine.Create('postgres://user:password@localhost:5432/mydb?charset=UTF8');

  // MySQL / MariaDB
  Con := TDeltaORMEngine.Create('mysql://root:password@127.0.0.1:3306/mydb');

  // Firebird
  Con := TDeltaORMEngine.Create('firebird://sysdba:masterkey@localhost:3050//var/db/enterprise.fdb');

  // Microsoft SQL Server
  Con := TDeltaORMEngine.Create('mssql://sa:password@127.0.0.1:1433/mydb');

  // Oracle Database
  Con := TDeltaORMEngine.Create('oracle://system:password@localhost:1521/XE');
```

---

## ⚡ Connection Pool (High Performance & Multithreaded)

For web APIs, REST microservices, and concurrent background workers, DeltaModel provides a thread-safe connection pool in unit [`DeltaModel.ORM.Pool`]:

### Key Advantages:
- **Zero Handshake Latency**: Pre-warmed connections (`MinConnections`) ready for immediate execution.
- **Full Thread-Safety**: Concurrency synchronization using lightweight `TCriticalSection` and `TEvent`.
- **Automatic Resource Cleanup (RAII)**: When using `IDeltaPooledEngine`, connections automatically return to the pool upon exiting scope or on unexpected exceptions.
- **Automatic Rollback**: Uncommitted transactions are safely rolled back upon returning to the pool.
- **Health Check & Auto-Reconnect**: Connections dropped by the database server are validated and reconnected prior to lease (`TestOnBorrow`).

### Example with RAII:

```pascal
uses DeltaModel.ORM.Pool;

var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaPooledEngine;
begin
  Pool := TDeltaConnectionPool.Create('postgres://user:pass@localhost:5432/mydb?pool_min=5&pool_max=20&pool_timeout=5000');
  try
    // Acquire RAII-protected connection lease
    Lease := Pool.Acquire;

    // Direct ORM operations on the leased engine
    Lease.Save(Person);
    Lease.Find(TPerson, 1);
    
    // Connection is automatically returned to the pool at block exit!
  finally
    Pool.Free;
  end;
end;
```

---

## 🛠️ Model Definition

DeltaModel supports two distinct modeling paradigms:

### Approach A: Descriptor-Based Field Objects (`TDF*`)

```pascal
unit Person.Model;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Types;

type
  { TPerson }
  TPerson = class(TDeltaModel)
  private
    Fid: TDFIntNull;
    Fname: TDFStringRequired;
    Femail: TDFStringRequired;
    Fsalary: TDFCurrencyRequired;
    Factive: TDFBooleanRequired;
    Fcreated: TDFDateTimeNull;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property email: TDFStringRequired read Femail write Femail;
    property salary: TDFCurrencyRequired read Fsalary write Fsalary;
    property active: TDFBooleanRequired read Factive write Factive;
    property created: TDFDateTimeNull read Fcreated write Fcreated;
  public
    procedure AfterConstruction; override;
    procedure Configure; override;
    procedure Validate; override;
    procedure BeforeInsert; override;
  end;

implementation

procedure TPerson.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.TableName := 'persons';
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.name.Size := 120;
  Self.email.Size := 150;
  Self.email.IsUnique := True;
  Self.created.IsIndexed := True;
end;

procedure TPerson.Configure;
begin
  inherited Configure;
  AddUniqueConstraint('uq_person_email', ['email']);
  AddIndex('ix_person_name_salary', ['name', 'salary']);
end;

procedure TPerson.Validate;
begin
  inherited Validate;
  Validator
    .RuleFor(email.Value, 'E-mail')
      .ValidEmail
    .RuleFor(salary.Value, 'Salary')
      .GreaterThan(0);
end;

procedure TPerson.BeforeInsert;
begin
  inherited BeforeInsert;
  if created.IsNull then
    created.Value := Now;
end;

end.
```

---

### Approach B: Fluent Class Schema & Generics (Zero Variant Boxing)

For maximal performance, minimal memory footprint (-53% heap), and pure native Pascal primitives (`Int64`, `string`, `Double`, `Boolean`):

```pascal
unit Product.Model;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DeltaModel, DeltaModel.ClassSchema, DeltaModel.Query;

type
  { TProduct }
  TProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(const S: TClassSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
  end;

implementation

class procedure TProduct.ConfigureSchema(const S: TClassSchema);
begin
  S.Table('products');
  S.PrimaryKey('Id').AutoIncrement;
  S.Field('Name').Required.MaxLength(100);
  S.Field('Price').Positive;
  S.Field('Active').DefaultValue('true');
end;

end.
```

#### Querying with `TDeltaQuery<T>`:

```pascal
var
  Query: TDeltaQuery<TProduct>;
  List: TGDeltaModelList<TProduct>;
  P: TProduct;
begin
  Query := TDeltaQuery<TProduct>.Create(Con);
  try
    List := Query
      .Where('Price', opGreaterThan, 50.0)
      .OrderBy('Price', odDesc)
      .Limit(10)
      .FindAll;
    try
      for P in List do
        WriteLn(P.Name, ' -> $ ', P.Price:0:2);
    finally
      List.Free;
    end;
  finally
    Query.Free;
  end;
end;
```

---

## 📊 Real Benchmark Results (SQLite3 Disk | -O3 Optimization)

Benchmark executed on Linux x86_64 comparing Legacy Wrapper Model (`TDF*`) vs Approach B (Native Primitives + Fluent Schema):

| Evaluated Metric | Volume (Records) | Legacy Model (`TDF*`) | Approach B (Native) | Gain / Optimization |
| :--- | :---: | :---: | :---: | :---: |
| **RAM Footprint (Heap)** | 1,000 | 1,257 bytes/obj (1.20 MB) | 585 bytes/obj (0.56 MB) | **-53.4% RAM** |
| **RAM Footprint (Heap)** | 10,000 | 1,257 bytes/obj (11.99 MB) | 585 bytes/obj (5.58 MB) | **-53.5% RAM** |
| **RAM Footprint (Heap)** | 50,000 | 1,256 bytes/obj (59.92 MB) | 584 bytes/obj (27.88 MB) | **-53.5% RAM (32 MB freed)** |
| **Object Allocation Time** | 50,000 | 257 ms | 73 ms | **3.5x faster** |
| **SQL Read + ORM Hydration** | 1,000 | 8 ms (125,000 recs/s) | 4 ms (250,000 recs/s) | **2.0x faster** |
| **SQL Read + ORM Hydration** | 10,000 | 83 ms (120,482 recs/s) | 44 ms (227,273 recs/s) | **1.9x faster** |
| **SQL Read + ORM Hydration** | 50,000 | 418 ms (119,617 recs/s) | 217 ms (230,415 recs/s) | **1.9x faster** |
| **JSON Serialization (FastStreamer)**| 50,000 | 245 ms (204,082 objs/s) | 230 ms (217,391 objs/s) | **23.6 MB/s sustained** |
| **Memory Deallocation (Free)** | 50,000 | 60 ms | 30 ms | **2.0x faster** |

---

## ⚡ High-Performance Architecture

### 1. `TDeltaFastJsonStreamer`: Direct JSON Streaming (Zero-DOM Allocation)
Traditional JSON serialization (`fpjson`) allocates hundreds of intermediate DOM nodes (`TJSONObject`, `TJSONString`, `TJSONFloat`) for every single database row, creating high heap thrashing and garbage collection overhead.

`TDeltaFastJsonStreamer` serializes `TDeltaModel` instances and collections directly into a contiguous `RawByteString` buffer:
- **Zero DOM Node Allocation**: Bypasses tree construction during `Model.ToJson`, `List.ToJson`, and `PaginatedList.ToJson`.
- **Fixed-Point Float Formatting (`ffFixed`)**: Resolves the historical `fpjson` limitation that outputs scientific exponents (`1.2E+05` or `1.0E-04`). Floats are strictly formatted in fixed-point decimal, preventing parser breaks on Web, Mobile, and fiscal/accounting integrations.

### 2. `TColumnBindingMap`: Direct Typed ORM Binding Without Variant Boxing
- **Pre-Allocated Mapping**: Resolves dataset column bindings once per query.
- **Direct Typed Transfer**: Reads typed values directly from `TDataSet` fields (`ADS.Fields[I].AsLargeInt`, `.AsString`, `.AsFloat`, `.AsBoolean`) into target model memory offsets.
- **Zero Variant Conversions**: Eliminates COM/OLE Variant boxing overhead across tens of thousands of records.

### 3. `TRTTICache` & `TModelMetadata`: Static RTTI Cache
Caches class reflection metadata (`TypInfo.GetPropList`) protected by `TCriticalSection`, ensuring $O(1)$ fast lookups across the lifecycle of the process.

---

## 📋 OpenAPI / Swagger 3.1 Schemas

DeltaModel models natively generate OpenAPI 3.1 specification schemas with `$ref` component resolution:

```pascal
var
  ObjSchema: string;
  RefObjSchema: string;
  PaginatedSchema: string;
begin
  // Generates standalone schema: {"type": "object", "properties": { ... }}
  ObjSchema := TPerson.SwaggerSchemaObject(True);

  // Registers model into components.schemas and returns: {"$ref": "#/components/schemas/Person"}
  RefObjSchema := TPerson.SwaggerSchemaRefObject;

  // Generates canonical paginated envelope schema:
  PaginatedSchema := TPerson.SwaggerSchemaRefPaginated;
end;
```

---

## 🧪 Automated Test Suite

DeltaModel includes comprehensive automated test coverage with **344 tests passing 100%**:

```bash
# Compile and execute the main test suite
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Run pool concurrency tests
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool

# Run real-world benchmarks
fpc -O3 -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/benchmark_real.pas
/tmp/benchmark_real
```

---

## 📦 Lazarus Package Compilation

The Lazarus package file is located at `deltamodel_pkg.lpk`:

```bash
lazbuild --build-all deltamodel_pkg.lpk
```

---

## 📄 License

Distributed under the MIT License. See `LICENSE` for details.
