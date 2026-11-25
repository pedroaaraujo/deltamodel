<p align="right">
  [ English ](README.md) | [ Português (Brasil) ](README.pt-BR.md) | [ Español ](README.es-ES.md)
</p>

# DeltaModel

[![FPC](https://img.shields.io/badge/FPC-%3E%3D%203.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-%3E%3D%203.0-orange.svg)](https://www.lazarus-ide.org/)
[![DBMS](https://img.shields.io/badge/Databases-6%20DBMS-blue.svg)](https://github.com/pedroaaraujo/deltamodel)
[![Codecs](https://img.shields.io/badge/Codecs-5%20Formats-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

DeltaModel is a typed micro-ORM, data validation engine, and multi-format serialization framework for Free Pascal and Lazarus. It provides generic query building (`TDeltaQuery<T>`), automated DDL migrations, connection pooling with RAII leases, 1:N eager loading, and zero-DOM codecs supporting 5 canonical formats: JSON, CBOR, BSON, XML, and CSV.

---

## Technical Specifications

- **Compilers**: Free Pascal Compiler (FPC) >= 3.2.2.
- **IDE / Tooling**: Lazarus >= 3.0 (`lazbuild`).
- **Operating Systems**: Linux (x86_64, aarch64), Windows (x86_64, i386), macOS (x86_64, arm64).
- **Supported Databases**:
  - PostgreSQL (via `pqconnection`)
  - MySQL / MariaDB (via `mysql57conn` / `mysql80conn`)
  - SQLite3 (via `sqlite3conn`)
  - Firebird 2.5 / 3.0+ (via `ibconnection`)
  - Microsoft SQL Server (via `mssqlconn` / ODBC)
  - Oracle Database (via `oracleconnection`)
- **Supported Serialization Formats**: JSON, CBOR (RFC 8949), BSON (v1.1), XML, CSV (RFC 4180).
- **Core Units**: `DeltaModel`, `DeltaModel.Generics`, `DeltaModel.ORM.Connection`, `DeltaModel.ORM.Pool`, `DeltaModel.Codec`, `DeltaModel.Codec.Cbor`, `DeltaModel.Codec.Bson`, `DeltaModel.Codec.Xml`, `DeltaModel.Codec.Csv`, `DeltaModel.JsonStreamer`, `DeltaValidator`.

---

## Installation

### Method 1: Lazarus Package Manager
1. In Lazarus, open `Package` -> `Open Package File (.lpk)`.
2. Select `deltamodel_pkg.lpk`.
3. Click `Compile` and `Use` -> `Install`.

### Method 2: Manual Unit Path
Add the `src/` directory to your project unit search path:
```bash
fpc -Fusrc my_application.pas
```

---

## Quickstart (60 Seconds)

A complete executable example demonstrating model definition, table creation, record insertion, querying, and multi-format serialization using SQLite:

```pascal
program QuickstartDeltaModel;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  SysUtils, Classes,
  DeltaModel, DeltaModel.Generics, DeltaModel.ORM.Connection,
  DeltaModel.Codec;

type
  { Model declaration with native primitive properties }
  TProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
  end;

class procedure TProduct.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('products');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Name').Column('name').Required.Size(120);
  ASchema.Field('Price').Column('price').Required;
  ASchema.Field('Active').Column('active').Required;
end;

var
  Engine: TDeltaORMEngine;
  Query: TDeltaQuery<TProduct>;
  P: TProduct;
  List: TGDeltaModelList<TProduct>;
  CsvOutput: string;
begin
  // Initialize SQLite in-memory database
  Engine := TDeltaORMEngine.Create('sqlite://:memory:');
  try
    Engine.ExecuteDirect(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(120) NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  active INTEGER NOT NULL' +
      ');'
    );

    // Initialize typed query builder
    Query := TDeltaQuery<TProduct>.Create(Engine);
    try
      // Insert record
      P := TProduct.Create;
      P.Name := 'Mechanical Keyboard';
      P.Price := 149.90;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      // Query single record by primary key
      P := Query.FindById(1);
      if P <> nil then
      try
        WriteLn('Found Product: ', P.Name, ' - $', P.Price:0:2);
      finally
        P.Free;
      end;

      // Query list with filter and ordering
      List := Query
        .Where('price > :min_price')
        .Param('min_price', 50.0)
        .OrderBy('price DESC')
        .All;
      try
        // Export to JSON
        WriteLn('JSON output: ', List.ToJson);

        // Export to CSV via Codec Registry
        CsvOutput := TDeltaCodecRegistry.EncodeListToString(List, 'csv');
        WriteLn('CSV output:' + sLineBreak + CsvOutput);
      finally
        List.Free;
      end;
    finally
      Query.Free;
    end;
  finally
    Engine.Free;
  end;
end.
```

---

## Model Definition & Fluent Schema

DeltaModel maps native Pascal primitives (`Int64`, `string`, `Double`, `Boolean`, `TDateTime`) directly to database columns. This eliminates COM/OLE `Variant` boxing and reduces memory usage by up to 53% compared to descriptor-wrapped records.

```pascal
type
  TAccount = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FBalance: Double;
    FLimit: Double;
    FCreatedAt: TDateTime;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(Schema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Balance: Double read FBalance write FBalance;
    property Limit: Double read FLimit write FLimit;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Active: Boolean read FActive write FActive;
  end;

class procedure TAccount.ConfigureSchema(Schema: TDeltaModelSchema);
begin
  Schema.Table('accounts');
  Schema.Field('Id').Column('id').AsPK.AutoInc;
  Schema.Field('Name').Column('name').Required.Size(150);
  Schema.Field('Balance').Column('balance').Required;
  Schema.Field('Limit').Column('limit_amount').Nullable;
  Schema.Field('CreatedAt').Column('created_at').Required;
  Schema.Field('Active').Column('active').Required;
end;
```

### Handling Nullable Fields
Nullability is tracked using an internal bitmask:

```pascal
// Set a field to SQL NULL
Account.SetNull('Limit');

// Check if a field is SQL NULL
if Account.IsNull('Limit') then
  WriteLn('No limit assigned');
```

---

## Database Connections & Connection Pool

### Standard Connection URLs
Engines are initialized via uniform connection strings:

```pascal
// SQLite (file or in-memory)
Engine := TDeltaORMEngine.Create('sqlite://:memory:');
Engine := TDeltaORMEngine.Create('sqlite:///var/lib/app.db');

// PostgreSQL
Engine := TDeltaORMEngine.Create('postgres://user:pass@localhost:5432/mydb?charset=UTF8');

// MySQL / MariaDB
Engine := TDeltaORMEngine.Create('mysql://root:pass@127.0.0.1:3306/mydb');

// Firebird
Engine := TDeltaORMEngine.Create('firebird://sysdba:masterkey@localhost:3050//var/db/app.fdb');

// Microsoft SQL Server
Engine := TDeltaORMEngine.Create('mssql://sa:pass@127.0.0.1:1433/mydb');

// Oracle Database
Engine := TDeltaORMEngine.Create('oracle://system:pass@localhost:1521/XE');
```

### Thread-Safe Connection Pool
For multi-threaded services and web APIs, `TDeltaConnectionPool` manages pre-warmed connection lifecycles:

```pascal
uses DeltaModel.ORM.Pool;

var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaPooledEngine;
  Query: TDeltaQuery<TProduct>;
begin
  Pool := TDeltaConnectionPool.Create(
    'postgres://user:pass@localhost:5432/mydb?pool_min=5&pool_max=20&pool_timeout=5000'
  );
  try
    // Acquire a thread-safe connection lease with RAII auto-release
    Lease := Pool.Acquire;

    Query := TDeltaQuery<TProduct>.Create(Lease);
    try
      WriteLn('Total products: ', Query.Count);
    finally
      Query.Free;
    end;
    // Lease returns to pool automatically when variable leaves scope
  finally
    Pool.Free;
  end;
end;
```

---

## Query Builder (`TDeltaQuery<T>`)

### Fetching Records
```pascal
// Fetch by Primary Key
Product := Query.FindById(42);

// Fetch first matching record
Product := Query.Where('active = 1').OrderBy('id ASC').First;

// Fetch all matching records
List := Query.Where('price > :price').Param('price', 100.0).All;
```

### Filters, Parameters and Sorting
```pascal
List := Query
  .Where('status = :status')
  .Param('status', 'SHIPPED')
  .AndWhere('total >= :min_total')
  .Param('min_total', 500.0)
  .OrWhere('is_priority = 1')
  .OrderBy('created_at DESC')
  .All;
```

> **Security:** prefer `:parameters` with `.Param(...)` for user-supplied values. The `Where(Field, Value)` shortcuts inline the value as an escaped literal; on MySQL/MariaDB backslashes are doubled as well (`TSQLCriteriaHelper.QuoteString`), assuming the default mode without `NO_BACKSLASH_ESCAPES`. `OrderBy(string)` and `Where(string)` take raw SQL: never pass client text without validating it against the model fields.

### Pagination and Limits
```pascal
// Direct Limit and Offset
List := Query.OrderBy('id ASC').Limit(25).Offset(50).All;

// Page helper: Page(PageNumber, PageSize)
List := Query.OrderBy('id ASC').Page(3, 25).All;
```

### Aggregates and Existence
```pascal
TotalRecords := Query.Where('active = 1').Count;
HasPending   := Query.Where('status = :s').Param('s', 'PENDING').Exists;
```

### Eager Loading (1:N Relations)
To prevent $N+1$ query overhead, `Include` fetches related collections in a single secondary batch query (`WHERE fk IN (...)`):

```pascal
// Fetches orders and populates each Order.Items in 2 queries total
Orders := OrderQuery.Include('Items').Where('status = :s').Param('s', 'OPEN').All;
```

### Massive Queries: Schema Flyweight & Reusable Cursor (Zero-Alloc Loop)

When processing large datasets (e.g., 5,000+ records), traditional ORM patterns like `Query.All` allocate an individual object instance for every row, putting tens of thousands of objects into the Free Pascal Heap. DeltaModel solves this via **Schema Flyweight** and the canonical Pascal **Reusable Cursor**:

1. **Schema Flyweight (`TModel.Schema`)**:
   Metadata (`FieldName`, `Size`, `IsRequired`, `DBOptions`) is decoupled from model row instances. It is configured once per class (`TModel.ConfigureSchema(ASchema)`) and accessed statically (`TMunicipio.Schema`), existing **exactly once in memory** regardless of how many records are fetched.

2. **Reusable Cursor (`while Query.Next do`)**:
   A single model instance (`Query.Current`) is reused across every row during cursor iteration. 5,000 rows consume **EXACTLY 1 OBJECT** on the heap (>99% memory reduction and 2.4x faster):

```pascal
Query := TDeltaQuery<TMunicipio>.Create(Engine);
try
  Query.OrderBy('id ASC').Open;
  while Query.Next do
  begin
    // Query.Current always points to the same single, updated instance
    ProcessMunicipio(Query.Current);
  end;
  Query.Close;
finally
  Query.Free;
end;
```

3. **Native Pascal `for..in` Enumerator (`for M in Query.Cursor do`)**:
```pascal
for M in Query.Cursor do
begin
  // M reuses the single model instance without allocations
  WriteLn(M.Nome, ' - ', M.Uf);
end;
```

4. **Functional Callback (`Query.Each`)**:
```pascal
Query.Each(@ProcessItem);
```

5. **Direct Zero-Alloc HTTP/Stream Output (`Query.StreamJsonTo`)**:
Streams JSON directly to an output stream (e.g. HTTP response stream in Zenith) without intermediate batch arrays or list allocations:
```pascal
RowsStreamed := Query.StreamJsonTo(Context.Response.OutputStream);
```

> [!NOTE]
> Legacy methods such as `TDeltaModelRecords.Add` are deprecated. Use `TDeltaQuery.Cursor` or `StreamJsonTo` instead.

---

## Persistence Operations

```pascal
// Insert a new record
Query.Insert(Product);

// Update an existing record (identified by Primary Key)
Product.Price := 189.90;
Query.Update(Product);

// Insert or Update (Save checks primary key existence)
Query.Save(Product);

// Delete records matching current query filter
Query.Where('active = 0').Delete;

// Bulk Insert using batches (e.g. 500 records per transaction)
TotalInserted := Query.BulkInsert(ProductList, 500);
```

---

## Multi-Format Serialization (JSON, CBOR, BSON, XML, CSV)

DeltaModel features an extensible, zero-DOM codec architecture managed by `TDeltaCodecRegistry` (unit `DeltaModel.Codec`). Codecs encode and decode models and generic lists directly to/from streams or memory buffers without intermediary DOM node trees.

### 1. Supported Formats & Canonical Media Types

| Format | Codec Class | Canonical Media Types | Standard Specification |
| :--- | :--- | :--- | :--- |
| **JSON** | `TDeltaJsonCodec` | `application/json` | RFC 8259 |
| **CBOR** | `TDeltaCborCodec` | `application/cbor` | RFC 8949 (Binary) |
| **BSON** | `TDeltaBsonCodec` | `application/bson` | BSON Specification v1.1 (Binary) |
| **XML**  | `TDeltaXmlCodec`  | `application/xml`, `text/xml` | W3C XML 1.0 (Zero-DOM Streamer) |
| **CSV**  | `TDeltaCsvCodec`  | `text/csv` | RFC 4180 |

### 2. Technical Tradeoffs & Selection Matrix

- **CBOR (`application/cbor`)**:
  - *Payload size*: ~40-60% smaller than JSON.
  - *Characteristics*: Direct IEEE 754 floats, integer packing (0..23 inline), tag semantics, zero string conversion overhead.
  - *Ideal for*: IoT devices, Flutter/Mobile clients, inter-microservice IPC where bandwidth and deserialization latency are critical.
- **BSON (`application/bson`)**:
  - *Payload size*: Comparable to JSON; prefixed with 32-bit document length.
  - *Characteristics*: Rich binary types (UTC datetime ms, 64-bit integers, binary BLOBs, typed elements).
  - *Ideal for*: Document databases, analytical pipelines, document indexing systems.
- **CSV (`text/csv`)**:
  - *Payload size*: Highly compact for flat tabular structures.
  - *Characteristics*: RFC 4180 compliant, automatic header row emission, double-quote escaping for delimiters, ISO-8601 date formatting.
  - *Ideal for*: Bulk data exports, financial reports, spreadsheets, analytics imports.
- **XML (`application/xml`)**:
  - *Payload size*: ~20-40% larger than JSON due to closing tags.
  - *Characteristics*: Standard UTF-8 declaration, strict entity escaping (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`), self-closing tags for NULL fields.
  - *Ideal for*: Enterprise integrations, banking/fiscal compliance, legacy SOAP/XML systems.
- **JSON (`application/json`)**:
  - *Payload size*: Baseline standard.
  - *Characteristics*: Universal browser/SPA support, zero-DOM streaming, fixed-point float formatting (`ffFixed`) without scientific notation breaks.
  - *Ideal for*: Public web REST APIs and JavaScript clients.

### 3. Security Hardening (RFC 8949 Section 10 & BSON Anti-DoS)

The binary decoders implement strict defense-in-depth protections against memory exhaustion and stack overflow:

- **Anti-OOM (Out-of-Memory Protection)**: Before calling `SetLength` or allocating memory buffers, decoders validate the claimed length against remaining bytes in `AInputStream`. If a claimed length exceeds the stream or exceeds `MAX_CBOR_STRING_LENGTH` / `MAX_BSON_STRING_LENGTH` (16 MB), deserialization halts immediately with `EDeltaDeserializationException`.
- **Recursion Depth Ceiling**: A hard limit of `MAX_NESTING_DEPTH = 64` prevents call-stack exhaustion attacks from deeply nested malicious structures.
- **Null-Terminated CString Boundary**: `MAX_BSON_CSTRING_LENGTH = 1024` prevents infinite byte scanning on malformed BSON documents missing `0x00` terminators.

### 4. Codec Registry Usage

```pascal
uses
  DeltaModel, DeltaModel.Codec, DeltaModel.Generics;

var
  P, DecodedP: TProduct;
  RawBytes: string;
  Codec: IDeltaCodec;
  Stream: TMemoryStream;
begin
  P := TProduct.Create;
  try
    P.Id := 101;
    P.Name := 'NVMe SSD 2TB';
    P.Price := 699.00;
    P.Active := True;

    // Direct string encoding via registry
    RawBytes := TDeltaCodecRegistry.EncodeModelToString(P, 'cbor');

    // Decode from binary payload
    DecodedP := TProduct.Create;
    try
      TDeltaCodecRegistry.DecodeModelFromString(RawBytes, DecodedP, 'application/cbor');
      WriteLn('Decoded via CBOR: ', DecodedP.Name, ' - $', DecodedP.Price:0:2);
    finally
      DecodedP.Free;
    end;

    // Stream-based processing
    Stream := TMemoryStream.Create;
    try
      Codec := TDeltaCodecRegistry.GetCodec('bson');
      Codec.EncodeModel(P, Stream);
      Stream.Position := 0;

      DecodedP := TProduct.Create;
      try
        Codec.DecodeModel(Stream, DecodedP);
        WriteLn('Decoded via BSON: ', DecodedP.Name);
      finally
        DecodedP.Free;
      end;
    finally
      Stream.Free;
    end;
  finally
    P.Free;
  end;
end;
```

### 5. HTTP Chunked JSON Streaming (`StreamJsonTo`)

For massive database tables, `TDeltaQuery<T>.StreamJsonTo` streams records directly into an output `TStream` in batches:

```pascal
var
  TotalStreamed: Int64;
begin
  // Streams 1,000 records per batch into HTTP response stream with constant heap footprint
  TotalStreamed := Query.Where('active = 1').StreamJsonTo(AResponse.ContentStream, 1000);
  AResponse.ContentType := 'application/json';
  AResponse.SendContent;
end;
```

---

## Transactions

### Manual Transaction Control
```pascal
Engine.BeginTransaction;
try
  Query.Insert(Order);
  Query.Insert(OrderItem);
  Engine.Commit;
except
  Engine.Rollback;
  raise;
end;
```

### RAII Scoped Transactions
When using `IDeltaTransaction`, transactions automatically roll back if an unhandled exception exits scope:

```pascal
var
  Tx: IDeltaTransaction;
begin
  Tx := Engine.BeginTransaction;
  Query.Insert(Order);
  Query.Insert(OrderItem);
  Tx.Commit; // If an exception occurs before Commit, auto-rollback is triggered
end;
```

---

## Validation Engine

Models can define business validation rules overriding `Validate`:

```pascal
procedure TUser.Validate;
begin
  inherited Validate;
  Validator
    .RuleFor(name.Value, 'Name')
      .NotEmpty
      .MinLength(3)
      .MaxLength(100)
    .RuleFor(email.Value, 'Email')
      .NotEmpty
      .ValidEmail;
end;
```

When a rule fails, DeltaModel raises an `EDeltaValidation` exception containing field names and localized validation messages.

---

## Test Suite

DeltaModel includes an automated test suite covering all ORM operations, connection pool concurrency, and all 5 codecs (JSON, CBOR, BSON, XML, CSV):

```bash
# Compile and run test suite (540 automated tests)
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Run pool concurrency test
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool
```

---

## License

MIT License. See [LICENSE](LICENSE) for details.
