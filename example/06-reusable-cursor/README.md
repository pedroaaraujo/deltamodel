# DeltaModel ORM — Example 06: Schema Flyweight & Reusable Cursor (`06-reusable-cursor`)

A console benchmark and demonstration of **DeltaModel ORM's Flyweight Schema and Zero-Allocation Reusable Cursor**, engineered for iterating and streaming thousands or millions of database rows with constant $O(1)$ heap memory usage.

---

## 🚀 Problem Solved

In traditional ORM query patterns (such as loading a full collection with `Query.All`), fetching 5,000 rows allocates tens of thousands of heap objects (one model instance per row plus field wrappers and list nodes).

DeltaModel eliminates per-row heap allocations through two architectural mechanisms:
1. **Schema Flyweight (`TMunicipio.Schema`)**:
   - Column metadata (`FieldName`, `FieldSize`, `IsRequired`, `DBOptions`) is decoupled from row instances and lives at the class level, existing **exactly once in memory** for the entire application lifecycle.
2. **Reusable Cursor (Zero-Allocation Loop)**:
   - A **single model instance** is allocated when the cursor opens and is repopulated in-place as the cursor advances through the dataset.
   - Iterating over 1,000 or 1,000,000 rows allocates **exactly 1 model object on the heap** (`0 bytes` of heap growth during iteration).

---

## 🛠️ Supported Iteration & Streaming Patterns

### 1. Canonical Pascal Cursor Loop (`while Query.Next do`)
```pascal
Query := TDeltaQuery<TMunicipio>.Create(Engine);
try
  Query.OrderBy('id ASC').Open;
  while Query.Next do
  begin
    // Query.Current always points to the same reused model instance
    ProcessItem(Query.Current);
  end;
  Query.Close;
finally
  Query.Free;
end;
```

### 2. Native `for..in` Enumerator (`for M in Query.Cursor do`)
```pascal
for M in Query.Cursor do
begin
  // M reuses the same underlying instance on every iteration with zero allocations
  WriteLn(M.Nome, ' - ', M.Uf);
end;
```

### 3. Functional Callback (`Query.Each`)
```pascal
Query.Each(Processor.ProcessItem);
```

### 4. Direct Zero-Allocation JSON Streaming (`Query.StreamJsonTo`)
Streams the JSON array directly to any `TStream` (such as an HTTP response stream) without building intermediate object lists in memory:
```pascal
RowsSent := Query.StreamJsonTo(OutputStream);
```

---

## 🛠️ Build & Run

### Compilation with Free Pascal Compiler (`fpc`)

Compile into a temporary directory to keep the repository clean:

```bash
mkdir -p /tmp/deltamodel_ex06
cd example/06-reusable-cursor

fpc -MDelphi -Scghi -O2 \
  -FE/tmp/deltamodel_ex06 \
  -FU/tmp/deltamodel_ex06 \
  -Fu../../src \
  -Fi../../src \
  reusable_cursor_demo.lpr
```

### Execution

```bash
/tmp/deltamodel_ex06/reusable_cursor_demo
```

The program populates a temporary SQLite database (`/tmp/deltamodel_cursor_demo.db`) with 1,000 records, verifies pointer identity and zero heap growth across `while Query.Next`, `for M in Query.Cursor`, `Query.Each`, and `Query.StreamJsonTo`, and removes the temporary database upon completion.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
