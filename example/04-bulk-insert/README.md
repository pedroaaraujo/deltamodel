# DeltaModel ORM — Example 04: High-Performance Bulk Insert (`04-bulk-insert`)

An interactive Lazarus LCL benchmark and SQL preview application demonstrating **`TDeltaORMEngine.BulkInsert`** and **`TDMSQLBuilder.CreateBulkInsert`** across multiple database dialects.

---

## 🚀 Key Features Demonstrated

- **Multi-Dialect Bulk Insert SQL Generation (`TDMSQLBuilder.CreateBulkInsert`)**:
  - Generates multi-row batch `INSERT` statements tailored to **PostgreSQL**, **MySQL / MariaDB**, **SQLite**, **Firebird** (`INSERT INTO ... SELECT ... FROM RDB$DATABASE UNION ALL`), **MSSQL**, and **Oracle** (`INSERT ALL INTO ... SELECT 1 FROM DUAL`).
- **Batch Execution Modes (`TDeltaORMEngine.BulkInsert`)**:
  - **Open Array Mode**: `Con.BulkInsert(Models, BatchSize)` accepting an array of `TDeltaModel` instances.
  - **Model List Mode**: `Con.BulkInsert(List, BatchSize)` accepting a `TDeltaModelList`.
  - Automatically chunks large datasets into configurable `BatchSize` packets inside an atomic transaction.
- **Performance Comparison Benchmark**:
  - Compares batch `BulkInsert` throughput (records/second) against traditional row-by-row `Con.Save(P)` execution.

---

## 🛠️ Build & Run

### Prerequisites
- Free Pascal Compiler (`fpc` 3.2.2+) and Lazarus LCL development libraries.
- SQLite 3 shared library (`libsqlite3.so` / `sqlite3.dll`) or another supported SQLDB driver.

### Option 1: Compile with `fpc` (Clean Temporary Build)

```bash
mkdir -p /tmp/deltamodel_ex04
cd example/04-bulk-insert

fpc -MObjFPC -Scghi -O2 \
  -FE/tmp/deltamodel_ex04 \
  -FU/tmp/deltamodel_ex04 \
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
lazbuild --bm=Default example/04-bulk-insert/project1.lpi
```

### Running the Application

```bash
/tmp/deltamodel_ex04/project1
```

---

## 📖 Usage Guide

1. **Preview Bulk SQL**: Select a target dialect (`PostgreSQL`, `MySQL`, `SQLite`, `Firebird`, `MSSQL`, `Oracle`) and click **Preview SQL** to inspect the multi-row `INSERT` syntax generated for 3 sample `TProduct` records.
2. **Bulk Insert (Open Array)**: Set the total record count (e.g., `1000`) and batch size (e.g., `500`), then click **Bulk Insert (Array)** to measure insertion speed.
3. **Bulk Insert (`TDeltaModelList`)**: Click **Bulk Insert (List)** to benchmark batch insertion from a `TDeltaModelList`.
4. **One-by-One Comparison**: Click **Save One-by-One** to run individual `INSERT` statements and compare total elapsed time and throughput.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
