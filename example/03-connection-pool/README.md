# DeltaModel ORM — Example 03: Multi-Threaded Connection Pool Benchmark (`03-connection-pool`)

An interactive Lazarus LCL benchmark utility demonstrating **`TDeltaConnectionPool`** (`DeltaModel.ORM.Pool`) under multi-threaded concurrency, comparing pooled RAII connection leasing (`IDeltaConnection` from `Pool.Acquire`) against unpooled per-operation connections (`TDeltaORM.Connect`).

---

## 🚀 Key Features Demonstrated

- **Thread-Safe Connection Leasing (`TDeltaConnectionPool`)**:
  - Configurable pool parameters via `TDeltaPoolConfig`: `MinConnections`, `MaxConnections`, `AcquireTimeoutMs`, and `HousekeeperIntervalSec`.
  - Automatic reference-counted lease release via `IDeltaConnection` (`Lease := FPool.Acquire`).
- **Real-Time Pool Telemetry (`TDeltaPoolStats`)**:
  - Reports `TotalAcquisitions`, `TotalReleases`, `ConnectionsCreated`, and `PeakActiveConnections`.
- **Multi-Threaded Comparative Benchmark (`TPooledWorker` vs `TUnpooledWorker`)**:
  - Spawns $N$ concurrent `TThread` workers, each performing $M$ transactional `INSERT` and `SELECT COUNT(*)` operations.
  - Computes total elapsed time (ms), throughput (operations/second), latency reduction percentage, and speedup factor.

---

## 🛠️ Build & Run

### Prerequisites
- Free Pascal Compiler (`fpc` 3.2.2+) and Lazarus LCL development libraries.
- SQLite 3 shared library (`libsqlite3.so` / `sqlite3.dll`) or another supported SQLDB driver.

### Option 1: Compile with `fpc` (Clean Temporary Build)

```bash
mkdir -p /tmp/deltamodel_ex03
cd example/03-connection-pool

fpc -MObjFPC -Scghi -O2 \
  -FE/tmp/deltamodel_ex03 \
  -FU/tmp/deltamodel_ex03 \
  -Fu../../src \
  -Fu../../src/orm \
  -Fu../../src/orm/dialects \
  -Fu../../src/orm/drivers \
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
lazbuild --bm=Default example/03-connection-pool/project1.lpi
```

### Running the Application

```bash
/tmp/deltamodel_ex03/project1
```

---

## 📖 Usage Guide

1. Configure the connection URL (e.g., `sqlite:///tmp/pool_bench.sqlite`), worker thread count (`Threads`), operations per thread (`Ops`), and pool bounds (`Min Pool` / `Max Pool`).
2. Click **Benchmark With Pool** to run concurrent workers acquiring leases from `TDeltaConnectionPool`.
3. Click **Benchmark Without Pool** to run concurrent workers opening and closing a physical database connection on every iteration.
4. Click **Compare** to execute both benchmarks back-to-back and view the speedup summary.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
