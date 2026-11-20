# DeltaModel Codecs Benchmark & Integrity Report

> **Scope:** Rigorous comparative evaluation of **JSON**, **CBOR (RFC 8949)**, **BSON (v1.1)**, **XML**, and **CSV (RFC 4180)** codecs in DeltaModel.
> **Target Environment:** Free Pascal `3.2.3` (`x86_64-linux`, `-O2` release build; `-gh -gl` HeapTrc build for memory leak verification).

---

## 1. Test Entity & Methodology

All benchmarks and round-trip integrity checks execute against a neutral, schema-configured domain model (`TBenchmarkOrder`, 11 fields) using DeltaModel's high-performance primitive-property schema (`ConfigureSchema`):

| Field | Type | Representative Content |
|---|---|---|
| `Id` | `Int64` | Sequential 64-bit primary key (`100001..105000`) |
| `OrderCode` | `string` | Alphanumeric identifier (`ORD-2026-000042`) |
| `CustomerName` | `string` | Multi-byte UTF-8 with diacritics and 4-byte emojis (`José Müller — Café 🚀✨ #42`) |
| `Description` | `string` | Escaping stress string (`Item "Alpha", <Premium> & Express Delivery #42`) |
| `Quantity` | `Integer` | `1..25` |
| `UnitPrice` | `Double` | Floating-point decimal (`149.95..161.20`) |
| `TotalAmount` | `Double` | Computed rounded monetary value |
| `DiscountRate` | `Double` | Fractional decimal (`0.125`) |
| `CreatedAt` | `TDateTime` | ISO-8601 timestamp (`2026-11-20T14:35:XX`) |
| `IsPriority` / `IsPaid` | `Boolean` | Alternating boolean flags |

### Scenarios Measured
1. **Round-Trip Integrity Suite (7 FPCUnit tests):** Exact field-by-field verification of UTF-8/emojis, XML/CSV/JSON special characters, floating-point precision, timestamps (<= 1s tolerance), booleans, and 50-item lists across all 5 codecs.
2. **Scenario A — Single Complex Object:** `10,000` serialize + `10,000` deserialize iterations per codec, verifying integrity on the final round-trip.
3. **Scenario B1 — Massive List (1,000 records):** Full `EncodeList` + `DecodeList` of `1,000` records per codec, verifying record count and boundary items.
4. **Scenario B2 — Massive List (5,000 records):** Full `EncodeList` + `DecodeList` of `5,000` records per codec, verifying record count and boundary items.
5. **Memory Safety (`-gh` HeapTrc):** Full suite execution under FPC's HeapTrc allocator tracking every allocated and freed block.

---

## 2. Benchmark Results (`-O2` Release Build)

### 2.1 Scenario A — Single Complex Object (10,000 Encode + 10,000 Decode)

| Codec | Payload (B) | Size vs JSON | Encode Time (ms) | Encode Throughput (ops/s) | Decode Throughput (ops/s) | Encode (MB/s) | Decode (MB/s) | Heap Delta (B) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| **JSON** | 295 | `0.0%` | 94 | 106,383 | 54,945 | 29.93 | 15.46 | 4,448 |
| **CBOR** | **258** | **-12.5%** | 76 | 131,579 | **129,870** | 32.37 | **31.95** | 4,608 |
| **BSON** | 289 | -2.0% | **65** | **153,846** | 105,263 | **42.40** | 29.01 | 4,608 |
| **XML** | 492 | +66.8% | 478 | 20,921 | 57,471 | 9.82 | 26.97 | 4,448 |
| **CSV** | 268 | -9.2% | 121 | 82,645 | 44,248 | 21.12 | 11.31 | 4,576 |

**Key Takeaways (Single Object):**
- **CBOR** achieves the smallest single-object payload (**-12.5%** vs JSON) and the **fastest decode throughput (129,870 ops/s — 2.36x faster than JSON)**.
- **BSON** achieves the **fastest encode throughput (153,846 ops/s — 1.45x faster than JSON, 42.40 MB/s)** due to fixed-width little-endian binary encoding.
- **XML** incurs a **+66.8%** payload overhead due to opening/closing tags and entity escaping (`&amp;`, `&lt;`, `&gt;`, `&quot;`).

---

### 2.2 Scenario B1 — Massive List (1,000 Records)

| Codec | Payload (B) | Size vs JSON | Encode Time (ms) | Encode (rec/s) | Decode (rec/s) | Encode (MB/s) | Decode (MB/s) | Heap Delta (B) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| **JSON** | 298,104 | `0.0%` | 10 | 100,000 | 47,619 | 28.43 | 13.54 | 1,076,320 |
| **CBOR** | 257,869 | **-13.5%** | **7** | **142,857** | **100,000** | 35.13 | **24.59** | 1,244,512 |
| **BSON** | 293,741 | -1.5% | **7** | **142,857** | 83,333 | **40.02** | 23.34 | 1,244,512 |
| **XML** | 432,201 | +45.0% | 46 | 21,739 | 37,037 | 8.96 | 15.27 | 1,260,640 |
| **CSV** | **160,213** | **-46.3%** | 32 | 31,250 | 55,556 | 4.77 | 8.49 | 1,101,920 |

---

### 2.3 Scenario B2 — Massive List (5,000 Records)

| Codec | Payload (B) | Size vs JSON | Encode Time (ms) | Encode (rec/s) | Decode (rec/s) | Encode (MB/s) | Decode (MB/s) | Heap Delta (B) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| **JSON** | 1,499,369 | `0.0%` | 49 | 102,041 | 45,872 | 29.18 | 13.12 | 5,388,992 |
| **CBOR** | 1,298,189 | **-13.4%** | 39 | 128,205 | **102,041** | 31.74 | **25.27** | 6,172,608 |
| **BSON** | 1,481,741 | -1.2% | **38** | **131,579** | 81,967 | **37.19** | 23.17 | 6,172,608 |
| **XML** | 2,169,466 | +44.7% | 286 | 17,483 | 37,313 | 7.23 | 15.44 | 6,212,288 |
| **CSV** | **809,478** | **-46.0%** | 746 | 6,702 | 53,763 | 1.03 | 9.12 | 5,476,032 |

---

## 3. Resource Consumption & Zero-Leak Proof

### 3.1 Process Resource Profile (`-O2` Full Suite)
- **Total Tests Executed:** `10 / 10` passed (`0 errors`, `0 failures`)
- **Total Wall-Clock Time:** `3.520 s` (including 100,000 single-object ops + 60,000 list record ops)
- **CPU Time:** `2.37 s` user / `1.12 s` system
- **Peak Resident Memory (Max RSS):** `25,448 KB` (`~24.85 MB`)

### 3.2 HeapTrc Memory Safety Audit (`-gh -gl`)
```text
Heap dump by heaptrc unit of /tmp/dm_codecs_leak/benchmark_codecs_standalone
8313443 memory blocks allocated : 1116666351/1164903288
8313443 memory blocks freed     : 1116666351/1164903288
0 unfreed memory blocks : 0
```
Over **8.31 million heap allocations** (~1.12 GB cumulative churn) were exercised across all 5 codecs with **0 unfreed memory blocks (0 bytes leaked)**.

---

## 4. Engineering Analysis & Codec Selection Guide

1. **CBOR (`application/cbor`) — Best Overall Binary Codec:**
   - Consistently **12.5%–13.5% smaller** than JSON while decoding **2.1x–2.4x faster** (`>100k rec/s` on 5,000-item lists).
   - Ideal for high-throughput REST/RPC APIs, mobile clients, and IoT/edge telemetry.
2. **BSON (`application/bson`) — Fastest Binary Encoder:**
   - Achieves peak encode throughput (**153k ops/s** single, **131k rec/s** on 5k lists, up to **42.4 MB/s**) with near-JSON payload size (`-1.2%` to `-2.0%`).
   - Ideal for document-oriented persistence pipelines and wire-speed write-heavy workloads.
3. **JSON (`application/json`) — Universal Baseline:**
   - Strong zero-DOM streaming encode (`>100k rec/s`) with universal browser/tooling interoperability.
4. **CSV (`text/csv`) — Maximum Tabular Compression:**
   - Eliminates repeated field keys across rows, achieving **-46.0% payload reduction** on large flat lists.
   - *Optimization note observed:* `TDeltaCsvCodec.EncodeList` scales non-linearly between 1,000 rows (`32 ms`) and 5,000 rows (`746 ms`) due to repeated string concatenation; streaming directly into a pre-sized buffer/stream in a future optimization pass would raise CSV 5k encode throughput by >10x.
5. **XML (`application/xml`) — Enterprise Interoperability:**
   - Preserves full UTF-8 and entity escaping fidelity with `+45%` to `+67%` payload overhead; recommended when strict XML contracts or legacy enterprise integrations are required.

---

## 5. Reproducing the Benchmarks

Compile and execute in an isolated temporary directory without modifying tracked binaries:

```bash
mkdir -p /tmp/dm_codecs_bench
fpc -Mobjfpc -Sh -O2 -FE/tmp/dm_codecs_bench -FU/tmp/dm_codecs_bench \
  -Fu./src \
  -Fu/path/to/tests/src \
  /path/to/tests/src/benchmark_codecs_standalone.lpr
/tmp/dm_codecs_bench/benchmark_codecs_standalone -a --format=plain
```

For zero-leak verification, recompile with `-gh -gl` instead of `-O2`.
