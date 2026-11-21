# DeltaModel ORM — Example 05: Fluent Class Schema & Generics (`05-fluent-generics`)

A console application demonstrating **DeltaModel ORM's Fluent Class Schema & Generics API** (`DeltaModel.Generics`), allowing domain models to use native Object Pascal primitive properties (`Int64`, `string`, `Double`, `Integer`, `Boolean`) with zero per-field wrapper allocations.

---

## 🚀 Key Features Demonstrated

- **Native Primitive Properties**:
  - `TProduct` declares standard `published` Pascal properties (`Id: Int64`, `Name: string`, `Price: Double`, `Cost: Double`, `Stock: Integer`, `Active: Boolean`) without requiring `TDF*` field wrapper objects.
- **Static Flyweight Class Schema (`ConfigureSchema`)**:
  - Metadata is registered once per class via `class procedure ConfigureSchema(ASchema: TDeltaModelSchema)` and cached in RTTI:
    ```pascal
    class procedure TProduct.ConfigureSchema(ASchema: TDeltaModelSchema);
    begin
      ASchema.Table('products');
      ASchema.Field('Id').Column('id').AsPK.AutoInc;
      ASchema.Field('Name').Column('name').Required.Size(150);
      ASchema.Field('Price').Column('price').Required;
      ASchema.Field('Cost').Column('cost').Nullable;
      ASchema.Field('Stock').Column('stock').Required;
      ASchema.Field('Active').Column('active').Required;
    end;
    ```
- **O(1) Nullability Bitmask (`FNullMask: UInt64`)**:
  - Nullable primitive properties can be marked or tested for SQL `NULL` in a single CPU cycle using `P.SetNull('Cost')` and `P.IsNull('Cost')`, avoiding `Variant` boxing overhead.
- **Strongly-Typed Generic Query (`TDeltaQuery<TProduct>`)**:
  - Fluent query execution: `Insert`, `FindById`, `Count`, `BulkInsert`, and `Where('price > :min_price').Param('min_price', 100.0).OrderBy('price DESC').All`.
- **Typed Generic Collection (`TGDeltaModelList<TProduct>`)**:
  - Supports idiomatic `for P in List do` iteration without typecasts and ultra-fast JSON serialization via `List.ToJson` (`TDeltaFastJsonStreamer`).

---

## 🛠️ Build & Run

### Compilation with Free Pascal Compiler (`fpc`)

Compile into `/tmp` so the repository stays clean of binaries and `.o`/`.ppu` artifacts:

```bash
mkdir -p /tmp/deltamodel_ex05
cd example/05-fluent-generics

fpc -MDelphi -Scghi -O2 \
  -FE/tmp/deltamodel_ex05 \
  -FU/tmp/deltamodel_ex05 \
  -Fu../../src \
  -Fi../../src \
  fluent_generics_demo.lpr
```

### Execution

```bash
/tmp/deltamodel_ex05/fluent_generics_demo
```

The demo creates a temporary SQLite database (`/tmp/deltamodel_fluent_demo.db`), runs all CRUD, bulk insert, filtered query, and JSON serialization steps, prints the output to stdout, and cleans up the temporary database file on exit.

---

## 🛡️ License

Part of the **DeltaModel ORM** ecosystem under the MIT License.
