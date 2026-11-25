program benchmark_real;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DateUtils, sqlite3conn,
  DeltaModel, DeltaModel.Fields, DeltaModel.Metadata,
  DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry, DeltaModel.ORM.Dialects,
  DeltaModel.ORM.Driver.SQLDB, DeltaModel.ORM.Persister, DeltaModel.ORM.DML,
  DeltaModel.Generics, DeltaModel.JsonStreamer, DeltaSerialization;

type
  { Legacy Model (TDF* wrappers with Variant boxing) }
  TLegacyProduct = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FName: TDFStringRequired;
    FPrice: TDFCurrencyRequired;
    FCost: TDFCurrencyNull;
    FStock: TDFIntRequired;
    FActive: TDFBoolRequired;
  published
    property Id: TDFIntRequired read FId write FId;
    property Name: TDFStringRequired read FName write FName;
    property Price: TDFCurrencyRequired read FPrice write FPrice;
    property Cost: TDFCurrencyNull read FCost write FCost;
    property Stock: TDFIntRequired read FStock write FStock;
    property Active: TDFBoolRequired read FActive write FActive;
  public
    procedure AfterConstruction; override;
  end;

  { Modern Model - Approach B (Fluent Schema + Native Primitive Properties) }
  TModernProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FCost: Double;
    FStock: Integer;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Cost: Double read FCost write FCost;
    property Stock: Integer read FStock write FStock;
    property Active: Boolean read FActive write FActive;
  end;

procedure TLegacyProduct.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'legacy_products';
  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Name.FieldName := 'name';
  Name.Size := 150;
  Name.DBOptions := [dboInsert, dboUpdate];
  Price.FieldName := 'price';
  Price.DBOptions := [dboInsert, dboUpdate];
  Cost.FieldName := 'cost';
  Cost.DBOptions := [dboInsert, dboUpdate];
  Stock.FieldName := 'stock';
  Stock.DBOptions := [dboInsert, dboUpdate];
  Active.FieldName := 'active';
  Active.DBOptions := [dboInsert, dboUpdate];
end;

class procedure TModernProduct.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('modern_products');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Name').Column('name').Required.Size(150);
  ASchema.Field('Price').Column('price').Required;
  ASchema.Field('Cost').Column('cost').Nullable;
  ASchema.Field('Stock').Column('stock').Required;
  ASchema.Field('Active').Column('active').Required;
end;

function GetAllocatedMemKB: Int64;
begin
  Result := GetHeapStatus.TotalAllocated div 1024;
end;

const
  BENCH_DB = '/tmp/deltamodel_benchmark_real.db';

procedure RunBenchmarkVolume(AVolume: Integer; const Conn: IDeltaConnection);
var
  T0, T1: TDateTime;
  ElapsedMs: Int64;
  MemBefore, MemAfter, MemLegacy, MemModern: Int64;
  I: Integer;
  
  // Listas
  LegList: TGDeltaModelList<TLegacyProduct>;
  ModList: TGDeltaModelList<TModernProduct>;
  LegReadList: TDeltaModelList;
  LegProd: TLegacyProduct;
  ModProd: TModernProduct;
  
  // Queries
  ModQuery: TDeltaQuery<TModernProduct>;
  LegDmlQuery: TQuery;
  
  // JSON
  LegJson, ModJson: RawByteString;
  LegJsonMB, ModJsonMB: Double;
  
  // Bench Metrics
  LegInsertMs, ModInsertMs: Int64;
  LegReadMs, ModReadMs: Int64;
  LegJsonMs, ModJsonMs: Int64;
  LegFreeMs, ModFreeMs: Int64;
begin
  WriteLn('-------------------------------------------------------------------------------------------------------');
  WriteLn(Format('>>> BENCHMARK VOLUME: %d REGISTROS REAIS', [AVolume]));
  WriteLn('-------------------------------------------------------------------------------------------------------');

  // ---------------------------------------------------------------------------
  // 1. HEAP MEMORY AND IN-RAM ALLOCATION
  // ---------------------------------------------------------------------------
  // Legado
  MemBefore := GetAllocatedMemKB;
  T0 := Now;
  LegList := TGDeltaModelList<TLegacyProduct>.Create;
  for I := 1 to AVolume do
  begin
    LegProd := TLegacyProduct.Create;
    LegProd.Id.AsInteger := I;
    LegProd.Name.AsText := 'Produto Teste Categoria Premium #' + IntToStr(I);
    LegProd.Price.AsFloat := 199.99 + (I mod 100);
    if (I mod 5 = 0) then
      LegProd.Cost.Clear
    else
      LegProd.Cost.AsFloat := 120.50;
    LegProd.Stock.AsInteger := 50 + (I mod 20);
    LegProd.Active.AsBoolean := (I mod 2 = 0);
    LegList.Add(LegProd);
  end;
  T1 := Now;
  MemAfter := GetAllocatedMemKB;
  MemLegacy := MemAfter - MemBefore;
  ElapsedMs := MilliSecondsBetween(T1, T0);
  WriteLn(Format('1. RAM Alloc [Legado]:  %6d ms | Heap: %6d KB (%5.2f MB) | %6.1f bytes/obj',
    [ElapsedMs, MemLegacy, MemLegacy / 1024.0, (MemLegacy * 1024.0) / AVolume]));

  // Moderno (Abordagem B)
  MemBefore := GetAllocatedMemKB;
  T0 := Now;
  ModList := TGDeltaModelList<TModernProduct>.Create;
  for I := 1 to AVolume do
  begin
    ModProd := TModernProduct.Create;
    ModProd.Id := I;
    ModProd.Name := 'Produto Teste Categoria Premium #' + IntToStr(I);
    ModProd.Price := 199.99 + (I mod 100);
    if (I mod 5 = 0) then
      ModProd.SetNull('Cost')
    else
      ModProd.Cost := 120.50;
    ModProd.Stock := 50 + (I mod 20);
    ModProd.Active := (I mod 2 = 0);
    ModList.Add(ModProd);
  end;
  T1 := Now;
  MemAfter := GetAllocatedMemKB;
  MemModern := MemAfter - MemBefore;
  ElapsedMs := MilliSecondsBetween(T1, T0);
  WriteLn(Format('1. RAM Alloc [Moderno]: %6d ms | Heap: %6d KB (%5.2f MB) | %6.1f bytes/obj  ==> REDUÇÃO: %.1f%%',
    [ElapsedMs, MemModern, MemModern / 1024.0, (MemModern * 1024.0) / AVolume,
     (1.0 - (Double(MemModern) / Double(MemLegacy))) * 100.0]));

  // ---------------------------------------------------------------------------
  // 2. JSON SERIALIZATION (TDeltaFastJsonStreamer)
  // ---------------------------------------------------------------------------
  T0 := Now;
  LegJson := LegList.ToJson;
  T1 := Now;
  LegJsonMs := MilliSecondsBetween(T1, T0);
  LegJsonMB := Length(LegJson) / (1024.0 * 1024.0);

  T0 := Now;
  ModJson := ModList.ToJson;
  T1 := Now;
  ModJsonMs := MilliSecondsBetween(T1, T0);
  ModJsonMB := Length(ModJson) / (1024.0 * 1024.0);

  if LegJsonMs = 0 then LegJsonMs := 1;
  if ModJsonMs = 0 then ModJsonMs := 1;

  WriteLn(Format('2. JSON Export [Legado]:  %6d ms | %5.2f MB | %8.1f MB/s | %8.0f objs/s',
    [LegJsonMs, LegJsonMB, (LegJsonMB / (LegJsonMs / 1000.0)), (AVolume / (LegJsonMs / 1000.0))]));
  WriteLn(Format('2. JSON Export [Moderno]: %6d ms | %5.2f MB | %8.1f MB/s | %8.0f objs/s  ==> %.1fx MAIS RÁPIDO',
    [ModJsonMs, ModJsonMB, (ModJsonMB / (ModJsonMs / 1000.0)), (AVolume / (ModJsonMs / 1000.0)),
     Double(LegJsonMs) / Double(ModJsonMs)]));

  // ---------------------------------------------------------------------------
  // 3. REAL SQLITE PERSISTENCE (BULK INSERT)
  // ---------------------------------------------------------------------------
  // 3. REAL SQLITE PERSISTENCE (BULK INSERT)
  // ---------------------------------------------------------------------------
  // Legado BulkInsert
  Conn.ExecuteDirect('DELETE FROM legacy_products;');
  if Conn.TransactionActive then Conn.Commit;

  T0 := Now;
  TDeltaPersister.BulkInsert(Conn, LegList, 1000);
  T1 := Now;
  LegInsertMs := MilliSecondsBetween(T1, T0);
  if LegInsertMs = 0 then LegInsertMs := 1;
  WriteLn(Format('3. DB BulkInsert [Legado]:  %6d ms | %8.0f recs/s',
    [LegInsertMs, (AVolume / (LegInsertMs / 1000.0))]));

  // Moderno BulkInsert
  ModQuery := TDeltaQuery<TModernProduct>.Create(Conn);
  try
    Conn.ExecuteDirect('DELETE FROM modern_products;');
    if Conn.TransactionActive then Conn.Commit;

    T0 := Now;
    ModQuery.BulkInsert(ModList, 1000);
    T1 := Now;
    ModInsertMs := MilliSecondsBetween(T1, T0);
    if ModInsertMs = 0 then ModInsertMs := 1;
    WriteLn(Format('3. DB BulkInsert [Moderno]: %6d ms | %8.0f recs/s  ==> %.1fx MAIS RÁPIDO',
      [ModInsertMs, (AVolume / (ModInsertMs / 1000.0)),
       Double(LegInsertMs) / Double(ModInsertMs)]));
  finally
    ModQuery.Free;
  end;

  // Free initial lists to measure clean database read
  T0 := Now;
  LegList.Free;
  T1 := Now;
  LegFreeMs := MilliSecondsBetween(T1, T0);

  T0 := Now;
  ModList.Free;
  T1 := Now;
  ModFreeMs := MilliSecondsBetween(T1, T0);

  // ---------------------------------------------------------------------------
  // 4. FULL DATABASE READ AND ORM HYDRATION (SELECT -> OBJECT LIST)
  // ---------------------------------------------------------------------------
  // Legado All
  LegDmlQuery := TQuery.Create(Conn);
  try
    LegDmlQuery.SetModel(TLegacyProduct);
    MemBefore := GetAllocatedMemKB;
    T0 := Now;
    LegReadList := LegDmlQuery.All;
    T1 := Now;
    MemAfter := GetAllocatedMemKB;
    LegReadMs := MilliSecondsBetween(T1, T0);
    if LegReadMs = 0 then LegReadMs := 1;
    WriteLn(Format('4. DB Read/Hydrate [Legado]:  %6d ms | %8.0f recs/s | Heap: %6d KB',
      [LegReadMs, (AVolume / (LegReadMs / 1000.0)), (MemAfter - MemBefore)]));
  finally
    LegDmlQuery.Free;
    LegReadList.Free;
  end;

  // Moderno All Tipado
  ModQuery := TDeltaQuery<TModernProduct>.Create(Conn);
  try
    MemBefore := GetAllocatedMemKB;
    T0 := Now;
    ModList := ModQuery.All;
    T1 := Now;
    MemAfter := GetAllocatedMemKB;
    ModReadMs := MilliSecondsBetween(T1, T0);
    if ModReadMs = 0 then ModReadMs := 1;
    WriteLn(Format('4. DB Read/Hydrate [Moderno]: %6d ms | %8.0f recs/s | Heap: %6d KB  ==> %.1fx MAIS RÁPIDO',
      [ModReadMs, (AVolume / (ModReadMs / 1000.0)), (MemAfter - MemBefore),
       Double(LegReadMs) / Double(ModReadMs)]));
  finally
    ModQuery.Free;
    ModList.Free;
  end;

  WriteLn(Format('5. Desalocação RAM (Free): Legado: %d ms | Moderno: %d ms (%.1fx mais rápido)',
    [LegFreeMs, ModFreeMs, Double(LegFreeMs) / (Double(ModFreeMs) + 0.001)]));
  WriteLn('');
end;

var
  Conn: IDeltaConnection;
begin
  WriteLn('=======================================================================================================');
  WriteLn('        BENCHMARK COMPARATIVO REAL: DELTAMODEL LEGADO (TDF*) vs ABORDAGEM B (NATIVA + GENERICS)        ');
  WriteLn('=======================================================================================================');
  WriteLn('Ambiente: Linux x86_64 | FPC 3.2.3 | SQLite 3 | Otimização -O3');
  WriteLn('');

  if FileExists(BENCH_DB) then DeleteFile(BENCH_DB);

  Conn := TDeltaORM.Connect('sqlite://' + BENCH_DB);
  try
    Conn.ExecuteDirect(
      'CREATE TABLE legacy_products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(150) NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  cost REAL,' +
      '  stock INTEGER NOT NULL,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    Conn.ExecuteDirect(
      'CREATE TABLE modern_products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(150) NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  cost REAL,' +
      '  stock INTEGER NOT NULL,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    if Conn.TransactionActive then Conn.Commit;

    // Run for 1,000, 10,000, and 50,000 real records
    RunBenchmarkVolume(1000, Conn);
    RunBenchmarkVolume(10000, Conn);
    RunBenchmarkVolume(50000, Conn);

  finally
    Conn := nil;
    if FileExists(BENCH_DB) then DeleteFile(BENCH_DB);
  end;

  WriteLn('=======================================================================================================');
  WriteLn('Benchmark finalizado com sucesso.');
  WriteLn('=======================================================================================================');
end.
