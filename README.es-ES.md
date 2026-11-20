<p align="right">
  [ English ](README.md) | [ Português (Brasil) ](README.pt-BR.md) | [ Español ](README.es-ES.md)
</p>

# DeltaModel

[![FPC](https://img.shields.io/badge/FPC-%3E%3D%203.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-%3E%3D%203.0-orange.svg)](https://www.lazarus-ide.org/)
[![DBMS](https://img.shields.io/badge/Bases%20de%20Datos-6%20SGBD-blue.svg)](https://github.com/pedroaaraujo/deltamodel)
[![Codecs](https://img.shields.io/badge/Codecs-5%20Formatos-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

DeltaModel es una biblioteca de micro-ORM tipado, motor de validación y framework de serialización multiformato para Free Pascal y Lazarus. Ofrece generador genérico de consultas (`TDeltaQuery<T>`), migraciones automáticas de DDL, pool de conexiones con gestión RAII, Eager Loading 1:N y códecs Zero-DOM compatibles con 5 formatos canónicos: JSON, CBOR, BSON, XML y CSV.

---

## Especificaciones Técnicas

- **Compilador**: Free Pascal Compiler (FPC) >= 3.2.2.
- **IDE / Herramientas**: Lazarus >= 3.0 (`lazbuild`).
- **Sistemas Operativos**: Linux (x86_64, aarch64), Windows (x86_64, i386), macOS (x86_64, arm64).
- **SGBD Compatibles**:
  - PostgreSQL (vía `pqconnection`)
  - MySQL / MariaDB (vía `mysql57conn` / `mysql80conn`)
  - SQLite3 (vía `sqlite3conn`)
  - Firebird 2.5 / 3.0+ (vía `ibconnection`)
  - Microsoft SQL Server (vía `mssqlconn` / ODBC)
  - Oracle Database (vía `oracleconnection`)
- **Formatos de Serialización Compatibles**: JSON, CBOR (RFC 8949), BSON (v1.1), XML, CSV (RFC 4180).
- **Unidades Principales**: `DeltaModel`, `DeltaModel.Generics`, `DeltaModel.ORM.Connection`, `DeltaModel.ORM.Pool`, `DeltaModel.Codec`, `DeltaModel.Codec.Cbor`, `DeltaModel.Codec.Bson`, `DeltaModel.Codec.Xml`, `DeltaModel.Codec.Csv`, `DeltaModel.JsonStreamer`, `DeltaValidator`.

---

## Instalación

### Método 1: Gestor de Paquetes de Lazarus
1. En Lazarus, vaya a `Paquete` -> `Abrir archivo de paquete (.lpk)`.
2. Seleccione `deltamodel_pkg.lpk`.
3. Haga clic en `Compilar` y luego en `Usar` -> `Instalar`.

### Método 2: Ruta Manual de Unidades
Añada el directorio `src/` a la ruta de búsqueda del compilador:
```bash
fpc -Fusrc mi_aplicacion.pas
```

---

## Guía Rápida (60 Segundos)

Ejemplo completo ejecutable que demuestra definición de modelo, creación de tabla, inserción, consulta tipada y serialización multiformato con SQLite:

```pascal
program QuickstartDeltaModel;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  SysUtils, Classes,
  DeltaModel, DeltaModel.Generics, DeltaModel.ORM.Connection,
  DeltaModel.Codec;

type
  { Declaración del modelo con propiedades primitivas nativas }
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
  // Inicialización de SQLite en memoria
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

    // Generador tipado de consultas
    Query := TDeltaQuery<TProduct>.Create(Engine);
    try
      // Inserción de registro
      P := TProduct.Create;
      P.Name := 'Teclado Mecánico';
      P.Price := 149.90;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      // Consulta de registro único por clave primaria
      P := Query.FindById(1);
      if P <> nil then
      try
        WriteLn('Producto localizado: ', P.Name, ' - $ ', P.Price:0:2);
      finally
        P.Free;
      end;

      // Consulta de lista con filtro y ordenación
      List := Query
        .Where('price > :min_price')
        .Param('min_price', 50.0)
        .OrderBy('price DESC')
        .All;
      try
        // Exportación a JSON
        WriteLn('JSON generado: ', List.ToJson);

        // Exportación a CSV vía Codec Registry
        CsvOutput := TDeltaCodecRegistry.EncodeListToString(List, 'csv');
        WriteLn('CSV generado:' + sLineBreak + CsvOutput);
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

## Definición de Modelos y Esquema Fluido

DeltaModel vincula primitivas nativas de Pascal (`Int64`, `string`, `Double`, `Boolean`, `TDateTime`) directamente a columnas de la base de datos, suprimiendo la sobrecarga de empaquetado OLE/COM de `Variant` y reduciendo el uso de memoria hasta un 53% respecto a modelos con descriptores de campo.

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

### Gestión de Campos Nulos
El estado nulo se controla mediante una máscara de bits interna:

```pascal
// Asignar valor SQL NULL a un campo
Account.SetNull('Limit');

// Comprobar si el campo es SQL NULL
if Account.IsNull('Limit') then
  WriteLn('Sin límite asignado');
```

---

## Conexiones y Pool de Conexiones

### URLs Estandarizadas de Conexión
Los motores se instancian mediante cadenas uniformes de conexión:

```pascal
// SQLite (fichero o en memoria)
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

### Pool de Conexiones Seguro para Subprocesos
Para servicios concurrentes y APIs web, `TDeltaConnectionPool` gestiona conexiones precalentadas:

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
    // Obtiene una conexión con liberación automática mediante RAII
    Lease := Pool.Acquire;

    Query := TDeltaQuery<TProduct>.Create(Lease);
    try
      WriteLn('Total de productos: ', Query.Count);
    finally
      Query.Free;
    end;
    // La conexión regresa al pool de forma automática al salir del ámbito
  finally
    Pool.Free;
  end;
end;
```

---

## Consultas con `TDeltaQuery<T>`

### Búsqueda de Registros
```pascal
// Búsqueda por Clave Primaria
Product := Query.FindById(42);

// Primer registro que coincide con el criterio
Product := Query.Where('active = 1').OrderBy('id ASC').First;

// Todos los registros filtrados
List := Query.Where('price > :price').Param('price', 100.0).All;
```

### Filtros, Parámetros y Ordenación
```pascal
List := Query
  .Where('status = :status')
  .Param('status', 'ENVIADO')
  .AndWhere('total >= :min_total')
  .Param('min_total', 500.0)
  .OrWhere('is_priority = 1')
  .OrderBy('created_at DESC')
  .All;
```

### Paginación y Límites
```pascal
// Limit y Offset directos
List := Query.OrderBy('id ASC').Limit(25).Offset(50).All;

// Asistente de página: Page(NumeroPagina, TamanoPagina)
List := Query.OrderBy('id ASC').Page(3, 25).All;
```

### Agregaciones y Existencia
```pascal
TotalRecords := Query.Where('active = 1').Count;
HasPending   := Query.Where('status = :s').Param('s', 'PENDIENTE').Exists;
```

### Eager Loading (Relaciones 1:N)
Para eliminar el problema de las $N+1$ consultas, `Include` recupera colecciones secundarias en lote en una única consulta adicional (`WHERE fk IN (...)`):

```pascal
// Carga pedidos y rellena Order.Items en 2 consultas en total
Orders := OrderQuery.Include('Items').Where('status = :s').Param('s', 'ABIERTO').All;
```

### Consultas Masivas: Schema Flyweight y Cursor Reutilizable (Zero-Alloc Loop)

Al procesar grandes volúmenes de datos (por ejemplo, más de 5.000 registros), el enfoque tradicional de los ORM (`Query.All`) asigna una instancia de objeto individual para cada fila, cargando decenas de miles de objetos en el Heap de Free Pascal. DeltaModel soluciona este cuello de botella mediante **Schema Flyweight** y el **Cursor Reutilizable** canónico de Pascal:

1. **Schema Flyweight (`TModel.Schema`)**:
   Los metadatos (`FieldName`, `Size`, `IsRequired`, `DBOptions`) se desacoplan de las instancias de fila del modelo. Se configuran una sola vez por clase (`TModel.ConfigureSchema(ASchema)`) y se accede a ellos de forma estática (`TMunicipio.Schema`), existiendo **exactamente 1 vez en memoria** independientemente del número de filas consultadas.

2. **Cursor Reutilizable (`while Query.Next do`)**:
   Una única instancia del modelo (`Query.Current`) se reutiliza a lo largo de todas las filas recorridas por el cursor. Para 5.000 filas, se asigna **EXACTAMENTE 1 OBJETO** en el Heap (>99% de reducción de memoria y 2.4x más rápido):

```pascal
Query := TDeltaQuery<TMunicipio>.Create(Engine);
try
  Query.OrderBy('id ASC').Open;
  while Query.Next do
  begin
    // Query.Current apunta SIEMPRE a la misma instancia actualizada
    ProcesarMunicipio(Query.Current);
  end;
  Query.Close;
finally
  Query.Free;
end;
```

3. **Compatibilidad con Enumerador Nativo de Pascal (`for M in Query.Cursor do`)**:
```pascal
for M in Query.Cursor do
begin
  // M reutiliza la misma instancia del modelo sin asignaciones en el bucle
  WriteLn(M.Nome, ' - ', M.Uf);
end;
```

4. **Callback Funcional (`Query.Each`)**:
```pascal
Query.Each(@ProcesarItem);
```

5. **Streaming Directo a HTTP/Socket (`Query.StreamJsonTo`)**:
Transmite el JSON formateado directamente a un `TStream` (como el flujo de respuesta HTTP del Zenith Framework) sin crear lotes ni listas intermedias en el Heap:
```pascal
FilasEnviadas := Query.StreamJsonTo(Context.Response.OutputStream);
```

> [!NOTE]
> Los métodos heredados como `TDeltaModelRecords.Add` han sido marcados como obsoletos (`deprecated 'Use TDeltaQuery.Cursor or StreamJsonTo instead'`).

---

## Operaciones de Persistencia

```pascal
// Insertar nuevo registro
Query.Insert(Product);

// Actualizar registro existente (identificado por la clave primaria)
Product.Price := 189.90;
Query.Update(Product);

// Insertar o actualizar (Save verifica la existencia de la clave primaria)
Query.Save(Product);

// Eliminar registros coincidentes con el filtro actual
Query.Where('active = 0').Delete;

// Inserción en lote (por ejemplo, 500 registros por transacción)
TotalInsertados := Query.BulkInsert(ProductList, 500);
```

---

## Serialización Multiformato (JSON, CBOR, BSON, XML, CSV)

DeltaModel implementa una arquitectura extensible de códecs Zero-DOM gestionada por la clase `TDeltaCodecRegistry` (unidad `DeltaModel.Codec`). Los códecs codifican y decodifican modelos y colecciones genéricas directamente hacia y desde streams o búferes de memoria sin crear árboles intermedios de objetos DOM.

### 1. Formatos Compatibles y Media Types Canónicos

| Formato | Clase del Códec | Media Types Canónicos | Especificación Estándar |
| :--- | :--- | :--- | :--- |
| **JSON** | `TDeltaJsonCodec` | `application/json` | RFC 8259 |
| **CBOR** | `TDeltaCborCodec` | `application/cbor` | RFC 8949 (Binario) |
| **BSON** | `TDeltaBsonCodec` | `application/bson` | BSON Specification v1.1 (Binario) |
| **XML**  | `TDeltaXmlCodec`  | `application/xml`, `text/xml` | W3C XML 1.0 (Zero-DOM Streamer) |
| **CSV**  | `TDeltaCsvCodec`  | `text/csv` | RFC 4180 |

### 2. Matriz de Tradeoffs Técnicos

- **CBOR (`application/cbor`)**:
  - *Tamaño del payload*: ~40-60% menor que JSON.
  - *Características*: Coma flotante IEEE 754 directa, empaquetado de enteros (0..23 inline), semántica de tags, cero coste de conversión a texto.
  - *Ideal para*: Dispositivos IoT, clientes móviles (Flutter), comunicación inter-microservicios donde el ancho de banda y la latencia son críticos.
- **BSON (`application/bson`)**:
  - *Tamaño del payload*: Similar a JSON; prefijado con el tamaño total del documento en 32 bits.
  - *Características*: Tipado binario enriquecido (UTC datetime en ms, enteros de 64 bits, BLOBs binarios, elementos tipados).
  - *Ideal para*: Bases de datos documentales, pipelines analíticos e interoperabilidad con MongoDB.
- **CSV (`text/csv`)**:
  - *Tamaño del payload*: Muy compacto para tablas planas.
  - *Características*: Conforme con RFC 4180, cabecera automática, escape de comillas dobles, formato canónico de fechas ISO-8601.
  - *Ideal para*: Exportaciones masivas, informes financieros, hojas de cálculo e importación en herramientas analíticas.
- **XML (`application/xml`)**:
  - *Tamaño del payload*: ~20-40% mayor que JSON debido a las etiquetas de cierre.
  - *Características*: Declaración UTF-8 estándar, escape estricto de entidades (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`), etiquetas autocerradas para campos nulos.
  - *Ideal para*: Integraciones corporativas enterprise, cumplimiento bancario/fiscal y sistemas SOAP/XML heredados.
- **JSON (`application/json`)**:
  - *Tamaño del payload*: Línea de base estándar.
  - *Características*: Compatibilidad universal en navegadores y SPAs, streaming Zero-DOM, números en coma fija decimal (`ffFixed`) sin notación científica.
  - *Ideal para*: APIs públicas REST y aplicaciones web JavaScript.

### 3. Protecciones de Seguridad (RFC 8949 Sección 10 y BSON Anti-DoS)

Los decodificadores binarios incorporan defensas activas contra el agotamiento de memoria y desbordamiento de pila:

- **Anti-OOM (Protección contra Agotamiento de Memoria)**: Antes de invocar `SetLength` o reservar búferes, los decodificadores validan la longitud declarada contra los bytes restantes en `AInputStream`. Si la longitud indicada excede el stream o supera `MAX_CBOR_STRING_LENGTH` / `MAX_BSON_STRING_LENGTH` (16 MB), la deserialización se detiene inmediatamente con `EDeltaDeserializationException`.
- **Límite Estricto de Recursión**: El techo de `MAX_NESTING_DEPTH = 64` previene ataques de desbordamiento de pila (stack overflow) causados por estructuras maliciosas profundamente anidadas.
- **Control de CStrings Nulas**: `MAX_BSON_CSTRING_LENGTH = 1024` evita bucles infinitos de escaneo ante documentos BSON corruptos sin terminador `0x00`.

### 4. Uso del Registro de Códecs

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
    P.Name := 'SSD NVMe 2TB';
    P.Price := 699.00;
    P.Active := True;

    // Codificación directa mediante el registro
    RawBytes := TDeltaCodecRegistry.EncodeModelToString(P, 'cbor');

    // Decodificación a partir del payload binario
    DecodedP := TProduct.Create;
    try
      TDeltaCodecRegistry.DecodeModelFromString(RawBytes, DecodedP, 'application/cbor');
      WriteLn('Decodificado vía CBOR: ', DecodedP.Name, ' - $ ', DecodedP.Price:0:2);
    finally
      DecodedP.Free;
    end;

    // Procesamiento con streams
    Stream := TMemoryStream.Create;
    try
      Codec := TDeltaCodecRegistry.GetCodec('bson');
      Codec.EncodeModel(P, Stream);
      Stream.Position := 0;

      DecodedP := TProduct.Create;
      try
        Codec.DecodeModel(Stream, DecodedP);
        WriteLn('Decodificado vía BSON: ', DecodedP.Name);
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

### 5. Transmisión HTTP en Lotes (`StreamJsonTo`)

Para grandes volúmenes de datos, `StreamJsonTo` envía lotes directamente a un `TStream` (como el flujo de respuesta HTTP) manteniendo un consumo de memoria constante:

```pascal
var
  TotalTransmitido: Int64;
begin
  // Transmite lotes de 1.000 registros directamente al flujo de respuesta HTTP
  TotalTransmitido := Query.Where('active = 1').StreamJsonTo(AResponse.ContentStream, 1000);
  AResponse.ContentType := 'application/json';
  AResponse.SendContent;
end;
```

---

## Control de Transacciones

### Transacción Manual
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

### Transacciones con Ámbito RAII
Al utilizar `IDeltaTransaction`, la reversión se produce automáticamente ante cualquier excepción no capturada:

```pascal
var
  Tx: IDeltaTransaction;
begin
  Tx := Engine.BeginTransaction;
  Query.Insert(Order);
  Query.Insert(OrderItem);
  Tx.Commit; // Si ocurre una excepción antes de Commit, se ejecuta rollback automáticamente
end;
```

---

## Motor de Validación

Los modelos pueden definir reglas de validación de negocio sobrescribiendo el método `Validate`:

```pascal
procedure TUser.Validate;
begin
  inherited Validate;
  Validator
    .RuleFor(name.Value, 'Nombre')
      .NotEmpty
      .MinLength(3)
      .MaxLength(100)
    .RuleFor(email.Value, 'E-mail')
      .NotEmpty
      .ValidEmail;
end;
```

Si una regla falla, DeltaModel lanza una excepción `EDeltaValidation` con el detalle de campos y mensajes traducidos.

---

## Batería de Pruebas Automatizadas

DeltaModel cuenta con una suite completa de pruebas que valida operaciones ORM, concurrencia del pool y los 5 códecs (JSON, CBOR, BSON, XML, CSV):

```bash
# Compilar y ejecutar suite de pruebas (540 pruebas automatizadas)
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Ejecutar prueba de concurrencia del pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool
```

---

## Licencia

Distribuido bajo la licencia MIT. Consulte el archivo [LICENSE](LICENSE) para más detalles.
