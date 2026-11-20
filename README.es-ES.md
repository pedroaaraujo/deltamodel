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

**DeltaModel** es una biblioteca moderna, ligera y extensible de micro-ORM y modelado de datos para **Lazarus / Free Pascal (FPC)** que reúne en un único framework:

- 🧱 **Modelado de Datos**: Descriptores de campo fuertemente tipados (`TDF*`) o propiedades nativas primitivas con Fluent Class Schema (`ConfigureSchema`).
- ✅ **Motor de Validación Fluida**: Reglas de validación declarativas en el esquema (`Min`, `Max`, `Range`, `MinLength`, `MaxLength`, `Positive`, `NotEmpty`, `Email`, `CPF`, `CNPJ`, `URL`) y ganchos de validación personalizados (`Validate`).
- 🔄 **Serialización JSON de Alto Rendimiento**: Serializador Zero-DOM `TDeltaFastJsonStreamer` y deserializador léxico streaming `TDeltaFastJsonParser`.
- 🌐 **Internacionalización (i18n)**: Mensajes de validación y excepciones internas multilingües (`en-US`, `pt-BR`, `es-ES`), configurables programáticamente (`SetDeltaLocale`) o mediante variable de entorno (`DELTAMODEL_LOCALE`).
- 📋 **Esquemas OpenAPI / Swagger**: Generación automática de esquemas compatibles con OpenAPI 3.1 con resolución canónica de componentes `$ref` para documentación de API.
- 🗄️ **Micro-ORM Multi-SGBD con Genéricos**:
  - Motor de migración DDL automatizado que inspecciona y crea/actualiza tablas, columnas, claves primarias, claves foráneas, restricciones (`UNIQUE`, `CHECK`) e índices.
  - Generador de consultas SQL fluido y seguro (`TDeltaQuery<T>`) con tipos genéricos y colecciones `TGDeltaModelList<T>`.
  - Carga impaciente (Eager Loading) 1:N automatizada mediante `Include('NombreRelacion')`.
  - Transmisión de respuesta HTTP fragmentada (Chunked Streaming via `StreamJsonTo`) con consumo constante y reducido de heap.
  - Administrador de transacciones atómicas con ámbito RAII y reversión automática (`IDeltaTransaction`, `BeginTransaction`).
  - Operaciones CRUD inteligentes y ganchos de ciclo de vida.

---

## 🚀 SGBD Compatibles

El módulo ORM de DeltaModel ofrece soporte agnóstico para los principales motores de bases de datos relacionales empresariales:

| SGBD | Dialecto | Auto-Incremento | Sintaxis de Paginación | Sintaxis de Retorno |
| :--- | :--- | :--- | :--- | :--- |
| **PostgreSQL** | `ddPostgreSQL` | `SERIAL / BIGSERIAL` | `LIMIT n OFFSET m` | `RETURNING *` |
| **MySQL / MariaDB** | `ddMySQL` | `AUTO_INCREMENT` | `LIMIT n OFFSET m` | INSERT estándar |
| **SQLite3** | `ddSQLite` | `AUTOINCREMENT` | `LIMIT n OFFSET m` | `RETURNING *` |
| **Firebird** | `ddFirebird` | Generador + trigger (2.5); `IDENTITY` (3+) | `ROWS m TO n` | `RETURNING *` |
| **SQL Server (MSSQL)** | `ddMSSQL` | `IDENTITY(1,1)` | `OFFSET-FETCH` | `OUTPUT INSERTED.*` |
| **Oracle Database** | `ddOracle` | `IDENTITY` | `OFFSET-FETCH` | INSERT estándar |

---

## 📦 URL Estándar de Conexión

Inicialice el motor ORM utilizando una URL de conexión estandarizada:

```pascal
uses DeltaModel.ORM.Connection;

var
  Con: TDeltaORMEngine;
begin
  // SQLite en memoria
  Con := TDeltaORMEngine.Create('sqlite://:memory:');

  // SQLite archivo local
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

## ⚡ Pool de Conexiones (Alto Rendimiento y Multiproceso)

Para APIs web, microservicios REST y workers en segundo plano con alta concurrencia, DeltaModel proporciona un pool de conexiones seguro para subprocesos en la unidad `DeltaModel.ORM.Pool`:

### Ventajas Principales:
- **Latencia de Saludo Cero**: Conexiones precalentadas (`MinConnections`) listas para ejecución inmediata.
- **Seguridad Multiproceso Total**: Sincronización de concurrencia mediante `TCriticalSection` y `TEvent`.
- **Limpieza Automática de Recursos (RAII)**: Al usar `IDeltaPooledEngine`, las conexiones regresan automáticamente al pool al salir del ámbito o ante excepciones inesperadas.
- **Rollback Automático**: Las transacciones no confirmadas se revierten de forma segura al devolver la conexión al pool.
- **Comprobación de Estado y Reconexión Automática**: Las conexiones caídas por el servidor se validan y reconectan antes de entregarse (`TestOnBorrow`).

### Ejemplo con RAII:

```pascal
uses DeltaModel.ORM.Pool;

var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaPooledEngine;
begin
  Pool := TDeltaConnectionPool.Create('postgres://user:pass@localhost:5432/mydb?pool_min=5&pool_max=20&pool_timeout=5000');
  try
    // Obtener conexión protegida por RAII
    Lease := Pool.Acquire;

    // Operaciones ORM directas sobre el motor arrendado
    Lease.Save(Person);
    Lease.Find(TPerson, 1);
    
    // ¡La conexión se devuelve automáticamente al pool al salir del bloque!
  finally
    Pool.Free;
  end;
end;
```

---

## 🛠️ Definición de Modelos

DeltaModel admite dos paradigmas de modelado distintos:

### Enfoque A: Objetos de Campo Basados en Descriptores (`TDF*`)

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
    .RuleFor(salary.Value, 'Salario')
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

### Enfoque B: Fluent Class Schema y Genéricos (Cero Conversión Variant)

Para el máximo rendimiento, mínima huella de memoria (-53% de heap) y primitivas nativas de Pascal puras (`Int64`, `string`, `Double`, `Boolean`):

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

#### Consultas con `TDeltaQuery<T>`:

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

## 📊 Resultados de Rendimiento Reales (SQLite3 Disco | Optimización -O3)

Benchmark ejecutado en Linux x86_64 comparando el modelo basado en descriptores (`TDF*`) vs el Enfoque B (Primitivas Nativas + Esquema Fluido):

| Métrica Evaluada | Volumen (Registros) | Modelo Tradicional (`TDF*`) | Enfoque B (Nativo) | Ganancia / Optimización |
| :--- | :---: | :---: | :---: | :---: |
| **Huella RAM (Heap)** | 1.000 | 1.257 bytes/obj (1,20 MB) | 585 bytes/obj (0,56 MB) | **-53,4% RAM** |
| **Huella RAM (Heap)** | 10.000 | 1.257 bytes/obj (11,99 MB) | 585 bytes/obj (5,58 MB) | **-53,5% RAM** |
| **Huella RAM (Heap)** | 50.000 | 1.256 bytes/obj (59,92 MB) | 584 bytes/obj (27,88 MB) | **-53,5% RAM (32 MB liberados)** |
| **Tiempo de Asignación de Objetos** | 50.000 | 257 ms | 73 ms | **3,5x más rápido** |
| **Lectura SQL + Hidratación ORM** | 1.000 | 8 ms (125.000 reg/s) | 4 ms (250.000 reg/s) | **2,0x más rápido** |
| **Lectura SQL + Hidratación ORM** | 10.000 | 83 ms (120.482 reg/s) | 44 ms (227.273 reg/s) | **1,9x más rápido** |
| **Lectura SQL + Hidratación ORM** | 50.000 | 418 ms (119.617 reg/s) | 217 ms (230.415 reg/s) | **1,9x más rápido** |
| **Serialización JSON (FastStreamer)**| 50.000 | 245 ms (204.082 obj/s) | 230 ms (217.391 obj/s) | **23,6 MB/s sostenidos** |
| **Desasignación de Memoria (Free)** | 50.000 | 60 ms | 30 ms | **2,0x más rápido** |

---

## ⚡ Arquitectura de Alto Rendimiento

### 1. `TDeltaFastJsonStreamer`: Transmisión JSON Directa (Cero Asignación DOM)
La serialización JSON tradicional (`fpjson`) asigna cientos de nodos DOM intermedios (`TJSONObject`, `TJSONString`, `TJSONFloat`) para cada fila de la base de datos, provocando alta fragmentación del heap y sobrecarga de recolección de basura.

`TDeltaFastJsonStreamer` serializa instancias y colecciones de `TDeltaModel` directamente en un búfer contiguo `RawByteString`:
- **Cero Asignación de Nodos DOM**: Evita la construcción del árbol durante `Model.ToJson`, `List.ToJson` y `PaginatedList.ToJson`.
- **Formateo de Punto Fijo (`ffFixed`)**: Resuelve la limitación histórica de `fpjson` que genera exponentes científicos (`1.2E+05` o `1.0E-04`). Los números en coma flotante se formatean estrictamente en punto fijo decimal, evitando fallos de análisis sintáctico en integraciones Web, Mobile y fiscales/contables.

### 2. `TColumnBindingMap`: Vinculación ORM Tipada Directa Sin Conversión Variant
- **Mapeo Preasignado**: Resuelve las asociaciones de columnas del dataset una sola vez por consulta.
- **Transferencia Tipada Directa**: Lee valores tipados directamente desde campos de `TDataSet` (`ADS.Fields[I].AsLargeInt`, `.AsString`, `.AsFloat`, `.AsBoolean`) hacia las direcciones de memoria del modelo de destino.
- **Cero Conversiones Variant**: Elimina la sobrecarga de empaquetado OLE/COM Variant en decenas de miles de registros.

### 3. `TRTTICache` & `TModelMetadata`: Caché RTTI Estática
Almacena en caché los metadatos de reflexión de clases (`TypInfo.GetPropList`) protegidos por `TCriticalSection`, garantizando búsquedas rápidas $O(1)$ a lo largo de todo el ciclo de vida del proceso.

---

## 📋 Esquemas OpenAPI / Swagger 3.1

Los modelos DeltaModel generan de forma nativa esquemas de especificación OpenAPI 3.1 con resolución de componentes `$ref`:

```pascal
var
  ObjSchema: string;
  RefObjSchema: string;
  PaginatedSchema: string;
begin
  // Genera esquema autónomo: {"type": "object", "properties": { ... }}
  ObjSchema := TPerson.SwaggerSchemaObject(True);

  // Registra el modelo en components.schemas y devuelve: {"$ref": "#/components/schemas/Person"}
  RefObjSchema := TPerson.SwaggerSchemaRefObject;

  // Genera esquema envolvente paginado canónico:
  PaginatedSchema := TPerson.SwaggerSchemaRefPaginated;
end;
```

---

## 🧪 Batería de Pruebas Automatizadas

DeltaModel incluye cobertura de pruebas automatizadas exhaustiva con **344 pruebas aprobadas al 100%**:

```bash
# Compilar y ejecutar la suite de pruebas principal
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Ejecutar pruebas de concurrencia del pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool

# Ejecutar benchmarks en el mundo real
fpc -O3 -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/benchmark_real.pas
/tmp/benchmark_real
```

---

## 📦 Compilación del Paquete Lazarus

El archivo de paquete Lazarus se encuentra en `deltamodel_pkg.lpk`:

```bash
lazbuild --build-all deltamodel_pkg.lpk
```

---

## 📄 Licencia

Distribuido bajo la Licencia MIT. Consulte `LICENSE` para más información.
