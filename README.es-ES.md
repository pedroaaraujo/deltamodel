<p align="right">
  [ English ](README.md) | [ Português (Brasil) ](README.pt-BR.md) | [ Español ](README.es-ES.md)
</p>

# DeltaModel

[![FPC](https://img.shields.io/badge/FPC-%3E%3D%203.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-%3E%3D%203.0-orange.svg)](https://www.lazarus-ide.org/)
[![DBMS](https://img.shields.io/badge/Bases%20de%20Datos-6%20SGBD-blue.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

DeltaModel es una biblioteca de micro-ORM tipado, motor de validación y serializador JSON Zero-DOM para Free Pascal y Lazarus. Ofrece generador genérico de consultas (`TDeltaQuery<T>`), migraciones automáticas de DDL, pool de conexiones con gestión RAII, Eager Loading 1:N y serialización de alto rendimiento con formato de coma fija decimal.

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
- **Unidades Principales**: `DeltaModel`, `DeltaModel.Generics`, `DeltaModel.ORM.Connection`, `DeltaModel.ORM.Pool`, `DeltaModel.JsonStreamer`, `DeltaValidator`.

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

Ejemplo completo ejecutable que demuestra definición de modelo, creación de tabla, inserción, consulta tipada y serialización JSON con SQLite:

```pascal
program QuickstartDeltaModel;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  SysUtils, Classes,
  DeltaModel, DeltaModel.Generics, DeltaModel.ORM.Connection;

type
  { Declaración del modelo con propiedades primitivas nativas }
  TProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(Schema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
  end;

class procedure TProduct.ConfigureSchema(Schema: TDeltaModelSchema);
begin
  Schema.Table('products');
  Schema.Field('Id').Column('id').AsPK.AutoInc;
  Schema.Field('Name').Column('name').Required.Size(120);
  Schema.Field('Price').Column('price').Required;
  Schema.Field('Active').Column('active').Required;
end;

var
  Engine: TDeltaORMEngine;
  Query: TDeltaQuery<TProduct>;
  P: TProduct;
  List: TGDeltaModelList<TProduct>;
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
        WriteLn('JSON generado: ', List.ToJson);
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

## Transmisión JSON Zero-DOM

Los serializadores tradicionales construyen árboles intermedios de objetos en heap (`TJSONObject`, `TJSONData`), produciendo alta fragmentación de memoria. DeltaModel utiliza `TDeltaFastJsonStreamer` para escribir directamente en búferes continuos de bytes.

### Características:
- **Cero Asignación DOM**: Produce JSON directo en `RawByteString` o flujos sin generar nodos intermedios.
- **Coma Fija Decimal (`ffFixed`)**: Los números se formatean en coma fija decimal sin notación científica exponencial (`1.2E+05`), evitando fallos en analizadores de JavaScript y clientes móviles.

### Transmisión HTTP en Lotes:
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

DeltaModel cuenta con una suíte completa de pruebas que valida operaciones ORM, concurrencia del pool y transmisión JSON:

```bash
# Compilar y ejecutar suite de pruebas
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Ejecutar prueba de concurrencia del pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool
```

---

## Licencia

Distribuido bajo la licencia MIT. Consulte el archivo [LICENSE](LICENSE) para más detalles.
