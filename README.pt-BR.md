<p align="right">
  [ English ](README.md) | [ Português (Brasil) ](README.pt-BR.md) | [ Español ](README.es-ES.md)
</p>

# DeltaModel

[![FPC](https://img.shields.io/badge/FPC-%3E%3D%203.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-%3E%3D%203.0-orange.svg)](https://www.lazarus-ide.org/)
[![DBMS](https://img.shields.io/badge/Bancos-6%20SGBDs-blue.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

DeltaModel é uma biblioteca de micro-ORM tipado, motor de validação e serializador JSON Zero-DOM para Free Pascal e Lazarus. Disponibiliza construtor genérico de consultas (`TDeltaQuery<T>`), migrações automáticas de DDL, pool de conexões com gerenciamento RAII, Eager Loading 1:N e serialização de alto rendimento com formatação de ponto fixo decimal.

---

## Especificações Técnicas

- **Compilador**: Free Pascal Compiler (FPC) >= 3.2.2.
- **IDE / Ferramental**: Lazarus >= 3.0 (`lazbuild`).
- **Sistemas Operacionais**: Linux (x86_64, aarch64), Windows (x86_64, i386), macOS (x86_64, arm64).
- **SGBDs Suportados**:
  - PostgreSQL (via `pqconnection`)
  - MySQL / MariaDB (via `mysql57conn` / `mysql80conn`)
  - SQLite3 (via `sqlite3conn`)
  - Firebird 2.5 / 3.0+ (via `ibconnection`)
  - Microsoft SQL Server (via `mssqlconn` / ODBC)
  - Oracle Database (via `oracleconnection`)
- **Units Principais**: `DeltaModel`, `DeltaModel.Generics`, `DeltaModel.ORM.Connection`, `DeltaModel.ORM.Pool`, `DeltaModel.JsonStreamer`, `DeltaValidator`.

---

## Instalação

### Método 1: Gerenciador de Pacotes do Lazarus
1. No Lazarus, acesse `Pacote` -> `Abrir Arquivo de Pacote (.lpk)`.
2. Abra `deltamodel_pkg.lpk`.
3. Clique em `Compilar` e em seguida `Usar` -> `Instalar`.

### Método 2: Caminho de Units Manual
Adicione o diretório `src/` ao caminho de busca do compilador:
```bash
fpc -Fusrc minha_aplicacao.pas
```

---

## Quickstart (60 Segundos)

Exemplo completo executável demonstrando definição de modelo, criação de tabela, inserção, consulta tipada e serialização JSON com SQLite:

```pascal
program QuickstartDeltaModel;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  SysUtils, Classes,
  DeltaModel, DeltaModel.Generics, DeltaModel.ORM.Connection;

type
  { Declaração do modelo com propriedades primitivas nativas }
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
  // Inicialização do SQLite em memória
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

    // Construtor tipado de consultas
    Query := TDeltaQuery<TProduct>.Create(Engine);
    try
      // Inserção de registro
      P := TProduct.Create;
      P.Name := 'Teclado Mecânico';
      P.Price := 149.90;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      // Consulta de registro único pela chave primária
      P := Query.FindById(1);
      if P <> nil then
      try
        WriteLn('Produto localizado: ', P.Name, ' - R$ ', P.Price:0:2);
      finally
        P.Free;
      end;

      // Consulta de lista com filtro e ordenação
      List := Query
        .Where('price > :min_price')
        .Param('min_price', 50.0)
        .OrderBy('price DESC')
        .All;
      try
        WriteLn('JSON gerado: ', List.ToJson);
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

## Definição de Modelos & Esquema Fluente

O DeltaModel mapeia tipos primitivos nativos do Pascal (`Int64`, `string`, `Double`, `Boolean`, `TDateTime`) diretamente para as colunas do banco de dados, eliminando o boxing OLE/COM de `Variant` e reduzindo o consumo de memória em até 53% em relação a registros encapsulados por descritores.

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

### Tratamento de Campos Nulos
A nulidade é controlada via bitmask interno:

```pascal
// Definir campo como SQL NULL
Account.SetNull('Limit');

// Verificar se campo é SQL NULL
if Account.IsNull('Limit') then
  WriteLn('Sem limite cadastrado');
```

---

## Conexões e Pool de Conexões

### URLs Padronizadas de Conexão
Motores são instanciados por strings de conexão uniformes:

```pascal
// SQLite (arquivo ou em memória)
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

### Pool de Conexões Thread-Safe
Para serviços multithreaded e APIs web, o `TDeltaConnectionPool` gerencia conexões pré-aquecidas:

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
    // Adquire empréstimo com liberação automática via RAII
    Lease := Pool.Acquire;

    Query := TDeltaQuery<TProduct>.Create(Lease);
    try
      WriteLn('Total de produtos: ', Query.Count);
    finally
      Query.Free;
    end;
    // Conexão retorna ao pool automaticamente ao término do escopo da variável
  finally
    Pool.Free;
  end;
end;
```

---

## Consultas com `TDeltaQuery<T>`

### Busca de Registros
```pascal
// Busca pela Chave Primária
Product := Query.FindById(42);

// Primeiro registro que atende ao critério
Product := Query.Where('active = 1').OrderBy('id ASC').First;

// Todos os registros filtrados
List := Query.Where('price > :price').Param('price', 100.0).All;
```

### Filtros, Parâmetros e Ordenação
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

### Paginação e Limites
```pascal
// Limit e Offset diretos
List := Query.OrderBy('id ASC').Limit(25).Offset(50).All;

// Auxiliar de página: Page(NumeroPagina, TamanhoPagina)
List := Query.OrderBy('id ASC').Page(3, 25).All;
```

### Agregações e Existência
```pascal
TotalRecords := Query.Where('active = 1').Count;
HasPending   := Query.Where('status = :s').Param('s', 'PENDENTE').Exists;
```

### Eager Loading (Relações 1:N)
Para eliminar problemas de $N+1$ consultas, o `Include` carrega coleções filhas em lote numa única consulta secundária (`WHERE fk IN (...)`):

```pascal
// Carrega pedidos e popula Order.Items em apenas 2 queries no total
Orders := OrderQuery.Include('Items').Where('status = :s').Param('s', 'ABERTO').All;
```

---

## Operações de Persistência

```pascal
// Inserir novo registro
Query.Insert(Product);

// Atualizar registro existente (identificado pela chave primária)
Product.Price := 189.90;
Query.Update(Product);

// Inserir ou atualizar (Save verifica chave primária)
Query.Save(Product);

// Excluir registros que atendem ao filtro
Query.Where('active = 0').Delete;

// Inserção em lote (ex.: 500 registros por transação)
TotalInseridos := Query.BulkInsert(ProductList, 500);
```

---

## Streaming JSON Zero-DOM

Serializadores convencionais constroem árvores de objetos intermediários em memória (`TJSONObject`, `TJSONData`), gerando alta fragmentação de heap. O DeltaModel utiliza o `TDeltaFastJsonStreamer` para escrever diretamente em buffers de bytes contíguos.

### Características:
- **Zero Alocação DOM**: Gera JSON direto em `RawByteString` ou streams sem criar nós intermediários.
- **Ponto Fixo Decimal (`ffFixed`)**: Números são serializados estritamente em notação decimal fixa sem expoentes científicos (`1.2E+05`), prevenindo falhas de interpretação em clientes JavaScript e mobile.

### Streaming HTTP em Lotes:
Para tabelas extensas, o `StreamJsonTo` envia lotes diretamente para um `TStream` (como o fluxo de resposta HTTP) com uso constante de memória:

```pascal
var
  TotalEnviado: Int64;
begin
  // Transmite lotes de 1.000 registros diretamente para o stream da resposta HTTP
  TotalEnviado := Query.Where('active = 1').StreamJsonTo(AResponse.ContentStream, 1000);
  AResponse.ContentType := 'application/json';
  AResponse.SendContent;
end;
```

---

## Controle Transacional

### Transação Manual
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

### Transação com Escopo RAII
Com `IDeltaTransaction`, a reversão ocorre automaticamente caso uma exceção não tratada saia do bloco:

```pascal
var
  Tx: IDeltaTransaction;
begin
  Tx := Engine.BeginTransaction;
  Query.Insert(Order);
  Query.Insert(OrderItem);
  Tx.Commit; // Se ocorrer exceção antes de Commit, o rollback é acionado automaticamente
end;
```

---

## Motor de Validação

Modelos podem declarar regras de validação sobrescrevendo `Validate`:

```pascal
procedure TUser.Validate;
begin
  inherited Validate;
  Validator
    .RuleFor(name.Value, 'Nome')
      .NotEmpty
      .MinLength(3)
      .MaxLength(100)
    .RuleFor(email.Value, 'E-mail')
      .NotEmpty
      .ValidEmail;
end;
```

Em caso de violação, o DeltaModel lança a exceção `EDeltaValidation` com o mapeamento dos campos e mensagens localizadas.

---

## Suíte de Testes Automatizados

O DeltaModel possui suíte de testes com cobertura completa de operações ORM, concorrência no pool e streaming JSON:

```bash
# Compilar e executar bateria de testes
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Teste de concorrência do pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool
```

---

## Licença

Distribuído sob a licença MIT. Consulte o arquivo [LICENSE](LICENSE) para mais detalhes.
