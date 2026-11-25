<p align="right">
  [ English ](README.md) | [ Português (Brasil) ](README.pt-BR.md) | [ Español ](README.es-ES.md)
</p>

# DeltaModel

[![FPC](https://img.shields.io/badge/FPC-%3E%3D%203.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-%3E%3D%203.0-orange.svg)](https://www.lazarus-ide.org/)
[![DBMS](https://img.shields.io/badge/Bancos-6%20SGBDs-blue.svg)](https://github.com/pedroaaraujo/deltamodel)
[![Codecs](https://img.shields.io/badge/Codecs-5%20Formatos-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

DeltaModel é uma biblioteca de micro-ORM tipado, motor de validação e framework de serialização multi-formato para Free Pascal e Lazarus. Disponibiliza construtor genérico de consultas (`TDeltaQuery<T>`), migrações automáticas de DDL, pool de conexões com gerenciamento RAII, Eager Loading 1:N e codecs Zero-DOM com suporte a 5 formatos canônicos: JSON, CBOR, BSON, XML e CSV.

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
- **Formatos de Serialização Suportados**: JSON, CBOR (RFC 8949), BSON (v1.1), XML, CSV (RFC 4180).
- **Units Principais**: `DeltaModel`, `DeltaModel.Generics`, `DeltaModel.ORM.Connection`, `DeltaModel.ORM.Pool`, `DeltaModel.Codec`, `DeltaModel.Codec.Cbor`, `DeltaModel.Codec.Bson`, `DeltaModel.Codec.Xml`, `DeltaModel.Codec.Csv`, `DeltaModel.JsonStreamer`, `DeltaValidator`.

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

Exemplo completo executável demonstrando definição de modelo, criação de tabela, inserção, consulta tipada e serialização multi-formato com SQLite:

```pascal
program QuickstartDeltaModel;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}cthreads, cmem,{$ENDIF}
  SysUtils, Classes,
  DeltaModel, DeltaModel.Generics, DeltaModel.ORM.Connection,
  DeltaModel.Codec;

type
  { Declaração do modelo com propriedades primitivas nativas }
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
        // Exportação para JSON
        WriteLn('JSON gerado: ', List.ToJson);

        // Exportação para CSV via Codec Registry
        CsvOutput := TDeltaCodecRegistry.EncodeListToString(List, 'csv');
        WriteLn('CSV gerado:' + sLineBreak + CsvOutput);
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

> **Segurança:** prefira `:parametros` com `.Param(...)` para valores vindos do usuário. Os atalhos `Where(Campo, Valor)` embutem o valor como literal escapado; em MySQL/MariaDB a barra invertida também é duplicada (`TSQLCriteriaHelper.QuoteString`), assumindo o modo padrão sem `NO_BACKSLASH_ESCAPES`. `OrderBy(string)` e `Where(string)` recebem SQL bruto: nunca repasse texto do cliente sem validar contra os campos do modelo.

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

### Consultas Massivas: Schema Flyweight & Cursor Reusável (Zero-Alloc Loop)

Ao processar grandes volumes de dados (ex.: mais de 5.000 registros), o padrão tradicional de ORMs (`Query.All`) aloca uma instância de objeto individual para cada linha, despejando dezenas de milhares de objetos no Heap do Free Pascal. O DeltaModel soluciona esse gargalo através do **Schema Flyweight** e do **Cursor Reusável** canônico do Pascal:

1. **Schema Flyweight (`TModel.Schema`)**:
   Os metadados (`FieldName`, `Size`, `IsRequired`, `DBOptions`) são desacoplados das instâncias de linha do modelo. São configurados uma única vez por classe (`TModel.ConfigureSchema(ASchema)`) e acessados estaticamente (`TMunicipio.Schema`), existindo **exatamente 1 vez na memória** independentemente do número de registros consultados.

2. **Cursor Reusável (`while Query.Next do`)**:
   Uma única instância do modelo (`Query.Current`) é reaproveitada ao longo de todas as linhas percorridas pelo cursor. Para 5.000 linhas, é alocado **EXATAMENTE 1 OBJETO** no Heap (>99% de redução de memória e 2.4x mais rápido):

```pascal
Query := TDeltaQuery<TMunicipio>.Create(Engine);
try
  Query.OrderBy('id ASC').Open;
  while Query.Next do
  begin
    // Query.Current aponta SEMPRE para a mesma instância atualizada
    ProcessarMunicipio(Query.Current);
  end;
  Query.Close;
finally
  Query.Free;
end;
```

3. **Suporte a Enumerador Pascal Nativo (`for M in Query.Cursor do`)**:
```pascal
for M in Query.Cursor do
begin
  // M reutiliza a mesma instância do modelo sem alocações no loop
  WriteLn(M.Nome, ' - ', M.Uf);
end;
```

4. **Callback Funcional (`Query.Each`)**:
```pascal
Query.Each(@ProcessarItem);
```

5. **Streaming Direto para HTTP/Socket (`Query.StreamJsonTo`)**:
Transmite o JSON formatado diretamente para um `TStream` (como o stream de resposta HTTP do Zenith Framework) sem criar lotes ou listas intermediárias no Heap:
```pascal
LinhasEnviadas := Query.StreamJsonTo(Context.Response.OutputStream);
```

> [!NOTE]
> Métodos legados como `TDeltaModelRecords.Add` foram marcados como depreciados (`deprecated 'Use TDeltaQuery.Cursor or StreamJsonTo instead'`).

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

## Serialização Multi-Formato (JSON, CBOR, BSON, XML, CSV)

O DeltaModel implementa uma arquitetura de codecs Zero-DOM extensível gerenciada pela classe `TDeltaCodecRegistry` (unit `DeltaModel.Codec`). Os codecs codificam e decodificam modelos e coleções genéricas diretamente em streams ou buffers de memória sem construir árvores intermediárias de objetos DOM.

### 1. Formatos Suportados e Media Types Canônicos

| Formato | Classe do Codec | Media Types Canônicos | Especificação Padrão |
| :--- | :--- | :--- | :--- |
| **JSON** | `TDeltaJsonCodec` | `application/json` | RFC 8259 |
| **CBOR** | `TDeltaCborCodec` | `application/cbor` | RFC 8949 (Binário) |
| **BSON** | `TDeltaBsonCodec` | `application/bson` | BSON Specification v1.1 (Binário) |
| **XML**  | `TDeltaXmlCodec`  | `application/xml`, `text/xml` | W3C XML 1.0 (Zero-DOM Streamer) |
| **CSV**  | `TDeltaCsvCodec`  | `text/csv` | RFC 4180 |

### 2. Matriz de Tradeoffs Técnicos

- **CBOR (`application/cbor`)**:
  - *Tamanho do payload*: ~40-60% menor que JSON.
  - *Características*: Ponto flutuante IEEE 754 direto, compactação de inteiros (0..23 inline), semântica de tags, zero overhead de conversão string.
  - *Ideal para*: Dispositivos IoT, clientes móveis (Flutter), comunicação inter-microsserviços onde largura de banda e latência de parsing são críticas.
- **BSON (`application/bson`)**:
  - *Tamanho do payload*: Similar ao JSON; prefixado com tamanho de documento em 32 bits.
  - *Características*: Tipagem binária rica (UTC datetime em ms, inteiros de 64 bits, BLOBs binários, elementos indexados).
  - *Ideal para*: Bancos documentais, pipelines de agregação, interoperabilidade com MongoDB.
- **CSV (`text/csv`)**:
  - *Tamanho do payload*: Extremamente compacto para estruturas tabulares planas.
  - *Características*: Conformidade com RFC 4180, cabeçalho automático, escape de aspas duplas, formatação canônica de datas ISO-8601.
  - *Ideal for*: Exportações em lote, relatórios contábeis/financeiros, planilhas, importação em ferramentas analíticas.
- **XML (`application/xml`)**:
  - *Tamanho do payload*: ~20-40% maior que JSON devido às tags de fechamento.
  - *Características*: Declaração UTF-8 padrão, escape rigoroso de entidades (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`), tags auto-fechadas para campos nulos.
  - *Ideal para*: Integrações corporativas enterprise, conformidade bancária/fiscal, sistemas SOAP/XML legados.
- **JSON (`application/json`)**:
  - *Tamanho do payload*: Linha de base padrão.
  - *Características*: Suporte universal em navegadores e SPAs, streaming Zero-DOM, números decimais em ponto fixo (`ffFixed`) sem quebra por notação científica.
  - *Ideal para*: APIs públicas REST e clientes JavaScript/Web.

### 3. Blindagens de Segurança (RFC 8949 Seção 10 & BSON Anti-DoS)

Os decodificadores binários incorporam defesas ativas contra exaustão de memória e estouro de pilha:

- **Anti-OOM (Prevenção de Esgotamento de Memória)**: Antes de executar `SetLength` ou alocar buffers, os decodificadores validam o tamanho declarado contra os bytes restantes no `AInputStream`. Caso o comprimento informado exceda o stream ou ultrapasse `MAX_CBOR_STRING_LENGTH` / `MAX_BSON_STRING_LENGTH` (16 MB), o processo é interrompido imediatamente com `EDeltaDeserializationException`.
- **Limite Rígido de Recursão**: O limite de `MAX_NESTING_DEPTH = 64` impede ataques de estouro de pilha (stack overflow) gerados por estruturas maliciosas profundamente aninhadas.
- **Controle de CStrings Nulas**: `MAX_BSON_CSTRING_LENGTH = 1024` previne loops infinitos de varredura diante de documentos BSON malformados sem terminador `0x00`.

### 4. Utilização do Registro de Codecs

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

    // Codificação direta via registry
    RawBytes := TDeltaCodecRegistry.EncodeModelToString(P, 'cbor');

    // Decodificação a partir de payload binário
    DecodedP := TProduct.Create;
    try
      TDeltaCodecRegistry.DecodeModelFromString(RawBytes, DecodedP, 'application/cbor');
      WriteLn('Decodificado via CBOR: ', DecodedP.Name, ' - R$ ', DecodedP.Price:0:2);
    finally
      DecodedP.Free;
    end;

    // Processamento com streams
    Stream := TMemoryStream.Create;
    try
      Codec := TDeltaCodecRegistry.GetCodec('bson');
      Codec.EncodeModel(P, Stream);
      Stream.Position := 0;

      DecodedP := TProduct.Create;
      try
        Codec.DecodeModel(Stream, DecodedP);
        WriteLn('Decodificado via BSON: ', DecodedP.Name);
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

### 5. Streaming HTTP em Lotes (`StreamJsonTo`)

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

## Documentação OpenAPI (`DescribeApiSchema`)

O `GenerateSchema` (unit `DeltaAPISchema`) lê metadados exclusivos de documentação declarados pelo model. Sobrescreva `DescribeApiSchema` para incluir a descrição do model e `description` / `enum` por propriedade. É separado do `ConfigureSchema`: não altera ORM, validação nem serialização, e funciona tanto com campos `TDF*` quanto com propriedades primitivas.

```pascal
uses DeltaModel.ApiDoc;

class procedure TParcela.DescribeApiSchema(ADoc: TDeltaApiDoc);
begin
  ADoc.Description('Parcela de contas a pagar');
  ADoc.Field('pago').Description('S = paga, N = em aberto').Enum(['S', 'N']);
  ADoc.Field('vencimento').Description('Data de vencimento (dd/mm/aaaa)');
end;
```

- Models aninhados aplicam o próprio `DescribeApiSchema`; o model pai ainda pode descrever a propriedade aninhada.
- Nomes de propriedade inexistentes são ignorados, então a documentação nunca quebra a geração do schema.
- Exemplos de campos `TDF*` saem com o tipo JSON correto (`"example": 12.5`, e não `"12.5"`).

## Suíte de Testes Automatizados

O DeltaModel possui suíte de testes com cobertura completa de operações ORM, concorrência no pool e todos os 5 codecs (JSON, CBOR, BSON, XML, CSV):

```bash
# Compilar e executar bateria de testes (540 testes automatizados)
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Teste de concorrência do pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool
```

---

## Licença

Distribuído sob a licença MIT. Consulte o arquivo [LICENSE](LICENSE) para mais detalhes.
