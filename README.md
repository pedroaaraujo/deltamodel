# DeltaModel

[![Lazarus](https://img.shields.io/badge/Lazarus-2.2%2B-blue.svg)](https://www.lazarus-ide.org/)
[![FreePascal](https://img.shields.io/badge/FPC-3.2.2%2B-green.svg)](https://www.freepascal.org/)
[![Multi--Database](https://img.shields.io/badge/Databases-6%20SGDBs-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**DeltaModel** é um microframework leve, moderno e extensível para **Lazarus / Free Pascal (FPC)** que reúne em uma única biblioteca:

- 🧱 **Modelagem de Dados**: Campos fortemente tipados com controle explícito de nulabilidade (`Null` vs `Required`).
- ✅ **Motor de Validação Fluente**: Validação de CPF, CNPJ (numérico e alfanumérico - padrão RFB), E-mail, URL, Ranges, Regex e regras de negócio personalizadas.
- 🔄 **Serialização JSON**: Conversão bidirecional entre Objetos/Listas e JSON (`ToJson`, `FromJson`), com suporte a objetos aninhados.
- 📋 **OpenAPI / Swagger Schemas**: Geração automática de esquemas compatíveis com OpenAPI 3.0 para documentação de APIs.
- 🗄️ **Micro-ORM Multi-SGDB**:
  - DDL automatizado que inspeciona e cria/atualiza tabelas, colunas, chaves primárias, chaves estrangeiras, constraints (`UNIQUE`, `CHECK`) e índices de banco de dados (simples e compostos).
  - Construtor fluente de consultas SQL com suporte a condições tipadas (`Where`, `WhereBetween`, `WhereIn`, etc.) e Joins (`InnerJoin`, `LeftJoin`, etc.).
  - Operações CRUD inteligentes (`Save`, `Insert`, `Update`, `Delete`, `Find`, `Count`).
  - Hooks de ciclo de vida (`BeforeInsert`, `AfterInsert`, `BeforeUpdate`, etc.).

---

## 🚀 SGDBs Suportados

O módulo ORM do DeltaModel oferece suporte nativo e agnóstico aos principais sistemas gerenciadores de banco de dados:

| SGDB | Dialeto | Auto-Incremento | Sintaxe de Paginação | Sintaxe Returning |
| :--- | :--- | :--- | :--- | :--- |
| **PostgreSQL** | `ddPostgreSQL` | `SERIAL / BIGSERIAL` | `LIMIT n OFFSET m` | `RETURNING *` |
| **MySQL / MariaDB** | `ddMySQL` | `AUTO_INCREMENT` | `LIMIT n OFFSET m` | Standard INSERT |
| **SQLite3** | `ddSQLite` | `AUTOINCREMENT` | `LIMIT n OFFSET m` | `RETURNING *` |
| **Firebird** | `ddFirebird` | Generator + trigger (2.5); `IDENTITY` (3+) | `ROWS m TO n` | `RETURNING *` |
| **SQL Server (MSSQL)** | `ddMSSQL` | `IDENTITY(1,1)` | `OFFSET-FETCH` | `OUTPUT INSERTED.*` |
| **Oracle Database** | `ddOracle` | `IDENTITY` | `OFFSET-FETCH` | Standard INSERT |

---

## 📦 Conexão por URL Padronizada

Inicialize o engine ORM passando uma connection string em formato de URL padronizada:

```pascal
uses DeltaModel.ORM.Connection;

var
  Con: TDeltaORMEngine;
begin
  // SQLite em memória
  Con := TDeltaORMEngine.Create('sqlite://:memory:');

  // SQLite em arquivo local
  Con := TDeltaORMEngine.Create('sqlite:///var/data/app.db');

  // PostgreSQL
  Con := TDeltaORMEngine.Create('postgres://usuario:senha@localhost:5432/meubanco?charset=UTF8');

  // MySQL / MariaDB
  Con := TDeltaORMEngine.Create('mysql://root:senha@127.0.0.1:3306/meubanco');

  // Firebird
  Con := TDeltaORMEngine.Create('firebird://sysdba:masterkey@localhost:3050//var/db/empresa.fdb');

  // Microsoft SQL Server
  Con := TDeltaORMEngine.Create('mssql://sa:senha@127.0.0.1:1433/meubanco');

  // Oracle Database
  Con := TDeltaORMEngine.Create('oracle://system:senha@localhost:1521/XE');
```

---

## ⚡ Pool de Conexões (High Performance & Multithread)

Para APIs web e daemons com múltiplas threads concorrentes, o DeltaModel disponibiliza um pool de conexões thread-safe de alta performance na unit [`DeltaModel.ORM.Pool`].

### Principais Vantagens:
- **Zero Handshake Latency**: Conexões pré-aquecidas (`MinConnections`) prontas para uso.
- **Thread-Safety Total**: Suporte a dezenas de threads simultâneas com sincronização eficiente via `TCriticalSection` e `TEvent`.
- **Liberação Automática (RAII)**: Ao usar `IDeltaPooledEngine`, a conexão é devolvida automaticamente ao pool ao sair do escopo ou em caso de exceção.
- **Auto-Rollback**: Transações esquecidas abertas sofrem rollback automático ao retornar ao pool.
- **Health Check & Auto-Reconnect**: Conexões derrubadas pelo SGBD são reconectadas automaticamente antes do empréstimo (`TestOnBorrow`).

### Exemplo de Uso com RAII:

```pascal
uses DeltaModel.ORM.Pool;

var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaPooledEngine;
begin
  // Configuração direta ou via parâmetros na própria URL
  Pool := TDeltaConnectionPool.Create('postgres://user:pass@localhost:5432/meubanco?pool_min=5&pool_max=20&pool_timeout=5000');
  try
    // Obtém uma conexão protegida por interface (RAII)
    Lease := Pool.Acquire;

    // Métodos ORM diretamente acessíveis pelo Lease
    Lease.Save(Person);
    Lease.Find(TPerson, 1);
    
    // Ao final do bloco (ou ao sair de escopo), Lease devolve a conexão ao pool automaticamente!
  finally
    Pool.Free;
  end;
end;
```

### Exemplo com Callbacks e Transações:

```pascal
// Execução simples garantindo liberação
Pool.Execute(procedure(Engine: TDeltaORMEngine)
begin
  Engine.Save(Person);
end);

// Execução transacional atômica com auto-commit / auto-rollback
Pool.InTransaction(procedure(Engine: TDeltaORMEngine)
begin
  Engine.Insert(Pedido);
  Engine.Insert(ItensPedido);
end);
```

---

## 🛠️ Definição de Modelos

Os modelos herdam de `TDeltaModel` e utilizam os tipos de campos especializados do DeltaModel:

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
    Fcpf: TDFStringRequired;
    Fsalary: TDFCurrencyRequired;
    Factive: TDFBooleanRequired;
    Fcreated: TDFDateTimeNull;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property email: TDFStringRequired read Femail write Femail;
    property cpf: TDFStringRequired read Fcpf write Fcpf;
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
  // Configurações de banco de dados
  Self.TableName := 'persons';
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.name.Size := 120;
  Self.email.Size := 150;
  Self.cpf.Size := 14;

  // Constraints e Índices a nível de campo
  Self.email.IsUnique := True;    // Garante coluna UNIQUE no banco
  Self.created.IsIndexed := True; // Cria índice secundário para performance
end;

procedure TPerson.Configure;
begin
  inherited Configure;
  // Constraints e Índices compostos a nível de tabela
  AddUniqueConstraint('uq_person_cpf', ['cpf']);
  AddIndex('ix_person_name_salary', ['name', 'salary']);
end;

procedure TPerson.Validate;
begin
  inherited Validate; // Valida campos obrigatórios automaticamente
  
  // Regras de validação de negócio com mensagens em português
  Validator
    .RuleFor(cpf.Value, 'CPF')
      .ValidCPF
    .RuleFor(email.Value, 'E-mail')
      .ValidEmail
    .RuleFor(salary.Value, 'Salário')
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

### Tipos de Campos Disponíveis

| Tipo Nulo | Tipo Obrigatório | Tipo Pascal |
| :--- | :--- | :--- |
| `TDFIntNull` | `TDFIntRequired` | `Integer` |
| `TDFInt64Null` | `TDFInt64Required` | `Int64` |
| `TDFStringNull` | `TDFStringRequired` | `String` |
| `TDFBooleanNull` | `TDFBooleanRequired` | `Boolean` |
| `TDFFloatNull` | `TDFFloatRequired` | `Double` |
| `TDFCurrencyNull` | `TDFCurrencyRequired` | `Currency` |
| `TDFDateTimeNull` | `TDFDateTimeRequired` | `TDateTime` |
| `TDFDateNull` | `TDFDateRequired` | `TDate` |
| `TDFHasOne` | *(Virtual 1:1)* | `TObject / TDeltaModel` |
| `TDFHasMany` | *(Virtual 1:N)* | `TCustomDeltaModelList` |

---

## 🔗 Relacionamentos entre Modelos

### Relacionamento 1:1 (`TDFHasOne`)

O `TDFHasOne` permite associar um modelo dependente a outro modelo de forma transparente:
- **Campo Virtual**: Não gera coluna física na tabela pai durante o DDL (`CREATE TABLE` / `ALTER TABLE`).
- **Serialização JSON Completa**: Serializa como objeto JSON aninhado quando preenchido e como `null` quando vazio.
- **Desserialização Automática**: Ao chamar `FromJson`, instancia e popula automaticamente a classe relacionada configurada.
- **Cópia Profunda (Clone)**: Operações de `Clone` e `CopyObject` clonam recursivamente a instância do modelo dependente.
- **OpenAPI / Swagger 3.0**: Gera esquema com `type: 'object'` incorporando as propriedades do modelo relacionado.

```pascal
type
  { Modelo Dependente (Ex: Perfil) }
  TProfileModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FBio: TDFStringNull;
    FTwitter: TDFStringNull;
  published
    property Id: TDFIntRequired read FId write FId;
    property Bio: TDFStringNull read FBio write FBio;
    property Twitter: TDFStringNull read FTwitter write FTwitter;
  public
    procedure AfterConstruction; override;
  end;

  { Modelo Principal (Ex: Usuário) com HasOne }
  TUserModel = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FUsername: TDFStringRequired;
    FProfile: TDFHasOne;
  published
    property Id: TDFIntRequired read FId write FId;
    property Username: TDFStringRequired read FUsername write FUsername;
    property Profile: TDFHasOne read FProfile write FProfile;
  public
    procedure Configure; override;
  end;

procedure TUserModel.Configure;
begin
  inherited Configure;
  // Configura a relação 1:1 apontando para a classe dependente e as chaves
  Profile.References(TProfileModel, 'user_id', 'id');
end;
```

## 🛡️ Constraints e Índices de Banco de Dados

O DeltaModel oferece suporte completo e declarativo para restrições de integridade e índices de performance em todos os 6 dialetos de SGBD:

### 1. Nível de Coluna (Fluente e Direto)

Defina restrições diretamente nos campos em `AfterConstruction` ou `Configure`:

```pascal
// Marca a coluna como UNIQUE
email.IsUnique := True; 
// Ou via DBOptions:
email.DBOptions := email.DBOptions + [dboUnique];

// Cria índice de performance para a coluna
createdAt.IsIndexed := True;
// Ou via DBOptions:
createdAt.DBOptions := createdAt.DBOptions + [dboIndex];
```

### 2. Nível de Tabela (Compostas e Nomeadas)

No método `Configure` do seu modelo, declare índices e constraints compostas multi-coluna:

```pascal
procedure TTenantUser.Configure;
begin
  inherited Configure;

  // 1. Constraint UNIQUE Composta (Multi-column)
  AddUniqueConstraint('uq_tenant_email', ['tenant_id', 'email']);
  // Sem nome (gera UQ_tablename_cols automaticamente):
  AddUniqueConstraint(['company_id', 'code']);

  // 2. Índices Secundários (B-Tree de alta performance)
  AddIndex('ix_tenant_status_date', ['tenant_id', 'status', 'created_at']);
  // Sem nome (gera IX_tablename_cols automaticamente):
  AddIndex(['category_id', 'created_at']);

  // 3. Índice Único Composto
  AddUniqueIndex('ix_tenant_cpf', ['tenant_id', 'cpf']);

  // 4. Constraint CHECK
  AddCheckConstraint('chk_age_adult', 'age >= 18');
end;
```

### 3. Comportamento Multi-Dialeto Automatizado

O DDL Builder e o Schema Migrator traduzem as definições para a sintaxe nativa de cada SGBD:

| Dialeto | `UNIQUE` no `CREATE TABLE` | `UNIQUE` via `ALTER TABLE` | Índices Secundários / Únicos | Sanitização de Nomes |
| :--- | :--- | :--- | :--- | :--- |
| **SQLite** | `CONSTRAINT ... UNIQUE(...)` | `CREATE UNIQUE INDEX IF NOT EXISTS` | `CREATE [UNIQUE] INDEX IF NOT EXISTS` | Padrão |
| **PostgreSQL** | `ADD CONSTRAINT ... UNIQUE` | `ADD CONSTRAINT ... UNIQUE` | `CREATE [UNIQUE] INDEX IF NOT EXISTS` | Padrão |
| **MySQL** | `ADD CONSTRAINT ... UNIQUE` | `ADD CONSTRAINT ... UNIQUE` | `CREATE [UNIQUE] INDEX` | Padrão |
| **Firebird** | `ADD CONSTRAINT ... UNIQUE` | `ADD CONSTRAINT ... UNIQUE` | `CREATE [UNIQUE] INDEX` | Padrão |
| **MSSQL** | `ADD CONSTRAINT ... UNIQUE` | `ADD CONSTRAINT ... UNIQUE` | `CREATE [UNIQUE] INDEX` | Padrão |
| **Oracle** | `ADD CONSTRAINT ... UNIQUE` | `ADD CONSTRAINT ... UNIQUE` | `CREATE [UNIQUE] INDEX` | Truncamento automático para <= 30 chars |

---

## 🗄️ DDL Automatizado (`TDeltaORMSchema`)

O `TDeltaORMSchema` inspeciona a estrutura das classes e cria ou atualiza as tabelas, colunas, constraints e índices no SGDB conectado:

```pascal
var
  Schema: TDeltaORMSchema;
begin
  Schema := TDeltaORMSchema.Create(Con);
  try
    Schema.RegisterModel(TPerson.Create);
    // Schema.RegisterModel(TOrder.Create);

    // True para executar diretamente no banco conectado (cria tabelas, campos, FKs, Unique e Índices)
    Schema.PrepareDB(True);

    // Número da última migração aplicada por PrepareDB nesta base
    WriteLn('Versão do banco: ', Schema.DatabaseVersion);

    // Inspecione o SQL gerado para o dialeto ativo:
    // WriteLn(Schema.SQL.Text);
  finally
    Schema.Free;
  end;
end;
```

### Ordem de execução das migrations

O preparo gera o plano completo antes de executar DDL, nesta ordem global:

1. `CREATE TABLE` para todas as tabelas ausentes.
2. Um `ALTER TABLE ADD` por campo ausente em tabelas existentes.
3. Constraints locais (`UNIQUE` e `CHECK`) geradas para a estrutura nova.
4. Índices definidos pelos modelos.
5. FKs pendentes, inclusive sobre colunas que já existem.
6. Registro da versão concluída.

Assim, o registro de um modelo filho antes do pai e as referências circulares
não antecipam FKs aos campos ou às chaves únicas geradas no mesmo plano.
As referências usam o `TableName` do modelo de destino. Os nomes de FKs incluem
a coluna de origem e um sufixo estável quando precisam ser encurtados.
A inspeção identifica FKs pela coluna e destino, aceitando nomes antigos;
ela não substitui FKs existentes para alterar `ON DELETE`/`ON UPDATE`.
O MySQL mantém a validação de FKs habilitada conforme a configuração da conexão.

No SQLite, FKs continuam dentro do `CREATE TABLE`. Uma FK ausente em uma tabela
já existente interrompe o planejamento antes de executar qualquer DDL, com uma
mensagem solicitando reconstrução da tabela. A reconstrução automática não está
implementada; faça uma migration explícita preservando dados, índices e triggers.
`PrepareDB(False)` permite inspecionar o plano em `Schema.SQL` sem aplicá-lo.

Teste específico de migrations:

```bash
fpc -B -FEtests -FUtests -Fusrc tests/test_migrations.lpr
./tests/test_migrations
```

### Integração real com Podman

Execute `./tests/podman/run.sh` a partir deste projeto. O script compila o código
atual e executa testes unitários e de integração em PostgreSQL, MySQL, SQLite e
Firebird moderno e legado (2.5), usando bases descartáveis. Veja
[instruções e imagens](tests/podman/README.md) e
[resultados de execução](tests/podman/RESULTS.md).

O migrador consulta os nomes dos índices existentes para não tentar recriá-los
nem gerar versões sem alterações. No Firebird, a infraestrutura do histórico é
confirmada antes das migrations dos modelos; o DDL dos modelos e sua entrada
no histórico permanecem na transação seguinte. `fkRestrict` é emitido como
`NO ACTION`, sintaxe aceita pelo Firebird.

### Auto incremento e versão do Firebird

`PrepareDB` consulta `RDB$GET_CONTEXT('SYSTEM', 'ENGINE_VERSION')` no servidor,
inclusive em `PrepareDB(False)`. Essa versão é independente de `DatabaseVersion`,
que numera migrations, e da versão da biblioteca cliente.

- **Firebird 2.5:** cria a coluna inteira, um `CREATE GENERATOR` e um trigger
  `BEFORE INSERT` que chama `GEN_ID(..., 1)` somente se `NEW.campo IS NULL`.
- **Firebird 3+:** usa `GENERATED BY DEFAULT AS IDENTITY`. O próprio servidor
  mantém o generator interno; o ORM não cria generator ou trigger adicional.

Os caminhos funcionam tanto em novas tabelas quanto em novas colunas. Campos
inteiros de 32/64 bits podem usar `dboAutoInc`, com ou sem `dboPrimaryKey` no
Firebird. Valores explícitos são preservados. No ORM, zero/vazio significa
solicitar geração automática; para inserir zero explicitamente use SQL direto.
`Insert`/`Save` recuperam o valor Firebird com `RETURNING campo`, na mesma operação.
Valores explícitos não sincronizam o generator Firebird com o maior ID da tabela.

Nomes legados usam `GEN_<tabela>_<campo>` e `BI_<tabela>_<campo>`, com limite de
31 caracteres e sufixo hash para nomes longos. Reexecutar o preparo não recria
nem reinicia generators. Colunas existentes não são convertidas automaticamente
entre trigger e identity ao atualizar o servidor. Adicionar campo obrigatório a
uma tabela com dados pode exigir migration manual para preencher valores prévios.
Esta compatibilidade não acrescenta suporte ao tipo `BOOLEAN` no Firebird 2.5.

O builder usado diretamente, sem conexão, mantém o padrão Firebird 3+ por
compatibilidade. Para gerar SQL legado explicitamente:

```pascal
SQL := TDDLBuilder.CreateTableAndFields(Model, Statements, ddFirebird, True, 2);
// Execute SQL e depois cada item de Statements como um comando completo.
```

A lista `Statements` é obrigatória para campos auto incremento legados. Não
separe o corpo do trigger por ponto e vírgula. Falha na consulta/identificação
da versão interrompe o planejamento, sem assumir silenciosamente uma versão.
Referências: [identity no Firebird 3](https://www.firebirdsql.org/file/documentation/chunk/en/refdocs/fblangref30/fblangref30-ddl-table.html)
e [ENGINE_VERSION](https://www.firebirdsql.org/refdocs/langrefupd25-intfunc-get_context.html).

### Versionamento das alterações

`PrepareDB(True)` calcula o DDL pendente comparando os modelos com a estrutura
atual. Quando há alterações de modelo, ele aplica esse conjunto como uma
migração e registra a próxima versão na tabela
`deltamodel_schema_migrations` (`version`, `migration_name`, `checksum` e
`applied_at`). A tabela de histórico é criada automaticamente; tabelas criadas
por versões anteriores recebem as colunas de nome e checksum no próximo preparo.
Chamadas sem DDL pendente não criam uma nova versão. O nome segue o formato
`preparedb_000001`, e o checksum identifica o conjunto de DDL daquela versão.

Se qualquer comando falhar, `PrepareDB` propaga a exceção e não registra a
migração como concluída. Isso permite corrigir o modelo ou o banco e executar o
preparo novamente. Em bancos que confirmam DDL implicitamente, comandos
anteriores ao erro podem já ter sido aplicados; na próxima chamada, a inspeção
da estrutura identifica o que ainda falta.

---

## 🔄 Operações CRUD Inteligentes

### Inserção e Atualização com `Save`
O método `Save` detecta se a Chave Primária possui valor:
- Se a PK estiver vazia ou com valor padrão (0), executa `INSERT`.
- Se a PK estiver preenchida, executa `UPDATE`.

```pascal
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.name.Value := 'Carlos Eduardo';
    Person.email.Value := 'carlos@empresa.com';
    Person.cpf.Value := '123.456.789-00';
    Person.salary.Value := 7500.00;
    Person.active.Value := True;

    // INSERT automático (preenche Person.id gerado pelo banco)
    Con.Save(Person);

    // Atualização
    Person.salary.Value := 8200.00;
    Con.Save(Person); // Executa UPDATE automático

    // Remoção
    Con.Delete(Person);
  finally
    Person.Free;
  end;
end;
```

### Inserção em Massa de Alta Performance com `BulkInsert`

Para inserção de dezenas, centenas ou milhares de registros em lote com máxima performance e segurança transacional:
- **Execução Atômica e Transacional**: Garante consistência total (se qualquer registro falhar na validação ou no banco, rollback automático é executado).
- **Chunking / Particionamento em Lotes**: Divide automaticamente grandes volumes em lotes configuráveis (`ABatchSize`, padrão 500), respeitando os limites de parâmetros de cada SGBD.
- **Suporte Multi-RDBMS**:
  - **PostgreSQL / SQLite / MySQL / MSSQL**: Gera instrução `INSERT INTO table (cols) VALUES (...), (...)` multi-row otimizada.
  - **Firebird**: Utiliza blocos anônimos `EXECUTE BLOCK AS BEGIN INSERT INTO ...; END`.
  - **Oracle**: Utiliza `INSERT ALL INTO ... SELECT 1 FROM DUAL`.
- **Tratamento Inteligente de Chave Primária AutoInc**: Ignora campos `dboAutoInc` vazios para permitir geração automática pelo banco.
- **Ciclo de Vida Completo**: Executa `BeforeInsert`, `Validate` e `AfterInsert` para cada registro.

```pascal
// Exemplo 1: BulkInsert com array aberto
var
  Persons: array of TDeltaModel;
  I: Integer;
begin
  SetLength(Persons, 1000);
  for I := 0 to 999 do
  begin
    Persons[I] := TPerson.Create;
    TPerson(Persons[I]).name.Value := 'Pessoa ' + IntToStr(I);
    TPerson(Persons[I]).salary.Value := 3000 + I;
  end;
  try
    // Insere os 1000 registros particionados em lotes de 500 (ou customizado)
    Con.BulkInsert(Persons);
  finally
    for I := 0 to 999 do Persons[I].Free;
  end;
end;

// Exemplo 2: BulkInsert com TDeltaModelList
var
  List: TDeltaModelList;
  P: TPerson;
  I: Integer;
begin
  List := TDeltaModelList.Create;
  try
    List.SetDeltaModelClass(TPerson);
    for I := 1 to 500 do
    begin
      P := TPerson.Create;
      P.name.Value := 'Cliente ' + IntToStr(I);
      List.Add(P);
    end;

    // Inserção direta via Engine, Lease ou Pool
    Con.BulkInsert(List);

    // Também diretamente no Pool:
    // Pool.BulkInsert(List);
  finally
    List.Free;
  end;
end;
```

---

## 🔍 Consultas Avançadas e Query Builder

### 1. Critérios Tipados (`Where`, `AndWhere`, `OrWhere`)

Além de strings SQL puras, você pode utilizar métodos com operadores tipados:

```pascal
var
  List: TDeltaModelList;
begin
  List := Con.Query(TPerson)
    // Condição com igualdade simples: campo, valor
    .Where('active', True)
    // Comparação tipada: campo, operador, valor
    .AndWhere('salary', opGreaterThanOrEqual, 5000)
    // Outros helpers tipados:
    .WhereBetween('salary', 5000, 15000)
    .WhereIn('id', [1, 2, 3, 4, 5])
    .WhereNotNull('email')
    .OrderBy('name ASC')
    .Page(1, 10) // Página 1, 10 registros por página
    .All;
  try
    // Processa lista de registros
  finally
    List.Free;
  end;
end;
```

### 2. Operadores Tipados Suportados (`TComparisonOp`)

- `opEqual` (`=`)
- `opNotEqual` (`<>`)
- `opGreaterThan` (`>`)
- `opGreaterThanOrEqual` (`>=`)
- `opLessThan` (`<`)
- `opLessThanOrEqual` (`<=`)
- `opLike` (`LIKE`)
- `opILike` (`ILIKE`)
- `opIn` / `opNotIn`
- `opIsNull` / `opIsNotNull`

### 3. Joins Relacionais

```pascal
var
  Builder: TDMSQLBuilder;
  Sql: string;
begin
  Builder := TDMSQLBuilder.Create(Order, ddPostgreSQL);
  try
    Sql := Builder
      .TableAlias('o')
      .InnerJoin('customers', 'c.id = o.customer_id', 'c')
      .LeftJoin('payments', 'p.order_id = o.id', 'p')
      .Where('o.status', 'COMPLETED')
      .OrderBy('o.created_at DESC')
      .Build;
  finally
    Builder.Free;
  end;
end;
```

---

## ⚡ Serialização JSON & Listas

O DeltaModel fornece serialização bidirecional robusta com distinção clara entre coleções puras (arrays de objetos) e coleções paginadas:

### 1. Objeto Individual
```pascal
var
  Person: TPerson;
  JsonStr: string;
begin
  Person := TPerson.Create;
  try
    Person.name.Value := 'Ana Beatriz';
    Person.email.Value := 'ana@empresa.com';

    // Objeto -> JSON: {"name": "Ana Beatriz", "email": "ana@empresa.com", ...}
    JsonStr := Person.ToJson;

    // JSON -> Objeto
    Person.FromJson('{"name": "Beatriz Lima", "email": "beatriz@empresa.com"}');
  finally
    Person.Free;
  end;
end;
```

### 2. Lista Pura de Objetos (`TDeltaModelList` e `TGDeltaModelList<T>`)
Ao invocar `ToJson`, listas comuns ou genéricas geram **exclusivamente um array JSON de objetos** (`[ {...}, {...} ]`):

```pascal
type
  TPersonList = specialize TGDeltaModelList<TPerson>;

var
  List: TPersonList;
  P: TPerson;
  JsonArr: RawByteString;
begin
  List := TPersonList.Create;
  try
    P := TPerson.Create;
    P.name.Value := 'Ana';
    List.Add(P);

    // Gera: [{"id": null, "name": "Ana", ...}]
    JsonArr := List.ToJson;

    // Desserializa tanto de array direto quanto de payload paginado:
    List.FromJson('[{"name": "Carlos"}]');
    WriteLn(List[0].name.AsString); // Acesso fortemente tipado direto
  finally
    List.Free;
  end;
end;
```

### 3. Lista Paginada (`TDeltaModelPaginatedList` e `TGDeltaModelPaginatedList<T>`)
Listas paginadas empacotam os itens em um envelope com metadados para APIs RESTful:

```pascal
type
  TPersonPaginatedList = specialize TGDeltaModelPaginatedList<TPerson>;

var
  PagList: TPersonPaginatedList;
  JsonPag: RawByteString;
begin
  PagList := TPersonPaginatedList.Create;
  try
    // Popula itens
    PagList.Add(TPerson.Create);
    PagList.page := 1;
    PagList.page_size := 10;
    PagList.total_records := 100;

    // Gera envelope JSON canônico:
    // {
    //   "items": [ {...} ],
    //   "page": 1,
    //   "page_size": 10,
    //   "total_records": 100,
    //   "total_pages": 10
    // }
    JsonPag := PagList.ToJson;

    // Desserializa preservando metadados e itens
    PagList.FromJson(JsonPag);
  finally
    PagList.Free;
  end;
end;
```

---

## 📋 Geração de Esquemas OpenAPI / Swagger

Gere esquemas compatíveis com OpenAPI 3.0 para documentação de rotas e APIs. O método original `SwaggerSchema` foi depreciado em prol de métodos semânticos e tipados:

### 1. Métodos de Classe no Modelo (`TDeltaModel`)

```pascal
var
  ObjSchema: string;
  ArraySchema: string;
  PaginatedSchema: string;
begin
  // 1. Esquema para um único objeto: {"type": "object", "properties": { ... }}
  ObjSchema := TPerson.SwaggerSchemaObject(AddExamples := True);

  // 2. Esquema para array de objetos: {"type": "array", "items": {"type": "object", ...}}
  ArraySchema := TPerson.SwaggerSchemaArray(AddExamples := True);

  // 3. Esquema para lista paginada: {"type": "object", "properties": {"items": ..., "page": ..., ...}}
  PaginatedSchema := TPerson.SwaggerSchemaPaginated(AddExamples := True);

  // NOTA: O método SwaggerSchema(IsArray, IsPaginated) está depreciado (deprecated)
end;
```

### 2. Métodos de Instância nas Listas

```pascal
var
  List: TPersonList;
  PagList: TPersonPaginatedList;
  JsonDoc: TJSONObject;
begin
  // Schema em formato array para listas comuns/genéricas:
  JsonDoc := List.SwaggerSchemaArray;
  JsonDoc.Free;

  // Schema em formato paginado para listas paginadas:
  JsonDoc := PagList.SwaggerSchemaPaginated;
  JsonDoc.Free;
end;
```

---

## 🧪 Suíte de Testes Automatizados

O DeltaModel possui uma cobertura abrangente de testes automatizados unitários e de integração, garantindo alta estabilidade, precisão de tipos e performance:

- **`tests/test_suite.lpr`**: Suíte consolidada com **218 testes unitários e de integração (100% aprovados)** cobrindo:
  - **Connection URL Parser** (SQLite memória/arquivo/relativo, PostgreSQL, MySQL, Firebird).
  - **Motor de Validação** (CPF e CNPJ numérico/alfanumérico com/sem máscara e dígitos inválidos, E-mail, UTF-8 multibyte, Between, GreaterThan, etc.).
  - **Serialização & Precisão Numérica** (JSON com escape de aspas, UTF-8, integridade de `Int64` sem truncamento, CSV tipado, `Clone`).
  - **Listas e Paginação** (distinção estrita entre `TDeltaModelList` gerando array puro de objetos e `TDeltaModelPaginatedList` gerando envelopes com metadados, suporte a genéricas `TGDeltaModelList<T>` e `TGDeltaModelPaginatedList<T>`).
  - **Geração de OpenAPI 3.0 / Swagger Schema** (`SwaggerSchemaObject`, `SwaggerSchemaArray`, `SwaggerSchemaPaginated` e compatibilidade com métodos legados).
  - **Geração de DDL Multi-Dialeto** (SQLite, PostgreSQL, Firebird, Oracle, MSSQL).
  - **Ciclo de Vida & Hooks** (`BeforeInsert`, `AfterInsert`, `BeforeUpdate`, `AfterUpdate`, `BeforeSave`, `AfterSave`, `BeforeDelete`, `AfterDelete` e tratamento de exceções).
  - **ORM CRUD, Query Builder & Transações** (`Where`, `WhereBetween`, `WhereIn`, `Limit`, `Offset`, `AsJsonString`, rollback automático e `InTransaction`).
  - **Constraints (`UNIQUE`, `CHECK`) & Índices de Banco de Dados** (restrições a nível de campo e tabela, geração de índices secundários e únicos, sanitização de identificadores para Oracle e validação de violação de integridade em runtime com SQLite).
  - **Relacionamento 1:1 (`TDFHasOne`)** (metadados virtuais sem coluna física em DDL, serialização aninhada/null, desserialização com instanciação dinâmica, cópia profunda em `Clone` e esquema Swagger com `type: 'object'`).
- **`tests/test_pool.lpr`**: Testes de concorrência multithread do Connection Pool (10 threads simultâneas vs pool de 4 conexões, timeouts, sanitização RAII e métricas).
- **`tests/test_bulk_insert.lpr`**: Validação de inserção em lote para todos os dialetos suportados.

### Executando os testes via linha de comando:

```bash
# Compilar e executar a suíte de testes principal
fpc -B -FEtests -Futests -Fusrc tests/test_suite.lpr
./tests/test_suite

# Compilar e executar o teste de concorrência do pool
fpc -B -FEtests -Futests -Fusrc tests/test_pool.lpr
./tests/test_pool
```

---

## 📦 Compilação do Pacote Lazarus

O pacote Lazarus está localizado em `deltamodel_pkg.lpk` (nome diferenciado da unit `DeltaModel.pas` para evitar colisões no compilador):

```bash
# Compilar o pacote Lazarus via lazbuild
lazbuild --build-all deltamodel_pkg.lpk
```

---

## 📁 Exemplos Incluídos

- **`example/00-serialization`**: Demonstração de serialização, cópia profunda (`Clone`, `CopyObject`), listas e geração de Swagger Schema.
- **`example/01-database`**: Interface Lazarus com demonstração do ORM multi-SGDB, pré-visualização de SQL para os 6 dialetos e execução de CRUD em SQLite `:memory:`.
- **`example/02-validation`**: Testes completos do motor de validação (`TValidator`) para documentos brasileiros, e-mails e regras customizadas.
- **`example/03-connection-pool`**: Demonstração do pool de conexões thread-safe com RAII, callbacks e transações atômicas.
- **`example/04-bulk-insert`**: Benchmark comparativo de inserção em massa (`BulkInsert` com array e `TDeltaModelList`) vs inserção individual (`Save`), com preview de SQL para os 6 dialetos.

---

## 📄 Licença

Distribuído sob a licença MIT. Veja `LICENSE` para mais detalhes.

### BulkInsert com Firebird

`BulkInsert` executa INSERTs parametrizados por registro, reutilizando o dataset
dentro de cada lote e mantendo a transação de toda a operação. Evita parâmetros
DSQL inválidos dentro de `EXECUTE BLOCK`. Mantém validações e hooks de lote e
não preenche IDs gerados nos modelos. Se o chamador já abriu uma transação,
cabe a ele confirmar ou desfazer; caso contrário, uma falha desfaz todos os lotes.
Esta execução prioriza compatibilidade e atomicidade; não usa o protocolo nativo
de batch do Firebird.
