<p align="right">
  <b>Language / Idioma:</b>
  <a href="README.md">English</a> |
  <a href="README.pt-BR.md">Português (Brasil)</a> |
  <a href="README.es-ES.md">Español</a>
</p>

# DeltaModel

[![Lazarus](https://img.shields.io/badge/Lazarus-2.2%2B-blue.svg)](https://www.lazarus-ide.org/)
[![FreePascal](https://img.shields.io/badge/FPC-3.2.2%2B-green.svg)](https://www.freepascal.org/)
[![Multi--Database](https://img.shields.io/badge/Databases-6%20SGDBs-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**DeltaModel** é um microframework leve, moderno e extensível para **Lazarus / Free Pascal (FPC)** que reúne em uma única biblioteca:

- 🧱 **Modelagem de Dados**: Campos fortemente tipados (`TDF*`) ou propriedades primitivas puras com Fluent Schema (`ConfigureSchema`).
- ✅ **Motor de Validação Fluente**: Regras declarativas no schema (`Min`, `Max`, `Range`, `MinLength`, `MaxLength`, `Positive`, `NotEmpty`, `Email`, `CPF`, `CNPJ`, `URL`) e validação customizada (`Validate`).
- 🔄 **Serialização JSON de Alta Performance**: Serializador zero-DOM `TDeltaFastJsonStreamer` e Desserializador lexical em streaming `TDeltaFastJsonParser`.
- 🌐 **Internacionalização (i18n)**: Mensagens e erros em múltiplos idiomas (`en-US`, `pt-BR`, `es-ES`), configurável programaticamente (`SetDeltaLocale`) ou via ambiente (`DELTAMODEL_LOCALE`).
- 📋 **OpenAPI / Swagger Schemas**: Geração automática de esquemas compatíveis com OpenAPI 3.1 para documentação de APIs.
- 🗄️ **Micro-ORM Multi-SGDB com Generics**:
  - DDL automatizado que inspeciona e cria/atualiza tabelas, colunas, chaves primárias, chaves estrangeiras, constraints (`UNIQUE`, `CHECK`) e índices de banco de dados.
  - Construtor fluente de consultas SQL tipadas `TDeltaQuery<T>` com Generics e coleções `TGDeltaModelList<T>`.
  - Eager Loading 1:N automático via `Include('RelationName')`.
  - HTTP Streaming Response (`StreamJsonTo`) com consumo constante de memória em lotes.
  - Escopo de transações atômico com auto-rollback RAII (`IDeltaTransaction`, `BeginTransaction`).
  - Operações CRUD inteligentes e hooks de ciclo de vida.

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
  finally
    PagList.Free;
  end;
end;
```

---

## ⚡ Alta Performance: Fluent Class Schema & Generics (Abordagem B)

No Free Pascal 3.2.x, propriedades do tipo `record` não podem ser `published` e não há suporte a Custom Attributes em tempo de execução via RTTI. O modelo legado utiliza classes wrappers (`TDF*`) com boxing de `Variant`, o que impõe um custo de alocação de ~6 a 8 objetos auxiliares por instância de modelo.

A **Abordagem B** resolve essa limitação combinando **propriedades primitivas nativas** (`Int64`, `Integer`, `Double`, `string`, `Boolean`) com uma configuração fluente estática de classe (`ConfigureSchema`):

```pascal
type
  TProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FCost: Double;
    FStock: Integer;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(Schema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Cost: Double read FCost write FCost;
    property Stock: Integer read FStock write FStock;
    property Active: Boolean read FActive write FActive;
  end;

class procedure TProduct.ConfigureSchema(Schema: TDeltaModelSchema);
begin
  Schema.Table('products');
  Schema.Field('Id').Column('id').AsPK.AutoInc;
  Schema.Field('Name').Column('name').Required.Size(150);
  Schema.Field('Price').Column('price').Required;
  Schema.Field('Cost').Column('cost').Nullable;
  Schema.Field('Stock').Column('stock').Required;
  Schema.Field('Active').Column('active').Required;
end;
```

### Vantagens Arquiteturais:
- **Zero Wrappers por Instância**: `TProduct.Create` aloca apenas o objeto principal, sem instanciar campos auxiliares.
- **Configuração Única em Cache**: `ConfigureSchema` é executado estaticamente apenas uma vez via `TRTTICache.GetModelMetadata`, garantindo custo zero de metadados em runtime.
- **Bitmask de Nulidade Ultraleve**: O controle de campos nulos utiliza um campo `FNullMask: UInt64` (8 bytes), permitindo testar e atribuir `null` em 1 ciclo de clock de CPU:
  ```pascal
  Product.SetNull('Cost');   // Marca o campo como nulo
  if Product.IsNull('Cost') then ... // Verifica nulidade sem Variant boxing
  Product.ClearNull('Cost'); // Desmarca nulidade
  ```
- **Consultas Fluentes e Tipadas (`TDeltaQuery<T>`)**:
  ```pascal
  uses DeltaModel.Generics;

  var
    Query: TDeltaQuery<TProduct>;
    List: TGDeltaModelList<TProduct>;
    P: TProduct;
  begin
    Query := TDeltaQuery<TProduct>.Create(Engine);
    try
      // Inserção direta
      Query.Insert(NewProduct);

      // Consulta fluente tipada
      List := Query.Where('price > :min')
                   .Param('min', 100.0)
                   .OrderBy('price DESC')
                   .All;
      try
        // Iteração idiomática direta sem typecasting!
        for P in List do
          WriteLn(P.Name, ' = R$ ', P.Price:0:2);
      finally
        List.Free;
      end;
    finally
      Query.Free;
    end;
  end;
  ```

### 📊 Resultados Reais de Benchmark (SQLite3 em Disco | -O3)

Benchmark executado em ambiente Linux x86_64 comparando o Modelo Legado (`TDF*`) vs Abordagem B (Primitivas Nativas + Fluent Schema):

| Métrica Avaliada | Volume (Regs) | Modelo Legado (`TDF*`) | Abordagem B (Nativa) | Ganho / Otimização |
| :--- | :---: | :---: | :---: | :---: |
| **Footprint de Memória RAM (Heap)** | 1.000 | 1.257 bytes/obj (1,20 MB) | 585 bytes/obj (0,56 MB) | **-53,4% de RAM** |
| **Footprint de Memória RAM (Heap)** | 10.000 | 1.257 bytes/obj (11,99 MB) | 585 bytes/obj (5,58 MB) | **-53,5% de RAM** |
| **Footprint de Memória RAM (Heap)** | 50.000 | 1.256 bytes/obj (59,92 MB) | 584 bytes/obj (27,88 MB) | **-53,5% de RAM (32 MB livres)** |
| **Tempo de Alocação de Objetos** | 50.000 | 257 ms | 73 ms | **3,5x mais rápido** |
| **Leitura SQL + Hydration ORM** | 1.000 | 8 ms (125.000 recs/s) | 4 ms (250.000 recs/s) | **2,0x mais rápido** |
| **Leitura SQL + Hydration ORM** | 10.000 | 83 ms (120.482 recs/s) | 44 ms (227.273 recs/s) | **1,9x mais rápido** |
| **Leitura SQL + Hydration ORM** | 50.000 | 418 ms (119.617 recs/s) | 217 ms (230.415 recs/s) | **1,9x mais rápido** |
| **Serialização JSON (FastStreamer)**| 50.000 | 245 ms (204.082 objs/s) | 230 ms (217.391 objs/s) | **23,6 MB/s sustentados** |
| **Desalocação de Memória (Free)** | 50.000 | 60 ms | 30 ms | **2,0x mais rápido** |
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

Gere esquemas compatíveis com OpenAPI 3.0 / 3.1 para documentação de rotas e APIs. O método original `SwaggerSchema` foi depreciado em prol de métodos semânticos, tipados e integrados com `$ref`:

### 1. Métodos de Classe no Modelo (`TDeltaModel`)

```pascal
var
  ObjSchema: string;
  ArraySchema: string;
  PaginatedSchema: string;
begin
  // 1. Esquema completo para um único objeto: {"type": "object", "properties": { ... }}
  ObjSchema := TPerson.SwaggerSchemaObject(AddExamples := True);

  // 2. Esquema completo para array de objetos: {"type": "array", "items": {"type": "object", ...}}
  ArraySchema := TPerson.SwaggerSchemaArray(AddExamples := True);

  // 3. Esquema completo para lista paginada: {"type": "object", "properties": {"items": ..., "page": ..., ...}}
  PaginatedSchema := TPerson.SwaggerSchemaPaginated(AddExamples := True);

  // NOTA: O método legado SwaggerSchema(IsArray, IsPaginated) está depreciado (deprecated)
end;
```

### 2. Registro Canônico em `components.schemas` e Resolução com `$ref`

Para documentações limpas e em conformidade com o OpenAPI 3.1, os modelos do DeltaModel suportam registro automático no dicionário `components.schemas` do Swagger:

```pascal
var
  RefObjSchema: string;
  RefArraySchema: string;
  RefPaginatedSchema: string;
begin
  // Registra o modelo em components.schemas e retorna:
  // {"$ref": "#/components/schemas/Person"}
  RefObjSchema := TPerson.SwaggerSchemaRefObject;

  // Retorna array com referência:
  // {"type": "array", "items": {"$ref": "#/components/schemas/Person"}}
  RefArraySchema := TPerson.SwaggerSchemaRefArray;

  // Retorna envelope paginado referenciando o modelo em items:
  RefPaginatedSchema := TPerson.SwaggerSchemaRefPaginated;
end;
```

A integração é desacoplada através do callback global configurado na inicialização de frameworks (como o **Zenith**):

```pascal
// Callback global acionado automaticamente ao chamar RegisterSwaggerSchema
SetDeltaModelRegisterSchemaProc(@ZenithRegisterModelProc);
```

### 3. Métodos de Instância nas Listas

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

## ⚡ Arquitetura de Alta Performance

O DeltaModel foi projetado com uma arquitetura de execução rápida para ambientes de alta volumetria e multithreading, eliminando os tradicionais gargalos de frameworks Pascal:

### 1. `TDeltaFastJsonStreamer`: Streaming JSON Direto (Zero DOM Allocation)
A serialização padrão via bibliotecas tradicionais (`fpjson`) instancia centenas de objetos DOM (`TJSONObject`, `TJSONString`, `TJSONFloat`) para cada linha de uma consulta, gerando alto overhead de alocação de heap e coleta de memória.

O `TDeltaFastJsonStreamer` serializa instâncias e coleções de `TDeltaModel` diretamente para um buffer contíguo de `RawByteString`:
- **Zero Alocação de Nós DOM**: Não cria nós intermediários em memória durante `Model.ToJson`, `List.ToJson` ou `PaginatedList.ToJson`.
- **Formatação de Floats em Ponto Fixo (`ffFixed`)**: Resolve a limitação histórica do `fpjson`, que serializa números decimais com expoentes científicos (`1.2E+05` ou `1.0E-04`). O streamer formata floats estritamente em ponto fixo (`FloatToStrF` com `ffFixed`), sem notação `E+`/`E-`, eliminando falhas de parsing em clientes Web, Mobile e integrações contábeis/fiscais.
- **Escape Otimizado de Strings**: Sanitização nativa de caracteres de controle e aspas em alta velocidade.

```pascal
uses DeltaModel.JsonStreamer;

var
  JsonBytes: RawByteString;
begin
  // Serialização direta sem instanciar TJSONObject
  JsonBytes := TDeltaFastJsonStreamer.ModelToJson(Person);
  JsonBytes := TDeltaFastJsonStreamer.ListToJson(PersonList);
  JsonBytes := TDeltaFastJsonStreamer.PaginatedListToJson(PaginatedList);
end;
```

### 2. `TColumnBindingMap`: Binding Direto ORM Sem Variant Boxing
Ao popular modelos a partir de um `TDataSet`, frameworks convencionais utilizam variantes intermediárias (`Field.Value`) e buscas de colunas por nome para cada registro.

O `TColumnBindingMap` elimina esse custo:
- **Mapeamento Pré-Alocado**: Ao executar `TQuery.All` ou `TQuery.First`, o mapa de colunas é construído uma única vez para o dataset ativo.
- **Acesso Tipado Direto**: Os valores são transferidos diretamente dos campos físicos do `TDataSet` para os campos tipados do `TDeltaModel` (`ADS.Fields[I].AsLargeInt`, `.AsString`, `.AsFloat`, `.AsBoolean`, etc.).
- **Zero Variant Boxing**: Elimina as conversões OLE/Variant em loops de dezenas de milhares de registros, multiplicando a taxa de vazão (throughput) do ORM.

### 3. `TRTTICache` & `TModelMetadata`: Cache Estático de RTTI por Classe
A reflexão em tempo de execução (`TypInfo.GetPropList`) é custosa quando executada por instância.

O `TRTTICache` é um componente estático thread-safe protegido por `TCriticalSection`:
- **Cache por `TClass`**: Inspeciona a classe do modelo uma única vez durante todo o ciclo de vida do processo.
- **`TModelMetadata`**: Mantém pré-processados o nome da tabela, nome da propriedade PK, nome da coluna PK (`PKFieldName`), tipos de dados mapeados e offsets de memória.
- **Busca $O(1)$ de Metadados**: Elimina chamadas repetitivas de RTTI em validações, DDLs e serializações.

### 4. Acesso Direto a Campos Tipados (Sem Variants)
Todos os campos do DeltaModel disponibilizam propriedades de leitura e escrita tipadas diretas:
```pascal
// Atribuição e leitura tipada direta:
Person.id.AsInt64 := 42;
Person.name.AsString := 'Carlos Eduardo';
Person.salary.AsFloat := 7500.50;
Person.active.AsBoolean := True;

// Verificação de nulabilidade atômica:
if not Person.salary.IsNull then
  ProcessarSalario(Person.salary.AsFloat);
```

---

## 🧪 Suíte de Testes Automatizados

O DeltaModel possui uma cobertura abrangente de testes automatizados unitários e de integração, garantindo alta estabilidade, precisão de tipos e performance:

- **`tests/test_suite.lpr`**: Suíte consolidada com **344 testes automatizados (100% aprovados)** cobrindo:
  - **Connection URL Parser** (SQLite memória/arquivo/relativo, PostgreSQL, MySQL, Firebird).
  - **Motor de Validação** (CPF e CNPJ numérico/alfanumérico com/sem máscara e dígitos inválidos, E-mail, UTF-8 multibyte, Between, GreaterThan, etc.).
  - **Serialização & Precisão Numérica** (JSON com escape de aspas, UTF-8, integridade de `Int64` sem truncamento, CSV tipado, `Clone`).
  - **Listas e Paginação** (distinção estrita entre `TDeltaModelList` gerando array puro de objetos e `TDeltaModelPaginatedList` gerando envelopes com metadados, suporte a genéricas `TGDeltaModelList<T>` e `TGDeltaModelPaginatedList<T>`).
  - **Geração de OpenAPI 3.0 / 3.1 & Schemas** (`SwaggerSchemaObject`, `SwaggerSchemaArray`, `SwaggerSchemaPaginated`, `$ref` canônico e compatibilidade retroativa).
  - **Geração de DDL Multi-Dialeto** (SQLite, PostgreSQL, Firebird, Oracle, MSSQL).
  - **Ciclo de Vida & Hooks** (`BeforeInsert`, `AfterInsert`, `BeforeUpdate`, `AfterUpdate`, `BeforeSave`, `AfterSave`, `BeforeDelete`, `AfterDelete` e tratamento de exceções).
  - **ORM CRUD, Query Builder & Transações** (`Where`, `WhereBetween`, `WhereIn`, `Limit`, `Offset`, `AsJsonString`, rollback automático e `InTransaction`).
  - **Constraints (`UNIQUE`, `CHECK`) & Índices de Banco de Dados** (restrições a nível de campo e tabela, geração de índices secundários e únicos, sanitização de identificadores para Oracle e validação de violação de integridade em runtime com SQLite).
  - **Relacionamento 1:1 (`TDFHasOne`)** (metadados virtuais sem coluna física em DDL, serialização aninhada/null, desserialização com instanciação dinâmica, cópia profunda em `Clone` e esquema Swagger com `type: 'object'`).
  - **TRTTICache & TModelMetadata** (inspeção única de RTTI por classe, integridade de cache hit, busca rápida de propriedades e campos).
  - **TColumnBindingMap & Direct Typed ORM Binding** (vinculação de datasets, leitura de tipos sem boxing de Variant em `TQuery.All` e `FromDataSet`).
  - **TDeltaFastJsonStreamer & Floats em Ponto Fixo** (ausência absoluta de notação científica `E+`/`E-`, números de ponto flutuante grandes e pequenos, preservação de nulls e parsing válido).
  - **Propriedades Tipadas de Campos** (acesso sem Variant: `.AsInt64`, `.AsLargeInt`, `.AsInteger`, `.AsString`, `.AsText`, `.AsFloat`, `.AsBoolean`).
  - **Fluent Class Schema & Generics - Abordagem B** (configuração fluente de metadados, bitmask ultraleve de nulidade, `TDeltaQuery<T>` com CRUD tipado, `for .. in` tipado, BulkInsert tipado e serialização direta de tipos primitivos sem Variant boxing).
- **`tests/test_pool.lpr`**: Testes de concorrência multithread do Connection Pool (10 threads simultâneas vs pool de 4 conexões, timeouts, sanitização RAII e métricas).
- **`tests/test_bulk_insert.lpr`**: Validação de inserção em lote para todos os dialetos suportados.
- **`tests/benchmark_real.pas`**: Benchmark comparativo com dados reais em SQLite para 1.000, 10.000 e 50.000 registros medindo footprint de memória Heap, velocidade de leitura SQL, BulkInsert e serialização JSON.

### Executando os testes via linha de comando:

```bash
# Compilar e executar a suíte de testes principal
fpc -B -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/test_suite.lpr
/tmp/test_suite

# Compilar e executar o teste de concorrência do pool
fpc -B -FE/tmp -FU/tmp -Fusrc tests/test_pool.lpr
/tmp/test_pool

# Compilar e executar os benchmarks reais
fpc -O3 -FE/tmp -FU/tmp -Fusrc -Fi../../src tests/benchmark_real.pas
/tmp/benchmark_real
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
- **`example/05-fluent-generics`**: Demonstração completa da Abordagem B com propriedades primitivas nativas, `ConfigureSchema` fluente, `TDeltaQuery<T>`, iteração tipada `for in` e serialização JSON ultrarrápida.

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
