# DeltaModel

[![Lazarus](https://img.shields.io/badge/Lazarus-2.2%2B-blue.svg)](https://www.lazarus-ide.org/)
[![FreePascal](https://img.shields.io/badge/FPC-3.2.2%2B-green.svg)](https://www.freepascal.org/)
[![Multi--Database](https://img.shields.io/badge/Databases-6%20SGDBs-orange.svg)](https://github.com/pedroaaraujo/deltamodel)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**DeltaModel** é um microframework leve, moderno e extensível para **Lazarus / Free Pascal (FPC)** que reúne em uma única biblioteca:

- 🧱 **Modelagem de Dados**: Campos fortemente tipados com controle explícito de nulabilidade (`Null` vs `Required`).
- ✅ **Motor de Validação Fluente**: Validação de CPF, CNPJ, E-mail, URL, Ranges, Regex e regras de negócio personalizadas.
- 🔄 **Serialização JSON**: Conversão bidirecional entre Objetos/Listas e JSON (`ToJson`, `FromJson`), com suporte a objetos aninhados.
- 📋 **OpenAPI / Swagger Schemas**: Geração automática de esquemas compatíveis com OpenAPI 3.0 para documentação de APIs.
- 🗄️ **Micro-ORM Multi-SGDB**:
  - DDL automatizado que inspeciona e cria/atualiza tabelas, colunas, chaves primárias e estrangeiras.
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
| **Firebird** | `ddFirebird` | `IDENTITY` | `ROWS m TO n` | `RETURNING *` |
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

---

## 🗄️ DDL Automatizado (`TDeltaORMSchema`)

O `TDeltaORMSchema` inspeciona a estrutura das classes e cria ou atualiza as tabelas no SGDB conectado:

```pascal
var
  Schema: TDeltaORMSchema;
begin
  Schema := TDeltaORMSchema.Create(Con);
  try
    Schema.RegisterModel(TPerson.Create);
    // Schema.RegisterModel(TOrder.Create);

    // True para executar diretamente no banco conectado
    Schema.PrepareDB(True);

    // Você também pode inspecionar o SQL gerado:
    // WriteLn(Schema.SQL.Text);
  finally
    Schema.Free;
  end;
end;
```

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

```pascal
var
  Person: TPerson;
  JsonStr: string;
  List: TDeltaModelList;
begin
  Person := TPerson.Create;
  try
    Person.name.Value := 'Ana Beatriz';
    Person.email.Value := 'ana@empresa.com';

    // Objeto -> JSON
    JsonStr := Person.ToJson;

    // JSON -> Objeto
    Person.FromJson('{"name": "Beatriz Lima", "email": "beatriz@empresa.com"}');
  finally
    Person.Free;
  end;

  // Coleções com TDeltaModelList
  List := TDeltaModelList.Create;
  try
    List.SetDeltaModelClass(TPerson);
    List.FromJson('[{"name": "Ana"}, {"name": "Carlos"}]');

    // List -> JSON
    JsonStr := List.ToJson;
  finally
    List.Free;
  end;
end;
```

---

## 📋 Geração de Esquemas OpenAPI / Swagger

Gere esquemas OpenAPI 3.0 diretamente de qualquer classe `TDeltaModel` ou coleção:

```pascal
var
  SingleSchema: string;
  ArraySchema: string;
begin
  // Esquema para um único objeto
  SingleSchema := TPerson.SwaggerSchema();

  // Esquema para array de objetos
  ArraySchema := TPerson.SwaggerSchema(True);
end;
```

---

## 📁 Exemplos Incluídos

- **`example/00-serialization`**: Demonstração de serialização, cópia profunda (`Clone`, `CopyObject`), listas e geração de Swagger Schema.
- **`example/01-database`**: Interface Lazarus com demonstração do ORM multi-SGDB, pré-visualização de SQL para os 6 dialetos e execução de CRUD em SQLite `:memory:`.
- **`example/02-validation`**: Testes completos do motor de validação (`TValidator`) para documentos brasileiros, e-mails e regras customizadas.

---

## 📄 Licença

Distribuído sob a licença MIT. Veja `LICENSE` para mais detalhes.
