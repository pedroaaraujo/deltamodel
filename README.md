# DeltaModel

**DeltaModel** é um microframework leve e extensível para **Lazarus / FreePascal (FPC)** que oferece:
- Modelagem de dados com campos tipados e controle de nulabilidade
- Validação fluente (CPF, CNPJ, E-mail, URL, Ranges, Regex)
- Serialização/Desserialização JSON bidirecional com suporte a objetos aninhados e listas
- Geração automática de esquemas OpenAPI / Swagger
- **Micro-ORM Multi-SGDB** com DDL automatizado, construtor de SQL fluente e operações CRUD inteligentes

---

## 🚀 SGDBs Suportados

O módulo ORM do DeltaModel oferece compatibilidade com os principais sistemas de banco de dados do mercado:

| SGDB | Dialeto | Auto-Incremento | Sintaxe Paginação | Sintaxe Returning |
| :--- | :--- | :--- | :--- | :--- |
| **PostgreSQL** | `ddPostgreSQL` | `SERIAL / BIGSERIAL` | `LIMIT n OFFSET m` | `RETURNING *` |
| **MySQL / MariaDB** | `ddMySQL` | `AUTO_INCREMENT` | `LIMIT n OFFSET m` | Standard INSERT |
| **SQLite3** | `ddSQLite` | `AUTOINCREMENT` | `LIMIT n OFFSET m` | `RETURNING *` |
| **Firebird** | `ddFirebird` | `IDENTITY` | `ROWS m TO n` | `RETURNING *` |
| **SQL Server (MSSQL)** | `ddMSSQL` | `IDENTITY(1,1)` | `OFFSET-FETCH` | `OUTPUT INSERTED.*` |
| **Oracle Database** | `ddOracle` | `IDENTITY` | `OFFSET-FETCH` | Standard INSERT |

---

## 📦 Conexão por URL

O engine de banco de dados utiliza URLs simples e padronizadas para inicializar qualquer conector:

```pascal
uses DeltaModel.ORM.Connection;

var
  Con: TDeltaORMEngine;
begin
  // SQLite em memória
  Con := TDeltaORMEngine.Create('sqlite://:memory:');

  // SQLite em arquivo
  Con := TDeltaORMEngine.Create('sqlite:///caminho/meubanco.db');

  // PostgreSQL
  Con := TDeltaORMEngine.Create('postgres://usuario:senha@localhost:5432/meubanco?charset=UTF8');

  // MySQL / MariaDB
  Con := TDeltaORMEngine.Create('mysql://root:senha@127.0.0.1:3306/meubanco');

  // Firebird
  Con := TDeltaORMEngine.Create('firebird://sysdba:masterkey@localhost:3050/caminho/banco.fdb');

  // SQL Server
  Con := TDeltaORMEngine.Create('mssql://sa:senha@127.0.0.1:1433/meubanco');

  // Oracle
  Con := TDeltaORMEngine.Create('oracle://system:senha@localhost:1521/XE');
```

---

## 🛠️ Exemplo de Modelo

```pascal
type
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
  end;

procedure TPerson.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.name.Size := 120;
end;
```

---

## 🔄 Operações ORM (CRUD)

### 1. Inserção / Atualização Inteligente (`Save`)
O método `Save` detecta automaticamente se a Chave Primária possui valor:
- Se a PK estiver vazia ou zero, executa `INSERT`.
- Se a PK estiver preenchida, executa `UPDATE`.

```pascal
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    Person.name.Value := 'Carlos Eduardo';
    Person.email.Value := 'carlos@empresa.com';
    Person.salary.Value := 7500.00;
    Person.active.Value := True;

    // Faz INSERT automaticamente
    Con.Save(Person);

    // Altera e salva de novo (faz UPDATE automaticamente)
    Person.salary.Value := 8200.00;
    Con.Save(Person);
  finally
    Person.Free;
  end;
end;
```

### 2. Consultas Fluentes com Paginação (`TQuery`)
```pascal
var
  List: TDeltaModelList;
  Person: TPerson;
  Total: Int64;
begin
  // Contagem
  Total := Con.Count(TPerson, 'active = 1');

  // Busca por ID
  Person := Con.Find(TPerson, 42) as TPerson;

  // Consulta paginada (Página 1 com 10 registros)
  List := Con.Query(TPerson)
    .Where('salary >= 5000')
    .OrderBy('name ASC')
    .Page(1, 10)
    .All;
  try
    // Itera resultados
  finally
    List.Free;
  end;
end;
```

### 3. Criação Automática de DDL (`TDeltaORMSchema`)
Cria ou atualiza tabelas e constraints de acordo com o dialeto do banco conectado:

```pascal
var
  Schema: TDeltaORMSchema;
begin
  Schema := TDeltaORMSchema.Create(Con);
  try
    Schema.RegisterModel(TPerson.Create);
    Schema.PrepareDB(True); // Persiste a estrutura no banco
  finally
    Schema.Free;
  end;
end;
```

---

## 📁 Exemplos Incluídos

- **`example/00-serialization`**: Serialização, desserialização e Swagger de modelos simples e aninhados.
- **`example/01-database`**: Demonstração do ORM multi-SGDB com pré-visualização de DDL/DML para os 6 bancos e execução real de CRUD via SQLite `:memory:`.
- **`example/02-validation`**: Validação de CPF, CNPJ, E-mail, URL e listas aninhadas.

---

## 📄 Licença

Distribuído sob licença MIT. Consulte o repositório para detalhes.
