# Orientações para agentes — DeltaModel

## Estrutura e preservação

Este diretório é um submódulo Git do Zenith. Inspecione `git status --short`
na raiz e aqui antes de editar. Preserve alterações locais e arquivos de testes
preexistentes. Não atualize o ponteiro do submódulo nem faça commits sem pedido.
Código em Free Pascal, modo ObjFPC; fontes em `src`, programas de teste em `tests`.

## DDL e auto incremento

- `DeltaModel.ORM.DDL` gera SQL puro; não consulta conexões nem usa estado global.
- `DeltaModel.ORM.Schema.PrepareDB` consulta a versão do servidor Firebird e passa
  explicitamente `FirebirdMajor` ao builder, inclusive para planejamento sem persistir.
- Firebird 2.5 usa generator + trigger; 3+ usa identity com generator interno.
  Não confundir versão do servidor, biblioteca cliente e `DatabaseVersion`
  (que representa o histórico de migrations).
- Preserve cada trigger como uma única entrada de `Schema.SQL`; não divida por
  ponto e vírgula, não envie `SET TERM` pela API SQLDB.
- Nunca recrie ou reinicie generators de colunas já existentes em `PrepareDB`.
- Recuperação de IDs Firebird deve usar `INSERT ... RETURNING` com nomes de
  colunas explícitos. Ler `GEN_ID(..., 0)` após inserir causa corrida entre conexões.
- Compatibilidade de auto incremento não implica suporte a todos os tipos de
  Firebird 2.5: por exemplo, esse servidor não possui o tipo SQL `BOOLEAN`.

## Validação e documentação

Compile com `-FE` e `-FU` apontando a `/tmp`, sem alterar binários rastreados.
Para mudanças de DDL/DML, rode os testes unitários e a matriz Podman descrita
em `tests/podman/README.md`. Use somente bases descartáveis.
Diferencie testes unitários (SQL gerado) de integração (servidores reais).
Registre versões, totais e limitações observadas em `tests/podman/RESULTS.md`;
não declare versões não executadas como verificadas. Atualize o README quando
mudar comportamento público. Evite ampliar a mudança a subsistemas sem relação.

---

## 🏛️ Arquitetura e Funcionamento do DeltaModel

O DeltaModel é um microframework moderno para Lazarus / Free Pascal projetado com alta coesão, baixo acoplamento e suporte a 6 dialetos de bancos relacionais (PostgreSQL, MySQL, SQLite, Firebird, MSSQL e Oracle).

### 1. Organização das Units

| Unit | Responsabilidade |
| :--- | :--- |
| `DeltaModel.pas` | Classe base `TDeltaModel`, `TDeltaModelList`, `TGDeltaModelList`, `TDeltaModelPaginatedList`, `TGDeltaModelPaginatedList` e hooks. |
| `DeltaModel.Fields.pas` | Tipos especializados `TDeltaField` (`TDFInt*`, `TDFString*`, `TDFCurrency*`, `TDFHasOne`, `TDFHasMany`, etc.). |
| `DeltaModel.List.pas` | Contrato abstrato `TCustomDeltaModelList` para listas interoperáveis. |
| `DeltaSerialization.pas` | Mecanismo de reflexão RTTI para serialização/desserialização JSON e CSV. |
| `DeltaAPISchema.pas` | Gerador de schemas OpenAPI 3.0 / Swagger. |
| `DeltaValidator.pas` | Motor de validação fluente (`TValidator`) com suporte a documentos e regras brasileiras. |
| `DeltaModel.ORM.Connection.pas` | Engine central de conexão e execução SQL (`TDeltaORMEngine`). |
| `DeltaModel.ORM.Pool.pas` | Pool de conexões multithread com suporte a empréstimo RAII (`IDeltaPooledEngine`). |
| `DeltaModel.ORM.DDL.pas` | Geração de scripts DDL agnósticos para tabelas, colunas, constraints e índices. |
| `DeltaModel.ORM.DML.pas` | Query builder fluente (`TQuery`) com operadores tipados e paginação nativa. |
| `DeltaModel.ORM.Schema.pas` | Migrador e planejador de esquema de banco de dados (`TDeltaORMSchema`). |

---

### 2. Definição de Modelos e Campos

- **Herança**: Modelos devem herdar de `TDeltaModel`.
- **Propriedades publicadas**: Todo campo persistível ou serializável deve ser publicado (`published`) como propriedade cujo tipo herde de `TDeltaField`.
- **Nulabilidade Estrita**:
  - `TDF*Null`: Campos opcionais (ex: `TDFIntNull`, `TDFStringNull`). Aceitam e serializam `null`.
  - `TDF*Required`: Campos obrigatórios (ex: `TDFIntRequired`, `TDFStringRequired`). Validados automaticamente em `Validate`.
- **Chaves Primárias e Opções de Banco (`DBOptions`)**:
  - `dboPrimaryKey`: Define o campo como chave primária.
  - `dboAutoInc`: Ativa auto-incremento (gerado pelo banco de dados).
  - `dboInsert`, `dboUpdate`: Controlam se o campo participa de instruções INSERT/UPDATE.
- **Campos Virtuais / Relacionamentos**:
  - `TDFHasOne`: Relacionamento 1:1. Não gera coluna física no banco de dados. Serializa objeto aninhado ou `null`. Ao desserializar, instancia o modelo relacionado automaticamente.
- **Configurações em `AfterConstruction` e `Configure`**:
  - Em `AfterConstruction`: definir `TableName`, tamanhos (`Size`), `DBOptions`, `IsUnique` e `IsIndexed`.
  - Em `Configure`: definir constraints compostas (`AddUniqueConstraint`, `AddCheckConstraint`) e índices multi-coluna (`AddIndex`, `AddUniqueIndex`).

---

### 3. Listas e Serialização JSON

Existem duas categorias fundamentais de listas:

#### A. Listas Puras de Objetos (`TDeltaModelList` e `TGDeltaModelList<T>`)
- Ao chamar `ToJson`, produzem **exclusivamente um array JSON de objetos** (`[ {...}, {...} ]`).
- Ideais para listas planas, payloads de inserção/atualização e respostas diretas sem paginação.
- `TGDeltaModelList<T>` atribui automaticamente o tipo de modelo `T` em seu construtor e disponibiliza itens tipados via `Item[Index]`, `First` e `Last`.

#### B. Listas Paginadas (`TDeltaModelPaginatedList` e `TGDeltaModelPaginatedList<T>`)
- Ao chamar `ToJson`, produzem o envelope de paginação canônico:
  ```json
  {
    "items": [ {...}, {...} ],
    "page": 1,
    "page_size": 20,
    "total_records": 100,
    "total_pages": 5
  }
  ```
- Ideais para endpoints REST paginados com metadados de navegação.

#### C. Desserialização Resiliente (`FromJson`)
- Todas as listas aceitam `FromJson` tanto recebendo um array direto `[ {...} ]` quanto recebendo um payload paginado `{ "items": [ {...} ], ... }`.

---

### 4. Geração de Esquemas OpenAPI / Swagger

O método original `SwaggerSchema(IsArray, IsPaginated)` está **depreciado** (`deprecated`).
Agentes devem utilizar ou gerar código utilizando os novos métodos explícitos:

1. **Objeto Único**:
   `TModel.SwaggerSchemaObject(AddExamples: Boolean = False): string;`
2. **Array de Objetos**:
   `TModel.SwaggerSchemaArray(AddExamples: Boolean = False): string;`
   Nas listas: `List.SwaggerSchemaArray(AddExamples: Boolean = False): TJSONObject;`
3. **Lista Paginada**:
   `TModel.SwaggerSchemaPaginated(AddExamples: Boolean = False): string;`
   Nas listas paginadas: `PagList.SwaggerSchemaPaginated(AddExamples: Boolean = False): TJSONObject;`

---

### 5. Boas Práticas para Agentes

1. **Preservação de Binários**: Sempre compile testes apontando a `/tmp`:
   ```bash
   fpc -FE/tmp -FU/tmp -Fu./src -Fisrc tests/test_suite.lpr
   ```
2. **Não deixar lixo no repositório**: Nunca comite nem crie arquivos `.ppu`, `.o`, `.a` ou binários executáveis na raiz ou em subpastas de código.
3. **Validação Contínua**: Ao alterar `DeltaModel.pas`, `DeltaSerialization.pas` ou ORM, execute `/tmp/test_suite` e garanta que 100% dos testes permaneçam aprovados.
4. **Respeito aos Dialetos de Banco**: Nunca assuma sintaxe específica de um único SGBD fora do respectivo driver ou builder de dialeto.
