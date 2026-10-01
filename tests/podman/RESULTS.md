# Auto incremento: validação com Podman

Execução em 29/09/2026: **536 verificações distintas aprovadas, nenhuma falha**.
O runner repete as 270 verificações unitárias/regressões na etapa legada,
totalizando 806 asserções aprovadas na execução completa. Saída final: código 0.

| Suíte / banco | Versão real | Migrations / regressões | Auto incremento |
| --- | --- | ---: | ---: |
| Suíte existente | FPC 3.2.2 | 201 | — |
| Planejamento de migrations | FPC 3.2.2 | 29 | — |
| SQL unitário de auto incremento | FPC 3.2.2 | — | 40 |
| PostgreSQL | 15.6 | 38 | 18 |
| MySQL (servidor MySQL real) | 8.4.11 | 38 | 18 |
| Firebird moderno | 5.0.4 | 38 | 24 |
| Firebird legado | 2.5.9 | 38 | 26 |
| SQLite | 3.40.1 | 19 | 9 |

## Achados e alterações

Antes, o builder sempre gerava `IDENTITY` para Firebird, sem detectar versão.
Agora `PrepareDB` consulta `ENGINE_VERSION`: 2.5 recebe generator + trigger,
3+ recebe identity com generator interno. O catálogo dos servidores reais
confirmou os dois caminhos, tanto em `CREATE TABLE` quanto em `ALTER TABLE ADD`.
Firebird 3 e 4 foram cobertos por testes unitários de SQL, não por servidores reais.

A recuperação anterior de ID consultava o contador compartilhado do generator
e ocultava exceções; agora usa `INSERT ... RETURNING campo`. As integrações
verificam ID retornado, crescimento entre inserções, valores explícitos,
NULL no trigger legado e reexecução sem reinício de sequência nem nova migration.

No SQLite, adicionar PK auto incremento exige reconstrução da tabela e não foi
implementado. Os testes de adição usam tabelas vazias nos demais bancos.
A compatibilidade legada aqui validada é de auto incremento e migrations destes
modelos; não implica suporte geral a todos os tipos ou operações no Firebird 2.5.

## Reproduzir e evidências

Consulte [instruções do runner](README.md). Logs desta execução:
`/tmp/deltamodel-podman.AdvcrJ/tests.log`, `legacy-tests.log`, `images.txt`,
`build.log` e logs dos servidores. Os containers descartáveis foram removidos.
O container legado exigiu SIGKILL na limpeza após o prazo de parada; os testes
já haviam terminado com sucesso. Os logs de FK incluem rejeições esperadas.

IDs locais das imagens (digests completos também constam em `images.txt`):

- Runner: `7e9d2be157651fe24fd4301cdccdefcff70e6c1ea06c4893f06cf48843e4ee6f`
- PostgreSQL: `bacb8d141d0d811b9a424b11ee36c723aaccce05d81acbbe540d1b54113802ee`
- MySQL: `8d132912e9d3c985237567595ec2aeae04936044eb1f9336e8e4da2326238b84`
- Firebird 5: `c7aa0e38823d05ec16edbab768d0336ce96331769d4558aa6625ec0ea90f50d3`
- Firebird 2.5: `b3d6bed7feda05bc69a313add5e10a3d8a46bac4bd86a97336936489cd24b2ea`

---

## Histórico: testes reais das migrations com Podman

Execução local em 29/09/2026. Resultado final: **363 verificações aprovadas,
nenhuma falha**. Os erros de FK escritos nos logs são rejeições esperadas pelos
testes, não falhas da suíte.

| Suíte / banco | Versão verificada | Verificações aprovadas |
| --- | --- | ---: |
| Suíte existente | Free Pascal 3.2.2 | 201 |
| Planejamento e regressões de migrations | Free Pascal 3.2.2 | 29 |
| PostgreSQL | 15.6 | 38 |
| MariaDB | 11.4.13 | 38 |
| Firebird | 5.0.4 | 38 |
| SQLite | 3.40.1 | 19 |

## Cenários

- Criação do filho registrado antes do pai, usando `TableName` personalizado.
- Duas FKs distintas para a mesma tabela.
- Campos ausentes em tabelas existentes criados antes de suas FKs.
- FKs novas em colunas já existentes.
- Reconhecimento de FK com nome antigo, sem duplicá-la.
- Referências circulares.
- Índices criados antes das FKs e reconhecidos na reexecução.
- Inserção válida aceita e inserção órfã rejeitada pelo banco.
- Dados órfãos existentes impedem criar a FK e não registram uma versão concluída.
- Reexecução sem DDL nem incremento da versão.

No SQLite, adicionar FKs a tabelas existentes continua exigindo reconstrução
manual. Os casos correspondentes verificam que o migrador recusa a operação
antes de alterar a estrutura ou os dados. Não representam suporte à reconstrução
automática. A fiscalização de FKs foi habilitada antes da conexão.

## Correções encontradas na execução

1. Índices existentes eram gerados novamente; no MariaDB isso falhava e no
   PostgreSQL criava uma versão desnecessária. Agora são consultados no catálogo.
2. Firebird não prepara a gravação na tabela de histórico recém-criada sem
   confirmar os metadados. A infraestrutura é confirmada antes do DDL dos modelos.
3. Firebird rejeitava o `RESTRICT` das opções padrão de FK. O gerador agora usa
   `NO ACTION` nesse dialeto.

Os testes de MariaDB usam o conector SQLDB MySQL 5.7, carregando explicitamente
`libmariadb.so.3` e desativando a verificação da versão da biblioteca cliente.
Isso valida o servidor MariaDB real, sem exigir um conector chamado `MariaDB`.

## Reproduzir

A partir da raiz do DeltaModel:

```bash
./tests/podman/run.sh
```

O script não expõe portas no host e monta o código somente para leitura.
Os bancos são descartáveis; containers e seus volumes são removidos ao final.
As imagens ficam em cache. Os containers preexistentes da máquina não são usados.

Logs da execução aprovada: `/tmp/deltamodel-podman.YLpJSe/`:
`tests.log`, `images.txt`, `build.log` e logs dos três servidores.

Imagens efetivamente utilizadas (IDs locais):

- Runner Debian Bookworm: `7e9d2be157651fe24fd4301cdccdefcff70e6c1ea06c4893f06cf48843e4ee6f`
- PostgreSQL 15.6: `bacb8d141d0d811b9a424b11ee36c723aaccce05d81acbbe540d1b54113802ee`
- MariaDB 11.4: `417af2374091b2072ea177e9973abbe221cf7ec5f2623a617ec557dd6e640760`
- Firebird 5.0.4 Bookworm: `c7aa0e38823d05ec16edbab768d0336ce96331769d4558aa6625ec0ea90f50d3`

Configuração das imagens baseada nas instruções oficiais de
[Firebird](https://github.com/FirebirdSQL/firebird-docker) e
[MariaDB](https://hub.docker.com/_/mariadb/).
