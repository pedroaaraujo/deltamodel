# Testes de bancos com Podman

Execute a partir da raiz do DeltaModel:

```sh
./tests/podman/run.sh
```

Pré-requisitos: Podman funcional (rootless é suficiente), acesso aos registries
para baixar imagens e espaço para o runner Free Pascal e os bancos. Não é
necessário instalar compilador ou servidores no host.

O runner compila o código atual montado somente para leitura, com executáveis e
units em `/tmp` dentro do container. Os bancos são novos e descartáveis, sem
portas publicadas, sem acesso aos bancos preexistentes. O pod e seus containers
são removidos ao terminar. Imagens ficam em cache. Os logs e IDs das imagens
ficam no diretório `/tmp/deltamodel-podman.*` informado ao final.

| Banco | Imagem padrão | Variável para substituição |
| --- | --- | --- |
| PostgreSQL | `docker.io/library/postgres:15.6` | `POSTGRES_IMAGE` |
| MySQL | `docker.io/library/mysql:8.4` | `MYSQL_IMAGE` |
| Firebird moderno | `docker.io/firebirdsql/firebird:5.0.4-bookworm` | `FIREBIRD_IMAGE` |
| Firebird legado | `docker.io/jacobalberty/firebird:v2.5.9-sc` | `FIREBIRD_LEGACY_IMAGE` |
| SQLite | Biblioteca do Debian Bookworm no runner | Containerfile |

O MySQL é testado com o servidor MySQL real. O conector SQLDB MySQL 5.7 usa
`libmariadb.so.3`, desativando somente a verificação de versão da biblioteca
cliente no executável de testes. Isso não substitui o servidor por MariaDB.
As imagens substitutas precisam aceitar as mesmas variáveis e caminhos.

Após os quatro bancos, o Firebird moderno é encerrado e o 2.5 assume sua porta
interna no mesmo pod. O banco legado é novo. A imagem 2.5 é do repositório
arquivado [jacobalberty/firebird-docker](https://github.com/jacobalberty/firebird-docker);
é utilizada apenas para regressão de compatibilidade.

## Suítes

- `test_suite`: regressões existentes de modelos, validação, SQL e SQLite.
- `test_migrations`: planejamento de migrations e regressões SQLite.
- `test_autoincrement`: testes unitários de SQL para SQLite, PostgreSQL, MySQL
  e Firebird 2/3/4/5, parsing da versão, nomes longos e colunas existentes.
- `test_migrations_integration`: migrations e constraints executadas nos bancos.
- `test_autoincrement_integration`: criação e adição de campo auto incremento,
  inserções pelo ORM, retorno de ID, valores explícitos e idempotência. Inspeciona
  os catálogos Firebird para distinguir generator/trigger de identity interna.
  SQLite testa criação; adicionar uma PK auto incremento exige reconstrução.

Falhas de compilação, conexão ou asserções produzem saída diferente de zero.
Não se deve interpretar indisponibilidade de banco como teste aprovado.

## Execução local

Com Free Pascal e bibliotecas clientes instalados, na raiz do DeltaModel:

```sh
mkdir -p /tmp/deltamodel-tests
fpc -B -FE/tmp/deltamodel-tests -FU/tmp/deltamodel-tests -Fusrc tests/test_autoincrement.lpr
/tmp/deltamodel-tests/test_autoincrement
fpc -B -FE/tmp/deltamodel-tests -FU/tmp/deltamodel-tests -Fusrc tests/test_autoincrement_integration.lpr
DELTAMODEL_TEST_URL='sqlite:///:memory:' /tmp/deltamodel-tests/test_autoincrement_integration
```

Para outros bancos, `DELTAMODEL_TEST_URL` deve apontar **exclusivamente para uma
base descartável e vazia**. Os testes criam tabelas e dados com nomes fixos.
Os executáveis de integração aguardam conexão por até 30 segundos.
