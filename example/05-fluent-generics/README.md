# Exemplo 05: Fluent Class Schema & Generics (Abordagem B)

Este exemplo demonstra a **Abordagem B** implementada no **DeltaModel ORM**:
- Uso de **propriedades primitivas nativas** (`Int64`, `string`, `Double`, `Boolean`) na classe do modelo em vez de wrappers auxiliares (`TDF*`).
- Configuração de metadados de classe fluente e estática (`ConfigureSchema`), que roda apenas **uma única vez** via cache RTTI, eliminando o overhead de alocação por instância.
- **Bitmask ultraleve de nulidade** (`FNullMask: UInt64`), permitindo testar e atribuir `null` em 1 ciclo de clock de CPU com zero boxing de Variant.
- Consulta tipada via `TDeltaQuery<T>` com sintaxe fluente (`Where`, `Param`, `OrderBy`, `Limit`, `FindById`, `All`).
- Coleção tipada `TGDeltaModelList<T>` com suporte ao laço idiomático `for Item in List do`.
- Inserção em lotes (`BulkInsert`) de alta performance com montagem nativa de parâmetros.
- Serialização JSON ultrarrápida com `TDeltaFastJsonStreamer`.

## Como compilar e executar

```bash
fpc -FE/tmp -FU/tmp -Fu../../src -Fi../../src fluent_generics_demo.lpr
/tmp/fluent_generics_demo
```
