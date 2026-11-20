# Exemplo 06 — Schema Flyweight & Cursor Reusável (Zero-Alloc Loop)

Este projeto demonstra a solução implementada no **DeltaModel ORM** para consultas em tabelas com milhares ou milhões de registros, eliminando o consumo exponencial de memória do Garbage Collector / Heap do Free Pascal.

---

## 🚀 Problema Resolvido

No modelo tradicional de ORM (ex: `Query.All`), uma consulta de 5.000 municípios gerava mais de **50.000 objetos no Heap** (cada linha instanciava o modelo, mais os objetos `TDeltaField` de cada propriedade, nós de lista e bindings).

Com as inovações introduzidas:
1. **Schema Flyweight (`TMunicipio.Schema`)**:
   - Metadados (`FieldName`, `Size`, `IsRequired`, `DBOptions`) são desacoplados da instância de linha.
   - Residem na classe e existem **exatamente 1 vez na memória** para todo o ciclo de vida da aplicação.
2. **Cursor Reusável (Zero-Alloc Loop)**:
   - Uma **única instância** do modelo é criada no início da iteração e reaproveitada a cada linha percorrida pelo dataset.
   - 5.000 linhas = **EXATAMENTE 1 OBJETO no Heap**!

---

## 🛠 Padrões de Uso Suportados

### 1. Padrão Canônico Pascal (`while Query.Next do`)
```pascal
Query := TDeltaQuery<TMunicipio>.Create(Engine);
try
  Query.Where('uf = :uf').Open;
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

### 2. Suporte a Enumerador Nativo (`for M in Query.Cursor do`)
```pascal
for M in Query.Cursor do
begin
  // M reutiliza a mesma instância a cada iteração com zero alocações
  WriteLn(M.Nome, ' - ', M.Uf);
end;
```

### 3. Padrão Callback Funcional (`Query.Each`)
```pascal
Query.Each(@ProcessarItem);
```

### 4. Streaming Direto para HTTP/Socket (`Query.StreamJsonTo`)
Transmite a resposta JSON diretamente para um `TStream` (como a resposta HTTP do Zenith Framework) sem criar listas intermediárias ou lotes de objetos na memória:
```pascal
LinhasEnviadas := Query.StreamJsonTo(Context.Response.OutputStream);
```

---

## 📦 Compilar e Executar

```bash
cd example/06-reusable-cursor
fpc -MDelphi -Fu../../src reusable_cursor_demo.lpr
./reusable_cursor_demo
```
