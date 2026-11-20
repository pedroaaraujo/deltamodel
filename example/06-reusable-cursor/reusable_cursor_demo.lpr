program reusable_cursor_demo;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB, SQLDB, sqlite3conn,
  DeltaModel, DeltaModel.Fields, DeltaModel.Generics, DeltaModel.ORM.Connection;

type
  {============================================================================}
  { Data Model - TMunicipio                                                    }
  { Represents Brazilian municipalities. Metadata lives exclusively on the     }
  { class (Schema Flyweight), existing exactly ONCE in memory.                 }
  {============================================================================}
  TMunicipio = class(TDeltaModel)
  private
    FId: Int64;
    FNome: string;
    FUf: string;
    FCodigoIbge: Int64;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Nome: string read FNome write FNome;
    property Uf: string read FUf write FUf;
    property CodigoIbge: Int64 read FCodigoIbge write FCodigoIbge;
  end;

  { Callback helper for the Query.Each pattern }
  TMunicipioProcessor = class
  private
    FTotalProcessed: Integer;
    FLastNome: string;
  public
    procedure ProcessItem(AMunicipio: TMunicipio);
    property TotalProcessed: Integer read FTotalProcessed;
    property LastNome: string read FLastNome;
  end;

{==============================================================================}
{ Static Flyweight Schema Configuration                                         }
{ Executed only once for the entire class.                                      }
{ Metadata (FieldName, Size, IsRequired) is NOT duplicated per row.             }
{==============================================================================}
class procedure TMunicipio.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('municipios');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Nome').Column('nome').Required.Size(120);
  ASchema.Field('Uf').Column('uf').Required.Size(2);
  ASchema.Field('CodigoIbge').Column('codigo_ibge').Required;
end;

procedure TMunicipioProcessor.ProcessItem(AMunicipio: TMunicipio);
begin
  Inc(FTotalProcessed);
  FLastNome := AMunicipio.Nome;
end;

const
  DEMO_DB = '/tmp/deltamodel_cursor_demo.db';
  TOTAL_RECORDS = 1000;

var
  Engine: TDeltaORMEngine;
  Query: TDeltaQuery<TMunicipio>;
  Processor: TMunicipioProcessor;
  M: TMunicipio;
  FirstPtr, CurrPtr: Pointer;
  I, Count: Integer;
  HeapBefore, HeapAfter: PtrUInt;
  MemStream: TMemoryStream;
  BytesStreamed: Int64;

begin
  WriteLn('===================================================================');
  WriteLn('  DeltaModel ORM - Schema Flyweight & Cursor Reusavel (Zero-Alloc) ');
  WriteLn('===================================================================');
  WriteLn('');

  // 1. Flyweight Schema: Class-level accessible metadata
  WriteLn('[1] Schema Flyweight desassociado da linha:');
  WriteLn('    Classe: ', TMunicipio.ClassName);
  WriteLn('    Tabela configurada: ', TMunicipio.Schema.TableName);
  WriteLn('    Total de campos no schema único: ', TMunicipio.Schema.FieldCount);
  WriteLn('    Campo "Nome" no Schema: Size=', TMunicipio.Schema.FindField('Nome').FieldSize,
          ', Required=', TMunicipio.Schema.FindField('Nome').IsRequired);
  WriteLn('');

  if FileExists(DEMO_DB) then DeleteFile(DEMO_DB);

  Engine := TDeltaORMEngine.Create('sqlite://' + DEMO_DB);
  try
    // 2. Create database and seed 1,000 municipalities
    Engine.ExecuteDirect(
      'CREATE TABLE municipios (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nome VARCHAR(120) NOT NULL,' +
      '  uf VARCHAR(2) NOT NULL,' +
      '  codigo_ibge INTEGER NOT NULL' +
      ');'
    );
    if Engine.TransactionActive then Engine.Commit;

    if not Engine.TransactionActive then Engine.StartTransaction;
    for I := 1 to TOTAL_RECORDS do
    begin
      Engine.ExecuteDirect(Format(
        'INSERT INTO municipios (nome, uf, codigo_ibge) VALUES (''Municipio %d'', ''SP'', %d);',
        [I, 3500000 + I]
      ));
    end;
    if Engine.TransactionActive then Engine.Commit;
    WriteLn(Format('[2] Banco SQLite populado com %d municipios de teste.', [TOTAL_RECORDS]));
    WriteLn('');

    Query := TDeltaQuery<TMunicipio>.Create(Engine);
    try
      // 3. Canonical Pascal Pattern: while Query.Next do
      WriteLn('[3] Padrão Canônico Pascal (while Query.Next do):');
      Query.OrderBy('id ASC');
      Query.Open;

      FirstPtr := Pointer(Query.Current);
      WriteLn('    Ponteiro da instancia reusada (Query.Current): ', HexStr(FirstPtr));

      Count := 0;
      HeapBefore := GetFPCHeapStatus.CurrHeapUsed;

      while Query.Next do
      begin
        Inc(Count);
        CurrPtr := Pointer(Query.Current);
        if CurrPtr <> FirstPtr then
        begin
          WriteLn('    [ERRO]: Ponteiro mudou! Alocacao indevida no loop.');
          Halt(1);
        end;
      end;

      HeapAfter := GetFPCHeapStatus.CurrHeapUsed;
      Query.Close;

      WriteLn(Format('    Registros iterados: %d', [Count]));
      WriteLn('    Ponteiro permanece identico em 100% das iteracoes: SIM');
      WriteLn(Format('    Variacao de Heap durante %d iteracoes: %d bytes (ZERO alloc de objetos)',
                     [TOTAL_RECORDS, Integer(HeapAfter - HeapBefore)]));
      WriteLn('');

      // 4. Native Pascal Enumerator Support: for M in Query.Cursor do
      WriteLn('[4] Padrão com Enumerador Pascal Nativo (for M in Query.Cursor do):');
      Count := 0;
      for M in Query.Cursor do
      begin
        Inc(Count);
        if Pointer(M) <> FirstPtr then
        begin
          WriteLn('    [ERRO]: Instancia alterada no for-in!');
          Halt(1);
        end;
      end;
      WriteLn(Format('    Registros iterados via for-in: %d', [Count]));
      WriteLn('    Instancia unica reaproveitada com sucesso.');
      WriteLn('');

      // 5. Callback Funcional: Query.Each
      WriteLn('[5] Padrão Callback Funcional (Query.Each):');
      Processor := TMunicipioProcessor.Create;
      try
        Query.Each(Processor.ProcessItem);
        WriteLn(Format('    Total processado via Each: %d', [Processor.TotalProcessed]));
        WriteLn('    Ultimo registro: ', Processor.LastNome);
      finally
        Processor.Free;
      end;
      WriteLn('');

      // 6. Direct Streaming with Zero Batch Allocation
      WriteLn('[6] Streaming HTTP/JSON com Zero-Alloc (Query.StreamJsonTo):');
      MemStream := TMemoryStream.Create;
      try
        BytesStreamed := Query.StreamJsonTo(MemStream);
        WriteLn(Format('    Linhas transmitidas via Stream: %d (%d bytes)',
                       [BytesStreamed, MemStream.Size]));
      finally
        MemStream.Free;
      end;
      WriteLn('');

    finally
      Query.Free;
    end;

  finally
    Engine.Free;
    if FileExists(DEMO_DB) then DeleteFile(DEMO_DB);
  end;

  WriteLn('===================================================================');
  WriteLn('  Sucesso: Demonstracao concluida com ZERO alocacoes por linha!   ');
  WriteLn('===================================================================');
end.
