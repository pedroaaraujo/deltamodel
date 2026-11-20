program fluent_generics_demo;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, DB, SQLDB, sqlite3conn,
  DeltaModel, DeltaModel.Fields, DeltaModel.Generics, DeltaModel.ORM.Connection;

type
  {============================================================================}
  { Modern Model - Approach B                                                 }
  { Uses native Free Pascal primitive properties (Int64, string,              }
  { Double, Boolean) without Variant boxing overhead or wrapper objects.      }
  {============================================================================}
  TProduct = class(TDeltaModel)
  private
    FId: Int64;
    FName: string;
    FPrice: Double;
    FCost: Double;
    FStock: Integer;
    FActive: Boolean;
  public
    class procedure ConfigureSchema(ASchema: TDeltaModelSchema); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Cost: Double read FCost write FCost;
    property Stock: Integer read FStock write FStock;
    property Active: Boolean read FActive write FActive;
  end;

{==============================================================================}
{ Static Metadata Configuration (Fluent Class Schema)                          }
{ Executed only once per class via RTTI Cache, with zero allocation per model  }
{ instance!                                                                    }
{==============================================================================}
class procedure TProduct.ConfigureSchema(ASchema: TDeltaModelSchema);
begin
  ASchema.Table('products');
  ASchema.Field('Id').Column('id').AsPK.AutoInc;
  ASchema.Field('Name').Column('name').Required.Size(150);
  ASchema.Field('Price').Column('price').Required;
  ASchema.Field('Cost').Column('cost').Nullable;
  ASchema.Field('Stock').Column('stock').Required;
  ASchema.Field('Active').Column('active').Required;
end;

const
  DEMO_DB = '/tmp/deltamodel_fluent_demo.db';

var
  Engine: TDeltaORMEngine;
  Query: TDeltaQuery<TProduct>;
  P, Found: TProduct;
  List, Lote: TGDeltaModelList<TProduct>;
  JsonData: RawByteString;
  I: Integer;
begin
  WriteLn('===================================================================');
  WriteLn('  DeltaModel ORM - Exemplo de Fluent Class Schema & Generics       ');
  WriteLn('===================================================================');
  WriteLn('Demonstra a Abordagem B com propriedades primitivas nativas,');
  WriteLn('consulta tipada via TDeltaQuery<T> e colecoes TGDeltaModelList<T>.');
  WriteLn('');

  if FileExists(DEMO_DB) then DeleteFile(DEMO_DB);

  Engine := TDeltaORMEngine.Create('sqlite://' + DEMO_DB);
  try
    // 1. Create sample table
    Engine.ExecuteDirect(
      'CREATE TABLE products (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(150) NOT NULL,' +
      '  price REAL NOT NULL,' +
      '  cost REAL,' +
      '  stock INTEGER NOT NULL,' +
      '  active INTEGER NOT NULL' +
      ');'
    );
    if Engine.TransactionActive then Engine.Commit;
    WriteLn('[1] Tabela "products" criada com sucesso.');

    // 2. Instanciar Query Tipada
    Query := TDeltaQuery<TProduct>.Create(Engine);
    try
      // Insert records
      P := TProduct.Create;
      P.Name := 'Notebook Dell Inspiron';
      P.Price := 4299.90;
      P.Cost := 3100.00;
      P.Stock := 12;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      P := TProduct.Create;
      P.Name := 'Mouse Gamer RGB';
      P.Price := 149.90;
      P.SetNull('Cost'); // Bitmask de nulidade ultraleve
      P.Stock := 80;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      P := TProduct.Create;
      P.Name := 'Monitor Curvo 34" Ultrawide';
      P.Price := 2890.00;
      P.Cost := 1950.00;
      P.Stock := 5;
      P.Active := True;
      Query.Insert(P);
      P.Free;

      WriteLn(Format('[2] Inseridos 3 produtos. Total no banco: %d', [Query.Count]));

      // 3. Typed Primary Key Lookup
      Found := Query.FindById(1);
      if Found <> nil then
      try
        WriteLn(Format('[3] Produto ID 1 localizado: "%s" | Preco: R$ %.2f | Estoque: %d',
          [Found.Name, Found.Price, Found.Stock]));
      finally
        Found.Free;
      end;

      // 4. Batch Insert (BulkInsert)
      Lote := TGDeltaModelList<TProduct>.Create;
      try
        for I := 1 to 5 do
        begin
          P := TProduct.Create;
          P.Name := 'Acessorio Avulso #' + IntToStr(I);
          P.Price := 29.90 * I;
          P.Stock := 20;
          P.Active := True;
          Lote.Add(P);
        end;
        Query.BulkInsert(Lote, 500);
        Query.Clear;
        WriteLn(Format('[4] BulkInsert executado com 5 itens. Total no banco: %d', [Query.Count]));
      finally
        Lote.Free;
      end;

      // 5. Filtered and Ordered Query with Idiomatic Iteration
      WriteLn('');
      WriteLn('[5] Listando produtos com preco > R$ 100,00 (ordenados por preco decrescente):');
      Query.Clear;
      List := Query.Where('price > :min_price')
                   .Param('min_price', 100.0)
                   .OrderBy('price DESC')
                   .All;
      try
        // Direct idiomatic loop without typecasting!
        for P in List do
        begin
          WriteLn(Format('    - [%d] %-30s | R$ %8.2f | Custo: %s | Estoque: %3d',
            [P.Id, P.Name, P.Price,
             IfThen(P.IsNull('Cost'), 'NULO', Format('R$ %8.2f', [P.Cost])),
             P.Stock]));
        end;

        // 6. Ultra High-Performance JSON Serialization
        JsonData := List.ToJson;
        WriteLn('');
        WriteLn('[6] Serializacao JSON (FastJsonStreamer):');
        WriteLn('    Tamanho gerado: ' + IntToStr(Length(JsonData)) + ' bytes.');
        WriteLn('    Amostra: ' + Copy(JsonData, 1, 140) + ' ... ]');

      finally
        List.Free;
      end;

    finally
      Query.Free;
    end;

  finally
    Engine.Free;
    if FileExists(DEMO_DB) then DeleteFile(DEMO_DB);
  end;

  WriteLn('');
  WriteLn('Exemplo executado com sucesso!');
end.
