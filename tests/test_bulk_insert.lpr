program test_bulk_insert;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DB, SQLDB, sqlite3conn,
  DeltaModel, DeltaModel.Fields, DeltaModel.ORM.Connection,
  DeltaModel.ORM.DML, DeltaModel.SQLBuilder, DeltaModel.ORM.Types,
  DeltaModel.ORM.Pool, DeltaValidator;

type
  { Modelo de teste para clientes }
  TCustomer = class(TDeltaModel)
  private
    FId: TDFIntRequired;
    FName: TDFStringRequired;
    FEmail: TDFStringNull;
    FScore: TDFIntNull;
  published
    property Id: TDFIntRequired read FId write FId;
    property Name: TDFStringRequired read FName write FName;
    property Email: TDFStringNull read FEmail write FEmail;
    property Score: TDFIntNull read FScore write FScore;
  public

    procedure AfterConstruction; override;
    procedure Validate; override;
  end;

procedure TCustomer.AfterConstruction;
begin
  inherited AfterConstruction;
  TableName := 'customers';

  Id.FieldName := 'id';
  Id.DBOptions := [dboPrimaryKey, dboAutoInc];

  Name.FieldName := 'name';
  Name.DBOptions := [dboUpdate];

  Email.FieldName := 'email';
  Email.DBOptions := [dboUpdate];

  Score.FieldName := 'score';
  Score.DBOptions := [dboUpdate];
end;

procedure TCustomer.Validate;
begin
  inherited Validate;
  Validator.AddField('name', Name.Value).AddValidator(TValidatorItemNotEmpty.Create);
end;

const
  TEST_DB = '/tmp/deltamodel_bulk_test.db';

procedure AssertTrue(const ACondition: Boolean; const AMsg: string);
begin
  if not ACondition then
  begin
    WriteLn('   [FALHA] ' + AMsg);
    Halt(1);
  end;
end;

procedure SetupTestDatabase(Engine: TDeltaORMEngine);
begin
  Engine.ExecuteDirect('DROP TABLE IF EXISTS customers;');
  Engine.ExecuteDirect(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  score INTEGER' +
    ');'
  );
  if Engine.TransactionActive then
    Engine.Commit;
end;

{ Teste 1: SQLBuilder - Geração de SQL por Dialeto }
procedure TestSQLBuilderDialects;
var
  C1, C2: TCustomer;
  SQL: string;
begin
  Write('Teste 1: TDMSQLBuilder.CreateBulkInsert por dialeto... ');

  C1 := TCustomer.Create;
  C2 := TCustomer.Create;
  try
    C1.Name.Value := 'Alice';
    C2.Name.Value := 'Bob';

    // Dialeto SQLite / Postgres / MySQL / MSSQL (padrão)
    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], ddSQLite);
    AssertTrue(SQL.StartsWith('INSERT INTO customers (name, email, score) VALUES'), 'SQLite deve iniciar com INSERT INTO ... VALUES');
    AssertTrue(SQL.Contains('(:name_0, :email_0, :score_0)'), 'Tupla 0 deve ter sufixo _0');
    AssertTrue(SQL.Contains('(:name_1, :email_1, :score_1)'), 'Tupla 1 deve ter sufixo _1');
    AssertTrue(not SQL.Contains('id'), 'AutoInc Id não preenchido não deve estar nas colunas');

    // Dialeto Firebird (EXECUTE BLOCK)
    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], ddFirebird);
    AssertTrue(SQL.StartsWith('EXECUTE BLOCK AS'), 'Firebird deve usar EXECUTE BLOCK');
    AssertTrue(SQL.Contains('INSERT INTO customers (name, email, score) VALUES (:name_0, :email_0, :score_0);'), 'Firebird deve conter inserção da linha 0');
    AssertTrue(SQL.Contains('INSERT INTO customers (name, email, score) VALUES (:name_1, :email_1, :score_1);'), 'Firebird deve conter inserção da linha 1');
    AssertTrue(SQL.EndsWith('END'), 'Firebird deve encerrar com END');

    // Dialeto Oracle (INSERT ALL)
    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], ddOracle);
    AssertTrue(SQL.StartsWith('INSERT ALL'), 'Oracle deve usar INSERT ALL');
    AssertTrue(SQL.Contains('INTO customers (name, email, score) VALUES (:name_0, :email_0, :score_0)'), 'Oracle deve conter INTO ... VALUES para linha 0');
    AssertTrue(SQL.Contains('INTO customers (name, email, score) VALUES (:name_1, :email_1, :score_1)'), 'Oracle deve conter INTO ... VALUES para linha 1');
    AssertTrue(SQL.Contains('SELECT 1 FROM DUAL'), 'Oracle deve encerrar com SELECT 1 FROM DUAL');

    WriteLn('OK!');
  finally
    C1.Free;
    C2.Free;
  end;
end;

{ Teste 2: BulkInsert básico com array de TDeltaModel }
procedure TestBulkInsertArray(Engine: TDeltaORMEngine);
var
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Count: Int64;
  Cust: TCustomer;
begin
  Write('Teste 2: Engine.BulkInsert com array of TDeltaModel... ');
  SetupTestDatabase(Engine);

  SetLength(Models, 10);
  for I := 0 to 9 do
  begin
    Cust := TCustomer.Create;
    Cust.Name.Value := 'Cliente ' + IntToStr(I + 1);
    Cust.Email.Value := 'cliente' + IntToStr(I + 1) + '@example.com';
    Cust.Score.Value := (I + 1) * 100;
    Models[I] := Cust;
  end;

  try
    Inserted := Engine.BulkInsert(Models);
    AssertTrue(Inserted = 10, 'Deve ter retornado 10 registros inseridos');

    Count := Engine.Count(TCustomer);
    AssertTrue(Count = 10, 'Count deve ser 10');
    WriteLn('OK! (' + IntToStr(Inserted) + ' inseridos)');
  finally
    for I := 0 to 9 do
      Models[I].Free;
  end;
end;

{ Teste 3: BulkInsert com TDeltaModelList }
procedure TestBulkInsertList(Engine: TDeltaORMEngine);
var
  List: TDeltaModelList;
  I, Inserted: Integer;
  Cust: TCustomer;
  Count: Int64;
begin
  Write('Teste 3: Engine.BulkInsert com TDeltaModelList... ');
  SetupTestDatabase(Engine);

  List := TDeltaModelList.Create;
  try
    List.SetDeltaModelClass(TCustomer);
    for I := 1 to 15 do
    begin
      Cust := TCustomer.Create;
      Cust.Name.Value := 'Pessoa ' + IntToStr(I);
      Cust.Email.Value := 'pessoa' + IntToStr(I) + '@teste.com';
      Cust.Score.Value := I * 50;
      List.Add(Cust);
    end;

    Inserted := Engine.BulkInsert(List);
    AssertTrue(Inserted = 15, 'Deve retornar 15');

    Count := Engine.Count(TCustomer);
    AssertTrue(Count = 15, 'Count deve ser 15');
    WriteLn('OK! (' + IntToStr(Inserted) + ' inseridos)');
  finally
    List.Free;
  end;
end;

{ Teste 4: BulkInsert com Chunking / Particionamento em lotes }
procedure TestBulkInsertChunking(Engine: TDeltaORMEngine);
var
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Cust: TCustomer;
  Count: Int64;
begin
  Write('Teste 4: BulkInsert com particionamento (25 itens, batch_size=7)... ');
  SetupTestDatabase(Engine);

  SetLength(Models, 25);
  for I := 0 to 24 do
  begin
    Cust := TCustomer.Create;
    Cust.Name.Value := 'Lote ' + IntToStr(I + 1);
    if I mod 2 = 0 then
      Cust.Email.Value := 'lote' + IntToStr(I + 1) + '@mail.com'
    else
      Cust.Email.Clear; // testa valores nulos intercalados
    Cust.Score.Value := I;
    Models[I] := Cust;
  end;

  try
    // Batch size pequeno (7) para forçar 4 iterações (7 + 7 + 7 + 4)
    Inserted := Engine.BulkInsert(Models, 7);
    AssertTrue(Inserted = 25, 'Total inserido deve ser 25');

    Count := Engine.Count(TCustomer);
    AssertTrue(Count = 25, 'Count no banco deve ser 25');
    WriteLn('OK! (25 itens em 4 batches de 7)');
  finally
    for I := 0 to 24 do
      Models[I].Free;
  end;
end;

{ Teste 5: Transacionalidade e Rollback em caso de falha de validação }
procedure TestBulkInsertValidationRollback(Engine: TDeltaORMEngine);
var
  Models: array of TDeltaModel;
  I: Integer;
  Cust: TCustomer;
  Count: Int64;
  Failed: Boolean;
begin
  Write('Teste 5: Validação prévia e atomicidade (Rollback em falha)... ');
  SetupTestDatabase(Engine);

  SetLength(Models, 5);
  for I := 0 to 4 do
  begin
    Cust := TCustomer.Create;
    if I = 3 then
      Cust.Name.Value := '' // Inválido! Campo obrigatório não vazio
    else
      Cust.Name.Value := 'Valido ' + IntToStr(I);
    Models[I] := Cust;
  end;

  Failed := False;
  try
    try
      Engine.BulkInsert(Models);
    except
      on E: EDeltaValidation do
        Failed := True;
    end;

    AssertTrue(Failed, 'Deveria ter lançado EDeltaValidation');

    Count := Engine.Count(TCustomer);
    AssertTrue(Count = 0, 'Nenhum registro deve ter sido gravado após falha');
    WriteLn('OK! (Rollback confirmado, nenhum registro inserido)');
  finally
    for I := 0 to 4 do
      Models[I].Free;
  end;
end;

{ Teste 6: BulkInsert através do Connection Pool }
procedure TestBulkInsertWithPool;
var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaPooledEngine;
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Cust: TCustomer;
  Engine: TDeltaORMEngine;
  Count: Int64;
begin
  Write('Teste 6: BulkInsert via Connection Pool (Direct & Lease)... ');
  
  Pool := TDeltaConnectionPool.Create('sqlite://' + TEST_DB + '?pool_min=2&pool_max=5');
  try
    // Prepara tabela
    Lease := Pool.Acquire;
    Engine := Lease.Engine;
    SetupTestDatabase(Engine);
    Lease := nil; // Devolve conexão ao pool

    // Inserção via Pool.BulkInsert direto
    SetLength(Models, 8);
    for I := 0 to 7 do
    begin
      Cust := TCustomer.Create;
      Cust.Name.Value := 'Pool Customer ' + IntToStr(I + 1);
      Cust.Score.Value := 1000 + I;
      Models[I] := Cust;
    end;

    try
      Inserted := Pool.BulkInsert(Models);
      AssertTrue(Inserted = 8, 'Pool.BulkInsert deve inserir 8');
    finally
      for I := 0 to 7 do
        Models[I].Free;
    end;

    // Inserção via Lease.BulkInsert
    Lease := Pool.Acquire;
    try
      SetLength(Models, 4);
      for I := 0 to 3 do
      begin
        Cust := TCustomer.Create;
        Cust.Name.Value := 'Lease Customer ' + IntToStr(I + 1);
        Models[I] := Cust;
      end;

      try
        Inserted := Lease.BulkInsert(Models);
        AssertTrue(Inserted = 4, 'Lease.BulkInsert deve inserir 4');
      finally
        for I := 0 to 3 do
          Models[I].Free;
      end;

      Count := Lease.Count(TCustomer);
      AssertTrue(Count = 12, 'Total no banco deve ser 12 (8 + 4)');
    finally
      Lease := nil;
    end;

    WriteLn('OK!');
  finally
    Pool.Free;
  end;
end;

var
  Engine: TDeltaORMEngine;
begin
  WriteLn('====================================================');
  WriteLn('      Iniciando Testes de Bulk Insert DeltaModel    ');
  WriteLn('====================================================');

  // Testes de SQL
  TestSQLBuilderDialects;

  // Inicializa engine para SQLite de teste
  Engine := TDeltaORMEngine.Create('sqlite://' + TEST_DB);
  try
    TestBulkInsertArray(Engine);
    TestBulkInsertList(Engine);
    TestBulkInsertChunking(Engine);
    TestBulkInsertValidationRollback(Engine);
  finally
    Engine.Free;
  end;

  // Teste com pool
  TestBulkInsertWithPool;

  // Limpa banco de teste
  if FileExists(TEST_DB) then
    DeleteFile(TEST_DB);

  WriteLn('====================================================');
  WriteLn('    TODOS OS TESTES DE BULK INSERT PASSARAM!       ');
  WriteLn('====================================================');
end.
