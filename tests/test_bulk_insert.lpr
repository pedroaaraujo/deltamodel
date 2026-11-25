program test_bulk_insert;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, sqlite3conn,
  DeltaModel, DeltaModel.Fields,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry,
  DeltaModel.ORM.Dialects, DeltaModel.ORM.Driver.SQLDB,
  DeltaModel.ORM.DML, DeltaModel.ORM.Persister, DeltaModel.SQLBuilder,
  DeltaModel.ORM.Pool, DeltaValidator;

type
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

function CountCustomers(const AConn: IDeltaConnection): Int64;
var
  Q: TQuery;
begin
  Q := TQuery.Create(AConn);
  try
    Q.SetModel(TCustomer);
    Result := Q.Count;
  finally
    Q.Free;
  end;
end;

procedure SetupTestDatabase(const AConn: IDeltaConnection);
begin
  AConn.ExecuteDirect('DROP TABLE IF EXISTS customers;');
  AConn.ExecuteDirect(
    'CREATE TABLE customers (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  name TEXT NOT NULL,' +
    '  email TEXT,' +
    '  score INTEGER' +
    ');'
  );
  if AConn.TransactionActive then
    AConn.Commit;
end;

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

    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], 'sqlite');
    AssertTrue(SQL.StartsWith('INSERT INTO customers (name, email, score) VALUES'), 'SQLite deve iniciar com INSERT INTO ... VALUES');
    AssertTrue(SQL.Contains('(:name_0, :email_0, :score_0)'), 'Tupla 0 deve ter sufixo _0');
    AssertTrue(SQL.Contains('(:name_1, :email_1, :score_1)'), 'Tupla 1 deve ter sufixo _1');
    AssertTrue(not SQL.Contains('id'), 'AutoInc Id nao preenchido nao deve estar nas colunas');

    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], 'firebird');
    AssertTrue(SQL.StartsWith('EXECUTE BLOCK AS'), 'Firebird deve usar EXECUTE BLOCK');
    AssertTrue(SQL.Contains('INSERT INTO customers (name, email, score) VALUES (:name_0, :email_0, :score_0);'), 'Firebird deve conter insercao da linha 0');
    AssertTrue(SQL.Contains('INSERT INTO customers (name, email, score) VALUES (:name_1, :email_1, :score_1);'), 'Firebird deve conter insercao da linha 1');
    AssertTrue(SQL.EndsWith('END'), 'Firebird deve encerrar com END');

    SQL := TDMSQLBuilder.CreateBulkInsert([C1, C2], 'oracle');
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

procedure TestBulkInsertArray(const AConn: IDeltaConnection);
var
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Count: Int64;
  Cust: TCustomer;
begin
  Write('Teste 2: TDeltaPersister.BulkInsert com array of TDeltaModel... ');
  SetupTestDatabase(AConn);

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
    Inserted := TDeltaPersister.BulkInsert(AConn, Models);
    AssertTrue(Inserted = 10, 'Deve ter retornado 10 registros inseridos');

    Count := CountCustomers(AConn);
    AssertTrue(Count = 10, 'Count deve ser 10');
    WriteLn('OK! (' + IntToStr(Inserted) + ' inseridos)');
  finally
    for I := 0 to 9 do
      Models[I].Free;
  end;
end;

procedure TestBulkInsertList(const AConn: IDeltaConnection);
var
  List: TDeltaModelList;
  I, Inserted: Integer;
  Cust: TCustomer;
  Count: Int64;
begin
  Write('Teste 3: TDeltaPersister.BulkInsert com TDeltaModelList... ');
  SetupTestDatabase(AConn);

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

    Inserted := TDeltaPersister.BulkInsert(AConn, List);
    AssertTrue(Inserted = 15, 'Deve retornar 15');

    Count := CountCustomers(AConn);
    AssertTrue(Count = 15, 'Count deve ser 15');
    WriteLn('OK! (' + IntToStr(Inserted) + ' inseridos)');
  finally
    List.Free;
  end;
end;

procedure TestBulkInsertChunking(const AConn: IDeltaConnection);
var
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Cust: TCustomer;
  Count: Int64;
begin
  Write('Teste 4: BulkInsert com particionamento (25 itens, batch_size=7)... ');
  SetupTestDatabase(AConn);

  SetLength(Models, 25);
  for I := 0 to 24 do
  begin
    Cust := TCustomer.Create;
    Cust.Name.Value := 'Lote ' + IntToStr(I + 1);
    if I mod 2 = 0 then
      Cust.Email.Value := 'lote' + IntToStr(I + 1) + '@mail.com'
    else
      Cust.Email.Clear;
    Cust.Score.Value := I;
    Models[I] := Cust;
  end;

  try
    Inserted := TDeltaPersister.BulkInsert(AConn, Models, 7);
    AssertTrue(Inserted = 25, 'Total inserido deve ser 25');

    Count := CountCustomers(AConn);
    AssertTrue(Count = 25, 'Count no banco deve ser 25');
    WriteLn('OK! (25 itens em 4 batches de 7)');
  finally
    for I := 0 to 24 do
      Models[I].Free;
  end;
end;

procedure TestBulkInsertValidationRollback(const AConn: IDeltaConnection);
var
  Models: array of TDeltaModel;
  I: Integer;
  Cust: TCustomer;
  Count: Int64;
  Failed: Boolean;
begin
  Write('Teste 5: Validacao previa e atomicidade (Rollback em falha)... ');
  SetupTestDatabase(AConn);

  SetLength(Models, 5);
  for I := 0 to 4 do
  begin
    Cust := TCustomer.Create;
    if I = 3 then
      Cust.Name.Value := ''
    else
      Cust.Name.Value := 'Valido ' + IntToStr(I);
    Models[I] := Cust;
  end;

  Failed := False;
  try
    try
      TDeltaPersister.BulkInsert(AConn, Models);
    except
      on E: EDeltaValidation do
        Failed := True;
    end;

    AssertTrue(Failed, 'Deveria ter lancado EDeltaValidation');

    Count := CountCustomers(AConn);
    AssertTrue(Count = 0, 'Nenhum registro deve ter sido gravado apos falha');
    WriteLn('OK! (Rollback confirmado, nenhum registro inserido)');
  finally
    for I := 0 to 4 do
      Models[I].Free;
  end;
end;

procedure TestBulkInsertWithPool;
var
  Pool: TDeltaConnectionPool;
  Lease: IDeltaConnection;
  Models: array of TDeltaModel;
  I, Inserted: Integer;
  Cust: TCustomer;
  Count: Int64;
begin
  Write('Teste 6: BulkInsert via Connection Pool (Direct & Lease)... ');

  Pool := TDeltaConnectionPool.Create('sqlite://' + TEST_DB + '?pool_min=2&pool_max=5');
  try
    Lease := Pool.Acquire;
    SetupTestDatabase(Lease);
    Lease := nil;

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
        Inserted := TDeltaPersister.BulkInsert(Lease, Models);
        AssertTrue(Inserted = 4, 'Lease BulkInsert deve inserir 4');
      finally
        for I := 0 to 3 do
          Models[I].Free;
      end;

      Count := CountCustomers(Lease);
      AssertTrue(Count = 12, 'Total no banco deve ser 12 (8 + 4)');
    finally
      Lease := nil;
    end;

    WriteLn('OK!');
  finally
    Pool.Free;
  end;
end;

procedure RunDirectTests;
var
  Conn: IDeltaConnection;
begin
  Conn := TDeltaORM.Connect('sqlite://' + TEST_DB);
  try
    TestBulkInsertArray(Conn);
    TestBulkInsertList(Conn);
    TestBulkInsertChunking(Conn);
    TestBulkInsertValidationRollback(Conn);
  finally
    Conn.Disconnect;
    Conn := nil;
  end;
end;

begin
  WriteLn('====================================================');
  WriteLn('      Iniciando Testes de Bulk Insert DeltaModel    ');
  WriteLn('====================================================');

  if FileExists(TEST_DB) then
    DeleteFile(TEST_DB);

  TestSQLBuilderDialects;
  RunDirectTests;
  TestBulkInsertWithPool;

  if FileExists(TEST_DB) then
    DeleteFile(TEST_DB);

  WriteLn('====================================================');
  WriteLn('    TODOS OS TESTES DE BULK INSERT PASSARAM!       ');
  WriteLn('====================================================');
end.
