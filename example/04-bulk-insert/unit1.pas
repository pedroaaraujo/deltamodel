unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.SQLBuilder,
  DeltaModel.ORM.Types, DeltaModel.ORM.Interfaces, DeltaModel.ORM.Registry,
  DeltaModel.ORM.Dialects, DeltaModel.ORM.Driver.SQLDB,
  DeltaModel.ORM.Persister, DeltaModel.ORM.Schema,
  DeltaModel.ORM.DDL, DeltaModel.ORM.DML;

type

  { TProduct - Sample model supporting all data types for Bulk Insert }

  TProduct = class(TDeltaModel)
  private
    Fid:                TDFIntNull;
    Fname:              TDFStringRequired;
    Fsku:               TDFStringRequired;
    Fbarcode:           TDFInt64Null;
    Fprice:             TDFCurrencyRequired;
    Fweight:            TDFDoubleNull;
    Fstock:             TDFIntRequired;
    FmanufacturingDate: TDFDateNull;
    Fdescription:       TDFTextNull;
    FproductUuid:       TDFUUIDNull;
    Factive:            TDFBooleanRequired;
    Fcreated_at:        TDFDateTimeNull;
  published
    property id:                TDFIntNull          read Fid                write Fid;
    property name:              TDFStringRequired   read Fname              write Fname;
    property sku:               TDFStringRequired   read Fsku               write Fsku;
    property barcode:           TDFInt64Null        read Fbarcode           write Fbarcode;
    property price:             TDFCurrencyRequired read Fprice             write Fprice;
    property weight:            TDFDoubleNull       read Fweight            write Fweight;
    property stock:             TDFIntRequired      read Fstock             write Fstock;
    property manufacturingDate: TDFDateNull         read FmanufacturingDate write FmanufacturingDate;
    property description:       TDFTextNull         read Fdescription       write Fdescription;
    property productUuid:       TDFUUIDNull         read FproductUuid       write FproductUuid;
    property active:            TDFBooleanRequired  read Factive            write Factive;
    property created_at:        TDFDateTimeNull     read Fcreated_at        write Fcreated_at;
  public
    procedure AfterConstruction; override;
    procedure BeforeInsert; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnBulkArray: TButton;
    btnBulkList: TButton;
    btnPreviewSQL: TButton;
    btnSaveOneByOne: TButton;
    edtURL: TEdit;
    lblURL: TLabel;
    lblQty: TLabel;
    lblBatchSize: TLabel;
    lblSQLPreview: TLabel;
    lblLog: TLabel;
    mmoLog: TMemo;
    mmoSQL: TMemo;
    pnlTop: TPanel;
    pnlMiddle: TPanel;
    spnQty: TSpinEdit;
    spnBatchSize: TSpinEdit;
    cmbDialect: TComboBox;
    lblDialect: TLabel;
    procedure btnBulkArrayClick(Sender: TObject);
    procedure btnBulkListClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure btnSaveOneByOneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Log(const AMsg: string);
    procedure PrepareSchema(const ACon: IDeltaConnection);
    function CountProducts(const ACon: IDeltaConnection): Int64;
    function CurrentDialect: IDeltaDialect;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TProduct }

procedure TProduct.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.TableName := 'products';
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.name.Size := 200;
  Self.sku.Size := 50;
end;

procedure TProduct.BeforeInsert;
begin
  inherited BeforeInsert;
  if created_at.IsNull then
    created_at.Value := Now;
end;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  spnQty.Value := 1000;
  spnBatchSize.Value := 500;
  cmbDialect.ItemIndex := 2; // SQLite by default
end;

function TForm1.CurrentDialect: IDeltaDialect;
begin
  case cmbDialect.ItemIndex of
    0: Result := TDeltaORM.Dialect('postgresql');
    1: Result := TDeltaORM.Dialect('mysql');
    2: Result := TDeltaORM.Dialect('sqlite');
    3: Result := TDeltaORM.Dialect('firebird');
    4: Result := TDeltaORM.Dialect('mssql');
    5: Result := TDeltaORM.Dialect('oracle');
  else
    Result := TDeltaORM.Dialect('sqlite');
  end;
end;

procedure TForm1.Log(const AMsg: string);
begin
  mmoLog.Lines.Add(AMsg);
  Application.ProcessMessages;
end;

function TForm1.CountProducts(const ACon: IDeltaConnection): Int64;
var
  Q: TQuery;
begin
  Q := TQuery.Create(ACon);
  try
    Q.SetModel(TProduct);
    Result := Q.Count;
  finally
    Q.Free;
  end;
end;

procedure TForm1.PrepareSchema(const ACon: IDeltaConnection);
var
  Schema: TDeltaORMSchema;
begin
  Schema := TDeltaORMSchema.Create(ACon);
  try
    Schema.RegisterModel(TProduct.Create);
    Schema.PrepareDB(True);
    Log('   Schema preparado: tabela "products" criada/verificada.');
  finally
    Schema.Free;
  end;
end;

{ === Bulk Insert SQL preview for the selected dialect === }

procedure TForm1.btnPreviewSQLClick(Sender: TObject);
var
  Models: array of TDeltaModel;
  I, PreviewCount: Integer;
  Dialect: IDeltaDialect;
begin
  mmoSQL.Clear;
  Dialect := CurrentDialect;

  // Generate preview with 3 records for readability
  PreviewCount := 3;
  SetLength(Models, PreviewCount);
  for I := 0 to PreviewCount - 1 do
  begin
    Models[I] := TProduct.Create;
    TProduct(Models[I]).name.Value := 'Produto ' + IntToStr(I + 1);
    TProduct(Models[I]).sku.Value := 'SKU-' + IntToStr(I + 1);
    TProduct(Models[I]).barcode.Value := 7891234560000 + I;
    TProduct(Models[I]).price.Value := 49.90 + I * 10;
    TProduct(Models[I]).weight.Value := 1.25 + I * 0.5;
    TProduct(Models[I]).stock.Value := 100 + I;
    TProduct(Models[I]).manufacturingDate.Value := EncodeDate(2025, 1, 10 + I);
    TProduct(Models[I]).description.Value := 'Descrição detalhada do produto ' + IntToStr(I + 1);
    TProduct(Models[I]).productUuid.Value := Format('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380%03d', [I + 1]);
    TProduct(Models[I]).active.Value := True;
    TProduct(Models[I]).created_at.Value := Now;
  end;

  try
    mmoSQL.Lines.Text := TDMSQLBuilder.CreateBulkInsert(Models, Dialect.SQL);
  finally
    for I := 0 to PreviewCount - 1 do
      Models[I].Free;
  end;
end;

{ === Bulk Insert with open array === }

procedure TForm1.btnBulkArrayClick(Sender: TObject);
var
  Con: IDeltaConnection;
  Models: array of TDeltaModel;
  I, Total, RowsInserted: Integer;
  StartTime: TDateTime;
  ElapsedMs: Int64;
  CountAfter: Int64;
begin
  mmoLog.Clear;
  Total := spnQty.Value;

  Log('=== Bulk Insert com Array Aberto ===');
  Log(Format('Quantidade: %d | BatchSize: %d', [Total, spnBatchSize.Value]));
  Log('');

  Con := TDeltaORM.Connect(edtURL.Text);
  try
    try
      Log('1. Conectado (' + Con.Dialect.Name + ' via ' + Con.DriverName + ')');

      PrepareSchema(Con);

      try
        Con.ExecuteDirect('DELETE FROM products');
      except
      end;

      Log(Format('2. Criando %d objetos TProduct...', [Total]));
      SetLength(Models, Total);
      for I := 0 to Total - 1 do
      begin
        Models[I] := TProduct.Create;
        TProduct(Models[I]).name.Value := Format('Produto %d', [I + 1]);
        TProduct(Models[I]).sku.Value := Format('SKU-%0.6d', [I + 1]);
        TProduct(Models[I]).barcode.Value := 7890000000000 + I;
        TProduct(Models[I]).price.Value := 10.00 + Random(99000) / 100;
        TProduct(Models[I]).weight.Value := 0.5 + Random(500) / 100;
        TProduct(Models[I]).stock.Value := Random(1000);
        TProduct(Models[I]).manufacturingDate.Value := EncodeDate(2025, 1 + Random(12), 1 + Random(28));
        TProduct(Models[I]).description.Value := Format('Item catalogado no lote de bulk insert %d', [I + 1]);
        TProduct(Models[I]).productUuid.Value := Format('550e8400-e29b-41d4-a716-%012d', [I + 1]);
        TProduct(Models[I]).active.Value := (Random(10) > 1);
        TProduct(Models[I]).created_at.Value := Now;
      end;

      Log(Format('3. Executando TDeltaPersister.BulkInsert (batch=%d)...', [spnBatchSize.Value]));
      StartTime := Now;

      RowsInserted := TDeltaPersister.BulkInsert(Con, Models, spnBatchSize.Value);

      ElapsedMs := MilliSecondsBetween(Now, StartTime);
      if ElapsedMs = 0 then ElapsedMs := 1;

      Log(Format('   ✅ %d registros inseridos em %d ms', [RowsInserted, ElapsedMs]));

      CountAfter := CountProducts(Con);
      Log(Format('4. Verificação: COUNT(*) = %d', [CountAfter]));

      Log('');
      Log(Format('📊 Performance: %.0f registros/segundo', [RowsInserted / (ElapsedMs / 1000)]));

      ShowMessage(Format('Bulk Insert concluído!%s%d registros em %d ms',
        [sLineBreak, RowsInserted, ElapsedMs]));
    except
      on E: Exception do
      begin
        Log('❌ ERRO: ' + E.Message);
        ShowMessage('Erro: ' + E.Message);
      end;
    end;
  finally
    for I := 0 to Length(Models) - 1 do
      Models[I].Free;
    Con := nil;
  end;
end;

{ === Bulk Insert with TDeltaModelList === }

procedure TForm1.btnBulkListClick(Sender: TObject);
var
  Con: IDeltaConnection;
  List: TDeltaModelList;
  P: TProduct;
  I, Total, RowsInserted: Integer;
  StartTime: TDateTime;
  ElapsedMs: Int64;
  CountAfter: Int64;
begin
  mmoLog.Clear;
  Total := spnQty.Value;

  Log('=== Bulk Insert com TDeltaModelList ===');
  Log(Format('Quantidade: %d | BatchSize: %d', [Total, spnBatchSize.Value]));
  Log('');

  Con := TDeltaORM.Connect(edtURL.Text);
  try
    try
      Log('1. Conectado (' + Con.Dialect.Name + ' via ' + Con.DriverName + ')');

      PrepareSchema(Con);

      try
        Con.ExecuteDirect('DELETE FROM products');
      except
      end;

      Log(Format('2. Criando lista com %d objetos TProduct...', [Total]));
      List := TDeltaModelList.Create;
      try
        List.SetDeltaModelClass(TProduct);
        for I := 0 to Total - 1 do
        begin
          P := TProduct.Create;
          P.name.Value := Format('Item %d', [I + 1]);
          P.sku.Value := Format('LIST-%0.6d', [I + 1]);
          P.barcode.Value := 7891000000000 + I;
          P.price.Value := 5.00 + Random(50000) / 100;
          P.weight.Value := 0.25 + Random(300) / 100;
          P.stock.Value := Random(500);
          P.manufacturingDate.Value := EncodeDate(2025, 1 + Random(12), 1 + Random(28));
          P.description.Value := Format('Item de lista bulk insert %d', [I + 1]);
          P.productUuid.Value := Format('6ba7b810-9dad-11d1-80b4-%012d', [I + 1]);
          P.active.Value := True;
          P.created_at.Value := Now;
          List.Add(P);
        end;

        Log(Format('3. Executando TDeltaPersister.BulkInsert(List, %d)...', [spnBatchSize.Value]));
        StartTime := Now;

        RowsInserted := TDeltaPersister.BulkInsert(Con, List, spnBatchSize.Value);

        ElapsedMs := MilliSecondsBetween(Now, StartTime);
        if ElapsedMs = 0 then ElapsedMs := 1;

        Log(Format('   ✅ %d registros inseridos em %d ms', [RowsInserted, ElapsedMs]));

        CountAfter := CountProducts(Con);
        Log(Format('4. Verificação: COUNT(*) = %d', [CountAfter]));

        Log('');
        Log(Format('📊 Performance: %.0f registros/segundo', [RowsInserted / (ElapsedMs / 1000)]));

        ShowMessage(Format('Bulk Insert (List) concluído!%s%d registros em %d ms',
          [sLineBreak, RowsInserted, ElapsedMs]));
      finally
        List.Free;
      end;
    except
      on E: Exception do
      begin
        Log('❌ ERRO: ' + E.Message);
        ShowMessage('Erro: ' + E.Message);
      end;
    end;
  finally
    Con := nil;
  end;
end;

{ === Comparison: One-by-one Insert (Save) benchmark === }

procedure TForm1.btnSaveOneByOneClick(Sender: TObject);
var
  Con: IDeltaConnection;
  P: TProduct;
  I, Total: Integer;
  StartTime: TDateTime;
  ElapsedMs: Int64;
  CountAfter: Int64;
begin
  mmoLog.Clear;
  Total := spnQty.Value;

  Log('=== Insert Um-a-Um (Save) para Comparação ===');
  Log(Format('Quantidade: %d', [Total]));
  Log('');

  Con := TDeltaORM.Connect(edtURL.Text);
  try
    try
      Log('1. Conectado (' + Con.Dialect.Name + ' via ' + Con.DriverName + ')');

      PrepareSchema(Con);

      try
        Con.ExecuteDirect('DELETE FROM products');
      except
      end;

      Log(Format('2. Inserindo %d registros com TDeltaPersister.Save() individual...', [Total]));
      StartTime := Now;

      for I := 0 to Total - 1 do
      begin
        P := TProduct.Create;
        try
          P.name.Value := Format('Unitário %d', [I + 1]);
          P.sku.Value := Format('UNIT-%0.6d', [I + 1]);
          P.barcode.Value := 7892000000000 + I;
          P.price.Value := 10.00 + Random(99000) / 100;
          P.weight.Value := 0.8;
          P.stock.Value := Random(1000);
          P.manufacturingDate.Value := EncodeDate(2025, 3, 1);
          P.description.Value := 'Inserção unitária individual';
          P.productUuid.Value := Format('7ca7b810-9dad-11d1-80b4-%012d', [I + 1]);
          P.active.Value := True;
          P.created_at.Value := Now;

          TDeltaPersister.Save(Con, P);
        finally
          P.Free;
        end;
      end;

      ElapsedMs := MilliSecondsBetween(Now, StartTime);

      Log(Format('   ✅ %d registros inseridos em %d ms', [Total, ElapsedMs]));

      CountAfter := CountProducts(Con);
      Log(Format('3. Verificação: COUNT(*) = %d', [CountAfter]));

      Log('');
      if ElapsedMs > 0 then
        Log(Format('📊 Performance: %.0f registros/segundo', [Total / (ElapsedMs / 1000)]))
      else
        Log('📊 Performance: instantâneo (< 1ms)');

      Log('');
      Log('💡 Compare este tempo com o BulkInsert para ver a diferença!');

      ShowMessage(Format('Insert individual concluído!%s%d registros em %d ms',
        [sLineBreak, Total, ElapsedMs]));
    except
      on E: Exception do
      begin
        Log('❌ ERRO: ' + E.Message);
        ShowMessage('Erro: ' + E.Message);
      end;
    end;
  finally
    Con := nil;
  end;
end;

end.
