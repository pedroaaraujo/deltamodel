unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.SQLBuilder,
  DeltaModel.ORM.Connection, DeltaModel.ORM.Types, DeltaModel.ORM.Schema,
  DeltaModel.ORM.DDL, DeltaModel.ORM.DML;

type

  { TProduct - Modelo de exemplo para demonstrar Bulk Insert }

  TProduct = class(TDeltaModel)
  private
    Fid: TDFIntNull;
    Fname: TDFStringRequired;
    Fsku: TDFStringRequired;
    Fprice: TDFCurrencyRequired;
    Fstock: TDFIntRequired;
    Factive: TDFBooleanRequired;
    Fcreated_at: TDFDateTimeNull;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property sku: TDFStringRequired read Fsku write Fsku;
    property price: TDFCurrencyRequired read Fprice write Fprice;
    property stock: TDFIntRequired read Fstock write Fstock;
    property active: TDFBooleanRequired read Factive write Factive;
    property created_at: TDFDateTimeNull read Fcreated_at write Fcreated_at;
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
    procedure PrepareSchema(ACon: TDeltaORMEngine);
    function CurrentDialect: TDatabaseDialect;
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
  cmbDialect.ItemIndex := 2; // SQLite por padrão
end;

function TForm1.CurrentDialect: TDatabaseDialect;
begin
  case cmbDialect.ItemIndex of
    0: Result := ddPostgreSQL;
    1: Result := ddMySQL;
    2: Result := ddSQLite;
    3: Result := ddFirebird;
    4: Result := ddMSSQL;
    5: Result := ddOracle;
  else
    Result := ddSQLite;
  end;
end;

procedure TForm1.Log(const AMsg: string);
begin
  mmoLog.Lines.Add(AMsg);
  Application.ProcessMessages;
end;

procedure TForm1.PrepareSchema(ACon: TDeltaORMEngine);
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

{ === Pré-visualização do SQL Bulk Insert para o dialeto selecionado === }

procedure TForm1.btnPreviewSQLClick(Sender: TObject);
var
  Models: array of TDeltaModel;
  I, PreviewCount: Integer;
  Dialect: TDatabaseDialect;
begin
  mmoSQL.Clear;
  Dialect := CurrentDialect;

  // Gera preview com 3 registros para ficar legível
  PreviewCount := 3;
  SetLength(Models, PreviewCount);
  for I := 0 to PreviewCount - 1 do
  begin
    Models[I] := TProduct.Create;
    TProduct(Models[I]).name.Value := 'Produto ' + IntToStr(I + 1);
    TProduct(Models[I]).sku.Value := 'SKU-' + IntToStr(I + 1);
    TProduct(Models[I]).price.Value := 49.90 + I * 10;
    TProduct(Models[I]).stock.Value := 100 + I;
    TProduct(Models[I]).active.Value := True;
    TProduct(Models[I]).created_at.Value := Now;
  end;

  try
    mmoSQL.Lines.Text := TDMSQLBuilder.CreateBulkInsert(Models, Dialect);
  finally
    for I := 0 to PreviewCount - 1 do
      Models[I].Free;
  end;
end;

{ === Bulk Insert com array aberto (open array) === }

procedure TForm1.btnBulkArrayClick(Sender: TObject);
var
  Con: TDeltaORMEngine;
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

  Con := TDeltaORMEngine.Create(edtURL.Text);
  try
    try
      Con.Connection.Open;
      Log('1. Conectado (' + TDatabaseDialectHelper.ToString(Con.Dialect) + ')');

      PrepareSchema(Con);

      // Limpa dados anteriores para comparação justa
      try
        Con.ExecuteDirect('DELETE FROM products');
      except
        // ignora se tabela vazia
      end;

      // 2. Monta o array de modelos
      Log(Format('2. Criando %d objetos TProduct...', [Total]));
      SetLength(Models, Total);
      for I := 0 to Total - 1 do
      begin
        Models[I] := TProduct.Create;
        TProduct(Models[I]).name.Value := Format('Produto %d', [I + 1]);
        TProduct(Models[I]).sku.Value := Format('SKU-%0.6d', [I + 1]);
        TProduct(Models[I]).price.Value := 10.00 + Random(99000) / 100;
        TProduct(Models[I]).stock.Value := Random(1000);
        TProduct(Models[I]).active.Value := (Random(10) > 1); // 90% ativos
      end;

      // 3. Executa BulkInsert com medição de tempo
      Log(Format('3. Executando BulkInsert (batch=%d)...', [spnBatchSize.Value]));
      StartTime := Now;

      RowsInserted := Con.BulkInsert(Models, spnBatchSize.Value);

      ElapsedMs := MilliSecondsBetween(Now, StartTime);

      Log(Format('   ✅ %d registros inseridos em %d ms', [RowsInserted, ElapsedMs]));

      // 4. Contagem de verificação
      CountAfter := Con.Count(TProduct);
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
    Con.Free;
  end;
end;

{ === Bulk Insert com TDeltaModelList === }

procedure TForm1.btnBulkListClick(Sender: TObject);
var
  Con: TDeltaORMEngine;
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

  Con := TDeltaORMEngine.Create(edtURL.Text);
  try
    try
      Con.Connection.Open;
      Log('1. Conectado (' + TDatabaseDialectHelper.ToString(Con.Dialect) + ')');

      PrepareSchema(Con);

      // Limpa dados anteriores
      try
        Con.ExecuteDirect('DELETE FROM products');
      except
      end;

      // 2. Monta a lista de modelos
      Log(Format('2. Criando lista com %d objetos TProduct...', [Total]));
      List := TDeltaModelList.Create;
      try
        List.SetDeltaModelClass(TProduct);
        for I := 0 to Total - 1 do
        begin
          P := TProduct.Create;
          P.name.Value := Format('Item %d', [I + 1]);
          P.sku.Value := Format('LIST-%0.6d', [I + 1]);
          P.price.Value := 5.00 + Random(50000) / 100;
          P.stock.Value := Random(500);
          P.active.Value := True;
          List.Add(P);
        end;

        // 3. Executa BulkInsert via List
        Log(Format('3. Executando BulkInsert(List, %d)...', [spnBatchSize.Value]));
        StartTime := Now;

        RowsInserted := Con.BulkInsert(List, spnBatchSize.Value);

        ElapsedMs := MilliSecondsBetween(Now, StartTime);

        Log(Format('   ✅ %d registros inseridos em %d ms', [RowsInserted, ElapsedMs]));

        // 4. Contagem e amostra
        CountAfter := Con.Count(TProduct);
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
    Con.Free;
  end;
end;

{ === Comparação: Insert um-a-um (Save) para benchmark === }

procedure TForm1.btnSaveOneByOneClick(Sender: TObject);
var
  Con: TDeltaORMEngine;
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

  Con := TDeltaORMEngine.Create(edtURL.Text);
  try
    try
      Con.Connection.Open;
      Log('1. Conectado (' + TDatabaseDialectHelper.ToString(Con.Dialect) + ')');

      PrepareSchema(Con);

      // Limpa dados anteriores
      try
        Con.ExecuteDirect('DELETE FROM products');
      except
      end;

      // 2. Insere um-a-um com medição
      Log(Format('2. Inserindo %d registros com Save() individual...', [Total]));
      StartTime := Now;

      for I := 0 to Total - 1 do
      begin
        P := TProduct.Create;
        try
          P.name.Value := Format('Unitário %d', [I + 1]);
          P.sku.Value := Format('UNIT-%0.6d', [I + 1]);
          P.price.Value := 10.00 + Random(99000) / 100;
          P.stock.Value := Random(1000);
          P.active.Value := True;

          Con.Save(P);
        finally
          P.Free;
        end;
      end;

      ElapsedMs := MilliSecondsBetween(Now, StartTime);

      Log(Format('   ✅ %d registros inseridos em %d ms', [Total, ElapsedMs]));

      CountAfter := Con.Count(TProduct);
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
    Con.Free;
  end;
end;

end.
