unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Variants,
  DeltaModel, DeltaModel.Fields, DeltaModel.SQLBuilder,
  DeltaModel.ORM.Connection, DeltaModel.ORM.Types, DeltaModel.ORM.Schema,
  DeltaModel.ORM.DDL, DeltaModel.ORM.DML;

type

  { TPerson - Modelo de teste com AutoInc e tipos variados }

  TPerson = class(TDeltaModel)
  private
    Fid: TDFIntNull;
    Fname: TDFStringRequired;
    Fsurname: String;
    Fage: TDFIntRequired;
    FcreditLimit: TDFCurrencyRequired;
    Factive: TDFBooleanRequired;
    Fcreated: TDFDateTimeNull;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property surname: String read Fsurname write Fsurname;
    property age: TDFIntRequired read Fage write Fage;
    property creditLimit: TDFCurrencyRequired read FcreditLimit write FcreditLimit;
    property active: TDFBooleanRequired read Factive write Factive;
    property created: TDFDateTimeNull read Fcreated write Fcreated;
  public
    procedure AfterConstruction; override;
    procedure Configure; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    btnTestCRUD: TButton;
    cmbDialect: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmoDDL: TMemo;
    mmoDDLPreview: TMemo;
    mmoDelete: TMemo;
    mmoInsert: TMemo;
    mmoSelect: TMemo;
    mmoUpdate: TMemo;
    PageControl1: TPageControl;
    pnlTop: TPanel;
    tbsSQL: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button2Click(Sender: TObject);
    procedure btnTestCRUDClick(Sender: TObject);
    procedure cmbDialectChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function CurrentDialect: TDatabaseDialect;
    procedure BuildSQL(ADialect: TDatabaseDialect);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TPerson }

procedure TPerson.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
end;

procedure TPerson.Configure;
begin
  Self.name.Size := 120;
end;

{ TForm1 }

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
    Result := ddPostgreSQL;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  cmbDialect.ItemIndex := 0;
  BuildSQL(CurrentDialect);
end;

procedure TForm1.cmbDialectChange(Sender: TObject);
begin
  BuildSQL(CurrentDialect);
end;

procedure TForm1.BuildSQL(ADialect: TDatabaseDialect);
var
  Person: TPerson;
  Builder: TDMSQLBuilder;
begin
  Person := TPerson.Create;
  try
    // 1. SELECT com paginação (Page 2, tamanho 10)
    Builder := TDMSQLBuilder.Create(Person, ADialect);
    try
      mmoSelect.Lines.Text := Builder
        .Select
        .Where('active = 1')
        .OrderBy('name ASC')
        .Page(2, 10)
        .Build;
    finally
      Builder.Free;
    end;

    // 2. INSERT com Returning / Output
    Person.name.Value := 'Carlos Eduardo';
    Person.surname := 'Silveira';
    Person.age.Value := 35;
    Person.creditLimit.Value := 12500.50;
    Person.active.Value := True;
    Person.created.Value := Now;

    mmoInsert.Lines.Text := TDMSQLBuilder.CreateInsertReturning(
      Person,
      ADialect,
      True
    );

    // 3. UPDATE
    Person.id.Value := 42;
    mmoUpdate.Lines.Text := TDMSQLBuilder.CreateUpdate(
      Person,
      ADialect
    );

    // 4. DELETE
    mmoDelete.Lines.Text := TDMSQLBuilder.CreateDelete(
      Person,
      ADialect
    );

    // 5. DDL gerado para o dialeto
    mmoDDLPreview.Lines.Text := TDDLBuilder.CreateTableAndFields(
      Person,
      nil,
      ADialect
    );
  finally
    Person.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Con: TDeltaORMEngine;
  Schema: TDeltaORMSchema;
begin
  Con := TDeltaORMEngine.Create(Edit1.Text);
  try
    Schema := TDeltaORMSchema.Create(Con);
    try
      try
        Con.Connection.Open;
        mmoDDL.Lines.Add('=== Conectado com sucesso via: ' + Con.Connection.ConnectorType + ' ===');
        mmoDDL.Lines.Add('Dialeto ativo: ' + TDatabaseDialectHelper.ToString(Con.Dialect));

        Schema.RegisterModel(TPerson.Create);
        Schema.PrepareDB(True);

        mmoDDL.Lines.Add('--- DDL Executado ---');
        mmoDDL.Lines.Add(Schema.SQL.Text);
        ShowMessage('Schema preparado com sucesso!');
      finally
        Schema.Free;
      end;
    except
      on E: Exception do
        ShowMessageFmt('Falha ao preparar schema:%s%s', [sLineBreak, E.Message]);
    end;
  finally
    Con.Free;
  end;
end;

procedure TForm1.btnTestCRUDClick(Sender: TObject);
var
  Con: TDeltaORMEngine;
  Schema: TDeltaORMSchema;
  P, FoundP: TPerson;
  List: TDeltaModelList;
  I: Integer;
  Total: Int64;
begin
  mmoDDL.Clear;
  mmoDDL.Lines.Add('=== Iniciando Teste de Operações ORM ===');

  Con := TDeltaORMEngine.Create(Edit1.Text);
  try
    try
      Con.Connection.Open;
      mmoDDL.Lines.Add('1. Conectado ao banco (' + TDatabaseDialectHelper.ToString(Con.Dialect) + ')');

      // 2. Prepara tabela
      Schema := TDeltaORMSchema.Create(Con);
      try
        Schema.RegisterModel(TPerson.Create);
        Schema.PrepareDB(True);
        mmoDDL.Lines.Add('2. Tabela person criada/verificada.');
      finally
        Schema.Free;
      end;

      // 3. Teste do Save (Insert automático porque PK está vazia)
      P := TPerson.Create;
      try
        P.name.Value := 'Ana Beatriz';
        P.surname := 'Lima';
        P.age.Value := 28;
        P.creditLimit.Value := 8000;
        P.active.Value := True;
        P.created.Value := Now;

        if Con.Save(P) then
          mmoDDL.Lines.Add('3. [SAVE - INSERT] Registro inserido com sucesso: ' + P.name.Value);
      finally
        P.Free;
      end;

      // 4. Inserindo segundo registro
      P := TPerson.Create;
      try
        P.name.Value := 'Bruno Martins';
        P.surname := 'Rocha';
        P.age.Value := 40;
        P.creditLimit.Value := 15000;
        P.active.Value := True;
        P.created.Value := Now;

        Con.Insert(P);
        mmoDDL.Lines.Add('4. [INSERT] Segundo registro inserido: ' + P.name.Value);
      finally
        P.Free;
      end;

      // 5. Contagem com Con.Count
      Total := Con.Count(TPerson);
      mmoDDL.Lines.Add(Format('5. [COUNT] Total de pessoas no banco: %d', [Total]));

      // 6. Consulta fluente com Query e paginação
      List := Con.Query(TPerson)
        .OrderBy('name ASC')
        .Page(1, 10)
        .All;
      try
        mmoDDL.Lines.Add('6. [QUERY.ALL] Registros retornados pela consulta:');
        for I := 0 to Pred(List.Records.Count) do
        begin
          FoundP := List.Records[I] as TPerson;
          mmoDDL.Lines.Add(Format('   [%d] Nome: %s, Idade: %d, Limite: R$ %.2f',
            [I + 1, FoundP.name.Value, FoundP.age.AsInteger, FoundP.creditLimit.AsCurrency]));
        end;
      finally
        List.Free;
      end;

      ShowMessage('Teste de CRUD executado com sucesso! Veja os logs no memo.');
    except
      on E: Exception do
      begin
        mmoDDL.Lines.Add('ERRO: ' + E.Message);
        ShowMessageFmt('Erro durante execução do CRUD:%s%s', [sLineBreak, E.Message]);
      end;
    end;
  finally
    Con.Free;
  end;
end;

end.
