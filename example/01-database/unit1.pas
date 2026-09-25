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

  { TProfile - Modelo 1:1 dependente com ForeignKey }

  TProfile = class(TDeltaModel)
  private
    Fid: TDFIntNull;
    FpersonId: TDFForeignKey;
    Fbio: TDFStringNull;
  published
    property id: TDFIntNull read Fid write Fid;
    property personId: TDFForeignKey read FpersonId write FpersonId;
    property bio: TDFStringNull read Fbio write Fbio;
  public
    procedure AfterConstruction; override;
    procedure Configure; override;
  end;

  { TPerson - Modelo com suporte a todos os tipos de dados do DeltaModel }

  TPerson = class(TDeltaModel)
  private
    Fid: TDFIntNull;
    Fname: TDFStringRequired;
    Fsurname: TDFStringNull;
    Fage: TDFIntRequired;
    FbigCode: TDFInt64Null;
    Fscore: TDFDoubleNull;
    FcreditLimit: TDFCurrencyRequired;
    Fbiography: TDFTextNull;
    FbirthDate: TDFDateNull;
    FshiftTime: TDFTimeNull;
    Fcreated: TDFDateTimeNull;
    Factive: TDFBooleanRequired;
    FexternalUuid: TDFUUIDNull;
    Fprofile: TDFHasOne;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property surname: TDFStringNull read Fsurname write Fsurname;
    property age: TDFIntRequired read Fage write Fage;
    property bigCode: TDFInt64Null read FbigCode write FbigCode;
    property score: TDFDoubleNull read Fscore write Fscore;
    property creditLimit: TDFCurrencyRequired read FcreditLimit write FcreditLimit;
    property biography: TDFTextNull read Fbiography write Fbiography;
    property birthDate: TDFDateNull read FbirthDate write FbirthDate;
    property shiftTime: TDFTimeNull read FshiftTime write FshiftTime;
    property created: TDFDateTimeNull read Fcreated write Fcreated;
    property active: TDFBooleanRequired read Factive write Factive;
    property externalUuid: TDFUUIDNull read FexternalUuid write FexternalUuid;
    property profile: TDFHasOne read Fprofile write Fprofile;
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

{ TProfile }

procedure TProfile.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.bio.Size := 255;
end;

procedure TProfile.Configure;
begin
  inherited Configure;
  personId.References(TPerson, 'id');
end;

{ TPerson }

procedure TPerson.AfterConstruction;
begin
  inherited AfterConstruction;
  Self.id.DBOptions := [dboPrimaryKey, dboAutoInc];
  Self.name.Size := 120;
  Self.surname.Size := 120;
  // Campo com índice secundário direto
  Self.active.IsIndexed := True;
end;

procedure TPerson.Configure;
begin
  inherited Configure;
  // Constraint UNIQUE composta no nível de tabela
  AddUniqueConstraint('uq_person_name_surname', ['name', 'surname']);
  // Índice de performance composto
  AddIndex('ix_person_created_age', ['created', 'age']);
  // Relacionamento 1:1 HasOne
  Profile.References(TProfile, 'personId', 'id');
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
  ConstraintsList, IndexesList: TStringList;
  DDLText: string;
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
    Person.surname.Value := 'Silveira';
    Person.age.Value := 35;
    Person.bigCode.Value := 9876543210123;
    Person.score.Value := 9.75;
    Person.creditLimit.Value := 12500.50;
    Person.biography.Value := 'Engenheiro sênior com vasta experiência em Pascal e Bancos de Dados.';
    Person.birthDate.Value := EncodeDate(1989, 5, 20);
    Person.shiftTime.Value := EncodeTime(8, 30, 0, 0);
    Person.active.Value := True;
    Person.created.Value := Now;
    Person.externalUuid.Value := 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11';

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
    Person.id.Value := 42;
    mmoDelete.Lines.Text := TDMSQLBuilder.CreateDelete(
      Person,
      ADialect
    );

    // 5. DDL gerado para o dialeto (Tabela, Constraints e Índices)
    ConstraintsList := TStringList.Create;
    IndexesList := TStringList.Create;
    try
      DDLText := TDDLBuilder.CreateTableAndFields(
        Person,
        ConstraintsList,
        ADialect
      );
      if ConstraintsList.Count > 0 then
        DDLText := DDLText + sLineBreak + ConstraintsList.Text;

      TDDLBuilder.GetIndexes(Person, ADialect, IndexesList);
      if IndexesList.Count > 0 then
        DDLText := DDLText + sLineBreak + IndexesList.Text;

      mmoDDLPreview.Lines.Text := DDLText;
    finally
      ConstraintsList.Free;
      IndexesList.Free;
    end;
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
        Schema.RegisterModel(TProfile.Create);
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
  Prof: TProfile;
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

      // 2. Prepara tabelas
      Schema := TDeltaORMSchema.Create(Con);
      try
        Schema.RegisterModel(TPerson.Create);
        Schema.RegisterModel(TProfile.Create);
        Schema.PrepareDB(True);
        mmoDDL.Lines.Add('2. Tabelas person e profile criadas/verificadas.');
      finally
        Schema.Free;
      end;

      // 3. Teste do Save (Insert automático porque PK está vazia)
      P := TPerson.Create;
      try
        P.name.Value := 'Ana Beatriz';
        P.surname.Value := 'Lima';
        P.age.Value := 28;
        P.bigCode.Value := 12345678901234;
        P.score.Value := 9.9;
        P.creditLimit.Value := 8000;
        P.biography.Value := 'Arquiteta de Soluções Cloud.';
        P.birthDate.Value := EncodeDate(1996, 3, 15);
        P.shiftTime.Value := EncodeTime(9, 0, 0, 0);
        P.active.Value := True;
        P.created.Value := Now;
        P.externalUuid.Value := '550e8400-e29b-41d4-a716-446655440000';

        if Con.Save(P) then
        begin
          mmoDDL.Lines.Add('3. [SAVE - INSERT] Registro inserido com sucesso: ' + P.name.Value + ' (Id: ' + P.id.AsString + ')');

          // Insere Profile associado
          Prof := TProfile.Create;
          try
            Prof.personId.Value := P.id.Value;
            Prof.bio.Value := 'Especialista em Microsserviços e Banco de Dados';
            Con.Save(Prof);
            mmoDDL.Lines.Add('   [1:1 HAS_ONE / FK] Profile criado para pessoa ' + P.id.AsString);
          finally
            Prof.Free;
          end;
        end;
      finally
        P.Free;
      end;

      // 4. Inserindo segundo registro
      P := TPerson.Create;
      try
        P.name.Value := 'Bruno Martins';
        P.surname.Value := 'Rocha';
        P.age.Value := 40;
        P.bigCode.Value := 98765432109876;
        P.score.Value := 8.4;
        P.creditLimit.Value := 15000;
        P.biography.Value := 'Tech Lead e Desenvolvedor Pascal.';
        P.birthDate.Value := EncodeDate(1984, 11, 2);
        P.shiftTime.Value := EncodeTime(10, 0, 0, 0);
        P.active.Value := True;
        P.created.Value := Now;
        P.externalUuid.Value := 'e7b0b230-6b3a-4f51-b84a-9ef8d5f308a2';

        Con.Insert(P);
        mmoDDL.Lines.Add('4. [INSERT] Segundo registro inserido: ' + P.name.Value + ' (Id: ' + P.id.AsString + ')');
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
        mmoDDL.Lines.Add('6. [QUERY.ALL] Registros retornados com todos os tipos:');
        for I := 0 to Pred(List.Records.Count) do
        begin
          FoundP := List.Records[I] as TPerson;
          mmoDDL.Lines.Add(Format('   [%d] Nome: %s %s | Idade: %d | BigCode: %s | Score: %.2f | Limite: R$ %.2f | Nasc: %s | UUID: %s',
            [I + 1, FoundP.name.Value, FoundP.surname.AsString, FoundP.age.AsInteger,
             FoundP.bigCode.AsString, FoundP.score.AsFloat, FoundP.creditLimit.AsCurrency,
             FoundP.birthDate.AsString, FoundP.externalUuid.AsString]));
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
