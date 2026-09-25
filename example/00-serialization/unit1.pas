unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  DeltaModel, DeltaModel.Fields, DeltaValidator,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin, Variants, fpjson;

type

  { TAddress - Modelo aninhado de endereço }

  { TUserProfile - Modelo 1:1 HasOne }

  TUserProfile = class(TDeltaModel)
  private
    Ftitle: TDFStringNull;
    Fdepartment: TDFStringNull;
  published
    property title: TDFStringNull read Ftitle write Ftitle;
    property department: TDFStringNull read Fdepartment write Fdepartment;
  end;

  { TAddress - Modelo aninhado de endereço }

  TAddress = class(TDeltaModel)
  private
    Fcity:    TDFStringRequired;
    Fstate:   TDFStringRequired;
    Fzipcode: TDFStringNull;
  published
    property city:    TDFStringRequired read Fcity    write Fcity;
    property state:   TDFStringRequired read Fstate   write Fstate;
    property zipcode: TDFStringNull     read Fzipcode write Fzipcode;
  end;

  { TUser - Modelo com suporte a todos os tipos de dados do DeltaModel }

  TUser = class(TDeltaModel)
  private
    Fid:        TDFUUIDNull;
    Fname:      TDFStringRequired;
    Femail:     TDFStringRequired;
    Fage:       TDFIntNull;
    FbigCode:   TDFInt64Null;
    Fscore:     TDFDoubleNull;
    Fsalary:    TDFCurrencyRequired;
    FbirthDate: TDFDateNull;
    FloginTime: TDFTimeNull;
    Factive:    TDFBooleanRequired;
    Fcreated:   TDFDateTimeNull;
    Fbio:       TDFTextNull;
    Faddress:   TAddress;
    Fprofile:   TDFHasOne;
  published
    property id:        TDFUUIDNull        read Fid        write Fid;
    property name:      TDFStringRequired  read Fname      write Fname;
    property email:     TDFStringRequired  read Femail     write Femail;
    property age:       TDFIntNull         read Fage       write Fage;
    property bigCode:   TDFInt64Null       read FbigCode   write FbigCode;
    property score:     TDFDoubleNull      read Fscore     write Fscore;
    property salary:    TDFCurrencyRequired read Fsalary   write Fsalary;
    property birthDate: TDFDateNull        read FbirthDate write FbirthDate;
    property loginTime: TDFTimeNull        read FloginTime write FloginTime;
    property active:    TDFBooleanRequired read Factive    write Factive;
    property created:   TDFDateTimeNull    read Fcreated   write Fcreated;
    property bio:       TDFTextNull        read Fbio       write Fbio;
    property address:   TAddress           read Faddress   write Faddress;
    property profile:   TDFHasOne          read Fprofile   write Fprofile;
  public
    procedure Configure; override;
    procedure Validate; override;
    procedure BeforeDestruction; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtDeserializationAge: TSpinEdit;
    edtValidationAge: TSpinEdit;
    edtSerializationName: TEdit;
    edtDeserializationName: TEdit;
    edtValidationName: TEdit;
    edtValidationEmail: TEdit;
    edtDeserializationEmail: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mmoSerialization: TMemo;
    mmoDeserialization: TMemo;
    mmoSchema: TMemo;
    pgcBase: TPageControl;
    edtSerializationAge: TSpinEdit;
    tbsValidation: TTabSheet;
    tbsSerialize: TTabSheet;
    tbsDeserialization: TTabSheet;
    tbsSchema: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TUser }

procedure TUser.Configure;
begin
  // Define o tamanho máximo do campo email
  Self.email.Size := 200;
  Self.name.Size  := 120;

  Faddress := TAddress.Create;
  Profile.References(TUserProfile);

  // Define campos invisíveis na serialização (ex: campo interno)
  // Self.someInternalField.Visible := False;
end;

procedure TUser.Validate;
var
  VResult: TValid;
begin
  // Chama validação de campos Required (IsNull check)
  inherited Validate;

  Self.Validator.Clear;

  // Validações do nome
  Self.Validator
    .AddField('name', Self.name.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorItemMinLength.Create(2))
    .AddValidator(TValidatorItemMaxLength.Create(120));

  // Validação de email
  Self.Validator
    .AddField('email', Self.email.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorEmail.Create);

  // Validação de salário
  Self.Validator
    .AddField('salary', Self.salary.Value)
    .AddValidator(TValidatorItemMinValue.Create(0));

  // Validação de idade (campo opcional, só valida se preenchido)
  if not Self.age.IsNull then
  begin
    Self.Validator
      .AddField('age', Self.age.Value)
      .AddValidator(TValidatorItemMinValue.Create(0))
      .AddValidator(TValidatorItemMaxValue.Create(150));
  end;

  // Validação de score (campo opcional)
  if not Self.score.IsNull then
  begin
    Self.Validator
      .AddField('score', Self.score.Value)
      .AddValidator(TValidatorItemBetween.Create(0, 10));
  end;

  VResult := Self.Validator.Validate;
  if not VResult.OK then
    raise EDeltaValidation.Create(VResult.Message);
end;

procedure TUser.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Faddress.Free;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  pgcBase.ActivePageIndex := 0;
end;

// Serialização: preenche um TUser com dados de todos os tipos e exibe o JSON resultante
procedure TForm1.Button1Click(Sender: TObject);
var
  User: TUser;
  Prof: TUserProfile;
begin
  User := TUser.Create;
  try
    User.id.Value        := '550e8400-e29b-41d4-a716-446655440000';
    User.name.Value      := edtSerializationName.Text;
    User.email.Value     := 'franz.schubert@exemplo.com';
    User.age.Value       := edtSerializationAge.Value;
    User.bigCode.Value   := 922337203685477580;
    User.score.Value     := 8.5;
    User.salary.Value    := 7850.75;
    User.birthDate.Value := EncodeDate(1995, 6, 15);
    User.loginTime.Value := EncodeTime(14, 30, 0, 0);
    User.active.Value    := True;
    User.created.Value   := Now;
    User.bio.Value       := 'Compositor e entusiasta de sistemas distribuídos de alta performance.';

    // Endereço aninhado
    User.address.city.Value    := 'São Paulo';
    User.address.state.Value   := 'SP';
    User.address.zipcode.Value := '01310-100';

    // Relação 1:1 HasOne
    Prof := TUserProfile.Create;
    Prof.title.Value := 'Tech Lead';
    Prof.department.Value := 'Engenharia de Software';
    User.profile.Model := Prof;

    mmoSerialization.Lines.Text := User.ToJson;
  finally
    User.Free;
  end;
end;

// Desserialização: lê JSON do memo e preenche os campos
procedure TForm1.Button2Click(Sender: TObject);
var
  User: TUser;
begin
  User := TUser.Create;
  try
    User.FromJson(mmoDeserialization.Lines.Text);
    edtDeserializationName.Text  := User.name.Value;
    if not User.age.IsNull then
      edtDeserializationAge.Value := User.age.AsInteger
    else
      edtDeserializationAge.Value := 0;
  finally
    User.Free;
  end;
end;

// Validação: valida os dados e exibe resultado
procedure TForm1.Button3Click(Sender: TObject);
var
  User: TUser;
begin
  User := TUser.Create;
  try
    User.name.Value   := edtValidationName.Text;
    User.email.Value  := edtValidationEmail.Text;
    User.age.Value    := edtValidationAge.Value;
    User.salary.Value := 5000.00;
    User.active.Value := True;

    // Endereço obrigatório para o exemplo
    User.address.city.Value  := 'Campinas';
    User.address.state.Value := 'SP';

    try
      User.Validate;
      ShowMessage('✓ Dados válidos!');
    except
      on E: EDeltaValidation do
        ShowMessageFmt('✗ Erro de validação:%s%s', [sLineBreak, E.Message]);
    end;
  finally
    User.Free;
  end;
end;

// Schema: gera o Swagger/OpenAPI schema do modelo com todos os tipos
procedure TForm1.Button4Click(Sender: TObject);
begin
  mmoSchema.Lines.Text := TUser.SwaggerSchema(False);
end;

end.
