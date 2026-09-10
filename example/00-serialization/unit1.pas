unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  DeltaModel, DeltaModel.Fields, DeltaValidator,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin, Variants, fpjson;

type

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

  { TUser - Modelo principal com nested model e UUID }

  TUser = class(TDeltaModel)
  private
    Fid:        TDFUUIDNull;
    Fname:      TDFStringRequired;
    Femail:     TDFStringRequired;
    Fage:       TDFIntNull;
    Fscore:     TDFDoubleNull;
    Factive:    TDFBooleanRequired;
    Fcreated:   TDFDateTimeNull;
    Faddress:   TAddress;
  published
    property id:      TDFUUIDNull      read Fid      write Fid;
    property name:    TDFStringRequired read Fname    write Fname;
    property email:   TDFStringRequired read Femail   write Femail;
    property age:     TDFIntNull        read Fage     write Fage;
    property score:   TDFDoubleNull     read Fscore   write Fscore;
    property active:  TDFBooleanRequired read Factive write Factive;
    property created: TDFDateTimeNull   read Fcreated write Fcreated;
    property address: TAddress          read Faddress write Faddress;
  public
    procedure Configure; override;
    procedure Validate; override;
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
    edtSerializationEmail: TEdit;
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  pgcBase.ActivePageIndex := 0;
end;

// Serialização: preenche um TUser com dados e exibe o JSON resultante
procedure TForm1.Button1Click(Sender: TObject);
var
  User: TUser;
begin
  User := TUser.Create;
  try
    User.name.Value    := edtSerializationName.Text;
    User.email.Value   := edtSerializationEmail.Text;
    User.age.Value     := edtSerializationAge.Value;
    User.active.Value  := True;
    User.score.Value   := 8.5;
    User.created.Value := Now;

    // Endereço aninhado
    User.address.city.Value    := 'São Paulo';
    User.address.state.Value   := 'SP';
    User.address.zipcode.Value := '01310-100';

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
    edtDeserializationEmail.Text := User.email.Value;
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

// Schema: gera o Swagger/OpenAPI schema do modelo
procedure TForm1.Button4Click(Sender: TObject);
begin
  mmoSchema.Lines.Text := TUser.SwaggerSchema(False);
end;

end.
