unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  DeltaModel, DeltaModel.Fields, DeltaValidator,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Variants, fpjson;

type

  { TCompany - Modelo com CPF/CNPJ, URL e lista de funcionários }

  TEmployee = class(TDeltaModel)
  private
    Fname:             TDFStringRequired;
    Femail:            TDFStringRequired;
    Fcpf:              TDFStringRequired;
    Fsalary:           TDFCurrencyRequired;
    Factive:           TDFBooleanRequired;
    Fage:              TDFIntNull;
    FregistrationCode: TDFInt64Null;
    FperformanceScore: TDFDoubleNull;
    FbirthDate:        TDFDateNull;
    FadmissionTime:    TDFTimeNull;
    FregisteredAt:     TDFDateTimeNull;
    Fnotes:            TDFTextNull;
    FbadgeUuid:        TDFUUIDNull;
  published
    property name:             TDFStringRequired   read Fname             write Fname;
    property email:            TDFStringRequired   read Femail            write Femail;
    property cpf:              TDFStringRequired   read Fcpf              write Fcpf;
    property salary:           TDFCurrencyRequired read Fsalary           write Fsalary;
    property active:           TDFBooleanRequired  read Factive           write Factive;
    property age:              TDFIntNull          read Fage              write Fage;
    property registrationCode: TDFInt64Null        read FregistrationCode write FregistrationCode;
    property performanceScore: TDFDoubleNull       read FperformanceScore write FperformanceScore;
    property birthDate:        TDFDateNull         read FbirthDate        write FbirthDate;
    property admissionTime:    TDFTimeNull         read FadmissionTime    write FadmissionTime;
    property registeredAt:     TDFDateTimeNull     read FregisteredAt     write FregisteredAt;
    property notes:            TDFTextNull         read Fnotes            write Fnotes;
    property badgeUuid:        TDFUUIDNull         read FbadgeUuid        write FbadgeUuid;
  public
    procedure Validate; override;
  end;

  TEmployeeList = class(TDeltaModelList);

  TCompany = class(TDeltaModel)
  private
    Fname:      TDFStringRequired;
    Fcnpj:      TDFStringRequired;
    Fwebsite:   TDFStringNull;
    Femployees: TEmployeeList;
  published
    property name:      TDFStringRequired read Fname      write Fname;
    property cnpj:      TDFStringRequired read Fcnpj      write Fcnpj;
    property website:   TDFStringNull     read Fwebsite   write Fwebsite;
    property employees: TEmployeeList     read Femployees write Femployees;
  public
    procedure Configure; override;
    procedure Validate; override;
    procedure BeforeDestruction; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnValidateEmployee: TButton;
    btnValidateCompany:  TButton;
    btnSerializeList:    TButton;
    btnDeserializeList:  TButton;
    edtEmployeeName:     TEdit;
    edtEmployeeEmail:    TEdit;
    edtEmployeeCPF:      TEdit;
    edtCompanyName:      TEdit;
    edtCompanyCNPJ:      TEdit;
    edtCompanyWebsite:   TEdit;
    lblEmpName:   TLabel;
    lblEmpEmail:  TLabel;
    lblEmpCPF:    TLabel;
    lblCoName:    TLabel;
    lblCoCNPJ:    TLabel;
    lblCoWebsite: TLabel;
    mmoJson:     TMemo;
    pgcBase:     TPageControl;
    tbsEmployee: TTabSheet;
    tbsCompany:  TTabSheet;
    tbsJson:     TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure btnValidateEmployeeClick(Sender: TObject);
    procedure btnValidateCompanyClick(Sender: TObject);
    procedure btnSerializeListClick(Sender: TObject);
    procedure btnDeserializeListClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TEmployee }

procedure TEmployee.Validate;
var
  VResult: TValid;
begin
  inherited Validate;

  Self.Validator.Clear;

  Self.Validator
    .AddField('name', Self.name.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorItemMinLength.Create(2))
    .AddValidator(TValidatorItemMaxLength.Create(100));

  Self.Validator
    .AddField('email', Self.email.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorEmail.Create);

  Self.Validator
    .AddField('cpf', Self.cpf.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorItemCPF.Create);

  Self.Validator
    .AddField('salary', Self.salary.Value)
    .AddValidator(TValidatorItemGreaterThanZero.Create)
    .AddValidator(TValidatorItemMaxValue.Create(999999.99));

  if not Self.age.IsNull then
  begin
    Self.Validator
      .AddField('age', Self.age.Value)
      .AddValidator(TValidatorItemBetween.Create(18, 120));
  end;

  if not Self.performanceScore.IsNull then
  begin
    Self.Validator
      .AddField('performanceScore', Self.performanceScore.Value)
      .AddValidator(TValidatorItemBetween.Create(0, 100));
  end;

  VResult := Self.Validator.Validate;
  if not VResult.OK then
    raise EDeltaValidation.Create(VResult.Message);
end;

{ TCompany }

procedure TCompany.Configure;
begin
  Self.name.Size    := 200;
  Self.website.Size := 500;
  Self.cnpj.Size    := 14; // só dígitos

  if Self.employees = nil then
    Self.employees := TEmployeeList.Create;

  Self.employees.SetDeltaModelClass(TEmployee);
end;

procedure TCompany.Validate;
var
  VResult: TValid;
  I: Integer;
begin
  inherited Validate;

  Self.Validator.Clear;

  Self.Validator
    .AddField('name', Self.name.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorItemMinLength.Create(2));

  Self.Validator
    .AddField('cnpj', Self.cnpj.Value)
    .AddValidator(TValidatorItemNotEmpty.Create)
    .AddValidator(TValidatorItemCNPJ.Create);

  if not Self.website.IsNull then
  begin
    Self.Validator
      .AddField('website', Self.website.Value)
      .AddValidator(TValidatorItemUrl.Create);
  end;

  VResult := Self.Validator.Validate;
  if not VResult.OK then
    raise EDeltaValidation.Create(VResult.Message);

  // Valida cada funcionário também
  for I := 0 to Pred(Self.employees.Records.Count) do
    (Self.employees.Records[I] as TEmployee).Validate;
end;

procedure TCompany.BeforeDestruction;
begin
  FEmployees.Free;
  inherited;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  pgcBase.ActivePageIndex := 0;

  // Preenche campos de exemplo
  edtEmployeeName.Text  := 'João da Silva';
  edtEmployeeEmail.Text := 'joao@exemplo.com';
  edtEmployeeCPF.Text   := '529.982.247-25'; // CPF válido para teste

  edtCompanyName.Text    := 'Acme Corp';
  edtCompanyCNPJ.Text    := '11.222.333/0001-81'; // CNPJ para teste
  edtCompanyWebsite.Text := 'https://www.acme.com';
end;

procedure TForm1.btnValidateEmployeeClick(Sender: TObject);
var
  Emp: TEmployee;
begin
  Emp := TEmployee.Create;
  try
    Emp.name.Value             := edtEmployeeName.Text;
    Emp.email.Value            := edtEmployeeEmail.Text;
    Emp.cpf.Value              := edtEmployeeCPF.Text;
    Emp.salary.Value           := 5000.00;
    Emp.active.Value           := True;
    Emp.age.Value              := 32;
    Emp.registrationCode.Value := 1029384756;
    Emp.performanceScore.Value := 96.5;
    Emp.birthDate.Value        := EncodeDate(1992, 8, 14);
    Emp.admissionTime.Value    := EncodeTime(8, 0, 0, 0);
    Emp.registeredAt.Value     := Now;
    Emp.notes.Value            := 'Colaborador com certificações em banco de dados e arquitetura.';
    Emp.badgeUuid.Value        := '550e8400-e29b-41d4-a716-446655440000';
    try
      Emp.Validate;
      ShowMessage('✓ Funcionário com todos os tipos validado com sucesso!');
    except
      on E: EDeltaValidation do
        ShowMessageFmt('✗ Erro:%s%s', [sLineBreak, E.Message]);
    end;
  finally
    Emp.Free;
  end;
end;

procedure TForm1.btnValidateCompanyClick(Sender: TObject);
var
  Co: TCompany;
  Emp: TEmployee;
begin
  Co := TCompany.Create;
  try
    Co.name.Value    := edtCompanyName.Text;
    Co.cnpj.Value    := edtCompanyCNPJ.Text;
    if edtCompanyWebsite.Text <> '' then
      Co.website.Value := edtCompanyWebsite.Text;

    // Adiciona funcionário com todos os tipos de exemplo
    Emp := TEmployee.Create;
    Emp.name.Value             := edtEmployeeName.Text;
    Emp.email.Value            := edtEmployeeEmail.Text;
    Emp.cpf.Value              := edtEmployeeCPF.Text;
    Emp.salary.Value           := 5000;
    Emp.active.Value           := True;
    Emp.age.Value              := 29;
    Emp.registrationCode.Value := 987654321;
    Emp.performanceScore.Value := 91.0;
    Emp.birthDate.Value        := EncodeDate(1995, 2, 28);
    Emp.admissionTime.Value    := EncodeTime(9, 30, 0, 0);
    Emp.registeredAt.Value     := Now;
    Emp.notes.Value            := 'Engenheiro full-stack sênior.';
    Emp.badgeUuid.Value        := 'a1b2c3d4-e5f6-7890-abcd-ef1234567890';
    Co.employees.Records.Add(Emp);

    try
      Co.Validate;
      ShowMessage('✓ Empresa e funcionários válidos!');
    except
      on E: EDeltaValidation do
        ShowMessageFmt('✗ Erro:%s%s', [sLineBreak, E.Message]);
    end;
  finally
    Co.Free;
  end;
end;

procedure TForm1.btnSerializeListClick(Sender: TObject);
var
  Co: TCompany;
  Emp: TEmployee;
begin
  Co := TCompany.Create;
  try
    Co.name.Value    := 'Tech Solutions Ltda';
    Co.cnpj.Value    := '11222333000181';
    Co.website.Value := 'https://techsolutions.com.br';

    Emp := TEmployee.Create;
    Emp.name.Value   := 'Maria Oliveira';
    Emp.email.Value  := 'maria@techsolutions.com.br';
    Emp.cpf.Value    := '52998224725';
    Emp.salary.Value := 8500;
    Emp.active.Value := True;
    Co.employees.Records.Add(Emp);

    Emp := TEmployee.Create;
    Emp.name.Value   := 'Carlos Souza';
    Emp.email.Value  := 'carlos@techsolutions.com.br';
    Emp.cpf.Value    := '52998224725';
    Emp.salary.Value := 12000;
    Emp.active.Value := True;
    Co.employees.Records.Add(Emp);

    pgcBase.ActivePageIndex := 2;
    mmoJson.Lines.Text := Co.ToJson;
  finally
    Co.Free;
  end;
end;

procedure TForm1.btnDeserializeListClick(Sender: TObject);
var
  Co: TCompany;
begin
  if mmoJson.Lines.Text.Trim.IsEmpty then
  begin
    ShowMessage('Cole um JSON no campo de texto primeiro.');
    Exit;
  end;

  Co := TCompany.Create;
  try
    Co.FromJson(mmoJson.Lines.Text);
    ShowMessageFmt(
      'Empresa: %s%sCNPJ: %s%sFuncionários: %d',
      [
        Co.name.Value,
        sLineBreak,
        Co.cnpj.Value,
        sLineBreak,
        Co.employees.Records.Count
      ]
    );
  finally
    Co.Free;
  end;
end;

end.
