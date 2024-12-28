unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DeltaModel, DeltaModel.Fields, DeltaModel.SQLBuilder, Variants, DeltaModel.ORM.Connection;

type

  { TPerson }

  TPerson = class(TDeltaModel)
  private
    Fage: TDFIntRequired;
    Fid: TDFIntNull;
    Flimit: TDFCurrencyRequired;
    Fname: TDFStringRequired;
  published
    property id: TDFIntNull read Fid write Fid;
    property name: TDFStringRequired read Fname write Fname;
    property age: TDFIntRequired read Fage write Fage;
    property limit: TDFCurrencyRequired read Flimit write Flimit;
  public
    procedure AfterConstruction; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mmoDBParams: TMemo;
    mmoDelete: TMemo;
    mmoInsert: TMemo;
    mmoSelect: TMemo;
    mmoUpdate: TMemo;
    PageControl1: TPageControl;
    tbsSQL: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure BuildSQL;
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
  Self.id.DBOptions := [dboPrimaryKey];
end;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  BuildSQL;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Con: TDeltaORMEngine;
begin
  Con := TDeltaORMEngine.Create(Edit1.Text);
  mmoDBParams.Lines.Assign(Con.Connection.Params);
end;

procedure TForm1.BuildSQL;
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    mmoSelect.Lines.Text := TDMSQLBuilder.CreateSelect(
      Person,
      ddFirebird,
      'id = 1'
    );

    Person.name.Value := 'No '' Name';
    Person.age.Value := 30;
    Person.limit.Value := 39.9999;
    mmoInsert.Lines.Text := TDMSQLBuilder.CreateInsert(
      Person,
      ddFirebird
    );

    Person.name.Value := 'New Name';
    Person.id.Value := 1;
    mmoUpdate.Lines.Text := TDMSQLBuilder.CreateUpdate(
      Person,
      ddFirebird
    );

    mmoDelete.Lines.Text := TDMSQLBuilder.CreateDelete(
      Person,
      ddFirebird
    );
  finally
    Person.Free;
  end;
end;

end.

