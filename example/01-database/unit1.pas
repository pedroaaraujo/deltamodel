unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DeltaModel, DeltaModel.Fields, DeltaModel.SQLBuilder, Variants,
  DeltaModel.ORM.Connection, DeltaModel.ORM.Types, DeltaModel.ORM.Schema;

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
    property creditLimit: TDFCurrencyRequired read Flimit write Flimit;
  public
    procedure AfterConstruction; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mmoDDL: TMemo;
    mmoDelete: TMemo;
    mmoInsert: TMemo;
    mmoSelect: TMemo;
    mmoUpdate: TMemo;
    PageControl1: TPageControl;
    tbsSQL: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
var
  Con: TDeltaORMEngine;
  Schema: TDeltaORMSchema;
begin
  Con := TDeltaORMEngine.Create(Edit1.Text);
  Schema := TDeltaORMSchema.Create(Con);
  try
    try
      Con.Connection.Open;
      ShowMessage('Connected');


      Schema.RegisterModel(TPerson.Create);
      Schema.PrepareDB(True);
      mmoDDL.Lines.Text := Schema.SQL.Text;
    finally
      Schema.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessageFmt('Failed.%s%s', [sLineBreak, E.Message]) ;
    end;
  end;
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
    Person.creditLimit.Value := 39.9999;
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

