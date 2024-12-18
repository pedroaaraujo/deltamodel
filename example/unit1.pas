unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  DeltaModel,

  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Spin;

type

  { TUser }

  TUser = class(TDeltaModel)
  private
    Fage: Integer;
    Fname: string;
  published
    property name: string read Fname write Fname;
    property age: Integer read Fage write Fage;
  end;


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtDeserializationAge: TSpinEdit;
    edtSerializationName: TEdit;
    edtDeserializationName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmoSerialization: TMemo;
    mmoDeserialization: TMemo;
    pgcBase: TPageControl;
    edtSerializationAge: TSpinEdit;
    tbsValidation: TTabSheet;
    tbsSerialize: TTabSheet;
    tbsDeserialization: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  pgcBase.ActivePageIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  User: TUser;
begin
  User := TUser.Create;
  try
    User.name := edtSerializationName.Text;
    User.age  := edtSerializationAge.Value;
    mmoSerialization.Lines.Text := User.ToJson;
  finally
    User.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  User: TUser;
begin
  User := TUser.Create;
  try
    User.FromJson(mmoDeserialization.Lines.Text);
    edtDeserializationName.Text := User.name;
    edtDeserializationAge.Value := User.age;
  finally
    User.Free;
  end;
end;

end.

