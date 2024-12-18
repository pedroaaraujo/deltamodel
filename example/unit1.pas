unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    pgcBase: TPageControl;
    tbsValidation: TTabSheet;
    tbsSerialize: TTabSheet;
    tbsDeserialization: TTabSheet;
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

end.

