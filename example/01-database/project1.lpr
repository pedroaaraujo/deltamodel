program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, DeltaModel.ORM.Connection, DatabaseURLParser,
  DeltaModel.DataSetConverter, DeltaValidator, DeltaSerialization,
  DeltaModelMessages, DeltaModel.Fields, DeltaAPISchema, DeltaModel.ORM.DML
  { you can add units after this },
  SQLite3Conn, DeltaModel.ORM.Interfaces, DeltaModel.ORM.Types,
DeltaModel.ORM.DDL, DeltaModel.ORM.Schema
  //IBConnection,
  //PQConnection,
  ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

