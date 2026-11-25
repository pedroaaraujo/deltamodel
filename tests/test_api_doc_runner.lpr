{
  Standalone FPCUnit runner for TDeltaModel.DescribeApiSchema (OpenAPI docs).
  Compile into /tmp to keep repository clean:
    fpc -FE/tmp -FU/tmp -Fu../src -Fu./src test_api_doc_runner.lpr
}

program test_api_doc_runner;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cwstring,
  {$ENDIF}
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  test_deltamodel_api_doc;

var
  App: TTestRunner;
begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;

  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Run;
  finally
    App.Free;
  end;
end.
