program test_domains_runner;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cwstring,
  {$ENDIF}
  Classes, SysUtils, consoletestrunner,
  test_deltamodel_domains;

type
  { TDomainsTestRunner }
  TDomainsTestRunner = class(TTestRunner)
  protected
    procedure WriteCustomHelp; override;
  end;

procedure TDomainsTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  WriteLn('Standalone FPCUnit runner for DeltaModel.Domains value objects.');
end;

var
  Application: TDomainsTestRunner;
begin
  Application := TDomainsTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'DeltaModel Domains FPCUnit Runner';
    Application.Run;
  finally
    Application.Free;
  end;
end.
