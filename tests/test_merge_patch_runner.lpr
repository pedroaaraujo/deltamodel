{
  Standalone FPCUnit Test Runner & Benchmark for RFC 7396 JSON Merge Patch.
  Compile into /tmp to keep repository clean:
    fpc -FE/tmp -FU/tmp -Fu../src -Fu../../ -Fu./src test_merge_patch_runner.lpr
}

program test_merge_patch_runner;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  cwstring,
  {$ENDIF}
  Classes, SysUtils, fpcunit, testregistry, consoletestrunner,
  test_deltamodel_merge_patch;

type
  TMergePatchTestRunner = class(TTestRunner)
  protected
    procedure WriteCustomHelp; override;
  end;

procedure TMergePatchTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  WriteLn('  --benchmark    Run RFC 7396 PATCH vs Full PUT performance benchmark');
end;

var
  App: TMergePatchTestRunner;
  Metrics: TBenchmarkMetrics;
begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;

  App := TMergePatchTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'RFC 7396 JSON Merge Patch Test Suite';
    App.Run;

    WriteLn('');
    WriteLn('=== RFC 7396 PATCH vs Full PUT Benchmark (20-field TProduct) ===');
    Metrics := RunMergePatchBenchmark(10000);
    WriteLn(Format('Iterations:          %d', [Metrics.Iterations]));
    WriteLn(Format('PATCH Elapsed:       %.1f ms (%.0f ops/sec)', [Metrics.PatchElapsedMs, Metrics.PatchOpsPerSec]));
    WriteLn(Format('Full PUT Elapsed:    %.1f ms (%.0f ops/sec)', [Metrics.PutElapsedMs, Metrics.PutOpsPerSec]));
    WriteLn(Format('Speedup Factor:      %.2fx faster', [Metrics.SpeedupFactor]));
    WriteLn(Format('PATCH SQL Size:      %d bytes', [Metrics.PatchSQLBytes]));
    WriteLn(Format('Full PUT SQL Size:   %d bytes', [Metrics.PutSQLBytes]));
    WriteLn(Format('SQL Size Reduction:  %.1f%%', [Metrics.SQLSizeReductionPct]));
    WriteLn(Format('PATCH Heap Delta:    %d bytes', [Metrics.PatchHeapDeltaBytes]));
    WriteLn(Format('Full PUT Heap Delta: %d bytes', [Metrics.PutHeapDeltaBytes]));
  finally
    App.Free;
  end;
end.
