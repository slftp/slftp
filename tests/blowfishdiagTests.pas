unit blowfishdiagTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestBlowfishDiagnostic = class(TTestCase)
  published
    procedure TestRecordsPayloadBuckets;
  end;

implementation

uses
  blowfishdiag;

procedure TTestBlowfishDiagnostic.TestRecordsPayloadBuckets;
var
  fBefore, fAfter: TBlowfishDiagnosticSnapshot;
begin
  fBefore := GetBlowfishDiagnosticSnapshot(bdmUdpCfbEncrypt);
  RecordBlowfishDiagnosticUse(bdmUdpCfbEncrypt, 16, 0);
  RecordBlowfishDiagnosticUse(bdmUdpCfbEncrypt, 16, 8);
  RecordBlowfishDiagnosticUse(bdmUdpCfbEncrypt, 16, 9);
  RecordBlowfishDiagnosticUse(bdmUdpCfbEncrypt, 16, 65);
  fAfter := GetBlowfishDiagnosticSnapshot(bdmUdpCfbEncrypt);

  CheckEquals(4, fAfter.Calls - fBefore.Calls);
  CheckEquals(1, fAfter.EmptyPayloads - fBefore.EmptyPayloads);
  CheckEquals(1, fAfter.SmallPayloads - fBefore.SmallPayloads);
  CheckEquals(1, fAfter.MediumPayloads - fBefore.MediumPayloads);
  CheckEquals(1, fAfter.LargePayloads - fBefore.LargePayloads);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('blowfishdiag', TTestBlowfishDiagnostic.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestBlowfishDiagnostic);
  {$ENDIF}
end.
