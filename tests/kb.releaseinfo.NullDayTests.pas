unit kb.releaseinfo.NullDayTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestT0DayRelease = class(TTestCase)
  published
    procedure TestT0DayRelease1;
    procedure TestT0DayRelease2;
    procedure TestT0DayRelease3;
    procedure TestT0DayRelease4;
    procedure TestT0DayRelease5;
    procedure TestT0DayRelease6;
    procedure TestT0DayRelease7;
    procedure TestT0DayRelease8;
    procedure TestT0DayRelease9;
  end;

implementation

uses
  SysUtils, kb.releaseinfo;

{ TTestT0DayRelease }

procedure TTestT0DayRelease.TestT0DayRelease1;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Alders.Blood.v1.0.10.RIP-SiMPLEX', '0DAY');
  try
    CheckEqualsString('WIN', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease2;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Simulation.Lab.Software.SimLab.Composer.10.Ultimate.v10.6.MACOSX-AMPED', '0DAY');
  try
    CheckEqualsString('MAC', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease3;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Toolchefs.Atoms.Crowd.v3.4.1.for.Maya.LINUX-AMPED', '0DAY');
  try
    CheckEqualsString('LINUX', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease4;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('VMware.Workstation.Pro.v15.5.6.X64.Incl.Keygen-AMPED', '0DAY');
  try
    CheckEqualsString('WIN', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease5;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Duden.Korrektor.fuer.Adobe.2019.v14.2.German.WinALL.Incl.Keygen.Patch-BLiZZARD', '0DAY');
  try
    CheckEqualsString('WIN', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease6;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('OsmAnd.OsmAnd.Plus.Maps.and.Navigation.v3.7.1.ANDROiD-rGPDA', 'PDA');
  try
    CheckEqualsString('ANDROID', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease7;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('McAfee.VirusScan.Command.Line.v6.1.3.FreeBSD.15TH.BIRTHDAY-DVT', '0DAY');
  try
    CheckEqualsString('UNIX', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease8;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Richardson.Software.RazorSQL.v9.1.2.Solaris.Incl.KeyMaker-DVT', '0DAY');
  try
    CheckEqualsString('UNIX', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestT0DayRelease.TestT0DayRelease9;
var
  fClass: T0DayRelease;
begin
  fClass := T0DayRelease.Create('Elsten.Software.Bliss.v20200423.QNAP.Incl.KeyMaker-DVT', '0DAY');
  try
    CheckEqualsString('NAS', fClass.nulldaysource, '0day source mismatch');
  finally
    fClass.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('T0DayRelease tests', TTestT0DayRelease.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestT0DayRelease);
  {$ENDIF}
end.

