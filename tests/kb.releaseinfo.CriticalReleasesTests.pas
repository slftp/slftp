unit kb.releaseinfo.CriticalReleasesTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestCriticalReleases = class(TTestCase)
  published
    { These releases showed high latency in the race timeline analysis.
      The tests ensure they are parsed consistently by the release info
      handlers, so latency spikes can be reproduced outside of the running
      bot (ruling out transient locks/IRC load as the only cause). }
    procedure TestParsePatriceRobertsBounce;
    procedure TestParseGhostInTheShellS01E01;
    procedure TestParseUltraBigTimeReissue;
    procedure TestParseSquattersS04E04;
  end;

implementation

uses
  SysUtils, kb, kb.releaseinfo;

{ TTestCriticalReleases }

procedure TTestCriticalReleases.TestParsePatriceRobertsBounce;
var
  fRls: TRelease;
  fHandler: TCRelease;
const
  cRls = 'Patrice_Roberts-Bounce-SINGLE-WEB-2026-JAH';
begin
  fHandler := FindSectionHandler('MP3');
  Check(fHandler <> nil, 'MP3 section handler not found');

  fRls := fHandler.Create(cRls, 'MP3', False);
  try
    CheckNotNull(fRls, 'Release object was not created');
    CheckEqualsString(cRls, fRls.rlsname, 'release name mismatch');
    CheckEqualsString('MP3', fRls.section, 'section mismatch');
    CheckEqualsString('JAH', fRls.groupname, 'groupname mismatch');
    Check(fRls is TMP3Release, 'expected TMP3Release');
  finally
    fRls.Free;
  end;
end;

procedure TTestCriticalReleases.TestParseGhostInTheShellS01E01;
var
  fRls: TRelease;
  fHandler: TCRelease;
const
  cRls = 'The.Ghost.in.the.Shell.S01E01.GERMAN.DL.ANiME.1080p.WEB.h264-SAUERKRAUT';
begin
  fHandler := FindSectionHandler('TV1080P');
  Check(fHandler <> nil, 'TV section handler not found');

  fRls := fHandler.Create(cRls, 'TV1080P', False);
  try
    CheckNotNull(fRls, 'Release object was not created');
    CheckEqualsString(cRls, fRls.rlsname, 'release name mismatch');
    CheckEqualsString('TV1080P', fRls.section, 'section mismatch');
    CheckEqualsString('SAUERKRAUT', fRls.groupname, 'groupname mismatch');
  finally
    fRls.Free;
  end;
end;

procedure TTestCriticalReleases.TestParseUltraBigTimeReissue;
var
  fRls: TRelease;
  fHandler: TCRelease;
const
  cRls = 'Ultra-Big_Time-25th_Anniversary_Limited_Edition-Reissue-2LP-2022-NOiR_INT';
begin
  fHandler := FindSectionHandler('MP3');
  Check(fHandler <> nil, 'MP3 section handler not found');

  fRls := fHandler.Create(cRls, 'MP3', False);
  try
    CheckNotNull(fRls, 'Release object was not created');
    CheckEqualsString(cRls, fRls.rlsname, 'release name mismatch');
    CheckEqualsString('MP3', fRls.section, 'section mismatch');
    CheckEqualsString('NOiR_INT', fRls.groupname, 'groupname mismatch');
    Check(fRls is TMP3Release, 'expected TMP3Release');
  finally
    fRls.Free;
  end;
end;

procedure TTestCriticalReleases.TestParseSquattersS04E04;
var
  fRls: TRelease;
  fHandler: TCRelease;
const
  cRls = 'Squatters.Get.the.F.Out.of.My.House.S04E04.POLISH.1080p.WEB.H264-FLAME';
begin
  fHandler := FindSectionHandler('TV1080P');
  Check(fHandler <> nil, 'TV section handler not found');

  fRls := fHandler.Create(cRls, 'TV1080P', False);
  try
    CheckNotNull(fRls, 'Release object was not created');
    CheckEqualsString(cRls, fRls.rlsname, 'release name mismatch');
    CheckEqualsString('TV1080P', fRls.section, 'section mismatch');
    CheckEqualsString('FLAME', fRls.groupname, 'groupname mismatch');
  finally
    fRls.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('kb.releaseinfo.critical', TTestCriticalReleases.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestCriticalReleases);
  {$ENDIF}
end.
