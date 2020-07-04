unit kb.releaseinfo.MVIDTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTMVIDRelease = class(TTestCase)
  published
    procedure TestTMVIDRelease1;
    procedure TestTMVIDRelease2;
    procedure TestTMVIDRelease3;
    procedure TestTMVIDRelease4;
    procedure TestTMVIDRelease5;
    procedure TestTMVIDRelease6;
    procedure TestTMVIDRelease7;
    procedure TestTMVIDRelease8;
    procedure TestTMVIDRelease9;
    procedure TestTMVIDRelease10;
    procedure TestTMVIDRelease11;
    procedure TestTMVIDRelease12;
    procedure TestTMVIDRelease13;
    procedure TestTMVIDRelease14;
  end;

implementation

uses
  SysUtils, kb.releaseinfo;

{ TTestTMVIDRelease }

procedure TTestTMVIDRelease.TestTMVIDRelease1;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('The_Big_3_Palladium_Orchestra-Live_At_39th_Internationale_Jazzwoche_Burghausen_2008-03-07-ES-x264-2008-gFViD', 'MVID');
  try
    CheckEquals(23, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2008, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease2;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Lauv-MTV_Live_2020-1080p-x264-2020-SRPx', 'MVID');
  try
    CheckEquals(4, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2020, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease3;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('The_Living_End-Live_At_Berlin_Live_2018-02-18-x264-2018-gFViD', 'MVID');
  try
    CheckEquals(52, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2018, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease4;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Momoiro_Clover_Z-Hakkin_no_Yoake-JA-BLURAY-RETAIL-x264-2016-DARKFLiX', 'MVID');
  try
    CheckEquals(52, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2016, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease5;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Carousel_Kings-Bad_Habit-DDC-720p-x264-2016-LOVERS_iNT', 'MVID');
  try
    CheckEquals(3, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2016, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease6;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Juliette_Armanet-Live_W9_Garden_Concert_(2017-09-09)-FR-x264-2017-iUF', 'MVID');
  try
    CheckEquals(1, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2017, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease7;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Skrillex_And_Poo_Bear-Would_You_Ever-DVDRip-x264-2017-SRPx', 'MVID');
  try
    CheckEquals(3, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2017, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease8;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Pimpinela.Las.Numero.1.2006.ES.NTSC.BONUS.COMPLETE.INTERNAL.MDVDR-NORTE', 'MDVDR');
  try
    CheckEquals(50, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckFalse(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckFalse(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2006, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease9;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Krabathor.Rebirth.Of.Brutality.Live.In.Uherske.Hradiste.2015.PAL.2DiSC.BONUS.MDVDR-AURORA', 'MDVDR');
  try
    CheckEquals(50, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckFalse(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2015, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease10;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Twisted.Sister.Metal.Meltdown.Live.From.The.Hard.Rock.Casino.Las.Vegas.2016.DVD9.NTSC.MDVDR-RUiL', 'MDVDR');
  try
    CheckEquals(81, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckFalse(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2016, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease11;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Bohemian.Rhapsody.The.Complete.Live.Aid.Movie.Performance.2018.2160p.UHD.MBluRay.x265-HFPA', 'MBLURAY');
  try
    CheckEquals(90, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckTrue(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2018, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease12;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('Belcea.Quartet.Beethoven.The.Complete.String.Quartets.DiSC.1.2012.COMPLETE.MBLURAY-EUROARTS', 'MBLURAY');
  try
    CheckEquals(63, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckFalse(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2012, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease13;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('New.Years.Eve.Concert.2019.2020.720p.MBLURAY.x264-MBLURAYFANS', 'MBLURAY');
  try
    CheckEquals(52, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckTrue(fClass.mvidpal, 'PAL mismatch');
    CheckTrue(fClass.mvidntsc, 'NTSC mismatch');
    CheckFalse(fClass.mvidva, 'Various Artists mismatch');
    CheckFalse(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2020, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMVIDRelease.TestTMVIDRelease14;
var
  fClass: TMVIDRelease;
begin
  fClass := TMVIDRelease.Create('VA-Tribute_To_George_Michael_(2017_BET_Awards)-720p-x264-2017-SRPx', 'MVID');
  try
    CheckEquals(1, fClass.mvidfiles, 'Filecount mismatch');
    CheckEqualsString('x,y,z', fClass.mvidgenre.CommaText, 'genre mismatch');
    CheckFalse(fClass.mvidpal, 'PAL mismatch');
    CheckFalse(fClass.mvidntsc, 'NTSC mismatch');
    CheckTrue(fClass.mvidva, 'Various Artists mismatch');
    CheckFalse(fClass.mvidlive, 'LIVE mismatch');
    CheckEquals(2017, fClass.mvidyear, 'Year mismatch');
  finally
    fClass.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TMVIDRelease tests', TTestTMVIDRelease.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTMVIDRelease);
  {$ENDIF}
end.

