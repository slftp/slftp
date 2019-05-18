unit dbtvinfoTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestShowFunctions = class(TTestCase)
  published
    procedure ReplaceTVShowChars1;
    procedure ReplaceTVShowChars2;
    procedure ReplaceTVShowChars3;
    procedure ReplaceTVShowChars4;
    procedure ReplaceTVShowChars5;
    procedure GetShowValues1;
    procedure GetShowValues2;
    procedure GetShowValues3;
    procedure GetShowValues4;
    procedure GetShowValues5;
    procedure GetShowValues6;
    procedure GetShowValues7;
    procedure GetShowValues8;
    procedure GetShowValues9;
    procedure GetShowValues10;
    procedure GetShowValues11;
    procedure GetShowValues12;
    procedure GetShowValues13;
    procedure GetShowValues14;
    procedure GetShowValues15;
    procedure GetShowValues16;
  end;

implementation

uses
  SysUtils, dbtvinfo;

{ TTestShowFunctions }

procedure TTestShowFunctions.ReplaceTVShowChars1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Greys Anatomy';

  fExpectedResultStr := 'Greys.Anatomy';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');

  fExpectedResultStr := 'Greys+Anatomy';
  fOutputStr := replaceTVShowChars(fInputStr, True);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars for web failed!');
end;

procedure TTestShowFunctions.ReplaceTVShowChars2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Double Shot at Love';
  
  fExpectedResultStr := 'Double.Shot.@.Love';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');
  
  fExpectedResultStr := 'Double+Shot+@+Love';
  fOutputStr := replaceTVShowChars(fInputStr, True);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars for web failed!');
end;

procedure TTestShowFunctions.ReplaceTVShowChars3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Andromeda';
  
  fExpectedResultStr := 'Andromeda';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');
  
  fExpectedResultStr := 'Andromeda';
  fOutputStr := replaceTVShowChars(fInputStr, True);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars for web failed!');
end;

procedure TTestShowFunctions.ReplaceTVShowChars4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Alvin and the Chipmunks';
  
  fExpectedResultStr := 'Alvin.&.the.Chipmunks';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');
  
  fOutputStr := replaceTVShowChars(fInputStr, True);
  fExpectedResultStr := 'Alvin+&+the+Chipmunks';
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars for web failed!');
end;

procedure TTestShowFunctions.ReplaceTVShowChars5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Prison Break '; // additional whitespace test
  
  fExpectedResultStr := 'Prison.Break';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');
  
  fOutputStr := replaceTVShowChars(fInputStr, True);
  fExpectedResultStr := 'Prison+Break';
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars for web failed!');
end;

procedure TTestShowFunctions.GetShowValues1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Greys.Anatomy.S15E14.1080p.HDTV.x264-CRAVERS';
  fExpectedResultStr := 'Greys.Anatomy';
  fSeason := 15;
  fEpisode := 14;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Gospodin.Savrseni.Late.Night.S01E06.CROATiAN.WEB.H264-RADiOACTiVE';
  fExpectedResultStr := 'Gospodin.Savrseni.Late.Night';
  fSeason := 1;
  fEpisode := 6;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Suits.S08E16.iNTERNAL.1080p.WEB.x264-BAMBOOZLE';
  fExpectedResultStr := 'Suits';
  fSeason := 8;
  fEpisode := 16;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.Goldbergs.2013.S06E17.iNTERNAL.720p.WEB.H264-AMRAP';
  fExpectedResultStr := 'The.Goldbergs.2013';
  fSeason := 6;
  fEpisode := 17;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'House.Hunters.International.S135E01.Falling.in.Love.with.Wroclaw.Poland.720p.WEBRip.x264-CAFFEiNE';
  fExpectedResultStr := 'House.Hunters.International';
  fSeason := 135;
  fEpisode := 1;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Mark.Kermodes.Secrets.of.Cinema.S01E00.Oscar.Winners-A.Secrets.of.Cinema.Special.720p.HDTV.X264-CREED';
  fExpectedResultStr := 'Mark.Kermodes.Secrets.of.Cinema';
  fSeason := 1;
  fEpisode := 0;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.Eccentric.Family.E03.Der.innere.Salon.des.Lehrmeisters.German.DL.ANiME.BDRiP.x264-ATAX';
  fExpectedResultStr := 'The.Eccentric.Family';
  fSeason := 0;
  fEpisode := 3;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'L.Echappee.S03E20.FRENCH.720p.HDTV.x264-BAWLS';
  fExpectedResultStr := 'L.Echappee';
  fSeason := 3;
  fEpisode := 20;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues9;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Big.Fix.Alaska.S01E02.RERIP.720p.HDTV.x264-CURIOSITY';
  fExpectedResultStr := 'Big.Fix.Alaska';
  fSeason := 1;
  fEpisode := 2;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues10;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Doctors.S17E198.720p.WEB.H264-FADE';
  fExpectedResultStr := 'Doctors';
  fSeason := 17;
  fEpisode := 198;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues11;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Casualty.S30E26.Fatal.Error.Part.Two.720p.HDTV.x264-ORGANiC';
  fExpectedResultStr := 'Casualty';
  fSeason := 30;
  fEpisode := 26;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues12;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.Flash.S02E05.Licht.in.der.Dunkelheit.GERMAN.DUBBED.DL.720p.WebHD.h264-euHD';
  fExpectedResultStr := 'The.Flash';
  fSeason := 2;
  fEpisode := 5;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues13;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Houdini.and.Doyle.S01E05.720p.HDTV.x264-TLA';
  fExpectedResultStr := 'Houdini.and.Doyle';
  fSeason := 1;
  fEpisode := 5;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues14;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Kaya.Yanar.LIVE.All.Inclusive.GERMAN.720p.HDTV.x264-TVP';
  fExpectedResultStr := 'Kaya.Yanar.LIVE.All.Inclusive';
  fSeason := 0;
  fEpisode := 0;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues15;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Nicky.Deuce.2013.720p.HDTV.x264-DEADPOOL';
  fExpectedResultStr := 'Nicky.Deuce';
  fSeason := 0;
  fEpisode := 0;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues16;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := '2017.Flick.Electric.Co.Comedy.Gala.Part.1.HDTV.x264-FiHTV';
  fExpectedResultStr := '2017.Flick.Electric.Co.Comedy.Gala';
  fSeason := -10;
  fEpisode := 1;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('dbtvinfo', TTestShowFunctions.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestShowFunctions);
  {$ENDIF}
end.