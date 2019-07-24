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
    procedure GetShowValues17;
    procedure GetShowValues18;
    procedure GetShowValues19;
    procedure GetShowValues20;
    procedure GetShowValues21;
    procedure GetShowValues22;
    procedure GetShowValues23;
    procedure GetShowValues24;
    procedure GetShowValues25;
    procedure GetShowValues26;
    procedure GetShowValues27;
    {
    procedure GetShowValues28;
    procedure GetShowValues29;
    procedure GetShowValues30;
    }
    procedure GetShowValues31;
    procedure GetShowValues32;
    procedure GetShowValues33;
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
  fSeason := -10;
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

procedure TTestShowFunctions.GetShowValues17;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Biodiversite.Climat.L.Europe.Peut.Elle.Stopper.La.Catastrophe.28.Minutes.2018.DOC.FRENCH.720p.WEB.H264-SLiPS';
  fExpectedResultStr := 'Biodiversite.Climat.L.Europe.Peut.Elle.Stopper.La.Catastrophe.28.Minutes';
  fSeason := 0;
  fEpisode := 0;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues18;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Super.League.2019.03.30.Lamia.vs.Panionios.GREEK.720p.HDTV.x264-IcHoR';
  fExpectedResultStr := 'Super.League';
  fSeason := -99;
  fEpisode := 1553904000;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues19;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Japan.von.oben.E03.Wiege.der.Tradition.GERMAN.DOKU.720p.HDTV.x264-BTVG';
  fExpectedResultStr := 'Japan.von.oben';
  fSeason := -10;
  fEpisode := 3;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues20;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.New.Frontier.S04E08.1080p.WEB.H264-EDHD';
  fExpectedResultStr := 'The.New.Frontier';
  fSeason := 4;
  fEpisode := 8;
  
  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
  
  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues21;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Rescue.Me.S07D02.COMPLETE.BLURAY-BluBlade';
  fExpectedResultStr := 'Rescue.Me';
  fSeason := 7;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues22;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Crashing.US.S02.COMPLETE.BLURAY-WESTCOAST';
  fExpectedResultStr := 'Crashing.US';
  fSeason := 2;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues23;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Father.Brown.2013.S04D03.COMPLETE.BLURAY-PFa';
  fExpectedResultStr := 'Father.Brown.2013';
  fSeason := 4;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues24;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'No.Offence.S03.MULTi.COMPLETE.BLURAY-SharpHD';
  fExpectedResultStr := 'No.Offence';
  fSeason := 3;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues25;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'All.Round.To.Mrs.Browns.S02D01.PAL.DVD9-WaLMaRT';
  fExpectedResultStr := 'All.Round.To.Mrs.Browns';
  fSeason := 2;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues26;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Designated.Survivor.S02.D01.MULTi.COMPLETE.BLURAY-SharpHD';
  fExpectedResultStr := 'Designated.Survivor';
  fSeason := 2;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues27;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Doctor.Who.2005.S10.Part.One.D01.COMPLETE.BLURAY-OCULAR';
  fExpectedResultStr := 'Doctor.Who.2005';
  fSeason := 10;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;
{
procedure TTestShowFunctions.GetShowValues28;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Alarm.fuer.Cobra.11.die.Autobahnpolizei.Staffel.30.German.1996.WS.PAL.DVDR-OldsMan';
  fExpectedResultStr := 'Alarm.fuer.Cobra.11.die.Autobahnpolizei';
  fSeason := 30;
  fEpisode := -10;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues29;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Designated.Survivor.Staffel.S02E01.German.DL.DUBBED.720p.WebHD.x264-AIDA';
  fExpectedResultStr := 'Designated.Survivor';
  fSeason := 2;
  fEpisode := 1;

  getShowValues(fInputStr, fOutputStr);
  // not equally is expected because its a group tagging failure
  CheckNotEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  // not equally is expected because its a group tagging failure
  CheckNotEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues30;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Adam.sucht.Eva.Gestrandet.im.Paradies.Best.of.Staffel.1-4.GERMAN.720p.HDTV.x264-RTL';
  fExpectedResultStr := 'Adam.sucht.Eva.Gestrandet.im.Paradies.Best.of';
  fSeason := 1;
  fEpisode := -10;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;
}

procedure TTestShowFunctions.GetShowValues31;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'UFC.Fight.Night.155.Prelims.REAL.1080p.HDTV.x264-VERUM';
  fExpectedResultStr := 'UFC.Fight.Night.155.Prelims';
  fSeason := 0;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues32;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'UFC.Fight.Night.155.REPACK.INTERNAL.REAL.WEB.H264-LEViTATE';
  fExpectedResultStr := 'UFC.Fight.Night.155';
  fSeason := 0;
  fEpisode := 0;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues33;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.Final.Quarter.2019.720p.HDTV.x264-CBFM';
  fExpectedResultStr := 'The.Final.Quarter';
  fSeason := 0;
  fEpisode := 0;

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