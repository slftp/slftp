unit dbtvinfoTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  dbtvinfo;

type
  TTestShowFunctions = class(TTestCase)
  published
    procedure ReplaceTVShowChars1;
    procedure ReplaceTVShowChars2;
    procedure ReplaceTVShowChars3;
    procedure ReplaceTVShowChars4;
    procedure ReplaceTVShowChars5;
    procedure ReplaceTVShowChars6;
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
    procedure GetShowValues34;
    procedure GetShowValues35;
    procedure GetShowValues36;
    procedure GetShowValues37;
    procedure GetShowValues38;
    procedure GetShowValues39;
    procedure GetShowValues40;
    procedure GetShowValues41;
    procedure GetShowValues42;
  end;

  { @abstract(Tests for the mORMot2 ORM persistence of dbtvinfo (Save/Get/Update/Delete and legacy table migration)) }
  TTestTVInfoDb = class(TTestCase)
  private
    { deletes the test database incl. WAL/SHM files }
    procedure DeleteTestDb;
    { creates a fully filled TTVInfoDB fixture for 'The Grand Show' (tvmaze_id 12345) }
    function CreateFixture: TTVInfoDB;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveAndGetByShowName;
    procedure TestSaveAndGetByShowID;
    procedure TestSaveAndGetByReleaseName;
    procedure TestSaveDuplicateIsIgnored;
    procedure TestExecuteUpdate;
    procedure TestSetTheTVDbIDAndTVRageID;
    procedure TestDeleteTVInfoByID;
    procedure TestDeleteTVInfoByRipName;
    procedure TestLegacyTableMigration;
  end;

implementation

uses
  SysUtils, Classes, tvinfo.types, globals, dbhandler, mormot.core.base,
  mormot.core.unicode, mormot.db.raw.sqlite3;

const
  CTEST_DB_NAME = 'test_tvinfo.db'; //< database file name used by the ORM persistence tests

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
  
  fExpectedResultStr := 'Double.Shot.at.Love';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');
  
  fExpectedResultStr := 'Double+Shot+at+Love';
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
  
  fExpectedResultStr := 'Alvin.%26.the.Chipmunks';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');

  fOutputStr := replaceTVShowChars(fInputStr, True);
  fExpectedResultStr := 'Alvin+%26+the+Chipmunks';
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

procedure TTestShowFunctions.ReplaceTVShowChars6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Let''s Make A Deal'; // High Comma Test

  fExpectedResultStr := 'Lets.Make.A.Deal';
  fOutputStr := replaceTVShowChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Replacing TV Show Chars failed!');

  fOutputStr := replaceTVShowChars(fInputStr, True);
  fExpectedResultStr := 'Lets+Make+A+Deal';
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
  fSeason := Ord(tvRegularSerieWithoutSeason);
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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);
  
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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);
  
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
  fSeason := Ord(tvRegularSerieWithoutSeason);
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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);
  
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
  fSeason := Ord(tvDatedShow);
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
  fSeason := Ord(tvRegularSerieWithoutSeason);
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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fEpisode := Ord(tvNoEpisodeTag);

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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

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
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;


procedure TTestShowFunctions.GetShowValues34;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Marvels.Jessica.Jones.S03E07.DIRFIX.PROPER.1080p.WEB.X264-METCON';
  fExpectedResultStr := 'Marvels.Jessica.Jones';
  fSeason := 3;
  fEpisode := 7;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues35;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'The.Man.Who.Saw.Too.Much.2009.NFOFIX.720p.HDTV.x264-PVR';
  fExpectedResultStr := 'The.Man.Who.Saw.Too.Much';
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues36;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Dersu.Uzala.1975.SUBBED.DiRFiX.NFOFiX.1080p.HDTV.x264-REGRET';
  fExpectedResultStr := 'Dersu.Uzala';
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues37;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'UFC.222.iNTERNAL.NFOFIX.720p.HDTV.x264-KOENiG';
  fExpectedResultStr := 'UFC.222';
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues38;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Brynhildr.In.The.Darkness.E02.SFVFIX.SUBFRENCH.720p.WEBRip.X264-SLEEPINGFOREST';
  fExpectedResultStr := 'Brynhildr.In.The.Darkness';
  fSeason := Ord(tvRegularSerieWithoutSeason);
  fEpisode := 2;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues39;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Planet.HD.unsere.Erde.in.High.Definition.S02E04.Vietnam.GERMAN.DL.DOKU.2160p.UHD.BluRay.x265.SAMPLEFiX.PROOFFiX-DOKUUHD';
  fExpectedResultStr := 'Planet.HD.unsere.Erde.in.High.Definition';
  fSeason := 2;
  fEpisode := 4;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues40;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Ascendance.Of.A.Bookworm.E01.SAMPLEFiX.WEB.x264-URANiME';
  fExpectedResultStr := 'Ascendance.Of.A.Bookworm';
  fSeason := Ord(tvRegularSerieWithoutSeason);
  fEpisode := 1;

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues41;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Min.Far.Er.Rocker.Thorhjoern.2019.SAMPLEFIX.DANISH.720p.WEB.h264-FFD';
  fExpectedResultStr := 'Min.Far.Er.Rocker.Thorhjoern';
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

procedure TTestShowFunctions.GetShowValues42;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
  fSeason, fOutSeason: integer;
  fEpisode, fOutEpisode: int64;
begin
  fInputStr := 'Cage.Fury.FC.77.DIRFIX.WEB.H264-LEViTATE';
  fExpectedResultStr := 'Cage.Fury.FC.77';
  fSeason := Ord(tvNoExplicitShowTag);
  fEpisode := Ord(tvNoExplicitShowTag);

  getShowValues(fInputStr, fOutputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');

  getShowValues(fInputStr, fOutputStr, fOutSeason, fOutEpisode);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags and getting season+episode failed!');
  CheckEquals(fSeason, fOutSeason, 'Getting season failed!');
  CheckEquals(fEpisode, fOutEpisode, 'Getting episode failed!');
end;

{ TTestTVInfoDb }

procedure TTestTVInfoDb.DeleteTestDb;
var
  fDbPath: String;
begin
  fDbPath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim + CTEST_DB_NAME;
  DeleteFile(fDbPath);
  DeleteFile(fDbPath + '-wal');
  DeleteFile(fDbPath + '-shm');
end;

function TTestTVInfoDb.CreateFixture: TTVInfoDB;
begin
  Result := TTVInfoDB.Create('The Grand Show');
  Result.tv_showname := 'The Grand Show';
  Result.tvmaze_id := '12345';
  Result.thetvdb_id := '6789';
  Result.tvrage_id := '4321';
  Result.tv_url := 'https://www.tvmaze.com/shows/12345/the-grand-show';
  Result.tv_premiered_year := 2019;
  Result.tv_country := 'USA';
  Result.tv_status := 'Running';
  Result.tv_classification := 'Scripted';
  Result.tv_network := 'HBO';
  Result.tv_genres.CommaText := 'Drama,Comedy';
  Result.tv_endedyear := -1;
  Result.tv_next_date := 1893456000;
  Result.tv_next_season := 2;
  Result.tv_next_ep := 5;
  Result.tv_days.CommaText := 'Monday,Tuesday';
  Result.tv_rating := 84;
  Result.tv_language := 'English';
end;

procedure TTestTVInfoDb.SetUp;
begin
  inherited SetUp;
  DeleteTestDb;
  // workaround for the known _CreateDatabaseFolder CWD bug in dbhandler.pas
  ForceDirectories(ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME);
  dbTVInfoStart(CTEST_DB_NAME);
end;

procedure TTestTVInfoDb.TearDown;
begin
  dbTVInfoUninit;
  DeleteTestDb;
  inherited TearDown;
end;

procedure TTestTVInfoDb.TestSaveAndGetByShowName;
var
  fTvi, fRead: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  CheckEquals(1, getTVInfoCount, 'one infos row expected after Save');
  CheckEquals(1, getTVInfoSeriesCount, 'one series row expected after Save');

  fRead := getTVInfoByShowName('The Grand Show');
  CheckTrue(fRead <> nil, 'getTVInfoByShowName should find the saved show');
  if fRead = nil then
    exit;
  try
    CheckEquals('The Grand Show', fRead.tv_showname, 'showname mismatch');
    CheckEquals('12345', fRead.tvmaze_id, 'tvmaze_id mismatch');
    CheckEquals('6789', fRead.thetvdb_id, 'thetvdb_id mismatch');
    CheckEquals('4321', fRead.tvrage_id, 'tvrage_id mismatch');
    CheckEquals('https://www.tvmaze.com/shows/12345/the-grand-show', fRead.tv_url, 'url mismatch');
    CheckEquals(2019, fRead.tv_premiered_year, 'premiered_year mismatch');
    CheckEquals('USA', fRead.tv_country, 'country mismatch');
    CheckEquals('Running', fRead.tv_status, 'status mismatch');
    CheckEquals('Scripted', fRead.tv_classification, 'classification mismatch');
    CheckEquals('HBO', fRead.tv_network, 'network mismatch');
    CheckEquals('Drama,Comedy', fRead.tv_genres.CommaText, 'genres mismatch');
    CheckEquals(-1, fRead.tv_endedyear, 'endedyear mismatch');
    CheckEquals(1893456000, fRead.tv_next_date, 'next_date mismatch');
    CheckEquals(2, fRead.tv_next_season, 'next_season mismatch');
    CheckEquals(5, fRead.tv_next_ep, 'next_ep mismatch');
    CheckEquals('Monday,Tuesday', fRead.tv_days.CommaText, 'airdays mismatch');
    CheckEquals(84, fRead.tv_rating, 'rating mismatch');
    CheckEquals('English', fRead.tv_language, 'language mismatch');
    CheckTrue(fRead.last_updated > 0, 'last_updated should be a unix timestamp');
    CheckTrue(fRead.tv_running, 'tv_running should be computed as true');
    CheckTrue(fRead.tv_scripted, 'tv_scripted should be computed as true');
  finally
    fRead.Free;
  end;

  // case-insensitive rip matching like the old LIKE query
  fRead := getTVInfoByShowName('the grand show');
  CheckTrue(fRead <> nil, 'getTVInfoByShowName should match case-insensitive');
  if fRead <> nil then
    fRead.Free;
end;

procedure TTestTVInfoDb.TestSaveAndGetByShowID;
var
  fTvi, fRead: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  fRead := getTVInfoByShowID('12345');
  CheckTrue(fRead <> nil, 'getTVInfoByShowID should find the saved show');
  if fRead = nil then
    exit;
  try
    CheckEquals('The Grand Show', fRead.rls_showname, 'rip mismatch');
    CheckEquals('12345', fRead.tvmaze_id, 'tvmaze_id mismatch');
    CheckEquals(2019, fRead.tv_premiered_year, 'premiered_year mismatch');
    CheckEquals(84, fRead.tv_rating, 'rating mismatch');
  finally
    fRead.Free;
  end;

  CheckTrue(getTVInfoByShowID('99999') = nil, 'unknown id should return nil');
end;

procedure TTestTVInfoDb.TestSaveAndGetByReleaseName;
var
  fTvi, fRead: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  fRead := getTVInfoByReleaseName('The.Grand.Show.S01E02.GERMAN.720p.HDTV.x264-TEST');
  CheckTrue(fRead <> nil, 'getTVInfoByReleaseName should find the saved show');
  if fRead = nil then
    exit;
  try
    CheckEquals('12345', fRead.tvmaze_id, 'tvmaze_id mismatch');
  finally
    fRead.Free;
  end;
end;

procedure TTestTVInfoDb.TestSaveDuplicateIsIgnored;
var
  fTvi: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  // second Save must be ignored like the old INSERT OR IGNORE
  fTvi := CreateFixture;
  try
    fTvi.Save;
    CheckEquals(3817, fTvi.last_updated, 'last_updated marker for ignored insert expected');
  finally
    fTvi.Free;
  end;

  CheckEquals(1, getTVInfoCount, 'duplicate Save must not add another infos row');
  CheckEquals(1, getTVInfoSeriesCount, 'duplicate Save must not add another series row');
end;

procedure TTestTVInfoDb.TestExecuteUpdate;
var
  fTvi, fRead: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;

    fTvi.tv_status := 'Ended';
    fTvi.tv_endedyear := 2023;
    fTvi.tv_rating := 91;
    CheckTrue(fTvi.executeUpdate, 'executeUpdate should return true for an existing show');
  finally
    fTvi.Free;
  end;

  CheckEquals(1, getTVInfoCount, 'executeUpdate must not add another infos row');

  fRead := getTVInfoByShowID('12345');
  CheckTrue(fRead <> nil, 'updated show should be readable');
  if fRead = nil then
    exit;
  try
    CheckEquals('Ended', fRead.tv_status, 'status should have been updated');
    CheckEquals(2023, fRead.tv_endedyear, 'endedyear should have been updated');
    CheckEquals(91, fRead.tv_rating, 'rating should have been updated');
    CheckFalse(fRead.tv_running, 'tv_running should be computed as false');
  finally
    fRead.Free;
  end;

  // update of a non existing show must fail
  fTvi := CreateFixture;
  try
    fTvi.tvmaze_id := '99999';
    CheckFalse(fTvi.executeUpdate, 'executeUpdate should return false for an unknown show');
  finally
    fTvi.Free;
  end;
end;

procedure TTestTVInfoDb.TestSetTheTVDbIDAndTVRageID;
var
  fTvi, fRead: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
    fTvi.setTheTVDbID(111);
    fTvi.setTVRageID(222);
  finally
    fTvi.Free;
  end;

  fRead := getTVInfoByShowID('12345');
  CheckTrue(fRead <> nil, 'show should be readable');
  if fRead = nil then
    exit;
  try
    CheckEquals('111', fRead.thetvdb_id, 'thetvdb_id should have been updated');
    CheckEquals('222', fRead.tvrage_id, 'tvrage_id should have been updated');
  finally
    fRead.Free;
  end;
end;

procedure TTestTVInfoDb.TestDeleteTVInfoByID;
var
  fTvi: TTVInfoDB;
begin
  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  CheckEquals(1, deleteTVInfoByID('12345'), 'delete of existing show should return 1');
  CheckEquals(0, getTVInfoCount, 'infos row should be deleted');
  CheckEquals(0, getTVInfoSeriesCount, 'series row should be deleted');
  CheckEquals(10, deleteTVInfoByID('12345'), 'delete of missing show should return 10');
end;

procedure TTestTVInfoDb.TestDeleteTVInfoByRipName;
var
  fTvi: TTVInfoDB;
begin
  CheckEquals(0, deleteTVInfoByRipName('Unknown Show'), 'delete of unknown rip should return 0');

  fTvi := CreateFixture;
  try
    fTvi.Save;
  finally
    fTvi.Free;
  end;

  CheckEquals(1, deleteTVInfoByRipName('The Grand Show'), 'delete of existing rip should return 1');
  CheckTrue(getTVInfoByShowName('The Grand Show') = nil, 'show should be gone after delete');
end;

procedure TTestTVInfoDb.TestLegacyTableMigration;
var
  fTvi: TTVInfoDB;
  fTables: TRawUTF8DynArray;
  fTableName: RawUTF8;
  fHasLegacyTables: boolean;
  fLegacyDb: TSQLDataBase;
begin
  // build a realistic legacy database from scratch (without any ORM tables):
  // the old Zeos code created the unique indexes tvinfo/Rips on every start and
  // index 'tvinfo' blocks CREATE TABLE TVInfo (shared SQLite name namespace),
  // so the fixture must include them to mirror a real user database
  dbTVInfoUninit;
  DeleteTestDb;

  fLegacyDb := TSQLDataBase.Create(StringToUTF8(GetDatabaseFilePath(CTEST_DB_NAME)));
  try
    fLegacyDb.Execute('CREATE TABLE infos(' +
      'tvdb_id INTEGER, tvrage_id INTEGER, tvmaze_id INTEGER NOT NULL, premiered_year INTEGER NOT NULL, ' +
      'country TEXT NOT NULL DEFAULT unknown, status TEXT NOT NULL DEFAULT unknown, ' +
      'classification TEXT NOT NULL DEFAULT unknown, network TEXT NOT NULL DEFAULT unknown, ' +
      'genre TEXT NOT NULL DEFAULT unknown, ended_year INTEGER, last_updated INTEGER NOT NULL DEFAULT -1, ' +
      'next_date INTEGER, next_season INTEGER, next_episode INTEGER, rating INTEGER, airdays TEXT, ' +
      'tv_language TEXT, PRIMARY KEY (tvmaze_id ASC));');
    fLegacyDb.Execute('INSERT INTO infos (tvdb_id, tvrage_id, tvmaze_id, premiered_year, country, status, ' +
      'classification, network, genre, ended_year, last_updated, next_date, next_season, next_episode, rating, ' +
      'airdays, tv_language) VALUES (6789, 4321, 12345, 2019, ''USA'', ''Running'', ''Scripted'', ''HBO'', ' +
      '''Drama,Comedy'', -1, 1700000000, 1893456000, 2, 5, 84, ''Monday,Tuesday'', ''English'');');
    fLegacyDb.Execute('CREATE TABLE series(' +
      'rip TEXT NOT NULL, showname TEXT NOT NULL, rip_country TEXT, tvmaze_url TEXT, id INTEGER NOT NULL, ' +
      'PRIMARY KEY (rip));');
    fLegacyDb.Execute('INSERT INTO series (rip, showname, rip_country, tvmaze_url, id) VALUES ' +
      '(''The Grand Show'', ''The Grand Show'', NULL, ''https://www.tvmaze.com/shows/12345/the-grand-show'', 12345);');
    // created by the old code on every start, see pre-migration dbTVInfoStart
    fLegacyDb.Execute('CREATE UNIQUE INDEX IF NOT EXISTS main.tvinfo ON infos (tvmaze_id ASC);');
    fLegacyDb.Execute('CREATE UNIQUE INDEX IF NOT EXISTS main.Rips ON series (rip ASC);');
  finally
    fLegacyDb.Free;
  end;

  // start triggers the one-time migration (and must drop the blocking indexes first)
  dbTVInfoStart(CTEST_DB_NAME);

  CheckEquals(1, getTVInfoCount, 'legacy infos row should have been migrated');
  CheckEquals(1, getTVInfoSeriesCount, 'legacy series row should have been migrated');

  fTvi := getTVInfoByShowName('The Grand Show');
  CheckTrue(fTvi <> nil, 'migrated show should be readable');
  if fTvi = nil then
    exit;
  try
    CheckEquals('12345', fTvi.tvmaze_id, 'tvmaze_id mismatch after migration');
    CheckEquals(2019, fTvi.tv_premiered_year, 'premiered_year mismatch after migration');
    CheckEquals('Drama,Comedy', fTvi.tv_genres.CommaText, 'genres mismatch after migration');
    CheckEquals(1700000000, fTvi.last_updated, 'last_updated must be kept from legacy row');
    CheckEquals(84, fTvi.tv_rating, 'rating mismatch after migration');
  finally
    fTvi.Free;
  end;

  fHasLegacyTables := False;
  GlTVInfoDb.DB.GetTableNames(fTables);
  for fTableName in fTables do
    if SameText(fTableName, 'infos') or SameText(fTableName, 'series') then
      fHasLegacyTables := True;
  CheckFalse(fHasLegacyTables, 'legacy tables should have been dropped after migration');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('dbtvinfo', TTestShowFunctions.Suite);
    RegisterTest('dbtvinfo', TTestTVInfoDb.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestShowFunctions);
    TDUnitX.RegisterTestFixture(TTestTVInfoDb);
  {$ENDIF}
end.
