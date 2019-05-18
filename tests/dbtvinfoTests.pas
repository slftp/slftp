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
    {
    procedure PlainShowName2;
    procedure PlainShowName3;
    procedure PlainShowName4;
    procedure PlainShowName5;
    procedure PlainShowName6;
    procedure PlainShowName7;
    procedure PlainShowName8;
    }
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
{
procedure TTestShowFunctions.PlainShowName1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Greys.Anatomy.S15E15.1080p.HDTV.x264-CRAVERS';
  fExpectedResultStr := 'Greys.Anatomy.S14';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;



procedure TTestShowFunctions.PlainShowName2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Gospodin.Savrseni.Late.Night.S01E06.CROATiAN.WEB.H264-RADiOACTiVE';
  fExpectedResultStr := 'Gospodin.Savrseni.Late.Night';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Suits.S08E16.iNTERNAL.1080p.WEB.x264-BAMBOOZLE';
  fExpectedResultStr := 'Suits';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'The.Goldbergs.2013.S06E17.iNTERNAL.720p.WEB.H264-AMRAP';
  fExpectedResultStr := 'The.Goldbergs.2013';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'House.Hunters.International.S135E01.Falling.in.Love.with.Wroclaw.Poland.720p.WEBRip.x264-CAFFEiNE';
  fExpectedResultStr := 'House.Hunters.International.S135E01.Falling.in.Love.with.Wroclaw.Poland';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Mark.Kermodes.Secrets.of.Cinema.S01E00.Oscar.Winners-A.Secrets.of.Cinema.Special.720p.HDTV.X264-CREED';
  fExpectedResultStr := 'Mark.Kermodes.Secrets.of.Cinema.S01E00.Oscar.Winners-A.Secrets.of.Cinema.Special';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'The.Eccentric.Family.E03.Der.innere.Salon.des.Lehrmeisters.German.DL.ANiME.BDRiP.x264-ATAX';
  fExpectedResultStr := 'The.Eccentric.Family.E03.Der.innere.Salon.des.Lehrmeisters';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;

procedure TTestShowFunctions.PlainShowName8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'L.Echappee.S03E20.FRENCH.720p.HDTV.x264-BAWLS';
  fExpectedResultStr := 'L.Echappee';
  getShowValues(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
end;
}
initialization
  {$IFDEF FPC}
    RegisterTest('dbtvinfo Showname functions', TTestShowFunctions.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestShowFunctions);
  {$ENDIF}
end.