unit dbtvinfoTests;

interface

uses
  TestFramework;

type
  TTestShowFunctions = class(TTestCase)
  protected
    procedure SetUpOnce; override;
    procedure TeardownOnce; override;
  published
    procedure PlainShowName1;
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

procedure TTestShowFunctions.SetUpOnce;
begin
  writeln('hi');
end;

procedure TTestShowFunctions.TeardownOnce;
begin
  writeln('bye!');
end;

procedure TTestShowFunctions.PlainShowName1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Greys.Anatomy.S15E15.1080p.HDTV.x264-CRAVERS';
  fExpectedResultStr := 'Greys.Anatomy.S14';
  fOutputStr := replaceTVShowChars(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Removing scene tags failed!');
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
  RegisterTest('dbtvinfo Showname functions', TTestShowFunctions.Suite);
end.