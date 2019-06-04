unit mystringsTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestMyStrings = class(TTestCase)
  published
    procedure TestMyIncludeTrailingSlash;
    procedure TestIsALetter;
    procedure TestIsANumber;
    procedure TestOccurrencesOfNumbers;
    procedure TestDatumIdentifierReplace;
    procedure TestSplitString;
    procedure TestMyDateToStr;
  end;

implementation

uses
  SysUtils, Classes, DateUtils, mystrings;

{ TTestMyStrings }

procedure TTestMyStrings.TestMyIncludeTrailingSlash;
begin
  CheckEquals('/', MyIncludeTrailingSlash(''), 'Empty dir should give /');
  CheckEquals('/inc/apps/', MyIncludeTrailingSlash('/inc/apps'), '/inc/apps -> /inc/apps/');
  CheckEquals('/inc/apps/', MyIncludeTrailingSlash('/inc/apps/'), '/inc/apps/ -> /inc/apps/');
end;

procedure TTestMyStrings.TestIsALetter;
begin
  CheckTrue(IsALetter('a'), 'a is letter');
  CheckTrue(IsALetter('g'), 'g is letter');
  CheckTrue(IsALetter('z'), 'z is letter');
  CheckTrue(IsALetter('A'), 'A is letter');
  CheckTrue(IsALetter('K'), 'K is letter');
  CheckTrue(IsALetter('Z'), 'Z is letter');
  CheckFalse(IsALetter(#0), '#0 is not a letter');
  CheckFalse(IsALetter(#50), '#50 is not a letter');
  CheckTrue(IsALetter(#97), 'decimal 97 is a');
  CheckFalse(IsALetter(#255), '#255 is not a letter');
  CheckFalse(IsALetter('0'), '0 is not a letter');
  CheckFalse(IsALetter('3'), '3 is not a letter');
  CheckFalse(IsALetter('9'), '9 is not a letter');
end;

procedure TTestMyStrings.TestIsANumber;
begin
  CheckFalse(IsANumber('a'), 'a is not a number');
  CheckFalse(IsANumber('g'), 'g is not a number');
  CheckFalse(IsANumber('z'), 'z is not a number');
  CheckFalse(IsANumber('A'), 'A is not a number');
  CheckFalse(IsANumber('K'), 'K is not a number');
  CheckFalse(IsANumber('Z'), 'Z is not a number');
  CheckFalse(IsANumber(#0), 'decimal 0 is not a number');
  CheckTrue(IsANumber(#50), 'decimal 50 is a number');
  CheckFalse(IsANumber(#255), 'decimal 255 is not a number');
  CheckTrue(IsANumber('0'), '0 is a number');
  CheckTrue(IsANumber('3'), '3 is a number');
  CheckTrue(IsANumber('9'), '9 is a number');
end;

procedure TTestMyStrings.TestOccurrencesOfNumbers;
begin
  CheckEquals(11, OccurrencesOfNumbers('Greys.Anatomy.S15E15.1080p.HDTV.x264-CRAVERS'), 'Should have 11 numbers');
  CheckEquals(7, OccurrencesOfNumbers('Take.It.or.Leave.It.2018.DVDRip.x264-EMX'), 'Should have 7 numbers');
  CheckEquals(0, OccurrencesOfNumbers('WavSupply.Nick.Mira.Forge.WAV-MASCHiNE'), 'Should have 0 numbers');
  CheckEquals(1, OccurrencesOfNumbers('This is a sentence with 2 questionmarks??'), 'Should have 2 numbers');
end;

procedure TTestMyStrings.TestDatumIdentifierReplace;
var
  fDateTime: TDateTime;
begin
  fDateTime := EncodeDate(2019, 4, 13); // 13/4/2019
  CheckEquals('/inc/mp3/2019/04/13/', DatumIdentifierReplace('/inc/mp3/<yyyy>/<mm>/<dd>/', fDateTime));
  CheckEquals('/inc/mp3/19/04/13/', DatumIdentifierReplace('/inc/mp3/<yy>/<mm>/<dd>/', fDateTime));
  CheckEquals('/inc/mvid/2019/15/', DatumIdentifierReplace('/inc/mvid/<yyyy>/<ww>/', fDateTime));

  fDateTime := EncodeDate(2017, 1, 2); // 2/1/2017
  CheckEquals('/inc/mp3/2017/01/02/', DatumIdentifierReplace('/inc/mp3/<yyyy>/<mm>/<dd>/', fDateTime));
  CheckEquals('/inc/mp3/17/01/02/', DatumIdentifierReplace('/inc/mp3/<yy>/<mm>/<dd>/', fDateTime));
  CheckEquals('/inc/mvid/17/01/', DatumIdentifierReplace('/inc/mvid/<yy>/<ww>/', fDateTime));
end;

procedure TTestMyStrings.TestSplitString;
var
  fOutStrList, fExpectedResultList: TStringList;
  fInputStr: String;
  i: Integer;
begin
  fOutStrList := TStringList.Create;
  try
    fExpectedResultList := TStringList.Create;
    try
      fInputStr := 'this:is just:a:simple:UNIT:test for mystrings';
      fExpectedResultList.CommaText := 'this, "is just", a, simple, UNIT, "test for mystrings"';

      SplitString(fInputStr, ':', fOutStrList);

      CheckEquals(fExpectedResultList.Count, fOutStrList.Count, 'Count of stringlist differs');

      for i := 0 to fExpectedResultList.Count - 1 do
        CheckEquals(fExpectedResultList[i], fOutStrList[i], fExpectedResultList[i] + '<>' + fOutStrList[i]);
    finally
      fExpectedResultList.Free;
    end;
  finally
    fOutStrList.Free;
  end;
end;

procedure TTestMyStrings.TestMyDateToStr;
var
  fDateTime: TDateTime;
begin
  fDateTime := EncodeDateTime(2018, 10, 29, 13, 15, 31, 0); // 29/10/2018 13:15:31:0
  CheckEquals('2018-10-29 13:15:31', MyDateToStr(fDateTime));

  fDateTime := EncodeDateTime(2019, 6, 2, 3, 56, 8, 13); // 2/6/2019 3:56:8:31
  CheckEquals('2019-06-02 03:56:08', MyDateToStr(fDateTime));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings', TTestMyStrings.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestMyStrings);
  {$ENDIF}
end.