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
    procedure TestTEnumToString;
    procedure TestTEnumFromString;
    procedure TestMyIncludeTrailingSlash;
    procedure TestIsALetter;
    procedure TestIsANumber;
    procedure TestOccurrencesOfNumbers;
    procedure TestDatumIdentifierReplace;
    procedure TestSplitString;
    procedure TestMyDateToStr;
    procedure TestParseXDupeResponseToFilenameListGlftpd;
    procedure TestParseXDupeResponseToFilenameListDrftpd;
    procedure TestParseXDupeResponseToFilenameListMultislaveftpd;
    procedure TestParseEPSVStringIPv41;
    procedure TestParseEPSVStringIPv42;
    procedure TestParseEPSVStringIPv43;
    procedure TestParseSTATLine1;
    procedure TestParseSTATLine2;
    procedure TestParseSTATLine3;
    procedure TestParseSTATLine4;
    procedure TestParseSTATLine5;
    procedure TestParseSTATLine6;
    procedure TestParseSTATLine7;
    procedure TestParseSTATLine8;
    procedure TestParseSTATLine9;
    procedure TestParseSTATLine10;
    procedure TestInternationalCharsToAsciiSceneChars1;
    procedure TestInternationalCharsToAsciiSceneChars2;
    procedure TestInternationalCharsToAsciiSceneChars3;
    procedure TestInternationalCharsToAsciiSceneChars4;
    procedure TestInternationalCharsToAsciiSceneChars5;
    procedure TestInternationalCharsToAsciiSceneChars6;
    procedure TestInternationalCharsToAsciiSceneChars7;
    procedure TestInternationalCharsToAsciiSceneChars8;
    procedure TestParseSiteSearchResult1;
    procedure TestParseSiteSearchResult2;
    procedure TestParseSiteSearchResult3;
    procedure TestParseSiteSearchResult4;
    procedure TestParseSiteSearchResult5;
    procedure TestParseSiteSearchResult6;
  end;

implementation

uses
  SysUtils, Classes, DateUtils, Generics.Collections, mystrings;

{ TTestMyStrings }

procedure TTestMyStrings.TestTEnumToString;
type
  TTestEnum = (teDEFAULT, teONE, teTWO, teThree, TEfour);
  //TTestWrongEnum = (tWeDEF, tWeNotThere);
var
  fExpectedResult, fOutputStr: String;
begin
  fOutputStr := TEnum<TTestEnum>.ToString(teTWO);
  fExpectedResult := 'teTWO';
  CheckEquals(fOutputStr, fExpectedResult, 'Enum to String conversion error');
  CheckEquals(TEnum<TTestEnum>.ToString(teone), 'teONE', 'Enum to String conversion error');
  CheckEquals(TEnum<TTestEnum>.ToString(tefour), 'TEfour', 'Enum to String conversion error');

  // does not compile as expected: Undeclared identifier: 'teNotThere'
  //CheckEquals(TEnum<TTestEnum>.ToString(teNotThere), 'teNotThere', 'Enum to String conversion error');
  // does not compile as expected: Incompatible types: 'TTestEnum' and 'TTestWrongEnum'
  //CheckEquals(TEnum<TTestEnum>.ToString(tWeNotThere), 'tWeNotThere', 'Enum to String conversion error');
end;

procedure TTestMyStrings.TestTEnumFromString;
type
  TTestEnum = (teDEFAULT, teONE, teTWO, teThree, TEfour);
  //TTestWrongEnum = (tWeDEF, tWeNotThere);
begin
  CheckEquals(Ord(TEnum<TTestEnum>.FromString('teTWO', teDEFAULT)), Ord(teTWO), 'String to Enum conversion error');
  CheckEquals(Ord(TEnum<TTestEnum>.FromString('teone', teDEFAULT)), Ord(teONE), 'String to Enum conversion error');
  CheckEquals(Ord(TEnum<TTestEnum>.FromString('tefour', teDEFAULT)), Ord(TEfour), 'String to Enum conversion error');
  CheckEquals(Ord(TEnum<TTestEnum>.FromString('teNotThere', teDEFAULT)), Ord(teDEFAULT), 'String to Enum conversion error');

  // does not compile as expected: Incompatible types: 'TTestEnum' and 'TTestWrongEnum'
  //CheckEquals(TEnum<TTestEnum>.FromString('tWeNotThere', tWeDEF), tWeNotThere, 'String to Enum conversion error');
end;

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

procedure TTestMyStrings.TestParseXDupeResponseToFilenameListGlftpd;
var
  fResp: TArray<String>;
  fFileList: TList<String>;
  fExpectedFileList: TArray<String>;
  i: Integer;
begin
  // glftpd x-dupe
  fResp := TArray<String>.Create('stor 01-absolution-resurrection-8ab20e53.mp3',
    '553- X-DUPE: 01-absolution-resurrection-8ab20e53.mp3',
    '553- X-DUPE: 02-absolution-resurrection_(audioappear_and_fuli_remix)-7e2f98a1.mp3',
    '553- X-DUPE: 03-absolution-resurrection_(fortytwo_remix)-fde9ba66.mp3',
    '553- 01-absolution-resurrection-8ab20e53.mp3: This file looks like a dupe!',
    '553 It was uploaded by <user> ( 3h 21m ago).');

  fExpectedFileList := TArray<String>.Create('01-absolution-resurrection-8ab20e53.mp3',
    '02-absolution-resurrection_(audioappear_and_fuli_remix)-7e2f98a1.mp3',
    '03-absolution-resurrection_(fortytwo_remix)-fde9ba66.mp3');

  fFileList := TList<String>.Create;
  try
    CheckTrue(ParseXDupeResponseToFilenameList(String.Join(#10, fResp), fFileList), 'No files parsed'); // different join parameter is intended

    CheckEquals(High(fExpectedFileList) + 1, fFileList.Count, 'Wrong amount of parsed X-DUPE files');

    for i := 0 to fFileList.Count - 1 do
    begin
      CheckEquals(fExpectedFileList[i], fFileList[i], 'Filenames not matching');
    end;
  finally
    fFileList.Free;
  end;
end;

procedure TTestMyStrings.TestParseXDupeResponseToFilenameListDrftpd;
var
  fResp: TArray<String>;
  fFileList: TList<String>;
  fExpectedFileList: TArray<String>;
  i: Integer;
begin
  // drftpd x-dupe
  fResp := TArray<String>.Create('stor 01-absolution-resurrection-8ab20e53.mp3',
    '553- X-DUPE: 00-absolution-resurrection-(hwr189)-ep-web-2019.jpg',
    '553- X-DUPE: 00-absolution-resurrection-(hwr189)-ep-web-2019.m3u',
    '553- X-DUPE: 00-absolution-resurrection-(hwr189)-ep-web-2019.nfo',
    '553- X-DUPE: 00-absolution-resurrection-(hwr189)-ep-web-2019.sfv',
    '553- X-DUPE: 01-absolution-resurrection-8ab20e53.mp3',
    '553- X-DUPE: 02-absolution-resurrection_(audioappear_and_fuli_remix)-7e2f98a1.mp3',
    '553- X-DUPE: 03-absolution-resurrection_(fortytwo_remix)-fde9ba66.mp3',
    '553 Requested action not taken. File exists.');

  fExpectedFileList := TArray<String>.Create('00-absolution-resurrection-(hwr189)-ep-web-2019.jpg',
    '00-absolution-resurrection-(hwr189)-ep-web-2019.m3u',
    '00-absolution-resurrection-(hwr189)-ep-web-2019.nfo',
    '00-absolution-resurrection-(hwr189)-ep-web-2019.sfv',
    '01-absolution-resurrection-8ab20e53.mp3',
    '02-absolution-resurrection_(audioappear_and_fuli_remix)-7e2f98a1.mp3',
    '03-absolution-resurrection_(fortytwo_remix)-fde9ba66.mp3');

  fFileList := TList<String>.Create;
  try
    CheckTrue(ParseXDupeResponseToFilenameList(String.Join(#13, fResp), fFileList), 'No files parsed'); // different join parameter is intended

    CheckEquals(High(fExpectedFileList) + 1, fFileList.Count, 'Wrong amount of parsed X-DUPE files');

    for i := 0 to fFileList.Count - 1 do
    begin
      CheckEquals(fExpectedFileList[i], fFileList[i], 'Filenames not matching');
    end;
  finally
    fFileList.Free;
  end;
end;

procedure TTestMyStrings.TestParseXDupeResponseToFilenameListMultislaveftpd;
var
  fResp: TArray<String>;
  fFileList: TList<String>;
  fExpectedFileList: TArray<String>;
  i: Integer;
begin
  // multislave FTP x-dupe
  fResp := TArray<String>.Create('PRET STOR godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r92',
    '553- X-DUPE: godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.sfv',
    '553- X-DUPE: godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.nfo',
    '553- X-DUPE: godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r92',
    '553- X-DUPE: godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r03',
    '553 Requested action not taken. File exists.',
    'Skip godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r92');

  fExpectedFileList := TArray<String>.Create('godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.sfv',
    'godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.nfo',
    'godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r92',
    'godzilla.king.of.the.monsters.2019.1080p.bluray.x264-sparks.r03');

  fFileList := TList<String>.Create;
  try
    CheckTrue(ParseXDupeResponseToFilenameList(String.Join(#13#10, fResp), fFileList), 'No files parsed'); // different join parameter is intended

    CheckEquals(High(fExpectedFileList) + 1, fFileList.Count, 'Wrong amount of parsed X-DUPE files');

    for i := 0 to fFileList.Count - 1 do
    begin
      CheckEquals(fExpectedFileList[i], fFileList[i], 'Filenames not matching');
    end;
  finally
    fFileList.Free;
  end;
end;

procedure TTestMyStrings.TestParseEPSVStringIPv41;
var
  fLine: String;
  fHost, fExpecHost: String;
  fPort, fExpecPort: Integer;
  fIPv4Mode, fExpecIPv4: Boolean;
begin
  fLine := '229 Entering Extended Passive Mode (|1|106.208.152.89|21073|)';
  fExpecHost := '106.208.152.89';
  fExpecPort := 21073;
  fExpecIPv4 := True;

  CheckTrue(ParseEPSVString(fLine, fHost, fPort, fIPv4Mode), 'Parsing should be successful');
  CheckEquals(fExpecHost, fHost);
  CheckEquals(fExpecPort, fPort);
  CheckEquals(fExpecIPv4, fIPv4Mode);
end;

procedure TTestMyStrings.TestParseEPSVStringIPv42;
var
  fLine: String;
  fHost, fExpecHost: String;
  fPort, fExpecPort: Integer;
  fIPv4Mode, fExpecIPv4: Boolean;
begin
  fLine := '229 Entering Extended Passive Mode (|1|214.102.219.112|60629|)';
  fExpecHost := '214.102.219.112';
  fExpecPort := 60629;
  fExpecIPv4 := True;

  CheckTrue(ParseEPSVString(fLine, fHost, fPort, fIPv4Mode), 'Parsing should be successful');
  CheckEquals(fExpecHost, fHost);
  CheckEquals(fExpecPort, fPort);
  CheckEquals(fExpecIPv4, fIPv4Mode);
end;

procedure TTestMyStrings.TestParseEPSVStringIPv43;
var
  fLine: String;
  fHost, fExpecHost: String;
  fPort, fExpecPort: Integer;
  fIPv4Mode, fExpecIPv4: Boolean;
begin
  fLine := '229 Entering Extended Passive Mode (|1|111.205.176.99|46938|)';
  fExpecHost := '111.205.176.99';
  fExpecPort := 46938;
  fExpecIPv4 := True;

  CheckTrue(ParseEPSVString(fLine, fHost, fPort, fIPv4Mode), 'Parsing should be successful');
  CheckEquals(fExpecHost, fHost);
  CheckEquals(fExpecPort, fPort);
  CheckEquals(fExpecIPv4, fIPv4Mode);
end;

procedure TTestMyStrings.TestParseSTATLine1;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.09 Unlimited
  fStatLine := '226  [Section: DEFAULT] [Credits: 14.6MB] [Ratio: UL&DL: Unlimited]';
  fExpectedCredits := '14.60 MB';
  fExpectedRatio := 'Unlimited';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine2;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.09 MB Ratio
  fStatLine := '226  [Section: DEFAULT] [Credits: 14.6MB] [Ratio: UL: 1:3 | DL: 1:1]';
  fExpectedCredits := '14.60 MB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine3;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.09 MB Ratio MiB -> GB
  fStatLine := '226  [Section: DEFAULT] [Credits: 1400.6MB] [Ratio: UL: 1:3 | DL: 1:1]';
  fExpectedCredits := '1.37 GB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine4;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.10 MiB Ratio
  fStatLine := '226  [Section: DEFAULT] [Credits: 14.6MiB] [Ratio: UL: 1:3 | DL: 1:1]';
  fExpectedCredits := '14.60 MB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine5;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.10 GiB Ratio
  fStatLine := '226  [Section: DEFAULT] [Credits: 14.6GiB] [Ratio: UL: 1:3 | DL: 1:1]';
  fExpectedCredits := '14.60 GB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine6;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // GL 2.10 MiB Ratio -> GB
  fStatLine := '226  [Section: DEFAULT] [Credits: 1400.6MiB] [Ratio: UL: 1:3 | DL: 1:1]';
  fExpectedCredits := '1.37 GB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine7;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // DrFTPd leech
  fStatLine := '200-      [Credits: 1.1TB] [Ratio: 1:0.0]';
  fExpectedCredits := '1.10 TB';
  fExpectedRatio := 'Unlimited';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine8;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // DrFTPd
  fStatLine := '200-      [Credits: 4.9TB] [Ratio: 1:3.0]';
  fExpectedCredits := '4.90 TB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine9;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // glftpd Credits mb -> gb
  fStatLine := '226  daydn(0.0mb) weekdn(0.0mb) monthdn(0.0mb) alup(1471.5mb) aldn(422927.2mb) credits(1476624.8mb) ratio(UL: 1:3 | DL: 1:1)';
  fExpectedCredits := '1.41 TB';
  fExpectedRatio := '1:3';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestParseSTATLine10;
var
  fStatLine, fExpectedCredits, fExpectedRatio, fCredits, fRatio: String;
begin
  // glftpd negative credits
  fStatLine := '[Section: DEFAULT] [Credits: -106.6MB] [Ratio: UL&DL: Unlimited]';
  fExpectedCredits := '-106.60 MB';
  fExpectedRatio := 'Unlimited';
  parseSTATLine(fStatLine, fCredits, fRatio);

  CheckEqualsString(fExpectedCredits, fCredits);
  CheckEqualsString(fExpectedRatio, fRatio);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars1;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt6586440/
  fMovieName := 'Ein Lächeln nachts um vier';
  fExpectedStr := 'Ein Laecheln nachts um vier';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars2;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt0566334/
  fMovieName := '&quot;The Drew Carey Show&quot; Bananas: Part 2';
  fExpectedStr := 'The Drew Carey Show Bananas Part 2';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars3;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt13649700/
  fMovieName := 'Crack: Cocaine, Corruption & Conspiracy';
  fExpectedStr := 'Crack Cocaine Corruption Conspiracy';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars4;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt12384470/
  fMovieName := '&quot;Cinematic Venom Presents: 1001 Movies You Must See Before You Die&quot; Whiplash';
  fExpectedStr := 'Cinematic Venom Presents 1001 Movies You Must See Before You Die Whiplash';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars5;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt13639672/
  fMovieName := 'Matthew Bourne''s Romeo and Juliet';
  fExpectedStr := 'Matthew Bournes Romeo and Juliet';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars6;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt13649700/
  fMovieName := '&quot;37 Grad&quot; Auf der Spur der Täter - Delikt Kinderpornografie';
  fExpectedStr := '37 Grad Auf der Spur der Taeter Delikt Kinderpornografie';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars7;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt3450958/
  fMovieName := 'War for the Planet of the Apes';
  fExpectedStr := 'War for the Planet of the Apes';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestInternationalCharsToAsciiSceneChars8;
var
  fMovieName, fExpectedStr, fAsciiToScene: String;
begin
  // https://www.imdb.com/title/tt12885852/
  fMovieName := 'Batman: Soul of the Dragon';
  fExpectedStr := 'Batman Soul of the Dragon';
  fAsciiToScene := InternationalCharsToAsciiSceneChars(fMovieName);

  CheckEqualsString(fExpectedStr, fAsciiToScene);
end;

procedure TTestMyStrings.TestParseSiteSearchResult1;
var
  fResp: TArray<String>;
  fStringList: TStringList;
begin
  //glftpd
  fResp := TArray<String>.Create('200- (Values displayed after dir names are Files/Megs/Age)',
    '200- Doing case-insensitive search for ''Test.Release-ASDF'':',
    '200- /ARCHIVE/SECTION/Test.Release-ASDF (4F/119.6M/106d 7h)',
    '200- /INCOMING/SECTION/Test.Release-ASDF (4F/119.6M/106d 7h)',
    '200-',
    '200 2 directories found.');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(2, fStringList.Count);
    CheckEquals('/ARCHIVE/SECTION/Test.Release-ASDF', fStringList[0]);
    CheckEquals('/INCOMING/SECTION/Test.Release-ASDF', fStringList[1]);
  finally
    fStringList.Free;
  end;
end;

procedure TTestMyStrings.TestParseSiteSearchResult2;
var
  fResp: TArray<String>;
  fStringList: TStringList;
begin
  //glftpd
  fResp := TArray<String>.Create('200- (Values displayed after dir names are Files/Megs/Age)',
    '200- Doing case-insensitive search for ''Test.Release-ASDF'':',
    '200- /REQUESTS/_FILLED/FILLED-Test.Release-ASDF/Test.Release-ASDF/Sample (1F/154.3M/58d 18h)',
    '200- /REQUESTS/_FILLED/FILLED-Test.Release-ASDF (85F/19354.7M/58d 18h)',
    '200- /REQUESTS/_FILLED/FILLED-Test.Release-ASDF/Test.Release-ASDF (85F/19354.7M/58d 18h)',
    '200-',
    '200 2 directories found.');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(1, fStringList.Count);
    CheckEquals('/REQUESTS/_FILLED/FILLED-Test.Release-ASDF/Test.Release-ASDF', fStringList[0]);
  finally
    fStringList.Free;
  end;
end;

procedure TTestMyStrings.TestParseSiteSearchResult3;
var
  fResp: TArray<String>;
  fStringList: TStringList;
begin
  //drftpd
  fResp := TArray<String>.Create('200- Found 1 entries in index (limit 200):',
    '200- /SECTION/Test.Release-ASDF',
    '200 Search complete ');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(1, fStringList.Count);
    CheckEquals('/SECTION/Test.Release-ASDF', fStringList[0]);
  finally
    fStringList.Free;
  end;
end;

procedure TTestMyStrings.TestParseSiteSearchResult4;
var
  fResp: TArray<String>;
  fStringList: TStringList;
  stri: String;
begin
  //ioftpd
  fResp := TArray<String>.Create('200- /SECTION/Test.Release-ASDF',
    '200 Command successful.');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(1, fStringList.Count);
    CheckEquals('/SECTION/Test.Release-ASDF', fStringList[0]);
  finally
    fStringList.Free;
  end;
end;

procedure TTestMyStrings.TestParseSiteSearchResult5;
var
  fResp: TArray<String>;
  fStringList: TStringList;
  stri: String;
begin
  //glftpd nuked dir
  fResp := TArray<String>.Create('200- Doing case-insensitive search for ''Test.Release-ASDF'':',
    '200- /SECTION/Test.Release-ASDF *NUKED*',
    '200- /SECTION/Test.Release-ASDF/Sample (1F/22.7M/26d 21h)',
    '200- /SECTION/(incomplete)-Test.Release-ASDF (49F/2253.1M/26d 21h)',
    '200-',
    '200 3 directories found.');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(0, fStringList.Count);
  finally
    fStringList.Free;
  end;
end;

procedure TTestMyStrings.TestParseSiteSearchResult6;
var
  fResp: TArray<String>;
  fStringList: TStringList;
  stri: String;
begin
  //drftpd nuked dir
  fResp := TArray<String>.Create('200- Found 1 entries in index (limit 50):',
    '200- /0DAY/0612/[NUKED]-Test.Release-ASDF',
    '200 Search complete');

  fStringList := TStringList.Create;
  try
    fStringList.Text := ParsePathFromSiteSearchResult(String.Join(#13#10, fResp), 'Test.Release-ASDF');

    CheckEquals(0, fStringList.Count);
  finally
    fStringList.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings', TTestMyStrings.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestMyStrings);
  {$ENDIF}
end.
