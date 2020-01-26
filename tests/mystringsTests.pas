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

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings', TTestMyStrings.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestMyStrings);
  {$ENDIF}
end.
