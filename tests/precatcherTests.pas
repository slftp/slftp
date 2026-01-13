unit precatcherTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestPrecatcher = class(TTestCase)
  published
    procedure TestFindSection1;
    procedure TestFindSection2;
    procedure TestFindSection3;
    procedure TestFindSection4;
    procedure TestProcessMappings_RegexPattern;
    procedure TestProcessMappings_MultiplePatterns;
    procedure TestProcessMappings_NoRegex;
    procedure TestProcessMappings_EmptyThirdField;
    procedure TestProcessMappings_CommentedLine;
    procedure TestProcessMappings_InvalidFormat;
  end;

implementation

uses
  SysUtils, precatcher;

{ TTestPrecatcher }

procedure TTestPrecatcher.TestFindSection1;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEWRACE - ENG-TV - Family.Feud.AU.2016.03.24.WEB.h264-spamTV - started by foob4r';
  fExpectedSection := 'ENGTV';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection2;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'New ChArTs - VA-Swiss_Top_100_Single_Charts_28.11.2021-AUDiAL_iNT - started by boon';
  fExpectedSection := 'MP3';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection3;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEW GER-CHARTS - VA-Swiss_Top_100_Single_Charts_28.11.2021-AUDiAL_iNT - started by foob4r';
  fExpectedSection := 'CHARTS';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection4;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEW in TV-HD - Tacoma.FD.S03E02.1080p.WEB.h264-KOGi by poweruser';
  fExpectedSection := '';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestProcessMappings_RegexPattern;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := 'MP3;FLAC;/.*\.mp3$/i';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount + 1, mappingslist.Count, 'Should add one mapping');
end;

procedure TTestPrecatcher.TestProcessMappings_MultiplePatterns;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := 'MP3;FLAC;/.*\.mp3$/i,/.*\.flac$/i';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount + 2, mappingslist.Count, 'Should add two mappings for two regex patterns');
end;

procedure TTestPrecatcher.TestProcessMappings_NoRegex;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := 'MP3;FLAC;test1,test2';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount + 2, mappingslist.Count, 'Should add two mappings for comma-separated strings');
end;

procedure TTestPrecatcher.TestProcessMappings_EmptyThirdField;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := 'MP3;FLAC;';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount + 1, mappingslist.Count, 'Should add one mapping even with empty third field');
end;

procedure TTestPrecatcher.TestProcessMappings_CommentedLine;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := '# MP3;FLAC;/.*\.mp3$/i';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount, mappingslist.Count, 'Commented lines should be skipped');
end;

procedure TTestPrecatcher.TestProcessMappings_InvalidFormat;
var
  fInput: String;
  fInitialCount: Integer;
begin
  fInitialCount := mappingslist.Count;
  fInput := 'MP3;FLAC';

  ProcessMappings(fInput);

  CheckEquals(fInitialCount, mappingslist.Count, 'Invalid format (less than 2 semicolons) should not add mapping');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('precatcher', TTestPrecatcher.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestPrecatcher);
  {$ENDIF}
end.
