unit tasktvinfolookupTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTVMazeSearchHelpers = class(TTestCase)
  published
    procedure TestReduceShowNameByOneWord;
    procedure TestCannotReduceSingleWord;
    procedure TestSafePrefixMatchForLocalizedTitle;
    procedure TestRejectUnrelatedPrefix;
    procedure TestRejectLowScorePrefix;
    procedure TestRejectGenericShortPrefix;
  end;

implementation

uses
  tasktvinfolookup;

procedure TTestTVMazeSearchHelpers.TestReduceShowNameByOneWord;
var
  showName: String;
begin
  showName := 'Murdoch.Mysteries.Auf.den.Spuren';
  CheckTrue(TVMazeReduceShowNameByOneWord(showName));
  CheckEquals('Murdoch.Mysteries.Auf.den', showName);
end;

procedure TTestTVMazeSearchHelpers.TestCannotReduceSingleWord;
var
  showName: String;
begin
  showName := 'Murdoch';
  CheckFalse(TVMazeReduceShowNameByOneWord(showName));
  CheckEquals('Murdoch', showName);
end;

procedure TTestTVMazeSearchHelpers.TestSafePrefixMatchForLocalizedTitle;
begin
  CheckTrue(TVMazeIsSafePrefixMatch(
    'Murdoch.Mysteries.Auf.den.Spuren.mysterioeser.Mordfaelle',
    'Murdoch Mysteries', 0.8));
end;

procedure TTestTVMazeSearchHelpers.TestRejectUnrelatedPrefix;
begin
  CheckFalse(TVMazeIsSafePrefixMatch(
    'Murdoch.Mysteries.Auf.den.Spuren',
    'Middag Paa Michelin Restauranterne', 0.99));
end;

procedure TTestTVMazeSearchHelpers.TestRejectLowScorePrefix;
begin
  CheckFalse(TVMazeIsSafePrefixMatch(
    'Murdoch.Mysteries.Auf.den.Spuren',
    'Murdoch Mysteries', 0.69));
end;

procedure TTestTVMazeSearchHelpers.TestRejectGenericShortPrefix;
begin
  CheckFalse(TVMazeIsSafePrefixMatch(
    'It.Happened.To.Me',
    'It', 1.0));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('tvmaze search', TTestTVMazeSearchHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTVMazeSearchHelpers);
  {$ENDIF}

end.
