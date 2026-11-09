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
    procedure TestExactMatchWinsOverPrefix;
    procedure TestAmbiguityStopsReduction;
    procedure TestPrefixAmbiguity;
    procedure TestStrictModeRejectsPrefix;
    procedure TestYearAndCountryConstraints;
    procedure TestMalformedResponseIsNotAMiss;
    procedure TestNextQueryBoundAndErrors;
  end;

implementation

uses
  Classes, tasktvinfolookup;

function _Parse(const aJson, aName: String; out aId: String;
  const aAllowPrefix: Boolean = True; const aYear: String = '';
  const aCountry: String = ''): TTVMazeSearchStatus;
var
  fResults: TStringList;
begin
  fResults := TStringList.Create;
  try
    Result := TVMazeParseSearchResponse(aJson, aName, aYear, aCountry,
      aYear <> '', aCountry <> '', False, aAllowPrefix, fResults, aId);
  finally
    fResults.Free;
  end;
end;

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


procedure TTestTVMazeSearchHelpers.TestExactMatchWinsOverPrefix;
var
  fId: String;
begin
  CheckEquals(Ord(tvssMatched), Ord(_Parse(
    '[{"score":0.9,"show":{"id":1,"name":"Murdoch Mysteries"}},' +
    '{"score":0.8,"show":{"id":2,"name":"Murdoch Mysteries Special"}}]',
    'Murdoch.Mysteries.Special', fId)));
  CheckEquals('2', fId);
end;

procedure TTestTVMazeSearchHelpers.TestAmbiguityStopsReduction;
var
  fId, fQuery: String;
  fAttempt: Integer;
  fStatus: TTVMazeSearchStatus;
begin
  fStatus := _Parse(
    '[{"show":{"id":1,"name":"The Office"}},{"show":{"id":2,"name":"The Office"}}]',
    'The.Office', fId);
  CheckEquals(Ord(tvssAmbiguous), Ord(fStatus));
  CheckEquals('', fId);
  fQuery := 'The.Office';
  fAttempt := 1;
  CheckFalse(TVMazeNextQuery(fQuery, fAttempt, fStatus));
  CheckEquals('The.Office', fQuery);
  CheckEquals(1, fAttempt);
  // Duplicate records for the same show are not ambiguous.
  CheckEquals(Ord(tvssMatched), Ord(_Parse(
    '[{"show":{"id":1,"name":"The Office"}},{"show":{"id":1,"name":"The Office"}}]',
    'The.Office', fId)));
  CheckEquals('1', fId);
end;

procedure TTestTVMazeSearchHelpers.TestPrefixAmbiguity;
var
  fId: String;
begin
  CheckEquals(Ord(tvssAmbiguous), Ord(_Parse(
    '[{"score":0.9,"show":{"id":1,"name":"Murdoch Mysteries"}},' +
    '{"score":0.9,"show":{"id":2,"name":"Murdoch Mysteries"}}]',
    'Murdoch.Mysteries.Auf.den.Spuren', fId)));
  CheckEquals('', fId);
end;

procedure TTestTVMazeSearchHelpers.TestStrictModeRejectsPrefix;
const
  fJson = '[{"score":0.9,"show":{"id":1,"name":"Murdoch Mysteries"}}]';
var
  fId: String;
begin
  CheckEquals(Ord(tvssMatched), Ord(_Parse(fJson,
    'Murdoch.Mysteries.Auf.den.Spuren', fId, True)));
  CheckEquals('1', fId);
  CheckEquals(Ord(tvssNoMatch), Ord(_Parse(fJson,
    'Murdoch.Mysteries.Auf.den.Spuren', fId, False)));
  CheckEquals('', fId);
  // A strict original-title match remains valid even at a low/missing score.
  CheckEquals(Ord(tvssMatched), Ord(_Parse(
    '[{"show":{"id":1,"name":"Murdoch Mysteries"}}]',
    'Murdoch.Mysteries', fId, False)));
end;

procedure TTestTVMazeSearchHelpers.TestYearAndCountryConstraints;
const
  fJson = '[{"show":{"id":1,"name":"The Office","premiered":"2001-07-09",' +
    '"network":{"country":{"code":"GB"}},"webChannel":null}},' +
    '{"show":{"id":2,"name":"The Office","premiered":"2005-03-24",' +
    '"network":{"country":{"code":"US"}},"webChannel":null}}]';
var
  fId: String;
begin
  CheckEquals(Ord(tvssMatched), Ord(_Parse(fJson, 'The.Office', fId, False, '2001', 'UK')));
  CheckEquals('1', fId);
  CheckEquals(Ord(tvssMatched), Ord(_Parse(fJson, 'The.Office', fId, True, '2005', 'US')));
  CheckEquals('2', fId);
  CheckEquals(Ord(tvssNoMatch), Ord(_Parse(fJson, 'The.Office', fId, True, '2001', 'US')));
  CheckEquals('', fId);
  // Tags must not be dropped when later shortened queries return a prefix.
  CheckEquals(Ord(tvssNoMatch), Ord(_Parse(fJson, 'The.Office.Extra', fId, True, '', 'UK')));
end;

procedure TTestTVMazeSearchHelpers.TestMalformedResponseIsNotAMiss;
var
  fId: String;
begin
  CheckEquals(Ord(tvssError), Ord(_Parse('', 'Example', fId)));
  CheckEquals(Ord(tvssError), Ord(_Parse('{}', 'Example', fId)));
  CheckEquals(Ord(tvssError), Ord(_Parse('[{}]', 'Example', fId)));
  CheckEquals(Ord(tvssNoMatch), Ord(_Parse('[]', 'Example', fId)));
  CheckEquals('', fId);
end;

procedure TTestTVMazeSearchHelpers.TestNextQueryBoundAndErrors;
var
  fQuery: String;
  fAttempt, i: Integer;
begin
  fQuery := 'One.Two.Three.Four.Five.Six';
  fAttempt := 1;
  CheckFalse(TVMazeNextQuery(fQuery, fAttempt, tvssError));
  CheckFalse(TVMazeNextQuery(fQuery, fAttempt, tvssMatched));
  CheckEquals(1, fAttempt);
  for i := 2 to 5 do
  begin
    CheckTrue(TVMazeNextQuery(fQuery, fAttempt, tvssNoMatch));
    CheckEquals(i, fAttempt);
  end;
  CheckEquals('One.Two', fQuery);
  CheckFalse(TVMazeNextQuery(fQuery, fAttempt, tvssNoMatch));
  CheckEquals('One.Two', fQuery);
  fQuery := 'One';
  fAttempt := 1;
  CheckFalse(TVMazeNextQuery(fQuery, fAttempt, tvssNoMatch));
  CheckEquals(1, fAttempt);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('tvmaze search', TTestTVMazeSearchHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTVMazeSearchHelpers);
  {$ENDIF}

end.
