unit taskhttpimdbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTImdbDataProcessor = class(TTestCase)
  private
    function LoadResource(const aResName: String): String;
  published
    procedure TestProcess_WarForThePlanetOfTheApes; // tt3450958 (Standard Movie)
    procedure TestProcess_PrisonBreak;              // tt0455275 (TV Series)
    procedure TestProcess_MarvelRising;             // tt7728344 (TV Movie / STV)
    procedure TestCountryOrder_WarForThePlanetOfTheApes; // Validates country order for rules
  end;

implementation

uses
  SysUtils, Classes, Variants, mormot.core.variants, taskhttpimdb, dbaddimdb;

{$IFDEF FPC}
  {$R taskhttpimdbTests.rc}
{$ELSE}
  {$R taskhttpimdbTests.res}
{$ENDIF}

{ TTestTImdbDataProcessor }

function TTestTImdbDataProcessor.LoadResource(const aResName: String): String;
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, aResName, RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      Result := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTImdbDataProcessor.TestProcess_WarForThePlanetOfTheApes;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
begin
  fTitleJson := _JsonFast(LoadResource('tt3450958_Main'));
  if VarIsNull(fTitleJson) then Fail('Failed to load tt3450958_Main JSON');
  
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, fImdbData);

  try
    CheckEquals(2017, fImdbData.imdb_year, 'IMDB Year mismatch');
    CheckEqualsString('War for the Planet of the Apes', fImdbData.imdb_origtitle);
    CheckFalse(fImdbData.imdb_stvm, 'Should not be STV');
    CheckEquals(2017, fImdbData.imdb_cineyear, 'IMDB CineYear mismatch');

    // Country validation
    // Note: HTML parser had "USA", API now returns "USA,Canada" (more data - improvement)
    CheckEqualsString('USA,Canada', fImdbData.imdb_countries.DelimitedText, 'Countries mismatch');
    CheckTrue(fImdbData.imdb_countries.IndexOf('USA') >= 0, 'USA should be present');
    CheckTrue(fImdbData.imdb_countries.IndexOf('Canada') >= 0, 'Canada should be present');
  finally
    fImdbData.Free;
  end;
end;

procedure TTestTImdbDataProcessor.TestProcess_PrisonBreak;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
begin
  fTitleJson := _JsonFast(LoadResource('tt0455275_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt0455275_ReleaseDates'));

  TImdbDataProcessor.Process('Prison.Break.S01E01.Pilot.720p.BluRay.x264-GRP', 'tt0455275', fTitleJson, fReleaseDatesJson, fImdbData);

  try
    CheckEqualsString('Prison Break', fImdbData.imdb_origtitle);
    CheckEquals(2005, fImdbData.imdb_year);
    // Prison Break is a TV Series, currently our logic might mark it STV or just series.
    // In our new logic: type=TV_SERIES and it IS a TV show (S01E01) -> STV = True
    CheckTrue(fImdbData.imdb_stvm, 'TV series releases should be marked as STV in slftp logic');

    // Country validation
    // CRITICAL: HTML parser had "UK,USA", API only returns "USA" - UK is MISSING!
    // This is a known API limitation - UK data lost compared to old HTML scraping
    CheckEqualsString('USA', fImdbData.imdb_countries.DelimitedText, 'Countries mismatch');
    CheckTrue(fImdbData.imdb_countries.IndexOf('USA') >= 0, 'USA should be present');
    CheckEquals(-1, fImdbData.imdb_countries.IndexOf('UK'), 'UK is missing from API (known issue)');
  finally
    fImdbData.Free;
  end;
end;

procedure TTestTImdbDataProcessor.TestProcess_MarvelRising;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
begin
  fTitleJson := _JsonFast(LoadResource('tt7728344_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt7728344_ReleaseDates'));

  TImdbDataProcessor.Process('Marvel.Rising.Secret.Warriors.2018.1080p.BluRay.x264-GRP', 'tt7728344', fTitleJson, fReleaseDatesJson, fImdbData);

  try
    // This is a TV Movie
    CheckTrue(fImdbData.imdb_stvm, 'Should be STV (TV Movie)');

    // Country validation
    // Note: HTML parser had "USA", API also returns "USA" (consistent)
    CheckEqualsString('USA', fImdbData.imdb_countries.DelimitedText, 'Countries mismatch');
    CheckTrue(fImdbData.imdb_countries.IndexOf('USA') >= 0, 'USA should be present');
  finally
    fImdbData.Free;
  end;
end;

procedure TTestTImdbDataProcessor.TestCountryOrder_WarForThePlanetOfTheApes;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
begin
  // This test validates that country ORDER is consistent for rule matching
  // Rules using "imdbcountries = <country>" (equals operator) only match if country is at INDEX 0
  // See rulesunit.pas TMultiStringEqualOperator.Match() -> IndexOf(GetOperandValue) = 0

  fTitleJson := _JsonFast(LoadResource('tt3450958_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, fImdbData);

  try
    // Validate first country (index 0) - critical for rule matching
    CheckEquals(2, fImdbData.imdb_countries.Count, 'Should have 2 countries');
    CheckEqualsString('USA', fImdbData.imdb_countries[0], 'USA must be first country for rules');
    CheckEqualsString('Canada', fImdbData.imdb_countries[1], 'Canada must be second country');

    // Demonstrate rule behavior:
    // Rule "imdbcountries = USA" would MATCH (USA at index 0)
    // Rule "imdbcountries = Canada" would FAIL (Canada not at index 0)
    // Users should use "imdbcountries in Canada" instead
    CheckEquals(0, fImdbData.imdb_countries.IndexOf('USA'), 'USA at index 0 - rule "= USA" works');
    CheckEquals(1, fImdbData.imdb_countries.IndexOf('Canada'), 'Canada at index 1 - rule "= Canada" FAILS');
  finally
    fImdbData.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestTImdbDataProcessor', TTestTImdbDataProcessor.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTImdbDataProcessor);
  {$ENDIF}

end.