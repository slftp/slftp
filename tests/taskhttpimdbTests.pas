unit taskhttpimdbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework{$IFDEF MSWINDOWS}, Windows {$ENDIF};
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility{$IFDEF MSWINDOWS}, Windows {$ENDIF};
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
    procedure TestCountryNormalizationGermany;
  end;

  TTestTHtmlBoxOfficeMojoParser = class(TTestCase)
  published
    procedure TestGetWidestScreensCountNoneAvailable;
    procedure TestGetOriginalReleaseGroupLinkNotFound;
    procedure TestGetOriginalReleaseGroupLinkFound;
  end;

  { Ghostbusters (1984) - Has release groups }
  TTestTHtmlBoxOfficeMojoParser_tt0087332 = class(TTestCase)
  private
    FOverviewPage: String;
    FOriginalReleasePage: String;
    FUSAReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestGetOriginalReleaseGroupLink;
    procedure TestGetCountrySpecificLinksFromOriginalRelease;
    procedure TestGetWidestScreensCountUSA;
  end;

  { Screen count classification tests }
  TTestScreenCountClassification = class(TTestCase)
  private
    function LoadResource(const aResName: String): String;
  published
    procedure TestWideClassification;
    procedure TestLimitedClassification;
    procedure TestFallbackToUSA;
  end;

  { Papillon (2017) }
  TTestTHtmlBoxOfficeMojoParser_tt5093026 = class(TTestCase)
  private
    FOverviewPage: String;
    FFranceReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestGetCountrySpecificLinks;
    procedure TestGetWidestScreensCountUSA;
    procedure TestGetWidestScreensCountFrance;
    procedure TestGetWidestScreensCountBelgium;
  end;

  { Astro Boy (2009) }
  TTestTHtmlBoxOfficeMojoParser_tt0375568 = class(TTestCase)
  private
    FOverviewPage: String;
    FUSAReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestGetCountrySpecificLinks;
    procedure TestGetWidestScreensCountUSA;
    procedure TestGetWidestScreensCountUK;
  end;

  { War for the Planet of the Apes (2017) }
  TTestTHtmlBoxOfficeMojoParser_tt3450958 = class(TTestCase)
  private
    FOverviewPage: String;
    FGermanyReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestGetCountrySpecificLinks;
    procedure TestGetWidestScreensCountUSA;
    procedure TestGetWidestScreensCountSpain;
    procedure TestGetWidestScreensCountGermany;
  end;

implementation

uses
  SysUtils, Classes, Variants, mormot.core.variants, taskhttpimdb, dbaddimdb,
  Generics.Collections;

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
  CheckFalse(VarIsNull(fTitleJson), 'Failed to load tt3450958_Main JSON');
  
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, nil, fImdbData);

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

  TImdbDataProcessor.Process('Prison.Break.S01E01.Pilot.720p.BluRay.x264-GRP', 'tt0455275', fTitleJson, fReleaseDatesJson, nil, fImdbData);

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

  TImdbDataProcessor.Process('Marvel.Rising.Secret.Warriors.2018.1080p.BluRay.x264-GRP', 'tt7728344', fTitleJson, fReleaseDatesJson, nil, fImdbData);

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

procedure TTestTImdbDataProcessor.TestCountryNormalizationGermany;
var
  fTitleJson: Variant;
  fImdbData: TDbImdbData;
begin
  fTitleJson := _JsonFast('{"originCountries":[{"name":"West Germany"},{"name":"East Germany"}]}');

  TImdbDataProcessor.Process('Example.Release.1984', 'tt0000000', fTitleJson, Null, nil, fImdbData);

  try
    CheckEqualsString('Germany,Germany', fImdbData.imdb_countries.DelimitedText,
      'Historical German country names must normalize to Germany');
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

  TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, nil, fImdbData);

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

{ TTestTHtmlBoxOfficeMojoParser }

procedure TTestTHtmlBoxOfficeMojoParser.TestGetWidestScreensCountNoneAvailable;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(0, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetOriginalReleaseGroupLinkNotFound;
var
  fPageSource: String;
  fLink: String;
begin
  // Page without release groups
  fPageSource := '<html><body>No release groups here</body></html>';
  fLink := THtmlBoxOfficeMojoParser.GetOriginalReleaseGroupLink(fPageSource);
  CheckEqualsString('', fLink, 'Should return empty when no Original Release group');
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetOriginalReleaseGroupLinkFound;
var
  fPageSource: String;
  fLink: String;
begin
  // Simulated page with Original Release group
  fPageSource := '<a class="a-link-normal" href="/releasegroup/gr2193641989/">Original Release</a>';
  fLink := THtmlBoxOfficeMojoParser.GetOriginalReleaseGroupLink(fPageSource);
  CheckEqualsString('/releasegroup/gr2193641989', fLink, 'Should extract Original Release link');
end;

{ TTestTHtmlBoxOfficeMojoParser_tt5093026 }

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt5093026_BOM', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOverviewPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt5093026_BOMREL', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FFranceReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);
    CheckEquals(23, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl4094002689', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl3985016577', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl3783689985', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl4119234305', fBOMCountryLinks.Items['Germany'], 'Link mismatch');
    CheckEqualsString('/release/rl4152788737', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestGetWidestScreensCountUSA;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>544 theaters</span></div>';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(544, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestGetWidestScreensCountFrance;
var
  fScreens: Integer;
begin
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(FFranceReleasePage);
  CheckEquals(112, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestGetWidestScreensCountBelgium;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(0, fScreens, 'Screens count mismatch');
end;

{ TTestTHtmlBoxOfficeMojoParser_tt0375568 }

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0375568_BOM', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOverviewPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0375568_BOMREL', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FUSAReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);
    CheckEquals(27, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl3947005441', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl2452522497', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl2335081985', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl2620294657', fBOMCountryLinks.Items['Spain'], 'Link mismatch');
    CheckEqualsString('/release/rl2637071873', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.TestGetWidestScreensCountUSA;
var
  fScreens: Integer;
begin
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(FUSAReleasePage);
  CheckEquals(3020, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.TestGetWidestScreensCountUK;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>424 theaters</span></div>';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(424, fScreens, 'Screens count mismatch');
end;

{ TTestTHtmlBoxOfficeMojoParser_tt3450958 }

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt3450958_BOM', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOverviewPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt3450958_BOMREL', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FGermanyReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);
    CheckEquals(45, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl1782744577', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl3156968961', fBOMCountryLinks.Items['UK'], 'Link mismatch');
    CheckEqualsString('/release/rl1730905601', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl1261143553', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl1965786625', fBOMCountryLinks.Items['Germany'], 'Link mismatch');
    CheckEqualsString('/release/rl1831568897', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestGetWidestScreensCountUSA;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>4,100 theaters</span></div>';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(4100, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestGetWidestScreensCountSpain;
var
  fPageSource: String;
  fScreens: Integer;
begin
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>976 theaters</span></div>';
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);
  CheckEquals(976, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestGetWidestScreensCountGermany;
var
  fScreens: Integer;
begin
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(FGermanyReleasePage);
  CheckEquals(932, fScreens, 'Screens count mismatch');
end;

{ TTestTHtmlBoxOfficeMojoParser_tt0087332 - Ghostbusters }

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0087332_BOM', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOverviewPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0087332_BOMREL', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOriginalReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0087332_BOMUSA', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FUSAReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetOriginalReleaseGroupLink;
var
  fLink: String;
begin
  // Ghostbusters has multiple release groups - should find "Original Release"
  fLink := THtmlBoxOfficeMojoParser.GetOriginalReleaseGroupLink(FOverviewPage);
  CheckEqualsString('/releasegroup/gr2193641989', fLink, 'Should find Original Release group link');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetCountrySpecificLinksFromOriginalRelease;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  // Parse country links from Original Release page
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOriginalReleasePage, fBOMCountryLinks);
    // Original Release should have USA (Domestic) at minimum
    CheckTrue(fBOMCountryLinks.ContainsKey('USA'), 'USA should be present in Original Release');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetWidestScreensCountUSA;
var
  fScreens: Integer;
begin
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(FUSAReleasePage);
  // Ghostbusters had a wide theatrical release
  CheckTrue(fScreens > 0, 'Ghostbusters USA should have screens');
end;

{ TTestScreenCountClassification }

function TTestScreenCountClassification.LoadResource(const aResName: String): String;
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

procedure TTestScreenCountClassification.TestWideClassification;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
  fBomScreenCounts: TDictionary<String, Integer>;
begin
  // Load War for the Planet of the Apes data
  fTitleJson := _JsonFast(LoadResource('tt3450958_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  // Create BOM screen counts with Wide threshold (500+)
  fBomScreenCounts := TDictionary<String, Integer>.Create;
  try
    fBomScreenCounts.Add('USA', 4100); // Wide release

    TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, fBomScreenCounts, fImdbData);

    try
      CheckEquals(4100, fImdbData.imdb_screens, 'Screen count mismatch');
      CheckTrue(fImdbData.imdb_wide, 'Should be Wide (4100 >= 500)');
      CheckFalse(fImdbData.imdb_ldt, 'Should not be Limited');
      CheckFalse(fImdbData.imdb_stvm, 'Should not be STV');
    finally
      fImdbData.Free;
    end;
  finally
    fBomScreenCounts.Free;
  end;
end;

procedure TTestScreenCountClassification.TestLimitedClassification;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
  fBomScreenCounts: TDictionary<String, Integer>;
begin
  // Load War for the Planet of the Apes data
  fTitleJson := _JsonFast(LoadResource('tt3450958_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  // Create BOM screen counts with Limited threshold (250-499)
  fBomScreenCounts := TDictionary<String, Integer>.Create;
  try
    fBomScreenCounts.Add('USA', 350); // Limited release

    TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.1080p.BluRay.x264-CiNEFiLE', 'tt3450958', fTitleJson, fReleaseDatesJson, fBomScreenCounts, fImdbData);

    try
      CheckEquals(350, fImdbData.imdb_screens, 'Screen count mismatch');
      CheckFalse(fImdbData.imdb_wide, 'Should not be Wide');
      CheckTrue(fImdbData.imdb_ldt, 'Should be Limited (350 >= 250 and < 500)');
      CheckFalse(fImdbData.imdb_stvm, 'Should not be STV');
    finally
      fImdbData.Free;
    end;
  finally
    fBomScreenCounts.Free;
  end;
end;

procedure TTestScreenCountClassification.TestFallbackToUSA;
var
  fTitleJson, fReleaseDatesJson: Variant;
  fImdbData: TDbImdbData;
  fBomScreenCounts: TDictionary<String, Integer>;
begin
  // Load War for the Planet of the Apes data
  fTitleJson := _JsonFast(LoadResource('tt3450958_Main'));
  fReleaseDatesJson := _JsonFast(LoadResource('tt3450958_ReleaseDates'));

  // Create BOM screen counts with only USA (German release should fall back to USA)
  fBomScreenCounts := TDictionary<String, Integer>.Create;
  try
    fBomScreenCounts.Add('USA', 4100);
    // Note: Germany is NOT in the dictionary

    // German release - should fall back to USA screen count
    TImdbDataProcessor.Process('War.for.the.Planet.of.the.Apes.2017.GERMAN.DL.1080p.BluRay.x264-GRP', 'tt3450958', fTitleJson, fReleaseDatesJson, fBomScreenCounts, fImdbData);

    try
      CheckEquals(4100, fImdbData.imdb_screens, 'Should fall back to USA screen count');
      CheckTrue(fImdbData.imdb_wide, 'Should be Wide (using USA fallback)');
    finally
      fImdbData.Free;
    end;
  finally
    fBomScreenCounts.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestTImdbDataProcessor', TTestTImdbDataProcessor.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser', TTestTHtmlBoxOfficeMojoParser.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser_Papillon', TTestTHtmlBoxOfficeMojoParser_tt5093026.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser_AstroBoy', TTestTHtmlBoxOfficeMojoParser_tt0375568.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser_WarPlanet', TTestTHtmlBoxOfficeMojoParser_tt3450958.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser_Ghostbusters', TTestTHtmlBoxOfficeMojoParser_tt0087332.Suite);
    RegisterTest('TTestScreenCountClassification', TTestScreenCountClassification.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTImdbDataProcessor);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt5093026);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt0375568);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt3450958);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt0087332);
    TDUnitX.RegisterTestFixture(TTestScreenCountClassification);
  {$ENDIF}

end.
