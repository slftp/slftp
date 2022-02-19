unit taskhttpimdbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  // TODO: missing tests for release info page
  { The Da Vinci Code (2006) }
  TTestTHtmlIMDbParser_tt0382625 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestIsSTVBasedOnTitleExtraInfo;
  end;

  { "The Detour" The Pilot (TV Episode 2016) }
  TTestTHtmlIMDbParser_tt4919664 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestIsSTVBasedOnTitleExtraInfo;
  end;

  { Sam & Cat (TV Series 2013–2014) }
  TTestTHtmlIMDbParser_tt2487090 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
  end;

  { Hot Shots! Part Deux (1993) }
  TTestTHtmlIMDbParser_tt0107144 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
  end;

  { DR-Friland Nybyggerne Part 1 (TV Episode 2002) }
  TTestTHtmlIMDbParser_tt0816352 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseNoVotesAndNoRating;
  end;

  { The Violators (2015) }
  TTestTHtmlIMDbParser_tt3876702 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseVotesAndRating;
  end;

  { The Witcher 3: Wild Hunt - Blood and Wine (Video Game 2016) }
  TTestTHtmlIMDbParser_tt5667286 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestIsSTVBasedOnTitleExtraInfo;
  end;

  { The White Queen (TV Mini-Series 2013) }
  TTestTHtmlIMDbParser_tt2372220 = class(TTestCase)
  private
    FMainPage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestIsSTVBasedOnTitleExtraInfo;
  end;



  { War for the Planet of the Apes (2017) }
  TTestTHtmlIMDbParser_tt3450958 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  { Prison Break (TV Series 2005–2017) }
  TTestTHtmlIMDbParser_tt0455275 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  { Fear Challenge (2018) }
  TTestTHtmlIMDbParser_tt7214470 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  { Marvel Rising Secret Warriors (TV Movie 2018) }
  TTestTHtmlIMDbParser_tt7728344 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  { Boys State (2020) }
  TTestTHtmlIMDbParser_tt11095742 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  { Astro Boy (2009) }
  TTestTHtmlIMDbParser_tt0375568 = class(TTestCase)
  private
    FMainPage: String;
    FReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestParseMetaTitleInformation;
    procedure TestParseVotesAndRating;
    procedure TestParseMovieLanguage;
    procedure TestParseMovieCountries;
    procedure TestParseMovieGenres;
    procedure TestParseReleaseDateInfo;
    procedure TestParseAlsoKnownAsInfo;
  end;

  TTestTHtmlBoxOfficeMojoParser = class(TTestCase)
  published
    procedure TestGetWidestScreensCountNoneAvailable;
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
    procedure TestListsOnlyReleaseGroups;
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
    procedure TestListsOnlyReleaseGroups;
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
    procedure TestListsOnlyReleaseGroups;
    procedure TestGetCountrySpecificLinks;
    procedure TestGetWidestScreensCountUSA;
    procedure TestGetWidestScreensCountSpain;
    procedure TestGetWidestScreensCountGermany;
  end;

  { Ghostbusters (1984) }
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
    procedure TestListsOnlyReleaseGroups;
    procedure TestGetGroupSpecificLinks;
    procedure TestGetCountrySpecificLinks;
    procedure TestGetWidestScreensCountUSA;
  end;

  { The Death of Superman (2018) }
  TTestTHtmlBoxOfficeMojoParser_tt7167658 = class(TTestCase)
  private
    FOverviewPage: String;
    FOriginalReleasePage: String;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  published
    procedure TestListsOnlyReleaseGroups;
    procedure TestGetGroupSpecificLinks;
    procedure TestGetCountrySpecificLinks;
  end;

  TTestTIMDbInfoChecks = class(TTestCase)
  published
    procedure TestEstimateEnglishCountryOrder1;
    procedure TestEstimateEnglishCountryOrder2;
    procedure TestEstimateEnglishCountryOrder3;
    procedure TestEstimateEnglishCountryOrder4;
  end;

implementation

uses
  SysUtils, taskhttpimdb, dbaddimdb, Generics.Collections, {$IFNDEF FPC}Types,{$ENDIF} Classes;

{$IFDEF FPC}
  {$R taskhttpimdbTests.rc}
{$ELSE}
  {$R taskhttpimdbTests.res}
{$ENDIF}

procedure TTestTHtmlIMDbParser_tt0382625.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0382625_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0382625.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0382625'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('The Da Vinci Code', fMovieTitle, 'Title mismatch');
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2006, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0382625.TestIsSTVBasedOnTitleExtraInfo;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
  fIsSTV: Boolean;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0382625'), fMovieTitle, fTitleExtraInfo, fYear);
  fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(fTitleExtraInfo);
  CheckFalse(fIsSTV, 'STV mismatch');
end;

procedure TTestTHtmlIMDbParser_tt4919664.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt4919664_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt4919664.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt4919664'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('The Pilot', fMovieTitle, 'Title mismatch'); // TODO: strip html chars?
  CheckEqualsString('TV Episode', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2016, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt4919664.TestIsSTVBasedOnTitleExtraInfo;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
  fIsSTV: Boolean;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt4919664'), fMovieTitle, fTitleExtraInfo, fYear);
  fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(fTitleExtraInfo);
  CheckTrue(fIsSTV, 'STV mismatch');
end;

procedure TTestTHtmlIMDbParser_tt2487090.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt2487090_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt2487090.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt2487090'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Sam & Cat', fMovieTitle, 'Title mismatch'); // TODO: replace & with and?
  CheckEqualsString('TV Series', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2013, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0107144.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0107144_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0107144.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0107144'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Hot Shots! Part Deux', fMovieTitle, 'Title mismatch'); // TODO: strip ? and !?
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(1993, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0816352.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0816352_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0816352.TestParseNoVotesAndNoRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0816352'), fVotes, fRating);

  CheckEquals(0, fVotes, 'Votes mismatch');
  CheckEquals(0, fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3876702.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt3876702_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt3876702.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt3876702'), fVotes, fRating);

  CheckTrue(400 < fVotes, 'Votes mismatch');
  CheckTrue(5000 > fVotes, 'Votes mismatch');
  CheckTrue(60 < fRating, 'Rating mismatch');
  CheckTrue(62 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt5667286.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt5667286_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt5667286.TestIsSTVBasedOnTitleExtraInfo;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
  fIsSTV: Boolean;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt5667286'), fMovieTitle, fTitleExtraInfo, fYear);
  fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(fTitleExtraInfo);
  CheckTrue(fIsSTV, 'STV mismatch');
end;

procedure TTestTHtmlIMDbParser_tt2372220.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt2372220_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt2372220.TestIsSTVBasedOnTitleExtraInfo;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
  fIsSTV: Boolean;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt2372220'), fMovieTitle, fTitleExtraInfo, fYear);
  fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(fTitleExtraInfo);
  CheckTrue(fIsSTV, 'STV mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt3450958_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt3450958_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt3450958'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('War for the Planet of the Apes', fMovieTitle, 'Title mismatch');
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2017, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt3450958'), fVotes, fRating);

  CheckTrue(229000 < fVotes, 'Votes mismatch');
  CheckTrue(249000 > fVotes, 'Votes mismatch');
  CheckTrue(73 < fRating, 'Rating mismatch');
  CheckTrue(76 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('English,American Sign Language', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('USA,Canada,New Zealand', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt3450958'), fGenresList);

  CheckEqualsString('Action,Adventure,Drama,Sci-Fi,Thriller', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('Italy', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('7 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Cine&Comic Fest Genova)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[2];
    CheckEqualsString('UK', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('11 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[21];
    CheckEqualsString('Romania', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('14 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[24];
    CheckEqualsString('France', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('17 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Paris) (premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt3450958.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('War for the Planet of the Apes', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[1];
    CheckEqualsString('Austria', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Planet der Affen Survival', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[3];
    CheckEqualsString('Canada (French title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('La guerre de la planète des singes', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[4];
    CheckEqualsString('Canada (English title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('War for the Planet of the Apes', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[9];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('La Planète des singes  Suprématie', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0455275.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0455275_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0455275_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0455275'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Prison Break', fMovieTitle, 'Title mismatch');
  CheckEqualsString('TV Series', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2005, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0455275'), fVotes, fRating);

  CheckTrue(500000 < fVotes, 'Votes mismatch');
  CheckTrue(510000 > fVotes, 'Votes mismatch');
  CheckTrue(80 < fRating, 'Rating mismatch');
  CheckTrue(86 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('English,Arabic,Spanish', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('UK,USA', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0455275'), fGenresList);

  CheckEqualsString('Action,Crime,Drama,Mystery,Thriller', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[1];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('29 August 2005', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[4];
    CheckEqualsString('Norway', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('5 January 2006', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[8];
    CheckEqualsString('Japan', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('11 May 2006', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(DVD premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[17];
    CheckEqualsString('Switzerland', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('7 June 2007', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(German speaking region)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0455275.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Prison Break', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[1];
    CheckEqualsString('Brazil (DVD title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Prison Break Em Busca da Verdade', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[2];
    CheckEqualsString('Brazil', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Prison Break', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[9];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Prison Break', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7214470.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt7214470_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt7214470_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7214470'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Heilstätten', fMovieTitle, 'Title mismatch');
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2018, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7214470'), fVotes, fRating);

  CheckTrue(1500 < fVotes, 'Votes mismatch');
  CheckTrue(1800 > fVotes, 'Votes mismatch');
  CheckTrue(39 < fRating, 'Rating mismatch');
  CheckTrue(47 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('German', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('Germany', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7214470'), fGenresList);

  CheckEqualsString('Horror,Mystery,Thriller', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('Germany', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('22 February 2018', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[1];
    CheckEqualsString('Austria', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('23 February 2018', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[3];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('12 February 2019', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Blu-ray and DVD premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[5];
    CheckEqualsString('Netherlands', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('21 February 2019', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(DVD premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7214470.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Heilstätten', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[1];
    CheckEqualsString('Austria', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Heilstätten', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[3];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Fear Challenge', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[4];
    CheckEqualsString('Germany', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Heilstätten', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7728344.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt7728344_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt7728344_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7728344'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Marvel Rising: Secret Warriors', fMovieTitle, 'Title mismatch'); // TODO: strip comma, semicolon, colon?
  CheckEqualsString('TV Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2018, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7728344'), fVotes, fRating);

  CheckTrue(1000 < fVotes, 'Votes mismatch');
  CheckTrue(1800 > fVotes, 'Votes mismatch');
  CheckTrue(50 < fRating, 'Rating mismatch');
  CheckTrue(58 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('English', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('USA', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt7728344'), fGenresList);

  CheckEqualsString('Animation,Action,Comedy,Fantasy,Sci-Fi', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('30 September 2018', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[1];
    CheckEqualsString('Spain', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('1 June 2019', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt7728344.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Marvel Rising Secret Warriors', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[1];
    CheckEqualsString('Brazil', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Marvel Rising Guerreiros Secretos', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[2];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Marvel Rising Secret Warriors', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[7];
    CheckEqualsString('USA (alternative title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Marvel Rising Initiation', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt11095742.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt11095742_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt11095742_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt11095742'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Boys State', fMovieTitle, 'Title mismatch');
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2020, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt11095742'), fVotes, fRating);

  CheckTrue(2700 < fVotes, 'Votes mismatch');
  CheckTrue(5000 > fVotes, 'Votes mismatch');
  CheckTrue(74 < fRating, 'Rating mismatch');
  CheckTrue(81 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('English', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('USA', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt11095742'), fGenresList);

  CheckEqualsString('Documentary', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('24 January 2020', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Sundance Film Festival)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[1];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('6 March 2020', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(True/False Film Festival)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[9];
    CheckEqualsString('Spain', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('14 August 2020', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(internet)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt11095742.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Boys State', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[2];
    CheckEqualsString('Canada (English title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Boys State', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[4];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Boys State', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[5];
    CheckEqualsString('Germany', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Boys State', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0375568.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0375568_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FMainPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fStrList.Clear;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0375568_ReleaseDates', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseMetaTitleInformation;
var
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  THtmlIMDbParser.ParseMetaTitleInformation(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0375568'), fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Astro Boy', fMovieTitle, 'Title mismatch');
  CheckEqualsString('Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2009, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseVotesAndRating;
var
  fVotes, fRating: Integer;
begin
  THtmlIMDbParser.ParseVotesAndRating(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0375568'), fVotes, fRating);

  CheckTrue(31000 < fVotes, 'Votes mismatch');
  CheckTrue(39000 > fVotes, 'Votes mismatch');
  CheckTrue(59 < fRating, 'Rating mismatch');
  CheckTrue(68 > fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseMovieLanguage;
var
  fLanguageList: String;
begin
  THtmlIMDbParser.ParseMovieLanguage(FMainPage, fLanguageList);

  CheckEqualsString('English', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseMovieCountries;
var
  fCountriesList: String;
begin
  THtmlIMDbParser.ParseMovieCountries(FMainPage, fCountriesList);

  CheckEqualsString('Hong Kong,USA', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseMovieGenres;
var
  fGenresList: String;
begin
  THtmlIMDbParser.ParseMovieGenres(THtmlIMDbParser.GenerateJSONObject(FMainPage, 'tt0375568'), fGenresList);

  CheckEqualsString('Animation,Action,Adventure,Comedy,Drama,Family,Fantasy,Sci-Fi', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseReleaseDateInfo;
var
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(FReleasePage, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('Japan', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('5 October 2009', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Tokyo) (premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[2];
    CheckEqualsString('Belgium', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('17 October 2009', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Gent International Film Festival)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[3];
    CheckEqualsString('Italy', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('18 October 2009', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Rome Film Festival)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[5];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('19 October 2009', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Hollywood, California) (premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser_tt0375568.TestParseAlsoKnownAsInfo;
var
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(FReleasePage, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[1];
    CheckEqualsString('Brazil', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[2];
    CheckEqualsString('Canada (French title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[3];
    CheckEqualsString('Canada (English title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[5];
    CheckEqualsString('Germany', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy - Der Film', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[6];
    CheckEqualsString('Greece (DVD title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[13];
    CheckEqualsString('Serbia', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro dečak', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[14];
    CheckEqualsString('Spain', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetWidestScreensCountNoneAvailable;
var
  fPageSource: String;
  fScreens: Integer;
begin
  // tt0455275
  fPageSource := '';

  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);

  CheckEquals(0, fScreens, 'Screens count mismatch');
end;

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

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestListsOnlyReleaseGroups;
var
  fOnlyReleaseGroups: Boolean;
begin
  fOnlyReleaseGroups := THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(FOverviewPage);

  CheckFalse(fOnlyReleaseGroups, 'Should not lists release groups');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt5093026.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);

    CheckEquals(15, fBOMCountryLinks.Count, 'Count mismatch');
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
  // domestic on BOM
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

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.TestListsOnlyReleaseGroups;
var
  fOnlyReleaseGroups: Boolean;
begin
  fOnlyReleaseGroups := THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(FOverviewPage);

  CheckFalse(fOnlyReleaseGroups, 'Should not lists release groups');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0375568.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);

    CheckEquals(19, fBOMCountryLinks.Count, 'Count mismatch');
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

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestListsOnlyReleaseGroups;
var
  fOnlyReleaseGroups: Boolean;
begin
  fOnlyReleaseGroups := THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(FOverviewPage);

  CheckFalse(fOnlyReleaseGroups, 'Should not lists release groups');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt3450958.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOverviewPage, fBOMCountryLinks);

    CheckEquals(32, fBOMCountryLinks.Count, 'Count mismatch');
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
  // domestic on BOM
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

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0087332_BOMOrigRel', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOriginalReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt0087332_BOMREL', RT_RCDATA);
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

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestListsOnlyReleaseGroups;
var
  fOnlyReleaseGroups: Boolean;
begin
  fOnlyReleaseGroups := THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(FOverviewPage);

  CheckTrue(fOnlyReleaseGroups, 'Should lists release groups');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetGroupSpecificLinks;
var
  fBOMGroupReleaseLinks: TDictionary<String, String>;
begin
  fBOMGroupReleaseLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetGroupSpecificLinks(FOverviewPage, fBOMGroupReleaseLinks);

    CheckEquals(7, fBOMGroupReleaseLinks.Count, 'Count mismatch');
    CheckEqualsString('/releasegroup/gr2193641989', fBOMGroupReleaseLinks.Items['Original Release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr2210419205', fBOMGroupReleaseLinks.Items['1985 Re-release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr2160087557', fBOMGroupReleaseLinks.Items['2011 Re-release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr2176864773', fBOMGroupReleaseLinks.Items['30th Anniversary Release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr2260750853', fBOMGroupReleaseLinks.Items['2019 Re-release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr3449901573', fBOMGroupReleaseLinks.Items['2020 Re-release'], 'Link mismatch');
    CheckEqualsString('/releasegroup/gr2091930117', fBOMGroupReleaseLinks.Items['2021 Re-release'], 'Link mismatch');
  finally
    fBOMGroupReleaseLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin
  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOriginalReleasePage, fBOMCountryLinks);

    CheckEquals(1, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl3696592385', fBOMCountryLinks.Items['USA'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt0087332.TestGetWidestScreensCountUSA;
var
  fScreens: Integer;
begin
  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(FUSAReleasePage);

  CheckEquals(1506, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt7167658.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fResStream: TResourceStream;
  fStrList: TStringList;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt7167658_BOM', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOverviewPage := fStrList.Text;
    finally
      fResStream.Free;
    end;

    fResStream := TResourceStream.Create(HINSTANCE, 'tt7167658_BOMOrigRel', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      FOriginalReleasePage := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt7167658.TestListsOnlyReleaseGroups;
var
  fOnlyReleaseGroups: Boolean;
begin
  fOnlyReleaseGroups := THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(FOverviewPage);

  CheckTrue(fOnlyReleaseGroups, 'Should lists release groups');
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt7167658.TestGetGroupSpecificLinks;
var
  fBOMGroupReleaseLinks: TDictionary<String, String>;
begin
  fBOMGroupReleaseLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetGroupSpecificLinks(FOverviewPage, fBOMGroupReleaseLinks);

    // the commented ones are correct if all release groups with same name would be extracted be we only extract the first occuring one
    CheckEquals(1, fBOMGroupReleaseLinks.Count, 'Count mismatch');
    //CheckEquals(2, fBOMGroupReleaseLinks.Count, 'Count mismatch');
    CheckEqualsString('/releasegroup/gr1831424517', fBOMGroupReleaseLinks.Items['Original Release'], 'Link mismatch');
    //CheckEqualsString('/releasegroup/gr2792903173', fBOMGroupReleaseLinks.Items['Original Release'], 'Link mismatch');
  finally
    fBOMGroupReleaseLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser_tt7167658.TestGetCountrySpecificLinks;
var
  fBOMCountryLinks: TDictionary<String, String>;
begin

  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(FOriginalReleasePage, fBOMCountryLinks);

    CheckEquals(1, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl1760265217', fBOMCountryLinks.Items['New Zealand'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTIMDbInfoChecks.TestEstimateEnglishCountryOrder1;
var
  fStrList: TStringList;
  fResStream: TResourceStream;
  fPageSource: String;
  fImdbCountry: String;
  fFirstListedCountry: String;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt3450958_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      fPageSource := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fImdbCountry);
  fFirstListedCountry := TIMDbInfoChecks.EstimateEnglishCountryOrder(fImdbCountry);

  CheckEqualsString('USA', fFirstListedCountry, 'First occurring country mismatch');
end;

procedure TTestTIMDbInfoChecks.TestEstimateEnglishCountryOrder2;
var
  fStrList: TStringList;
  fResStream: TResourceStream;
  fPageSource: String;
  fImdbCountry: String;
  fFirstListedCountry: String;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0375568_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      fPageSource := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fImdbCountry);
  fFirstListedCountry := TIMDbInfoChecks.EstimateEnglishCountryOrder(fImdbCountry);

  CheckEqualsString('USA', fFirstListedCountry, 'First occurring country mismatch');
end;

procedure TTestTIMDbInfoChecks.TestEstimateEnglishCountryOrder3;
var
  fStrList: TStringList;
  fResStream: TResourceStream;
  fPageSource: String;
  fImdbCountry: String;
  fFirstListedCountry: String;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt0455275_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      fPageSource := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fImdbCountry);
  fFirstListedCountry := TIMDbInfoChecks.EstimateEnglishCountryOrder(fImdbCountry);

  CheckEqualsString('UK', fFirstListedCountry, 'First occurring country mismatch');
end;

procedure TTestTIMDbInfoChecks.TestEstimateEnglishCountryOrder4;
var
  fStrList: TStringList;
  fResStream: TResourceStream;
  fPageSource: String;
  fImdbCountry: String;
  fFirstListedCountry: String;
begin
  fStrList := TStringList.Create;
  try
    fResStream := TResourceStream.Create(HINSTANCE, 'tt7214470_Main', RT_RCDATA);
    try
      fStrList.LoadFromStream(fResStream);
      fPageSource := fStrList.Text;
    finally
      fResStream.Free;
    end;
  finally
    fStrList.Free;
  end;

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fImdbCountry);
  fFirstListedCountry := TIMDbInfoChecks.EstimateEnglishCountryOrder(fImdbCountry);

  CheckEqualsString('USA', fFirstListedCountry, 'First occurring country mismatch');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestTHtmlIMDbParser_tt3450958', TTestTHtmlIMDbParser_tt3450958.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt0455275', TTestTHtmlIMDbParser_tt0455275.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt7214470', TTestTHtmlIMDbParser_tt7214470.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt7728344', TTestTHtmlIMDbParser_tt7728344.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt11095742', TTestTHtmlIMDbParser_tt11095742.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt0375568', TTestTHtmlIMDbParser_tt0375568.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt3876702', TTestTHtmlIMDbParser_tt3876702.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt0382625', TTestTHtmlIMDbParser_tt0382625.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt4919664', TTestTHtmlIMDbParser_tt4919664.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt2487090', TTestTHtmlIMDbParser_tt2487090.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt0107144', TTestTHtmlIMDbParser_tt0107144.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt0816352', TTestTHtmlIMDbParser_tt0816352.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt5667286', TTestTHtmlIMDbParser_tt5667286.Suite);
    RegisterTest('TTestTHtmlIMDbParser_tt2372220', TTestTHtmlIMDbParser_tt2372220.Suite);

    RegisterTest('THtmlBoxOfficeMojoParser', TTestTHtmlBoxOfficeMojoParser.Suite);
    RegisterTest('TTestTHtmlBoxOfficeMojoParser_tt5093026', TTestTHtmlBoxOfficeMojoParser_tt5093026.Suite);
    RegisterTest('TTestTHtmlBoxOfficeMojoParser_tt0375568', TTestTHtmlBoxOfficeMojoParser_tt0375568.Suite);
    RegisterTest('TTestTHtmlBoxOfficeMojoParser_tt3450958', TTestTHtmlBoxOfficeMojoParser_tt3450958.Suite);
    RegisterTest('TTestTHtmlBoxOfficeMojoParser_tt0087332', TTestTHtmlBoxOfficeMojoParser_tt0087332.Suite);
    RegisterTest('TTestTHtmlBoxOfficeMojoParser_tt7167658', TTestTHtmlBoxOfficeMojoParser_tt7167658.Suite);

    RegisterTest('TTestTIMDbInfoChecks', TTestTIMDbInfoChecks.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt3450958);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt0455275);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt7214470);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt7728344);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt11095742);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt0375568);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt3876702);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt0382625);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt4919664);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt2487090);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt0107144);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt0816352);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt5667286);
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser_tt2372220);

    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt5093026);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt0375568);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt3450958);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt0087332);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser_tt7167658);

    TDUnitX.RegisterTestFixture(TTestTIMDbInfoChecks);
  {$ENDIF}
end.
