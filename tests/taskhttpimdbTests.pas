unit taskhttpimdbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTHtmlIMDbParser = class(TTestCase)
  published
    procedure TestParseMetaTitleInformation1;
    procedure TestParseMetaTitleInformation2;
    procedure TestParseMetaTitleInformation3;
    procedure TestParseMetaTitleInformation4;
    procedure TestParseMetaTitleInformation5;
    procedure TestParseMetaTitleInformation6;
    procedure TestParseMetaTitleInformation7;
    procedure TestParseVotesAndRating1;
    procedure TestParseVotesAndRating2;
    procedure TestParseVotesAndRating3;
    procedure TestParseMovieLanguage1;
    procedure TestParseMovieLanguage2;
    procedure TestParseMovieLanguage3;
    procedure TestParseMovieCountries1;
    procedure TestParseMovieCountries2;
    procedure TestParseMovieCountries3;
    procedure TestParseMovieGenres1;
    procedure TestParseMovieGenres2;
    procedure TestParseMovieGenres3;
    procedure TestParseReleaseDateInfo1;
    procedure TestParseReleaseDateInfo2;
    procedure TestParseReleaseDateInfo3;
    procedure TestParseAlsoKnownAsInfo1;
    procedure TestParseAlsoKnownAsInfo2;
    procedure TestParseAlsoKnownAsInfo3;
  end;

  TTestTHtmlBoxOfficeMojoParser = class(TTestCase)
  published
    procedure TestGetCountrySpecificLinks1;
    procedure TestGetCountrySpecificLinks2;
    procedure TestGetCountrySpecificLinks3;
    procedure TestGetWidestScreensCount1;
    procedure TestGetWidestScreensCount2;
    procedure TestGetWidestScreensCount3;
  end;

implementation

uses
  SysUtils, taskhttpimdb, Generics.Collections;

{ TTestTHtmlIMDbParser }

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation1;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt3450958
  fPageSource := '<meta property=''og:title'' content="War for the Planet of the Apes (2017) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('War for the Planet of the Apes', fMovieTitle, 'Title mismatch');
  CheckEqualsString('', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2017, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation2;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt0382625
  fPageSource := '<meta property=''og:title'' content="The Da Vinci Code (2006) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('The Da Vinci Code', fMovieTitle, 'Title mismatch');
  CheckEqualsString('', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2006, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation3;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt0455275
  fPageSource := '<meta property=''og:title'' content="Prison Break (TV Series 2005–2017) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Prison Break', fMovieTitle, 'Title mismatch');
  CheckEqualsString('TV Series', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2005, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation4;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt7728344
  fPageSource := '<meta property=''og:title'' content="Marvel Rising: Secret Warriors (TV Movie 2018) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Marvel Rising: Secret Warriors', fMovieTitle, 'Title mismatch'); // TODO: strip comma, semicolon, colon?
  CheckEqualsString('TV Movie', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2018, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation5;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt4919664
  fPageSource := '      <meta property=''og:title'' content="&quot;The Detour&quot; The Pilot (TV Episode 2016) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('&quot;The Detour&quot; The Pilot', fMovieTitle, 'Title mismatch'); // TODO: strip html chars?
  CheckEqualsString('TV Episode', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2016, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation6;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt2487090
  fPageSource := '<meta property=''og:title'' content="Sam & Cat (TV Series 2013–2014) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Sam & Cat', fMovieTitle, 'Title mismatch'); // TODO: replace & with and?
  CheckEqualsString('TV Series', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(2013, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMetaTitleInformation7;
var
  fPageSource: String;
  fMovieTitle, fTitleExtraInfo: String;
  fYear: Integer;
begin
  // tt0107144
  fPageSource := '      <meta property=''og:title'' content="Hot Shots! Part Deux (1993) - IMDb" />';

  THtmlIMDbParser.ParseMetaTitleInformation(fPageSource, fMovieTitle, fTitleExtraInfo, fYear);

  CheckEqualsString('Hot Shots! Part Deux', fMovieTitle, 'Title mismatch'); // TODO: strip ? and !?
  CheckEqualsString('', fTitleExtraInfo, 'Title extrainfo mismatch');
  CheckEquals(1993, fYear, 'Year mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseVotesAndRating1;
var
  fPageSource: String;
  fVotes, fRating: String;
begin
  // tt7214470
  fPageSource := '<div class="ratingValue"><strong title="4.1 based on 1,106 user ratings">' +
    '<span itemprop="ratingValue">4.1</span></strong><span class="grey">/</span><span class="grey" itemprop="bestRating">10</span>' +
    '                    </div><a href="/title/tt7214470/ratings?ref_=tt_ov_rt"><span class="small" itemprop="ratingCount">1,106</span></a>';

  THtmlIMDbParser.ParseVotesAndRating(fPageSource, fVotes, fRating);

  CheckEqualsString('1106', fVotes, 'Votes mismatch');
  CheckEqualsString('41', fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseVotesAndRating2;
var
  fPageSource: String;
  fVotes, fRating: String;
begin
  // tt0455275
  fPageSource := '<div class="ratingValue"><strong title="8.3 based on 458,169 user ratings"><span itemprop="ratingValue">8.3</span>' +
    '</strong><span class="grey">/</span><span class="grey" itemprop="bestRating">10</span>                    </div>' +
    '<a href="/title/tt0455275/ratings?ref_=tt_ov_rt"><span class="small" itemprop="ratingCount">458,169</span></a>';

  THtmlIMDbParser.ParseVotesAndRating(fPageSource, fVotes, fRating);

  CheckEqualsString('458169', fVotes, 'Votes mismatch');
  CheckEqualsString('83', fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseVotesAndRating3;
var
  fPageSource: String;
  fVotes, fRating: String;
begin
  // tt0816352
  fPageSource := '    <div class="star-rating-button"><button> <span class="star-rating-star no-rating"></span>' +
    '            <span class="star-rating-text">Rate This</span></button></div>';

  THtmlIMDbParser.ParseVotesAndRating(fPageSource, fVotes, fRating);

  CheckEqualsString('0', fVotes, 'Votes mismatch');
  CheckEqualsString('0', fRating, 'Rating mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieLanguage1;
var
  fPageSource: String;
  fLanguageList: String;
begin
  // tt7214470
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Language:</h4>' +
    '        <a href="/search/title?title_type=feature&primary_language=de&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>German</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieLanguage(fPageSource, fLanguageList);

  CheckEqualsString('German', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieLanguage2;
var
  fPageSource: String;
  fLanguageList: String;
begin
  // tt0455275
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Language:</h4>' +
    '        <a href="/search/title?title_type=feature&primary_language=en&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>English</a>' +
    '            <span class="ghost">|</span>' +
    '        <a href="/search/title?title_type=feature&primary_language=ar&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>Arabic</a>' +
    '            <span class="ghost">|</span>' +
    '        <a href="/search/title?title_type=feature&primary_language=es&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>Spanish</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieLanguage(fPageSource, fLanguageList);

  CheckEqualsString('English,Arabic,Spanish', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieLanguage3;
var
  fPageSource: String;
  fLanguageList: String;
begin
  // tt3450958
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Language:</h4>' +
    '        <a href="/search/title?title_type=feature&primary_language=en&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>English</a>' +
    '            <span class="ghost">|</span>' +
    '        <a href="/search/title?title_type=feature&primary_language=ase&sort=moviemeter,asc&ref_=tt_dt_dt"' +
    '>American Sign Language</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieLanguage(fPageSource, fLanguageList);

  CheckEqualsString('English,American Sign Language', fLanguageList, 'Language(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieCountries1;
var
  fPageSource: String;
  fCountriesList: String;
begin
  // tt7214470
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Country:</h4>' +
    '        <a href="/search/title?country_of_origin=de&ref_=tt_dt_dt"' +
    '>Germany</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fCountriesList);

  CheckEqualsString('Germany', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieCountries2;
var
  fPageSource: String;
  fCountriesList: String;
begin
  // tt0455275
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Country:</h4>' +
    '        <a href="/search/title?country_of_origin=gb&ref_=tt_dt_dt"' +
    '>UK</a>' +
    '              <span class="ghost">|</span>' +
    '        <a href="/search/title?country_of_origin=us&ref_=tt_dt_dt"' +
    '>USA</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fCountriesList);

  CheckEqualsString('UK,USA', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieCountries3;
var
  fPageSource: String;
  fCountriesList: String;
begin
  // tt3450958
  fPageSource := '    <div class="txt-block">' +
    '    <h4 class="inline">Country:</h4>' +
    '        <a href="/search/title?country_of_origin=us&ref_=tt_dt_dt"' +
    '>USA</a>' +
    '              <span class="ghost">|</span>' +
    '        <a href="/search/title?country_of_origin=ca&ref_=tt_dt_dt"' +
    '>Canada</a>' +
    '              <span class="ghost">|</span>' +
    '        <a href="/search/title?country_of_origin=nz&ref_=tt_dt_dt"' +
    '>New Zealand</a>' +
    '    </div>';

  THtmlIMDbParser.ParseMovieCountries(fPageSource, fCountriesList);

  CheckEqualsString('USA,Canada,New Zealand', fCountriesList, 'Countrie(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieGenres1;
var
  fPageSource: String;
  fGenresList: String;
begin
  // tt11095742
  fPageSource := '            <h4 class="inline">Genres:</h4>' +
    '<a href="/search/title?genres=documentary&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Documentary</a>' +
    '        </div>      ';

  THtmlIMDbParser.ParseMovieGenres(fPageSource, fGenresList);

  CheckEqualsString('Documentary', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieGenres2;
var
  fPageSource: String;
  fGenresList: String;
begin
  // tt7728344
  fPageSource := '            <h4 class="inline">Genres:</h4>' +
    '<a href="/search/title?genres=animation&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Animation</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=action&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Action</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=comedy&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Comedy</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=fantasy&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Fantasy</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=sci-fi&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Sci-Fi</a>' +
    '        </div>      ';

  THtmlIMDbParser.ParseMovieGenres(fPageSource, fGenresList);

  CheckEqualsString('Animation,Action,Comedy,Fantasy,Sci-Fi', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseMovieGenres3;
var
  fPageSource: String;
  fGenresList: String;
begin
  // tt3450958
  fPageSource := '<h4 class="inline">Genres:</h4>' +
    '<a href="/search/title?genres=action&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Action</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=adventure&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Adventure</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=drama&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Drama</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=sci-fi&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Sci-Fi</a>&nbsp;<span>|</span>' +
    '<a href="/search/title?genres=thriller&explore=title_type,genres&ref_=tt_ov_inf"' +
    '> Thriller</a>' +
    '        </div>';

  THtmlIMDbParser.ParseMovieGenres(fPageSource, fGenresList);

  CheckEqualsString('Action,Adventure,Drama,Sci-Fi,Thriller', fGenresList, 'Genre(s) mismatch');
end;

procedure TTestTHtmlIMDbParser.TestParseReleaseDateInfo1;
var
  fPageSource: String;
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  // tt3450958
  fPageSource := '                    <td class="release-date-item__country-name"><a href="/calendar/?region=it&r' +
    'ef_=ttrel_rel_1"' +
    '>Italy' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">7 July 2017' +
    '</td>                        <td class="release-date-item__attributes" align="left">           ' +
    '                 (Cine&Comic Fest Genova)' +
    '</td>' +
    '                </tr>' +
    '                <tr class' +
    '="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__co' +
    'untry-name"><a href="/calendar/?region=us&ref_=ttrel_rel_2"' +
    '>USA' +
    '</a></td>' +
    '<td class="release-d' +
    'ate-item__date" align="right">10 July 2017</td>                        <td class="release-date-' +
    'item__attributes" align="left">                            (New York City, New York)' +
    '</td>' +
    '    ' +
    '            </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '         ' +
    '           <td class="release-date-item__country-name"><a href="/calendar/?region=gb&ref_=ttrel' +
    '_rel_3"' +
    '>UK' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">11 July 2017</td>      ' +
    '                  <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td cla' +
    'ss="release-date-item__country-name"><a href="/calendar/?region=ie&ref_=ttrel_rel_4"' +
    '>Ireland' +
    '<' +
    '/a></td>' +
    '<td class="release-date-item__date" align="right">11 July 2017</td>                   ' +
    '     <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '             ' +
    '   <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-d' +
    'ate-item__country-name"><a href="/calendar/?region=be&ref_=ttrel_rel_5"' +
    '>Belgium' +
    '</a></td>' +
    '<td ' +
    'class="release-date-item__date" align="right">12 July 2017</td>                        <td clas' +
    's="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class=' +
    '"ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__cou' +
    'ntry-name"><a href="/calendar/?region=es&ref_=ttrel_rel_6"' +
    '>Spain' +
    '</a></td>' +
    '<td class="release-' +
    'date-item__date" align="right">12 July 2017</td>                        <td class="release-date' +
    '-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list' +
    '__item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a h' +
    'ref="/calendar/?region=fi&ref_=ttrel_rel_7"' +
    '>Finland' +
    '</a></td>' +
    '<td class="release-date-item__da' +
    'te" align="right">12 July 2017</td>                        <td class="release-date-item__attrib' +
    'utes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item releas' +
    'e-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calenda' +
    'r/?region=no&ref_=ttrel_rel_8"' +
    '>Norway' +
    '</a></td>' +
    '<td class="release-date-item__date" align="rig' +
    'ht">12 July 2017</td>                        <td class="release-date-item__attributes--empty"><' +
    '/td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?region=nz&r' +
    'ef_=ttrel_rel_9"' +
    '>New Zealand' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">12 Ju' +
    'ly 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '    ' +
    '            </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '         ' +
    '           <td class="release-date-item__country-name"><a href="/calendar/?region=ph&ref_=ttrel' +
    '_rel_10"' +
    '>Philippines' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">12 July 2017<' +
    '/td>                        <td class="release-date-item__attributes--empty"></td>' +
    '            ' +
    '    </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                 ' +
    '   <td class="release-date-item__country-name"><a href="/calendar/?region=se&ref_=ttrel_rel_11"' +
    '' +
    '>Sweden' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">12 July 2017</td>         ' +
    '               <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '   ' +
    '             <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class=' +
    '"release-date-item__country-name"><a href="/calendar/?region=cl&ref_=ttrel_rel_12"' +
    '>Chile' +
    '</a><' +
    '/td>' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>                       ' +
    ' <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <' +
    'tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-' +
    'item__country-name"><a href="/calendar/?region=cy&ref_=ttrel_rel_13"' +
    '>Cyprus' +
    '</a></td>' +
    '<td clas' +
    's="release-date-item__date" align="right">13 July 2017</td>                        <td class="r' +
    'elease-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl' +
    '-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__country' +
    '-name"><a href="/calendar/?region=cz&ref_=ttrel_rel_14"' +
    '>Czech Republic' +
    '</a></td>' +
    '<td class="re' +
    'lease-date-item__date" align="right">13 July 2017</td>                        <td class="releas' +
    'e-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebr' +
    'a-list__item release-date-item">' +
    '                    <td class="release-date-item__country-name' +
    '"><a href="/calendar/?region=dk&ref_=ttrel_rel_15"' +
    '>Denmark' +
    '</a></td>' +
    '<td class="release-date-i' +
    'tem__date" align="right">13 July 2017</td>                        <td class="release-date-item_' +
    '_attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item' +
    ' release-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/' +
    'calendar/?region=ge&ref_=ttrel_rel_16"' +
    '>Georgia' +
    '</a></td>' +
    '<td class="release-date-item__date" a' +
    'lign="right">13 July 2017</td>                        <td class="release-date-item__attributes-' +
    '-empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-dat' +
    'e-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?re' +
    'gion=gr&ref_=ttrel_rel_17"' +
    '>Greece' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">' +
    '13 July 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '    ' +
    '                <td class="release-date-item__country-name"><a href="/calendar/?region=hk&ref_=' +
    'ttrel_rel_18"' +
    '>Hong Kong' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 20' +
    '17</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '         ' +
    '       </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '              ' +
    '      <td class="release-date-item__country-name"><a href="/calendar/?region=hr&ref_=ttrel_rel_' +
    '19"' +
    '>Croatia' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>     ' +
    '                   <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td cl' +
    'ass="release-date-item__country-name"><a href="/calendar/?region=hu&ref_=ttrel_rel_20"' +
    '>Hungary' +
    '' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>                 ' +
    '       <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '           ' +
    '     <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release' +
    '-date-item__country-name"><a href="/calendar/?region=il&ref_=ttrel_rel_21"' +
    '>Israel' +
    '</a></td>' +
    '<t' +
    'd class="release-date-item__date" align="right">13 July 2017</td>                        <td cl' +
    'ass="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr clas' +
    's="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__c' +
    'ountry-name"><a href="/calendar/?region=it&ref_=ttrel_rel_22"' +
    '>Italy' +
    '</a></td>' +
    '<td class="relea' +
    'se-date-item__date" align="right">13 July 2017</td>                        <td class="release-d' +
    'ate-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-l' +
    'ist__item release-date-item">' +
    '                    <td class="release-date-item__country-name"><' +
    'a href="/calendar/?region=kh&ref_=ttrel_rel_23"' +
    '>Cambodia' +
    '</a></td>' +
    '<td class="release-date-ite' +
    'm__date" align="right">13 July 2017</td>                        <td class="release-date-item__a' +
    'ttributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item r' +
    'elease-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/ca' +
    'lendar/?region=kw&ref_=ttrel_rel_24"' +
    '>Kuwait' +
    '</a></td>' +
    '<td class="release-date-item__date" alig' +
    'n="right">13 July 2017</td>                        <td class="release-date-item__attributes--em' +
    'pty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-i' +
    'tem">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?regio' +
    'n=nl&ref_=ttrel_rel_25"' +
    '>Netherlands' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right' +
    '">13 July 2017</td>                        <td class="release-date-item__attributes--empty"></t' +
    'd>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '  ' +
    '                  <td class="release-date-item__country-name"><a href="/calendar/?region=pt&ref' +
    '_=ttrel_rel_26"' +
    '>Portugal' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 2' +
    '017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '        ' +
    '        </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '             ' +
    '       <td class="release-date-item__country-name"><a href="/calendar/?region=ru&ref_=ttrel_rel' +
    '_27"' +
    '>Russia' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>     ' +
    '                   <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td cl' +
    'ass="release-date-item__country-name"><a href="/calendar/?region=sg&ref_=ttrel_rel_28"' +
    '>Singapo' +
    're' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>               ' +
    '         <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '         ' +
    '       <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="relea' +
    'se-date-item__country-name"><a href="/calendar/?region=tw&ref_=ttrel_rel_29"' +
    '>Taiwan' +
    '</a></td>' +
    '' +
    '<td class="release-date-item__date" align="right">13 July 2017</td>                        <td ' +
    'class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr cl' +
    'ass="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item_' +
    '_country-name"><a href="/calendar/?region=ua&ref_=ttrel_rel_30"' +
    '>Ukraine' +
    '</a></td>' +
    '<td class="r' +
    'elease-date-item__date" align="right">13 July 2017</td>                        <td class="relea' +
    'se-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zeb' +
    'ra-list__item release-date-item">' +
    '                    <td class="release-date-item__country-nam' +
    'e"><a href="/calendar/?region=bd&ref_=ttrel_rel_31"' +
    '>Bangladesh' +
    '</a></td>' +
    '<td class="release-da' +
    'te-item__date" align="right">14 July 2017</td>                        <td class="release-date-i' +
    'tem__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__' +
    'item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a hre' +
    'f="/calendar/?region=bg&ref_=ttrel_rel_32"' +
    '>Bulgaria' +
    '</a></td>' +
    '<td class="release-date-item__da' +
    'te" align="right">14 July 2017</td>                        <td class="release-date-item__attrib' +
    'utes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item releas' +
    'e-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calenda' +
    'r/?region=ca&ref_=ttrel_rel_33"' +
    '>Canada' +
    '</a></td>' +
    '<td class="release-date-item__date" align="ri' +
    'ght">14 July 2017</td>                        <td class="release-date-item__attributes--empty">' +
    '</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?region=ee&' +
    'ref_=ttrel_rel_34"' +
    '>Estonia' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">14 July' +
    ' 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '      ' +
    '          </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '           ' +
    '         <td class="release-date-item__country-name"><a href="/calendar/?region=in&ref_=ttrel_r' +
    'el_35"' +
    '>India' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">14 July 2017</td>    ' +
    '                    <td class="release-date-item__attributes--empty"></td>' +
    '                </tr' +
    '>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td c' +
    'lass="release-date-item__country-name"><a href="/calendar/?region=lt&ref_=ttrel_rel_36"' +
    '>Lithua' +
    'nia' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">14 July 2017</td>              ' +
    '          <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '        ' +
    '        <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="rele' +
    'ase-date-item__country-name"><a href="/calendar/?region=lv&ref_=ttrel_rel_37"' +
    '>Latvia' +
    '</a></td>' +
    '' +
    '<td class="release-date-item__date" align="right">14 July 2017</td>                        <td' +
    ' class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr c' +
    'lass="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item' +
    '__country-name"><a href="/calendar/?region=ro&ref_=ttrel_rel_38"' +
    '>Romania' +
    '</a></td>' +
    '<td class="' +
    'release-date-item__date" align="right">14 July 2017</td>                        <td class="rele' +
    'ase-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-ze' +
    'bra-list__item release-date-item">' +
    '                    <td class="release-date-item__country-na' +
    'me"><a href="/calendar/?region=tr&ref_=ttrel_rel_39"' +
    '>Turkey' +
    '</a></td>' +
    '<td class="release-date-' +
    'item__date" align="right">14 July 2017</td>                        <td class="release-date-item' +
    '__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__ite' +
    'm release-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="' +
    '/calendar/?region=us&ref_=ttrel_rel_40"' +
    '>USA' +
    '</a></td>' +
    '<td class="release-date-item__date" alig' +
    'n="right">14 July 2017</td>                        <td class="release-date-item__attributes--em' +
    'pty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-i' +
    'tem">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?regio' +
    'n=vn&ref_=ttrel_rel_41"' +
    '>Vietnam' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">14' +
    ' July 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    ' ' +
    '               </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '      ' +
    '              <td class="release-date-item__country-name"><a href="/calendar/?region=fr&ref_=tt' +
    'rel_rel_42"' +
    '>France' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">17 July 2017</t' +
    'd>                        <td class="release-date-item__attributes" align="left">              ' +
    '              (Paris) (premiere)' +
    '</td>' +
    '                </tr>' +
    '                <tr class="ipl-zeb' +
    'ra-list__item release-date-item">' +
    '                    <td class="release-date-item__country-nam' +
    'e"><a href="/calendar/?region=id&ref_=ttrel_rel_43"' +
    '>Indonesia' +
    '</a></td>' +
    '<td class="release-dat' +
    'e-item__date" align="right">25 July 2017</td>                        <td class="release-date-it' +
    'em__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__i' +
    'tem release-date-item">' +
    '                    <td class="release-date-item__country-name"><a href' +
    '="/calendar/?region=au&ref_=ttrel_rel_44"' +
    '>Australia' +
    '</a></td>' +
    '<td class="release-date-item__da' +
    'te" align="right">27 July 2017</td>                        <td class="release-date-item__attrib' +
    'utes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item releas' +
    'e-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calenda' +
    'r/?region=co&ref_=ttrel_rel_45"' +
    '>Colombia' +
    '</a></td>' +
    '<td class="release-date-item__date" align="' +
    'right">27 July 2017</td>                        <td class="release-date-item__attributes--empty' +
    '"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item' +
    '">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?region=m' +
    'x&ref_=ttrel_rel_46"' +
    '>Mexico' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">27 Jul' +
    'y 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '     ' +
    '           </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '          ' +
    '          <td class="release-date-item__country-name"><a href="/calendar/?region=pl&ref_=ttrel_' +
    'rel_47"' +
    '>Poland' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">28 July 2017</td>  ' +
    '                      <td class="release-date-item__attributes--empty"></td>' +
    '                </' +
    'tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td' +
    ' class="release-date-item__country-name"><a href="/calendar/?region=ar&ref_=ttrel_rel_48"' +
    '>Arge' +
    'ntina' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">1 August 2017</td>           ' +
    '             <td class="release-date-item__attributes" align="left">                           ' +
    ' (Buenos Aires) (premiere)' +
    '</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-lis' +
    't__item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a ' +
    'href="/calendar/?region=fr&ref_=ttrel_rel_49"' +
    '>France' +
    '</a></td>' +
    '<td class="release-date-item__d' +
    'ate" align="right">2 August 2017</td>                        <td class="release-date-item__attr' +
    'ibutes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item rele' +
    'ase-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calen' +
    'dar/?region=ar&ref_=ttrel_rel_50"' +
    '>Argentina' +
    '</a></td>' +
    '<td class="release-date-item__date" alig' +
    'n="right">3 August 2017</td>                        <td class="release-date-item__attributes--e' +
    'mpty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-' +
    'item">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?regi' +
    'on=br&ref_=ttrel_rel_51"' +
    '>Brazil' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">3 ' +
    'August 2017</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '     ' +
    '               <td class="release-date-item__country-name"><a href="/calendar/?region=de&ref_=t' +
    'trel_rel_52"' +
    '>Germany' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">3 August 2017' +
    '</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '           ' +
    '     </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                ' +
    '    <td class="release-date-item__country-name"><a href="/calendar/?region=pe&ref_=ttrel_rel_53' +
    '"' +
    '>Peru' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">3 August 2017</td>         ' +
    '               <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '   ' +
    '             <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class=' +
    '"release-date-item__country-name"><a href="/calendar/?region=py&ref_=ttrel_rel_54"' +
    '>Paraguay' +
    '</' +
    'a></td>' +
    '<td class="release-date-item__date" align="right">3 August 2017</td>                   ' +
    '     <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '             ' +
    '   <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-d' +
    'ate-item__country-name"><a href="/calendar/?region=uy&ref_=ttrel_rel_55"' +
    '>Uruguay' +
    '</a></td>' +
    '<td' +
    ' class="release-date-item__date" align="right">3 August 2017</td>                        <td cl' +
    'ass="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr clas' +
    's="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__c' +
    'ountry-name"><a href="/calendar/?region=at&ref_=ttrel_rel_56"' +
    '>Austria' +
    '</a></td>' +
    '<td class="rel' +
    'ease-date-item__date" align="right">4 August 2017</td>                        <td class="releas' +
    'e-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebr' +
    'a-list__item release-date-item">' +
    '                    <td class="release-date-item__country-name' +
    '"><a href="/calendar/?region=kr&ref_=ttrel_rel_57"' +
    '>South Korea' +
    '</a></td>' +
    '<td class="release-da' +
    'te-item__date" align="right">15 August 2017</td>                        <td class="release-date' +
    '-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list' +
    '__item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a h' +
    'ref="/calendar/?region=cn&ref_=ttrel_rel_58"' +
    '>China' +
    '</a></td>' +
    '<td class="release-date-item__dat' +
    'e" align="right">15 September 2017</td>                        <td class="release-date-item__at' +
    'tributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item re' +
    'lease-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/cal' +
    'endar/?region=jp&ref_=ttrel_rel_59"' +
    '>Japan' +
    '</a></td>' +
    '<td class="release-date-item__date" align=' +
    '"right">13 October 2017</td>                        <td class="release-date-item__attributes--e' +
    'mpty"></td>' +
    '                </tr>';

  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(fPageSource, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('Italy', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('7 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Cine&Comic Fest Genova)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[2];
    CheckEqualsString('UK', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('11 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[21];
    CheckEqualsString('Italy', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('13 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[41];
    CheckEqualsString('France', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('17 July 2017', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(Paris) (premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser.TestParseReleaseDateInfo2;
var
  fPageSource: String;
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  // tt0455275
  fPageSource := '                    <td class="release-date-item__country-name"><a href="/calendar/?region=us&r' +
    'ef_=ttrel_rel_1"' +
    '>USA' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">29 August 200' +
    '5</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '          ' +
    '      </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '               ' +
    '     <td class="release-date-item__country-name"><a href="/calendar/?region=br&ref_=ttrel_rel_2' +
    '"' +
    '>Brazil' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">10 October 2005</td>     ' +
    '                   <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td cl' +
    'ass="release-date-item__country-name"><a href="/calendar/?region=se&ref_=ttrel_rel_3"' +
    '>Sweden' +
    '<' +
    '/a></td>' +
    '<td class="release-date-item__date" align="right">28 December 2005</td>               ' +
    '         <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '         ' +
    '       <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="relea' +
    'se-date-item__country-name"><a href="/calendar/?region=no&ref_=ttrel_rel_4"' +
    '>Norway' +
    '</a></td>' +
    '<' +
    'td class="release-date-item__date" align="right">5 January 2006</td>                        <td' +
    ' class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr c' +
    'lass="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item' +
    '__country-name"><a href="/calendar/?region=gb&ref_=ttrel_rel_5"' +
    '>UK' +
    '</a></td>' +
    '<td class="releas' +
    'e-date-item__date" align="right">23 January 2006</td>                        <td class="release' +
    '-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra' +
    '-list__item release-date-item">' +
    '                    <td class="release-date-item__country-name"' +
    '><a href="/calendar/?region=is&ref_=ttrel_rel_6"' +
    '>Iceland' +
    '</a></td>' +
    '<td class="release-date-ite' +
    'm__date" align="right">31 January 2006</td>                        <td class="release-date-item' +
    '__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__ite' +
    'm release-date-item">' +
    '                    <td class="release-date-item__country-name"><a href="' +
    '/calendar/?region=au&ref_=ttrel_rel_7"' +
    '>Australia' +
    '</a></td>' +
    '<td class="release-date-item__date"' +
    ' align="right">February 2006</td>                        <td class="release-date-item__attribut' +
    'es--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-' +
    'date-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/' +
    '?region=nl&ref_=ttrel_rel_8"' +
    '>Netherlands' +
    '</a></td>' +
    '<td class="release-date-item__date" align="' +
    'right">16 March 2006</td>                        <td class="release-date-item__attributes--empt' +
    'y"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-ite' +
    'm">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?region=' +
    'dk&ref_=ttrel_rel_9"' +
    '>Denmark' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">26 Ma' +
    'rch 2006</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '   ' +
    '             </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '        ' +
    '            <td class="release-date-item__country-name"><a href="/calendar/?region=jp&ref_=ttre' +
    'l_rel_10"' +
    '>Japan' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">11 May 2006</td>  ' +
    '                      <td class="release-date-item__attributes" align="left">                  ' +
    '          (DVD premiere)' +
    '</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list_' +
    '_item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a hr' +
    'ef="/calendar/?region=it&ref_=ttrel_rel_11"' +
    '>Italy' +
    '</a></td>' +
    '<td class="release-date-item__date' +
    '" align="right">18 May 2006</td>                        <td class="release-date-item__attribute' +
    's--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-d' +
    'ate-item">' +
    '                    <td class="release-date-item__country-name"><a href="/calendar/?' +
    'region=tr&ref_=ttrel_rel_12"' +
    '>Turkey' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right' +
    '">16 June 2006</td>                        <td class="release-date-item__attributes--empty"></t' +
    'd>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '  ' +
    '                  <td class="release-date-item__country-name"><a href="/calendar/?region=mx&ref' +
    '_=ttrel_rel_13"' +
    '>Mexico' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">10 July 200' +
    '6</td>                        <td class="release-date-item__attributes--empty"></td>' +
    '          ' +
    '      </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '               ' +
    '     <td class="release-date-item__country-name"><a href="/calendar/?region=hu&ref_=ttrel_rel_1' +
    '4"' +
    '>Hungary' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">23 August 2006</td>    ' +
    '                    <td class="release-date-item__attributes--empty"></td>' +
    '                </tr' +
    '>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td c' +
    'lass="release-date-item__country-name"><a href="/calendar/?region=fr&ref_=ttrel_rel_15"' +
    '>France' +
    '' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">31 August 2006</td>               ' +
    '         <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '         ' +
    '       <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="relea' +
    'se-date-item__country-name"><a href="/calendar/?region=es&ref_=ttrel_rel_16"' +
    '>Spain' +
    '</a></td>' +
    '<' +
    'td class="release-date-item__date" align="right">4 September 2006</td>                        <' +
    'td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr' +
    ' class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-it' +
    'em__country-name"><a href="/calendar/?region=ee&ref_=ttrel_rel_17"' +
    '>Estonia' +
    '</a></td>' +
    '<td class' +
    '="release-date-item__date" align="right">8 September 2006</td>                        <td class' +
    '="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="' +
    'ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__coun' +
    'try-name"><a href="/calendar/?region=fi&ref_=ttrel_rel_18"' +
    '>Finland' +
    '</a></td>' +
    '<td class="releas' +
    'e-date-item__date" align="right">12 September 2006</td>                        <td class="relea' +
    'se-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zeb' +
    'ra-list__item release-date-item">' +
    '                    <td class="release-date-item__country-nam' +
    'e"><a href="/calendar/?region=pl&ref_=ttrel_rel_19"' +
    '>Poland' +
    '</a></td>' +
    '<td class="release-date-i' +
    'tem__date" align="right">28 January 2007</td>                        <td class="release-date-it' +
    'em__attributes--empty"></td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__i' +
    'tem release-date-item">' +
    '                    <td class="release-date-item__country-name"><a href' +
    '="/calendar/?region=ch&ref_=ttrel_rel_20"' +
    '>Switzerland' +
    '</a></td>' +
    '<td class="release-date-item__' +
    'date" align="right">7 June 2007</td>                        <td class="release-date-item__attri' +
    'butes" align="left">                            (German speaking region)' +
    '</td>' +
    '                ' +
    '</tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <' +
    'td class="release-date-item__country-name"><a href="/calendar/?region=de&ref_=ttrel_rel_21"' +
    '>Ge' +
    'rmany' +
    '</a></td>' +
    '<td class="release-date-item__date" align="right">21 June 2007</td>            ' +
    '            <td class="release-date-item__attributes--empty"></td>' +
    '                </tr>' +
    '      ' +
    '          <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="re' +
    'lease-date-item__country-name"><a href="/calendar/?region=ca&ref_=ttrel_rel_22"' +
    '>Canada' +
    '</a></t' +
    'd>' +
    '<td class="release-date-item__date" align="right">12 January 2010</td>                      ' +
    '  <td class="release-date-item__attributes" align="left">                            (DVD premi' +
    'ere)' +
    '</td>' +
    '                </tr>';

  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(fPageSource, fReleaseDateInfoList);

    fReleaseDateInfo := fReleaseDateInfoList[0];
    CheckEqualsString('USA', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('29 August 2005', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[3];
    CheckEqualsString('Norway', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('5 January 2006', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[9];
    CheckEqualsString('Japan', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('11 May 2006', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(DVD premiere)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');

    fReleaseDateInfo := fReleaseDateInfoList[19];
    CheckEqualsString('Switzerland', fReleaseDateInfo.Country, 'Releasedate Country mismatch');
    CheckEqualsString('7 June 2007', fReleaseDateInfo.ReleaseDate, 'Releasedate Date mismatch');
    CheckEqualsString('(German speaking region)', fReleaseDateInfo.ExtraInfo, 'Releasedate Extra info mismatch');
  finally
    fReleaseDateInfoList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser.TestParseReleaseDateInfo3;
var
  fPageSource: String;
  fReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fReleaseDateInfo: TIMDbReleaseDateInfo;
begin
  // tt7728344
  fPageSource := '                    <td class="release-date-item__country-name"><a href="/calendar/?region=us&' +
    'ref_=ttrel_rel_1">USA</a></td><td class="release-date-item__date" ' +
    'align="right">30 September 2018</td>                        <td' +
    ' class="release-date-item__attributes--empty"></td>                </tr>' +
    '                <tr class="ipl-zebra-list__item release-date-item">' +
    '                    <td class="release-date-item__country-name"><a' +
    ' href="/calendar/?region=es&ref_=ttrel_rel_2">Spain</a></td>' +
    '<td class="release-date-item__date" align="right">1 June 2019</td>            ' +
    '            <td class="release-date-item__attributes--empty"></td>                </tr>';

  fReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
  try
    THtmlIMDbParser.ParseReleaseDateInfo(fPageSource, fReleaseDateInfoList);

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

procedure TTestTHtmlIMDbParser.TestParseAlsoKnownAsInfo1;
var
  fPageSource: String;
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  // tt3450958
  fPageSource := '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name"> (original title)</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Argentina</td>' +
    '                    <td class="aka-item__title">El planeta de los simios: La guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Australia</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Austria</td>' +
    '                    <td class="aka-item__title">Planet der Affen: Survival</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Brazil</td>' +
    '                    <td class="aka-item__title">Planeta dos Macacos: A Guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Bulgaria (Bulgarian title)</td>' +
    '                    <td class="aka-item__title">Войната за планетата на маймуните</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Canada (French title)</td>' +
    '                    <td class="aka-item__title">La guerre de la planète des singes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Canada (English title)</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Chile</td>' +
    '                    <td class="aka-item__title">El Planeta de los Simios: La Guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Colombia</td>' +
    '                    <td class="aka-item__title">La guerra del planeta de los simios</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Croatia</td>' +
    '                    <td class="aka-item__title">Planet majmuna: Rat</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Czech Republic</td>' +
    '                    <td class="aka-item__title">Válka o planetu opic</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Denmark</td>' +
    '                    <td class="aka-item__title">Abernes planet: Opgøret</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Estonia</td>' +
    '                    <td class="aka-item__title">Ahvide planeedi sõda</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Finland</td>' +
    '                    <td class="aka-item__title">Sota apinoiden planeetasta</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">France</td>' +
    '                    <td class="aka-item__title">La planète des singes: Suprématie</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Germany</td>' +
    '                    <td class="aka-item__title">Planet der Affen: Survival</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Greece</td>' +
    '                    <td class="aka-item__title">Ο πλανήτης των πιθήκων: Η σύγκρουση</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Hungary</td>' +
    '                    <td class="aka-item__title">A majmok bolygója: Háború</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">India</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Ireland</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Israel (Hebrew title)</td>' +
    '                    <td class="aka-item__title">Kokhav ha''kofim: Ha''milkhama</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Italy (pre-release title)</td>' +
    '                    <td class="aka-item__title">La guerra del pianeta delle scimmie</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Italy</td>' +
    '                    <td class="aka-item__title">The War - Il pianeta delle scimmie</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Japan</td>' +
    '                    <td class="aka-item__title">Saru no wakusei: Gurêto wô</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Japan (literal English title)</td>' +
    '                    <td class="aka-item__title">Planet of the Apes: Great War</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Japan (Japanese title)</td>' +
    '                    <td class="aka-item__title">猿の惑星：聖戦記（グレート・ウォー）</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Latvia</td>' +
    '                    <td class="aka-item__title">Karš par Pērtiķu planētu</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Lithuania</td>' +
    '                    <td class="aka-item__title">Karas uz bezdzioniu planeta</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Mexico</td>' +
    '                    <td class="aka-item__title">El planeta de los simios: la guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">New Zealand</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Peru</td>' +
    '                    <td class="aka-item__title">El planeta de los simios: La guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Poland</td>' +
    '                    <td class="aka-item__title">Wojna o planetę małp</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Portugal</td>' +
    '                    <td class="aka-item__title">Planeta dos Macacos: A Guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Romania</td>' +
    '                    <td class="aka-item__title">Planeta Maimutelor: Razboiul</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Russia</td>' +
    '                    <td class="aka-item__title">Планета обезьян: Война</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Serbia</td>' +
    '                    <td class="aka-item__title">Планета мајмуна: Рат</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Slovakia</td>' +
    '                    <td class="aka-item__title">Vojna o planétu opíc</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Slovenia</td>' +
    '                    <td class="aka-item__title">Vojna za Planet opic</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Spain</td>' +
    '                    <td class="aka-item__title">La guerra del planeta de los simios</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Sweden</td>' +
    '                    <td class="aka-item__title">Apornas planet: Striden</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Taiwan</td>' +
    '                    <td class="aka-item__title">猩球崛起：終極決戰</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Turkey (Turkish title)</td>' +
    '                    <td class="aka-item__title">Maymunlar Cehennemi: Savaş</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Ukraine</td>' +
    '                    <td class="aka-item__title">Війна за планету мавп</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">UK</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">USA (working title)</td>' +
    '                    <td class="aka-item__title">Planet of the Apes 3</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">USA</td>' +
    '                    <td class="aka-item__title">War for the Planet of the Apes</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Uruguay (3-D version)</td>' +
    '                    <td class="aka-item__title">El planeta de los simios: La guerra</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Vietnam</td>' +
    '                    <td class="aka-item__title">Đại Chiến Hành Tinh Khỉ</td>' +
    '                </tr>';

  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(fPageSource, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('War for the Planet of the Apes', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[6];
    CheckEqualsString('Canada (French title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('La guerre de la planète des singes', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser.TestParseAlsoKnownAsInfo2;
var
  fPageSource: String;
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  // tt7214470
  fPageSource := '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name"> (original title)</td>' +
    '                    <td class="aka-item__title">Heilstätten</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Argentina</td>' +
    '                    <td class="aka-item__title">El manicomio: La cuna del terror</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Austria</td>' +
    '                    <td class="aka-item__title">Heilstätten</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Brazil</td>' +
    '                    <td class="aka-item__title">O Manicômio</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Chile</td>' +
    '                    <td class="aka-item__title">El manicomio: La cuna del terror</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Colombia</td>' +
    '                    <td class="aka-item__title">El manicomio</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">France</td>' +
    '                    <td class="aka-item__title">Fear Challenge</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Germany</td>' +
    '                    <td class="aka-item__title">Heilstätten</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Mexico</td>' +
    '                    <td class="aka-item__title">El manicomio: la cuna del terror</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Norway</td>' +
    '                    <td class="aka-item__title">Heilstätten- The haunted Sanctuary</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Norway (transliterated title)</td>' +
    '                    <td class="aka-item__title">Senatoriet</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Peru</td>' +
    '                    <td class="aka-item__title">El manicomio: La cuna del terror</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Russia</td>' +
    '                    <td class="aka-item__title">Обитель тьмы</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Spain</td>' +
    '                    <td class="aka-item__title">El manicomio - La cuna del terror (Heilstatten)</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Ukraine</td>' +
    '                    <td class="aka-item__title">Клініка</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">UK (DVD title)</td>' +
    '                    <td class="aka-item__title">The Sanctuary</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">USA (DVD title)</td>' +
    '                    <td class="aka-item__title">Haunted Hospital</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Uruguay (original subtitled version)</td>' +
    '                    <td class="aka-item__title">El manicomio: La cuna del terror</td>' +
    '                </tr>';

  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(fPageSource, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Heilstätten', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[6];
    CheckEqualsString('France', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Fear Challenge', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[7];
    CheckEqualsString('Germany', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Heilstätten', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlIMDbParser.TestParseAlsoKnownAsInfo3;
var
  fPageSource: String;
  fAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
begin
  // tt0375568
  fPageSource := '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name"> (original title)</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Argentina</td>' +
    '                    <td class="aka-item__title">Astroboy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Australia</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Brazil</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Bulgaria (Bulgarian title)</td>' +
    '                    <td class="aka-item__title">Астробой</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Canada (French title)</td>' +
    '                    <td class="aka-item__title">Astro</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Chile</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">France</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Greece (DVD title)</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Japan</td>' +
    '                    <td class="aka-item__title">Atom</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Japan (Japanese title)</td>' +
    '                    <td class="aka-item__title">ATOM</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Lithuania</td>' +
    '                    <td class="aka-item__title">Astro vaikis</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Mexico</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Peru</td>' +
    '                    <td class="aka-item__title">Astroboy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Portugal</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Russia</td>' +
    '                    <td class="aka-item__title">Астробой</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Serbia</td>' +
    '                    <td class="aka-item__title">Astro dečak</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Spain</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Taiwan</td>' +
    '                    <td class="aka-item__title">原子小金剛</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Turkey (Turkish title)</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Ukraine</td>' +
    '                    <td class="aka-item__title">Астробой</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">UK</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">USA</td>' +
    '                    <td class="aka-item__title">Astro Boy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">Venezuela</td>' +
    '                    <td class="aka-item__title">Astroboy</td>' +
    '                </tr>' +
    '                <tr class="ipl-zebra-list__item aka-item">' +
    '                        <td class="aka-item__name">World-wide (English title) (alternative spelling)</td>' +
    '                    <td class="aka-item__title">Astroboy</td>' +
    '                </tr>';

  fAlsoKnownAsList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
  try
    THtmlIMDbParser.ParseAlsoKnownAsInfo(fPageSource, fAlsoKnownAsList);

    fAlsoKnownAsInfo := fAlsoKnownAsList[0];
    CheckEqualsString('(original title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[5];
    CheckEqualsString('Canada (French title)', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');

    fAlsoKnownAsInfo := fAlsoKnownAsList[17];
    CheckEqualsString('Spain', fAlsoKnownAsInfo.Country, 'AKA Country mismatch');
    CheckEqualsString('Astro Boy', fAlsoKnownAsInfo.Title, 'AKA Title mismatch');
  finally
    fAlsoKnownAsList.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetCountrySpecificLinks1;
var
  fPageSource: String;
  fBOMCountryLinks: TDictionary<String, String>;
begin
  // tt5093026
  fPageSource := '<div class="a-section a-spacing-none a-spacing-top-base"><div class="a-section mojo-gutter"><di' +
    'v class="a-section mojo-h-scroll"><h3>Domestic</h3>' +
    '                    <table class="a-bordere' +
    'd a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2">R' +
    'elease Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right">G' +
    'ross</th></tr><tr><td><a class="a-link-normal" href="/release/rl4094002689?ref_=bo_tt_gr_1">Dom' +
    'estic</a></td><td>Aug 24, 2018</td><td class="a-text-right"><span class="money">$1,090,073</spa' +
    'n></td><td class="a-text-right"><span class="money">$2,335,896</span></td></tr></table><h3>Euro' +
    'pe, Middle East, and Africa</h3>' +
    '                    <table class="a-bordered a-horizontal-stri' +
    'pes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2">Release Date</th><th' +
    ' class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right">Gross</th></tr><tr><' +
    'td><a class="a-link-normal" href="/release/rl4035348225?ref_=bo_tt_gr_1">Belgium</a></td><td>Se' +
    'p 19, 2018</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><spa' +
    'n class="money">$159,340</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl3934' +
    '684929?ref_=bo_tt_gr_2">Croatia</a></td><td>Jan 3, 2019</td><td class="a-text-right"><span clas' +
    's="money">$60,860</span></td><td class="a-text-right"><span class="money">$210,081</span></td><' +
    '/tr><tr><td><a class="a-link-normal" href="/release/rl4136011521?ref_=bo_tt_gr_3">Denmark</a></' +
    'td><td>Aug 23, 2018</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-ri' +
    'ght"><span class="money">$321,462</span></td></tr><tr><td><a class="a-link-normal" href="/relea' +
    'se/rl4152788737?ref_=bo_tt_gr_4">France</a></td><td>Aug 15, 2018</td><td class="a-text-right"><' +
    'span class="money">$266,683</span></td><td class="a-text-right"><span class="money">$485,806</s' +
    'pan></td></tr><tr><td><a class="a-link-normal" href="/release/rl4119234305?ref_=bo_tt_gr_5">Ger' +
    'many</a></td><td>Jul 26, 2018</td><td class="a-text-right"><span class="money">$211,029</span><' +
    '/td><td class="a-text-right"><span class="money">$483,618</span></td></tr><tr><td><a class="a-l' +
    'ink-normal" href="/release/rl3901130497?ref_=bo_tt_gr_6">Greece</a></td><td>Aug 23, 2018</td><t' +
    'd class="a-text-right"><span class="money">$61,120</span></td><td class="a-text-right"><span cl' +
    'ass="money">$130,361</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl39514621' +
    '45?ref_=bo_tt_gr_7">Israel</a></td><td>Aug 23, 2018</td><td class="a-text-right">' +
    '        &ndas' +
    'h;' +
    '    </td><td class="a-text-right"><span class="money">$445,260</span></td></tr><tr><td><a cl' +
    'ass="a-link-normal" href="/release/rl3985016577?ref_=bo_tt_gr_8">Italy</a></td><td>Jun 28, 2018' +
    '</td><td class="a-text-right"><span class="money">$299,292</span></td><td class="a-text-right">' +
    '<span class="money">$878,754</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl' +
    '4018571009?ref_=bo_tt_gr_9">Lithuania</a></td><td>Sep 28, 2018</td><td class="a-text-right"><sp' +
    'an class="money">$20,601</span></td><td class="a-text-right"><span class="money">$54,138</span>' +
    '</td></tr><tr><td><a class="a-link-normal" href="/release/rl3917907713?ref_=bo_tt_gr_10">Nether' +
    'lands</a></td><td>Sep 13, 2018</td><td class="a-text-right"><span class="money">$201,811</span>' +
    '</td><td class="a-text-right"><span class="money">$834,930</span></td></tr><tr><td><a class="a-' +
    'link-normal" href="/release/rl3783689985?ref_=bo_tt_gr_11">Portugal</a></td><td>Aug 30, 2018</t' +
    'd><td class="a-text-right"><span class="money">$99,702</span></td><td class="a-text-right"><spa' +
    'n class="money">$336,904</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl3817' +
    '244417?ref_=bo_tt_gr_12">Romania</a></td><td>Jan 18, 2019</td><td class="a-text-right"><span cl' +
    'ass="money">$83,122</span></td><td class="a-text-right"><span class="money">$293,189</span></td' +
    '></tr><tr><td><a class="a-link-normal" href="/release/rl3834021633?ref_=bo_tt_gr_13">Serbia and' +
    ' Montenegro</a></td><td>Jan 24, 2019</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td' +
    ' class="a-text-right"><span class="money">$44,193</span></td></tr><tr><td><a class="a-link-norm' +
    'al" href="/release/rl3850798849?ref_=bo_tt_gr_14">Slovenia</a></td><td>Jan 24, 2019</td><td cla' +
    'ss="a-text-right"><span class="money">$12,879</span></td><td class="a-text-right"><span class="' +
    'money">$48,280</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl3666249473?ref' +
    '_=bo_tt_gr_15">South Africa</a></td><td>Oct 19, 2018</td><td class="a-text-right"><span class="' +
    'money">$15,159</span></td><td class="a-text-right"><span class="money">$39,559</span></td></tr>' +
    '<tr><td><a class="a-link-normal" href="/release/rl3867576065?ref_=bo_tt_gr_16">Turkey</a></td><' +
    'td>Mar 8, 2019</td><td class="a-text-right"><span class="money">$25,983</span></td><td class="a' +
    '-text-right"><span class="money">$56,726</span></td></tr><tr><td><a class="a-link-normal" href=' +
    '"/release/rl3632695041?ref_=bo_tt_gr_17">Ukraine</a></td><td>Sep 27, 2018</td><td class="a-text' +
    '-right"><span class="money">$61,608</span></td><td class="a-text-right"><span class="money">$14' +
    '7,810</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl3649472257?ref_=bo_tt_g' +
    'r_18">United Arab Emirates</a></td><td>Aug 23, 2018</td><td class="a-text-right"><span class="m' +
    'oney">$70,230</span></td><td class="a-text-right"><span class="money">$101,442</span></td></tr>' +
    '</table><h3>Latin America</h3>' +
    '                    <table class="a-bordered a-horizontal-stripe' +
    's a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2">Release Date</th><th c' +
    'lass="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right">Gross</th></tr><tr><td' +
    '><a class="a-link-normal" href="/release/rl4052125441?ref_=bo_tt_gr_1">Bolivia</a></td><td>Nov ' +
    '22, 2018</td><td class="a-text-right"><span class="money">$4,375</span></td><td class="a-text-r' +
    'ight"><span class="money">$11,043</span></td></tr><tr><td><a class="a-link-normal" href="/relea' +
    'se/rl4068902657?ref_=bo_tt_gr_2">Brazil</a></td><td>Oct 4, 2018</td><td class="a-text-right"><s' +
    'pan class="money">$109,301</span></td><td class="a-text-right"><span class="money">$220,417</sp' +
    'an></td></tr><tr><td><a class="a-link-normal" href="/release/rl4085679873?ref_=bo_tt_gr_3">Colo' +
    'mbia</a></td><td>Jan 17, 2019</td><td class="a-text-right"><span class="money">$91,058</span></' +
    'td><td class="a-text-right"><span class="money">$151,543</span></td></tr><tr><td><a class="a-li' +
    'nk-normal" href="/release/rl3766912769?ref_=bo_tt_gr_4">Mexico</a></td><td>Sep 28, 2018</td><td' +
    ' class="a-text-right"><span class="money">$662,453</span></td><td class="a-text-right"><span cl' +
    'ass="money">$1,511,143</span></td></tr></table><h3>Asia Pacific</h3>' +
    '                    <table' +
    ' class="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th ' +
    'class="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span' +
    '3 a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl3968239361?ref' +
    '_=bo_tt_gr_1">India</a></td><td>Aug 31, 2018</td><td class="a-text-right">' +
    '        &ndash;' +
    '    ' +
    '</td><td class="a-text-right"><span class="money">$25,130</span></td></tr><tr><td><a class="a-l' +
    'ink-normal" href="/release/rl3800467201?ref_=bo_tt_gr_2">Russia/CIS</a></td><td>Sep 28, 2018</t' +
    'd><td class="a-text-right"><span class="money">$247,309</span></td><td class="a-text-right"><sp' +
    'an class="money">$3,820</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl40017' +
    '93793?ref_=bo_tt_gr_3">South Korea</a></td><td>Feb 27, 2019</td><td class="a-text-right"><span ' +
    'class="money">$120,895</span></td><td class="a-text-right"><span class="money">$237,681</span><' +
    '/td></tr><tr><td><a class="a-link-normal" href="/release/rl3884353281?ref_=bo_tt_gr_4">Taiwan</' +
    'a></td><td>Sep 21, 2018</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-tex' +
    't-right"><span class="money">$334,278</span></td></tr></table><h3>China</h3>' +
    '                  ' +
    '  <table class="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area<' +
    '/th><th class="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class' +
    '="a-span3 a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl410245' +
    '7089?ref_=bo_tt_gr_1">China</a></td><td>Jul 26, 2019</td><td class="a-text-right"><span class="' +
    'money">$158,099</span></td><td class="a-text-right"><span class="money">$158,099</span></td></t' +
    'r></table></div></div></div></div></main>' +
    '            <div class="a-section a-spacing-none mojo' +
    '-navigation-frame"><div class="a-section a-spacing-none mojo-navigation mojo-footer"><a class="' +
    'a-link-normal imdb-pro-logo" href="https://pro.imdb.com/?ref_=mojo_ft_tt_prologo&amp;rf=mojo_ft' +
    '_tt_prologo"></a><p>' +
    '            Latest Updates:' +
    '            <a class="a-link-normal" href="/ne' +
    'ws/?ref_=bo_ft_tt_news">News</a> |' +
    '            <a class="a-link-normal" href="/daily/?ref_=bo_f' +
    't_tt_daily">Daily</a> |' +
    '            <a class="a-link-normal" href="/weekend/?ref_=bo_ft_tt_week' +
    'end">Weekend</a> |';

  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fPageSource, fBOMCountryLinks);

    CheckEquals(28, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl4094002689', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl3985016577', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl3783689985', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl4119234305', fBOMCountryLinks.Items['Germany'], 'Link mismatch');
    CheckEqualsString('/release/rl4152788737', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetCountrySpecificLinks2;
var
  fPageSource: String;
  fBOMCountryLinks: TDictionary<String, String>;
begin
  // tt0375568
  fPageSource := '<div class="a-section a-spacing-none a-spacing-top-base"><div class="a-section mojo-gutter"><di' +
    'v class="a-section mojo-h-scroll"><h3>Domestic</h3>' +
    '                    <table class="a-bordere' +
    'd a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2">R' +
    'elease Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right">G' +
    'ross</th></tr><tr><td><a class="a-link-normal" href="/release/rl3947005441?ref_=bo_tt_gr_1">Dom' +
    'estic</a></td><td>Oct 23, 2009</td><td class="a-text-right"><span class="money">$6,702,923</spa' +
    'n></td><td class="a-text-right"><span class="money">$19,551,067</span></td></tr></table><h3>Eur' +
    'ope, Middle East, and Africa</h3>' +
    '                    <table class="a-bordered a-horizontal-str' +
    'ipes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2">Release Date</th><t' +
    'h class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right">Gross</th></tr><tr>' +
    '<td><a class="a-link-normal" href="/release/rl2754512385?ref_=bo_tt_gr_1">Belgium</a></td><td>O' +
    'ct 28, 2009</td><td class="a-text-right"><span class="money">$66,979</span></td><td class="a-te' +
    'xt-right"><span class="money">$507,079</span></td></tr><tr><td><a class="a-link-normal" href="/' +
    'release/rl2670626305?ref_=bo_tt_gr_2">Croatia</a></td><td>Dec 3, 2009</td><td class="a-text-rig' +
    'ht"><span class="money">$12,069</span></td><td class="a-text-right"><span class="money">$54,139' +
    '</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2569963009?ref_=bo_tt_gr_3">' +
    'Czech Republic</a></td><td>Oct 29, 2009</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td>' +
    '<td class="a-text-right"><span class="money">$62,669</span></td></tr><tr><td><a class="a-link-n' +
    'ormal" href="/release/rl2603517441?ref_=bo_tt_gr_4">Egypt</a></td><td>Apr 14, 2010</td><td clas' +
    's="a-text-right"><span class="money">$2,073</span></td><td class="a-text-right"><span class="mo' +
    'ney">$5,626</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2637071873?ref_=b' +
    'o_tt_gr_5">France</a></td><td>Dec 9, 2009</td><td class="a-text-right"><span class="money">$714' +
    ',588</span></td><td class="a-text-right"><span class="money">$1,189,211</span></td></tr><tr><td' +
    '><a class="a-link-normal" href="/release/rl2418968065?ref_=bo_tt_gr_6">Hungary</a></td><td>Nov ' +
    '25, 2009</td><td class="a-text-right"><span class="money">$25,716</span></td><td class="a-text-' +
    'right"><span class="money">$33,453</span></td></tr><tr><td><a class="a-link-normal" href="/rele' +
    'ase/rl2435745281?ref_=bo_tt_gr_7">Iceland</a></td><td>Apr 23, 2010</td><td class="a-text-right"' +
    '><span class="money">$5,670</span></td><td class="a-text-right"><span class="money">$21,892</sp' +
    'an></td></tr><tr><td><a class="a-link-normal" href="/release/rl2452522497?ref_=bo_tt_gr_8">Ital' +
    'y</a></td><td>Dec 18, 2009</td><td class="a-text-right"><span class="money">$246,378</span></td' +
    '><td class="a-text-right"><span class="money">$696,229</span></td></tr><tr><td><a class="a-link' +
    '-normal" href="/release/rl2502854145?ref_=bo_tt_gr_9">Lebanon</a></td><td>Dec 10, 2009</td><td ' +
    'class="a-text-right"><span class="money">$4,599</span></td><td class="a-text-right"><span class' +
    '="money">$6,535</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2335081985?re' +
    'f_=bo_tt_gr_10">Portugal</a></td><td>Jun 3, 2010</td><td class="a-text-right"><span class="mone' +
    'y">$38,393</span></td><td class="a-text-right"><span class="money">$86,797</span></td></tr><tr>' +
    '<td><a class="a-link-normal" href="/release/rl2385413633?ref_=bo_tt_gr_11">Slovakia</a></td><td' +
    '>Dec 10, 2009</td><td class="a-text-right"><span class="money">$24,354</span></td><td class="a-' +
    'text-right"><span class="money">$24,354</span></td></tr><tr><td><a class="a-link-normal" href="' +
    '/release/rl2267973121?ref_=bo_tt_gr_12">South Africa</a></td><td></td><td class="a-text-right">' +
    '<span class="money">$36,495</span></td><td class="a-text-right"><span class="money">$221,264</s' +
    'pan></td></tr><tr><td><a class="a-link-normal" href="/release/rl2620294657?ref_=bo_tt_gr_13">Sp' +
    'ain</a></td><td>Sep 24, 2010</td><td class="a-text-right"><span class="money">$241,785</span></' +
    'td><td class="a-text-right"><span class="money">$679,352</span></td></tr><tr><td><a class="a-li' +
    'nk-normal" href="/release/rl2150532609?ref_=bo_tt_gr_14">Turkey</a></td><td>Apr 16, 2010</td><t' +
    'd class="a-text-right"><span class="money">$539,389</span></td><td class="a-text-right"><span c' +
    'lass="money">$668,110</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2200864' +
    '257?ref_=bo_tt_gr_15">Ukraine</a></td><td>Nov 12, 2009</td><td class="a-text-right"><span class' +
    '="money">$5,300</span></td><td class="a-text-right"><span class="money">$20,111</span></td></tr' +
    '><tr><td><a class="a-link-normal" href="/release/rl2217641473?ref_=bo_tt_gr_16">United Arab Emi' +
    'rates</a></td><td>Dec 10, 2009</td><td class="a-text-right"><span class="money">$36,274</span><' +
    '/td><td class="a-text-right"><span class="money">$87,245</span></td></tr><tr><td><a class="a-li' +
    'nk-normal" href="/release/rl2234418689?ref_=bo_tt_gr_17">United Kingdom</a></td><td>Feb 5, 2010' +
    '</td><td class="a-text-right"><span class="money">$1,710,567</span></td><td class="a-text-right' +
    '"><span class="money">$5,148,738</span></td></tr></table><h3>Latin America</h3>' +
    '               ' +
    '     <table class="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Ar' +
    'ea</th><th class="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th cl' +
    'ass="a-span3 a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl272' +
    '0957953?ref_=bo_tt_gr_1">Argentina</a></td><td>Jan 28, 2010</td><td class="a-text-right"><span ' +
    'class="money">$49,620</span></td><td class="a-text-right"><span class="money">$174,796</span></' +
    'td></tr><tr><td><a class="a-link-normal" href="/release/rl2771289601?ref_=bo_tt_gr_2">Bolivia</' +
    'a></td><td>Apr 8, 2010</td><td class="a-text-right"><span class="money">$5,182</span></td><td c' +
    'lass="a-text-right"><span class="money">$9,353</span></td></tr><tr><td><a class="a-link-normal"' +
    ' href="/release/rl2788066817?ref_=bo_tt_gr_3">Brazil</a></td><td>Jan 22, 2010</td><td class="a-' +
    'text-right"><span class="money">$151,743</span></td><td class="a-text-right"><span class="money' +
    '">$490,745</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2553185793?ref_=bo' +
    '_tt_gr_4">Chile</a></td><td>Jan 21, 2010</td><td class="a-text-right"><span class="money">$57,8' +
    '06</span></td><td class="a-text-right"><span class="money">$188,580</span></td></tr><tr><td><a ' +
    'class="a-link-normal" href="/release/rl2804844033?ref_=bo_tt_gr_5">Colombia</a></td><td>Jan 15,' +
    ' 2010</td><td class="a-text-right"><span class="money">$26,965</span></td><td class="a-text-rig' +
    'ht"><span class="money">$77,549</span></td></tr><tr><td><a class="a-link-normal" href="/release' +
    '/rl2586740225?ref_=bo_tt_gr_6">Ecuador</a></td><td>Jan 22, 2010</td><td class="a-text-right"><s' +
    'pan class="money">$27,436</span></td><td class="a-text-right"><span class="money">$60,064</span' +
    '></td></tr><tr><td><a class="a-link-normal" href="/release/rl2519631361?ref_=bo_tt_gr_7">Mexico' +
    '</a></td><td>Jan 29, 2010</td><td class="a-text-right"><span class="money">$245,340</span></td>' +
    '<td class="a-text-right"><span class="money">$710,349</span></td></tr><tr><td><a class="a-link-' +
    'normal" href="/release/rl2301527553?ref_=bo_tt_gr_8">Peru</a></td><td>Jan 28, 2010</td><td clas' +
    's="a-text-right"><span class="money">$73,645</span></td><td class="a-text-right"><span class="m' +
    'oney">$245,227</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2184087041?ref' +
    '_=bo_tt_gr_9">Uruguay</a></td><td>Jan 29, 2010</td><td class="a-text-right"><span class="money"' +
    '>$6,153</span></td><td class="a-text-right"><span class="money">$30,546</span></td></tr><tr><td' +
    '><a class="a-link-normal" href="/release/rl2251195905?ref_=bo_tt_gr_10">Venezuela</a></td><td>J' +
    'an 29, 2010</td><td class="a-text-right"><span class="money">$37,673</span></td><td class="a-te' +
    'xt-right"><span class="money">$96,172</span></td></tr></table><h3>Asia Pacific</h3>' +
    '           ' +
    '         <table class="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4' +
    '">Area</th><th class="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><t' +
    'h class="a-span3 a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/r' +
    'l2737735169?ref_=bo_tt_gr_1">Australia</a></td><td>Oct 15, 2009</td><td class="a-text-right"><s' +
    'pan class="money">$825,805</span></td><td class="a-text-right"><span class="money">$2,683,576</' +
    'span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2653849089?ref_=bo_tt_gr_2">Ho' +
    'ng Kong</a></td><td>Oct 22, 2009</td><td class="a-text-right"><span class="money">$169,667</spa' +
    'n></td><td class="a-text-right"><span class="money">$390,901</span></td></tr><tr><td><a class="' +
    'a-link-normal" href="/release/rl2469299713?ref_=bo_tt_gr_3">Japan</a></td><td>Oct 10, 2009</td>' +
    '<td class="a-text-right"><span class="money">$329,641</span></td><td class="a-text-right"><span' +
    ' class="money">$864,683</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl25364' +
    '08577?ref_=bo_tt_gr_4">Malaysia</a></td><td>Nov 19, 2009</td><td class="a-text-right"><span cla' +
    'ss="money">$64,945</span></td><td class="a-text-right"><span class="money">$167,521</span></td>' +
    '</tr><tr><td><a class="a-link-normal" href="/release/rl2284750337?ref_=bo_tt_gr_5">New Zealand<' +
    '/a></td><td>Jan 21, 2010</td><td class="a-text-right"><span class="money">$66,252</span></td><t' +
    'd class="a-text-right"><span class="money">$180,477</span></td></tr><tr><td><a class="a-link-no' +
    'rmal" href="/release/rl2318304769?ref_=bo_tt_gr_6">Philippines</a></td><td>Oct 28, 2009</td><td' +
    ' class="a-text-right"><span class="money">$77,757</span></td><td class="a-text-right"><span cla' +
    'ss="money">$151,113</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl235185920' +
    '1?ref_=bo_tt_gr_7">Russia/CIS</a></td><td>Oct 22, 2009</td><td class="a-text-right"><span class' +
    '="money">$636,850</span></td><td class="a-text-right"><span class="money">$1,438,385</span></td' +
    '></tr><tr><td><a class="a-link-normal" href="/release/rl2368636417?ref_=bo_tt_gr_8">Singapore</' +
    'a></td><td>Nov 5, 2009</td><td class="a-text-right"><span class="money">$215,653</span></td><td' +
    ' class="a-text-right"><span class="money">$513,876</span></td></tr><tr><td><a class="a-link-nor' +
    'mal" href="/release/rl2486076929?ref_=bo_tt_gr_9">South Korea</a></td><td>Jan 13, 2010</td><td ' +
    'class="a-text-right"><span class="money">$906,361</span></td><td class="a-text-right"><span cla' +
    'ss="money">$2,201,763</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2167309' +
    '825?ref_=bo_tt_gr_10">Taiwan</a></td><td>Oct 24, 2009</td><td class="a-text-right"><span class=' +
    '"money">$30,109</span></td><td class="a-text-right"><span class="money">$71,942</span></td></tr' +
    '><tr><td><a class="a-link-normal" href="/release/rl2402190849?ref_=bo_tt_gr_11">Thailand</a></t' +
    'd><td>Dec 3, 2009</td><td class="a-text-right"><span class="money">$11,216</span></td><td class' +
    '="a-text-right"><span class="money">$20,348</span></td></tr></table></div></div></div></div></m' +
    'ain>' +
    '            <div class="a-section a-spacing-none mojo-navigation-frame"><div class="a-sect' +
    'ion a-spacing-none mojo-navigation mojo-footer"><a class="a-link-normal imdb-pro-logo" href="ht' +
    'tps://pro.imdb.com/?ref_=mojo_ft_tt_prologo&amp;rf=mojo_ft_tt_prologo"></a><p>' +
    '            Late' +
    'st Updates:' +
    '            <a class="a-link-normal" href="/news/?ref_=bo_ft_tt_news">News</a> |' +
    '  ' +
    '          <a class="a-link-normal" href="/daily/?ref_=bo_ft_tt_daily">Daily</a> |' +
    '            <' +
    'a class="a-link-normal" href="/weekend/?ref_=bo_ft_tt_weekend">Weekend</a> |';

  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fPageSource, fBOMCountryLinks);

    CheckEquals(39, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl3947005441', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl2452522497', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl2335081985', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl2620294657', fBOMCountryLinks.Items['Spain'], 'Link mismatch');
    CheckEqualsString('/release/rl2637071873', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetCountrySpecificLinks3;
var
  fPageSource: String;
  fBOMCountryLinks: TDictionary<String, String>;
begin
  // tt3450958
  fPageSource := '            <a class="a-size-base a-link-normal mojo-navigation-tab mojo-navigation-tab-active"' +
    ' href="/title/tt3450958/?ref_=bo_tt_tab#tabs">Releases</a><a class="a-size-base a-link-normal m' +
    'ojo-navigation-tab" href="/title/tt3450958/credits/?ref_=bo_tt_tab#tabs">Cast and Crew</a><a cl' +
    'ass="a-size-base a-link-normal mojo-navigation-tab" href="/title/tt3450958/rankings/?ref_=bo_tt' +
    '_tab#tabs">All-Time Rankings</a><a class="a-size-base a-link-normal mojo-navigation-tab" href="' +
    '/title/tt3450958/news/?ref_=bo_tt_tab#tabs">Related Stories</a><a class="a-size-base a-link-nor' +
    'mal mojo-navigation-tab" href="/title/tt3450958/similar/?ref_=bo_tt_tab#tabs">Similar Movies</a' +
    '></div>' +
    '    </div>' +
    '<div class="a-section a-spacing-none a-spacing-top-base"><div class="a-secti' +
    'on mojo-gutter"><div class="a-section mojo-h-scroll"><h3>Domestic</h3>' +
    '                    <tab' +
    'le class="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><t' +
    'h class="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-sp' +
    'an3 a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl1782744577?r' +
    'ef_=bo_tt_gr_1">Domestic</a></td><td>Jul 14, 2017</td><td class="a-text-right"><span class="mon' +
    'ey">$56,262,929</span></td><td class="a-text-right"><span class="money">$146,880,162</span></td' +
    '></tr></table><h3>Europe, Middle East, and Africa</h3>' +
    '                    <table class="a-bord' +
    'ered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-span2' +
    '">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-right' +
    '">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl2049672705?ref_=bo_tt_gr_1">' +
    'Austria</a></td><td>Aug 3, 2017</td><td class="a-text-right"><span class="money">$406,635</span' +
    '></td><td class="a-text-right"><span class="money">$1,309,851</span></td></tr><tr><td><a class=' +
    '"a-link-normal" href="/release/rl2116781569?ref_=bo_tt_gr_2">Bahrain</a></td><td>Jul 13, 2017</' +
    'td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="mon' +
    'ey">$988,989</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl2083227137?ref_=' +
    'bo_tt_gr_3">Belgium</a></td><td>Jul 12, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    ' +
    '</td><td class="a-text-right"><span class="money">$2,495,043</span></td></tr><tr><td><a class="' +
    'a-link-normal" href="/release/rl2100004353?ref_=bo_tt_gr_4">Bulgaria</a></td><td>Jul 14, 2017</' +
    'td><td class="a-text-right"><span class="money">$50,425</span></td><td class="a-text-right"><sp' +
    'an class="money">$195,527</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl163' +
    '0242305?ref_=bo_tt_gr_5">Croatia</a></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '       ' +
    ' &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$299,608</span></td></tr><tr><td' +
    '><a class="a-link-normal" href="/release/rl1949009409?ref_=bo_tt_gr_6">Czech Republic</a></td><' +
    'td>Jul 13, 2017</td><td class="a-text-right"><span class="money">$191,352</span></td><td class=' +
    '"a-text-right"><span class="money">$19,846</span></td></tr><tr><td><a class="a-link-normal" hre' +
    'f="/release/rl1982563841?ref_=bo_tt_gr_7">Denmark</a></td><td>Jul 13, 2017</td><td class="a-tex' +
    't-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$1,368,374</spa' +
    'n></td></tr><tr><td><a class="a-link-normal" href="/release/rl1781237249?ref_=bo_tt_gr_8">Egypt' +
    '</a></td><td>Jul 12, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-t' +
    'ext-right"><span class="money">$607,219</span></td></tr><tr><td><a class="a-link-normal" href="' +
    '/release/rl1764460033?ref_=bo_tt_gr_9">Estonia</a></td><td>Jul 14, 2017</td><td class="a-text-r' +
    'ight">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$146,961</span></t' +
    'd></tr><tr><td><a class="a-link-normal" href="/release/rl1814791681?ref_=bo_tt_gr_10">Finland</' +
    'a></td><td>Jul 12, 2017</td><td class="a-text-right"><span class="money">$88,597</span></td><td' +
    ' class="a-text-right"><span class="money">$463,324</span></td></tr><tr><td><a class="a-link-nor' +
    'mal" href="/release/rl1831568897?ref_=bo_tt_gr_11">France</a></td><td>Aug 2, 2017</td><td class' +
    '="a-text-right"><span class="money">$7,330,631</span></td><td class="a-text-right"><span class=' +
    '"money">$24,197,812</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl196578662' +
    '5?ref_=bo_tt_gr_12">Germany</a></td><td>Aug 3, 2017</td><td class="a-text-right"><span class="m' +
    'oney">$3,094,659</span></td><td class="a-text-right"><span class="money">$10,156,360</span></td' +
    '></tr><tr><td><a class="a-link-normal" href="/release/rl1848346113?ref_=bo_tt_gr_13">Greece</a>' +
    '</td><td>Jul 13, 2017</td><td class="a-text-right"><span class="money">$117,870</span></td><td ' +
    'class="a-text-right"><span class="money">$448,438</span></td></tr><tr><td><a class="a-link-norm' +
    'al" href="/release/rl1647019521?ref_=bo_tt_gr_14">Hungary</a></td><td>Jul 13, 2017</td><td clas' +
    's="a-text-right"><span class="money">$207,158</span></td><td class="a-text-right"><span class="' +
    'money">$566,704</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1714128385?re' +
    'f_=bo_tt_gr_15">Iceland</a></td><td>Jul 14, 2017</td><td class="a-text-right"><span class="mone' +
    'y">$35,042</span></td><td class="a-text-right"><span class="money">$123,041</span></td></tr><tr' +
    '><td><a class="a-link-normal" href="/release/rl1680573953?ref_=bo_tt_gr_16">Israel</a></td><td>' +
    'Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><s' +
    'pan class="money">$1,146,242</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl' +
    '1730905601?ref_=bo_tt_gr_17">Italy</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span ' +
    'class="money">$1,325,225</span></td><td class="a-text-right"><span class="money">$4,295,433</sp' +
    'an></td></tr><tr><td><a class="a-link-normal" href="/release/rl1496024577?ref_=bo_tt_gr_18">Jor' +
    'dan</a></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="' +
    'a-text-right"><span class="money">$208,429</span></td></tr><tr><td><a class="a-link-normal" hre' +
    'f="/release/rl1546356225?ref_=bo_tt_gr_19">Kuwait</a></td><td>Jul 13, 2017</td><td class="a-tex' +
    't-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$1,807,829</spa' +
    'n></td></tr><tr><td><a class="a-link-normal" href="/release/rl1596687873?ref_=bo_tt_gr_20">Latv' +
    'ia</a></td><td>Jul 14, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a' +
    '-text-right"><span class="money">$64,127</span></td></tr><tr><td><a class="a-link-normal" href=' +
    '"/release/rl1563133441?ref_=bo_tt_gr_21">Lebanon</a></td><td>Jul 13, 2017</td><td class="a-text' +
    '-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$369,655</span><' +
    '/td></tr><tr><td><a class="a-link-normal" href="/release/rl1579910657?ref_=bo_tt_gr_22">Lithuan' +
    'ia</a></td><td>Jul 14, 2017</td><td class="a-text-right"><span class="money">$27,181</span></td' +
    '><td class="a-text-right"><span class="money">$84,896</span></td></tr><tr><td><a class="a-link-' +
    'normal" href="/release/rl1865123329?ref_=bo_tt_gr_23">Netherlands</a></td><td>Jul 13, 2017</td>' +
    '<td class="a-text-right"><span class="money">$1,040,645</span></td><td class="a-text-right"><sp' +
    'an class="money">$3,717,606</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1' +
    '395361281?ref_=bo_tt_gr_24">Norway</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span ' +
    'class="money">$309,334</span></td><td class="a-text-right"><span class="money">$469,816</span><' +
    '/td></tr><tr><td><a class="a-link-normal" href="/release/rl1428915713?ref_=bo_tt_gr_25">Oman</a' +
    '></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text' +
    '-right"><span class="money">$469,816</span></td></tr><tr><td><a class="a-link-normal" href="/re' +
    'lease/rl1244366337?ref_=bo_tt_gr_26">Poland</a></td><td>Jul 28, 2017</td><td class="a-text-righ' +
    't"><span class="money">$275,377</span></td><td class="a-text-right"><span class="money">$760,58' +
    '9</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1261143553?ref_=bo_tt_gr_27' +
    '">Portugal</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span class="money">$256,388</' +
    'span></td><td class="a-text-right"><span class="money">$915,738</span></td></tr><tr><td><a clas' +
    's="a-link-normal" href="/release/rl1277920769?ref_=bo_tt_gr_28">Qatar</a></td><td>Jul 13, 2017<' +
    '/td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="mo' +
    'ney">$1,031,787</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1311475201?re' +
    'f_=bo_tt_gr_29">Romania</a></td><td>Jul 14, 2017</td><td class="a-text-right"><span class="mone' +
    'y">$286,648</span></td><td class="a-text-right"><span class="money">$1,036,391</span></td></tr>' +
    '<tr><td><a class="a-link-normal" href="/release/rl1328252417?ref_=bo_tt_gr_30">Serbia and Monte' +
    'negro</a></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class' +
    '="a-text-right"><span class="money">$132,871</span></td></tr><tr><td><a class="a-link-normal" h' +
    'ref="/release/rl1126925825?ref_=bo_tt_gr_31">Slovakia</a></td><td>Jul 13, 2017</td><td class="a' +
    '-text-right"><span class="money">$103,683</span></td><td class="a-text-right"><span class="mone' +
    'y">$208,290</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1110148609?ref_=b' +
    'o_tt_gr_32">Slovenia</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span class="money">' +
    '$14,538</span></td><td class="a-text-right"><span class="money">$59,458</span></td></tr><tr><td' +
    '><a class="a-link-normal" href="/release/rl3190523393?ref_=bo_tt_gr_33">South Africa</a></td><t' +
    'd>Jul 14, 2017</td><td class="a-text-right"><span class="money">$200,702</span></td><td class="' +
    'a-text-right"><span class="money">$724,860</span></td></tr><tr><td><a class="a-link-normal" hre' +
    'f="/release/rl1798014465?ref_=bo_tt_gr_34">Spain</a></td><td>Jul 14, 2017</td><td class="a-text' +
    '-right"><span class="money">$3,304,612</span></td><td class="a-text-right"><span class="money">' +
    '$10,273,417</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1076594177?ref_=b' +
    'o_tt_gr_35">Sweden</a></td><td>Jul 12, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    <' +
    '/td><td class="a-text-right"><span class="money">$1,948,681</span></td></tr><tr><td><a class="a' +
    '-link-normal" href="/release/rl1143703041?ref_=bo_tt_gr_36">Switzerland</a></td><td>Aug 2, 2017' +
    '</td><td class="a-text-right"><span class="money">$421,392</span></td><td class="a-text-right">' +
    '<span class="money">$1,221,978</span></td></tr><tr><td><a class="a-link-normal" href="/release/' +
    'rl1177257473?ref_=bo_tt_gr_37">Turkey</a></td><td>Jul 14, 2017</td><td class="a-text-right"><sp' +
    'an class="money">$597,785</span></td><td class="a-text-right"><span class="money">$2,718,935</s' +
    'pan></td></tr><tr><td><a class="a-link-normal" href="/release/rl3123414529?ref_=bo_tt_gr_38">Uk' +
    'raine</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span class="money">$313,284</span>' +
    '</td><td class="a-text-right"><span class="money">$731,501</span></td></tr><tr><td><a class="a-' +
    'link-normal" href="/release/rl3140191745?ref_=bo_tt_gr_39">United Arab Emirates</a></td><td>Jul' +
    ' 13, 2017</td><td class="a-text-right"><span class="money">$1,836,240</span></td><td class="a-t' +
    'ext-right"><span class="money">$4,090,081</span></td></tr><tr><td><a class="a-link-normal" href' +
    '="/release/rl3156968961?ref_=bo_tt_gr_40">United Kingdom</a></td><td>Jul 11, 2017</td><td class' +
    '="a-text-right"><span class="money">$9,274,100</span></td><td class="a-text-right"><span class=' +
    '"money">$26,717,325</span></td></tr></table><h3>Latin America</h3>' +
    '                    <table c' +
    'lass="a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th cl' +
    'ass="a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 ' +
    'a-text-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl2032895489?ref_=' +
    'bo_tt_gr_1">Argentina</a></td><td>Aug 3, 2017</td><td class="a-text-right"><span class="money">' +
    '$1,369,572</span></td><td class="a-text-right"><span class="money">$3,525,754</span></td></tr><' +
    'tr><td><a class="a-link-normal" href="/release/rl2133558785?ref_=bo_tt_gr_2">Bolivia</a></td><t' +
    'd>Aug 3, 2017</td><td class="a-text-right"><span class="money">$134,948</span></td><td class="a' +
    '-text-right"><span class="money">$699,985</span></td></tr><tr><td><a class="a-link-normal" href' +
    '="/release/rl1881900545?ref_=bo_tt_gr_3">Brazil</a></td><td>Aug 3, 2017</td><td class="a-text-r' +
    'ight"><span class="money">$4,339,291</span></td><td class="a-text-right"><span class="money">$1' +
    '3,690,557</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1932232193?ref_=bo_' +
    'tt_gr_4">Chile</a></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td>' +
    '<td class="a-text-right"><span class="money">$3,076,189</span></td></tr><tr><td><a class="a-lin' +
    'k-normal" href="/release/rl1898677761?ref_=bo_tt_gr_5">Colombia</a></td><td>Aug 3, 2017</td><td' +
    ' class="a-text-right"><span class="money">$1,342,093</span></td><td class="a-text-right"><span ' +
    'class="money">$3,222,183</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1999' +
    '341057?ref_=bo_tt_gr_6">Dominican Republic</a></td><td>Aug 3, 2017</td><td class="a-text-right"' +
    '>' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$260,804</span></td></t' +
    'r><tr><td><a class="a-link-normal" href="/release/rl1747682817?ref_=bo_tt_gr_7">Ecuador</a></td' +
    '><td>Aug 4, 2017</td><td class="a-text-right"><span class="money">$380,694</span></td><td class' +
    '="a-text-right"><span class="money">$1,338,029</span></td></tr><tr><td><a class="a-link-normal"' +
    ' href="/release/rl1479247361?ref_=bo_tt_gr_8">Jamaica</a></td><td>Jul 12, 2017</td><td class="a' +
    '-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$108,672</s' +
    'pan></td></tr><tr><td><a class="a-link-normal" href="/release/rl1361806849?ref_=bo_tt_gr_9">Mex' +
    'ico</a></td><td>Jul 27, 2017</td><td class="a-text-right"><span class="money">$6,606,295</span>' +
    '</td><td class="a-text-right"><span class="money">$16,465,421</span></td></tr><tr><td><a class=' +
    '"a-link-normal" href="/release/rl1462470145?ref_=bo_tt_gr_10">Paraguay</a></td><td>Aug 3, 2017<' +
    '/td><td class="a-text-right"><span class="money">$36,818</span></td><td class="a-text-right"><s' +
    'pan class="money">$81,837</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl144' +
    '5692929?ref_=bo_tt_gr_11">Peru</a></td><td>Aug 3, 2017</td><td class="a-text-right"><span class' +
    '="money">$1,919,501</span></td><td class="a-text-right"><span class="money">$4,918,977</span></' +
    'td></tr><tr><td><a class="a-link-normal" href="/release/rl1194034689?ref_=bo_tt_gr_12">Trinidad' +
    ' & Tobago</a></td><td>Jul 12, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td c' +
    'lass="a-text-right"><span class="money">$584,025</span></td></tr><tr><td><a class="a-link-norma' +
    'l" href="/release/rl3106637313?ref_=bo_tt_gr_13">Uruguay</a></td><td>Aug 3, 2017</td><td class=' +
    '"a-text-right"><span class="money">$72,039</span></td><td class="a-text-right"><span class="mon' +
    'ey">$204,743</span></td></tr></table><h3>Asia Pacific</h3>' +
    '                    <table class="a-' +
    'bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="a-s' +
    'pan2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 a-text-r' +
    'ight">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl2066449921?ref_=bo_tt_gr' +
    '_1">Australia</a></td><td>Jul 27, 2017</td><td class="a-text-right"><span class="money">$3,256,' +
    '241</span></td><td class="a-text-right"><span class="money">$8,131,449</span></td></tr><tr><td>' +
    '<a class="a-link-normal" href="/release/rl1613465089?ref_=bo_tt_gr_2">Hong Kong</a></td><td>Jul' +
    ' 13, 2017</td><td class="a-text-right"><span class="money">$1,736,483</span></td><td class="a-t' +
    'ext-right"><span class="money">$4,560,489</span></td></tr><tr><td><a class="a-link-normal" href' +
    '="/release/rl1697351169?ref_=bo_tt_gr_3">India</a></td><td>Jul 14, 2017</td><td class="a-text-r' +
    'ight"><span class="money">$2,547,849</span></td><td class="a-text-right"><span class="money">$3' +
    ',639,945</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1663796737?ref_=bo_t' +
    't_gr_4">Indonesia</a></td><td>Jul 26, 2017</td><td class="a-text-right"><span class="money">$2,' +
    '280,671</span></td><td class="a-text-right"><span class="money">$3,929,555</span></td></tr><tr>' +
    '<td><a class="a-link-normal" href="/release/rl1512801793?ref_=bo_tt_gr_5">Japan</a></td><td>Oct' +
    ' 13, 2017</td><td class="a-text-right"><span class="money">$2,360,803</span></td><td class="a-t' +
    'ext-right"><span class="money">$6,611,633</span></td></tr><tr><td><a class="a-link-normal" href' +
    '="/release/rl1378584065?ref_=bo_tt_gr_6">Malaysia</a></td><td>Jul 13, 2017</td><td class="a-tex' +
    't-right"><span class="money">$1,422,692</span></td><td class="a-text-right"><span class="money"' +
    '>$3,157,885</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1345029633?ref_=b' +
    'o_tt_gr_7">Mongolia</a></td><td>Jul 21, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    ' +
    '</td><td class="a-text-right"><span class="money">$144,352</span></td></tr><tr><td><a class="a-' +
    'link-normal" href="/release/rl1412138497?ref_=bo_tt_gr_8">New Zealand</a></td><td>Jul 13, 2017<' +
    '/td><td class="a-text-right"><span class="money">$656,570</span></td><td class="a-text-right"><' +
    'span class="money">$1,846,081</span></td></tr><tr><td><a class="a-link-normal" href="/release/r' +
    'l1227589121?ref_=bo_tt_gr_9">Pakistan</a></td><td>Jul 14, 2017</td><td class="a-text-right">' +
    '  ' +
    '      &ndash;' +
    '    </td><td class="a-text-right"><span class="money">$258,697</span></td></tr><t' +
    'r><td><a class="a-link-normal" href="/release/rl1210811905?ref_=bo_tt_gr_10">Philippines</a></t' +
    'd><td>Jul 12, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-rig' +
    'ht"><span class="money">$1,521,436</span></td></tr><tr><td><a class="a-link-normal" href="/rele' +
    'ase/rl1294697985?ref_=bo_tt_gr_11">Russia/CIS</a></td><td>Jul 13, 2017</td><td class="a-text-ri' +
    'ght"><span class="money">$5,316,664</span></td><td class="a-text-right"><span class="money">$11' +
    ',616,165</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1093371393?ref_=bo_t' +
    't_gr_12">Singapore</a></td><td>Jul 13, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    <' +
    '/td><td class="a-text-right"><span class="money">$2,222,992</span></td></tr><tr><td><a class="a' +
    '-link-normal" href="/release/rl1529579009?ref_=bo_tt_gr_13">South Korea</a></td><td>Aug 15, 201' +
    '7</td><td class="a-text-right"><span class="money">$11,363,587</span></td><td class="a-text-rig' +
    'ht"><span class="money">$14,974,431</span></td></tr><tr><td><a class="a-link-normal" href="/rel' +
    'ease/rl3089860097?ref_=bo_tt_gr_14">Taiwan</a></td><td>Jul 13, 2017</td><td class="a-text-right' +
    '"><span class="money">$2,115,810</span></td><td class="a-text-right"><span class="money">$5,280' +
    ',017</span></td></tr><tr><td><a class="a-link-normal" href="/release/rl1160480257?ref_=bo_tt_gr' +
    '_15">Thailand</a></td><td>Jul 13, 2017</td><td class="a-text-right"><span class="money">$1,353,' +
    '345</span></td><td class="a-text-right"><span class="money">$2,268,541</span></td></tr><tr><td>' +
    '<a class="a-link-normal" href="/release/rl3173746177?ref_=bo_tt_gr_16">Vietnam</a></td><td>Jul ' +
    '14, 2017</td><td class="a-text-right">' +
    '        &ndash;' +
    '    </td><td class="a-text-right"><span ' +
    'class="money">$977,187</span></td></tr></table><h3>China</h3>' +
    '                    <table class=' +
    '"a-bordered a-horizontal-stripes a-size-base-plus"><tr><th class="a-span4">Area</th><th class="' +
    'a-span2">Release Date</th><th class="a-span3 a-text-right">Opening</th><th class="a-span3 a-tex' +
    't-right">Gross</th></tr><tr><td><a class="a-link-normal" href="/release/rl1915454977?ref_=bo_tt' +
    '_gr_1">China</a></td><td>Sep 15, 2017</td><td class="a-text-right"><span class="money">$59,797,' +
    '866</span></td><td class="a-text-right"><span class="money">$112,150,301</span></td></tr></tabl' +
    'e></div></div></div></div></main>' +
    '            <div class="a-section a-spacing-none mojo-navigat' +
    'ion-frame"><div class="a-section a-spacing-none mojo-navigation mojo-footer"><a class="a-link-n' +
    'ormal imdb-pro-logo" href="https://pro.imdb.com/?ref_=mojo_ft_tt_prologo&amp;rf=mojo_ft_tt_prol' +
    'ogo"></a><p>' +
    '            Latest Updates:' +
    '            <a class="a-link-normal" href="/news/?ref_' +
    '=bo_ft_tt_news">News</a> |' +
    '            <a class="a-link-normal" href="/daily/?ref_=bo_ft_tt_dai' +
    'ly">Daily</a> |' +
    '            <a class="a-link-normal" href="/weekend/?ref_=bo_ft_tt_weekend">Wee' +
    'kend</a> |';

  fBOMCountryLinks := TDictionary<String, String>.Create;
  try
    THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fPageSource, fBOMCountryLinks);

    CheckEquals(71, fBOMCountryLinks.Count, 'Count mismatch');
    CheckEqualsString('/release/rl1782744577', fBOMCountryLinks.Items['USA'], 'Link mismatch');
    CheckEqualsString('/release/rl1730905601', fBOMCountryLinks.Items['Italy'], 'Link mismatch');
    CheckEqualsString('/release/rl1261143553', fBOMCountryLinks.Items['Portugal'], 'Link mismatch');
    CheckEqualsString('/release/rl1965786625', fBOMCountryLinks.Items['Germany'], 'Link mismatch');
    CheckEqualsString('/release/rl1831568897', fBOMCountryLinks.Items['France'], 'Link mismatch');
  finally
    fBOMCountryLinks.Free;
  end;
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetWidestScreensCount1;
var
  fPageSource: String;
  fScreens: Integer;
begin
  // tt0375568
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>3,020 theaters</span></div>';

  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);

  CheckEquals(3020, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetWidestScreensCount2;
var
  fPageSource: String;
  fScreens: Integer;
begin
  // tt0455275
  fPageSource := '';

  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);

  CheckEquals(0, fScreens, 'Screens count mismatch');
end;

procedure TTestTHtmlBoxOfficeMojoParser.TestGetWidestScreensCount3;
var
  fPageSource: String;
  fScreens: Integer;
begin
  // tt3450958
  fPageSource := '<div class="a-section a-spacing-none"><span>Widest Release</span><span>4,100 theaters</span></div>';

  fScreens := THtmlBoxOfficeMojoParser.GetWidestScreensCount(fPageSource);

  CheckEquals(4100, fScreens, 'Screens count mismatch');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('THtmlIMDbParser', TTestTHtmlIMDbParser.Suite);
    RegisterTest('THtmlBoxOfficeMojoParser', TTestTHtmlBoxOfficeMojoParser.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser);
    TDUnitX.RegisterTestFixture(TTestTHtmlBoxOfficeMojoParser);
  {$ENDIF}
end.
