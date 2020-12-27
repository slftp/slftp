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

initialization
  {$IFDEF FPC}
    RegisterTest('THtmlIMDbParser', TTestTHtmlIMDbParser.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTHtmlIMDbParser);
  {$ENDIF}
end.
