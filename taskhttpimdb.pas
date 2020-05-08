unit taskhttpimdb;

interface

uses
  Classes, tasksunit, sltcp;

{ Just a helper function to initialize @link(ImdbVotesRegexList) and @link(ImdbRatingRegexList) }
procedure TaskHttpImdbInit;

{ Just a helper function to free @link(ImdbVotesRegexList) and @link(ImdbRatingRegexList) }
procedure TaskHttpImdbUnInit;

type
  TPazoHTTPImdbTask = class(TTask)
  private
    rls: String;
    imdb_id: String;
  public
    constructor Create(const imdb_id: String; const rls: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SysUtils, irc, StrUtils, debugunit, dateutils, configunit, kb, http,
  sitesunit, regexpr, dbaddimdb, mystrings, dbtvinfo;

const
  section = 'taskhttpimdb';

var
  ImdbVotesRegexList, ImdbRatingRegexList: TStringList;

procedure TaskHttpImdbInit;
begin
  (* Initialize StringLists with valid regex for imdb rating/votes
     This allows us to reduce alot of code duplication and nested if/else
     constructs. Create them here as unit variables instead of in the function
     to not have to recreate them on every call to imdb, which is just useless
     overhead.

     Newest versions should be added on top as they will be checked against the
     website in ascending order. Quicker match = less overhead.

     Syntax is regex=matchVariableIndex
  *)
  ImdbVotesRegexList := TStringList.Create;
  with ImdbVotesRegexList do
  begin
    Add('<strong.*?on (\S+) user ratings\"><span.*?>\d+[,.]\d<\/span>=1');
    Add('<span[^<>]*itemprop="ratingCount">(\S+)<\/span>=1');
    Add('\>(\S+) votes<\/a>\)=1');
    Add('<a href=\"ratings\" class=\"tn15more\">(.*?) (Bewertungen|votes|Stimmen)<\/a>=1');
  end;
  ImdbRatingRegexList := TStringList.Create;
  with ImdbRatingRegexList do
  begin
    Add('<strong.*?user ratings\"><span.*?>(\d+[,.]\d)<\/span>=1');
    Add('<span[^<>]*itemprop="ratingValue">(\d+[,.]\d+)<\/span>=1');
    Add('<span class="rating-rating">(\d+[,.]\d+)<span>\/10<\/span><\/span>=1');
    Add('<b>(\d+[,.]\d+)\/10<\/b>=1');
  end;
end;

procedure TaskHttpImdbUnInit;
begin
  if Assigned(ImdbVotesRegexList) then
    ImdbVotesRegexList.Free;
  if Assigned(ImdbRatingRegexList) then
    ImdbRatingRegexList.Free;
end;

{ TPazoHTTPImdbTask }

constructor TPazoHTTPImdbTask.Create(const imdb_id: String; const rls: String);
begin
  self.imdb_id := imdb_id;
  self.rls := rls;
  inherited Create('', '', getAdminSiteName);
end;

function TPazoHTTPImdbTask.Execute(slot: Pointer): Boolean;
var
  imdb_stv: boolean;
  imdb_year, imdb_screens: Integer;
  imdbdata: TDbImdbData;
  rr, rr2: TRegexpr;
  imdb_mtitle, imdb_extra, imdb_date, s, imdb_counline, imdb_country, rlang,
    imdb_genr, imdb_countr, imdb_lang, imdb_region, bom_date, imdb_votes, imdb_rating: String;
  ir: TImdbRelease;
  i: integer;
  mainsite, rlsdatesite, businesssite, bomsite: String;
  release_date: TDateTime;
  formatSettings: TFormatSettings;
  showname: String;
  season: integer;
  episode: int64;
  fBOMSearchNeeded: boolean;
  fBusinessInfoPart: String;
  fRlsdateExtraInfo: String;
  fPictureID: String;
  fHttpGetErrMsg: String;
begin
  Result := False;
  fPictureID := '';

  if (rls = '') then
  begin
    irc_Adderror(Format('<c4>[ERROR]</c> TPazoHTTPImdbTask rls empty.', []));
    Result := True;
    ready := True;
    exit;
  end;

  try
    ir := TImdbRelease.Create(rls, '');
  except
    on e: Exception do
    begin
      mainsite := '';
      rlsdatesite := '';
      businesssite := '';
      Debug(dpError, section,
        Format('[EXCEPTION] TPazoHTTPImdbTask TImdbRelease.Create: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoHTTPImdbTask TImdbRelease.Create: %s', [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;

  rr := TRegexpr.Create;
  try
    rr.ModifierI := True;

    imdb_year := 0;

    (*  Fetch MainInfoPage from iMDB *)
    if not HttpGetUrl('https://www.imdb.com/title/' + imdb_id + '/', mainsite, fHttpGetErrMsg) then
    begin
      Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask mainpage --> %s ', [fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask mainpage --> %s', [fHttpGetErrMsg]));
      Result := True;
      ready := True;
      exit;
    end;

    (*  Fetch MovieTitle/Extra/Year from iMDB *)
//    rr.Expression :=
//      '<title>(\&\#x22;|\")?(.*?)\1?\s*\((TV\s*Series|TV\s*mini-series|TV|TV\s*Movie|Video|Video Game)?\s*(\d{4})((\-|&ndash;|–|&emdash;)(\d{4})?\s*(&nbsp;)?)?(\/.+)?\)( - IMDb)?<\/title>';
//    if rr.Exec(mainsite) then
//    begin
//     imdb_year := StrToInt(rr.Match[4]);
//      imdb_mtitle := rr.Match[2];
//      imdb_extra := rr.Match[3];
//    end;

    {
    * Examples:
    * <meta property='og:title' content="War for the Planet of the Apes (2017)" />
    * <meta property='og:title' content="The Hunger Games: Catching Fire (2013)" />
    * <meta property='og:title' content="The Da Vinci Code (2006)" />
    }
    rr.Expression := '<meta property=\''og:title\'' content="(.*?)\s*\((.*?)?\s*(\d{4}).*?\"';
    if rr.Exec(mainsite) then
    begin
      imdb_mtitle := rr.Match[1];
      imdb_extra := rr.Match[2];
      imdb_year := StrToInt(rr.Match[3]);
    end;


    {
    * examples
    * old -> mtitle: Die glorreichen Sieben -- year: 2016 -- extra:
    * new -> mtitle: The Magnificent Seven -- year: 2016 -- extra:
    * old -> mtitle: Mrs. Parker und ihr lasterhafter Kreis -- year: 1994 -- extra:
    * new -> mtitle: Mrs. Parker and the Vicious Circle -- year: 1994 -- extra:
    * new -> mtitle: &quot;The Detour&quot; The Pilot -- year: 2016 -- extra: TV Episode
    * new -> mtitle: &quot;The Detour&quot; The Hotel -- year: 2016 -- extra: TV Episode
    * old -> mtitle: Sam &amp; Cat -- year: 2013 -- extra: TV Series
    * new -> mtitle: Sam & Cat -- year: 2013 -- extra: TV Series
    * old -> mtitle: Cars 3: Evolution -- year: 2017 -- extra:
    * new -> mtitle: Cars 3 -- year: 2017 -- extra:
    * old -> mtitle: Shameless - Nicht ganz nÃ¼chtern -- year: 2011 -- extra: TV Series
    * new -> mtitle: Shameless -- year: 2011 -- extra: TV Series
    * old -> mtitle: Hot Shots! Der 2. Versuch -- year: 1993 -- extra:
    * new -> mtitle: Hot Shots! Part Deux -- year: 1993 -- extra:
    * old -> mtitle: Tad Jones und das Geheimnis von KÃ¶ng Midas -- year: 2017 -- extra:
    * new -> mtitle: Tad the Lost Explorer and the Secret of King Midas -- year: 2017 -- extra:
    * old -> mtitle: The Da Vinci Code - Sakrileg -- year: 2006 -- extra:
    * new -> mtitle: The Da Vinci Code -- year: 2006 -- extra:
    }

    (*
    if imdb_year = 0 then begin
        rr.free;
        exit;
    end;
    *)

    imdbdata := TDbImdbData.Create(imdb_id);

    imdbdata.imdb_origtitle := imdb_mtitle;

    rr2 := TRegexpr.Create;
    try
      rr2.ModifierI := True;
      imdb_lang := '';
      imdb_countr := '';
      imdb_genr := '';
      imdb_votes := '-10';
      imdb_rating := '-10';

      rr.Expression :=
        '\((voting begins after release|awaiting 5 votes|noch keine 5 Bewertungen)\)|<div class="notEnoughRatings">Needs 5 Ratings</div>|<div class="notEnoughRatings">Coming Soon</div>';
      if rr.Exec(mainsite) then
      begin
        imdb_votes := '-1';
        imdb_rating := '-1';
      end
      else
      begin
        (*  Fetch Votes from iMDB *)
        for i := 0 to ImdbVotesRegexList.Count-1 do
        begin
          rr.Expression := ImdbVotesRegexList.Names[i];
          if rr.Exec(mainsite) then
          begin
            imdb_votes := rr.Match[StrToIntDef(ImdbVotesRegexList.ValueFromIndex[i], 1)];
            break;
          end;
        end;

        (*  Fetch Rating from iMDB  *)
        for i := 0 to ImdbRatingRegexList.Count-1 do
        begin
          rr.Expression := ImdbRatingRegexList.Names[i];
          if rr.Exec(mainsite) then
          begin
            imdb_rating := rr.Match[StrToIntDef(ImdbRatingRegexList.ValueFromIndex[i], 1)];
            break;
          end;
        end;
      end;

      imdb_votes := StringReplace(imdb_votes, '.', '', [rfReplaceAll, rfIgnoreCase]);
      imdb_votes := StringReplace(imdb_votes, ',', '', [rfReplaceAll, rfIgnoreCase]);
      imdbdata.imdb_votes := StrToIntDef(imdb_votes, -5);
      imdb_rating := StringReplace(imdb_rating, '.', '', [rfReplaceAll, rfIgnoreCase]);
      imdb_rating := StringReplace(imdb_rating, ',', '', [rfReplaceAll, rfIgnoreCase]);
      imdbdata.imdb_rating := StrToIntDef(imdb_rating, -5);

      (*  Fetch Languages from iMDB  *)
      // Expression designed to work with new and old layouts of iMDB (04/10/2010)
      // new 'relaxed' expression to work with latest imdb changes (13/9/2016)
      rr2.Expression :=
        '<a[^>]+href=[^>]+>([^<]+)<\/a>';
      // Trying new layout of iMDB first
      rr.Expression :=
        '<div class="txt-block">[^<]*<h4 class="inline">Language:<\/h4>[^<]*(<.*?<\/a>)[^<]*<\/div>';
      if rr.Exec(mainsite) then
      begin
        if rr2.Exec(rr.Match[1]) then
        begin
          repeat imdb_lang := imdb_lang + rr2.Match[1] + ',';
          until not rr2.ExecNext;
        end;
      end
      else
      begin
        // Trying old layout of iMDB if new layout fails
        rr.Expression :=
          '<div class=\"info\"><h5>Language:<\/h5><div class=\"info-content\">(<.*?<\/a>)<\/div><\/div>';
        if rr.Exec(mainsite) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat imdb_lang := imdb_lang + rr2.Match[3] + ',';
            until not rr2.ExecNext;
        end;
      end;
      delete(imdb_lang, length(imdb_lang), 1);
      imdbdata.imdb_languages.CommaText := imdb_lang;

      (*  Fetch Countries from iMDB  *)
      // Expression designed to work with new and old layouts of iMDB (04/10/2010)
      // new 'relaxed' expression to work with latest imdb changes (13/9/2016)
      rr2.Expression :=
        '<a[^>]+href=[^>]+>([^<]+)<\/a>';
      // Trying new layout of iMDB first
      rr.Expression :=
        '<div class="txt-block">[^<]*<h4 class="inline">Country:<\/h4>[^<]*(<.*?<\/a>)[^<]*<\/div>';
      if rr.Exec(mainsite) then
      begin
        if rr2.Exec(rr.Match[1]) then
          repeat imdb_countr := imdb_countr + rr2.Match[1] + ',';
          until not rr2.ExecNext;
      end
      else
      begin
        // Trying old layout of iMDB if new layout fails
        rr.Expression :=
          '<div class=\"info\"><h5>Country:<\/h5><div class=\"info-content\">(<.*?<\/a>)<\/div><\/div>';
        if rr.Exec(mainsite) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat imdb_countr := imdb_countr + rr2.Match[4] + ',';
            until not rr2.ExecNext;
        end;
      end;
      delete(imdb_countr, length(imdb_countr), 1);
      imdbdata.imdb_countries.CommaText := imdb_countr;
      (*
      end;
      *)

      (*  Fetch Gernres from iMDB  *)
      // Expression designed to work with new and old layouts of iMDB (24.09.2011)
      rr2.Expression :=
        '<a[^>]+>\s(\S+)<\/a>';
      // Trying new layout of iMDB first
      rr.Expression := '(<h4 class="inline">Genres:<\/h4>(\s*<a href\S+\s>.*?<\/a>(\S+<\/span>)?\s*)+<\/div>)';
      if rr.Exec(mainsite) then
      begin
        if rr2.Exec(rr.Match[1]) then
          repeat imdb_genr := imdb_genr + rr2.Match[1] + ',';
          until not rr2.ExecNext;
      end
      else
      begin
        // Trying old layout of iMDB if new layout fails
        rr.Expression :=
          '<h5>Genre:<\/h5>\n<div class=\"info-content\">\s+(.*?)<\/div>';
        if rr.Exec(mainsite) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat imdb_genr := imdb_genr + rr2.Match[3] + ',';
            until not rr2.ExecNext;
        end;
      end;

      delete(imdb_genr, length(imdb_genr), 1);
      imdbdata.imdb_genres.CommaText := imdb_genr;

      //irc_addtext('CONSOLE','ADMIN','LANGUAGE = %s -- rlang = %s',[ir.languages.text,rlang]);

      (* Get Cleanup STV Infos - mod done by a kraut so u see we can do beauty things too ;) - *)
      ir.imdb_stvs := '/!\ UNTOUCHED /!\';
      ir.imdb_stvm := True;
      imdb_stv := False;
      imdb_country := '';
      imdb_counline := '';
      imdb_region := '';

      if (ir.language <> 'English') then
        rlang := ir.language
      else
        rlang := 'USA';

      imdb_counline := imdbcountries.ReadString('COMMON', rlang, '');
      imdb_region := SubString(imdb_counline, ',', 1);
      imdb_country := SubString(imdb_counline, ',', 2);

      (* Movie is actually a MiniSeries designed for Television *)
      if imdb_extra = 'TV mini-series' then
      begin
        imdbdata.imdb_stvm := True;
        imdb_stv := True;
        imdbdata.imdb_stvs := 'Mini_series';
      end;

      (* Movie is actually a Videogame, STV or TV Production *)
      if not imdb_stv then
      begin
        if ( AnsiContainsText(imdb_extra, 'TV') or AnsiContainsText(imdb_extra, 'Video') ) then
        begin
          imdbdata.imdb_stvm := True;
          imdb_stv := True;
          imdbdata.imdb_stvs := 'Videogame_TV_Video';
        end;
      end;

      imdb_date := '';

      (* Get STV Info through releaseinfo page from iMDB *)
      if not HttpGetUrl('https://www.imdb.com/title/' + imdb_id + '/releaseinfo', rlsdatesite, fHttpGetErrMsg) then
      begin
        Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask releaseinfo --> %s ', [fHttpGetErrMsg]));
        irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask releaseinfo --> %s', [fHttpGetErrMsg]));
        Result := True;
        ready := True;
        exit; // TODO: skip releaseinfo webpage crawl if failed instead of stoping complete imdb parsing task
      end;

      if not imdb_stv then
      begin
        rr.ModifierI := True;

        // Examples:
        //          <tr class="even"><td><a href="/calendar/?region=my&ref_=ttrel_rel_2">Malaysia</a></td><td class="release_date">28 September 2017</td><td></td></tr>
        //          <tr class="odd"><td><a href="/calendar/?region=us&ref_=ttrel_rel_11">USA</a></td><td class="release_date">20 October 1983</td><td> (New York City, New York)</td></tr>
        //          <tr class="even"><td><a href="/calendar/?region=us&ref_=ttrel_rel_12">USA</a></td><td class="release_date">1 December 1983</td><td></td></tr>
        //          <tr class="odd"><td><a href="/calendar/?region=us&ref_=ttrel_rel_1">USA</a></td><td class="release_date">24 September 2017</td><td> (Beijing) (premiere)</td></tr>
        //          <tr class="even"><td><a href="/calendar/?region=us&ref_=ttrel_rel_32">USA</a></td><td class="release_date">18 December 2017</td><td> (internet)</td></tr>
        rr.Expression :=
        '<tr class="(odd|even)">[\s\n]*?<td><a href=\"\/calendar\/\?region\=' + imdb_region + '\&ref\_\=ttrel\_rel\_\d+"\s*>' + imdb_country
          + '<\/a><\/td>[\s\n]*?<td class="release_date">(.*?)<\/td>[\s\n]*?<td>(.*?)<\/td><\/tr>';
        if rr.Exec(rlsdatesite) then
        begin
          imdb_date := Trim(rr.Match[2]);
          fRlsdateExtraInfo := Trim(rr.Match[3]);
          imdbdata.imdb_stvs := 'Cinedate: ' + imdb_date;
          imdbdata.imdb_stvm := False;
          imdb_stv := False;
          (* Fetching Cinedate for imdb_country *)
          imdbdata.imdb_cineyear :=  StrToIntDef(copy(imdb_date, Length(imdb_date) - 4, 4), -1);

          if fRlsdateExtraInfo <> '' then
          begin
            rr2.ModifierI := True;
            rr2.Expression := '(DVD|video|TV|Bluray|Blueray)(\s|\.|\-)?premiere';
            if rr2.Exec(fRlsdateExtraInfo) then
            begin
              imdbdata.imdb_stvs := Format('%s, %s [%s]', [imdb_country, imdb_date, fRlsdateExtraInfo]); // USA, 5 December 2005 [(New York City, New York) (premiere)]
              imdb_stv := True;
            end;
            (*  Fetching Festival infos for imdb_country  *)
            rr2.Expression := 'F(estival|ilmfest|est|ilm(\s|\.|\-)?Market?)';
            if rr2.Exec(fRlsdateExtraInfo) then
            begin
              imdbdata.imdb_festival := True;
            end;
          end;
        end;
      end;

      s := '0';
      imdb_screens := 0;
      fBOMSearchNeeded := True;

      imdbdata.imdb_screens := imdb_screens;
      imdbdata.imdb_wide := False;
      imdbdata.imdb_ldt := False;

      // if we get values for season or episode, it's a tv show which don't has any screens
      getShowValues(rls, showname, season, episode);
      if (season = 0) and (episode = 0) then
      begin

        rr.Expression := '<h\d?.*?>Box\s*Office<\/h\d?>(.*?)<hr\s*\/>';
        if rr.Exec(mainsite) then
        begin
          fBusinessInfoPart := rr.Match[1];

          // maybe check first if it list: Opening Weekend USA or other relevant countries

          //Debug(dpError, section, Format('IMDb Box Office: %s', [fBusinessInfoPart]));

          // check what kind of Box Office info we have
          if AnsiContainsText(fBusinessInfoPart, 'Wide Release') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Wide Release';
            imdbdata.imdb_wide := True;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := False;
          end
          else if AnsiContainsText(fBusinessInfoPart, 'Limited') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Limited';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := True;
            fBOMSearchNeeded := False;
          end
          else if AnsiContainsText(fBusinessInfoPart, 'Gross') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Gross weight found, so its not STV!';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := True; // better to a BOM check
          end
          else
          begin
            imdb_stv := True;
            imdbdata.imdb_stvs := 'Box Office found, but not handled!';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := True; // BOM check needed!
          end;
        end
        else
        begin
          imdb_stv := True;
          imdbdata.imdb_stvs := 'No Box Office found, so its STV!';
          imdbdata.imdb_wide := False;
          imdbdata.imdb_ldt := False;
          fBOMSearchNeeded := True;
        end;

        if config.ReadBool('dbaddimdb', 'parse_boxofficemojo_always', False) then
        begin
          fBOMSearchNeeded := True;
        end;

        if fBOMSearchNeeded then
        begin
          if not HttpGetUrl('https://www.boxofficemojo.com/title/' + imdb_id + '/', bomsite, fHttpGetErrMsg) then
          begin
            Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
            irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
            Result := True;
            ready := True;
            exit; // TODO: skip boxofficemojo webpage crawl if failed instead of stoping complete imdb parsing task
          end;

          rr2.Expression := '<option value="(/releasegroup/gr\d+/)">Original Release</option>';
          if rr2.Exec(bomsite) then
          begin
            if not HttpGetUrl('https://www.boxofficemojo.com' + rr2.Match[1], bomsite, fHttpGetErrMsg) then
            begin
              Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
              irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
              Result := True;
              ready := True;
              exit;
            end;

            rr2.Expression := '<a class="a-link-normal" href="(/release/rl\d+/)[^\"]*">Domestic[^\n]*</a>';
            if rr2.Exec(bomsite) then
            begin
              if not HttpGetUrl('https://www.boxofficemojo.com' + rr2.Match[1], bomsite, fHttpGetErrMsg) then
              begin
                Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
                irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
                Result := True;
                ready := True;
                exit;
              end;

              rr2.Expression := '<div[^>]*><span>Widest Release</span><span>([0-9,]*) theaters</span></div>';
              if rr2.Exec(bomsite) then
              begin
                s := ReplaceText(rr2.Match[1], ',', '');
                if StrToIntDef(s, 0) > imdb_screens then
                  imdb_screens := StrToIntDef(s, 0);
              end;
            end;
          end;

          imdbdata.imdb_screens := imdb_screens;

          imdbdata.imdb_wide := False;
          imdbdata.imdb_ldt := False;

          if (imdbdata.imdb_screens > 599) then
          begin
            imdbdata.imdb_wide := True;
            imdbdata.imdb_ldt := False;
          end
          else
          begin
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := True;
          end;

          if rlang = 'USA' then
          begin
            if imdbdata.imdb_screens = 0 then
            begin
              imdb_stv := True;
              imdbdata.imdb_stvs := 'USA and zero screens = STV!';
              imdbdata.imdb_wide := False;
              imdbdata.imdb_ldt := False;

              //Sometimes imdb box office has no screens for cine stuff, so we need to be tricky ;) yay i love that game :)
              rr.Expression :=
                '<a href="\/title\/tt\d+\/business\?ref_=.*?"[\r\n\s]+class=\"quicklink quicklinkGray\" >Box Office\/Business<\/a>';
              if not rr.Exec(mainsite) then
              begin
                imdb_stv := True;
                imdbdata.imdb_stvs := 'No Link to business found, so its STV!';
                imdbdata.imdb_wide := False;
                imdbdata.imdb_ldt := False;
              end;

              rr.Expression :=
                '<h\d?.*?>Box\s*Office<\/h\d?>(.*?)<hr\s*\/>';
              if not rr.Exec(mainsite) then
              begin
                imdb_stv := True;
                imdbdata.imdb_stvs := 'No Box Office found, so its STV!';
                imdbdata.imdb_wide := False;
                imdbdata.imdb_ldt := False;
              end;
            end
            else
            begin
              imdb_stv := False;
              imdbdata.imdb_stvs := 'USA with screens can''t be STV!';
            end;
          end;

        end;

      end;

    finally
      rr2.free;
    end;
  finally
    rr.free;
  end;


  imdbdata.imdb_id := imdb_id;
  imdbdata.imdb_year := imdb_year;
  imdbdata.imdb_stvm := imdb_stv;
  mainsite := '';
  rlsdatesite := '';
  businesssite := '';
  bomsite := '';

  ir.Free;

  try
    dbaddimdb_SaveImdbData(rls, imdbdata);
  except
    on e: Exception do
    begin
      Debug(dpError, section,
        Format('[EXCEPTION] TPazoHTTPImdbTask dbaddimdb_SaveImdb: %s ',
        [e.Message]));
    end;
  end;

  ready := True;
  Result := True;
end;

function TPazoHTTPImdbTask.Name: String;
begin
  try
    Result := Format('HTTPImdb %s : %s', [rls, imdb_id]);
  except
    Result := 'HTTPImdb';
  end;
end;

destructor TPazoHTTPImdbTask.Destroy;
begin
  inherited;
end;

end.

