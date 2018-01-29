unit taskhttpimdb;

interface

uses
  Classes, pazo, tasksunit, sltcp;

type
  TPazoHTTPImdbTask = class(TTask)
  private
    rls: AnsiString;
    imdb_id: AnsiString;
  public
    constructor Create(const imdb_id: AnsiString; const rls: AnsiString);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: AnsiString; override;
  end;

implementation

uses
  SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
  configunit, dirlist, mystrings, sitesunit, console, slhttp, regexpr,
  dbaddimdb, tasksitenfo, Contnrs, dbtvinfo;

const
  section = 'taskhttpimdb';

  { TPazoHTTPImdbTask }

constructor TPazoHTTPImdbTask.Create(const imdb_id: AnsiString; const rls: AnsiString);
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
    imdb_genr, imdb_countr, imdb_lang, imdb_region, bom_date: AnsiString;
  ir: TImdbRelease;

  mainsite, rlsdatesite, businesssite, bomsite: AnsiString;
  release_date: TDateTime;
  formatSettings: TFormatSettings;
  showname: AnsiString;
  season: integer;
  episode: int64;
  fBOMSearchNeeded: boolean;
  fBusinessInfoPart: AnsiString;
  fRlsdateExtraInfo: AnsiString;
  fPictureID: AnsiString;
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
    try

      mainsite := slUrlGet('http://www.imdb.com/title/' + imdb_id + '/', '');
    except
      on e: Exception do
      begin
        Debug(dpError, section,
          Format('[EXCEPTION] TPazoHTTPImdbTask slUrlGet: %s ', [e.Message]));
        irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoHTTPImdbTask slUrlGet: %s',
          [e.Message]));
        Result := True;
        ready := True;
        exit;
      end;
    end;

    if (length(mainsite) < 10) then
    begin
      Result := True;
      ready := True;
      irc_Adderror(Format('<c4>[ERROR]</c> TPazoHTTPImdbTask Size (%d) not enought : http://www.imdb.com/title/%s/ ', [length(mainsite), imdb_id]));
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
      imdbdata.imdb_rating := -5;
      imdbdata.imdb_votes := -5;
      imdb_lang := '';
      imdb_countr := '';
      imdb_genr := '';

      (*  Fetch Votes from iMDB *)
      // Trying newest iMDB layout from 24.09.2011 first
      rr.Expression := '<span[^<>]*itemprop="ratingCount">(\S+)<\/span>';
      if rr.Exec(mainsite) then
      begin
        rr2.Expression := '[\.\,]';
        imdbdata.imdb_votes := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
      end
      else
      begin
        // Trying new layout of iMDB next
        rr.Expression := '\>(\S+) votes<\/a>\)';
        if rr.Exec(mainsite) then
        begin
          rr2.Expression := '[\.\,]';
          imdbdata.imdb_votes := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
        end
        else
        begin
          // Trying old layout of iMDB if the other layouts fail
          rr.Expression :=
            '<a href=\"ratings\" class=\"tn15more\">(.*?) (Bewertungen|votes|Stimmen)<\/a>';
          if rr.Exec(mainsite) then
          begin
            rr2.Expression := '[\.\,]';
            imdbdata.imdb_votes := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
          end
          else
          begin
            // Apparently no votes yet - no-votes regex should work for new and old layout
            rr.Expression :=
              '\((voting begins after release|awaiting 5 votes|noch keine 5 Bewertungen)\)|<div class="notEnoughRatings">Needs 5 Ratings</div>|<div class="rating rating-big" [^<]+ title="Awaiting enough ratings - click stars to rate">';
            if rr.Exec(mainsite) then
              imdbdata.imdb_votes := -1;
              imdbdata.imdb_rating := -1;
          end;
        end;
      end;

      (*  Fetch Rating from iMDB  *)
      // Trying newest iMDB layout from 24.09.2011 first
      rr.Expression := '<span[^<>]*itemprop="ratingValue">(\d+\.\d+)<\/span>';
      if rr.Exec(mainsite) then
      begin
        rr2.Expression := '[\.\,]';
        imdbdata.imdb_rating := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
      end
      else
      begin
        // Trying new layout of iMDB next
        rr.Expression :=
          '<span class="rating-rating">(\d+\.\d+)<span>\/10<\/span><\/span>';
        if rr.Exec(mainsite) then
        begin
          rr2.Expression := '[\.\,]';
          imdbdata.imdb_rating := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
        end
        else
        begin
          // Trying old layout of iMDB if the other layouts fail
          rr.Expression := '<b>(\d+\.\d+)\/10<\/b>';
          if rr.Exec(mainsite) then
          begin
            rr2.Expression := '[\.\,]';
            imdbdata.imdb_rating := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
          end;
        end;
      end;

      (* Rating/Votes combined occured on some of the newer layouts but vanished by now *)
      rr.Expression :=
        '<div class="rating rating-big" [^<]+ title="Users rated this (\d+\.\d+)\/10 \((\d+(,\d+)?) votes\) - click stars to rate">';
      if rr.Exec(mainsite) then
      begin
        rr2.Expression := '[\.\,]';
        imdbdata.imdb_rating := StrToIntDef(rr2.Replace(rr.Match[1], '', False), 0);
        imdbdata.imdb_votes := StrToIntDef(rr2.Replace(rr.Match[2], '', False), 0);
      end;

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
        '<a\s*(onclick=\"[^\"]+\")?\s*href=\"\/(Sections\/)?Genres?\/?[^<>]+\/?\"\s*>([^<>]+)<\/a>';
      // Trying new layout of iMDB first
      rr.Expression := '<h4 class="inline">Genres:<\/h4>\s*(<.*?<\/a>)\s*<\/div>';
      if rr.Exec(mainsite) then
      begin
        if rr2.Exec(rr.Match[1]) then
          repeat imdb_genr := imdb_genr + rr2.Match[3] + ',';
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

      (*
          if config.ReadBool('kb','use_new_language_base',False) then begin
          if uppercase(ir.languages.text) <> 'ENGLISH' then rlang:='USA' else rlang:=ir.languages.Strings[0];
          end else begin
          if ir.languages.text <> '' then rlang:=ir.languages.Strings[0] else rlang:='USA';
          end;
      *)

      if (uppercase(trim(ir.languages.text)) <> 'ENGLISH') then
        rlang := ir.languages.Strings[0]
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
      rlsdatesite := slUrlGet('http://www.imdb.com/title/' + imdb_id + '/releaseinfo', '');

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




        (* EXAMPLES BELOW ARE COLLECTED WITH parse_boxofficemojo_exact True *)

        // fails on TV stuff:
        //Searching on Box Office Mojo with Legion for Legion.S01D01.MULTi.COMPLETE.BLURAY-iFPD - Found more than one movie - Screens via BOM: 2476 with 2476
        //The Girlfriend Experience for The.Girlfriend.Experience.S02E10.MULTi.1080p.HDTV.x264-SH0W - Only one movie matched - Screens via BOM: 0 with
        //The Crown for The.Crown.S02E03.German.Dubbed.DL.720p.WEBRip.x264-BiGiNT - Found more than one movie - Screens via BOM: 3 with 3

        // wrong info for: Wind for Wind.1992.720p.BluRay.x264-PSYCHD - Found more than one movie - Screens via BOM: 1 with 1
        //King Kong for King.Kong.2005.EXTENDED.2160p.UHD.BluRay.x265-DEPTH - Found more than one movie - Screens via BOM: 5 with 5


        // missing info when parsed
        //Gook for Gook.2017.LIMITED.1080p.BluRay.x264-WEST - Only one movie matched - Screens via BOM: 0 with
        //California Typewriter for California.Typewriter.2016.720p.BluRay.x264-BRMP - Only one movie matched - Screens via BOM: 0 with
        //Mother! for Mother.German.DL.AC3.Dubbed.1080p.WEB.h264-PsO - Only one movie matched - Screens via BOM: 0 with
        //Men of Honor for Men.Of.Honor.2000.1080p.iNTERNAL.BluRay.x264-MOOVEE - Only one movie matched - Screens via BOM: 0 with
        //The Trip to Spain for The.Trip.to.Spain.2017.LIMITED.720p.BluRay.x264-DRONES - Only one movie matched - Screens via BOM: 0 with

        // it's there but not found
        //Victoria &amp; Abdul for Victoria.and.Abdul.2017.FRENCH.BDRip.x264-VENUE - Found more than one movie

        // The Walking Dead for The.Walking.Dead.S08E07.Fuer.Danach.und.Rick.GERMAN.DUBBED.DL.1080p.WebHD.x264-TVP -- Found more than one movie -- Screens via BOM: 1 with 1

        //wrong: The Opus for The.Opus.Der.Film.German.2008.WS.DOKU.DVDRiP.x264-CiA - Found more than one movie - Screens via BOM: 1 with 1

        //mvoie where it seems to be correct & which has no business info on imdb yet:
        //Singularity for Singularity.2017.BDRip.X264-AMIABLE - Found more than one movie - Screens via BOM: 10 with 10

        // maybe use orignal title instead of title shown @ imdb:
        // Crazies for The.Crazies.1973.REMASTERED.1080p.BluRay.X264-AMIABLE - original title: The Crazies

        (* EXAMPLES BELOW ARE COLLECTED WITH parse_boxofficemojo_exact False *)

        //Searching on Box Office Mojo with Warrior for Warrior.2011.COMPLETE.UHD.BLURAY-SUPERSIZE - Found more than one movie - Screens via BOM: 53 with 53

        // uses country specific movie title
        //Searching on Box Office Mojo with Die Tribute von Panem - Catching Fire for The.Hunger.Games.Catching.Fire.2013.2160p.UHD.BluRay.X265-IAMABLE - Found more than one movie
        //Planet der Affen: Survival for Planet.der.Affen.Survival.GERMAN.2017.DL.PAL.DVDR-Pumuck - Found more than one movie

        // didn't detect the Theaters stuff from webpage
        //Dunkirk for Dunkirk.2017.DVDR-JRKDVD - Only one movie matched - Screens via BOM: 0 with
        //Bigger Stronger Faster* for Bigger.Stronger.Faster.2008.LIMITED.DOCU.1080p.BluRay.x264-VETO - Only one movie matched - Screens via BOM: 0 with
        //Brad's Status for Brads.Status.2017.LIMITED.BDRip.x264-GECKOS - Only one movie matched - Screens via BOM: 0 with
        //Vincent N Roxxy for Vincent.N.Roxxy.2016.FRENCH.720p.BluRay.x264-LOST - Only one movie matched - Screens via BOM: 0 with      // case needed when website shows n/a

        // detects the wrong movie with wrong year
        //Heartbeats for Heartbeats.2017.1080p.BluRay.x264-CURSE - Found more than one movie - Screens via BOM: 1 with 1
        //Attraction for Attraction.2017.MULTi.1080p.BluRay.x264-LOST - Found more than one movie - Screens via BOM: 4 with 4

        //detects wrong movie
        //The LEGO Movie for The.LEGO.Movie.2014.COMPLETE.UHD.BLURAY-COASTER - Found more than one movie - Screens via BOM: 4047 with 4047


        if fBOMSearchNeeded then
        begin
          bomsite := slUrlGet('http://www.boxofficemojo.com/search/', 'q=' + imdb_mtitle);

          Debug(dpError, section, Format('Searching on Box Office Mojo with %s for %s', [imdb_mtitle, rls]));

          if not AnsiContainsText(bomsite, 'No Movies or People found.') then
          begin

            // BOM uses the same picture as IMDB does, so we can use it do identify the correct movie
            {
            * Examples:
            * IMDB: <meta property='og:image' content="https://images-na.ssl-images-amazon.com/images/M/MV5BMTkxMjgwMDM4Ml5BMl5BanBnXkFtZTgwMTk3NTIwNDE@._V1_UY1200_CR90,0,630,1200_AL_.jpg" />
            * BOM:  <img src="https://images-na.ssl-images-amazon.com/images/M/MV5BMTkxMjgwMDM4Ml5BMl5BanBnXkFtZTgwMTk3NTIwNDE@._V1_UY119_CR0,0,80,119_AL.jpg" border="1"
            }
            rr.Expression := '<meta property=\''og:image\'' content=\"https:\/\/.*?images\/\w\/([\w@?]+)';
            if rr.Exec(mainsite) then
            begin
              fPictureID := rr.Match[1];
            end;

            if fPictureID <> '' then
            begin
              Debug(dpError, section, Format('Found movie by Picture Link: %s', [fPictureID]));
              rr2.Expression := '<a href=\".*?\"https:\/\/.*?(images)\/\w\/' + fPictureID + '.*?td>\s*[^\n]*<td\s*[^\n]*\s*[^\n]*(.*?)\s*[^\n]*\">(.*?)<\/font>';
            end
            else if AnsiContainsText(bomsite, '1 Movie Matches:') then
            begin
              Debug(dpError, section, 'Only one movie matched');
              rr2.Expression := '<td>\s*[^\n]*<b><font[^<>]*><a href="(\/movies\/[^<>]*)">[^<>]*<\/a><\/font><\/b><\/td>\s*(<td[^<>]*>[^\n]+\s*)+>([0-9,]+)<\/font><\/td>\s*<td[^<>]*><font\s+';
            end
            else
            begin
              Debug(dpError, section, 'Found more than one movie');

              bom_date := '[^<>]+' + IntToStr(imdb_year);

              if imdb_date <> '' then
              begin
                Debug(dpError, section, Format('imdb_date: %s', [imdb_date]));

                // [EXCEPTION] TSiteSlot.Execute(if todotask.Execute(self) then) HTTPImdb Class.1983.PAL.FULL.MULTi.DVDR-VFC : tt0085346: "22 July 1983" is not a valid date format
                {$IFDEF MSWINDOWS}
                  GetLocaleFormatSettings(1033, formatSettings);
                {$ELSE}
                  formatSettings := DefaultFormatSettings;
                {$ENDIF}
                // some imdb_date data could be like "January 1984" - http://www.imdb.com/title/tt0085346/releaseinfo?ref_=tt_dt_dt
                // so we need to handle it different
                formatSettings.ShortDateFormat := 'd mmmm yyyy';
                release_date := StrToDate(imdb_date, formatSettings);
                formatSettings.ShortDateFormat := 'mm/dd/yyyy'; // dd & mm could lead to problems, as date is shown as "7/22/1983" or "6/2/1989"
                imdb_date := DateToStr(release_date, formatSettings);

                Debug(dpError, section, Format('exact date: %s -- non exact date: %s|%s', [bom_date, bom_date, imdb_date]));

                if config.ReadBool(section, 'parse_boxofficemojo_exact', False) then
                  bom_date := imdb_date
                else
                  bom_date := bom_date + '|' + imdb_date;
              end;

              rr2.Expression := '<td>\s*[^\n]*<b><font[^<>]*><a href="(\/movies\/[^<>]*)">[^<>]*<\/a><\/font><\/b><\/td>\s*(<td[^<>]*>[^\n]+\s*)+>([0-9,]+)<\/font><\/td>\s*<td[^<>]*><font[^<>]*|<a href="\/schedule[^\"]+">(' + bom_date + ')<\/a>';
            end;

            if rr2.Exec(bomsite) then
            begin
              s := Csere(rr2.Match[3], ',', '');

              if StrToIntDef(s, 0) > imdb_screens then
                imdb_screens := StrToIntDef(s, 0);

              Debug(dpError, section, Format('Screens via BOM: %s with %s', [IntToStr(imdb_screens), s]));
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

function TPazoHTTPImdbTask.Name: AnsiString;
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

