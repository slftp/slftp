unit taskhttpimdb;

interface

uses Classes, pazo, tasksunit, sltcp;

type
  TPazoHTTPImdbTask = class(TTask)
  private
    rls: AnsiString;
    imdb_id: AnsiString;
  public
    constructor Create(const imdb_id: AnsiString; rls: AnsiString);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: AnsiString; override;
  end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
  configunit, dirlist, mystrings, sitesunit, console, slhttp, regexpr,
  dbaddimdb, tasksitenfo, Contnrs;

const
  section = 'taskhttpimdb';

  { TPazoHTTPImdbTask }

constructor TPazoHTTPImdbTask.Create(const imdb_id: AnsiString; rls: AnsiString);
begin
  self.imdb_id := imdb_id;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename',
    'SLFTP'));
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

  mainsite, rlsdatesite, businesssite: AnsiString;
  release_date: TDateTime;
  formatSettings: TFormatSettings;
begin
  Result:=False;

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
  rr.Expression :=
    '<title>(\&\#x22;|\")?(.*?)\1?\s*\((TV\s*Series|TV\s*mini-series|TV|TV\s*Movie|Video|Video Game)?\s*(\d{4})((\-|&ndash;|–|&emdash;)(\d{4})?\s*(&nbsp;)?)?(\/.+)?\)( - IMDb)?<\/title>';
  if rr.Exec(mainsite) then
  begin
    imdb_year := StrToInt(rr.Match[4]);
    imdb_mtitle := rr.Match[2];
    imdb_extra := rr.Match[3];
  end;

  (*
  if imdb_year = 0 then begin
      rr.free;
      exit;
  end;
  *)

  imdbdata := TDbImdbData.Create(imdb_id);
  rr2 := TRegexpr.Create;
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

  (* Get STV Info through releaseinfo page from iMDB *)
  rlsdatesite := slUrlGet('http://www.imdb.com/title/' + imdb_id + '/releaseinfo', '');

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
    //if ((imdb_extra = 'TV') or (imdb_extra = 'TV Movie') or (imdb_extra = 'Video Game') or (imdb_extra = 'Video')) then // <- need to at every single match which could be parsed
    if ( AnsiContainsText(imdb_extra, 'TV') or AnsiContainsText(imdb_extra, 'Video') ) then
    begin
      imdbdata.imdb_stvm := True;
      imdb_stv := True;
      imdbdata.imdb_stvs := 'Videogame_TV_Video';
    end;
  end;

  imdb_date := '';

  if not imdb_stv then
  begin
    rr.ModifierI := True;

    //New regex since rev.327 03.10.2013
    rr.Expression :=
      '<tr class="(odd|even)">[\s\n]*?<td><a href=\"\/calendar\/\?region\=' + imdb_region
      + '\&ref\_\=ttrel\_rel\_\d+"\s*>' + imdb_country +
      '<\/a><\/td>[\s\n]*?<td class="release_date">\s*([\w\s\d]+)\s*<a href="\/year\/(\d{4})\/\?ref\_=ttrel\_rel\_\d+"\s*>\d{4}<\/a><\/td>[\s\n]*?<td><\/td>[\s\n]*?<\/tr>';

    if rr.Exec(rlsdatesite) then
    begin
      imdb_date := Format('%s %s', [rr.Match[2], rr.Match[3]]);
      imdbdata.imdb_stvs := 'Cinedate: ' + imdb_date;
      imdbdata.imdb_stvm := False;
      imdb_stv := False;
      (* Fetching Cinedate for imdb_country *)
      imdbdata.imdb_cineyear := Strtointdef(rr.Match[3], -1);
    end
    else
    begin
      imdbdata.imdb_stvs := 'No infos around for ' + imdb_country + ' so it is STV?!';
      imdbdata.imdb_stvm := True;
      imdb_stv := True;
    end;
  end;
  //New regex since rev.327 03.10.2013
  rr.Expression :=
    '<tr class="(odd|even)">[\s\n]*?<td><a href=\"\/calendar\/\?region\=' + imdb_region
    + '\&ref\_\=ttrel\_rel\_\d+"\s*>' + imdb_country +
    '<\/a><\/td>[\s\n]*?<td class="release_date">\s*([\d\s\w]+)\s*<a href="\/year\/(\d{4})\/\?ref\_=ttrel\_rel\_\d+"\s*>\d{4}<\/a><\/td>[\s\n]*?<td>(.*?)<\/td>[\s\n]*?<\/tr>';
  if rr.Exec(rlsdatesite) then
  begin
    s := rr.Match[4];
    if s <> '' then
    begin
      rr2.ModifierI := True;
      rr2.Expression := '(DVD|video|TV)(\s|\.|\-)?premiere'; // 'TV' taken from Optimus Autotrader
      if rr2.Exec(s) then
      begin
        imdbdata.imdb_stvs := Format('%s (%s %s)', [rr2.Match[0], rr.Match[2], rr.Match[3]]);
        imdb_stv := True;
      end;
      (*  Fetching Festival infos for imdb_country  *)
      rr2.Expression := 'F(estival|ilmfest|est|ilm(\s|\.|\-)?Market)'; // 'Film Market' taken from Optimus Autotrader
      if rr2.Exec(s) then
        imdbdata.imdb_festival := True;
    end;

  end;

  s := '0';
  imdb_screens := 0;
  (*  Get BOX/Business Infos  *)
  if config.ReadBool(section, 'parse_boxofficemojo', False) then
  begin
    businesssite := slUrlGet('http://www.boxofficemojo.com/search/', 'q=' + imdb_mtitle);

    rr.Expression := '<br><b>1 Movie Matches:\s*</b>';
    if rr.Exec(businesssite) then
    begin
      rr2.Expression := '<td>\s*[^\n]*<b><font[^<>]*><a href="(/movies/[^<>]*)">[^<>]*</a></font></b></td>';
    else
    begin
      bom_date := '[^<>]+' + imdb_year;
      if imdb_date <> '' then
      begin
        {$IFDEF MSWINDOWS}
          GetLocaleFormatSettings(1033, formatSettings);
        {$ELSE}
          formatSettings := DefaultFormatSettings;
        {$ENDIF}
        formatSettings.ShortDateFormat := 'd mmmm yyyy';
        release_date := StrToDate(imdb_date, formatSettings);
        formatSettings.ShortDateFormat := 'mm/dd/yyyy';
        imdb_date := DateToStr(release_date, formatSettings);

        if config.ReadBool(section, 'parse_boxofficemojo_exact', False) then
          bom_date := imdb_date
        else
          bom_date := bom_date + '|' + imdb_date;
      end;

      rr2.Expression := '<td>\s*[^\n]*<b><font[^<>]*><a href="(/movies/[^<>]*)">[^<>]*</a></font></b></td>(\s*<td[^<>]*>[^\n]*</td>)+\s*<td[^<>]*><font[^<>]*><a href="\/schedule[^\"]+">(' + bom_date + ')</a>';
    end;

    if rr2.Exec(businesssite) then
    begin
      businesssite := slUrlGet('http://www.boxofficemojo.com' + rr2.Match[1], '');

      rr.Expression := '<td>Widest&nbsp;Release:<\/td>\s+<td>(<b>)?&nbsp;([0-9\,]+) theaters(</b>)?</td>';
      if rr.Exec(businesssite) then begin
        s := Csere(rr.Match[2], ',', '');
        if StrToIntDef(s, 0) > imdb_screens then
          imdb_screens := StrToIntDef(s, 0)
      end;
    end;

  end
  else
  begin
    businesssite := slUrlGet('http://www.imdb.com/title/' + imdb_id + '/business', '');

    rr.Expression := '\((USA|UK)\)[^\n]*?\(([\d\,\.]+)\s?Screens\)';

    if rr.Exec(businesssite) then
    begin
      repeat
        s := Csere(rr.Match[2], ',', '');
        s := Csere(s, '.', '');

        Debug(dpSpam, section,
          Format('TPazoHTTPImdbTask dbaddimdb_SaveImdb: match=%s',
            [rr.Match[0]]));

        if StrToIntDef(s, 0) > imdb_screens then
          imdb_screens := StrToIntDef(s, 0)
      until not rr.ExecNext;
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
        '<h\d[^>]>Box\s*Office<\/h\d>.*?<h\d class="inline">Gross\:<\/h\d>';
      if not rr.Exec(mainsite) then
      begin
        imdb_stv := True;
        imdbdata.imdb_stvs := 'No gross weight found, so its STV!';
        imdbdata.imdb_wide := False;
        imdbdata.imdb_ldt := False;
      end;

      //

    end
    else
    begin
      imdb_stv := False;
      imdbdata.imdb_stvs := 'USA with screens can''t be STV!';
    end;
  end;

  rr.free;
  rr2.free;
  imdbdata.imdb_id := imdb_id;
  imdbdata.imdb_year := imdb_year;
  imdbdata.imdb_stvm := imdb_stv;
  mainsite := '';
  rlsdatesite := '';
  businesssite := '';

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

