unit tasktvragelookup;

interface

uses Classes, pazo, tasksunit, taskrace;

type
  TPazoTvRageLookupTask = class(TPazoPlainTask)
  private
    attempt:      integer;
    initial_site: string;
  public
    constructor Create(const netname, channel: string; site: string;
      pazo: TPazo; attempt: integer);
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
    procedure PostResults(id, network, country, classi, status: string;
      genre: TStringList; premyear: string);
  end;

implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  sltcp, slhttp, RegExpr, irc, dbaddtvrage;

const
  section = 'tasktvrage';


{ TPazoTvRageLookup }

constructor TPazoTvRageLookupTask.Create(const netname, channel: string;
  site: string; pazo: TPazo; attempt: integer);
begin
  self.attempt      := attempt;
  self.initial_site := site;
  inherited Create(netname, channel, site, '', pazo);
end;

function csakangolabc(s: string): string;
var
  i: integer;
begin
  s      := LowerCase(s);
  Result := '';
  for i := 1 to length(s) do
    if ((s[i] >= 'a') and (s[i] <= 'z')) then
    begin
      Result := Result + s[i];
    end;
end;

function TPazoTvRageLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTvRelease;
  r:  TPazoTvRageLookupTask;
  cur_endyear, cur_premyear, I: integer;

  ss_show, cur_id, cur_name, cur_country, cur_status, cur_cassi, cur_netw, cur_url: string;
  cur_runnt: string;
  response, ssec, uurl: string;
  alle: boolean;
  xs: TStringList;
  x:  TRegExpr;
  cur_running: boolean;
  db_tvrage: TDbTVRage;
  ps: TPazoSite;
begin
  tr := TTvRelease(mainpazo.rls);

//  db_tvrage := nil;
  try
    db_tvrage := dbaddtvrage_gettvrage_show(tr.showname);
    if (db_tvrage <> nil) then
    begin
      db_tvrage.SetTVRageRelease(tr);
      ready  := True;
      Result := True;
      exit;
    end;
  except
    on e: Exception do
    begin
   //   db_tvrage := nil;
      Debug(dpError, section, Format('Exception in dbaddtvrage_gettvrage_show: %s',
        [e.Message]));

      ready  := True;
      Result := True;
      exit;
    end;
  end;

  ss_show := tr.showname;

  x := TRegexpr.Create;
  x.ModifierI := True;
  x.ModifierM := True;


  x.Expression := '[\s\.]';
  ss_show      := x.Replace(tr.showname, '+');


  uurl := 'show=' + ss_show;
  alle := config.ReadBool('tasktvrage', 'all_exact', False);
  ssec := config.readstring('tasktvrage', 'exact_sections', 'NONO');
  if not alle then
  begin
    xs := TStringList.Create;
    xs.Delimiter := chr(44);
    xs.DelimitedText := ssec;

    for I := 0 to xs.Count - 1 do
    begin
      if tr.section = xs.Strings[i] then
      begin
        uurl := 'show=' + ss_show + '&exact=1';
        break;
      end;
    end;
    xs.Free;
  end
  else
    uurl := 'show=' + tr.showname + '&exact=1';

  try
    response := slUrlGet('http://services.tvrage.com/tools/quickinfo.php', uurl);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format(
        '[EXCEPTION] TPazoTvRageLookupTask slUrlGet: Exception : %s', [e.Message]));
      irc_Adderror(Format(
        '<c4>[EXCEPTION]</c> TPazoTvRageLookupTask slUrlGet: Exception : %s', [e.Message]));
      Result := True;
      ready  := True;
      x.Free;
      exit;
    end;
  end;

  if response = '' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying tv rage lookup for %s later', [tr.showname]);

      r := TPazoTvRageLookupTask.Create(netname, channel, initial_site,
        mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section,
            Format('[Exception] in TPazoTvRageLookupTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoTvRageLookupTask AddTask %s',
            [e.Message]));
          readyerror := True;
          result:=True;
          x.Free;
          exit;
        end;
      end;
    end
    else
    begin
      debug(dpSpam, section, 'READD: no more attempts...');
    end;

    ready  := True;
    Result := True;
    exit;
    x.Free;
  end;

  debug(dpSpam, section, 'TVRage results for %s' + #13#10 + '%s', [tr.showname, response]);


  //  x:=TRegexpr.Create;
  //  x.ModifierI:=True;
  //  x.ModifierM:=True;


  {###Read  ShowID  ###}
  x.Expression := 'Show ID\@(\d+)$';
  if x.Exec(response) then
    cur_id := x.Match[1];

  {###Read  ShowName  ###}
  x.Expression := '^Show Name\@(.*?)$';
  if x.Exec(response) then
    cur_name := x.Match[1];

  {###Read  ShowUrl  ###}
  x.Expression := '^Show URL\@(.*?)$';
  if x.Exec(response) then
    cur_url := x.Match[1];

  {###Read  ShowPremiered  ###}
  x.Expression := '^Premiered\@(\d{4})$';
  if x.Exec(response) then
    cur_premyear := StrToInt(x.Match[1]) else cur_premyear :=  -1; 

  {###Read  ShowEnded  ###}
  x.Expression := '^Ended\@\w+\/(\d{4})$';
  cur_running  := True;
  cur_endyear  := -1;
  if x.Exec(response) then
  begin
    cur_running := False;
    cur_endyear := strtointdef(x.Match[1], -1);
  end;

  {###Read  ShowCountry  ###}
  x.Expression := '^Country\@(.*?)$';
  if x.Exec(response) then
    cur_country := x.Match[1];

  {###Read  ShowStatusAsString  ###}
  x.Expression := '^Status\@(.*?)$';
  if x.Exec(response) then
    cur_status := x.Match[1];

    if ((cur_status = 'Ended') or (cur_status = 'Canceled/Ended')) then cur_running := False;

  {###Read  ShowClassification  ###}
  x.Expression := '^Classification\@(.*?)$';
  if x.Exec(response) then
    cur_cassi := x.Match[1];
  if cur_cassi = 'Scripted' then
    tr.scripted := True;
  tr.classification := cur_cassi;
(*
  {###Read  ShowGenres  ###}
  x.Expression:='^Genres\@(.*?)$';
  if x.Exec(response) then
    tr.genres.DelimitedText:=x.Match[1];

  {###Read  ShowGenres  ###}
  x.Expression:='^Genres\@(.*?)$';
  if x.Exec(response) then
  splitString(x.Match[1],'|',tr.genres);
  *)

  {###Read  ShowGenres  ###}
  x.Expression := '^Genres\@(.*?)$';
  if x.Exec(response) then
    tr.genres.DelimitedText := Csere(x.Match[1], '|', ',');

  {###Read  ShowNetwork  ###}
  x.Expression := '^Network\@(.*?)$';
  if x.Exec(response) then
    cur_netw := x.Match[1];


  {###Read  ShowRuntime  ###}
  x.Expression := '^Runtime\@(.*?)$';
  if x.Exec(response) then
    cur_runnt := x.Match[1];

  x.Free;

  (* here is an exp. from the new results.....
    Show ID@6454
    Show Name@Two and a Half Men
    Show URL@http://www.tvrage.com/Two_and_a_Half_Men
    Premiered@2003
    Started@Sep/22/2003
    Ended@
    Latest Episode@07x14^Crude and Uncalled For^Feb/01/2010
    Next Episode@07x15^Aye, Aye, Captain^Feb/08/2010
    RFC3339@2010-02-08T21:00:00-5:00
    GMT+0 NODST@1265677200
    Country@USA
    Status@Returning Series
    Classification@Scripted
    Genres@Comedy
    Network@CBS
    Airtime@Monday at 09:00 pm
    Runtime@30
  *)

  (* Loop deprecated in favor of a regexp solution
    while(true) do
    begin
      s:= Elsosor(response);
      if s = '' then break;
      ss:= Fetch(s, '@');
      if AnsiSameText(ss, 'Show Name') then begin
        trshowname:= s;
        break;
      end;
    end;
  *)

  if csakangolabc(cur_name) = csakangolabc(tr.showname) then    //only English alphabet
  begin
    db_tvrage := TDbTVRage.Create(tr.showname);
    db_tvrage.rls_showname := tr.showname;

    tr.showname := cur_name;
    tr.showid   := cur_id;
    tr.premier_year := cur_premyear;
    tr.country  := cur_country;
    tr.classification := cur_cassi;
    if cur_cassi = 'Scripted' then
      tr.scripted := True;
    tr.status := cur_status;
    tr.network    := cur_netw;
    tr.running    := cur_running;
    tr.ended_year := cur_endyear;

    db_tvrage.tv_showid    := tr.showid;
    db_tvrage.tv_showname  := cur_name;
    db_tvrage.tv_showurl   := cur_url;
    db_tvrage.tv_premiered_year := tr.premier_year;
    db_tvrage.tv_country   := tr.country;
    db_tvrage.tv_status    := tr.status;
    db_tvrage.tv_classification := tr.classification;
    db_tvrage.tv_genres    := tr.genres;
    db_tvrage.tv_network   := tr.network;
    db_tvrage.tv_runtime   := StrToIntDef(cur_runnt, 0);
    db_tvrage.tv_endedyear := cur_endyear;
    db_tvrage.tv_running   := tr.running;

    try
      dbaddtvrage_SaveTVRage(db_tvrage.tv_showid, db_tvrage, mainpazo.rls.rlsname);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddtvrage_SaveTVRage: %s',
          [e.Message]));
          result:=True;
          readyerror := True;
        exit;
      end;
    end;

  end
  else
  begin
    irc_addadmin('<c4><b>ERROR</c> english alphabet check failed! %s <> %s   </b>',
      [cur_name, tr.showname]);
    if config.ReadBool(section, 'stop_on_englishcheck', True) then
    begin
      Result := True;
      ready  := True;
      exit;
    end;
  end;


  if config.ReadBool(section, 'post_lookup_infos', False) then
  begin
    PostResults(cur_id, cur_netw, cur_country, cur_cassi, cur_status,
      tr.genres, IntToStr(cur_premyear));
  end;

  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE',
      mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTvRageLookupTask kb_add: %s',
        [e.Message]));
    end;
  end;

  ready  := True;
  Result := True;

end;

function TPazoTvRageLookupTask.Name: string;
begin
  try
    Result := format('PTVRAGE PazoID(%d) %s @ %s Count(%d)',
      [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'PTVRAGE';
  end;
end;



procedure TPazoTvRageLookupTask.PostResults(id, network, country, classi, status: string;
  genre: TStringList; premyear: string);
begin
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/',
    [mainpazo.rls.rlsname, premyear, id]));
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s',
    [genre.CommaText, classi, status]));
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ',
    [country, network]));
end;



end.

