unit taskthetvdblookup;

interface

uses Classes, pazo, tasksunit, taskrace, xmlwrapper, dbthetvdb;

//const apkikey:string = 'FFFFFFFFFFFFFFFF'; //   just a 64bit (16*4) hex string and you are vailed, no RESTful api :P

type
  TPazoTheTVDbLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    initial_site: string;
  public
    constructor Create(const netname, channel: string; site: string;
      pazo: TPazo; attempt: integer = 0);
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
    procedure PostResults(id, network, country, classi, status: string;
      genre: TStringList; premyear: string);
  end;

  (*for !addtvrage channels*)

  TPazoHTTPTheTVDbTask = class(TTask)
  private
    rls: string;
    tv_showid: string;
  public
    constructor Create(const tv_showid: string; rls: string = '');
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
  end;

function parseTVMazeInfos(jsonStr: string; Showname: string = ''): TTheTvDB;
function findTheTVDbIDByName(name: string): string;

implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  sltcp, slhttp, RegExpr, irc, mrdohutils, uLkJSON;

const
  section = 'tasktvdb';

function findTheTVDbIDByName(name: string): string;
var
  s, sname, url, response: string;
  js: TlkJSONobject;
begin
  result := 'FAILED';
  s := Csere(name, '.and.', '.&.');
  s := Csere(s, '.at.', '.@.');
  s := Csere(s, '.and.', '_&_');
  s := Csere(s, '.at.', '_@_');
  s := Csere(s, '', chr(39));
  sname := Csere(s, ' ', '+');
  sname := Csere(sname, '.', '+');
  url := 'http://api.tvmaze.com/singlesearch/shows?q=' + sname;
  try
    response := slUrlGet(url);
  except on E: Exception do
    begin
      Debug(dpError, section,
        Format('Exception in findTheTVDbIDByName.httpGET: %s',
        [e.Message]));
      irc_Adderror(Format('Exception in findTheTVDbIDByName.httpGET: %s',
        [e.Message]));
      Exit;
    end;
  end;

  js := TlkJSON.ParseText(response) as TlkJSONobject;
  try
    result := string(js.Field['externals'].Field['thetvdb'].Value);
  finally
    js.Free;
  end;
end;

function parseTVMazeInfos(jsonStr: string; Showname: string = ''): TTheTvDB;
var
  tvr: TTheTvDB;
  i: integer;
  s: string;
  js: TlkJSONobject;
  x: TStringlist;
begin
  result := nil;
  if Showname <> '' then
    s := Csere(Showname, '.', ' ')
  else
    s := '';
  tvr := TTheTvDB.Create(s);
  try
    try
      js := TlkJSON.ParseText(jsonStr) as TlkJSONobject;
    except
      on E: Exception do
        irc_Adderror(format(
          '<c4>[Exception]</c> in parseTVInfos.JSON.ParseText: %s',
          [E.Message]));
    end;

    tvr.tv_showid := string(js.Field['externals'].Field['thetvdb'].Value);

    tvr.tv_showname := string(js.Field['name'].Value);
    if js.Field['network'].SelfType = jsNull then
    begin
      tvr.tv_network := string(js.Field['webChannel'].Field['name'].Value);
      tvr.tv_country :=
        string(js.Field['webChannel'].Field['country'].Field['code'].Value);
    end
    else
    begin
      tvr.tv_network := string(js.Field['network'].Field['name'].Value);
      tvr.tv_country :=
        string(js.Field['network'].Field['country'].Field['code'].Value);

      tvr.tv_status := string(js.Field['status'].Value);

      for I := 0 to js.Field['genres'].Count - 1 do
        tvr.tv_genres.Add(string(js.Field['genres'].Child[i].Value));

      tvr.tv_classification := string(js.Field['type'].Value);

      x := TStringlist.Create;
      try
        x.Delimiter:= '-';
        x.DelimitedText := string(js.Field['premiered'].Value);
        tvr.tv_premiered_year := StrToIntDef(x.Strings[0], -1);
      finally
        x.free;
      end;

      tvr.tv_running := Boolean(tvr.tv_status = 'Running');
      tvr.tv_scripted := Boolean(tvr.tv_classification = 'Scripted');

    end;

  finally
    js.free;
  end;
result:=tvr;
end;

{ TPazoTheTVDbLookupTask }

constructor TPazoTheTVDbLookupTask.Create(const netname, channel: string;
  site: string; pazo: TPazo; attempt: integer = 0);
begin
  self.attempt := attempt;
  self.initial_site := site;
  inherited Create(netname, channel, site, '', pazo);
end;

procedure TPazoTheTVDbLookupTask.PostResults(id, network, country, classi,
  status: string; genre: TStringList; premyear: string);
begin
  irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://thetvdb.com/?tab=series&id=%s', [mainpazo.rls.rlsname, premyear, id]));
  irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [genre.CommaText, classi, status]));
  irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s', [country, network]));
end;

function TPazoTheTVDbLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTvRelease;
  r: TPazoTheTVDbLookupTask;
  tvmaz, tvdb, sid, response, uurl: string;
  db_thetvdb: TTheTvDB;
  ps: TPazoSite;
  //  xml: TSLXMLDocument;
  //  st: TStream;

begin
  tr := TTvRelease(mainpazo.rls);

  // Show is in DataBase? Here we could add some Update routine and CurrentAired EP.
  try
    db_thetvdb := getTheTVDBbyShowName(tr.showname);
    if (db_thetvdb <> nil) then
    begin
      db_thetvdb.SetTVRageRelease(tr);
      ready := True;
      Result := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      //   db_tvrage := nil;
      Debug(dpError, section,
        Format('Exception in getTheTVDBbyShowName: %s',
        [e.Message])); // anpassen.
      ready := True;
      Result := True;
      exit;
    end;
  end;

  //Show is not found in the DB.
  sid := findTheTVDbIDByName(tr.showname);

  if sid = 'FAILED' then
  begin

    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying tv rage lookup for %s later',
        [tr.showname]);
      r := TPazoTheTVDbLookupTask.Create(netname, channel, initial_site,
        mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval',
        60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section,
            Format('[Exception] in TPazoTheTVDbLookupTask Search %s',
            [e.Message]));
          irc_Adderror(Format(
            '<c4>[Exception]</c> in TPazoTheTVDbLookupTask Search %s',
            [e.Message]));
          readyerror := True;
          Result := True;
          //          x.Free;
          exit;
        end;
      end;

    end
    else
    begin
      debug(dpSpam, section, 'READD: no more attempts...');
    end;
    irc_addadmin('<c4><b>ERROR</c> No TheTVDb ID found for %s</b>',
      [tr.showname]);

    ready := True;
    Result := True;
    exit;
  end;

  try
    uurl := 'http://api.tvmaze.com/lookup/shows?thetvdb=' + sid;
    tvmaz := slUrlGet(uurl);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format(
        '[EXCEPTION] TPazoTheTVDbLookupTask.execute httpGET(TVMaze): Exception : %s',
        [e.Message]));
      irc_Adderror(Format(
        '<c4>[EXCEPTION]</c> TPazoTheTVDbLookupTask.execute httpGET(TVMaze): Exception : %s',
        [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;
  (*
    try
      uurl := 'thetvdb.com/api/' + RandomHEXString + '/series/' + sid + '/';
      tvdb := slUrlGet(uurl);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format(
          '[EXCEPTION] TPazoTheTVDbLookupTask.execute httpGET(TheTVDB): Exception : %s',
          [e.Message]));
        irc_Adderror(Format(
          '<c4>[EXCEPTION]</c> TPazoTheTVDbLookupTask.execute httpGET(TheTVDB): Exception : %s',
          [e.Message]));
        Result := True;
        ready := True;
        exit;
      end;
    end;
   *)
    //maybe adding some if respons = '' ...

  db_thetvdb := parseTVMazeInfos(tvmaz, tr.showname);

  if config.ReadBool(section, 'post_lookup_infos', False) then
  begin

    PostResults(db_thetvdb.tv_showid, db_thetvdb.tv_network,
      db_thetvdb.tv_country,
      db_thetvdb.tv_classification, db_thetvdb.tv_status, db_thetvdb.tv_genres,
      IntToStr(db_thetvdb.tv_premiered_year));
    //  PostResults(cur_id, cur_netw, cur_country, cur_cassi, cur_status,tr.genres, IntToStr(cur_premyear));
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
      Debug(dpError, section,
        Format('Exception in TPazoTheTVDbLookupTask kb_add: %s',
        [e.Message]));
    end;
  end;

  ready := True;
  Result := True;
end;

function TPazoTheTVDbLookupTask.Name: string;
begin
  try
    Result := format('TTVRelease PazoID(%d) %s @ %s Count(%d)',
      [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'TTVRelease';
  end;
end;

{ TPazoHTTPTheTVDbTask }

constructor TPazoHTTPTheTVDbTask.Create(const tv_showid: string; rls: string =
  '');
begin
  self.tv_showid := tv_showid;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename',
    'SLFTP'));
end;

function TPazoHTTPTheTVDbTask.Name: string;
begin
  try
    Result := Format('httpTheTVDb : %s', [tv_showid]);
  except
    Result := 'httpTheTVDb';
  end;
end;

destructor TPazoHTTPTheTVDbTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPTheTVDbTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTheTvDB;
  rx: TRegExpr;
  uurl, sname: string;

begin

  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;

    rx.Expression :=
      '(.*)[\._-](\d{4}\.\d{2}\.\d{2}|\d{2}\.\d{2}\.\d{4})[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[3], 0);
    end;

    rx.Expression :=
      '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?)?[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[5], 0);
    end;

    rx.Expression := '[\.\_]';
    sname := rx.Replace(sname, ' ');
  finally
    rx.Free;
  end;

  uurl := 'thetvdb=' + tv_showid;
  response := slUrlGet('http://api.tvmaze.com/lookup/shows', uurl);

  if response <> '' then
  begin

    tvdb := parseTVMazeInfos(response, sname);
    if tvdb <> nil then
      saveTheTVDbInfos(tv_showid, tvdb, rls);
  end;
end;

end.

