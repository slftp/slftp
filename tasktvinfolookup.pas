unit tasktvinfolookup;

interface

uses Classes, pazo, tasksunit, taskrace, xmlwrapper, dbtvinfo;

//const apkikey:string = 'FFFFFFFFFFFFFFFF'; //   just a 64bit (16*4) hex string and you are vailed, no RESTful api :P

type
  TPazoTVInfoLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    initial_site: string;
  public
    constructor Create(const netname, channel: string; site: string;
      pazo: TPazo; attempt: integer = 0);
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
    procedure PostResults(network, country, classi, status, premyear, url: string;
      genre: TStringList);
  end;

  (*didn't work like i wanted :/*)
  TPazoHTTPUpdateTVInfoTask = class(TPazoPlainTask)
  private
    showname: string;
    tvmaze_id: string;
  public
    constructor Create(const netname, channel: string; site: string; pazo: TPazo; attempt: integer = 0);
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
  end;

  (*for !addtvrage channels*)

  TPazoHTTPTVInfoTask = class(TTask)
  private
    rls: string;
    tvmaze_id: string;
  public
    constructor Create(const tvmaze_id: string; rls: string = '');
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
  end;

function parseTVMazeInfos(jsonStr: string; Showname: string = ''): TTVInfoDB;
function findTheTVDbIDByName(name: string): string;

function findTVMazeIDByName(name: string): string;

implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  sltcp, slhttp, RegExpr, irc, mrdohutils, uLkJSON;

const
  section = 'tasktvinfo';

function replaceTVShowChars(name: string; forWebFetch: boolean = false): string;
begin
  //  result := name;
  name := Csere(name, 'and', '&');
  name := Csere(name, 'at', '@');
  name := Csere(name, '.and.', '.&.');
  name := Csere(name, '.at.', '.@.');
  name := Csere(name, '.and.', '_&_');
  name := Csere(name, '.at.', '_@_');
  name := Csere(name, '', chr(39));
  if forWebFetch then
  begin
    result := Csere(name, ' ', '+');
    result := Csere(name, '.', '+');
  end;
  result := name;
end;

function findTVMazeIDByName(name: string): string;
var
  year, country, s, sname, url, response: string;
  js: TlkJSONobject;
  hadYear, hadCountry: boolean;
  r: TRegexpr;
  I: Integer;
begin
  result := 'FAILED';
  s := Csere(name, '.and.', '.&.');
  s := Csere(s, '.at.', '.@.');
  s := Csere(s, '.and.', '_&_');
  s := Csere(s, '.at.', '_@_');
  s := Csere(s, '', chr(39));
  s := Csere(s, ' ', '+');
  s := Csere(s, '.', '+');
  (*
      r := TRegexpr.Create;
    try
      r.Expression := '\d{4}$';
      if r.Exec(s) then
      begin
        hadYear = True;
        year := r.Match[0];
        s := r.Replace(s, '');
      end;
      r.Expression := '\+(\w+)$';
      hadCountry = True;
      country := r.Match[0];
      s := r.Replace(s, '');
    finally
      r.free;
    end;
  *)
    //  sname:=replaceTVShowChars(name,true);  we will test it somewhere else....
  url := 'http://api.tvmaze.com/singlesearch/shows?q=' + s;
  try
    response := slUrlGet(url);
  except on E: Exception do
    begin
      Debug(dpError, section,
        Format('Exception in findTVMazeIDByName.httpGET: %s',
        [e.Message]));
      irc_Adderror(Format('Exception in findTVMazeIDByName.httpGET: %s',
        [e.Message]));
      Exit;
    end;
  end;

  if ((response = '') or (response = '[]')) then
  begin
    //    Debug(dpError, section, 'Can not find '+ name + ' (' + sname + ') on TVMaze');
    //    irc_addadmin('Can not find '+ name + ' (' + sname + ') on TVMaze');
    // we already give an failed result no need to send this.
    Exit;
  end;

  js := TlkJSON.ParseText(response) as TlkJSONobject;

  try
    if js.Field['id'].SelfType <> jsNull then
      result := string(js.Field['id'].Value)
    else
    begin
      result := '-1';
      //      Debug(dpError, section, 'Can not find TVMaze id for ' + name + ' (' + sname + ')');
      //      irc_addadmin('Can not find TVMaze id for ' + name + ' (' + sname + ')');
      // we already give an failed result no need to send this.
    end;
  finally
    js.Free;
  end;
end;

function findTheTVDbIDByName(name: string): string;
var
  year, country, s, sname, url, response: string;
  js: TlkJSONobject;
  hadYear, hadCountry: boolean;
  r: TRegexpr;
begin
  hadYear := False;
  hadCountry := False;
  result := 'FAILED';
  s := Csere(name, '.and.', '.&.');
  s := Csere(s, '.at.', '.@.');
  s := Csere(s, '.and.', '_&_');
  s := Csere(s, '.at.', '_@_');
  s := Csere(s, '', chr(39));
  s := Csere(s, ' ', '+');
  s := Csere(s, '.', '+');
  (*
    try
      r := TRegexpr.Create;
      r.Expression:='\d{4}$';
      if r.Exec(s) then begin
      hadYear=True;
      year:=r.Match[0];
      s:=r.Replace(s,'');
      end;
      r.Expression:='\+(\w+)$';
      hadCountry=True;
      country:=r.Match[0];
      s:=r.Replace(s,'');
    finally
      r.free;
    end;
  *)
  url := 'http://api.tvmaze.com/singlesearch/shows?q=' + s;
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

  if ((response = '') or (response = '[]')) then
  begin
    Debug(dpError, section, 'Cant find theTVDB id for ' + name + ' (' + sname + ')');
    irc_addadmin('Cant find theTVDB id for ' + name + ' (' + sname + ')');
    Exit;
  end;

  js := TlkJSON.ParseText(response) as TlkJSONobject;
  try
    if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      result := string(js.Field['externals'].Field['thetvdb'].Value)
    else
    begin
      Debug(dpError, section, 'Cant find theTVDB id for ' + name + ' (' + sname + ')');
      irc_addadmin('<b><c14>Info</b></c>: Cant find theTVDB id for ' + name + ' (' + sname + ')');
    end;

  finally
    js.Free;
  end;
end;

function getGenreFromTheTVDb(id: string): string;
var
  s, url, response: string;
  xml: TSLXMLDocument;
  gn, nn, n: TSLXMLNode;
  x: TStringlist;
  rx: TRegexpr;
  ts: TStream;
begin
  xml := TSLXMLDocument.Create;
  rx := TRegexpr.create;
  x := TStringlist.Create;
  try
    x.QuoteChar := '"';
    rx.ModifierI := True;
    s := slUrlGet('http://thetvdb.com/api/FFFFFFFFFFFFFFFF/series/' + id + '/');
    ts := TStringStream.Create(s);
    try
      ts.Position := 0;
      xml.LoadFromStream(ts);
    finally
      ts.free;
    end;
    n := xml.GetDocumentElement;
    nn := xml.FindChildNode(n, 'Series');
    gn := xml.FindChildNode(nn, 'Genre');
    s := xml.GetNodeValue(gn);
    rx.Expression := '\|?(.*?)\|';

    if rx.Exec(s) then
      repeat
        x.Add(rx.Match[1]);
      until not rx.ExecNext();

    result := x.CommaText;
  finally
    x.free;
    rx.free;
    xml.free;
  end;
end;

procedure findCurrentAirDate(json: TlkJSONobject; out season, episdoe: Integer; out date: TDateTime);
var
  ep_nextnum, ep_prevnum: integer;
  se_nextnum, se_prevnum: integer;
  nextdt, prevdt: TDateTime;
  airt: string;

  hadPrev, hadNext: boolean;
begin

  ShortDateFormat := 'yyyy-mm-dd';
  ShortTimeFormat := 'hh:mm';
  DateSeparator := '-';
  TimeSeparator := ':';

  hadPrev := False;
  hadNext := False;

  try

    if ((json.Field['_embedded'] <> nil) and (json.Field['_embedded'].Field['previousepisode'] <> nil)) then
    begin
      ep_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['number'].Value), -1);
      se_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['season'].Value), -1);
      prevdt := UnixToDateTime(0);

      if string(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := string(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value);
      prevdt := StrToDateTime(string(json.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value) + ' ' + airt);

      hadPrev := True;
    end;
  except on E: Exception do
    begin
      Debug(dpError, section, '[Exception] in findCurrentAirDate.previousepisode: ' + E.Message);
      Irc_AddAdmin('[Exception] in findCurrentAirDate.previousepisode: ' + E.Message);
    end;
  end;

  try
    if ((json.Field['_embedded'] <> nil) and (json.Field['_embedded'].Field['nextepisode'] <> nil)) then
    begin
      ep_nextnum := StrToIntDef(string(json.Field['_embedded'].Field['nextepisode'].Field['number'].Value), -1);
      se_nextnum := StrToIntDef(string(json.Field['_embedded'].Field['nextepisode'].Field['season'].Value), -1);
      nextdt := UnixToDateTime(0);
      if string(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := string(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value);
      nextdt := StrToDateTime(string(json.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value) + ' ' + airt);
      hadNext := True;
    end;
  except on E: Exception do
    begin
      Debug(dpError, section, '[Exception] in findCurrentAirDate.nextepisode: ' + E.Message);
      Irc_AddAdmin('[Exception] in findCurrentAirDate.previousepisode: ' + E.Message);
    end;
  end;

  if ((not hadNext) and (not hadPrev)) then
  begin
    episdoe := -15;
    season := -15;
    date := DateTimeToUnix(0);
    Exit;
  end;

  if not hadNext then
  begin
    episdoe := ep_prevnum;
    season := se_prevnum;
    date := prevdt;
    exit;
  end;

  if IsSameDay(prevdt, nextdt) then
  begin
    episdoe := -5;
    season := -5;
    date := DateTimeToUnix(nextdt);
    Exit;
  end;

  if (DateTimeToUnix(nextdt)) <= DateTimeToUnix(now()) then
  begin
    // next date is smaller|equal to now()..
    //Irc_AddAdmin('next date is smaller|equal to now()..');
    episdoe := ep_nextnum;
    season := se_nextnum;
    date := DateTimeToUnix(nextdt);
    Exit;
  end;

  if (DateTimeToUnix(prevdt) + 86400) >= DateTimeToUnix(now()) then
  begin
    //previous date + 1Day is grater|equal to now()
    //Irc_AddAdmin('previous date + 1Day is grater|equal to now()');
    episdoe := ep_prevnum;
    season := se_prevnum;
    date := DateTimeToUnix(prevdt);
    Exit;
  end;

  if (DateTimeToUnix(nextdt)) > DateTimeToUnix(now()) then
  begin
    // nothing before matched and next_date is grater then now, so we took this.
    //Irc_AddAdmin('nothing before matched and next_date is grater then now, so we took next.');
    episdoe := ep_nextnum;
    season := se_nextnum;
    date := DateTimeToUnix(nextdt);
  end;

end;

function parseTVMazeInfos(jsonStr: string; Showname: string = ''): TTVInfoDB;
const
  genreList: string = 'Action, Adult, Adventure, Animals, Anime, Animation, Children, Comedy, Cooking, Crime, DIY, Documentary, Drama, Espionage, Family, ' +
    'Fantasy, Food, Game Show, History, Horror, Home and Garden, News, Medical, Mini-Series, Music, Mystery, Reality, Romance, Science-Fiction, ' +
    'Special Interest, Soap, Sport, Suspense, Talk Show, Thriller, Travel, War, Western';
var
  tvr: TTVInfoDB;
  i: integer;
  s: string;
  js: TlkJSONobject;
  slGen, gMaze, gTVDB, x: TStringlist;
  season, episdoe: Integer;
  date: TDateTime;
begin
  result := nil;
  js := nil;
  if Showname <> '' then
    s := Csere(Showname, '.', ' ')
  else
    s := '';
  tvr := TTVInfoDB.Create(s);
  tvr.tv_genres.Sorted := True;
  tvr.tv_genres.Duplicates := dupIgnore;
  slGen := TStringlist.Create;
  gTVDB := TStringlist.Create;
  slGen.CommaText := genreList;
  try
    try
      js := TlkJSON.ParseText(jsonStr) as TlkJSONobject;
    except
      on E: Exception do
      begin
        irc_Adderror(format('<c4>[Exception]</c> in parseTVInfos.JSON.ParseText: %s', [E.Message]));
        exit;
      end;
    end;

    if js = nil then
      Exit;

    tvr.tv_showname := string(js.Field['name'].Value);

    if LowerCase(tvr.tv_showname) = 'not found' then
    begin
      irc_addAdmin('<c14><b>Info</c></b>: TVMaze returned 404 - Not Found.');
      Exit;
    end;

    tvr.tvmaze_id := string(js.Field['id'].Value);
    tvr.tv_url := string(js.Field['url'].Value);

    if js.Field['status'].SelfType = jsNull then
      tvr.tv_status := 'unknown'
    else
      tvr.tv_status := string(js.Field['status'].Value);

    if js.Field['type'].SelfType = jsNull then
      tvr.tv_classification := 'unknown'
    else

      tvr.tv_classification := string(js.Field['type'].Value);

    tvr.tv_running := Boolean(lowercase(tvr.tv_status) = 'running');
    tvr.tv_scripted := Boolean(lowercase(tvr.tv_classification) = 'scripted');

    if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      tvr.thetvdb_id := string(js.Field['externals'].Field['thetvdb'].Value);
    if js.Field['externals'].Field['tvrage'].SelfType <> jsNull then
      tvr.tvrage_id := string(js.Field['externals'].Field['tvrage'].Value);

    if js.Field['network'].SelfType = jsNull then
    begin
      if js.Field['webChannel'].SelfType <> jsNull then
      begin
        tvr.tv_network := string(js.Field['webChannel'].Field['name'].Value);
        if js.Field['webChannel'].Field['country'].SelfType = jsNull then
        begin
          tvr.tv_country := 'unknown';
        end
        else
        begin
          tvr.tv_country := string(js.Field['webChannel'].Field['country'].Field['code'].Value);
        end;
      end
      else
      begin
        tvr.tv_network := 'unknown';
        tvr.tv_country := 'unknown';
      end;
    end
    else
    begin
      tvr.tv_network := string(js.Field['network'].Field['name'].Value);
      tvr.tv_country := string(js.Field['network'].Field['country'].Field['code'].Value);
    end;

    if js.Field['schedule'].SelfType <> jsNull then
    begin
      for i := 0 to js.Field['schedule'].Field['days'].Count - 1 do
        tvr.tv_days.Add(string(js.Field['schedule'].Field['days'].Child[i].Value));
    end;

    if tvr.tv_country = 'US' then
      tvr.tv_country := 'USA';
    if tvr.tv_country = 'GB' then
      tvr.tv_country := 'UK';

    try

      if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      begin

        gTVDB.CommaText := getGenreFromTheTVDb(tvr.thetvdb_id);

        for I := 0 to gTVDB.Count - 1 do
          if slGen.IndexOf(gTVDB.Strings[i]) > -1 then
            tvr.tv_genres.Add(gTVDB.Strings[i]);
      end;

      for I := 0 to js.Field['genres'].Count - 1 do
        if slGen.IndexOf(string(js.Field['genres'].Child[i].Value)) > -1 then
          tvr.tv_genres.Add(string(js.Field['genres'].Child[i].Value));

    except on E: Exception do
      begin
        irc_addadmin(e.Message);
      end;

    end;

    if js.Field['premiered'].SelfType <> jsNull then
    begin

      x := TStringlist.Create;
      try
        x.Delimiter := '-';
        x.DelimitedText := string(js.Field['premiered'].Value);
        tvr.tv_premiered_year := StrToIntDef(x.Strings[0], -1);
      finally
        x.free;
      end;

    end
    else
      tvr.tv_premiered_year := -1;

    tvr.tv_next_ep := -10;
    tvr.tv_next_season := -10;
    tvr.tv_next_date := DateTimeToUnix(0);

    //Show not ended so we check for next.
    if lowercase(tvr.tv_status) <> 'ended' then
    begin
      findCurrentAirDate(js, season, episdoe, date);
      tvr.tv_next_season := season;
      tvr.tv_next_ep := episdoe;
      tvr.tv_next_date := DateTimeToUnix(date);
    end;

    {
try
 if ((js.Field['_embedded'] <> nil) and (js.Field['_embedded'].Field['previousepisode'] <> nil)) then
 begin
   ep_prevnum := StrToIntDef(string(js.Field['_embedded'].Field['previousepisode'].Field['number'].Value), -1);
   se_prevnum := StrToIntDef(string(js.Field['_embedded'].Field['previousepisode'].Field['season'].Value), -1);
   prevdt := UnixToDateTime(0);
   prevdt := StrToDateTime(string(js.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value) + ' ' +
     string(js.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value));
 end;
 if ((js.Field['_embedded'] <> nil) and (js.Field['_embedded'].Field['nextepisode'] <> nil)) then
 begin
   ep_nextnum := StrToIntDef(string(js.Field['_embedded'].Field['nextepisode'].Field['number'].Value), -1);
   se_nextnum := StrToIntDef(string(js.Field['_embedded'].Field['nextepisode'].Field['season'].Value), -1);
   nextdt := UnixToDateTime(0);
   nextdt := StrToDateTime(string(js.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value) + ' ' +
     string(js.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value));
 end;

 //Show ended there is no next!
 if lowercase(tvr.tv_status) = 'ended' then
 begin
   tvr.tv_next_ep := -1;
   tvr.tv_next_season := -1;
   tvr.tv_next_date := DateTimeToUnix(0);
 end
 else if (DateTimeToUnix(nextdt)) <= DateTimeToUnix(now()) then
 begin
   // next date is smaller|equal to now()..
   //Irc_AddAdmin('next date is smaller|equal to now()..');
   tvr.tv_next_ep := ep_nextnum;
   tvr.tv_next_season := se_nextnum;
   tvr.tv_next_date := DateTimeToUnix(nextdt);
 end
 else if (DateTimeToUnix(prevdt) + 86400) >= DateTimeToUnix(now()) then
 begin
   //previous date + 1Day is grater|equal to now()
   //Irc_AddAdmin('previous date + 1Day is grater|equal to now()');
   tvr.tv_next_ep := ep_prevnum;
   tvr.tv_next_season := se_prevnum;
   tvr.tv_next_date := DateTimeToUnix(prevdt);
 end
 else if (DateTimeToUnix(nextdt)) > DateTimeToUnix(now()) then
 begin
   // nothing before matched and next_date is grater then now, so we took this.
   //Irc_AddAdmin('nothing before matched and next_date is grater then now, so we took next.');
   tvr.tv_next_ep := ep_nextnum;
   tvr.tv_next_season := se_nextnum;
   tvr.tv_next_date := DateTimeToUnix(nextdt);
 end;

 (*
  if ((js.Field['_embedded'] <> nil) and (js.Field['_embedded'].Field['nextepisode'] <> nil)) then
       begin
         tvr.tv_next_season := StrToIntDef(string(js.Field['_embedded'].Field['nextepisode'].Field['season'].Value), -1);
         tvr.tv_next_ep := StrToIntDef(string(js.Field['_embedded'].Field['nextepisode'].Field['number'].Value), -1);
         tvr.tv_next_date := DateTimeToUnix(StrToDateTime(string(js.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value+' '+string(js.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value))));
       end;
 *)
except on E: Exception do
 begin
   Debug(dpError, section, Format('[Exception] in parseTVMazeInfos.findNext: %s', [e.Message]));
   irc_Adderror(Format('<c4>[Exception]</c> in parseTVMazeInfos.findNext: %s', [e.Message]));
 end;
end;
  }
//string(js.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value)
    result := tvr;
  finally
    js.free;
    slGen.free;
    gTVDB.free;
    // tvr.free;
  end;

end;

{ TPazoTheTVDbLookupTask }

constructor TPazoTVInfoLookupTask.Create(const netname, channel: string;
  site: string; pazo: TPazo; attempt: integer = 0);
begin
  self.attempt := attempt;
  self.initial_site := site;
  inherited Create(netname, channel, site, '', pazo);
end;

procedure TPazoTVInfoLookupTask.PostResults(network, country, classi,
  status, premyear, url: string; genre: TStringList);
begin
  if config.ReadBool(section, 'use_new_announce_style', True) then
  begin
    irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>TVMaze info</b> %s', [mainpazo.rls.rlsname,
      premyear, url]));
    irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [genre.CommaText, classi, status]));
    irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>Country</b> %s - <b>Network</b> %s', [country, network]));
  end
  else
  begin
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVInfo</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - %s',
      [mainpazo.rls.rlsname,
      premyear, url]));
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVInfo</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [genre.CommaText, classi, status]));
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVInfo</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ', [country, network]));
  end;
end;

function TPazoTVInfoLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTvRelease;
  r: TPazoTVInfoLookupTask;
  showA, showB, tvmaz, sid, uurl: string;
  db_tvinfo: TTVInfoDB;
  ps: TPazoSite;
  //  xml: TSLXMLDocument;
  //  st: TStream;

begin
  tr := TTvRelease(mainpazo.rls);

  // Show is in DataBase? Here we could add some Update routine and CurrentAired EP.
  try
    db_tvinfo := getTVInfoByShowName(tr.showname);
    if (db_tvinfo <> nil) then
    begin
      db_tvinfo.SetTVDbRelease(tr);
      ready := True;
      Result := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      //   db_tvrage := nil;
      Debug(dpError, section, Format('Exception in getTVInfoByShowName: %s', [e.Message])); // anpassen.
      ready := True;
      Result := True;
      exit;
    end;
  end;

  //Show is not found in the DB.
//  sid := findTheTVDBIDByName(tr.showname);

  sid := findTVMazeIDByName(tr.showname);

  if sid = 'FAILED' then
  begin

    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying TVMaze lookup for %s later', [tr.showname]);
      r := TPazoTVInfoLookupTask.Create(netname, channel, initial_site, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoTVInfoLookupTask Search %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoTVInfoLookupTask Search %s', [e.Message]));
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
      irc_addadmin('<c4><b>ERROR</c> No TVMaze ID found for %s</b>', [tr.showname]);
    end;
    ready := True;
    Result := True;
    exit;
  end;

  try
    uurl := 'http://api.tvmaze.com/shows/' + sid + '?embed[]=nextepisode&embed[]=previousepisode';
    tvmaz := slUrlGet(uurl);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoTVInfoLookupTask.execute httpGET(TVMaze): Exception : %s', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoTVInfoLookupTask.execute httpGET(TVMaze): Exception : %s', [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;

  if tvmaz = '' then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> http respons is empty for ' + tr.showname);
    Debug(dpSpam, section, 'ERROR http respons is empty for ' + tr.showname);
    Result := True;
    readyerror := True;
    exit;
  end;

  db_tvinfo := parseTVMazeInfos(tvmaz, tr.showname);

  if db_tvinfo = nil then
  begin
    Debug(dpError, section, 'Error parseTVMazeInfos returns nil.');
    Result := True;
    readyerror := True;
    exit;
  end;

  showA := replaceTVShowChars(db_tvinfo.tv_showname);
  showB := replaceTVShowChars(tr.showname);

  if onlyEnglishAlpha(showA) = onlyEnglishAlpha(showB) then
  begin
    try
      //      dbaddtvrage_SaveTVRage(db_tvrage.tv_showid, db_tvrage, mainpazo.rls.rlsname);
      irc_Addtext_by_key('ADDTVMAZE', Format('%s %s %s',
        [config.ReadString(section,
          'addcmd', '!addtvmaze'), mainpazo.rls.rlsname,
        db_tvinfo.tvmaze_id]));
      db_tvinfo.Save;
      db_tvinfo.SetTVDbRelease(tr);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in addtvinfo_SaveTVRage: %s', [e.Message]));
        Result := True;
        readyerror := True;
        db_tvinfo.free;
        exit;
      end;
    end;
  end
  else
  begin
    irc_addadmin('<c14><b>Info</b></c>: Alphanumeric check dont match! %s <> %s', [db_tvinfo.tv_showname, tr.showname]);

    if config.ReadBool(section, 'stop_on_englishcheck', True) then
    begin
      Result := True;
      ready := True;
      exit;
    end;
  end;
  (*
    if config.ReadBool(section, 'post_lookup_infos', False) then
    begin
      PostResults(db_tvinfo.tv_network, db_tvinfo.tv_country, db_tvinfo.tv_classification, db_tvinfo.tv_status,IntToStr(db_tvinfo.tv_premiered_year),db_tvinfo.tv_url, db_tvinfo.tv_genres);
    end;
  *)
  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTVInfoLookupTask kb_add: %s', [e.Message]));
    end;
  end;
  db_tvinfo.free;
  ready := True;
  Result := True;
end;

function TPazoTVInfoLookupTask.Name: string;
begin
  try
    Result := format('TVInfo PazoID(%d) %s @ %s Count(%d)', [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'TVInfo';
  end;
end;

//TPazoHTTPUpdateTVInfoTask

constructor TPazoHTTPUpdateTVInfoTask.Create(const netname: string; const channel: string; site: string; pazo: TPazo; attempt: Integer = 0);
begin

  inherited Create(netname, channel, site, '', pazo);
  //  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

destructor TPazoHTTPUpdateTVInfoTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPUpdateTVInfoTask.Name: string;
begin
  Result := Format('httpUpdateTVInfo : %s', [showname]);
end;

function TPazoHTTPUpdateTVInfoTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTVInfoDB;
  response: string;
  ps: TPazoSite;
begin
  showname := TTvRelease(mainpazo.rls).showname;
  tvmaze_id := TTvRelease(mainpazo.rls).showid;

  response := slUrlGet('http://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode');
  if response = '' then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> TVInfo updated for ' + showname + ' failed, response for was empty');
    Debug(dpSpam, section, 'ERROR TVInfo updated for ' + showname + ' failed, response for was empty');
    Result := True;
    readyerror := True;
    exit;
  end;
  try
    tvdb := parseTVMazeInfos(response, showname);
  except on E: Exception do
      irc_adderror(Format('parseTVMazeInfos  -=> %s', [e.Message]));
  end;

  try
    try
      result := tvdb.Update;
    except on E: Exception do
        irc_adderror(Format('parseTVMazeInfos  -=> %s', [e.Message]));
    end;

  finally
    //tvdb.free;
  end;

  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTVInfoUpdateTask kb_add: %s', [e.Message]));
    end;
  end;

end;

{ TPazoHTTPTVInfoTask }

constructor TPazoHTTPTVInfoTask.Create(const tvmaze_id: string; rls: string =
  '');
begin
  self.tvmaze_id := tvmaze_id;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

function TPazoHTTPTVInfoTask.Name: string;
begin
  try
    Result := Format('httpTVMaze : %s', [tvmaze_id]);
  except
    Result := 'httpTVMaze';
  end;
end;

destructor TPazoHTTPTVInfoTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPTVInfoTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTVInfoDB;
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
      '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?(E\d{1,4})?)?[\._-](.*)';
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

  uurl := 'http://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode';
  response := slUrlGet(uurl);

  if response = '' then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> no infos found for ' + rls + '(' + sname + ')');
    Debug(dpSpam, section, '<c4><b>ERROR</c></b> no infos found for ' + rls + '(' + sname + ')');
    Result := True;
    readyerror := True;
    exit;
  end;

  tvdb := parseTVMazeInfos(response, sname);
  try
    if tvdb <> nil then
      saveTVInfos(tvmaze_id, tvdb, rls, false);
  finally
    tvdb.free;
  end;

  result := True;
end;

end.

