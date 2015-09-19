unit dbthetvdb;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TTheTvDB = class
  private
    last_updated: integer;
    function getClassification: string;
  public
    rls_showname: string;
    tv_showid: string;
    tv_showname: string;
    tv_country: string;
    tv_status: string;
    tv_classification: string;
    tv_genres: TStringList;
    tv_network: string;
    //tv_runtime: integer;
    tv_premiered_year: integer;
    tv_endedyear: integer;
    tv_running: boolean;
    tv_scripted: boolean;
    constructor Create(rls_showname: string); //overload;
    destructor Destroy; override;
    function Name: string;
    procedure Save;
    procedure PostResults(rls: string = ''); overload;
    procedure PostResults(Netname, Channel: string; rls: string = ''); overload;
    procedure SetTVRageRelease(var tr: TTVRelease);

  end;

function getTheTVDbInfoCount: integer;
function TheTVDbStatus: string;


procedure dbthetvdbInit;
procedure dbthetvdbStart;
procedure dbthetvdbUnInit;



function getTheTVDBbyShowName(rls_showname: string): TTheTvDB;
function getTheTVDBbyReleaseName(rls: string): TTheTvDB;
function getTheTVDBbyShowID(tv_showid: string): TTheTvDB;

procedure saveTheTVDbInfos(tv_showid: string; tvrage: TTheTvDB; rls: string = '');
procedure addTheTVDBInfos(params: string);
procedure TheTVDbFireKbAdd(rls: string);


implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit,
  console, ircblowfish,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  taskthetvdblookup, pazo, mrdohutils;

const
  section = 'tasktvdb';

var
  thetvdb: TslSqliteDB = nil;
  sql_addthetvdb: Psqlite3_stmt = nil;
  sql_countthetvdb: Psqlite3_stmt = nil;

  addthetvdbcmd: string;
  oldthetvdbcmd: string;

  last_addthetvdb: THashedStringList;

function getTheTVDbInfoCount: integer;
var
  icount: Psqlite3_stmt;
begin
  icount := thetvdb.Open('SELECT count(*) FROM infos;');
  if thetvdb.Step(icount) then
    result := thetvdb.column_int(icount, 0)
  else
    result := -1;
end;

procedure TTheTvDB.Save;
var
  n: Psqlite3_stmt;
  dbid: integer;
begin
  dbid := -1;

try
  thetvdb.ExecSQL(Format('INSERT OR IGNORE INTO  infos (tvdb_id,premiered_year,country,status,classification,network,genre,endedyear,last_updated) VALUES (%d,%d,"%s","%s","%s","%s","%s",%d,%d)',
    [StrToInt(tv_showid), tv_premiered_year, tv_country, tv_status, tv_classification, tv_network,tv_genres.CommaText, tv_endedyear, DateTimeToUnix(now())]));
except on E: Exception do
Irc_AddText('','','Error@TTheTvDB.Save_INSERT infos %s',[e.Message]);
end;



  n := thetvdb.Open('SELECT id from infos WHERE tvdb_id = ' + tv_showid);
  if thetvdb.Step(n) then
    dbid := thetvdb.column_int(n, 0);

  if dbid = -1 then
  begin
    //showInfos was not added!
  end;

  thetvdb.ExecSQL(Format('INSERT OR IGNORE INTO series (rip,showname,info_id) VALUES ("%s","%s",%d);',[rls_showname,tv_showname,dbid]));


end;

procedure TTheTvDB.SetTVRageRelease(var tr: TTVRelease);
begin
  tr.showname := rls_showname;
  tr.showid := tv_showid;
  tr.premier_year := tv_premiered_year;
  tr.country := tv_country;
  tr.status := tv_status;
  tr.classification := tv_classification;
  tr.genres.Assign(tv_genres);
  tr.network := tv_network;
  tr.running := tv_running;
  tr.ended_year := tv_endedyear;
  tr.scripted := tv_scripted;
  //  tr.season:= tv_seasons;

//  PostResults(tr.rlsname);
end;

function TTheTvDB.getClassification: string;
begin
  //tv_genres.IndexOf('')
  tv_scripted := False;
  if tv_genres.IndexOf('Animation') > -1 then
  begin
    result := 'Animation';
    tv_classification := 'Animation';
    Exit;
  end;

  if tv_genres.IndexOf('Documentary') > -1 then
  begin
    result := 'Documentary';
    tv_classification := 'Documentary';
    Exit;
  end;

  if tv_genres.IndexOf('Game Show') > -1 then
  begin
    result := 'Game Show';
    tv_classification := 'Game Show';
    Exit;
  end;

  if tv_genres.IndexOf('Mini Series') > -1 then
  begin
    result := 'Mini-Series';
    tv_classification := 'Mini-Series';
    tv_scripted := True;
    Exit;
  end;

  if tv_genres.IndexOf('News') > -1 then
  begin
    result := 'News';
    tv_classification := 'News';
    Exit;
  end;

  if tv_genres.IndexOf('Reality') > -1 then
  begin
    result := 'Reality';
    tv_classification := 'Reality';
    Exit;
  end;

  if tv_genres.IndexOf('Sport') > -1 then
  begin
    result := 'Sports';
    tv_classification := 'Sports';
    Exit;
  end;

  if tv_genres.IndexOf('Talk Show') > -1 then
  begin
    result := 'Talk Show';
    tv_classification := 'Talk Show';
    Exit;
  end;

  if tv_genres.IndexOf('Special interest') > -1 then
  begin
    result := 'Variety';
    tv_classification := 'Variety';
    Exit;
  end;

  tv_classification := 'Scripted';
  tv_scripted := True;

end;

constructor TTheTvDB.Create(rls_showname: string);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_endedyear:=-1;
end;

destructor TTheTvDB.Destroy;
begin
  self.tv_genres.Free;
  inherited;
end;

function TTheTvDB.Name: string;
begin
  try
    Result := 'TTVRelease :' + rls_showname + ' : ';
  except
    Result := 'TTVRelease';
  end;
end;


(*
<c3>[<b>TTVRelease</b>]</c><b>Falling Skies</b> -<b>Premiere Year</b> 2011 - <b>The TVDB info</b>http://tvrage.com/shows/id-26205/
<c3>[<b>TTVRelease</b>]</c><b>Genre</b> Action,Military/War,Mystery,Sci-Fi - <b>Classification</b>Scripted - <b>Status</b> Final Season
<c3>[<b>TTVRelease</b>]</c><b>Country</b> USA - <b>Network</b> TNT DRAMA

*)


procedure TTheTvDB.PostResults(rls: string = '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
      irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://thetvdb.com/?tab=series&id=%s',[rls,IntToStr(tv_premiered_year), tv_showid]));
      irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s',[tv_genres.CommaText, tv_classification, tv_status]));
      irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s',[tv_country, tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbTVRage.PostResultsA: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TDbTVRage.PostResultsA: %s', [e.Message]));
    end;
  end;
end;

procedure TTheTvDB.PostResults(Netname: string; Channel: string; rls: string = '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
      irc_Addtext(Netname,Channel,Format('<c3>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://thetvdb.com/?tab=series&id=%s',[rls,IntToStr(tv_premiered_year), tv_showid]));
      irc_Addtext(Netname,Channel,Format('<c3>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s',[tv_genres.CommaText, tv_classification, tv_status]));
      irc_Addtext(Netname,Channel,Format('<c3>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s',[tv_country, tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbTVRage.PostResultsB: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TDbTVRage.PostResultsB: %s', [e.Message]));
    end;
  end;
end;



function TheTVDbStatus: string;
begin
  Result := Format('<b>TTVRelease.db</b>: %d Series infos',
    [getTheTVDbInfoCount]);
end;

procedure dbTheTVDbStart;
var
  db_name, db_params: string;
begin
  addthetvdbcmd := config.ReadString(section, 'addcmd', '!addthetvdb');
  if slsqlite_inited then
  begin
    db_name := Trim(config.ReadString(section, 'db_file', 'ttvrelease.db'));
    db_params := config.ReadString(section, 'pragma', '');
    thetvdb := TslSqliteDB.Create(db_name, db_params);

    thetvdb.ExecSQL('CREATE TABLE IF NOT EXISTS "infos" ("id"  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"tvdb_id"  INTEGER NOT NULL,' +
      '"premiered_year"  INTEGER NOT NULL,"country"  TEXT NOT NULL,"status"  TEXT NOT NULL,"classification"  TEXT NOT NULL,"network"  TEXT NOT NULL,' +
      '"genre"  TEXT NOT NULL,"endedyear"  INTEGER NOT NULL DEFAULT -1,"last_updated" INTEGER NOT NULL DEFAULT -1);');

    thetvdb.ExecSQL('CREATE TABLE IF NOT EXISTS "series" ("id"  INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"rip"  TEXT NOT NULL,"showname"  TEXT NOT NULL,' +
      '"country"  TEXT,"info_id"  INTEGER NOT NULL,CONSTRAINT "Series" FOREIGN KEY ("info_id") REFERENCES "infos" ("id") ON DELETE CASCADE ON UPDATE CASCADE);');

    thetvdb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "Rips" ON "series" ("rip" ASC);');

    Console_Addline('', 'TTVRelease SQLite db loaded. '+IntToStr(getTheTVDbInfoCount));
  end;
end;

procedure dbTheTVDbInit;
begin
  last_addthetvdb := THashedStringList.Create;
  last_addthetvdb.CaseSensitive := False;
end;

procedure dbTheTVDbUninit;
begin
  last_addthetvdb.Free;
  if thetvdb <> nil then
  begin
    thetvdb.Free;
    thetvdb := nil;
  end;
end;


function getTheTVDBbyShowName(rls_showname: string): TTheTvDB;
var
  i:      integer;
  tvrage: TTheTvDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if thetvdb = nil then
    exit;

  if (rls_showname = '') then
    exit;

  try

    i := last_addthetvdb.IndexOf(rls_showname);
    if i <> -1 then
    begin
      Result := TTheTvDB(last_addthetvdb.Objects[i]);
//      irc_addtext('','','whatts');
    end;
  except
    Result := nil;
  end;
  if (Result = nil) then
  begin
    try
      gettvrage := thetvdb.Open(
        'SELECT * FROM addtvrage WHERE rls_showname LIKE "' + rls_showname + '"');
      if thetvdb.Step(gettvrage) then
      begin
        if (LowerCase(rls_showname) <> LowerCase(thetvdb.column_text(gettvrage, 0))) then
        begin
          Result := nil;
          exit;
        end;

        tvrage := TTheTvDB.Create(rls_showname);
        tvrage.tv_showid := thetvdb.column_text(gettvrage, 1);
        tvrage.tv_showname := thetvdb.column_text(gettvrage, 2);
//        tvrage.tv_showurl := thetvdb.column_text(gettvrage, 3);
        tvrage.tv_premiered_year := StrToIntDef(thetvdb.column_text(gettvrage, 4), 0);
        tvrage.tv_country := thetvdb.column_text(gettvrage, 5);
        tvrage.tv_status := thetvdb.column_text(gettvrage, 6);
        tvrage.tv_classification := thetvdb.column_text(gettvrage, 7);
        tvrage.tv_genres.CommaText := thetvdb.column_text(gettvrage, 8);
        tvrage.tv_network := thetvdb.column_text(gettvrage, 9);
//        tvrage.tv_runtime := StrToIntDef(thetvdb.column_text(gettvrage, 10), 0);
        tvrage.tv_running := StrToBoolDef(thetvdb.column_text(gettvrage, 11), False);
        tvrage.tv_endedyear := StrToIntDef(thetvdb.column_text(gettvrage, 12), -1);
        //tvrage.tv_seasons:= StrToIntDef(addtvrageDB.column_text(gettvrage, 13),-1);
        //last_updated:=StrToIntDef(addtvrageDB.column_text(gettvrage, 14),-1);

        //         last_addtvrage.AddObject(rls_showname, tvrage);
        Result := tvrage;
      end;
    except
      on e: Exception do
      begin
        Result := nil;
        Debug(dpError, section, Format('[EXCEPTION] dbaddtvrage_gettvrage_show: %s ',
          [e.Message]));
      end;
    end;
  end;
end;



function getTheTVDBbyReleaseName(rls: string): TTheTvDB;
var
  showname: string;
  //  tvr:      TTVRelease;
  rx: TRegexpr;
begin
  Result := nil;


  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;

    rx.Expression := '(.*)[\._-](\d{4}[\._-]\d{2}[\._-]\d{2}|\d{2}[\._-]\d{2}[\._-]\d{4})[\._-](.*)';
    if rx.Exec(rls) then
    begin
      showname := rx.Match[1];
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rls) then
      showname := rx.Match[1];

    rx.Expression := '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?)?[\._-](.*)';
    if rx.Exec(rls) then
      showname := rx.Match[1];
    rx.Expression := '[\.\_]';
    showname := rx.Replace(showname, ' ');
  finally
    rx.Free;
  end;

  if (showname <> '') then
  begin
      Result := getTheTVDBbyShowName(showname);
  end;
end;

function getTheTVDBbyShowID(tv_showid: string): TTheTvDB;
var
  i:      integer;
  tvrage: TTheTvDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if thetvdb = nil then
    exit;

  for i := last_addthetvdb.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    try
      tvrage := TTheTvDB(last_addthetvdb.Objects[i]);
      if (tvrage.tv_showid = tv_showid) then
      begin
        Result := tvrage;
        break;
      end;
    except
      break;
    end;
  end;

  if (Result = nil) then
  begin
    try
      gettvrage := thetvdb.Open('SELECT * FROM addtvrage WHERE tv_showid = "' +
        tv_showid + '"');
      if thetvdb.Step(gettvrage) then
      begin
        tvrage := TTheTvDB.Create(thetvdb.column_text(gettvrage, 0));
        tvrage.tv_showid := thetvdb.column_text(gettvrage, 1);
        tvrage.tv_showname := thetvdb.column_text(gettvrage, 2);
//        tvrage.tv_showurl := thetvdb.column_text(gettvrage, 3);
        tvrage.tv_premiered_year := thetvdb.column_int(gettvrage, 4);
        tvrage.tv_country := thetvdb.column_text(gettvrage, 5);
        tvrage.tv_status := thetvdb.column_text(gettvrage, 6);
        tvrage.tv_classification := thetvdb.column_text(gettvrage, 7);
        //          tvrage.tv_genres.DelimitedText := addtvrageDB.column_text(gettvrage, 8);
        tvrage.tv_genres.CommaText := thetvdb.column_text(gettvrage, 8);
        tvrage.tv_network := thetvdb.column_text(gettvrage, 9);
//        tvrage.tv_runtime := thetvdb.column_int(gettvrage, 10);
        tvrage.tv_running :=
          StrToBoolDef(thetvdb.column_text(gettvrage, 11), False);
        tvrage.tv_endedyear := thetvdb.column_int(gettvrage, 12);
        //          tvrage.tv_seasons := addtvrageDB.column_int(gettvrage, 13);
        //          tvrage.last_updated := addtvrageDB.column_int(gettvrage, 14);

        //          last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
        Result := tvrage;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddtvrage_gettvrage_id: %s ',
          [e.Message]));
      end;
    end;
  end;
end;


procedure addTheTVDBInfos(params: string);
var
  rls: string;
  tv_showid: string;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  tv_showid := '';
  tv_showid := SubString(params, ' ', 2);

  try
    AddTask(TPazoHTTPTheTVDbTask.Create(tv_showid, rls));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in dbaddtvrage_addtvrage AddTask: %s',
        [e.Message]));
      exit;
    end;
  end;
end;

procedure saveTheTVDbInfos(tv_showid: string; tvrage: TTheTvDB; rls: string = '');
var
  save_tvrage: TTheTvDB;
begin
  if (getTheTVDBbyShowID(tv_showid) = nil) then
  begin
    // add the tvrage
    save_tvrage := TTheTvDB(tvrage);
    try
      //      last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
      if (rls <> '') then
        irc_Addtext_by_key('ADDTVRAGE', '!addtvrage ' + rls + ' ' + tv_showid);
    except
      on e: Exception do
      begin
        Debug(dpError, section,
          Format('Exception in dbaddtvrage_SaveTVRage last_addtvrage.Add: %s',
          [e.Message]));
        exit;
      end;
    end;

    try
      save_tvrage.Save;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] tvrage.Save: %s ', [e.Message]));
      end;
    end;



    if (rls <> '') then
    begin
      TheTVDbFireKbAdd(rls);
    end;

  end;
end;

procedure TheTVDbFireKbAdd(rls: string);
var
  p:  TPazo;
  ps: TPazoSite;
begin
  p := FindPazoByRls(rls);
  if (p <> nil) then
  begin
    ps := FindMostCompleteSite(p);
    if ((ps = nil) and (p.sites.Count > 0)) then
      ps := TPazoSite(p.sites[0]);

    if (ps <> nil) then
    begin
      try
        if spamcfg.ReadBool('addinfo', 'tvrageupdate', True) then
          irc_Addadmin(Format('<c3>[TTVRelease]</c> %s %s now has TV infos (%s)',
            [p.rls.section, p.rls.rlsname, ps.Name]));
        kb_Add('', '', ps.Name, p.rls.section, '', 'UPDATE', p.rls.rlsname, '');
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] TTVRelease_kb_Add : %s', [e.Message]);
        end;
      end;
    end;
  end;
end;


end.

