unit dbaddimdb;

interface

uses Classes, IniFiles, irc, kb, Contnrs, SyncObjs;

type
  TDbImdb = class
    rls: AnsiString;
    imdb_id: AnsiString;
    constructor Create(rls, imdb_id: AnsiString);
    destructor Destroy; override;
  end;

  TDbImdbData = class
    imdb_id: AnsiString;
    imdb_year: Integer;
    imdb_languages: TStringList;
    imdb_countries: TStringList;
    imdb_genres: TStringList;
    imdb_screens: Integer;
    imdb_rating: Integer;
    imdb_votes: Integer;
    imdb_cineyear:integer;
    imdb_ldt:boolean;
    imdb_wide:boolean;
    imdb_festival:boolean;
    imdb_stvm:boolean;
    imdb_stvs:AnsiString;
    constructor Create(imdb_id :AnsiString);
    destructor Destroy; override;
    procedure PostResults(rls : AnsiString = '');overload;
    procedure PostResults(const netname, channel: AnsiString; rls : AnsiString = '');overload;
  end;

function dbaddimdb_Process(net, chan, nick, msg: AnsiString): Boolean;
procedure dbaddimdb_SaveImdb(rls, imdb_id: AnsiString);
procedure dbaddimdb_SaveImdbData(rls: AnsiString; imdbdata: TDbImdbData);
procedure dbaddimdb_addimdb(params: AnsiString);
procedure dbaddimdb_ParseImdb(rls, imdb_id: AnsiString);
procedure dbaddimdb_FireKbAdd(rls : AnsiString);

function dbaddimdb_Status: AnsiString;
function dbaddimdb_checkid(const imdbid: AnsiString): Boolean;
function dbaddimdb_parseid(const text: AnsiString; out imdbid: AnsiString): Boolean;

procedure dbaddimdbInit;
procedure dbaddimdbStart;
procedure dbaddimdbUnInit;

var
  last_addimdb: THashedStringList;
  last_imdbdata: THashedStringList;
  dbaddimdb_cs: TCriticalSection;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, FLRE,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit, taskhttpimdb, pazo, mrdohutils;

const
  section = 'dbaddimdb';

var
  rx_imdbid: TFLRE;
  rx_captures: TFLREMultiCaptures;
  addimdbcmd: AnsiString;

{ TDbImdb }
constructor TDbImdb.Create(rls, imdb_id: AnsiString);
begin
  self.rls := rls;
  self.imdb_id := imdb_id;
end;

destructor TDbImdb.Destroy;
begin
  inherited;
end;

{ TDbImdbData }
constructor TDbImdbData.Create(imdb_id:AnsiString);
begin
  self.imdb_id := imdb_id;
  imdb_languages:= TStringList.Create;
  imdb_countries:= TStringList.Create;
  imdb_genres:= TStringList.Create;
end;

destructor TDbImdbData.Destroy;
begin
  imdb_languages.Free;
  imdb_countries.Free;
  imdb_genres.Free;
  inherited;
end;

procedure TDbImdbData.PostResults(rls : AnsiString = '');
var status:AnsiString;
begin

  if imdb_stvm then status := 'STV'
  else if imdb_festival then status := 'Festival'
  else if imdb_ldt then status := 'Limited'
  else if imdb_wide then status := 'Wide'
  else status :=' Cine';

  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: http://www.imdb.com/title/%s/   (%d)',[rls, imdb_id, imdb_year]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s',[imdb_countries.DelimitedText,imdb_languages.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [imdb_genres.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) @ %d Screens (%s)',[imdb_rating,imdb_votes,imdb_screens,status]));
end;

procedure TDbImdbData.PostResults(const netname, channel: AnsiString; rls : AnsiString = '');
var status:AnsiString;
begin
  if imdb_stvm then status := 'STV'
  else if imdb_festival then status := 'Festival'
  else if imdb_ldt then status := 'Limited'
  else if imdb_wide then status := 'Wide'
  else status :=' Cine';

  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: http://www.imdb.com/title/%s/   (%d)',[rls, imdb_id, imdb_year]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s',[imdb_countries.DelimitedText,imdb_languages.DelimitedText]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [imdb_genres.DelimitedText]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) @ %d Screens (%s)',[imdb_rating,imdb_votes,imdb_screens,status]));
end;

{ Proc/Func }

function dbaddimdb_Process(net, chan, nick, msg: AnsiString): Boolean;
begin
  Result := False;
  if (1 = Pos(addimdbcmd, msg)) then
  begin
    msg := Copy(msg, length(addimdbcmd + ' ') + 1, 1000);
    dbaddimdb_addimdb(msg);
    Result := True;
  end;
end;

procedure dbaddimdb_addimdb(params: AnsiString);
var
  rls: AnsiString;
  imdb_id: AnsiString;
  i: Integer;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  imdb_id := '';
  imdb_id := SubString(params, ' ', 2);

  if not dbaddimdb_checkid(imdb_id) then
  begin
    Debug(dpSpam, section, '[ADDIMDB] Invalid IMDB ID for %s: %s', [rls, imdb_id]);
    exit;
  end;

  if ((rls <> '') and (imdb_id <> '')) then
  begin
    dbaddimdb_cs.Enter;
    try
      i:= last_addimdb.IndexOf(rls);
    finally
      dbaddimdb_cs.Leave;
    end;

    if i <> -1 then
    begin
      exit;
    end;

    try
      dbaddimdb_SaveImdb(rls, imdb_id);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddimdb_addimdb (SaveImdb): %s', [e.Message]));
        exit;
      end;
    end;
  end;
end;

procedure dbaddimdb_SaveImdb(rls, imdb_id: AnsiString);
var
  i: Integer;
  db_imdb: TDbImdb;
begin
  dbaddimdb_cs.Enter;
  try
    i:= last_addimdb.IndexOf(rls);
  finally
    dbaddimdb_cs.Leave;
  end;
  if i = -1 then
  begin
    db_imdb := TDbImdb.Create(rls, imdb_id);

    dbaddimdb_cs.Enter;
    try
      try
        last_addimdb.AddObject(rls, db_imdb);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdb (AddObject): %s', [e.Message]));
          exit;
        end;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;

    irc_AddInfo(Format('<c7>[iMDB]</c> for <b>%s</b> : %s', [rls, imdb_id]));
    irc_Addtext_by_key('addimdb', '!addimdb '+rls+' '+imdb_id);

    try
      dbaddimdb_ParseImdb(rls, imdb_id);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdb (Parse): %s', [e.Message]));
        exit;
      end;
    end;

    dbaddimdb_cs.Enter;
    try
      i:= last_addimdb.Count;
      try
        while i > 100 do
        begin
          last_addimdb.Delete(0);
          i:= last_addimdb.Count - 1;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdb (cleanup): %s', [e.Message]));
          exit;
        end;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;
  end;
end;

procedure dbaddimdb_SaveImdbData(rls: AnsiString; imdbdata: TDbImdbData);
var
  i: Integer;
begin
  dbaddimdb_cs.Enter;
  try
    i:= last_imdbdata.IndexOf(rls);
  finally
    dbaddimdb_cs.Leave;
  end;

  if i = -1 then
  begin
    dbaddimdb_cs.Enter;
    try
      try
        last_imdbdata.AddObject(rls, imdbdata);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdbData (AddObject): %s', [e.Message]));
          exit;
        end;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;

    if config.ReadBool(section, 'post_lookup_infos', false) then
    begin
      irc_AddInfo(Format('<c7>[iMDB Data]</c> for <b>%s</b> : %s', [rls, imdbdata.imdb_id]));
      imdbdata.PostResults(rls);
    end;

    try
      dbaddimdb_FireKbAdd(rls);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdbData (FireKbAdd): %s', [e.Message]));
        exit;
      end;
    end;

    dbaddimdb_cs.Enter;
    try
      i:= last_imdbdata.Count;
      try
        while i > 100 do
        begin
          last_imdbdata.Delete(0);
          i:= last_imdbdata.Count - 1;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdbData (cleanup): %s', [e.Message]));
          exit;
        end;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;
  end;
end;

procedure dbaddimdb_ParseImdb(rls, imdb_id: AnsiString);
begin
  try
    AddTask(TPazoHTTPImdbTask.Create(imdb_id, rls));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in dbaddimdb_SaveImdbRls AddTask: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure dbaddimdb_FireKbAdd(rls : AnsiString);
var p : TPazo;
    ps: TPazoSite;
begin
  try
    p:= FindPazoByRls(rls);
    if (p <> nil) then
    begin
      if p.rls is TIMDBRelease then
      begin
        p.rls.Aktualizal(p);
        ps := FindMostCompleteSite(p);
        if ((ps = nil) and (p.sites.Count > 0)) then
          ps:= TPazoSite(p.sites[0]);

        if (ps <> nil) then
        begin
          if spamcfg.ReadBool('addinfo','imdbupdate',True) then
            irc_SendUPDATE(Format('<c3>[ADDIMDB]</c> %s %s now has iMDB infos (%s)', [p.rls.section, p.rls.rlsname, ps.name]));
          kb_Add('', '', ps.name, p.rls.section, '', 'UPDATE', p.rls.rlsname, '');
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] dbaddimdb_FireKbAdd : %s', [e.Message]);
    end;
  end;
end;

{ Checkid }
function dbaddimdb_checkid(const imdbid: AnsiString): Boolean;
begin
  Result := False;
  dbaddimdb_cs.Enter;
  try
    try
      if rx_imdbid.Find(imdbid) <> 0 then
        Result := True;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_checkid: Exception : %s', [e.Message]));
        exit;
      end;
    end;
  finally
    dbaddimdb_cs.Leave;
  end;
end;

{ Parseid }
function dbaddimdb_parseid(const text: AnsiString; out imdbid: AnsiString): Boolean;
begin
  imdbid := '';
  Result := False;
  try
    dbaddimdb_cs.Enter;
    try
      if rx_imdbid.MatchAll(text, rx_captures, 1 ,1) then
      begin
        imdbid := Copy(text, rx_captures[0][0].Start, rx_captures[0][0].Length);
        Result := True;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_checkid: Exception : %s', [e.Message]));
        exit;
      end;
    end;
  finally
    SetLength(rx_captures, 0);
    dbaddimdb_cs.Leave;
  end;
end;


{ Status }
function dbaddimdb_Status: AnsiString;
begin
  Result := Format('<b>iMDB</b>: %d, <b>iMDB data</b>: %d',[last_addimdb.Count, last_imdbdata.Count]);
end;

{ Init }

procedure dbaddimdbInit;
begin
  dbaddimdb_cs := TCriticalSection.Create;
  last_addimdb:= THashedStringList.Create;
  last_addimdb.CaseSensitive:= False;
  last_imdbdata:= THashedStringList.Create;
  last_imdbdata.CaseSensitive:= False;
  rx_imdbid := TFLRE.Create('tt(\d{6,7})', [rfIGNORECASE]);
end;

procedure dbaddimdbStart;
begin
  addimdbcmd := config.ReadString(section, 'addimdbcmd', '!addimdb');
end;

procedure dbaddimdbUninit;
begin
  dbaddimdb_cs.Enter;
  try
    FreeAndNil(last_addimdb);
    FreeAndNil(last_imdbdata);
    FreeAndNil(rx_imdbid);
  finally
    dbaddimdb_cs.Leave;
  end;
  dbaddimdb_cs.Free;
end;

end.


