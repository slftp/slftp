unit dbaddimdb;

interface

uses Classes, IniFiles, irc, kb, Contnrs;

type
  TDbImdb = class
    rls: String;
    imdb_id: String;
    constructor Create(rls, imdb_id: string);
    destructor Destroy; override;
  end;

  TDbImdbData = class
    imdb_id: string;
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
    imdb_stvs:string;
    constructor Create(imdb_id :string);
    destructor Destroy; override;
    procedure PostResults(rls : String = '');overload;
    procedure PostResults(const netname, channel: string; rls : String = '');overload;
  end;

function dbaddimdb_Process(net, chan, nick, msg: string): Boolean;
procedure dbaddimdb_SaveImdb(rls, imdb_id: string);
procedure dbaddimdb_SaveImdbData(rls: string; imdbdata: TDbImdbData);
procedure dbaddimdb_addimdb(params: string);
procedure dbaddimdb_ParseImdb(rls, imdb_id: string);
procedure dbaddimdb_FireKbAdd(rls : String);

function dbaddimdb_Status: string;

procedure dbaddimdbInit;
procedure dbaddimdbStart;
procedure dbaddimdbUnInit;

var
  last_addimdb: THashedStringList;
  last_imdbdata: THashedStringList;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit, taskhttpimdb, pazo, mrdohutils;

const
  section = 'dbaddimdb';

var
  addimdbcmd: string;

{ TDbImdb }
constructor TDbImdb.Create(rls, imdb_id: string);
begin
  self.rls := rls;
  self.imdb_id := imdb_id;
end;

destructor TDbImdb.Destroy;
begin
  inherited;
end;

{ TDbImdbData }
constructor TDbImdbData.Create(imdb_id:string);
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

procedure TDbImdbData.PostResults(rls : String = '');
var stvstatus:string;
begin
  if imdb_stvm then stvstatus:='STV' else stvstatus:='CINE';
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: http://www.imdb.com/title/%s/   (%d)',[rls, imdb_id, imdb_year]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s',[imdb_countries.DelimitedText,imdb_languages.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [imdb_genres.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) @ %d Screens (%s)',[imdb_rating,imdb_votes,imdb_screens,stvstatus]));
end;

procedure TDbImdbData.PostResults(const netname, channel: string; rls : String = '');
var stvstatus:string;
begin
  if imdb_stvm then stvstatus:='STV' else stvstatus:='CINE';
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: http://www.imdb.com/title/%s/   (%d)',[rls, imdb_id, imdb_year]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s',[imdb_countries.DelimitedText,imdb_languages.DelimitedText]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [imdb_genres.DelimitedText]));
  irc_AddText(netname, channel, Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) @ %d Screens (%s)',[imdb_rating,imdb_votes,imdb_screens,stvstatus]));
end;

{ Proc/Func }

function dbaddimdb_Process(net, chan, nick, msg: string): Boolean;
begin
  Result := False;
  if (1 = Pos(addimdbcmd, msg)) then
  begin
    msg := Copy(msg, length(addimdbcmd + ' ') + 1, 1000);
    dbaddimdb_addimdb(msg);
    Result := True;
  end;
end;

procedure dbaddimdb_addimdb(params: string);
var
  rls: string;
  imdb_id: string;
  i: Integer;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  imdb_id := '';
  imdb_id := SubString(params, ' ', 2);

  if ((rls <> '') and (imdb_id <> '')) then
  begin
    i:= last_addimdb.IndexOf(rls);
    if i <> -1 then
    begin
      exit;
    end;
    
    try
      dbaddimdb_SaveImdb(rls, imdb_id);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddimdb_addimdb AddTask: %s', [e.Message]));
        exit;
      end;
    end;
  end;
end;

procedure dbaddimdb_SaveImdb(rls, imdb_id: string);
var
  i: Integer;
  db_imdb: TDbImdb;
begin
  i:= last_addimdb.IndexOf(rls);
  if i = -1 then
  begin
    db_imdb:= TDbImdb.Create(rls, imdb_id);
    last_addimdb.AddObject(rls, db_imdb);

    irc_AddInfo(Format('<c7>[iMDB]</c> for <b>%s</b> : %s', [rls, imdb_id]));
    irc_Addtext_by_key('addimdb', '!addimdb '+rls+' '+imdb_id);

    dbaddimdb_ParseImdb(rls, imdb_id);

    i:= last_addimdb.Count;
    while i > 100 do
    begin
      last_addimdb.Delete(0);
      i:= last_addimdb.Count - 1;
    end;
  end;
end;

procedure dbaddimdb_SaveImdbData(rls: string; imdbdata: TDbImdbData);
var i: Integer;
    p : TPazo;
begin
  i:= last_imdbdata.IndexOf(rls);
  if i = -1 then
  begin
    last_imdbdata.AddObject(rls, imdbdata);

    irc_AddInfo(Format('<c7>[iMDB Data]</c> for <b>%s</b> : %s', [rls, imdbdata.imdb_id]));
    imdbdata.PostResults(rls);

    dbaddimdb_FireKbAdd(rls);

    i:= last_imdbdata.Count;
    if i > 125 then
    begin
      while i > 100 do
      begin
        last_imdbdata.Delete(0);
        i:= last_imdbdata.Count - 1;
      end;
    end;
  end;
end;

procedure dbaddimdb_ParseImdb(rls, imdb_id: string);
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

procedure dbaddimdb_FireKbAdd(rls : String);
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
            irc_Addadmin(Format('<c3>[ADDIMDB RLZ]</c> %s %s now have iMDB infos (%s)', [p.rls.section, p.rls.rlsname, ps.name]));
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

{ Status }

function dbaddimdb_Status: string;
begin
  Result := '';

  Result:= Format('<b>iMDB</b>: %d, <b>iMDB data</b>: %d',[last_addimdb.Count, last_imdbdata.Count]);
end;

{ Init }

procedure dbaddimdbInit;
begin
  last_addimdb:= THashedStringList.Create;
  last_addimdb.CaseSensitive:= False;
  last_imdbdata:= THashedStringList.Create;
  last_imdbdata.CaseSensitive:= False;
end;

procedure dbaddimdbStart;
begin
  addimdbcmd := config.ReadString(section, 'addimdbcmd', '!addimdb');
end;

procedure dbaddimdbUninit;
begin
  last_addimdb.Free;
  last_imdbdata.Free;
end;

end.

