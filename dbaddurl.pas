unit dbaddurl;

interface

uses Classes, IniFiles, irc, kb, Contnrs;

type
  TDbUrl = class
    rls: String;
    url: String;
    constructor Create(rls, url: string);
    destructor Destroy; override;
  end;

function dbaddurl_Process(net, chan, nick, msg: string): Boolean;
procedure dbaddurl_SaveUrl(rls, url: string);
procedure dbaddurl_addurl(params: string);
procedure dbaddurl_ParseUrl(rls, url: string);

function dbaddurl_Status: string;

procedure dbaddurlInit;
procedure dbaddurlStart;
procedure dbaddurlUnInit;

var
  last_addurl: TStringList;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit, dbaddimdb;

const
  section = 'dbaddurl';

var
  addurlcmd: string;

{ TDbUrl }
constructor TDbUrl.Create(rls, url: string);
begin
  self.rls := rls;
  self.url := url;
end;

destructor TDbUrl.Destroy;
begin
  inherited;
end;

{ Proc/Func }

function dbaddurl_Process(net, chan, nick, msg: string): Boolean;
begin
  Result := False;
  if (1 = Pos(addurlcmd, msg)) then
  begin
    msg := Copy(msg, length(addurlcmd + ' ') + 1, 1000);
    dbaddurl_addurl(msg);
    Result := True;
  end;
end;

procedure dbaddurl_addurl(params: string);
var
  rls: string;
  url: string;
  i: Integer;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  url := '';
  url := SubString(params, ' ', 2);

  if ((rls <> '') and (url <> '')) then
  begin
    i:= last_addurl.IndexOf(rls);
    if i <> -1 then
    begin
      exit;
    end;
    
    try
      dbaddurl_SaveUrl(rls, url);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddurl_addurl AddTask: %s', [e.Message]));
        exit;
      end;
    end;
  end;
end;

procedure dbaddurl_SaveUrl(rls, url: string);
var
  i: Integer;
  db_url: TDbUrl;
begin
  i:= last_addurl.IndexOf(rls);
  if i = -1 then
  begin
    db_url:= TDbUrl.Create(rls, url);
    last_addurl.AddObject(rls, db_url);

    irc_AddInfo(Format('<c7>[URL]</c> for <b>%s</b> : %s', [rls, url]));
    irc_Addtext_by_key('ADDURL', '!addurl '+rls+' '+url);

    dbaddurl_ParseUrl(rls, url);

    i:= last_addurl.Count;
    if i > 125 then
    begin
      while i > 100 do
      begin
        last_addurl.Delete(0);
        i:= last_addurl.Count - 1;
      end;
    end;
  end;
end;

procedure dbaddurl_ParseUrl(rls, url: string);
var rr: TRegexpr;
    imdb_id: String;
begin
  rr:=TRegexpr.Create;
  rr.ModifierI:=True;
  rr.Expression:='tt(\d{5,7})';
  if rr.exec(url) then
  begin
    imdb_id:='tt' + Format('%-7.7d', [StrToInt(rr.Match[1])]);
    try
      dbaddimdb_SaveImdb(rls, imdb_id);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddurl_SaveUrl dbaddimdb_SaveImdbRls: %s ', [e.Message]));
      end;
    end;
  end;
  rr.Free;
end;

{ Status }

function dbaddurl_Status: string;
begin
  Result := '';

  Result:= Format('<b>Url</b>: %d',[last_addurl.Count]);
end;

{ Init }

procedure dbaddurlInit;
begin
  last_addurl:= TStringList.Create;
  last_addurl.CaseSensitive:= False;
  last_addurl.Duplicates:= dupIgnore;
end;

procedure dbaddurlStart;
begin
  addurlcmd := config.ReadString(section, 'addurlcmd', '!addurl');
end;

procedure dbaddurlUninit;
begin
  last_addurl.Free;
end;

end.

