unit dbaddurl;

interface

uses Classes, IniFiles, irc, kb, Contnrs;

type
  TDbUrl = class
    rls: AnsiString;
    url: AnsiString;
    constructor Create(const rls, url: AnsiString);
    destructor Destroy; override;
  end;

function dbaddurl_Process(net, chan, nick, msg: AnsiString): Boolean;
procedure dbaddurl_SaveUrl(const rls, url: AnsiString);
procedure dbaddurl_addurl(const params: AnsiString);
procedure dbaddurl_ParseUrl(const rls, url: AnsiString);

function dbaddurl_Status: AnsiString;

procedure dbaddurlInit;
procedure dbaddurlStart;
procedure dbaddurlUnInit;

var
  last_addurl: TStringList;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slmasks, slhttp, debugunit, dbaddimdb;

const
  section = 'dbaddurl';

var
  addurlcmd: AnsiString;

{ TDbUrl }

constructor TDbUrl.Create(const rls, url: AnsiString);
begin
  self.rls := rls;
  self.url := url;
end;

destructor TDbUrl.Destroy;
begin
  inherited;
end;

{ Proc/Func }

function dbaddurl_Process(net, chan, nick, msg: AnsiString): Boolean;
begin
  Result := False;
  if (1 = Pos(addurlcmd, msg)) then
  begin
    msg := Copy(msg, length(addurlcmd + ' ') + 1, 1000);
    dbaddurl_addurl(msg);
    Result := True;
  end;
end;

procedure dbaddurl_addurl(const params: AnsiString);
var
  rls: AnsiString;
  url: AnsiString;
  i: Integer;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  url := '';
  url := SubString(params, ' ', 2);

  if ((rls <> '') and (url <> '')) then
  begin
    i := last_addurl.IndexOf(rls);
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

procedure dbaddurl_SaveUrl(const rls, url: AnsiString);
var
  i: Integer;
  db_url: TDbUrl;
begin
  i := last_addurl.IndexOf(rls);
  if i = -1 then
  begin
    db_url := TDbUrl.Create(rls, url);
    last_addurl.AddObject(rls, db_url);

    irc_AddInfo(Format('<c7>[URL]</c> for <b>%s</b> : %s', [rls, url]));
    irc_Addtext_by_key('ADDURL', '!addurl ' + rls + ' ' + url);

    dbaddurl_ParseUrl(rls, url);

    // clean old db entries
    last_addurl.BeginUpdate;
    try
      i := last_addurl.Count;
      if i > 125 then
      begin
        while i > 100 do
        begin
          last_addurl.Delete(0);
          i := last_addurl.Count - 1;
        end;
      end;
    finally
      last_addurl.EndUpdate;
    end;

  end;
end;

procedure dbaddurl_ParseUrl(const rls, url: AnsiString);
var
  imdbid: AnsiString;
begin
  try
    if dbaddimdb_parseid(url, imdbid) then
      dbaddimdb_SaveImdb(rls, imdbid);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddurl_SaveUrl dbaddimdb_SaveImdbRls: %s ', [e.Message]));
    end;
  end;
end;

{ Status }
function dbaddurl_Status: AnsiString;
begin
  Result := Format('<b>Url</b>: %d', [last_addurl.Count]);
end;

{ Init }

procedure dbaddurlInit;
begin
  last_addurl := TStringList.Create;
  last_addurl.CaseSensitive := False;
  last_addurl.Duplicates := dupIgnore;
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

