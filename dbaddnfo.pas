unit dbaddnfo;

interface

uses Classes, IniFiles, irc, kb, Contnrs, encinifile;

type
  TDbNfo = class
    rls:      AnsiString;
    nfo_name: AnsiString;
    constructor Create(rls, nfo_name: AnsiString);
    destructor Destroy; override;
  end;

function dbaddnfo_Process(net, chan, nick, msg: AnsiString): boolean;
procedure dbaddnfo_SaveNfo(rls, nfo_name, nfo_data: AnsiString); overload;
procedure dbaddnfo_SaveNfo(rls, section, nfo_name, nfo_data: AnsiString); overload;
procedure dbaddnfo_addnfo(params: AnsiString);
procedure dbaddnfo_ParseNfo(const rls, nfo_data: AnsiString); overload;
procedure dbaddnfo_ParseNfo(const rls, section, nfo_data: AnsiString); overload;

function dbaddnfo_Status: AnsiString;

procedure dbaddnfoInit;
procedure dbaddnfoStart;
procedure dbaddnfoUnInit;

var
  last_addnfo: TStringList;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit, taskhttpnfo,
  dbaddurl, dbaddimdb, pazo;

const
  section = 'dbaddnfo';

var
  addnfocmd: AnsiString;
  oldnfocmd: AnsiString;

{ TDbNfo }
constructor TDbNfo.Create(rls, nfo_name: AnsiString);
begin
  self.rls      := rls;
  self.nfo_name := nfo_name;
end;

destructor TDbNfo.Destroy;
begin
  inherited;
end;

{ Proc/Func }

function dbaddnfo_Process(net, chan, nick, msg: AnsiString): boolean;
begin
  Result := False;
  if (1 = Pos(addnfocmd, msg)) then
  begin
    msg := Copy(msg, length(addnfocmd + ' ') + 1, 1000);
    dbaddnfo_addnfo(msg);
    Result := True;
  end;
  if (1 = Pos(oldnfocmd, msg)) then
  begin
    msg := Copy(msg, length(oldnfocmd + ' ') + 1, 1000);
    dbaddnfo_addnfo(msg);
    Result := True;
  end;
end;

procedure dbaddnfo_addnfo(params: AnsiString);
var
  rls: AnsiString;
  nfo_url: AnsiString;
  nfo_name: AnsiString;
  i: integer;
begin
  rls      := '';
  rls      := SubString(params, ' ', 1);
  nfo_url  := '';
  nfo_url  := SubString(params, ' ', 2);
  nfo_name := '';
  nfo_name := SubString(params, ' ', 3);

  if ((rls <> '') and (nfo_url <> '') and (nfo_name <> '')) then
  begin
    i := last_addnfo.IndexOf(rls);
    if i <> -1 then
    begin
      exit;
    end;

    try
      AddTask(TPazoHTTPNfoTask.Create(rls, nfo_url, nfo_name));
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddnfo_addnfo AddTask: %s',
          [e.Message]));
        exit;
      end;
    end;
  end;
end;


procedure dbaddnfo_SaveNfo(rls, section, nfo_name, nfo_data: AnsiString); overload;
var
  i:      integer;
  db_nfo: TDbNfo;
begin
  i := last_addnfo.IndexOf(rls);
  if i = -1 then
  begin
    db_nfo := TDbNfo.Create(rls, nfo_name);
    last_addnfo.AddObject(rls, db_nfo);

    irc_AddInfo(Format('<c7>[NFO]</c> for <b>%s</b> : %s', [rls, nfo_name]));

    dbaddnfo_ParseNfo(rls, section, nfo_data);

    // clean old db entries
    last_addnfo.BeginUpdate;
    try
      i := last_addnfo.Count;
      if i > 125 then
      begin
        while i > 100 do
        begin
          last_addnfo.Delete(0);
          i := last_addnfo.Count - 1;
        end;
      end;
    finally
      last_addnfo.EndUpdate;
    end;

  end;
end;

procedure dbaddnfo_SaveNfo(rls, nfo_name, nfo_data: AnsiString); overload;
var
  i:      integer;
  db_nfo: TDbNfo;
begin
  i := last_addnfo.IndexOf(rls);
  if i = -1 then
  begin
    db_nfo := TDbNfo.Create(rls, nfo_name);
    last_addnfo.AddObject(rls, db_nfo);

    irc_AddInfo(Format('<c7>[NFO]</c> for <b>%s</b> : %s', [rls, nfo_name]));

    dbaddnfo_ParseNfo(rls, nfo_data);

    last_addnfo.BeginUpdate;
    try
      i := last_addnfo.Count;
      if i > 125 then
      begin
        while i > 100 do
        begin
          last_addnfo.Delete(0);
          i := last_addnfo.Count - 1;
        end;
      end;
    finally
      last_addnfo.EndUpdate;
    end;
  end;
end;


procedure dbaddnfo_ParseNfo(const rls, section, nfo_data: AnsiString); overload;
var
  sec: TCRelease;
  imdbid: AnsiString;
begin
  sec := FindSectionHandler(section);

  if sec.ClassName = 'TIMDBRelease' then
  begin
    if dbaddimdb_parseid(nfo_data, imdbid) then
      dbaddurl_SaveUrl(rls, 'http://www.imdb.com/title/' + imdbid + '/');
    exit;
  end;

  // do further parsing if section not related to IMDB
  dbaddnfo_ParseNfo(rls, nfo_data);
end;

procedure dbaddnfo_ParseNfo(const rls, nfo_data: AnsiString); overload;
var
  URLTemplate: AnsiString;
  url: AnsiString;
begin
  // Search URL
  URLTemplate :=
    '' + '(' + '([fF][tT][pP]|[hH][tT][tT][pP])://'                  // Protocol
    + '|[wW]{3}\.)'
    // trick to catch links without
    // protocol - by detecting of starting 'www.'
    + '([\w\d\-]+(\.[\w\d\-]+)+)'                           // TCP addr or domain name
    + '(:\d\d?\d?\d?\d?)?'                                  // port number
    + '(((/[%+\w\d\-\\\.]*)+)*)'                            // unix path
    + '(\?[^\s=&]+=[^\s=&]+(&[^\s=&]+=[^\s=&]+)*)?'         // request (GET) params
    + '(#[\w\d\-%+]+)?';                                    // bookmark

  with TRegExpr.Create do
    try
      Expression := URLTemplate;
      if Exec(nfo_data) then
        repeat
          if (CompareText(Match[1], 'www.') = 0) then
          begin
            url := 'http://' + Match[0];
          end
          else
          begin
            url := Match[0];
          end;

          dbaddurl_SaveUrl(rls, url);
        until not ExecNext;
    finally
      Free;
    end;
end;

{ Status }
function dbaddnfo_Status: AnsiString;
begin
  Result := Format('<b>Nfo</b>: %d', [last_addnfo.Count]);
end;

{ Init }

procedure dbaddnfoInit;
begin
  last_addnfo := TStringList.Create;
  last_addnfo.CaseSensitive := False;
  last_addnfo.Duplicates := dupIgnore;
end;

procedure dbaddnfoStart;
begin
  addnfocmd := config.ReadString(section, 'addnfocmd', '!addnfo');
  oldnfocmd := config.ReadString(section, 'oldnfocmd', '!oldnfo');

end;

procedure dbaddnfoUninit;
begin
  last_addnfo.Free;
end;

end.

