unit dbaddgenre;

interface

uses Classes, IniFiles, irc, kb, Contnrs;

type
  TDbGenre = class
    rls: String;
    genre: String;
    constructor Create(rls, genre: string);
    destructor Destroy; override;
  end;

function dbaddgenre_Process(net, chan, nick, msg: string): Boolean;
procedure dbaddgenre_SaveGenre(rls, genre: string);
procedure dbaddgenre_addgenre(params: string);
function dbaddgenre_ParseGenre(rls, genre: string): Boolean;

function dbaddgenre_Status: string;

procedure dbaddgenreInit;
procedure dbaddgenreStart;
procedure dbaddgenreUnInit;

var
  last_addgenre: TStringList;

implementation

uses DateUtils, SysUtils, StrUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slmasks, slhttp, debugunit, pazo;

const
  section = 'dbaddgenre';

var
  addgenrecmd: string;

{ TDbGenre }
constructor TDbGenre.Create(rls, genre: string);
begin
  self.rls := rls;
  self.genre := genre;
end;

destructor TDbGenre.Destroy;
begin
  inherited;
end;

{ Proc/Func }

function dbaddgenre_Process(net, chan, nick, msg: string): Boolean;
begin
  Result := False;
  if (1 = Pos(addgenrecmd, msg)) then
  begin
    msg := Copy(msg, length(addgenrecmd + ' ') + 1, 1000);
    dbaddgenre_addgenre(msg);
    Result := True;
  end;
end;

procedure dbaddgenre_addgenre(params: string);
var
  rls: string;
  genre: string;
  i: Integer;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  genre := '';
  genre := SubString(params, ' ', 2);

  if ((rls <> '') and (genre <> '')) then
  begin
    i:= last_addgenre.IndexOf(rls);
    if i <> -1 then
    begin
      exit;
    end;
    
    try
      dbaddgenre_SaveGenre(rls, genre);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddgenre_addgenre AddTask: %s', [e.Message]));
        exit;
      end;
    end;
  end;
end;

procedure dbaddgenre_SaveGenre(rls, genre: string);
var
  i: Integer;
  db_genre: TDbGenre;
begin
  i:= last_addgenre.IndexOf(rls);
  if i = -1 then
  begin
    if (dbaddgenre_ParseGenre(rls, genre)) then
    begin
      db_genre:= TDbGenre.Create(rls, genre);
      last_addgenre.AddObject(rls, db_genre);

      //irc_AddInfo(Format('<c7>[GENRE]</c> for <b>%s</b> : %s', [rls, genre]));
    end else begin
      exit;
    end;

    i:= last_addgenre.Count;
    if i > 75 then
    begin
      while i > 50 do
      begin
        last_addgenre.Delete(0);
        i:= last_addgenre.Count - 1;
      end;
    end;
  end;
end;

function dbaddgenre_ParseGenre(rls, genre: string): Boolean;
var p: TPazo;
    mp3genre: string;
    i: Integer;
begin
  Result:=False;
  p:= FindPazoByRls(rls);
  if (p <> nil) then
  begin
    if p.rls is TMP3Release then
    begin
      mp3genre:='';
      for i:=0 to mp3genres.Count-1 do
      begin
        if AnsiContainsText(genre, mp3genres[i]) then
        begin
          mp3genre:= mp3genres[i];
          Break;
        end;
      end;
      if (mp3genre <> '') then
      begin
        kb_add('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'), p.rls.section, mp3genre, 'UPDATE', p.rls.rlsname, '');
        Result:=True;
      end;
    end;
  end;
end;

{ Status }

function dbaddgenre_Status: string;
begin
  Result:='';

  Result:= Format('<b>Genre</b>: %d',[last_addgenre.Count]);
end;

{ Init }

procedure dbaddgenreInit;
begin
  last_addgenre:= TStringList.Create;
  last_addgenre.CaseSensitive:= False;
  last_addgenre.Duplicates:= dupIgnore;
end;

procedure dbaddgenreStart;
begin
  addgenrecmd := config.ReadString(section, 'addgenrecmd', '!gn');
end;

procedure dbaddgenreUninit;
begin
  last_addgenre.Free;
end;

end.

