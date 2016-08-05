unit dbaddgenre;

interface

uses Classes, IniFiles, irc, kb, Contnrs;

type
  TDbGenre = class
    rls: AnsiString;
    genre: AnsiString;
    constructor Create(rls, genre: AnsiString);
    destructor Destroy; override;
  end;

function dbaddgenre_Process(net, chan, nick, msg: AnsiString): Boolean;
procedure dbaddgenre_SaveGenre(rls, genre: AnsiString);
procedure dbaddgenre_addgenre(params: AnsiString);
function dbaddgenre_ParseGenre(rls, genre: AnsiString): Boolean;

function dbaddgenre_Status: AnsiString;

procedure dbaddgenreInit;
procedure dbaddgenreStart;
procedure dbaddgenreUnInit;

var
  last_addgenre: TStringList;

implementation

uses SysUtils, StrUtils, configunit, mystrings, irccommandsunit,
  sitesunit, queueunit, debugunit, pazo {$IFDEF MSWINDOWS},Windows{$ENDIF}
  ;

const
  section = 'dbaddgenre';

var
  addgenrecmd: AnsiString;

{ TDbGenre }
constructor TDbGenre.Create(rls, genre: AnsiString);
begin
  self.rls := rls;
  self.genre := genre;
end;

destructor TDbGenre.Destroy;
begin
  inherited;
end;

{ Proc/Func }

function dbaddgenre_Process(net, chan, nick, msg: AnsiString): Boolean;
begin
  Result := False;
  if (1 = Pos(addgenrecmd, msg)) then
  begin
    msg := Copy(msg, length(addgenrecmd + ' ') + 1, 1000);
    dbaddgenre_addgenre(msg);
    Result := True;
  end;
end;

procedure dbaddgenre_addgenre(params: AnsiString);
var
  rls: AnsiString;
  genre: AnsiString;
  i: Integer;
begin
  if (Count(' ', params) > 1) then begin
    irc_AddInfo(Format('<c7>[GENRE]</c> rejected <b>%s</b> because it contains more than 2 parameters', [params]));
    exit;
  end;

  rls := '';
  rls := SubString(params, ' ', 1);
  genre := '';
  genre := StringReplace(SubString(params, ' ', 2), '_', ' ', [rfReplaceAll]);

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

procedure dbaddgenre_SaveGenre(rls, genre: AnsiString);
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
    end else begin
      exit;
    end;

    last_addgenre.BeginUpdate;
    try
      i:= last_addgenre.Count;
      if i > 75 then begin
        while i > 50 do  begin
          last_addgenre.Delete(0);
          i:= last_addgenre.Count - 1;
        end;
      end;
    finally
      last_addgenre.EndUpdate;
    end;
  end;
end;

function dbaddgenre_ParseGenre(rls, genre: AnsiString): Boolean;
var p: TPazo;
    mp3genre: AnsiString;
    ss: AnsiString;
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
        if (0 = AnsiCompareText(genre, mp3genres[i])) then
        begin
          mp3genre:= mp3genres[i];
          if i > 0 then
          begin
            ss:= Csere(mp3genres[i-1], ' ', '');
            if (0 = AnsiCompareText(ss, mp3genre)) then
            begin
              mp3genre:= mp3genres[i-1];
            end;
          end;
          Break;
        end;
      end;
      if (mp3genre <> '') then
      begin
        kb_add('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'), p.rls.section, mp3genre, 'UPDATE', p.rls.rlsname, '');
        irc_AddInfo(Format('<c7>[GENRE]</c> for <b>%s</b> : %s', [rls, mp3genre]));
        Result:=True;
      end;
    end;
  end;
end;

{ Status }

function dbaddgenre_Status: AnsiString;
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

