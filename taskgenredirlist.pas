unit taskgenredirlist;

interface

uses pazo, taskrace;

type
  TPazoGenreDirlistTask = class(TPazoPlainTask)
  private
    attempt: Integer;
    function FetchGenre(filename: string): string;
  public
    constructor Create(const netname, channel: string;site: string; pazo: TPazo; attempt: Integer);
    function Execute(slot: Pointer): Boolean; override;
    function Name: string; override;
  end;

implementation

uses SysUtils, StrUtils, kb, debugunit, dateutils, queueunit, tags, configunit, tasksunit, dirlist, mystrings, sitesunit, irc;

const
  section = 'taskgenredirlist';

{ TPazoGenreDirlistTask }

constructor TPazoGenreDirlistTask.Create(const netname, channel: string;site: string; pazo: TPazo; attempt: Integer);
begin
  self.attempt:= attempt;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoGenreDirlistTask.FetchGenre(filename: string): string;
var i: Integer; s:string;
begin
  Result:= '';
  try
    s:= Csere(filename, '-', ' ');
    Irc_addtext(Netname,Channel,'Checking: %s (%s)',[filename,s]);
    for i:= 0 to mp3genres.Count-1 do
    begin
      if AnsiContainsText(filename, mp3genres[i]) then
      begin
        Result:= mp3genres[i];
        Break;
      end;
    end;
    except
    on e: Exception do
    result := '';
  end;
end;

function TPazoGenreDirlistTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    j: Integer;
    r: TPazoGenreDirlistTask;
    d: TDirList;
    event, tagfile, genre: string;
begin
  Result:= False;
  s:= slot;

  if mainpazo.stopped then
  begin
    readyerror:= True;
    exit;
  end;

  Debug(dpMessage, section, Name);

  if (mainpazo.rls is TMP3Release) then
  begin
    if (TMP3Release(mainpazo.rls).mp3genre <> '') then
    begin
      Result:= True;
      ready:= True;
      exit;
    end;
  end; // else mas nem nagyon lehet...


ujra:
  if s.status <> ssOnline then
  begin
    if not s.ReLogin then
    begin
      readyerror:= True;
      exit;
    end;
  end;

  try
    if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir)+ MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
    begin
      readyerror:= True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  j:= 0;
  tagfile:= '';
  try
    try
      d:= TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
      j:= TagComplete(d.complet_tag);
      if j <> 0 then
        tagfile:= d.complet_tag;
    finally
      d.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  try
    genre:= FetchGenre(tagfile);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  if ((j = 0) or (genre = '')) then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
//      Debug(dpSpam, section, 'READD: nincs meg a complete tag vagy nincs meg a genre...');

      Debug(dpSpam, section, 'READD: No member or the complete lack of genre...');
      try
        r:= TPazoGenreDirlistTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
        r.startat:= IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoGenreDirlistTask AddTask %s', [e.Message]));
          readyerror:= True;
          exit;
        end;
      end;
    end else
      Debug(dpSpam, section, 'READD: There is no longer read...');
  end else
  begin
    if j = -1 then event:= 'NEWDIR' else event:= 'COMPLETE';
    try
      kb_add(netname, channel,ps1.name, mainpazo.rls.section, genre, 'UPDATE', mainpazo.rls.rlsname, '');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask kb_add %s', [e.Message]));
        irc_Adderror(Format('<c4>[Exception]</c> in TPazoGenreDirlistTask kb_add %s', [e.Message]));
        readyerror:= True;
        exit;
      end;
    end;
  end;

  Result:= True;
  ready:= True;
end;

function TPazoGenreDirlistTask.Name: string;
begin
  try
    Result:= Format('GENREDIRLIST: %s (Count:%d)',[mainpazo.rls.rlsname,attempt]);
  except
    Result:= 'GENREDIRLIST';
  end;
end;

end.
