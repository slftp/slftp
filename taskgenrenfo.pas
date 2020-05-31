unit taskgenrenfo;

interface

uses Classes, pazo, taskrace, sltcp;

type
  TPazoGenreNfoTask = class(TPazoPlainTask)
  private
    ss: TStringStream;
    attempt: Integer;
    function FetchGenre(const text: String): String;
  public
    constructor Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SysUtils, irc, StrUtils, kb, kb.release, debugunit, dateutils, queueunit, tags,
  console, configunit, tasksunit, dirlist, sitesunit, dbaddnfo, mystrings;

const
  section = 'taskgenrenfo';

{ TPazoGenreNfoTask }

constructor TPazoGenreNfoTask.Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
begin
  ss := TStringStream.Create('');
  self.attempt := attempt;
  self.wanted_dn := True;
  inherited Create(netname, channel, site, '', pazo);
end;

destructor TPazoGenreNfoTask.Destroy;
begin
  ss.Free;
  inherited;
end;

function TPazoGenreNfoTask.FetchGenre(const text: String): String;
var
  i: Integer;
  s: String;
begin
  Result := '';
  i := Pos('genre', LowerCase(text));
  if i = 0 then exit;

  Result := Copy(text, i + 5, 100);
  for i := 1 to length(Result) do
  begin
    if Result[i] in [#13,#10] then
    begin
      Result := Copy(Result, 1, i-1);
      Break;
    end;
    if (not (Result[i] in ['a'..'z','A'..'Z'])) then
      Result[i] := ' ';
  end;

  while(true) do
  begin
    s:= ReplaceText(Result, '  ', ' ');
    if s = Result then Break;
    Result := s;
  end;

  Result := Trim(Result);
end;

function TPazoGenreNfoTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  i: Integer;
  s: TSiteSlot;
  de: TDirListEntry;
  r: TPazoGenreNfoTask;
  d: TDirList;
  tname, nfofile, genre: String;
  numerrors: Integer;
begin
  Result := False;
  numerrors := 0;
  s := slot;
  tname := Name;

  Debug(dpMessage, section, '--> ' + tname);

  // exit if pazo is stopped
  if mainpazo.stopped then
  begin
    readyerror := True;
    exit;
  end;

  // exit if nfo is already in dbaddnfo
  try
    i := last_addnfo.IndexOf(mainpazo.rls.rlsname);
    if i <> -1 then
    begin
      Result := True;
      ready := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoGenreNfoTask last_addnfo.IndexOf: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;

  // Number of errors too high. Exiting.
  ujra:
  try
    inc(numerrors);
    if numerrors > 3 then
    begin
      irc_Adderror(Format('<c4>[ERROR]</c> %s', [name]));
      mainpazo.errorreason := 'Protocol errors on ' + site1;
      readyerror := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoGenreNfoTask.Execute error: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;

  // no idea what it really does
  queue_lock.Enter;
  try
    if (mainpazo.rls is TNFORelease) then
    begin
      if (TNFORelease(mainpazo.rls).nfogenre <> '') then
      begin
        Result := True;
        ready := True;
        exit;
      end;
    end;
  finally
    queue_lock.Leave;
  end;

  // Check if slot is online. If not try to relogin once.
  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1) then
    begin
      readyerror := True;
      Debug(dpSpam, section, 'site status is offline.');
      exit;
    end;
  end;

  // Trying to list files in release directory
  if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
  begin
    Debug(dpSpam, section, 'Dirlist Failed.');
    if s.status = ssDown then
      goto ujra;
    readyerror := True;
    exit;
  end;

  // Trying to look for a nfo file in the dirlist
  nfofile := '';
  try
    d := TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
    try
      de := d.FindNfo;
      if (de <> nil) then
        nfofile := de.filename;
    finally
      d.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoGenreNfoTask TDirlist : %s', [e.Message]));
      readyerror := true;
      exit;
    end;
  end;

  // no nfo file found. Reschedule the task and exit.
  queue_lock.Enter;
  try
    if (nfofile = '') then
    begin
      if attempt < config.readInteger(section, 'readd_attempts', 5) then
      begin
        Debug(dpSpam, section, '[iNFO]: No nfo file found for ' + mainpazo.rls.rlsname);

        r := TPazoGenreNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
        r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
        try
          AddTask(r);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[Exception] in TPazoGenreNfoTask AddTask %s', [e.Message]));
            readyerror := True;
            exit;
          end;
        end;
      end else
      begin
        Debug(dpSpam, section, 'FAIL: Maximum readd attempts reached.');
      end;

      ready := True;
      Result := True;
      exit;
    end;
  finally
    queue_lock.Leave;
  end;

  // try to get the nfo file
  i := s.LeechFile(ss, nfofile);

  // nfo file could not be downloaded. Reschedule the task and exit.
  queue_lock.Enter;
  try
    if i <> 1 then
    begin
      if attempt < config.readInteger(section, 'readd_attempts', 5) then
      begin
        Debug(dpSpam, section, '[iNFO]: Nfo file could not be downloaded for ' + mainpazo.rls.rlsname);
        try
          r := TPazoGenreNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
          r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
          AddTask(r);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[Exception] in TPazoGenreNfoTask AddTask %s', [e.Message]));
            readyerror := True;
            exit;
          end;
        end;
      end
      else
      begin
        Debug(dpSpam, section, 'FAIL: Maximum readd attempts reached.');

      end;
      ready := True;
      Result := True;
      exit;
    end;
  finally
    queue_lock.Leave;
  end;

  // nfo file was downloaded. Parsing it and adding it to dbaddnfo
  genre := FetchGenre(ss.DataString);

  queue_lock.Enter;
  try
    try
      kb_add(netname, channel, ps1.name, mainpazo.rls.section, genre, kbeUPDATE, mainpazo.rls.rlsname, '');
      dbaddnfo_SaveNfo(mainpazo.rls.rlsname, mainpazo.rls.section, nfofile, ss.DataString);
      Console_Addline('', 'NFO for '+mainpazo.rls.rlsname+' added from '+s.Name);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[Exception] in TPazoGenreNfoTask kb_add %s', [e.Message]));
        readyerror := True;
        exit;
      end;
    end;
  finally
    queue_lock.Leave;
  end;

  Result := True;
  ready := True;
  Debug(dpMessage, section, '<-- ' + tname);
end;

function TPazoGenreNfoTask.Name: String;
begin
  try
    Result := Format('GENRENFO: %s [pazo_id: %d] [site: %s] [attempt: %d]',[mainpazo.rls.rlsname, pazo_id, site1, attempt]);
  except
    Result := 'GENRENFO';
  end;
end;

end.
