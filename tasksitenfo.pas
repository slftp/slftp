unit tasksitenfo;

interface

uses Classes, pazo, taskrace, sltcp;

type
  TPazoSiteNfoTask = class(TPazoPlainTask)
  private
    ss: TStringStream;
    attempt: Integer;
  public
    constructor Create(const netname, channel: AnsiString; site: AnsiString; pazo: TPazo; attempt: Integer);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: AnsiString; override;
  end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags, console, regexpr, dbaddimdb,
  configunit, tasksunit, dirlist, mystrings, sitesunit, dbaddnfo, dbaddurl;

const
  section = 'tasksitenfo';

procedure parseNFO(rls,rls_section,nfo_data: AnsiString);
var
  sec: TCRelease;
  r: TRegExpr;
begin
  sec := FindSectionHandler(rls_section);
  r := TRegExpr.Create;
  try
    if sec.ClassName = 'TIMDBRelease' then
    begin
      r.Expression := 'tt\d{5,7}';
      if r.Exec(nfo_data) then
      dbaddimdb_SaveImdb(rls,r.Match[0]);
      dbaddurl_SaveUrl(rls, 'http://www.imdb.com/title/' + r.Match[0] + '/');
    end;
  finally
    r.Free;
  end;

end;

{ TPazoSiteNfoTask }

constructor TPazoSiteNfoTask.Create(const netname, channel: AnsiString; site: AnsiString; pazo: TPazo; attempt: Integer);
begin
  ss := TStringStream.Create('');
  self.attempt := attempt;
  self.wanted_dn := True;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoSiteNfoTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  i: Integer;
  de: TDirListEntry;
  r: TPazoSiteNfoTask;
  d: TDirList;
  numerrors: Integer;
  tname, nfofile: AnsiString;
begin
  Result := False;
  s := slot;
  numerrors := 0;
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
    i:= last_addnfo.IndexOf(mainpazo.rls.rlsname);
    if i <> -1 then
    begin
      Result:= True;
      ready:= True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask last_addnfo.IndexOf: %s', [e.Message]));
      readyerror:= True;
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
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask.Execute error: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
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
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask TDirlist : %s', [e.Message]));
      readyerror := true;
      exit;
    end;
  end;

  // no nfo file found. Reschedule the task and exit.
  queue_lock.Enter;
  if (nfofile = '') then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, '[iNFO]: No nfo file found for ' + mainpazo.rls.rlsname);
      try
        r := TPazoSiteNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
        r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
        AddTask(r);
      except
        on e: Exception do
        begin
          queue_lock.Leave;
          Debug(dpError, section, Format('[Exception] in TPazoSiteNfoTask AddTask %s', [e.Message]));
          readyerror := True;
          exit;
        end;
      end;
    end
    else
    begin
      Debug(dpSpam, section, 'FAIL: Maximum readd attempts reached.');
    end;
    queue_lock.Leave;
    ready := True;
    Result := True;
    exit;
  end;
  queue_lock.Leave;

  // try to get the nfo file
  try
    i := s.LeechFile(ss, nfofile);

  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask: LeechFile : %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;
  
  // nfo file could not be downloaded. Reschedule the task and exit.
  queue_lock.Enter;
  if i <> 1 then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, '[iNFO]: Nfo file could not be downloaded for ' + mainpazo.rls.rlsname);

      try
        r := TPazoSiteNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
        r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
        AddTask(r);
      except
        on e: Exception do
        begin
          queue_lock.Leave;
          Debug(dpError, section, Format('[Exception] in TPazoSiteNfoTask AddTask %s', [e.Message]));
          readyerror := True;
          exit;
        end;
      end;
    end
    else
    begin
      Debug(dpSpam, section, 'FAIL: Maximum readd attempts reached.');

    end;
    queue_lock.Leave;
    ready := True;
    Result := True;
    exit;
  end;
  queue_lock.Leave;

  // nfo file was downloaded. Parsing it and adding it to dbaddnfo
  queue_lock.Enter;
  try
    parseNFO(mainpazo.rls.rlsname, mainpazo.rls.section, ss.DataString);
    dbaddnfo_SaveNfo(mainpazo.rls.rlsname, mainpazo.rls.section, nfofile, ss.DataString);
    Console_Addline('', 'NFO for '+mainpazo.rls.rlsname+' added from '+s.Name);
  except
    on e: Exception do
    begin
      queue_lock.Leave;
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;
  queue_lock.Leave;

  ready := True;
  Result := True;
  Debug(dpMessage, section, '<-- ' + tname);
end;

function TPazoSiteNfoTask.Name: AnsiString;
begin
  try
    Result := Format('GENRENFO: %s [pazo_id: %d] [site: %s] [attempt: %d]',[mainpazo.rls.rlsname, IntToStr(pazo_id), site1, attempt]);
  except
    Result := 'SITENFO';
  end;
end;

destructor TPazoSiteNfoTask.Destroy;
begin
  ss.Free;
  inherited;
end;

end.
