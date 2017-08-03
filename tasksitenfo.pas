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

procedure parseNFO(const rls, rls_section, nfo_data: AnsiString);
var
  sec: TCRelease;
  r: TRegExpr;
  imdbid: AnsiString;
begin
  sec := FindSectionHandler(rls_section);

  if sec.ClassName = 'TIMDBRelease' then
  begin
    if dbaddimdb_parseid(nfo_data, imdbid) then
      dbaddimdb_SaveImdb(rls, imdbid);
      dbaddurl_SaveUrl(rls, 'http://www.imdb.com/title/' + imdbid + '/');
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
  TryAgain;
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


  // exit if imdb info is already known in last_imdbdata
  try
    i := last_imdbdata.IndexOf(mainpazo.rls.rlsname);
    if i <> -1 then
    begin
      Result := True;
      ready := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask last_imdbdata.IndexOf: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
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
      Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask last_addnfo.IndexOf: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;


  // we don't want to use this site for NFO download (e.g. they banned our IP for download because it's a rented one)
  if s.site.UseForNFOdownload <> 1 then
  begin
    Result := True;
    ready := True;
    exit;
  end;


  // Number of errors too high. Exiting.
  TryAgain:
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
      goto TryAgain;
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
  try
    if (nfofile = '') then
    begin
      if attempt < config.readInteger(section, 'readd_attempts', 5) then
      begin
        Debug(dpSpam, section, '[iNFO]: No nfo file found for ' + mainpazo.rls.rlsname);
        try
          r := TPazoSiteNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
          r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 3));
          AddTask(r);
        except
          on e: Exception do
          begin
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
      ready := True;
      Result := True;
      exit;
    end;
  finally
    queue_lock.Leave;
  end;

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
  try
    if i <> 1 then
    begin
      if attempt < config.readInteger(section, 'readd_attempts', 5) then
      begin
        Debug(dpSpam, section, '[iNFO]: Nfo file could not be downloaded for ' + mainpazo.rls.rlsname);

        try
          r := TPazoSiteNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
          r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 3));
          AddTask(r);
        except
          on e: Exception do
          begin
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
      ready := True;
      Result := True;
      exit;
    end;
  finally
    queue_lock.Leave;
  end;

  // nfo file was downloaded. Parsing it and adding it to dbaddnfo
  queue_lock.Enter;
  try
    try
      parseNFO(mainpazo.rls.rlsname, mainpazo.rls.section, ss.DataString);
      dbaddnfo_SaveNfo(mainpazo.rls.rlsname, mainpazo.rls.section, nfofile, ss.DataString);
      Console_Addline('', 'NFO for ' + mainpazo.rls.rlsname + ' added from ' + s.Name);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSiteNfoTask: %s', [e.Message]));
        readyerror := True;
        exit;
      end;
    end;
  finally
    queue_lock.Leave;
  end;

  ready := True;
  Result := True;
  Debug(dpMessage, section, '<-- ' + tname);
end;

function TPazoSiteNfoTask.Name: AnsiString;
begin
  try
    Result := Format('GENRENFO: %s [pazo_id: %d] [site: %s] [attempt: %d]',[mainpazo.rls.rlsname, pazo_id, site1, attempt]);
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

