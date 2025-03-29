unit tasksitesfv;

interface

uses Classes, pazo, taskrace;

type
  TPazoSiteSfvTask = class(TPazoPlainTask)
  private
    FAttempt: Integer;
    FDir, FSFVFilename: String;
    FInitialTaskCreationTime: TDateTime;
    procedure CreateReattemptTask(const aIncrementAttempts: boolean);
    constructor Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer; const aInitialTaskCreationTime: TDateTime); overload;
  public
    constructor Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer); overload;
    function Execute(slot: Pointer): boolean; override;
    function Name: String; override;
    property Dir: String read FDir;
    property SFVFilename: String read FSFVFilename;
  end;

implementation

uses
  SysUtils, SyncObjs, StrUtils, debugunit, dateutils, queueunit, dirlist, sitesunit, irc, mystrings;

const
  section = 'sfv';

  { TPazoSiteSfvTask }
constructor TPazoSiteSfvTask.Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer);
begin
  Create(netname, channel, site, pazo, aDir, aSFVFilename, aAttempt, Now())
end;

constructor TPazoSiteSfvTask.Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer; const aInitialTaskCreationTime: TDateTime);
begin
  self.FAttempt := aAttempt;
  self.FDir := aDir;
  self.FSFVFilename := aSFVFilename;
  self.wanted_dn := True;
  self.FInitialTaskCreationTime := aInitialTaskCreationTime;
  inherited Create(netname, channel, site, '', pazo);
end;

procedure TPazoSiteSfvTask.CreateReattemptTask(const aIncrementAttempts: boolean);
var
  fSfvTask: TPazoSiteSfvTask;
  fAttempts: Integer;
begin
  fAttempts := self.FAttempt;
  if aIncrementAttempts then
  begin
    if fAttempts > 2 then
    begin
      Debug(dpSpam, section, Format('Don''t retry after %d attempts: %s', [fAttempts, Name]));
      exit;
    end;

    fAttempts := fAttempts + 1;
  end;

  if MinutesBetween(Now, self.FInitialTaskCreationTime) > 10 then
  begin
    Debug(dpSpam, section, Format('Could not download SFV after %d minutes: %s', [MinutesBetween(Now, self.FInitialTaskCreationTime), Name]));
    exit;
  end;

  fSfvTask := TPazoSiteSfvTask.Create(netname, channel, ps1.Name, mainpazo, FDir, FSFVFilename, fAttempts);
  fSfvTask.startat := IncMilliSecond(Now, 50);
  fSfvTask.FInitialTaskCreationTime := self.FInitialTaskCreationTime;
  AddTask(fSfvTask);
end;

function TPazoSiteSfvTask.Execute(slot: Pointer): boolean;
var
  fSlot: TSiteSlot;
  i: Integer;
  fDirlistEntry: TDirListEntry;
  fDirlist: TDirList;
  fStream: TStringStream;
  fRelativePath: String;
begin
  Result := False;
  fSlot := slot;
  fStream := nil;

  Debug(dpMessage, section, '--> ' + Name);

  // exit if pazo is stopped
  if mainpazo.stopped or mainpazo.ready or mainpazo.readyerror then
  begin
    Debug(dpSpam, section, 'Pazo stopped ' + Name);
    readyerror := True;
    exit;
  end;

  // SFV/NFO download disabled for this site
  if fSlot.site.UseForNFOdownload <> ufnEnabled then
  begin
    Debug(dpSpam, section, 'Site disabled for download ' + Name);
    Result := True;
    ready := True;
    exit;
  end;

  Debug(dpSpam, section, 'SFV Task start ' + Name);
  if not self.mainpazo.PazoSFV.SetSFVDownloadRunning(True) then
  begin
    Debug(dpSpam, section, 'SFV Task already running ' + Name);
    CreateReattemptTask(False);
    Result := True;
    ready := True;
    exit;
  end;

  try
    Debug(dpSpam, section, 'SFV Task download start ' + Name);
    try
      if self.mainpazo.PazoSFV.HasSFV(self.FDir) then
      begin
        Debug(dpSpam, section, 'SFV Task already has sfv ' + Name);
        Result := True;
        ready := True;
        exit;
      end;

      // Check if slot is online. If not try to relogin once.
      if fSlot.status <> ssOnline then
      begin
        if not fSlot.ReLogin(1) then
        begin
          readyerror := True;
          Debug(dpSpam, section, 'SFV Download: site status is offline.');
          exit;
        end;
      end;

      // check if the SFV is available yet on this site, else wait a bit (rescedule task)
      fDirlist := self.ps1.dirlist.FindDirlist(FDir, True);
      fDirlistEntry := fDirlist.Find(FSFVFilename);
      if (fDirlistEntry = nil) or not fDirlistEntry.IsOnSite or fDirlistEntry.IsBeingUploaded then
      begin
        Debug(dpSpam, section, 'SFV Task SFV not ready ' + Name);
        CreateReattemptTask(False);
        Result := True;
        ready := True;
        exit;
      end;

      fRelativePath := MyIncludeTrailingSlash(mainpazo.rls.rlsname);
      if fDir <> '' then
        fRelativePath := fRelativePath + MyIncludeTrailingSlash(FDir);


      Debug(dpSpam, section, 'SFV Task CWD ' + Name);
      if not fSlot.Cwd(MyIncludeTrailingSlash(ps1.maindir) + fRelativePath) then
      begin
        Debug(dpError, section, Format('Unable to CWD for SFV download on %s: %s', [self.site1, fRelativePath]));
        readyerror := True;
        exit;
      end;

      Debug(dpSpam, section, 'SFV Task leechfile start ' + Name);
      // try to get the SFV file
      fStream := TStringStream.Create('');
      i := fSlot.LeechFile(fStream, FSFVFilename);
      Debug(dpSpam, section, 'SFV Task end ' + Name);

      // SFV file could not be downloaded. Reschedule the task and exit.
      if i <> 1 then
      begin
        if i <> 0 then // LeechFile return value 0 means, currently no slot available
          Debug(dpError, section, Format('SFV download failed on %s: %s', [self.site1, fRelativePath]));
        CreateReattemptTask(i <> 0);  // LeechFile return value 0 means, currently no slot available
        readyerror := True;
        exit;
      end;

      // SFV file was downloaded. Parse
      self.mainpazo.PazoSFV.SetSFVList(FDir, ParseSFV(fStream.DataString));
      irc_SendUPDATE(Format('<c3>[SFV]</c> %s %s%s now has SFV information (%s)', [mainpazo.rls.section, fRelativePath, FSFVFilename, self.site1]));

      // remove SFV tasks for other sites
      RemovePazoSfv(pazo_id, FDir);

      Debug(dpSpam, section, 'SFV Task finished ' + Name);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSiteSfvTask: LeechFile : %s', [e.Message]));
        irc_Adderror(Format('[EXCEPTION] TPazoSiteSfvTask: LeechFile : %s', [e.Message]));
        readyerror := True;
        exit;
      end;
    end;
  finally
    self.mainpazo.PazoSFV.SetSFVDownloadRunning(False);
    if fStream <> nil then
      fStream.Free;
  end;

  ready := True;
  Result := True;
  Debug(dpMessage, section, '<-- ' + Name);
end;

function TPazoSiteSfvTask.Name: String;
begin
  try
    Result := Format('SITESFV: %s %s [pazo_id: %d] [site: %s] [attempt: %d]', [mainpazo.rls.rlsname, FDir, pazo_id, site1, FAttempt]);
  except
    Result := 'SITESFV';
  end;
end;

end.
