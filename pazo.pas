unit pazo;

// EZTA Z UNITOT CSAK A QUEUE_LOCK ZARASA UTAN SZABAD HIVNI!
// THIS ONLY ON THE QUEUE_LOCK CLOSING is FREE to call!
interface

uses
  Classes, kb, SyncObjs, Contnrs, dirlist, skiplists, globals, IdThreadSafe, Generics.Collections;

type
  TQueueNotifyEvent = procedure(Sender: TObject; Value: integer) of object;

  { threadsafe integer class with notify event on Increase/Decrease }
  TIdThreadSafeInt32WithEvent = class(TIdThreadSafeInt32)
  private
    FonChange: TQueueNotifyEvent;
  public
    constructor Create; override;
    procedure Increase;
    procedure Decrease;
    property OnChange: TQueueNotifyEvent read FonChange write FonChange;
  end;

  TCacheFile = class
  private
    dir: String;
    filename: String;
    filesize: Int64;
  public
    constructor Create(const dir, filename: String; const filesize: Int64);
  end;

  TPazo = class;

  {
  @value(rssNotAllowed release is not allowed on this site)
  @value(rssNotAllowedButItsThere release is not allowed on this site but it's there)
  @value(rssAllowed release is allowed on this site)
  @value(rssShouldPre release should be pred on this site but wasn't pred on it)
  @value(rssRealPre release was a pre on this site)
  @value(rssComplete release is complete on this site)
  @value(rssNuked release got nuked on this site)
  }
  TRlsSiteStatus = (rssNotAllowed, rssNotAllowedButItsThere, rssAllowed, rssShouldPre, rssRealPre, rssComplete, rssNuked);

  TPazoSite = class
  public
    midnightdone: boolean;
    Name: String;
    maindir: String;
    pazo: TPazo;
    //sources: TObjectList;
    destinations: TObjectList;
    destinationRanks: TList<integer>;
    dirlist: TDirList;

    delay_leech: integer; //< value of delay for leeching from site in seconds
    delay_upload: integer; //< value of delay for uploading to site in seconds

    s_dirlisttasks: TIdThreadSafeInt32;
    s_racetasks: TIdThreadSafeInt32;
    s_mkdirtasks: TIdThreadSafeInt32;

    ircevent: boolean; //< returns @true if we got atleast one catchadd
    error: boolean; //< returns @true if the site went down or mkd (make directory) failed

    ts: TDateTime;
    lookupforcedhere: boolean;
    status: TRlsSiteStatus;

    reason: String;
    dirlistgaveup: boolean;

    badcrcevents: integer; //< total number of bad crc events

    firesourcesinstead: boolean;

    last_dirlist: TDateTime;
    last_race: TDateTime;

    speed_from: TStringList;

    activeTransfers: TStringList;

    function StatusRealPreOrShouldPre: boolean;  //< returns @true if its a pre or at least it should be one
    function Source: boolean;
    function Complete: boolean;

    function Age: integer;
    function AsText: String;
    function RoutesText: String;
    function DirlistGaveUpAndSentNoFiles: Boolean;
    procedure DelaySetup;

    procedure RemoveMkdir;
    procedure MarkSiteAsFailed(echomsg: boolean = False);

    function ParseDirlist(const netname, channel, dir, liststring: String; pre: boolean = False): boolean;
    function MkdirReady(const dir: String): boolean;
    function MkdirError(const dir: String): boolean;
    function AddDestination(const sitename: String; const rank: integer): boolean; overload;
    function AddDestination(ps: TPazoSite; const rank: integer): boolean; overload;
    constructor Create(pazo: TPazo; const Name, maindir: String);
    destructor Destroy; override;
    procedure ParseXdupe(const netname, channel, dir, resp: String; added: boolean = False);
    function ParseDupe(const netname, channel, dir, filename: String; byme: boolean): boolean; overload;
    function ParseDupe(const netname, channel: String; dl: TDirlist; const dir, filename: String; byme: boolean): boolean; overload;
    function SetFileError(const netname, channel, dir, filename: String): boolean; //< Sets error flag to true for filename if it cannot be transfered
    function Stats: String;
    function Allfiles: String;
    procedure SetComplete(const cdno: String);
    function StatusText: String;
    procedure Clear;
  private
    cds: String;
    function Tuzelj(const netname, channel, dir: String; de: TDirListEntry): boolean;
  end;

  TPazo = class
  private
    lastannounceconsole: String;
    lastannounceirc: String;
    lastannounceroutes: String;
    procedure QueueEvent(Sender: TObject; Value: integer);
    function StatsAllFiles: integer;
  public
    pazo_id: integer;

    stated: boolean;
    cleared: boolean;

    cs: TCriticalSection;

    completezve: boolean;

    autodirlist: boolean;
    srcsite: String;
    dstsite: String;
    rls: TRelease; //< holds the information of @link(kb.TRelease) or its descendant

    stopped: boolean;
    ready: boolean;
    readyerror: boolean;
    errorreason: String;
    readyat: TDateTime;
    lastTouch: TDateTime;

    sites: TObjectList; //< list of @link(TPazoSite) which passed checks of @link(AddSites) and are now part of this @link(TPazo)
    sl: TSkipList;

    added: TDateTime;

    //global dirlist
    main_dirlist: TDirlist;

    // Integers with locking and event
    queuenumber: TIdThreadSafeInt32WithEvent;
    dirlisttasks: TIdThreadSafeInt32;
    racetasks: TIdThreadSafeInt32;
    mkdirtasks: TIdThreadSafeInt32;

    cache_files: TStringList;

    function allfiles: integer;
    { Searches for @link(sitename) via @link(TPazo.FindSite) and calls TPazoSite.MarkSiteAsFailed if site was found
      @param(sitename Sitename which sould be set down) }
    procedure SiteDown(const sitename: String);
    procedure Clear;
    function StatusText: String;
    function Age: integer;
    function AsText: String;
    function RoutesText: String;
    function Stats(const console: boolean; withdirlist: boolean = True): String;
    function FullStats: String;
    constructor Create(rls: TRelease; const pazo_id: integer);
    destructor Destroy; override;
    function FindSite(const sitename: String): TPazoSite;
    function AddSite(const sitename, maindir: String; delay: boolean = True): TPazoSite;
    { Iterates through all @link(sitesunit.sites) and adds a @link(TPazoSite) to @link(TPazo.sites) if the site is not down, has the section, rls fits pretime, etc and sets @link(TPazoSite.status)
      @returns(@true if at least one site was added, @false otherwise) }
    function AddSites: boolean; overload;
    { Iterates through all @link(sitesunit.sites) and adds a @link(TPazoSite) to @link(TPazo.sites) if the site is not down, has the section, rls fits pretime, etc and sets @link(TPazoSite.status)
      @param(aIsSpreadJob Set it to @true if its a spread job for purpose of preeing (skips some checks), @false otherwise)
      @returns(@true if at least one site was added, @false otherwise) }
    function AddSites(const aIsSpreadJob: boolean): boolean; overload;
    function PFileSize(const dir, filename: String): Int64;
    function PRegisterFile(const dir, filename: String; const filesize: Int64): integer;
  end;

function FindPazoById(const id: integer): TPazo;
function FindPazoByName(const section, rlsname: String): TPazo;
function FindPazoByRls(const rlsname: String): TPazo;
function PazoAdd(rls: TRelease): TPazo; //; addlocal: Boolean = False
procedure PazoInit;

function FindMostCompleteSite(pazo: TPazo): TPazoSite;

implementation

uses
  SysUtils, StrUtils, mainthread, sitesunit, DateUtils, debugunit, queueunit,
  taskrace, mystrings, irc, sltcp, slhelper, Math, taskpretime, configunit,
  mrdohutils, console, RegExpr, statsunit;

const
  section = 'pazo';

var
  local_pazo_id: integer;


{ TIdThreadSafeInt32WithEvent }

constructor TIdThreadSafeInt32WithEvent.Create;
begin
  inherited;
  OnChange := nil;
end;

procedure TIdThreadSafeInt32WithEvent.Increase;
begin
  Increment;
  if (Assigned(OnChange)) then
    OnChange(self, Value);
end;

procedure TIdThreadSafeInt32WithEvent.Decrease;
begin
  Decrement;
  if (Assigned(OnChange)) then
    OnChange(self, Value);
end;

{ TCacheFile }

constructor TCacheFile.Create(const dir, filename: String; const filesize: Int64);
begin
  self.dir := dir;
  self.filename := filename;
  self.filesize := filesize;
end;

function FindMostCompleteSite(pazo: TPazo): TPazoSite;
var
  ps: TPazoSite;
  i: integer;
begin
  Result := nil;
  try
    for i := pazo.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.sites[i]);
      if ps.lookupforcedhere then
      begin
        ps.lookupforcedhere := False;
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.sites[i]);
      if ps.ts <> 0 then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.sites[i]);
      if (ps.Complete) then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.sites[i]);
      if (ps.ircevent) then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.sites[i]);
      if (ps.status in [rssAllowed, rssRealPre, rssComplete]) then
      begin
        Result := ps;
        exit;
      end;
    end;
  except
    Result := nil;
  end;
end;

function PazoAdd(rls: TRelease): TPazo; //; addlocal: Boolean = False
begin
  Result := TPazo.Create(rls, local_pazo_id);
  //  if  addlocal then
  //    pazos.Add(Result);
  Inc(local_pazo_id);
end;

function FindPazoById(const id: integer): TPazo;
var
  i: integer;
  p: TPazo;
begin
  Result := nil;
  try
    for i := kb_list.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      p := TPazo(kb_list.Objects[i]);
      if p = nil then
        exit;
      if p.pazo_id = id then
      begin
        Result := p;
        p.lastTouch := Now();
        exit;
      end;
    end;
  except
    Result := nil;
  end;
end;

function FindPazoByName(const section, rlsname: String): TPazo;
var
  i: integer;
begin
  Result := nil;
  try
    i := kb_list.IndexOf(section + '-' + rlsname);
    if i <> -1 then
    begin
      Result := TPazo(kb_list.Objects[i]);

      if Result <> nil then
        Result.lastTouch := Now();

      exit;
    end;
  except
    Result := nil;
  end;
end;

function FindPazoByRls(const rlsname: String): TPazo;
var
  i: integer;
  p: TPazo;
begin
  Result := nil;
  kb_lock.Enter;
  try
    try
      for i := kb_list.Count - 1 downto 0 do
      begin
        if i < 0 then
          Break;

        p := TPazo(kb_list.Objects[i]);

        if p = nil then
          Continue;

        if (p.rls.rlsname = rlsname) then
        begin
          Result := p;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] FindPazoByRls: %s', [e.Message]));
        Result := nil;
      end;
    end;
  finally
    kb_lock.Leave;
  end;
end;

procedure PazoInit;
begin
  local_pazo_id := 0;
end;

function TPazoSite.Tuzelj(const netname, channel, dir: String; de: TDirListEntry): boolean;
// de is TDirListEntry from sourcesite
// dstdl is TDirList on destination site
// dde is TDirListEntry on destination site
var
  i: integer;
  dst: TPazoSite;
  dstdl: TDirList;
  pm: TPazoMkdirTask;
  pr: TPazoRaceTask;
  pd: TPazoDirlistTask;
  dde: TDirListEntry;
  s: TSite;
  fd: String;
  fExtensionMatchSFV, fExtensionMatchNFO: boolean;
begin
  Result := False;
  dst := nil;
  dstdl := nil;
  dde := nil;

  // something's fucked
  if error then exit;

  if (dir <> '') then
    fd := pazo.rls.rlsname + '/' + dir
  else
    fd := pazo.rls.rlsname;

  // ignore this site if you don't have setup download slots for it
  s := FindSiteByName('', Name);
  if (status in [rssRealPre, rssShouldPre]) then
    if s.max_pre_dn = 0 then exit
  else
    if s.max_dn = 0 then exit;

  pazo.lastTouch := Now();

  // enumerate possible destinations
  for i := destinations.Count - 1 downto 0 do
  begin
    try
      if i < 0 then Break;
    except
      Break;
    end;

    // set destination
    try
      try
        dst := TPazoSite(destinations[i]);
      except
        Continue;
      end;

      if error then exit;
      if dst.error then Continue;

      // ignore this destination if we don't want to upload there
      s := FindSiteByName('', dst.Name);
      if (s.max_up = 0) then exit;

      // drop sending to this destination if too much crc events
      if (dst.badcrcevents > config.ReadInteger('taskrace', 'badcrcevents', 15)) then Continue;

      // Problem with dirlist
      if dirlist = nil then Continue;
      if dirlist.error then Continue;
      if dst.dirlist = nil then Continue;

      // Find the dirlist for destination site
      try
        dstdl := dst.dirlist.FindDirlist(dir, True);
        // When in a subdir, pass along the DirType of the source site subdir
        if (dstdl.parent <> nil) then
          dstdl.parent.DirType := de.DirType;
      except
        Continue;
      end;

      // Dirlist for destination site not available
      if dstdl = nil then Continue;
      if dstdl.error then Continue;

      // find the dirlist entry
      try
        dde := dstdl.Find(de.filename);
        // Pass along the DirType of the source site entry
        if (dde <> nil) then
          dde.DirType := de.DirType;
      except
        Continue;
      end;

      // not really sure
      (*
        if ((dde <> nil) and (dde.done)) then Continue;
      *)
      if ((dde <> nil) and (dde.megvanmeg)) then Continue;
      if ((dde <> nil) and (dde.error)) then Continue;

      // Check if mkdir is needed
      Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Checking if mkdir is needed on %s', [fd, Name, dst.Name, dst.Name]);
      if ((dstdl.entries <> nil) and (dstdl.entries.Count = 0)) then
      begin
        if ((dstdl.need_mkdir) and (dstdl.dependency_mkdir = '')) then
        begin
          Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Adding MKDIR task on %s', [fd, Name, dst.Name, dst.Name]);

          // Create the mkdir task
          pm := TPazoMkdirTask.Create(netname, channel, dst.Name, pazo, dir);

          // add delay to mkdir if delay_upload enabled
          if dst.delay_upload > 0 then pm.startat := IncSecond(Now, dst.delay_upload);

          // Add mkdir dependencies if needed
          if ((dstdl.parent <> nil) and (dstdl.parent.dirlist.dependency_mkdir <> '')) then
            pm.dependencies.Add(dstdl.parent.dirlist.dependency_mkdir);

          dstdl.dependency_mkdir := pm.UidText;

          // Finally add mkdir task
          try
            AddTask(pm);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj AddTask(pm): %s', [e.Message]));
              Break;
            end;
          end;
        end;
      end;

      // Add dirlist task if needed
      Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Checking if dirlist is needed on %s', [fd, Name, dst.Name, dst.Name]);
      if ((dst.status <> rssNotAllowed) and (not dstdl.dirlistadded) and (not dst.dirlistgaveup)) then
      begin
        try
          pd := TPazoDirlistTask.Create(netname, channel, dst.Name, pazo, dir, False);
          Debug(dpSpam, section, '%s %s :: Checking routes from %s to %s :: Dirlist added to %s (DEST SITE)', [fd, dir, Name, dst.Name, dst.Name]);
          irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[PAZO]</c> %s %s %s Dirlist added to : %s (DEST SITE)', [fd, pazo.rls.rlsname, dir, dst.Name]));
          dstdl.dirlistadded := True;
          AddTask(pd);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj AddTask(pd): %s', [e.Message]));
            Break;
          end;
        end;
      end;

      // We're handling a file
      if not de.directory then
      begin
        // destination dir is not complete
        if not dstdl.complete then
        begin
          fExtensionMatchSFV := LowerCase(de.Extension) = '.sfv';
          fExtensionMatchNFO := LowerCase(de.Extension) = '.nfo';
          // skip nfo and sfv if already there
          if ((dstdl.hassfv) and (fExtensionMatchSFV)) then
            Continue;
          if ((dstdl.hasnfo) and (fExtensionMatchNFO)) then
            Continue;

          // Check if we already have traded this same file more than we want
          if ((dde <> nil) and (dde.tradeCount > config.ReadInteger('taskrace', 'max_same_trade', 100))) then
            Continue;

          // Create the race task
          Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Adding RACE task on %s %s', [fd, Name, dst.Name, dst.Name, de.filename]);
          pr := TPazoRaceTask.Create(netname, channel, Name, dst.Name, pazo, dir, de.filename, de.filesize, destinationRanks[i]);

          // Set file type for subdirs
          if (dstdl.parent <> nil) then
            case de.DirType of
              IsSample: pr.IsSample := True;
              IsProof: pr.IsProof := True;
              IsCovers: pr.IsCovers := True;
              IsSubs: pr.IsSubs := True;
            end;

          // Set file type
          if (fExtensionMatchSFV) then
            pr.IsSfv := True;
          if (fExtensionMatchNFO) then
            pr.IsNfo := True;

          // sfv not found so we won't race this file yet
          if ((dstdl.sfv_status = dlSFVNotFound) and (not pr.IsNfo) and (not pr.IsSfv)) then
          begin
            // Sample, Proof, Covers don't usually contain nfo/sfv so we'll race those regardless
            if not (pr.IsSample or pr.IsProof or pr.IsCovers) then
            begin
              Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Not creating racetask, missing sfv on %s', [fd, Name, dst.Name, dst.Name]);
              FreeAndNil(pr);
              Continue;
            end;
          end;

          // Delay leech stuff
          if ((delay_leech > 0) or (dst.delay_upload > 0)) then
          begin
            if delay_leech > dst.delay_upload then
              pr.startat := IncSecond(Now, delay_leech)
            else
              pr.startat := IncSecond(Now, dst.delay_upload);
          end;

          // Add dependency if a dependent mkdir exists
          if dstdl.dependency_mkdir <> '' then
            pr.dependencies.Add(dstdl.dependency_mkdir);

          // finally we can add the task
          try
            AddTask(pr);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj (AddTask(pr)): %s', [e.Message]));
              Break;
            end;
          end;
        end;
        Result := True;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj (Loop): %s', [e.Message]));
        Break;
      end;
    end;
  end;
end;

{ TPazo }

// TODO: Remove it add replace all calls to AddSite with TPazoSite.Create(self, s.Name, sectiondir);
// TPazo.AddSites is used from kb_AddB, which calls TPazoSite.Create()
// Sometime before TPazoSite.AddSites called AddSite but was changed to create TPazoSite instantly!
function TPazo.AddSite(const sitename, maindir: String; delay: boolean = True): TPazoSite;
begin
  Result := TPazoSite.Create(self, sitename, maindir);
  if delay then
    Result.DelaySetup;
  sites.Add(Result);
end;

function TPazo.Age: integer;
var
  i: integer;
  ps: TPazoSite;
  a: integer;
begin
  (*
      ts: TDateTime;

    if ts <> 0 then
    begin
      Result:= SecondsBetween(Now, ts);
      exit;
    end;
  *)

  Result := -1;
  for i := sites.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    ps := TPazoSite(sites[i]);
    a := ps.Age;
    if ((a <> -1) and ((Result = -1) or (Result < a))) then
      Result := a;
  end;

  if Result = -1 then
    Result := SecondsBetween(Now, added);
end;

function TPazo.AsText: String;
var
  i: integer;
  ps: TPazoSite;
begin
  Result := rls.AsText(pazo_id);
  Result := Result + 'Age: ' + IntToStr(age) + 's' + #13#10;
  Result := Result + 'Sites: ' + IntToStr(sites.Count) + '' + #13#10;
  for i := 0 to sites.Count - 1 do
  begin
    try
      if i > sites.Count then
        Break;
    except
      Break;
    end;
    ps := TPazoSite(sites[i]);
    Result := Result + ps.AsText;
  end;
end;

function TPazo.RoutesText: String;
var
  i: integer;
  ps: TPazoSite;
begin
  Result := '<c3>[ROUTES]</c> : <b>' + rls.rlsname + '</b> (' + IntToStr(sites.Count) + ' sites)' + #13#10;
  for i := 0 to sites.Count - 1 do
  begin
    try
      ps := TPazoSite(sites[i]);
      Result := Result + ps.RoutesText;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazo.RoutesText : %s', [e.Message]));
        break;
      end;
    end;
  end;

  if (Result <> lastannounceroutes) then
  begin
    lastannounceroutes := Result;
  end
  else
  begin
    Result := '';
  end;

end;

function TPazo.PRegisterFile(const dir, filename: String; const filesize: Int64): integer;
var
  i: integer;
  cache_file: TCacheFile;
begin
  try
    cs.Enter;
    try
      i := cache_files.IndexOf(dir + '/' + filename);
      if i = -1 then
      begin
        cache_file := TCacheFile.Create(dir, filename, filesize);
        cache_files.AddObject(dir + '/' + filename, cache_file);

        Result := filesize;
      end
      else
      begin
        cache_file := TCacheFile(cache_files.Objects[i]);
        if cache_file.filesize < filesize then
        begin
          cache_file.filesize := filesize;
        end;

        Result := cache_file.filesize;
      end;
    finally
      cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.PRegisterFile: %s', [e.Message]));
      Result := filesize;
    end;
  end;
end;

function TPazo.PFileSize(const dir, filename: String): Int64;
var
  i: integer;
begin
  Result := -1;
  try
    i := cache_files.IndexOf(dir + '/' + filename);
    if i <> -1 then
      Result := TCacheFile(cache_files.Objects[i]).filesize;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.PFileSize: %s', [e.Message]));
      Result := -1;
    end;
  end;
end;

constructor TPazo.Create(rls: TRelease; const pazo_id: integer);
begin
  if rls <> nil then
  begin
    Debug(dpSpam, section, 'TPazo.Create: %s', [rls.rlsname]);
    sl := FindSkipList(rls.section);
    self.rls := rls;
  end
  else
  begin
    Debug(dpSpam, section, 'TPazo.Create: SPEEDTEST');
    self.rls := nil;
  end;

  added := Now;
  cs := TCriticalSection.Create;
  autodirlist := False;
  queuenumber := TIdThreadSafeInt32WithEvent.Create;
  queuenumber.OnChange := QueueEvent;
  dirlisttasks := TIdThreadSafeInt32.Create;
  racetasks := TIdThreadSafeInt32.Create;
  mkdirtasks := TIdThreadSafeInt32.Create;
  main_dirlist := nil;

  readyerror := False;
  sites := TObjectList.Create(False);
  self.pazo_id := pazo_id;
  stopped := False;
  ready := False;
  readyat := 0;
  lastTouch := Now();
  cache_files := TStringList.Create;
  cache_files.CaseSensitive := False;
  cache_files.Duplicates := dupIgnore;

  self.stated := False;
  self.cleared := False;

  inherited Create;
end;

destructor TPazo.Destroy;
begin
  Debug(dpSpam, section, 'TPazo.Destroy: %s', [rls.rlsname]);
  sites.Free;
  queuenumber.Free;
  dirlisttasks.Free;
  racetasks.Free;
  mkdirtasks.Free;
  cache_files.Free;
  FreeAndNil(rls);
  cs.Free;
  inherited;
end;

function TPazo.FindSite(const sitename: String): TPazoSite;
var
  i: integer;
begin
  Result := nil;
  try
    for i := sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      if TPazoSite(sites[i]).Name = sitename then
      begin
        Result := TPazoSite(sites[i]);
        Break;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.FindSite: %s', [e.Message]));
      Result := nil;
    end;
  end;
end;

procedure TPazo.QueueEvent(Sender: TObject; Value: integer);
var
  s: String;
begin
  if Value < 0 then
    Value := 0;

  if (Value <> 0) then
  begin
    ready := False;
    readyerror := False;
  end
  else if Value = 0 then
  begin
    readyat := Now();
    ready := True;

    if ((not slshutdown) and (rls <> nil)) then
    begin
      Debug(dpSpam, section, 'Number of pazo tasks is now zero! ' + IntToStr(pazo_id));

      if not stopped then
      begin
        // display race stats on console
        s := Stats(True, False);
        if ((lastannounceconsole <> s) and (s <> '')) then
        begin
          irc_addtext('CONSOLE', 'Stats', rls.section + ' ' + rls.rlsname + ' (' + IntToStr(StatsAllFiles) + '): ' + s);
          lastannounceconsole := s;
        end;

        // we don't want to display this section race stats on irc
        if noannouncesections.IndexOf(rls.section) <> -1 then
          exit;

        // display race stats on irc
        s := Stats(False, False);
        if ((lastannounceirc <> s) and (s <> '')) then
        begin
          irc_addstats('<c10>[<b>STATS</b>]</c> ' + rls.section + ' ' + rls.rlsname + ' (' + IntToStr(StatsAllFiles) + '):');
          irc_AddstatsB(Stats(False, True));
          lastannounceirc := s;
        end;
      end
      else
      begin
        irc_addstats('<c10>[<b>STATS</b>]</c> Pazo Stopped.');
      end;
    end;
  end;
end;

function TPazo.StatsAllFiles: integer;
var
  i, j: integer;
begin
  Result := 0;
  if main_dirlist = nil then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      j := TPazoSite(sites[i]).dirlist.Done;
      if Result < j then
        Result := j;
    end;
  end
  else
    Result := main_dirlist.Done;

end;

function CompareCompleteTimes(pazo1, pazo2: TPazoSite): Integer;
begin
  if (pazo1.StatusRealPreOrShouldPre and pazo2.StatusRealPreOrShouldPre) then
    Result := CompareText(pazo1.Name, pazo2.name)
  else if (pazo1.StatusRealPreOrShouldPre) then
    Result := -1
  else if (pazo2.StatusRealPreOrShouldPre) then
    Result := 1
  else
  begin
    if (((pazo1.status = rssNotAllowed) and (pazo2.status = rssNotAllowed)) or ((pazo1.DirlistGaveUpAndSentNoFiles) and pazo2.DirlistGaveUpAndSentNoFiles)) then
        Result := CompareText(pazo1.Name, pazo2.name)
    else if ((pazo1.status = rssNotAllowed) or (pazo1.DirlistGaveUpAndSentNoFiles)) then
        Result := 1
    else if ((pazo2.status = rssNotAllowed) or (pazo2.DirlistGaveUpAndSentNoFiles)) then
        Result := -1
    else if ((pazo1.dirlist <> nil) and (pazo2.dirlist <> nil)) then
      begin
        if ((pazo1.dirlist.date_completed = 0) and (pazo2.dirlist.date_completed = 0)) then
          Result := CompareText(pazo1.Name, pazo2.name)
        else if (pazo1.dirlist.date_completed = 0) then
          Result := 1
        else if (pazo2.dirlist.date_completed = 0) then
          Result := -1
        else
          Result := CompareDateTime(pazo1.dirlist.date_completed, pazo2.dirlist.date_completed);
      end
    else
      Result := 0;
  end;
end;

function TPazo.Stats(const console: boolean; withdirlist: boolean = True): String;
var
  i, numComplete: integer;
  ps: TPazoSite;
  s: TSite;
  sitesSorted: TObjectList;
  completeTimeReference: TDateTime;
  secondsAfter: Int64;
begin
  Result := '';
  numComplete := 1;
  completeTimeReference := 0;

  sitesSorted := TObjectList.Create(False);
  try
    sitesSorted.Assign(sites);
    sitesSorted.Sort(@CompareCompleteTimes);

    for i := 0 to sitesSorted.Count - 1 do
    begin
      try
        ps := TPazoSite(sitesSorted[i]);
        if ps.status = rssNotAllowed then
          Continue;
        s := FindSiteByName('', ps.Name);
        if s = nil then
          Continue;
        if s.noannounce and not console then
          Continue;

        if Result <> '' then
          Result := Result + ', ';

        if ((ps.status in [rssRealPre, rssShouldPre, rssNotAllowed]) or ps.DirlistGaveUpAndSentNoFiles or (ps.dirlist.date_completed = 0)) then
          Result := Result + '"' + ps.Stats + '"'
        else
        begin
          if CompleteTimeReference = 0 then
          begin
            Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, '"');
            completeTimeReference := ps.dirlist.date_completed;
          end
          else
          begin
            secondsAfter := SecondsBetween(completeTimeReference, ps.dirlist.date_completed);
            if secondsAfter <> 0 then
              Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, ' (+', IntToStr(secondsAfter), 's)"')
            else
              Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, '"');
          end;
          inc(numComplete);
        end;
      except
        Continue;
      end;
    end;
  finally
    sitesSorted.Free;
  end;

end;

function TPazo.FullStats: String;
var
  i: integer;
  ps: TPazoSite;
  //    mysources: TObjectList;
begin
  Result := '';

  for i := 0 to sites.Count - 1 do
  begin
    ps := TPazoSite(sites[i]);
    if ps.status = rssNotAllowed then
      Continue;

    if Result <> '' then
      Result := Result + ', ';

    if ((ps.dirlist <> nil) and (ps.dirlist.dirlistadded)) then
      Result := Result + ps.Allfiles
    else
      Result := Result + ps.AllFiles;
  end;
end;

function TPazo.StatusText: String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to sites.Count - 1 do
  begin
    try
      if i > sites.Count then
        Break;
    except
      Break
    end;
    Result := Result + TPazoSite(sites[i]).StatusText;
    if i <> sites.Count - 1 then
      Result := Result + ' ';
  end;
end;

procedure TPazo.Clear;
begin
  try
    RemovePazo(pazo_id);

    completezve := False;
    stopped := False; // ha stoppoltak korabban akkor ez most szivas
    ready := False;
    readyerror := False;
    errorreason := '';
    cache_files.Clear;
    sites.Clear;
    main_dirlist := nil;

    self.cleared := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TPazo.Clear : %s', [e.Message]);
      exit;
    end;
  end;
end;

function TPazo.AddSites: boolean;
begin
  Result := AddSites(False);
end;

function TPazo.AddSites(const aIsSpreadJob: boolean): boolean;
var
  s: TSite;
  i: integer;
  sectiondir: String;
  ps: TPazoSite;
begin
  Result := False;
  for i := sitesunit.sites.Count - 1 downto 0 do
  begin
    try
      s := TSite(sitesunit.sites[i]);
      if not (s.WorkingStatus in [sstUnknown, sstUp]) then
        Continue;
      if s.PermDown then
        Continue;
      if aIsSpreadJob then
      begin
        if s.SkipPre then
          Continue;
      end;

      sectiondir := s.sectiondir[rls.section];
      if (sectiondir = '') then
        Continue;

      sectiondir := DatumIdentifierReplace(sectiondir);

      if FindSite(s.Name) <> nil then
        Continue;

      if not aIsSpreadJob then
      begin
        if TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)) <> plmNone then
        begin
          if not (rls.pretime <> 0) then
            Continue;

          if not (s.IsPretimeOk(rls.section, rls.pretime)) then
            Continue;
        end;
      end;

      ps := TPazoSite.Create(self, s.Name, sectiondir);
      ps.status := rssNotAllowed;
      if not aIsSpreadJob then
      begin
        ps.DelaySetup;
      end;

      if s.IsAffil(rls.groupname) then
      begin
        Debug(dpMessage, section, '[IsAffilShouldPre] Site: %s - affil: %s - rlsName: %s - affils: %s ', [ps.Name, rls.groupname, rls.rlsname, s.siteaffils]);
        ps.status := rssShouldPre;
      end;

      sites.Add(ps);
    except
      Continue;
    end;

    Result := True;
  end;
end;

procedure TPazo.SiteDown(const sitename: String);
var
  ps: TPazoSite;
begin
  ps := FindSite(sitename);
  if ps <> nil then
    ps.MarkSiteAsFailed;
end;

function TPazo.allfiles: integer;
begin
  Result := cache_files.Count;
end;

{ TPazoSite }

function TPazoSite.AddDestination(const sitename: String; const rank: integer): boolean;
var
  ps: TPazoSite;
begin
  Result := False;
  ps := pazo.FindSite(sitename);
  if (ps <> nil) then
  begin
    try
      Result := AddDestination(ps, rank);
    except
      on e: Exception do
      begin
        Debug(dpError, section,
          Format('[EXCEPTION] TPazoSite.AddDestination: %s',
          [e.Message]));
        Result := False;
      end;
    end;
  end
  else
    pazo.errorreason := 'AddDest - PazoSite is NIL';
end;

function TPazoSite.AddDestination(ps: TPazoSite; const rank: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if error = True then
    exit;

  try
    pazo.cs.Enter;
    try
      if ps <> nil then
      begin
        if ps.error then
          exit;

        i := destinations.Indexof(ps);
        if i = -1 then
        begin
          Result := True;
          destinations.Add(ps);
          destinationRanks.Add(rank);
          //i:= ps.sources.IndexOf(self);
          //if i = -1 then
          //  ps.sources.Add(self);
        end;
      end
      else
        pazo.errorreason := 'AddDest - PazoSite is NIL';
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.AddDestination: %s',
        [e.Message]));
      Result := False;
    end;
  end;
end;

function mySpeedComparer(List: TStringList; Index1, Index2: integer): integer;
begin
  try
    Result :=
      CompareValue(StrToIntDef(list.ValueFromIndex[Index2], 0),
      StrToIntDef(list.ValueFromIndex[Index1], 0));
  except
    Result := 0;
  end;
end;

constructor TPazoSite.Create(pazo: TPazo; const Name, maindir: String);
begin
  Debug(dpSpam, section, 'TPazoSite.Create: %s', [Name]);
  inherited Create;

  activeTransfers := TStringList.Create;

  self.ts := 0;
  self.maindir := maindir;
  self.pazo := pazo;
  self.Name := Name;
  //sources:= TObjectList.Create(False);
  destinations := TObjectList.Create(False);
  destinationRanks := TList<integer>.Create;

  dirlist := TDirlist.Create(Name, nil, pazo.sl);
  if dirlist <> nil then
  begin
    if pazo.rls <> nil then
      dirlist.SetFullPath(MyIncludeTrailingSlash(maindir) + MyIncludeTrailingSlash(pazo.rls.rlsname))
    else
      dirlist.SetFullPath(MyIncludeTrailingSlash(maindir));
  end;

  s_dirlisttasks := TIdThreadSafeInt32.Create;
  s_racetasks := TIdThreadSafeInt32.Create;
  s_mkdirtasks := TIdThreadSafeInt32.Create;

  speed_from := TStringList.Create;
  try
    sitesdat.ReadSectionValues('speed-from-' + Name, speed_from);
    speed_from.CustomSort(myspeedcomparer);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Create speed(s): %s', [e.Message]));
      speed_from.Clear;
    end;
  end;
end;

destructor TPazoSite.Destroy;
begin
  Debug(dpSpam, section, 'TPazoSite.Destroy: %s', [Name]);
  //sources.Free;
  destinations.Free;
  destinationRanks.Free;
  dirlist.Free;
  activeTransfers.Free;
  s_dirlisttasks.Free;
  s_racetasks.Free;
  s_mkdirtasks.Free;
  speed_from.Free;
  inherited;
end;

function TPazoSite.MkdirReady(const dir: String): boolean;
var
  d: TDirList;
begin
  Result := False;

  d := dirlist.FindDirlist(dir);
  if d <> nil then
  begin
    debug(dpSpam, section, 'MkdirReady ' + Name + ' ' + dir);
    // Result will be true if need_mkdir is true even if try..except fails! So we say MkdirReady is true (ready)
    if d.need_mkdir then
    begin
      Result := True;
    end;
    // dir exist, we can set need_mkdir to false
    d.need_mkdir := False;
    d.dependency_mkdir := '';
  end;

  try
    RemovePazoMKDIR(pazo.pazo_id, Name, dir);
  except
    on e: Exception do
    begin
      Debug(dpError, section, 'TPazoSite.MkdirReady exception in RemovePazoMKDIR : %s', [e.Message]);
      exit;
    end;
  end;

  Result := True;
end;

function TPazoSite.MkdirError(const dir: String): boolean;
var
  d: TDirList;
begin
  d := dirlist.FindDirlist(dir);
  // dirlist was found but there is some error with this dir
  // so we set need_mkdir back to true!
  if d <> nil then
  begin
    debug(dpSpam, section, 'MkdirError ' + Name + ' ' + dir);
    irc_Addstats(Format('<c7>[MKDIR ERROR]</c> : %s %s/%s @ <b>%s</b>', [pazo.rls.section, pazo.rls.rlsname, dir, Name]));
    d.need_mkdir := True;
    d.error := True;
  end;

  Result := True;
end;

function TPazoSite.ParseDirlist(const netname, channel, dir, liststring: String; pre: boolean = False): boolean;
var
  d: TDirList;
  i: integer;
  de: TDirListEntry;
begin
  Result := False;

  // exit if no access to dirlist object
  if dirlist = nil then
    exit;

  // find the dirlist entry for this dir
  try
    d := dirlist.FindDirlist(dir, True);
  except
    exit;
  end;

  // exit if dirlist not found for this dir
  if d = nil then
    exit;

  // parse the dirlist
  try
    pazo.cs.Enter;
    try
      d.ParseDirlist(liststring);
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TPazoSite.ParseDirlist (d.ParseDirlist): %s', [e.Message]);
      exit;
    end;
  end;

  // exit if no entries added to the dirlist
  if d.entries = nil then
    exit;
  if d.entries.Count = 0 then
    exit;

  // sort the dirlist
  try
    pazo.cs.Enter;
    try
      d.Sort;
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TPazoSite.ParseDirlist (d.Sort): %s', [e.Message]);
      exit;
    end;
  end;

  // Do some stuff obviously
  // Do some stuff obviously
  pazo.cs.Enter;
  d.dirlist_lock.Enter;
  try
    for i := 0 to d.entries.Count - 1 do
    begin
      try
        if i > d.entries.Count then
          Break;
      except
        Break;
      end;

      try
        de := TDirListEntry(d.entries.items[i]);
        if ((not de.skiplisted) and (de.megvanmeg)) then
        begin
          if not de.Directory then
          begin
            if (de.justadded) then
            begin
              de.justadded := False;
              RemovePazoRace(pazo.pazo_id, Name, dir, de.filename);
            end;
          end;
          if not de.Directory then
            de.filesize := pazo.PRegisterFile(dir, de.filename, de.filesize);

          if Tuzelj(netname, channel, dir, de) then
          begin
            QueueFire;
          end;
        end;
      except
        Continue;
      end;
    end;
  finally
    d.dirlist_lock.Leave;
    pazo.cs.Leave;
  end;

  // Everything went fine
  Result := True;

  // Sort the queue
  if Result then
    QueueSort;

end;

function TPazoSite.SetFileError(const netname, channel, dir, filename: String): boolean;
var
  dl: TDirList;
  de: TDirlistEntry;

begin
  Result := False;
  try
    dl := dirlist.FindDirlist(dir);
    if dl = nil then
    begin
      pazo.errorreason := 'Dirlist is NIL';
      exit;
    end;

    de := dl.Find(filename);
    if de <> nil then
    begin
      de.error := True;
    end
    else
    begin
      de := TDirListEntry.Create(filename, dl);
      de.error := True;
      dl.entries.Add(de);
    end;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.SetFileError: %s', [e.Message]));
      Result := False;
    end;
  end;
end;

function TPazoSite.ParseDupe(const netname, channel: String; dl: TDirlist; const dir, filename: String; byme: boolean): boolean;
var
  de: TDirlistEntry;
  rrgx: TRegExpr;

begin
  Result := False;

  // skip filenames dotfiles
  if ((filename = '.') or (filename = '..') or (filename[1] = '.')) then
    exit;

  // skip global_skip matches
  rrgx := TRegExpr.Create;
  try
    try
      rrgx.ModifierI := True;
      rrgx.Expression := global_skip;

      if rrgx.Exec(filename) then
      begin
        exit;
      end;

    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] ParseDupe global_skip regex: %s',
          [e.Message]));
      end;
    end;

  finally
    rrgx.Free;
  end;


  //Debug(dpSpam, section, '--> '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, dir, filename]));
  try
    de := dl.Find(filename);
    if de = nil then
    begin
      // ez azt jelenti hogy meg nem tuzeltuk vegig
      de := TDirListEntry.Create(filename, dl);
      de.directory := False;
      de.done := True;
      de.filesize := -1;
      if byme then
        de.racedbyme := byme;

      dl.entries.Add(de);
      dl.LastChanged := Now();
      Result := True;
    end;
    (*
        if ((dl.parent <> nil) and (dl.parent.Sample)) then
          dl.cache_completed:= True;
      *)
        //inc(de.tradeCount);

    if (AnsiLowerCase(de.Extension) = '.sfv') then
    begin
      dl.sfv_status := dlSFVFound;
    end;

    if not de.done then
      Result := True;

    if byme then
      de.racedbyme := byme;

    de.done := True;
    if (not de.megvanmeg) then
    begin
      de.megvanmeg := True;
      // crashes
      RemovePazoRace(pazo.pazo_id, Name, dir, filename);
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s',
        [e.Message]));
      Result := False;
    end;
  end;
  //Debug(dpSpam, section, '<-- '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, dir, filename]));
end;

function TPazoSite.ParseDupe(const netname, channel, dir, filename: String; byme: boolean): boolean;
var
  dl: TDirList;

begin
  Result := False;
  try
    dl := dirlist.FindDirlist(dir);
    if dl = nil then
      pazo.errorreason := 'Dirlist is NIL';
    if dl = nil then
      exit;

    pazo.cs.Enter;
    try
      Result := ParseDupe(netname, channel, dl, dir, filename, byme);
    finally
      pazo.cs.Leave;
    end;
    // crash
    RemovePazoRace(pazo.pazo_id, Name, dir, filename);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
      Result := False;
    end;
  end;
end;

procedure TPazoSite.ParseXdupe(const netname, channel, dir, resp: String; added: boolean = False);
var
  s: String;
  dl: TDirList;
  lines_read: integer;
  fHelper: String;
begin
  fHelper := resp;
  try
    dl := dirlist.FindDirlist(dir);
    if dl = nil then
      pazo.errorreason := 'Dirlist is NIL';
    if dl = nil then
      exit;

    lines_read := 0;
    // crashes
    while (True) do
    begin
      s := Elsosor(fHelper);
      if s = '' then
        Break;

      Inc(lines_read);
      if (lines_read > 500) then
        break;

      //553- X-DUPE: 09-soulless-deadly_sins.mp3
      if (Pos('553- X-DUPE: ', s) = 1) then
      begin
        pazo.cs.Enter;
        try
          ParseDupe(netname, channel, dl, dir, Copy(s, 14, 1000), False);
        finally
          pazo.cs.Leave;
        end;

        RemovePazoRace(pazo.pazo_id, Name, dir, Copy(s, 14, 1000));
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseXdupe: %s', [e.Message]));
    end;
  end;
end;

function TPazoSite.Stats: String;
var
  fsize: double;
  fsname, fsizetrigger: String;
  i: integer;
  de: TDirlistEntry;
  sum: Int64;
begin
  fsname := Name;

  if status = rssRealPre then
    fsname := Format('<c10>%s</c>', [Name]); //Cyan(name); || release was a pre on this site
  if status = rssShouldPre then
    fsname := Format('<c7>%s</c>', [Name]); //Orange(name); || release should be pred on this site but wasn't pred there
  if status = rssNotAllowed then
    fsname := Format('<c4>%s</c>', [Name]); //Red(name); || release is not allowed on this site

  Result := fsname;

  if ( (not (status in [rssRealPre, rssShouldPre, rssNotAllowed])) and (dirlist <> nil) ) then
  begin
    if dirlist.RacedByMe(True) >= 1 then
    begin
      // we send some files
      fsize := dirlist.SizeRacedByMe(True) / 1024;
      RecalcSizeValueAndUnit(fsize, fsizetrigger, 1);

      if not dirlist.Complete then
        fsname := Format('<c11>%s</c>', [fsname]); //Light Blue(name); || release is incomplete (0byte files, dupefiles, etc) but we raced some files

      Result := Format('%s-(<b>%d</b>F @ <b>%.2f</b>%s)', [fsname, dirlist.RacedByMe(True), fsize, fsizetrigger]);

      // TODO: Find out why it is negative sometimes + try to fix
      // note: seems that de.filesize is negative as this is sum up in SizeRacedByMe() and shows -1 as result
      if fsize < 0 then
      begin
        Debug(dpError, section, Format('[NEGATIVE BYTES]: %f for %s with SizeRacedByMe(True) = %d, dirname : %s, full path : %s, complete_tag : %s',
          [fsize, fsname, dirlist.SizeRacedByMe(True), dirlist.Dirname, dirlist.full_path, dirlist.complete_tag]));

        // get more infos about dirlist entries
        sum := 0;
        dirlist.dirlist_lock.Enter;
        try
          for i := dirlist.entries.Count - 1 downto 0 do
          begin
            if i < 0 then Break;
            try
              de := TDirlistEntry(dirlist.entries[i]);
              if (de.racedbyme and de.Useful) then
                Inc(sum, de.filesize);
              //if ((de.directory) and (de.subdirlist <> nil)) then inc(sum, de.subdirlist.SizeRacedByMe(True));

              Debug(dpError, section, Format('%d for %s -- filename %s filesize %d byme %s useful %s (sum: %d)',
                [i, fsname, de.filename, de.filesize, BoolToStr(de.racedbyme, True), BoolToStr(de.Useful, True), sum]));
            except
              on E: Exception do
              begin
                Continue;
              end;
            end;
          end;
        finally
          dirlist.dirlist_lock.Leave;
        end;

      end;
    end
    else
    begin
      // we didn't send some files
      if (destinations.Count = 0) then
        Result := Format('<c5>%s</c>', [fsname]) //Brown(name); || not used to race from because no destination(s) added
      else if not dirlist.Complete then
        Result := Format('<c11>%s</c>', [fsname]) //Light Blue(name); || release is incomplete (0byte files, dupefiles, etc) but we didn't raced any files
      else if (dirlistgaveup) then
        Result := Format('<c13>%s</c>', [fsname]) //Pink(name); || dirlisting was stopped because there was an error (mkdir denied, out of space, etc) or race took too long
      else
        Result := Format('<c14>%s</c>', [fsname]); //Grey(name); || site was used but we didn't raced something
    end;
  end;

end;

function TPazoSite.Complete: boolean;
begin
  // xperia test if dirlist is complete
  Result := (status in [rssRealPre, rssComplete]) or (dirlist.Complete);
end;

function TPazoSite.Source: boolean;
begin
  Result := (status = rssAllowed) or Complete;
end;

procedure TPazoSite.SetComplete(const cdno: String);
var
  i: integer;
  d: TDirlist;
begin
  if (cdno = '') then
  begin
    d := dirlist.FindDirlist(cdno);
    if d <> nil then
      d.cache_completed := True;

    status := rssComplete;
    exit;
  end;

  cds := cds + cdno;
  i := StrToIntDef(cdno, 0);
  if i > dirlist.biggestcd then
    dirlist.biggestcd := i;

  if dirlist.biggestcd < 2 then
  begin
    exit;
  end;

  for i := 1 to dirlist.biggestcd do
    if 0 = Pos(IntToStr(i), cds) then
      exit;

  status := rssComplete;
end;

function TPazoSite.Age: integer;
begin
  if ts <> 0 then
  begin
    Result := SecondsBetween(Now, ts);
    exit;
  end;

  Result := -1;
  if dirlist <> nil then
    Result := SecondsBetween(Now, dirlist.LastChanged);
end;

function TPazoSite.AsText: String;
var
  i: integer;
begin
  Result := '<u><b>SITE: ' + Name + '</b></u>';
  Result := Result + ': ' + maindir + ' (' + IntToStr(dirlist.entries.Count) + ' items)';

  if (dirlist.Complete) then
  begin
    Result := Result + ' <b>COMPLETE</b>';
  end;
  Result := Result + #13#10;

  //Result:= Result + 'Sources: ';
  //for i:= 0 to sources.Count -1 do
  //  Result:= Result + TPazoSite(sources[i]).name+' ';
  //Result:= Result + #13#10;

  Result := Result + 'Destinations: ';
  for i := 0 to destinations.Count - 1 do
    Result := Result + TPazoSite(destinations[i]).Name + '(' + IntToStr( destinationRanks[i]) + ')' + ' ';

  if ((dirlist.GetCompleteInfo <> 'Not Complete') and (not StatusRealPreOrShouldPre)) then
  begin
    Result := Result + #13#10;
    Result := Result + 'Completion Time: ' + TimeToStr(dirlist.date_completed) + ' via ' + dirlist.GetCompleteInfo;
  end;

  Result := Result + #13#10;
  Result := Result + 'Status: ';
  case status of
    rssNotAllowed: Result := Result + '<c4>not allowed</c> (' + reason + ')';
    rssNotAllowedButItsThere: Result := Result + 'not allowed but its there (' + reason + ')';
    rssAllowed: Result := Result + 'allowed (' + reason + ')';
    rssShouldPre: Result := Result + '(?)pre';
    rssRealPre: Result := Result + '<b>pre</b>';
    rssComplete: Result := Result + 'complete (' + reason + ')';
    rssNuked: Result := Result + 'nuked';
  end;
  Result := Result + #13#10;
end;

function TPazoSite.RoutesText: String;
var
  i: integer;
begin
  Result := '<u>' + Name + '</u> ->';
  for i := 0 to destinations.Count - 1 do
  begin
    Result := Result + TPazoSite(destinations[i]).Name + '(' + IntToStr(destinationRanks[i]) + ')' + ' ';
  end;
  Result := Result + #13#10;
end;

function TPazoSite.Allfiles: String;
begin
  Result := Name;

  if ((status = rssRealPre) and (pazo.main_dirlist <> self.dirlist)) then
    Result := Name + '-' + IntToStr(pazo.main_dirlist.Done)
  else if dirlist <> nil then
    Result := Name + '-' + IntToStr(dirlist.Done);
end;

function TPazoSite.StatusText: String;
begin
  Result := Name + '-';
  case status of
    rssNotAllowed: Result := Result + 'N';
    rssNotAllowedButItsThere: Result := 'NABIT';
    rssAllowed: Result := Result + 'A';
    rssShouldPre: Result := Result + 'S';
    rssRealPre: Result := Result + 'P';
    rssComplete: Result := Result + 'C';
  end;
end;

function TPazoSite.StatusRealPreOrShouldPre: boolean;
begin
  Result := status in [rssShouldPre, rssRealPre];
end;

procedure TPazoSite.Clear;
begin
  error := False;
  lookupforcedhere := False;
  firesourcesinstead := False;
  badcrcevents := 0;

  dirlistgaveup := False;
  try
    if dirlist <> nil then
      dirlist.Clear;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Clear: %s',
        [e.Message]));
    end;
  end;
end;

procedure TPazoSite.MarkSiteAsFailed(echomsg: boolean = False);
begin
  error := True;
  Debug(dpSpam, section, Format('--> TPazoSite.MarkSiteAsFailed', []));

  try
    dirlistgaveup := True;

    RemoveRaceTasks(pazo.pazo_id, Name);
    RemoveDirlistTasks(pazo.pazo_id, Name);

    if echomsg then
      irc_Addstats(Format('<c7>[SITE FAILED]</c> : %s %s @ <b>%s</b> <b>%s</b>', [pazo.rls.section, pazo.rls.rlsname, Name, pazo.errorreason]));

  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.MarkSiteAsFailed: %s', [e.Message]));
    end;
  end;

  Debug(dpSpam, section, Format('<-- TPazoSite.MarkSiteAsFailed', []));
end;

procedure TPazoSite.RemoveMkdir;
begin
  if dirlist <> nil then
  begin
    RemovePazoMKDIR(pazo.pazo_id, Name, '');
  end;
end;

function TPazoSite.DirlistGaveUpAndSentNoFiles: Boolean;
begin
  Result := dirlistgaveup and (dirlist.RacedByMe(False) = 0);
end;

procedure TPazoSite.DelaySetup;
var
  fSite: TSite;
begin
  if not Assigned(pazo.rls) then
    exit;

  fSite := FindSiteByName('', Name);
  delay_leech := fSite.delayleech[pazo.rls.section];
  delay_upload := fSite.delayupload[pazo.rls.section];

  Debug(dpSpam, section, 'DelaySetup %s: %d s for delayleech, %d s for delayupload', [Name, delay_leech, delay_upload]);
end;

end.

