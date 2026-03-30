unit pazo;

// EZTA Z UNITOT CSAK A QUEUE_LOCK ZARASA UTAN SZABAD HIVNI!
// THIS ONLY ON THE QUEUE_LOCK CLOSING is FREE to call!
interface

uses
  Classes, kb.releaseinfo, SyncObjs, Contnrs, dirlist, skiplists, globals, IdThreadSafe, Generics.Collections, IniFiles, sfv, slcriticalsection2,
  routeconfig;

type
  TQueueNotifyEvent = procedure(Sender: TObject; Value: integer) of object;

  { threadsafe integer class with notify event on Increase/Decrease }
  TIdThreadSafeInt32WithEvent = class(TIdThreadSafeInt32)
  private
    FOnChange: TQueueNotifyEvent;
  public
    constructor Create; override;
    procedure Increase;
    procedure Decrease;
    property OnChange: TQueueNotifyEvent read FOnChange write FOnChange;
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

  TPazoSite = class;

  //used to store destination site and rank
  TDestinationRank = record
    private
      FPazoSite: TPazoSite; //< destination site
      FRank: integer; //< rank
    public
      property PazoSite: TPazoSite read FPazoSite;
      property Rank: integer read FRank;
      constructor Create(const aPazoSite: TPazoSite; const aRank: integer);
   end;

  TPazoSite = class
  private
    cds: String;
    FDestinations: TList<TDestinationRank>; //< destination sites and ranks
    FActiveTransfers: TDictionary<string, string>; //< stores which files have an active tranfer to this destination site. Key: filepath, Value: source site
    FActiveTransfersCS: TCriticalSection;
    function Tuzelj(const netname, channel, dir: String; aDirListEntries: TList<TDirListEntry>): boolean;
    function GetDirlistGaveUp: boolean;
    procedure SetDirlistGaveUp(const aGaveUp: boolean);
    function GetActiveTransferCount: Int32;

  public

    midnightdone: boolean;
    Name: String; //< sitename
    maindir: String; //< sectiondir? TODO: debug real value
    pazo: TPazo; //< pazo where this TPazoSite belongs to
    destinations_cs: TCriticalSection; //< Critical section to protect adding of values to @link(destinations)
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

    badcrcevents: integer; //< total number of bad crc events

    // will be true when autofollow rule is used, otherwise default false
    firesourcesinstead: boolean;

    speed_from: TList<TSpeedFromRouteInfo>;

    property dirlistgaveup: boolean read GetDirlistGaveUp write SetDirListGaveUp; //< gets or sets a value indicating whether dirlisting have been given up for this site
    property Destinations: TList<TDestinationRank> read FDestinations; //< destination sites and ranks
    property ActiveTransferCount: Int32 read GetActiveTransferCount;

    function StatusRealPreOrShouldPre: boolean;  //< returns @true if its a pre or at least it should be one
    function Source: boolean;
    function Complete: boolean;

    function Age: integer;
    { Create formatted output string for !kbshow }
    function AsText: String;
    { Create formatted output string for [ROUTES] announce }
    function RoutesText: String;
    function DirlistGaveUpAndSentNoFiles: Boolean;
    procedure DelaySetup;

    { Stop dirlisting and racing on this site as there was a catastrophic failure
      @param(aMessage specific error message with detailed information) }
    procedure MarkSiteAsFailed(const aMessage: string);

    function ParseDirlist(const netname, channel, dir, liststring: String; pre: boolean = False): boolean;
    function MkdirReady(const dir: String): boolean;
    function MkdirError(const dir: String): boolean;
    function AddDestination(const sitename: String; const rank: integer): boolean; overload;
    function AddDestination(const ps: TPazoSite; const rank: integer): boolean; overload;

    { Add the raced file and appropiate infos into database
      @param(aParentPazo pazo where this TPazoSite belongs to)
      @param(aName sitename)
      @param(aMaindir sectiondir? TODO: debug real value) }
    constructor Create(const aParentPazo: TPazo; const aName, aMaindir: String; const aSite: TObject = nil);
    destructor Destroy; override;

    { Processes the X-DUPE response from FTPd which is send if file already exists when trying to transfer it.
      @param(aNetname netname)
      @param(aChannel channelname)
      @param(aDir directory inside the rls e.g. '/' if it's the main dir or '/Sample')
      @param(aFullResponse complete X-DUPE response from FTPd) }
    procedure ProcessXDupeResponse(const aNetname, aChannel, aDir, aFullResponse: String);

    { Adds info about that the given file exists on this site (DirListEntry)
      @param(aNetname netname)
      @param(aChannel channelname)
      @param(aDir directory inside the rls e.g. '/' if it's the main dir or '/Sample')
      @param(aFilename filename)
      @param(aSentByMe the file was sent or is being sent by me)
      @param(aIsComplete the file is complete) }
    procedure ParseDupe(const aNetname, aChannel, aDir, aFilename: String; const aSentByMe, aIsComplete: boolean) overload;

    { Adds info about that the given file exists on this site (DirListEntry)
      @param(aNetname netname)
      @param(aChannel channelname)
      @param(aDirlist TDirlist where it should be added to)
      @param(aDir directory inside the rls e.g. '/' if it's the main dir or '/Sample')
      @param(aFilename filename)
      @param(aSentByMe the file was sent or is being sent by me)
      @param(aIsComplete the file is complete) }
    procedure ParseDupe(const aNetname, aChannel: String; aDirlist: TDirlist; const aDir, aFilename: String; const aSentByMe, aIsComplete: boolean) overload;

    procedure ParseDupe(const aNetname, aChannel: String; aDirlist: TDirList; const aDir: string; const aFilenames: TArray<String>; const aSentByMe, aIsComplete: boolean) overload;

    function SetFileError(const netname, channel, dir, filename: String): boolean; //< Sets error flag to true for filename if it cannot be transfered
    function Stats: String;
    procedure SetComplete(const cdno: String);
    function StatusText: String;
    procedure Clear;
    procedure RemoveActiveTransfer(const aFilepath: String);
    function HasActiveTransfer(const aFilepath: String): boolean; overload;
    function HasActiveTransfer(const aFilepath, aSourceSite: String): boolean; overload;
    procedure AddActiveTransfer(const aFilepath, aSourceSite: String);

  end;

  TPazo = class
  private
    lastannounceconsole: String;
    lastannounceirc: String; //< last announce string for [STATS] after race
    lastannounceroutes: String; //< last announce string from @link(TPazo.RoutesText)
    FExcludeFromIncfiller: boolean; //< @true if the incomplete filler should ignore this TPazo (e.g. already handled once), @false otherwise.
    FUniqueFileListOfRelease_cs: TSlCriticalSection2; //< Critical section for Add calls to @link(FUniqueFileListOfRelease)
    FUniqueFileListOfRelease: TDictionary<String, Int64>; //< Dictionary with files (including subdirs) and corresponding filesize (biggest value seen on any site) for this release, Key="dir + '/' + filename" and Value=filesize
    FPazoSFV: TPazoSFV;

    { Creates/Updates the filesize for given subdir and filename combination
      @param(aDir Location of the file inside releasedir)
      @param(de The TDirListEntry of the file)
      @returns(filesize in bytes which could be @link(aFilesize) or bigger if seen somewhere else) }
    function PRegisterFile(const aDir: String; const de: TDirListEntry): Int64;

    procedure QueueEvent(Sender: TObject; Value: integer);

  public
    pazo_id: integer;

    stated: boolean;
    cleared: boolean;

    // TODO: debug both variables, seems to be empty as it is never initialized?
    // needed for rules like 'source' or 'completesource'
    srcsite: String;
    dstsite: String;

    rls: TRelease; //< holds the information of @link(kb.TRelease) or its descendant

    stopped: boolean;
    ready: boolean;
    readyerror: boolean;
    errorreason: String;
    lastTouch: TDateTime;
    FDetectedUTC: TDateTime; //< UTC timestamp of release detection
    FDetectedTick: Int64;    //< GetTickCount64 at detection time
    FAddedTick: Int64;       //< GetTickCount64 at pazo creation time
    FDirlistRequestedTick: Int64;  //< GetTickCount64 when first dirlist was requested
    FTasksCreatedTick: Int64;      //< GetTickCount64 when first tasks were created
    FLastTaskCreatedTick: Int64;   //< GetTickCount64 when most recent task was created

    PazoSitesList: TObjectList<TPazoSite>; //< list of @link(TPazoSite) which are part of this @link(TPazo) due to calling @link(AddSites)
    sl: TSkipList;

    added: TDateTime;

    // Integers with locking and event
    queuenumber: TIdThreadSafeInt32WithEvent;
    dirlisttasks: TIdThreadSafeInt32;
    racetasks: TIdThreadSafeInt32;
    mkdirtasks: TIdThreadSafeInt32;

    { Searches for @link(sitename) via @link(TPazo.FindSite) and calls TPazoSite.MarkSiteAsFailed if site was found
      @param(sitename Sitename which sould be set down) }
    procedure SiteDown(const sitename: String);
    procedure Clear;
    function StatusText: String;
    function Age: integer;
    { Show @link(rls), @link(age), @link(PazoSitesList) count and infos for each item in @link(PazoSitesList) for !kbshow }
    function AsText: String;
    { Show headline for [ROUTES] announce and print infos for each item in @link(PazoSitesList) if different then previous @link(lastannounceroutes) }
    function RoutesText: String;
    { @abstract(Returns true if UDP announce is enabled and configured) }
    function IsUDPEnabled: Boolean;
    function Stats(const console: boolean; withdirlist: boolean = True): String;
    { Generate site completion times statistics relative to pazo added time
      @param(console @true if output is for console, @false for IRC)
      @returns(Formatted string with site completion times) }
    function SiteCompleteTimesStats(const console: boolean): String;
    constructor Create(const rls: TRelease; const pazo_id: integer);
    destructor Destroy; override;
    function FindSite(const sitename: String): TPazoSite;
    function AddSite(const sitename, maindir: String; delay: boolean = True): TPazoSite;
    { Iterates through all @link(sitesunit.sites) and adds a @link(TPazoSite) to @link(TPazo.PazoSitesList) if the site is not down, has the section, rls fits pretime, etc and sets @link(TPazoSite.status)
      @returns(@true if at least one site was added, @false otherwise) }
    function AddSites: boolean; overload;
    { Iterates through all @link(sitesunit.sites) and adds a @link(TPazoSite) to @link(TPazo.PazoSitesList) if the site is not down, has the section, rls fits pretime, etc and sets @link(TPazoSite.status)
      @param(aIsSpreadJob Set it to @true if its a spread job for purpose of preeing (skips some checks), @false otherwise)
      @returns(@true if at least one site was added, @false otherwise) }
    function AddSites(const aIsSpreadJob: boolean): boolean; overload;
    { Returns the filesize for given filname of the release
      @param(aDir Location of the file inside releasedir)
      @param(aFilename Name of the file)
      @returns(filesize in bytes, -1 if not found or unknown file) }
    function PFileSize(const aDir, aFilename: String): Int64;
    { Returns the amount of files for the release, includes files in subdirs
      @returns(Total file count of @link(rls)) }
    function GetCountOfCachedFiles: integer;

    property ExcludeFromIncfiller: Boolean read FExcludeFromIncfiller write FExcludeFromIncfiller;
    property PazoSFV: TPazoSFV read FPazoSFV;
  end;

function PazoAdd(const rls: TRelease): TPazo;
procedure PazoInit;

function FindMostCompleteSite(pazo: TPazo): TPazoSite;

implementation

uses
  SysUtils, StrUtils, mainthread, sitesunit, DateUtils, debugunit, queueunit,
  taskrace, mystrings, irc, sltcp, slhelper, Math, taskpretime, configunit,
  mrdohutils, console, RegExpr, statsunit, Generics.Defaults, kb, tasksitesfv;

const
  section = 'pazo';

var
  local_pazo_id: integer;
  glMaxBadcrcEvents: integer; //< max number of bad crc events read from config
  glPazoPreTimeLookupMode: TPretimeLookupMode;
  glShowCompleteTimeStats: boolean;


constructor TDestinationRank.Create(const aPazoSite: TPazoSite; const aRank: integer);
begin
  FPazoSite := aPazoSite;
  FRank := aRank;
end;

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

function FindMostCompleteSite(pazo: TPazo): TPazoSite;
var
  ps: TPazoSite;
  i: integer;
begin
  Result := nil;
  try
    for i := pazo.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.PazoSitesList[i]);
      if ps.lookupforcedhere then
      begin
        ps.lookupforcedhere := False;
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.PazoSitesList[i]);
      if ps.ts <> 0 then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.PazoSitesList[i]);
      if (ps.Complete) then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.PazoSitesList[i]);
      if (ps.ircevent) then
      begin
        Result := ps;
        exit;
      end;
    end;

    for i := pazo.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(pazo.PazoSitesList[i]);
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

function PazoAdd(const rls: TRelease): TPazo;
begin
  Result := TPazo.Create(rls, local_pazo_id);
  Inc(local_pazo_id);
end;

procedure PazoInit;
begin
  local_pazo_id := 0;
  glMaxBadcrcEvents := config.ReadInteger('taskrace', 'badcrcevents', 15);
  glPazoPreTimeLookupMode := TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0));
  glShowCompleteTimeStats := config.ReadBool('taskrace', 'show_complete_time_stats', False);
end;

function TPazoSite.GetDirlistGaveUp: boolean;
begin
  Result := False;
  if (dirlist <> nil) then
    Result := dirlist.DirlistGaveUp;
end;

procedure TPazoSite.SetDirlistGaveUp(const aGaveUp: boolean);
begin
  if (dirlist <> nil) then
    dirlist.DirlistGaveUp := aGaveUp;
end;

function TPazoSite.Tuzelj(const netname, channel, dir: String; aDirListEntries: TList<TDirListEntry>): boolean;
// de is TDirListEntry from sourcesite
// dstdl is TDirList on destination site
// dde is TDirListEntry on destination site
var
  fDestination: TDestinationRank;
  dst: TPazoSite;
  dstrank: Integer;
  dstdl: TDirList;
  pm: TPazoMkdirTask;
  pr: TPazoRaceTask;
  pd: TPazoDirlistTask;
  de, dde: TDirListEntry;
  s: TSite;
  fd: String;
begin
  Result := False;
  dst := nil;
  dstdl := nil;
  dde := nil;
  pm := nil;
  pr := nil;

  // something's fucked
  if error then exit;

  if (dir <> '') then
    fd := pazo.rls.rlsname + '/' + dir
  else
    fd := pazo.rls.rlsname;

  // ignore this site if you don't have setup download slots for it
  s := FindSiteByName('', Name);
  if ((status in [rssRealPre, rssShouldPre])) then
  begin
    if s.max_pre_dn = 0 then exit;
  end
  else if s.max_dn = 0 then
    exit;

  pazo.lastTouch := Now();

  // enumerate possible destinations
  for fDestination in destinations do
  begin
    dst := fDestination.PazoSite;
    dstrank := fDestination.Rank;
    try
      try
        if error then exit;
        if dst.error then Continue;

      // ignore this destination if we don't want to upload there
      s := FindSiteByName('', dst.Name);
      if (s.max_up = 0) then Continue;

        //if the destination is going sstTempDown during the race we would spam race tasks
        //avoid this and also check other down states just to be sure
        if s.WorkingStatus in [sstDown, sstTempDown, sstMarkedAsDownByUser] then continue;

        // drop sending to this destination if too much crc events
        if (dst.badcrcevents > glMaxBadcrcEvents) then Continue;

        // Problem with dirlist
        if dirlist = nil then Continue;
        if dirlist.error then Continue;
        if dst.dirlist = nil then Continue;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj General: %s', [e.Message]));
          Break;
        end;
      end;

      // Find the dirlist for destination site
      dstdl := dst.dirlist.FindDirlist(dir, True);

      // Dirlist for destination site not available
      if dstdl = nil then Continue;
      if dstdl.error then Continue;

      for de in aDirListEntries do
      begin

        if (not de.Directory) then
        begin
          if ((de.IsBeingUploaded or (de.filesize < 1)) and (s.SkipBeingUploadedFiles = sbuBeingUploaded)) then
            Continue;
          if ((de.filesize < 1) and (s.SkipBeingUploadedFiles = sbuOnly0Byte)) then
            Continue;
        end;

        // find the dirlist entry
        try
          dde := dstdl.Find(de.filename);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj dstdl.Find: %s', [e.Message]));
            Continue;
          end;
        end;

        // not really sure
        (*
          if ((dde <> nil) and (dde.done)) then Continue;
        *)
        if ((dde <> nil) and (dde.IsOnSite)) then Continue;
        if ((dde <> nil) and (dde.error)) then Continue;

        pm := nil;
        // Check if mkdir is needed
        Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Checking if mkdir is needed on %s', [fd, Name, dst.Name, dst.Name]);
        if ((dstdl.entries <> nil) and (dstdl.entries.Count = 0)) then
        begin
          dstdl.dirlist_lock.Enter('TPazoSite.Tuzelj');
          try
            if ((dstdl.need_mkdir) and (dstdl.dependency_mkdir = '')) then
            begin
              Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Adding MKDIR task on %s', [fd, Name, dst.Name, dst.Name]);

            // Create the mkdir task
              if (dstdl.parent <> nil) then
                pm := TPazoMkdirTask.Create(netname, channel, dst.Name, pazo, dstdl.parent.dirlist, dir)
              else
                pm := TPazoMkdirTask.Create(netname, channel, dst.Name, pazo, nil, dir);

              // add delay to mkdir if delay_upload enabled
              if dst.delay_upload > 0 then
                pm.startat := IncSecond(Now, dst.delay_upload);

              dstdl.dependency_mkdir := pm.UidText;
            end;
          finally
            dstdl.dirlist_lock.Leave;
          end;
            // Finally add mkdir task
          if pm <> nil then
          begin
            try
              AddTask(pm, True);
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
            AddTask(pd, true);
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
            // skip nfo and sfv if already there
            if ((dstdl.HasSFV) and (de.IsSFV)) then
              Continue;
            if ((dstdl.HasNFO) and (de.IsNFO)) then
              Continue;

            // Create the race task
            Debug(dpSpam, section, '%s :: Checking routes from %s to %s :: Adding RACE task on %s %s', [fd, Name, dst.Name, dst.Name, de.filename]);
            pr := TPazoRaceTask.Create(netname, channel, Name, dst.Name, pazo, dstdl, dir, de.filename, de.filesize, dstrank);

            // Set file type for subdirs
            if (dstdl.parent <> nil) then
              case de.DirType of
                IsSample: pr.IsSample := True;
                IsProof: pr.IsProof := True;
                IsCovers: pr.IsCovers := True;
                IsSubs: pr.IsSubs := True;
              end;

            // Set file type
            if (de.IsSFV) then
              pr.IsSfv := True;
            if (de.IsNFO) then
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

            // finally we can add the task
            try
              AddTask(pr);
              Result := True;
            except
              on e: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj (AddTask(pr)): %s', [e.Message]));
                Break;
              end;
            end;
          end;
          end;
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
  PazoSitesList.Add(Result);
  CheckSiteSlots(sitename);
end;

function TPazo.Age: integer;
var
  ps: TPazoSite;
  a: integer;
begin
  Result := -1;

  for ps in PazoSitesList do
  begin
    a := ps.Age;
    if ((a <> -1) and ((Result = -1) or (Result < a))) then
      Result := a;
  end;

  if Result = -1 then
    Result := SecondsBetween(Now, added);
end;

function TPazo.AsText: String;
var
  ps: TPazoSite;
begin
  Result := rls.AsText(pazo_id);

  Result := Result + Format('Age: %ds %s', [age, #13#10]);

  Result := Result + 'Skiplist: ';
  if sl <> nil then
    Result := Result + Format('%s%s', [sl.sectionname, #13#10])
  else
    Result := Result + '?#13#10';

  Result := Result + Format('Sites: %d %s', [PazoSitesList.Count, #13#10]);

  for ps in PazoSitesList do
  begin
    Result := Result + ps.AsText;
  end;
end;

function TPazo.RoutesText: String;
var
  ps: TPazoSite;
begin
  Result := Format('<c3>[ROUTES]</c> : <b>%s</b> (%d sites)', [rls.rlsname, PazoSitesList.Count]);
  Result := Result + #13#10;

  for ps in PazoSitesList do
  begin
    Result := Result + ps.RoutesText;
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

function TPazo.PRegisterFile(const aDir: String; const de: TDirListEntry): Int64;
var
  fKey: String;
  fFilesize: Int64;
  fWasAdded: boolean;
  fPazoSite: TPazoSite;
begin
  fKey := aDir + '/' + de.FilenameLowerCased;
  fWasAdded := False;

  FUniqueFileListOfRelease_cs.Enter('PRegisterFile');
  try
    if not FUniqueFileListOfRelease.TryGetValue(fKey, fFilesize) then
    begin
      FUniqueFileListOfRelease.Add(fKey, de.filesize);
      fWasAdded := True;
      Result := de.filesize;
    end
    else
    begin
      if fFilesize < de.filesize then
      begin
        FUniqueFileListOfRelease[fKey] := de.filesize;
        Result := de.filesize;
      end
      else
      begin
        Result := fFilesize;
      end;
    end;
  finally
    FUniqueFileListOfRelease_cs.Leave;
  end;

  if fWasAdded And de.IsSFV and self.rls.IsSFVRelease and not FPazoSFV.HasSFV(aDir) then
  begin
    if FPazoSFV.RegisterSFV(aDir) then
    begin
      // create the SFV task just for the sites that are in the pazo now. some sites might be added later
      // due to delayed rules (imdb, tv, ...) but we don't care about those, the ones we have now should be
      // enough to get the SFV

      for fPazoSite in PazoSitesList do
      begin
        if FindSiteByName('', fPazoSite.Name).UseForNFOdownload = ufnEnabled then
        begin
          Debug(dpSpam, section, 'Add SFV task for %s %s (%s)', [rls.rlsname, aDir, fPazoSite.Name]);
          AddTask(TPazoSiteSfvTask.Create('', '', fPazoSite.Name, self, aDir, de.filename, 1));
        end;
      end;
    end;
  end;
end;

function TPazo.GetCountOfCachedFiles: integer;
begin
  Result := FUniqueFileListOfRelease.Count;
end;

function TPazo.IsUDPEnabled: Boolean;
begin
  Result := False; // UDP announce integration not enabled in this build
end;

constructor TPazo.Create(const rls: TRelease; const pazo_id: integer);
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
  FAddedTick := GetTickCount64;
  FDirlistRequestedTick := 0;
  FTasksCreatedTick := 0;
  FLastTaskCreatedTick := 0;
  FDetectedTick := 0;
  FDetectedUTC := 0;
  queuenumber := TIdThreadSafeInt32WithEvent.Create;
  queuenumber.OnChange := QueueEvent;
  dirlisttasks := TIdThreadSafeInt32.Create;
  racetasks := TIdThreadSafeInt32.Create;
  mkdirtasks := TIdThreadSafeInt32.Create;

  readyerror := False;
  PazoSitesList := TObjectList<TPazoSite>.Create(True);
  self.pazo_id := pazo_id;
  stopped := False;
  ready := False;
  lastTouch := Now();
  FUniqueFileListOfRelease_cs := TSlCriticalSection2.Create('UniqueFileList_' + rls.Name + '_' + IntToStr(pazo_id));
  FUniqueFileListOfRelease := TDictionary<String, Int64>.Create;

  self.stated := False;
  self.cleared := False;

  FExcludeFromIncfiller := False;
  if rls.IsSFVRelease then
    FPazoSFV := TPazoSFV.Create;

  inherited Create;
end;

destructor TPazo.Destroy;
begin
  Debug(dpSpam, section, 'TPazo.Destroy: %s', [rls.rlsname]);
  Clear;
  PazoSitesList.Free;
  queuenumber.Free;
  dirlisttasks.Free;
  racetasks.Free;
  mkdirtasks.Free;
  FUniqueFileListOfRelease.Free;
  FUniqueFileListOfRelease_cs.Free;
  FreeAndNil(rls);
  if FPazoSFV <> nil then FPazoSFV.Free;

  inherited;
end;

function TPazo.FindSite(const sitename: String): TPazoSite;
var
  ps: TPazoSite;
begin
  Result := nil;
  try
    for ps in PazoSitesList do
    begin
      if ps.Name = sitename then
      begin
        Result := ps;
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
    ready := True;

    if ((not slshutdown) and (rls <> nil)) then
    begin
      Debug(dpSpam, section, 'Number of pazo tasks is now zero! ' + IntToStr(pazo_id));

      if not stopped then
      begin
        s := Stats(True, False);
        if ((lastannounceconsole <> s) and (s <> '')) then
        begin
          irc_addtext('CONSOLE', 'Stats', Format('%s %s (%d):%s', [rls.section, rls.rlsname, GetCountOfCachedFiles, s]));
          lastannounceconsole := s;
        end;

        s := Stats(False, False);
        if ((lastannounceirc <> s) and (s <> '')) then
        begin
          irc_addstats(Format('<c10>[<b>STATS</b>]</c> %s %s (%d):', [rls.section, rls.rlsname, GetCountOfCachedFiles, s]));
          irc_AddstatsB(Stats(False, True));
          lastannounceirc := s;

          if glShowCompleteTimeStats then
          begin
            s := SiteCompleteTimesStats(False);
            if s <> '' then
            begin
              irc_addstats(Format('<c10>[<b>COMPLETE TIME AFTER ADDPRE</b>]</c> %s %s (%d):', [rls.section, rls.rlsname, GetCountOfCachedFiles]));
              irc_addstats(s);
            end;
          end;
        end;
      end
      else
      begin
        irc_addstats('<c10>[<b>STATS</b>]</c> Pazo Stopped.');
      end;
    end;
  end;
end;

function _CompareCompleteTimes({$IFDEF FPC}constref{$ELSE}const{$ENDIF} pazo1, pazo2: TPazoSite): Integer;
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
        if ((pazo1.dirlist.CompletedTime = 0) and (pazo2.dirlist.CompletedTime = 0)) then
          Result := CompareText(pazo1.Name, pazo2.name)
        else if (pazo1.dirlist.CompletedTime = 0) then
          Result := 1
        else if (pazo2.dirlist.CompletedTime = 0) then
          Result := -1
        else
          Result := CompareDateTime(pazo1.dirlist.CompletedTime, pazo2.dirlist.CompletedTime);
      end
    else
      Result := 0;
  end;
end;

function TPazo.Stats(const console: boolean; withdirlist: boolean = True): String;
var
  numComplete: integer;
  ps: TPazoSite;
  s: TSite;
  sitesSorted: TObjectList<TPazoSite>;
  completeTimeReference: TDateTime;
  secondsAfter: Int64;
begin
  Result := '';
  numComplete := 1;
  completeTimeReference := 0;

  sitesSorted := TObjectList<TPazoSite>.Create(False);
  try
    // add references to original sites list and sort them
    sitesSorted.AddRange(PazoSitesList);
    sitesSorted.Sort(TComparer<TPazoSite>.Construct(_CompareCompleteTimes));

    for ps in sitesSorted do
    begin
      try
        if ps.status = rssNotAllowed then
          Continue;
        s := FindSiteByName('', ps.Name);
        if s = nil then
          Continue;
        if s.noannounce and not console then
          Continue;

        if Result <> '' then
          Result := Result + ', ';

        if ((ps.status in [rssRealPre, rssShouldPre, rssNotAllowed]) or ps.DirlistGaveUpAndSentNoFiles or (ps.dirlist.CompletedTime = 0)) then
          Result := Result + '"' + ps.Stats + '"'
        else
        begin
          if CompleteTimeReference = 0 then
          begin
            Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, '"');
            completeTimeReference := ps.dirlist.CompletedTime;
          end
          else
          begin
            secondsAfter := SecondsBetween(completeTimeReference, ps.dirlist.CompletedTime);
            if secondsAfter <> 0 then
              Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, ' (+', IntToStr(secondsAfter), 's)"')
            else
              Result := Concat(Result, '"', IntToStr(numComplete), '. ', ps.Stats, '"');
          end;
          inc(numComplete);
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TPazo.Stats: %s', [e.Message]));
          Continue;
        end;
      end;
    end;
  finally
    sitesSorted.Free;
  end;
end;

function TPazo.SiteCompleteTimesStats(const console: boolean): String;
var
  numComplete: integer;
  ps: TPazoSite;
  s: TSite;
  sitesSorted: TObjectList<TPazoSite>;
  secondsFromAddpre: Double;
begin
  Result := '';
  numComplete := 1;

  Debug(dpSpam, section, Format('[TIMING DEBUG] Starting SiteCompleteTimesStats | Release: %s | ADDPRE Time: %s',
    [rls.rlsname, FormatDateTime('hh:nn:ss.zzz', added)]));

  sitesSorted := TObjectList<TPazoSite>.Create(False);
  try
    sitesSorted.AddRange(PazoSitesList);
    sitesSorted.Sort(TComparer<TPazoSite>.Construct(_CompareCompleteTimes));

    for ps in sitesSorted do
    begin
      try
        if ps.status = rssNotAllowed then
        begin
          Debug(dpSpam, section, Format('[TIMING DEBUG] SKIPPED: %s - Reason: rssNotAllowed', [ps.Name]));
          Continue;
        end;
        s := FindSiteByName('', ps.Name);
        if s = nil then
        begin
          Debug(dpSpam, section, Format('[TIMING DEBUG] SKIPPED: %s - Reason: Site not found', [ps.Name]));
          Continue;
        end;
        if s.noannounce and not console then
        begin
          Debug(dpSpam, section, Format('[TIMING DEBUG] SKIPPED: %s - Reason: noannounce setting', [ps.Name]));
          Continue;
        end;

        if ps.status in [rssRealPre, rssShouldPre] then
        begin
          if Result <> '' then
            Result := Result + ' ';

          Debug(dpSpam, section, Format('[TIMING DEBUG] Site: %s | Status: PRE (%d)',
            [ps.Name, Ord(ps.status)]));

          Result := Concat(Result, '<c9>', IntToStr(numComplete), '.</c> ', ps.Name, ' <c8>(PRE)</c>');
          inc(numComplete);
          Continue;
        end;

        if ps.status <> rssComplete then
        begin
          Debug(dpSpam, section, Format('[TIMING DEBUG] SKIPPED: %s - Reason: Status not rssComplete (Status: %d)',
            [ps.Name, Ord(ps.status)]));
          Continue;
        end;

        if ps.DirlistGaveUpAndSentNoFiles or (ps.dirlist.CompletedTime = 0) then
        begin
          Debug(dpSpam, section, Format('[TIMING DEBUG] SKIPPED: %s - Reason: GaveUp or no CompletedTime (GaveUp: %s, CompletedTime: %s)',
            [ps.Name, BoolToStr(ps.DirlistGaveUpAndSentNoFiles, True),
            FormatDateTime('hh:nn:ss.zzz', ps.dirlist.CompletedTime)]));
          Continue;
        end;

        if Result <> '' then
          Result := Result + ' ';

        secondsFromAddpre := MilliSecondsBetween(added, ps.dirlist.CompletedTime) / 1000.0;

        Debug(dpSpam, section, Format('[TIMING DEBUG] Site: %s | ADDPRE: %s | Completed: %s | Diff: %.3fs | Status: %d',
          [ps.Name, FormatDateTime('hh:nn:ss.zzz', added), FormatDateTime('hh:nn:ss.zzz', ps.dirlist.CompletedTime),
          secondsFromAddpre, Ord(ps.status)]));

        Result := Concat(Result, '<c9>', IntToStr(numComplete), '.</c> ', ps.Name, ' <c8>(', Format('%.3f', [secondsFromAddpre]), 's)</c>');
        inc(numComplete);
      except
        on e: Exception do
        begin
          Debug(dpSpam, section, Format('[EXCEPTION] TPazo.SiteCompleteTimesStats: %s', [e.Message]));
          Continue;
        end;
      end;
    end;

    Debug(dpSpam, section, Format('[TIMING DEBUG] Completed SiteCompleteTimesStats | Total sites included: %d | Result length: %d',
      [numComplete - 1, Length(Result)]));
  finally
    sitesSorted.Free;
  end;
end;

function TPazo.StatusText: String;
var
  ps: TPazoSite;
begin
  Result := '';
  for ps in PazoSitesList do
  begin
    Result := Result + ps.StatusText + ' ';
  end;

  // remove superfluous whitespace
  Result.TrimRight;
end;

procedure TPazo.Clear;
begin
  try
    RemovePazo(pazo_id, True);

    FExcludeFromIncfiller := False;
    stopped := False; // ha stoppoltak korabban akkor ez most szivas
    ready := False;
    readyerror := False;
    errorreason := '';
    FUniqueFileListOfRelease.Clear;
    PazoSitesList.Clear;

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
        if glPazoPreTimeLookupMode <> plmNone then
        begin
          if not (rls.pretime <> 0) then
            Continue;

          if not (s.IsPretimeOk(rls.section, rls.pretime)) then
            Continue;
        end;
      end;

      ps := TPazoSite.Create(self, s.Name, sectiondir, s);
      ps.status := rssNotAllowed;
      if not aIsSpreadJob then
      begin
        ps.DelaySetup;
      end;

      if s.IsAffil(rls.groupname) then
      begin
        ps.status := rssShouldPre;
      end;

      PazoSitesList.Add(ps);
      CheckSiteSlots(s);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazo.AddSites: %s', [e.Message]));
        Continue;
      end;
    end;

    Result := True;
  end;
end;

function TPazo.PFileSize(const aDir, aFilename: String): Int64;
var
  fKey: String;
begin
  fKey := aDir + '/' + LowerCase(aFilename);

  if not FUniqueFileListOfRelease.TryGetValue(fKey, Result) then
    Result := -1;
end;

procedure TPazo.SiteDown(const sitename: String);
var
  ps: TPazoSite;
begin
  ps := FindSite(sitename);
  if ps <> nil then
    ps.MarkSiteAsFailed('');
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
        Debug(dpError, section, Format('[EXCEPTION] TPazoSite.AddDestination: %s', [e.Message]));
        Result := False;
      end;
    end;
  end
  else
    pazo.errorreason := 'AddDest - PazoSite is NIL';
end;

function TPazoSite.AddDestination(const ps: TPazoSite; const rank: integer): boolean;
var
  fDestinationRank: TDestinationRank;
begin
  Result := False;
  if error = True then
    exit;

  try
    if ps <> nil then
    begin
      if ps.error then
        exit;

      destinations_cs.Enter;
      try
        for fDestinationRank in destinations do
        begin
          if fDestinationRank.FPazoSite.Name = ps.Name then
            exit; //already have this destination
        end;

        fDestinationRank := TDestinationRank.Create(ps, rank);
        destinations.Add(fDestinationRank);
        destinations.Sort;
      finally
        destinations_cs.Leave;
      end;
      Result := True;
    end
    else
      pazo.errorreason := 'AddDest - PazoSite is NIL';
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.AddDestination: %s', [e.Message]));
      Result := False;
    end;
  end;
end;

//compare function to sort by rank
function _CompareDestinationRanks({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right: TDestinationRank): Integer;
begin
  Result := TComparer<Integer>.Default.Compare(Right.FRank, Left.FRank); //descending
end;

constructor TPazoSite.Create(const aParentPazo: TPazo; const aName, aMaindir: String; const aSite: TObject = nil);
var
  fSite: TSite;
begin
  inherited Create;

  if aSite = nil then
    fSite := FindSiteByName('', aName)
  else
    fSite := TSite(aSite);

  maindir := aMaindir;
  pazo := aParentPazo;
  Name := aName;

  FActiveTransfers := TDictionary<string, string>.Create(GetCaseInsensitveStringComparer);
  FActiveTransfersCS := TCriticalSection.Create;
  ts := 0;
  firesourcesinstead := False;
  badcrcevents := 0;

  FDestinations := TList<TDestinationRank>.Create(TComparer<TDestinationRank>.Construct(_CompareDestinationRanks));
  destinations_cs := TCriticalSection.Create;

  dirlist := TDirlist.Create(Name, nil, pazo.sl, pazo.FPazoSFV);
  if dirlist <> nil then
  begin
    if pazo.rls <> nil then
      dirlist.FullPath := MyIncludeTrailingSlash(maindir) + MyIncludeTrailingSlash(pazo.rls.rlsname)
    else
      dirlist.FullPath := MyIncludeTrailingSlash(maindir);
  end;

  s_dirlisttasks := TIdThreadSafeInt32.Create;
  s_racetasks := TIdThreadSafeInt32.Create;
  s_mkdirtasks := TIdThreadSafeInt32.Create;

  speed_from := fSite.Speed_From;

  Debug(dpSpam, section, 'TPazoSite.Create: %s', [Name]);
end;

destructor TPazoSite.Destroy;
begin
  Debug(dpSpam, section, 'TPazoSite.Destroy: %s', [Name]);
  FActiveTransfers.Free;
  FActiveTransfersCS.Free;
  destinations.Free;
  destinations_cs.Free;
  dirlist.Free;
  s_dirlisttasks.Free;
  s_racetasks.Free;
  s_mkdirtasks.Free;
  speed_from.Free;
  inherited;
end;

function TPazoSite.MkdirReady(const dir: String): boolean;
var
  d: TDirList;
  fSiteName: String;
  fPazoSite: TPazoSite;
  fSitesList: TList<String>;
  fDestinationRank: TDestinationRank;
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
    d.dirlist_lock.Enter('TPazoSite.MkdirReady');
    try
      d.need_mkdir := False;
      d.dependency_mkdir := '';
    finally
      d.dirlist_lock.Leave;
    end;

    //fire the queue for all source sites
    fSitesList := TList<String>.Create;
    try
      //find all sites which have this site as destination
      for fPazoSite in self.pazo.PazoSitesList do
      begin
        for fDestinationRank in fPazoSite.FDestinations do
        begin
          if (fDestinationRank.FPazoSite.Name = self.Name) then
          begin
            if not fSitesList.Contains(fDestinationRank.PazoSite.Name) then
            begin
              fSitesList.Add(fDestinationRank.PazoSite.Name);
            end;
            break;
          end;
        end;
      end;

      for fSiteName in fSitesList do
      begin
        FindSiteByName('', fSiteName).QueueFire;
      end;
    finally
      fSitesList.Free;
    end

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
  de: TDirListEntry;
  fFoundDirListEntries, fRemovePazoRaceEntries: TObjectList<TDirListEntry>;
  fTasksAdded: boolean;
  fSite: TSite;
begin
  Result := False;

  // exit if no access to dirlist object
  if dirlist = nil then
    exit;

  // find the dirlist entry for this dir
  try
    d := dirlist.FindDirlist(dir, True);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDirlist (dirlist.FindDirlist): %s', [e.Message]));
      exit;
    end;
  end;

  // exit if dirlist not found for this dir
  if d = nil then
    exit;

  // parse the dirlist
  try
    d.ParseDirlist(liststring);
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


  // Do some stuff obviously
  if d.entries.Count > 0 then
  begin
    fFoundDirListEntries := TObjectList<TDirListEntry>.Create(False);
    fRemovePazoRaceEntries := TObjectList<TDirListEntry>.Create(False);
    try
      d.dirlist_lock.Enter('TPazoSite.ParseDirlist');
      try
        for de in d.entries.Values do
        begin
          if ((not de.skiplisted) and (de.IsOnSite)) then
          begin
            if not de.Directory then
            begin
              if (de.justadded) then
              begin
                de.justadded := False;
                fRemovePazoRaceEntries.Add(de);
              end;
            end;

            fFoundDirListEntries.Add(de);
          end;
        end;
      finally
        d.dirlist_lock.Leave;
      end;

      SortDirlistEntries(fFoundDirListEntries);

      //do this outside dirlist_lock to avoid deadlocks
      fTasksAdded := Tuzelj(netname, channel, dir, fFoundDirListEntries);

      if fTasksAdded then
      begin
        fSite := FindSiteByName('', Name);
        fSite.QueueSort;
        fSite.QueueFire;
      end;

      for de in fRemovePazoRaceEntries do
      begin
        RemovePazoRace(self, pazo.pazo_id, Name, dir, de.filename);
      end;

      for de in fFoundDirListEntries do
      begin
        if not de.Directory and de.FSizeChanged then
        begin
          pazo.PRegisterFile(dir, de);
          de.FSizeChanged := False;
        end;
      end;

    finally
      fFoundDirListEntries.Free;
      fRemovePazoRaceEntries.Free;
    end;
  end;

  // Everything went fine
  Result := True;
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

    dl.dirlist_lock.Enter('TPazoSite.SetFileError');
    try
      de := dl.Find(filename);
      if de <> nil then
      begin
        de.error := True;
      end
      else
      begin
        de := TDirListEntry.Create(filename, dl, False);
        de.error := True;
        dl.entries.Add(de.filename, de);
      end;
    finally
      dl.dirlist_lock.Leave;
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

procedure TPazoSite.ParseDupe(const aNetname, aChannel: String; aDirlist: TDirlist; const aDir, aFilename: String; const aSentByMe, aIsComplete: boolean);
begin
  ParseDupe(aNetname, aChannel, aDirlist, aDir, [aFilename], aSentByMe, aIsComplete);
end;

procedure TPazoSite.ParseDupe(const aNetname, aChannel: String; aDirlist: TDirList; const aDir: string; const aFilenames: TArray<String>; const aSentByMe, aIsComplete: boolean);
var
  de: TDirListEntry;
  fTasksAdded: boolean;
  fFilesToRace: TList<TDirListEntry>;
  fFilename: string;
  fSite: TSite;
begin
  //Debug(dpSpam, section, '--> '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, aDir, aFilename]));
  fTasksAdded := False;
  fFilesToRace := TList<TDirListEntry>.Create;
  try
    try
      aDirlist.dirlist_lock.Enter('TPazoSite.ParseDupe');
      try
        for fFilename in aFilenames do
        begin
          if not aDirlist.IsValidFilenameCached(fFilename) then
            exit;

          de := aDirlist.Find(fFilename);
          if de = nil then
          begin
            // this means that it has not been fired
            de := TDirListEntry.Create(fFilename, aDirlist, False);
            de.filesize := -1;

            de.RegenerateSkiplist;
            aDirlist.entries.Add(de.filename, de);
            aDirlist.LastChanged := Now();
          end;

          if aIsComplete then
          begin
            if (de.filesize < 1) then
            begin
              //try to get the actual file size from pazo
              de.filesize := pazo.PFileSize(aDir, fFilename);

              if (de.filesize < 1) then
              begin
                //since the file is complete, it must have at least size 1.
                de.filesize := 1;
              end;
            end;
            de.IsBeingUploaded := False;
          end;

          if (de.IsSFV) then
          begin
            aDirlist.sfv_status := dlSFVFound;
          end;

          if aSentByMe then
            de.RacedByMe := aSentByMe;

          if (not de.IsOnSite) then
          begin
            de.IsOnSite := True;
            if not de.skiplisted then
              fFilesToRace.Add(de);
          end;
        end;
      finally
        aDirlist.dirlist_lock.Leave;
      end;

      SortDirlistEntries(fFilesToRace);

      //do this outside dirlist_lock to avoid deadlocks
      if fFilesToRace.Count > 0 then
      begin
        fTasksAdded := Tuzelj(aNetname, aChannel, aDir, fFilesToRace);
      end;

      if fTasksAdded then
      begin
        fSite := FindSiteByName('', Name);
        fSite.QueueSort;
        fSite.QueueFire;
      end;

      for de in fFilesToRace do
        RemovePazoRace(self, pazo.pazo_id, Name, aDir, de.filename);

    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
      end;
    end;
  finally
    fFilesToRace.Free;
  end;
  //Debug(dpSpam, section, '<-- '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, aDir, aFilename]));
end;

procedure TPazoSite.ParseDupe(const aNetname, aChannel, aDir, aFilename: String; const aSentByMe, aIsComplete: boolean);
var
  dl: TDirList;
begin
  try
    dl := dirlist.FindDirlist(aDir);
    if dl = nil then
    begin
      pazo.errorreason := 'Dirlist is NIL';
      exit;
    end;

    ParseDupe(aNetname, aChannel, dl, aDir, aFilename, aSentByMe, aIsComplete);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
    end;
  end;
end;

procedure TPazoSite.ProcessXDupeResponse(const aNetname, aChannel, aDir, aFullResponse: String);
var
  dl: TDirList;
  fFileList: TList<String>;
begin
  try
    dl := dirlist.FindDirlist(aDir);
    if dl = nil then
    begin
      pazo.errorreason := 'Dirlist is NIL';
      exit;
    end;

    fFileList := TList<String>.Create;
    try
      if not ParseXDupeResponseToFilenameList(aFullResponse, fFileList) then
        exit;

      ParseDupe(aNetname, aChannel, dl, aDir, fFileList.ToArray, False, False);
    finally
      fFileList.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ProcessXDupeResponse: %s', [e.Message]));
    end;
  end;
end;

function TPazoSite.Stats: String;
var
  fsize: double;
  fsname, fsizetrigger: String;
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
    if dirlist.FilesRacedByMe(True) >= 1 then
    begin
      // we send some files
      fsize := dirlist.SizeRacedByMe(True) / 1024;
      RecalcSizeValueAndUnit(fsize, fsizetrigger, 1);

      if not dirlist.Complete then
        fsname := Format('<c11>%s</c>', [fsname]); //Light Blue(name); || release is incomplete (0byte files, dupefiles, etc) but we raced some files

      Result := Format('%s-(<b>%d</b>F @ <b>%.2f</b>%s)', [fsname, dirlist.FilesRacedByMe(True), fsize, fsizetrigger]);

      // TODO: Find out why it is negative sometimes + try to fix
      // note: seems that de.filesize is negative as this is sum up in SizeRacedByMe() and shows -1 as result
      if fsize < 0 then
      begin
        Debug(dpError, section, Format('[NEGATIVE BYTES]: %f for %s with SizeRacedByMe(True) = %d, dirname : %s, full path : %s, CompleteDirTag : %s',
          [fsize, fsname, dirlist.SizeRacedByMe(True), dirlist.Dirname, dirlist.FullPath, dirlist.CompleteDirTag]));

        // get more infos about dirlist entries
        sum := 0;
        dirlist.dirlist_lock.Enter('TPazoSite.Stats');
        try
          for de in dirlist.entries.Values do
          begin
            try
              if (de.RacedByMe and not de.IsAsciiFiletype) then
                Inc(sum, de.filesize);
              //if ((de.directory) and (de.subdirlist <> nil)) then inc(sum, de.subdirlist.SizeRacedByMe(True));

              Debug(dpError, section, Format('%s -- filename %s filesize %d byme %s IsAsciiFiletype %s (sum: %d)',
                [fsname, de.filename, de.filesize, BoolToStr(de.RacedByMe, True), BoolToStr(de.IsAsciiFiletype, True), sum]));
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Stats: %s', [e.Message]));
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
      d.CachedCompleteResult := True;

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
  fDestination: TDestinationRank;
begin
  Result := '<u><b>SITE: ' + Name + '</b></u>';
  Result := Result + Format(': %s (%d items)', [maindir, dirlist.entries.Count]);

  if (dirlist.Complete) then
  begin
    Result := Result + ' <b>COMPLETE</b>';
  end;
  Result := Result + #13#10;

  Result := Result + 'Destinations:';
  for fDestination in destinations do
  begin
    Result := Result + Format(' %s(%d)', [fDestination.FPazoSite.Name, fDestination.FRank]);
  end;
  Result := Result + #13#10;

  if ((dirlist.GetCompleteInfo <> 'Not Complete') and (not StatusRealPreOrShouldPre)) then
  begin
    Result := Result + Format('Completion Time: %s via %s', [TimeToStr(dirlist.CompletedTime), dirlist.GetCompleteInfo]);
    Result := Result + #13#10;
  end;

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
  fDestination: TDestinationRank;
begin
  Result := '<u>' + Name + '</u> -> ';

  for fDestination in destinations do
  begin
    Result := Result + Format('%s(%d) ', [fDestination.FPazoSite.Name, fDestination.FRank]);

    if (fDestination.FPazoSite.delay_upload > 0) then
      Result := Result + Format('[delayed upload for %ds] ', [fDestination.FPazoSite.delay_upload])
    else if (fDestination.FPazoSite.delay_leech > 0) then
      Result := Result + Format('[delayed leech for %ds] ', [fDestination.FPazoSite.delay_leech]);
  end;

  // remove superfluous whitespace
  Result.TrimRight;

  Result := Result + #13#10;
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
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Clear: %s', [e.Message]));
    end;
  end;
end;

procedure TPazoSite.MarkSiteAsFailed(const aMessage: string);
begin
  error := True;
  Debug(dpSpam, section, Format('--> TPazoSite.MarkSiteAsFailed', []));

  try
    dirlistgaveup := True;

    RemoveRaceTasks(pazo.pazo_id, Name);
    RemovePazoDirTasks(pazo.pazo_id, Name);

    if not aMessage.IsEmpty then
      irc_Addstats(Format('<c7>[SITE FAILED]</c> : %s %s @ <b>%s</b> <b>%s</b>', [pazo.rls.section, pazo.rls.rlsname, Name, aMessage]));

  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.MarkSiteAsFailed: %s', [e.Message]));
    end;
  end;

  Debug(dpSpam, section, Format('<-- TPazoSite.MarkSiteAsFailed', []));
end;

function TPazoSite.DirlistGaveUpAndSentNoFiles: Boolean;
begin
  Result := dirlistgaveup and (dirlist.FilesRacedByMe(False) = 0);
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

function TPazoSite.GetActiveTransferCount;
begin
  Result := FActiveTransfers.Count;
end;

procedure TPazoSite.RemoveActiveTransfer(const aFilepath: String);
begin
  FActiveTransfersCS.Enter;
  try
    try
      FActiveTransfers.Remove(aFilepath);
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TSite.RemoveActiveTransfer: %s', [E.Message]));
      end;
    end;
  finally
    FActiveTransfersCS.Leave;
  end;
end;

function TPazoSite.HasActiveTransfer(const aFilepath: String): boolean;
begin
  FActiveTransfersCS.Enter;
  try
    Result := FActiveTransfers.ContainsKey(aFilepath);
  finally
    FActiveTransfersCS.Leave;
  end;
end;

function TPazoSite.HasActiveTransfer(const aFilepath, aSourceSite: String): boolean;
var
  fSourceSiteName: string;
begin
  FActiveTransfersCS.Enter;
  try
    Result := FActiveTransfers.TryGetValue(aFilepath, fSourceSiteName) and (fSourceSiteName = aSourceSite);
  finally
    FActiveTransfersCS.Leave;
  end;
end;

procedure TPazoSite.AddActiveTransfer(const aFilepath, aSourceSite: String);
begin
  FActiveTransfersCS.Enter;
  try
    if not FActiveTransfers.TryAdd(aFilepath, aSourceSite) then
    begin
      Debug(dpError, section, Format('[WARN] TPazoSite.AddActiveTransfer: Tried to add active transfer, but it was already there %s: %s', [Name, aFilepath]));
    end;
  finally
    FActiveTransfersCS.Leave;
  end;
end;

end.
