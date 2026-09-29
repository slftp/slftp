{
  @abstract(In-memory per-release performance timeline (race timings))

  Records timing markers for a raced release (TPazo): when it was detected,
  first dirlist per site and per dir, mkdir created/done per site, race task
  counts, queue wait and complete timestamps. Everything is kept in memory on
  the TPazo instance and is gone with it; nothing is written to disk or
  database. All timestamps are microseconds from QueryPerformanceMicroSeconds,
  0 means "not set". Marker updates and snapshots are protected by a lock;
  marker methods catch and log exceptions.
}
unit raceperfunit;

interface

uses
  Classes, SysUtils, Generics.Collections, slcriticalsection2;

type
  { Reason why a race slot assignment was rejected as busy. }
  TRacePerfBusyReason = (
    rpbrMaxSimUp, //< destination upload cooldown
    rpbrMaxSimDown, //< source download cooldown
    rpbrBusyDestination, //< destination was already marked busy in this queue
    rpbrAssignmentLock, //< destination assignment lock could not be acquired
    rpbrActiveTransfer, //< file is already being transferred to the destination
    rpbrReverseTransfer //< file is already being transferred along the reverse route
  );

  { Timing markers for the dirlist tasks of one dir of one site of a raced
    release. Instances are owned by @link(TRacePerfSiteInfo); do not access
    them from outside, use the marker methods of @link(TRacePerf) instead. }
  TRacePerfDirInfo = class
  private
    FDir: String; //< dir inside the release, '' is the main dir
    FCreatedUs: Int64; //< first dirlist task created for this dir
    FCreatedInfo: String; //< what triggered the first dirlist task for this dir (kb event, 'tuzelj', 'subdir', 'readd', 'incfiller')
    FStartedUs: Int64; //< first dirlist task for this dir started executing on a slot
    FParsedUs: Int64; //< first dirlist answer for this dir successfully parsed
    FTasksCreated: integer; //< number of created dirlist tasks for this dir
    FTasksExecuted: integer; //< dirlist tasks which entered slot execution
    FTasksDupDropped: integer; //< dirlist tasks rejected by the queue as duplicates
    FErrors: integer; //< dirlist tasks for this dir which finished with an error
  end;

  { Timing markers and counters for one site of a raced release.
    Instances are owned by @link(TRacePerf); do not access them from outside,
    use the marker methods of @link(TRacePerf) instead. }
  TRacePerfSiteInfo = class
  private
    FDirlistTasksExecuted: integer; //< dirlist tasks which entered slot execution
    FDirlistTasksDupDropped: integer; //< dirlist tasks rejected by the queue as duplicates
    FCompleteSource: String; //< origin of the first complete marker
    FAssignBusyReasons: array[TRacePerfBusyReason] of integer; //< busy attempts by cause
    FDirInfos: TObjectDictionary<String, TRacePerfDirInfo>; //< per-dir dirlist markers, key is the uppercase dir ('' is the main dir)
    FMkdirCreatedInfo: String; //< what triggered the first mkdir task ('tuzelj', 'dirlist550')
    FAssignBlockedNoSlot: integer; //< task assignments rejected because this site had no free/online slot or a transfer limit (max_up/max_dn/maxupperrip) was reached
    FAssignBlockedBusy: integer; //< task assignments rejected because the site was busy (maxsim cooldown, busy destination, assignment lock contention, file already being transferred)
  public
    SiteName: String; //< name of the site
    FirstDirlistCreatedUs: Int64; //< first dirlist task created for this site
    FirstDirlistCreatedInfo: String; //< what triggered the first dirlist task (kb event, 'tuzelj', 'incfiller')
    FirstDirlistStartedUs: Int64; //< first dirlist task started executing on a slot of this site
    FirstDirlistParsedUs: Int64; //< first dirlist answer successfully parsed from this site
    DirlistTasksCreated: integer; //< number of created dirlist tasks
    DirlistErrors: integer; //< dirlist tasks which finished with an error
    MkdirCreatedUs: Int64; //< first mkdir task created for this site
    MkdirStartedUs: Int64; //< first mkdir task started executing on a slot of this site
    MkdirDoneUs: Int64; //< first mkdir successfully done on this site
    MkdirErrors: integer; //< mkdir tasks which failed
    RaceTasksCreated: integer; //< race tasks created with this site as destination
    RaceTasksDupDropped: integer; //< race tasks dropped as duplicates by AddTask (already in queue)
    FirstRaceCreatedUs: Int64; //< first race task created with this site as destination
    FirstRaceAssignedUs: Int64; //< first race task got slots assigned by the queue thread
    FirstRaceStartedUs: Int64; //< first race task started executing on its slots
    RacesFinishedOk: integer; //< race tasks which finished successfully
    RaceErrors: integer; //< race tasks which finished with an error
    CompleteUs: Int64; //< release detected as complete on this site
    { Creates the owned directory marker dictionary. }
    constructor Create;
    { Frees the owned directory markers. }
    destructor Destroy; override;
  end;

  { Thread-safe in-memory performance timeline of one raced release }
  TRacePerf = class
  private
    fLock: TSlCriticalSection2; //< protects all fields and @link(fSites)
    fSites: TObjectDictionary<String, TRacePerfSiteInfo>; //< per-site markers, key is the uppercase sitename
    fDetectedUs: Int64; //< T0: release detected for trading (pazo created)
    fDetectedInfo: String; //< kb event which detected the release (e.g. 'NEWDIR', 'ADDPRE')
    fFirstDirlistCreatedUs: Int64; //< first dirlist task created on any site
    fFirstRaceCreatedUs: Int64; //< first race task created on any site
    fFirstRaceAssignedUs: Int64; //< first race task assigned on any site
    fFirstRaceStartedUs: Int64; //< first race task started on any site
    fAllTasksIdleUs: Int64; //< queue of the pazo ran empty (queuenumber reached 0)
    fTuzeljCalls: integer; //< how often TPazoSite.Tuzelj ran for this pazo
    fTuzeljTotalUs: Int64; //< total time spent in TPazoSite.Tuzelj for this pazo
    { @returns(the site info for @link(aSiteName), creating it on first use; caller must hold @link(fLock)) }
    function GetSiteLocked(const aSiteName: String): TRacePerfSiteInfo;
    { @returns(the dir info of @link(aSite) for @link(aDir), creating it on first use; caller must hold @link(fLock)) }
    function GetDirLocked(const aSite: TRacePerfSiteInfo; const aDir: String): TRacePerfDirInfo;
    { @returns(@link(aUs) formatted relative to @link(fDetectedUs), e.g. '+123.456 ms', or '-' if unset) }
    function FormatRelUs(const aUs: Int64): String;
  public
    constructor Create(const aDetectedUs: Int64; const aDetectedInfo: String = '');
    destructor Destroy; override;

    { Current time in the same unit as the stored timestamps
      @returns(microseconds from QueryPerformanceMicroSeconds) }
    class function NowMicroSeconds: Int64;

    { A dirlist task was created for @link(aSiteName)
      @param(aDir dir inside the release, '' is the main dir)
      @param(aInfo what triggered the creation, e.g. the kb event name, 'tuzelj', 'subdir', 'readd' or 'incfiller')
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkDirlistCreated(const aSiteName: String; const aDir: String = ''; const aInfo: String = ''; const aNowUs: Int64 = 0);
    { A nonempty dirlist for @link(aSiteName) finished parsing and follow-up processing
      @param(aDir dir inside the release, '' is the main dir)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkDirlistParsed(const aSiteName: String; const aDir: String = ''; const aNowUs: Int64 = 0);
    { A dirlist task entered execution on a slot of @link(aSiteName); increments the execution count
      @param(aDir dir inside the release, '' is the main dir)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkDirlistStarted(const aSiteName: String; const aDir: String = ''; const aNowUs: Int64 = 0);
    { A dirlist task was discarded as a duplicate before slot execution
      @param(aDir dir inside the release) }
    procedure MarkDirlistDupDropped(const aSiteName: String; const aDir: String);
    { A dirlist task for @link(aSiteName) finished with an error
      @param(aDir dir inside the release, '' is the main dir) }
    procedure MarkDirlistError(const aSiteName: String; const aDir: String = '');
    { A mkdir task was created for @link(aSiteName)
      @param(aInfo what triggered the creation, e.g. 'tuzelj' or 'dirlist550')
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkMkdirCreated(const aSiteName: String; const aInfo: String = ''; const aNowUs: Int64 = 0);
    { A mkdir task started executing on a slot of @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkMkdirStarted(const aSiteName: String; const aNowUs: Int64 = 0);
    { A mkdir task finished successfully on @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkMkdirDone(const aSiteName: String; const aNowUs: Int64 = 0);
    { A mkdir task failed on @link(aSiteName) }
    procedure MarkMkdirError(const aSiteName: String);
    { A race task was created with @link(aSiteName) as destination
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkRaceTaskCreated(const aSiteName: String; const aNowUs: Int64 = 0);
    { A race task with @link(aSiteName) as destination was dropped by AddTask
      because an identical task was already in the queue (duplicate) }
    procedure MarkRaceTaskDupDropped(const aSiteName: String);
    { A race task got slots assigned by the queue thread
      @param(aSiteName destination site)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkRaceAssigned(const aSiteName: String; const aNowUs: Int64 = 0);
    { A race task started executing
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkRaceStarted(const aSiteName: String; const aNowUs: Int64 = 0);
    { A race task finished
      @param(aSuccess @true if the transfer worked, @false on error) }
    procedure MarkRaceFinished(const aSiteName: String; const aSuccess: boolean);
    { A task assignment was rejected because @link(aSiteName) had no free or
      online slot left or a transfer limit (max_up/max_dn/maxupperrip) was reached }
    procedure MarkAssignBlockedNoSlot(const aSiteName: String);
    { A task assignment was rejected because a site was busy: maxsim cooldown,
      busy destination, slots assignment lock contention or the file is already
      being transferred
      @param(aReason cause of this rejected assignment attempt) }
    procedure MarkAssignBlockedBusy(const aSiteName: String; const aReason: TRacePerfBusyReason);
    { The release was detected as complete on @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time")
      @param(aSource origin of the event; stored with the first timestamp only) }
    procedure MarkComplete(const aSiteName: String; const aNowUs: Int64 = 0; const aSource: String = 'unknown');
    { The queue of the pazo ran empty (no open tasks left)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkAllTasksIdle(const aNowUs: Int64 = 0);
    { One TPazoSite.Tuzelj run finished
      @param(aDurationUs how long the Tuzelj run took in microseconds) }
    procedure MarkTuzeljDone(const aDurationUs: Int64);

    { Formats the whole timeline as text lines (for the releaseperf IRC command)
      @returns(a string list with one entry per line, caller must free it) }
    function AsStrings: TStringList;

    property DetectedUs: Int64 read fDetectedUs; //< T0 timestamp, all formatted times are relative to it
    property DetectedInfo: String read fDetectedInfo; //< kb event which detected the release
  end;

implementation

uses
  debugunit, mormot.core.os, Generics.Defaults, Math;

const
  section = 'raceperf';

{ Formats a microsecond duration with invariant decimal separator
  @param(aUs microseconds)
  @param(aDigits fraction digits)
  @returns(the duration as milliseconds string, e.g. '123.456') }
function _FormatUsAsMs(const aUs: Int64; const aDigits: integer = 3): String;
var
  fFormatSettings: TFormatSettings;
begin
  {$IFDEF FPC}
    fFormatSettings := DefaultFormatSettings;
  {$ELSE}
    fFormatSettings := FormatSettings;
  {$ENDIF}
  fFormatSettings.DecimalSeparator := '.';
  Result := FloatToStrF(aUs / 1000, ffFixed, 15, aDigits, fFormatSettings);
end;

{ Compares two @link(TRacePerfDirInfo) by their creation timestamp (used to sort the per-dir output) }
function _CompareDirInfos({$IFDEF FPC}constref{$ELSE}const{$ENDIF} aLeft, aRight: TRacePerfDirInfo): integer;
begin
  Result := CompareValue(aLeft.FCreatedUs, aRight.FCreatedUs);
end;

class function TRacePerf.NowMicroSeconds: Int64;
begin
  QueryPerformanceMicroSeconds(Result);
end;

constructor TRacePerfSiteInfo.Create;
begin
  FDirInfos := TObjectDictionary<String, TRacePerfDirInfo>.Create([doOwnsValues]);
  inherited Create;
end;

destructor TRacePerfSiteInfo.Destroy;
begin
  FDirInfos.Free;
  inherited Destroy;
end;

constructor TRacePerf.Create(const aDetectedUs: Int64; const aDetectedInfo: String);
begin
  fDetectedUs := aDetectedUs;
  fDetectedInfo := aDetectedInfo;
  fLock := TSlCriticalSection2.Create('raceperf_' + IntToHex(NativeUInt(Self), SizeOf(Pointer) * 2));
  fSites := TObjectDictionary<String, TRacePerfSiteInfo>.Create([doOwnsValues]);
  inherited Create;
end;

destructor TRacePerf.Destroy;
begin
  fSites.Free;
  fLock.Free;
  inherited Destroy;
end;

function TRacePerf.GetSiteLocked(const aSiteName: String): TRacePerfSiteInfo;
begin
  if not fSites.TryGetValue(UpperCase(aSiteName), Result) then
  begin
    Result := TRacePerfSiteInfo.Create;
    Result.SiteName := aSiteName;
    fSites.Add(UpperCase(aSiteName), Result);
  end;
end;

function TRacePerf.GetDirLocked(const aSite: TRacePerfSiteInfo; const aDir: String): TRacePerfDirInfo;
begin
  if not aSite.FDirInfos.TryGetValue(UpperCase(aDir), Result) then
  begin
    Result := TRacePerfDirInfo.Create;
    Result.FDir := aDir;
    aSite.FDirInfos.Add(UpperCase(aDir), Result);
  end;
end;

function TRacePerf.FormatRelUs(const aUs: Int64): String;
var
  fDelta: Int64;
begin
  if ((aUs = 0) or (fDetectedUs = 0)) then
  begin
    Result := '-';
    exit;
  end;

  fDelta := aUs - fDetectedUs;
  if fDelta < 0 then
    fDelta := 0;

  if fDelta < 10000000 then
    Result := '+' + _FormatUsAsMs(fDelta) + ' ms'
  else
    Result := '+' + _FormatUsAsMs(fDelta div 1000) + ' s';
end;

procedure TRacePerf.MarkDirlistCreated(const aSiteName: String; const aDir: String; const aInfo: String; const aNowUs: Int64);
var
  fNow: Int64;
  fSite: TRacePerfSiteInfo;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkDirlistCreated');
    try
      if fFirstDirlistCreatedUs = 0 then
        fFirstDirlistCreatedUs := fNow;
      fSite := GetSiteLocked(aSiteName);
      with fSite do
      begin
        if FirstDirlistCreatedUs = 0 then
        begin
          FirstDirlistCreatedUs := fNow;
          FirstDirlistCreatedInfo := aInfo;
        end;
        Inc(DirlistTasksCreated);
      end;
      with GetDirLocked(fSite, aDir) do
      begin
        if FCreatedUs = 0 then
        begin
          FCreatedUs := fNow;
          FCreatedInfo := aInfo;
        end;
        Inc(FTasksCreated);
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistCreated: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistStarted(const aSiteName: String; const aDir: String; const aNowUs: Int64);
var
  fNow: Int64;
  fSite: TRacePerfSiteInfo;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkDirlistStarted');
    try
      fSite := GetSiteLocked(aSiteName);
      Inc(fSite.FDirlistTasksExecuted);
      if fSite.FirstDirlistStartedUs = 0 then
        fSite.FirstDirlistStartedUs := fNow;
      with GetDirLocked(fSite, aDir) do
      begin
        Inc(FTasksExecuted);
        if FStartedUs = 0 then
          FStartedUs := fNow;
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistStarted: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistParsed(const aSiteName: String; const aDir: String; const aNowUs: Int64);
var
  fNow: Int64;
  fSite: TRacePerfSiteInfo;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkDirlistParsed');
    try
      fSite := GetSiteLocked(aSiteName);
      if fSite.FirstDirlistParsedUs = 0 then
        fSite.FirstDirlistParsedUs := fNow;
      with GetDirLocked(fSite, aDir) do
        if FParsedUs = 0 then
          FParsedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistParsed: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistDupDropped(const aSiteName: String; const aDir: String);
var
  fSite: TRacePerfSiteInfo;
begin
  try
    fLock.Enter('MarkDirlistDupDropped');
    try
      fSite := GetSiteLocked(aSiteName);
      Inc(fSite.FDirlistTasksDupDropped);
      Inc(GetDirLocked(fSite, aDir).FTasksDupDropped);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistDupDropped: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistError(const aSiteName: String; const aDir: String);
var
  fSite: TRacePerfSiteInfo;
begin
  try
    fLock.Enter('MarkDirlistError');
    try
      fSite := GetSiteLocked(aSiteName);
      Inc(fSite.DirlistErrors);
      Inc(GetDirLocked(fSite, aDir).FErrors);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistError: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkMkdirCreated(const aSiteName: String; const aInfo: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkMkdirCreated');
    try
      with GetSiteLocked(aSiteName) do
        if MkdirCreatedUs = 0 then
        begin
          MkdirCreatedUs := fNow;
          FMkdirCreatedInfo := aInfo;
        end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkMkdirCreated: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkMkdirStarted(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkMkdirStarted');
    try
      with GetSiteLocked(aSiteName) do
        if MkdirStartedUs = 0 then
          MkdirStartedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkMkdirStarted: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkMkdirDone(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkMkdirDone');
    try
      with GetSiteLocked(aSiteName) do
        if MkdirDoneUs = 0 then
          MkdirDoneUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkMkdirDone: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkMkdirError(const aSiteName: String);
begin
  try
    fLock.Enter('MarkMkdirError');
    try
      Inc(GetSiteLocked(aSiteName).MkdirErrors);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkMkdirError: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkRaceTaskCreated(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkRaceTaskCreated');
    try
      if fFirstRaceCreatedUs = 0 then
        fFirstRaceCreatedUs := fNow;
      with GetSiteLocked(aSiteName) do
      begin
        if FirstRaceCreatedUs = 0 then
          FirstRaceCreatedUs := fNow;
        Inc(RaceTasksCreated);
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkRaceTaskCreated: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkRaceTaskDupDropped(const aSiteName: String);
begin
  try
    fLock.Enter('MarkRaceTaskDupDropped');
    try
      Inc(GetSiteLocked(aSiteName).RaceTasksDupDropped);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkRaceTaskDupDropped: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkRaceAssigned(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkRaceAssigned');
    try
      if fFirstRaceAssignedUs = 0 then
        fFirstRaceAssignedUs := fNow;
      with GetSiteLocked(aSiteName) do
        if FirstRaceAssignedUs = 0 then
          FirstRaceAssignedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkRaceAssigned: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkRaceStarted(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkRaceStarted');
    try
      if fFirstRaceStartedUs = 0 then
        fFirstRaceStartedUs := fNow;
      with GetSiteLocked(aSiteName) do
        if FirstRaceStartedUs = 0 then
          FirstRaceStartedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkRaceStarted: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkRaceFinished(const aSiteName: String; const aSuccess: boolean);
begin
  try
    fLock.Enter('MarkRaceFinished');
    try
      if aSuccess then
        Inc(GetSiteLocked(aSiteName).RacesFinishedOk)
      else
        Inc(GetSiteLocked(aSiteName).RaceErrors);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkRaceFinished: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkAssignBlockedNoSlot(const aSiteName: String);
begin
  try
    fLock.Enter('MarkAssignBlockedNoSlot');
    try
      Inc(GetSiteLocked(aSiteName).FAssignBlockedNoSlot);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkAssignBlockedNoSlot: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkAssignBlockedBusy(const aSiteName: String; const aReason: TRacePerfBusyReason);
begin
  try
    fLock.Enter('MarkAssignBlockedBusy');
    try
      with GetSiteLocked(aSiteName) do
      begin
        Inc(FAssignBlockedBusy);
        Inc(FAssignBusyReasons[aReason]);
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkAssignBlockedBusy: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkComplete(const aSiteName: String; const aNowUs: Int64; const aSource: String);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkComplete');
    try
      with GetSiteLocked(aSiteName) do
        if CompleteUs = 0 then
        begin
          CompleteUs := fNow;
          FCompleteSource := aSource;
        end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkComplete: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkAllTasksIdle(const aNowUs: Int64);
begin
  try
    fLock.Enter('MarkAllTasksIdle');
    try
      if fAllTasksIdleUs = 0 then
      begin
        if aNowUs <> 0 then
          fAllTasksIdleUs := aNowUs
        else
          fAllTasksIdleUs := NowMicroSeconds;
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkAllTasksIdle: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkTuzeljDone(const aDurationUs: Int64);
begin
  try
    fLock.Enter('MarkTuzeljDone');
    try
      Inc(fTuzeljCalls);
      Inc(fTuzeljTotalUs, aDurationUs);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkTuzeljDone: %s', [E.Message]);
  end;
end;

function TRacePerf.AsStrings: TStringList;
var
  fSite: TRacePerfSiteInfo;
  fDirInfo: TRacePerfDirInfo;
  fDirInfos: TList<TRacePerfDirInfo>;
  fLine, fMkdirWait, fMkdirExec, fMkdirInfo, fRaceWait, fDirlistInfo, fGlobalLine, fDirName, fDirCreatedInfo: String;
begin
  Result := TStringList.Create;
  fLock.Enter('AsStrings');
  try
    fGlobalLine := Format('Global: first dirlist task %s | first race created %s | first race assigned %s | first race started %s | all tasks done %s',
      [FormatRelUs(fFirstDirlistCreatedUs), FormatRelUs(fFirstRaceCreatedUs), FormatRelUs(fFirstRaceAssignedUs),
       FormatRelUs(fFirstRaceStartedUs), FormatRelUs(fAllTasksIdleUs)]);

    if fTuzeljCalls > 0 then
      fGlobalLine := fGlobalLine + Format(' | tuzelj %d calls, total %s (avg %s)',
        [fTuzeljCalls, _FormatUsAsMs(fTuzeljTotalUs) + ' ms', _FormatUsAsMs(fTuzeljTotalUs div fTuzeljCalls) + ' ms']);

    Result.Add(fGlobalLine);

    for fSite in fSites.Values do
    begin
      if fSite.FirstDirlistCreatedInfo <> '' then
        fDirlistInfo := Format(' via %s', [fSite.FirstDirlistCreatedInfo])
      else
        fDirlistInfo := '';
      fLine := Format('%s: dirlist %s%s (started %s, parsed %s, %d created, %d executed, %d dup dropped, %d err)', [fSite.SiteName,
        FormatRelUs(fSite.FirstDirlistCreatedUs), fDirlistInfo, FormatRelUs(fSite.FirstDirlistStartedUs),
        FormatRelUs(fSite.FirstDirlistParsedUs),
        fSite.DirlistTasksCreated, fSite.FDirlistTasksExecuted, fSite.FDirlistTasksDupDropped, fSite.DirlistErrors]);

      if ((fSite.MkdirCreatedUs <> 0) or (fSite.MkdirStartedUs <> 0) or (fSite.MkdirErrors > 0)) then
      begin
        if fSite.FMkdirCreatedInfo <> '' then
          fMkdirInfo := Format(' via %s', [fSite.FMkdirCreatedInfo])
        else
          fMkdirInfo := '';
        if ((fSite.MkdirCreatedUs <> 0) and (fSite.MkdirStartedUs <> 0) and (fSite.MkdirStartedUs >= fSite.MkdirCreatedUs)) then
          fMkdirWait := _FormatUsAsMs(fSite.MkdirStartedUs - fSite.MkdirCreatedUs) + ' ms'
        else
          fMkdirWait := '-';
        if ((fSite.MkdirStartedUs <> 0) and (fSite.MkdirDoneUs <> 0) and (fSite.MkdirDoneUs >= fSite.MkdirStartedUs)) then
          fMkdirExec := _FormatUsAsMs(fSite.MkdirDoneUs - fSite.MkdirStartedUs) + ' ms'
        else
          fMkdirExec := '-';
        fLine := fLine + Format(' | mkdir %s%s -> started %s -> done %s (queue %s, exec %s, %d err)',
          [FormatRelUs(fSite.MkdirCreatedUs), fMkdirInfo, FormatRelUs(fSite.MkdirStartedUs), FormatRelUs(fSite.MkdirDoneUs),
           fMkdirWait, fMkdirExec, fSite.MkdirErrors]);
      end;

      if ((fSite.RaceTasksCreated > 0) or (fSite.RacesFinishedOk > 0) or (fSite.RaceErrors > 0) or (fSite.RaceTasksDupDropped > 0)) then
      begin
        if ((fSite.FirstRaceCreatedUs <> 0) and (fSite.FirstRaceAssignedUs <> 0) and (fSite.FirstRaceAssignedUs >= fSite.FirstRaceCreatedUs)) then
          fRaceWait := _FormatUsAsMs(fSite.FirstRaceAssignedUs - fSite.FirstRaceCreatedUs) + ' ms'
        else
          fRaceWait := '-';
        fLine := fLine + Format(' | races %d created (first %s), assigned %s (queue wait %s), started %s, %d ok / %d err',
          [fSite.RaceTasksCreated, FormatRelUs(fSite.FirstRaceCreatedUs), FormatRelUs(fSite.FirstRaceAssignedUs), fRaceWait,
           FormatRelUs(fSite.FirstRaceStartedUs), fSite.RacesFinishedOk, fSite.RaceErrors]);
        if fSite.RaceTasksDupDropped > 0 then
          fLine := fLine + Format(' | %d dup dropped', [fSite.RaceTasksDupDropped]);
      end;

      if ((fSite.FAssignBlockedNoSlot > 0) or (fSite.FAssignBlockedBusy > 0)) then
        fLine := fLine + Format(' | assign blocked %dx no slot / %dx busy', [fSite.FAssignBlockedNoSlot, fSite.FAssignBlockedBusy]);

      if fSite.FAssignBlockedBusy > 0 then
        fLine := fLine + Format(' (cooldown up %d / down %d, destination %d, lock %d, active file %d, reverse file %d)',
          [fSite.FAssignBusyReasons[rpbrMaxSimUp], fSite.FAssignBusyReasons[rpbrMaxSimDown],
           fSite.FAssignBusyReasons[rpbrBusyDestination], fSite.FAssignBusyReasons[rpbrAssignmentLock],
           fSite.FAssignBusyReasons[rpbrActiveTransfer], fSite.FAssignBusyReasons[rpbrReverseTransfer]]);
      fLine := fLine + Format(' | complete %s', [FormatRelUs(fSite.CompleteUs)]);
      if fSite.CompleteUs <> 0 then
        fLine := fLine + ' via ' + fSite.FCompleteSource;

      Result.Add(fLine);

      // per-dir dirlist timings, only shown when more than the main dir was listed
      if fSite.FDirInfos.Count > 1 then
      begin
        fDirInfos := TList<TRacePerfDirInfo>.Create;
        try
          for fDirInfo in fSite.FDirInfos.Values do
            fDirInfos.Add(fDirInfo);
          fDirInfos.Sort(TComparer<TRacePerfDirInfo>.Construct(_CompareDirInfos));
          for fDirInfo in fDirInfos do
          begin
            fDirName := fDirInfo.FDir;
            if fDirName = '' then
              fDirName := '/';
            if fDirInfo.FCreatedInfo <> '' then
              fDirCreatedInfo := Format(' via %s', [fDirInfo.FCreatedInfo])
            else
              fDirCreatedInfo := '';
            Result.Add(Format('  dir %s: created %s%s, started %s, parsed %s, %d created, %d executed, %d dup dropped, %d err',
              [fDirName, FormatRelUs(fDirInfo.FCreatedUs), fDirCreatedInfo, FormatRelUs(fDirInfo.FStartedUs),
               FormatRelUs(fDirInfo.FParsedUs), fDirInfo.FTasksCreated, fDirInfo.FTasksExecuted, fDirInfo.FTasksDupDropped, fDirInfo.FErrors]));
          end;
        finally
          fDirInfos.Free;
        end;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

end.
