{
  @abstract(In-memory per-release performance timeline (race timings))

  Records timing markers for a raced release (TPazo): when it was detected,
  first dirlist per site, mkdir created/done per site, race task counts,
  queue wait and complete timestamps. Everything is kept in memory on the
  TPazo instance and is gone with it; nothing is written to disk or database.
  All timestamps are microseconds from QueryPerformanceMicroSeconds,
  0 means "not set". All public methods are thread-safe and never raise.
}
unit raceperfunit;

interface

uses
  Classes, SysUtils, Generics.Collections, slcriticalsection2;

type
  { Timing markers and counters for one site of a raced release.
    Instances are owned by @link(TRacePerf); do not access them from outside,
    use the marker methods of @link(TRacePerf) instead. }
  TRacePerfSiteInfo = class
  public
    SiteName: String; //< name of the site
    FirstDirlistCreatedUs: Int64; //< first dirlist task created for this site
    FirstDirlistCreatedInfo: String; //< what triggered the first dirlist task (kb event, 'tuzelj', 'incfiller')
    FirstDirlistParsedUs: Int64; //< first dirlist answer successfully parsed from this site
    DirlistTasksCreated: integer; //< number of created dirlist tasks
    DirlistErrors: integer; //< dirlist tasks which finished with an error
    MkdirCreatedUs: Int64; //< first mkdir task created for this site
    MkdirDoneUs: Int64; //< first mkdir successfully done on this site
    MkdirErrors: integer; //< mkdir tasks which failed
    RaceTasksCreated: integer; //< race tasks created with this site as destination
    FirstRaceCreatedUs: Int64; //< first race task created with this site as destination
    FirstRaceAssignedUs: Int64; //< first race task got slots assigned by the queue thread
    FirstRaceStartedUs: Int64; //< first race task started executing on its slots
    RacesFinishedOk: integer; //< race tasks which finished successfully
    RaceErrors: integer; //< race tasks which finished with an error
    CompleteUs: Int64; //< release detected as complete on this site
  end;

  { Thread-safe in-memory performance timeline of one raced release }
  TRacePerf = class
  private
    fLock: TSlCriticalSection2; //< protects all fields and @link(fSites)
    fSites: TObjectDictionary<String, TRacePerfSiteInfo>; //< per-site markers, key is the uppercase sitename
    fDetectedUs: Int64; //< T0: release detected for trading (pazo created)
    fFirstDirlistCreatedUs: Int64; //< first dirlist task created on any site
    fFirstRaceCreatedUs: Int64; //< first race task created on any site
    fFirstRaceAssignedUs: Int64; //< first race task assigned on any site
    fFirstRaceStartedUs: Int64; //< first race task started on any site
    fAllTasksIdleUs: Int64; //< queue of the pazo ran empty (queuenumber reached 0)
    { @returns(the site info for @link(aSiteName), creating it on first use; caller must hold @link(fLock)) }
    function GetSiteLocked(const aSiteName: String): TRacePerfSiteInfo;
    { @returns(@link(aUs) formatted relative to @link(fDetectedUs), e.g. '+123.456 ms', or '-' if unset) }
    function FormatRelUs(const aUs: Int64): String;
  public
    constructor Create(const aDetectedUs: Int64);
    destructor Destroy; override;

    { Current time in the same unit as the stored timestamps
      @returns(microseconds from QueryPerformanceMicroSeconds) }
    class function NowMicroSeconds: Int64;

    { A dirlist task was created for @link(aSiteName)
      @param(aInfo what triggered the creation, e.g. the kb event name, 'tuzelj' or 'incfiller')
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkDirlistCreated(const aSiteName: String; const aInfo: String = ''; const aNowUs: Int64 = 0);
    { A dirlist answer from @link(aSiteName) was successfully parsed
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkDirlistParsed(const aSiteName: String; const aNowUs: Int64 = 0);
    { A dirlist task for @link(aSiteName) finished with an error }
    procedure MarkDirlistError(const aSiteName: String);
    { A mkdir task was created for @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkMkdirCreated(const aSiteName: String; const aNowUs: Int64 = 0);
    { A mkdir task finished successfully on @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkMkdirDone(const aSiteName: String; const aNowUs: Int64 = 0);
    { A mkdir task failed on @link(aSiteName) }
    procedure MarkMkdirError(const aSiteName: String);
    { A race task was created with @link(aSiteName) as destination
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkRaceTaskCreated(const aSiteName: String; const aNowUs: Int64 = 0);
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
    { The release was detected as complete on @link(aSiteName)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkComplete(const aSiteName: String; const aNowUs: Int64 = 0);
    { The queue of the pazo ran empty (no open tasks left)
      @param(aNowUs explicit timestamp for testing, 0 means "use current time") }
    procedure MarkAllTasksIdle(const aNowUs: Int64 = 0);

    { Formats the whole timeline as text lines (for the releaseperf IRC command)
      @returns(a string list with one entry per line, caller must free it) }
    function AsStrings: TStringList;

    property DetectedUs: Int64 read fDetectedUs; //< T0 timestamp, all formatted times are relative to it
  end;

implementation

uses
  debugunit, mormot.core.os;

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

class function TRacePerf.NowMicroSeconds: Int64;
begin
  QueryPerformanceMicroSeconds(Result);
end;

constructor TRacePerf.Create(const aDetectedUs: Int64);
begin
  fDetectedUs := aDetectedUs;
  fLock := TSlCriticalSection2.Create('raceperf');
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

procedure TRacePerf.MarkDirlistCreated(const aSiteName: String; const aInfo: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkDirlistCreated');
    try
      if fFirstDirlistCreatedUs = 0 then
        fFirstDirlistCreatedUs := fNow;
      with GetSiteLocked(aSiteName) do
      begin
        if FirstDirlistCreatedUs = 0 then
        begin
          FirstDirlistCreatedUs := fNow;
          FirstDirlistCreatedInfo := aInfo;
        end;
        Inc(DirlistTasksCreated);
      end;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistCreated: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistParsed(const aSiteName: String; const aNowUs: Int64);
var
  fNow: Int64;
begin
  try
    fNow := aNowUs;
    if fNow = 0 then
      fNow := NowMicroSeconds;
    fLock.Enter('MarkDirlistParsed');
    try
      with GetSiteLocked(aSiteName) do
        if FirstDirlistParsedUs = 0 then
          FirstDirlistParsedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistParsed: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkDirlistError(const aSiteName: String);
begin
  try
    fLock.Enter('MarkDirlistError');
    try
      Inc(GetSiteLocked(aSiteName).DirlistErrors);
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkDirlistError: %s', [E.Message]);
  end;
end;

procedure TRacePerf.MarkMkdirCreated(const aSiteName: String; const aNowUs: Int64);
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
          MkdirCreatedUs := fNow;
    finally
      fLock.Leave;
    end;
  except
    on E: Exception do
      Debug(dpError, section, 'MarkMkdirCreated: %s', [E.Message]);
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

procedure TRacePerf.MarkComplete(const aSiteName: String; const aNowUs: Int64);
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
          CompleteUs := fNow;
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

function TRacePerf.AsStrings: TStringList;
var
  fSite: TRacePerfSiteInfo;
  fLine, fMkdirWait, fRaceWait, fDirlistInfo: String;
begin
  Result := TStringList.Create;
  fLock.Enter('AsStrings');
  try
    Result.Add(Format('Global: first dirlist task %s | first race created %s | first race assigned %s | first race started %s | all tasks done %s',
      [FormatRelUs(fFirstDirlistCreatedUs), FormatRelUs(fFirstRaceCreatedUs), FormatRelUs(fFirstRaceAssignedUs),
       FormatRelUs(fFirstRaceStartedUs), FormatRelUs(fAllTasksIdleUs)]));

    for fSite in fSites.Values do
    begin
      if fSite.FirstDirlistCreatedInfo <> '' then
        fDirlistInfo := Format(' via %s', [fSite.FirstDirlistCreatedInfo])
      else
        fDirlistInfo := '';
      fLine := Format('%s: dirlist %s%s (parsed %s, %d tasks, %d err)', [fSite.SiteName,
        FormatRelUs(fSite.FirstDirlistCreatedUs), fDirlistInfo, FormatRelUs(fSite.FirstDirlistParsedUs),
        fSite.DirlistTasksCreated, fSite.DirlistErrors]);

      if ((fSite.MkdirCreatedUs <> 0) or (fSite.MkdirErrors > 0)) then
      begin
        if ((fSite.MkdirCreatedUs <> 0) and (fSite.MkdirDoneUs <> 0)) then
          fMkdirWait := _FormatUsAsMs(fSite.MkdirDoneUs - fSite.MkdirCreatedUs) + ' ms'
        else
          fMkdirWait := '-';
        fLine := fLine + Format(' | mkdir %s -> done %s (waited %s, %d err)',
          [FormatRelUs(fSite.MkdirCreatedUs), FormatRelUs(fSite.MkdirDoneUs), fMkdirWait, fSite.MkdirErrors]);
      end;

      if ((fSite.RaceTasksCreated > 0) or (fSite.RacesFinishedOk > 0) or (fSite.RaceErrors > 0)) then
      begin
        if ((fSite.FirstRaceCreatedUs <> 0) and (fSite.FirstRaceAssignedUs <> 0) and (fSite.FirstRaceAssignedUs >= fSite.FirstRaceCreatedUs)) then
          fRaceWait := _FormatUsAsMs(fSite.FirstRaceAssignedUs - fSite.FirstRaceCreatedUs) + ' ms'
        else
          fRaceWait := '-';
        fLine := fLine + Format(' | races %d created (first %s), assigned %s (queue wait %s), started %s, %d ok / %d err',
          [fSite.RaceTasksCreated, FormatRelUs(fSite.FirstRaceCreatedUs), FormatRelUs(fSite.FirstRaceAssignedUs), fRaceWait,
           FormatRelUs(fSite.FirstRaceStartedUs), fSite.RacesFinishedOk, fSite.RaceErrors]);
      end;

      fLine := fLine + Format(' | complete %s', [FormatRelUs(fSite.CompleteUs)]);

      Result.Add(fLine);
    end;
  finally
    fLock.Leave;
  end;
end;

end.
