unit loadmonitorunit;

interface

uses
  Classes, SysUtils, sltimer, slcriticalsection2, Contnrs
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  { Performance Level Listener Interface }
  IPerformanceLevelListener = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-123456789DEF}']
    procedure OnPerformanceLevelChanged(NewLevel: Integer);
  end;

  { Performance Level Change Event }
  TPerformanceLevelChangeEvent = procedure(Sender: TObject; NewLevel: Integer) of object;

  { Load Monitor - CPU & Performance Tracking }
  TLoadMonitor = class(TThread)
  private
    FCurrentPerformanceLevel: Integer;
    FCurrentCPUUsageTotal: Integer;
    FCurrentCPUUsageWorker: Integer;
    FCurrentWorkerQueueSize: Integer;
    FListeners: TInterfaceList;
    FLock: TSlCriticalSection2;
    FLastCPUCheck: TDateTime;
    FOnPerformanceLevelChange: TPerformanceLevelChangeEvent;
    FEnabled: Boolean;

    // CPU History (60 seconds @ 250ms = 240 samples)
    FCPUHistory: array[0..239] of Integer;
    FHistoryPosition: Integer;
    FHistoryCount: Integer;

    // CPU timing variables
    {$IFDEF MSWINDOWS}
    FLastIdleTime: Int64;
    FLastTotalTime: Int64;
  {$ELSE}
    FLastIdleJiffies: Int64;
    FLastTotalJiffies: Int64;
  {$ENDIF}

    procedure UpdateCPUUsage;
    procedure UpdatePerformanceLevel;
    procedure NotifyListeners(NewLevel: Integer);
    function GetAverageCPUUsage(Seconds: Integer): Integer;
    procedure AddCPUHistoryValue(Value: Integer);
    function CalculateCPUPercentage: Integer;

  protected
    procedure Execute; override;

  public
    constructor Create;
    destructor Destroy; override;

    // Properties
    property CurrentPerformanceLevel: Integer read FCurrentPerformanceLevel;
    property CurrentCPUUsageTotal: Integer read FCurrentCPUUsageTotal;
    property CurrentCPUUsageWorker: Integer read FCurrentCPUUsageWorker;
    property CurrentWorkerQueueSize: Integer read FCurrentWorkerQueueSize;
    property OnPerformanceLevelChange: TPerformanceLevelChangeEvent read FOnPerformanceLevelChange write FOnPerformanceLevelChange;
    property Enabled: Boolean read FEnabled write FEnabled;

    // Methods
    procedure AddListener(const Listener: IPerformanceLevelListener);
    procedure RemoveListener(const Listener: IPerformanceLevelListener);
    function GetCPUHistory(Seconds: Integer): TArray<Integer>;
    procedure StartMonitoring;
    procedure StopMonitoring;
  end;

// Global instance function
function GlLoadMonitor: TLoadMonitor;

// Check if LoadMonitor is available and running
function IsLoadMonitorAvailable: Boolean;

implementation

uses
  debugunit, configunit, queueunit, DateUtils, Math, sitesunit;

const
  section = 'loadmonitor';

  // Performance Level Constants
  MONITOR_INTERVAL_MS = 250;          // 250ms check interval
  HISTORY_LENGTH_SECONDS = 60;        // 60 seconds history
  HISTORY_SLOTS = 240;                // 60s * 4 checks/second

  PERFLEVEL_MIN = 1;
  PERFLEVEL_MAX = 9;
  PERFLEVEL_DEFAULT = 5;

  // CPU Thresholds
  CPU_THRESHOLD_HIGH = 75;            // Above 75% → reduce performance level
  CPU_THRESHOLD_LOW = 65;             // Below 65% → increase performance level
  CPU_THRESHOLD_TOLERANCE = 10;       // ±10% tolerance

var
  GlLoadMonitorInstance: TLoadMonitor = nil;
{$IFDEF MSWINDOWS}
type
  TGetSystemTimesFunc = function(lpIdleTime, lpKernelTime, lpUserTime: PFileTime): BOOL; stdcall;

var
  GetSystemTimesFunc: TGetSystemTimesFunc = nil;

function EnsureGetSystemTimesLoaded: Boolean;
var
  Kernel: HMODULE;
begin
  if Assigned(GetSystemTimesFunc) then
    Exit(True);

  Kernel := GetModuleHandle('kernel32.dll');
  if Kernel = 0 then
    Kernel := LoadLibrary('kernel32.dll');

  if Kernel <> 0 then
    GetSystemTimesFunc := TGetSystemTimesFunc(GetProcAddress(Kernel, 'GetSystemTimes'));

  Result := Assigned(GetSystemTimesFunc);
end;
{$ENDIF}

// Helper function to get current queue size
function GetCurrentQueueSize: Integer;
begin
  try
    Result := QueueTotalTaskCount;
  except
    on E: Exception do
    begin
      Debug(dpError, section, 'GetCurrentQueueSize error: %s', [E.Message]);
      Result := 0;
    end;
  end;
end;

function GlLoadMonitor: TLoadMonitor;
begin
  if not Assigned(GlLoadMonitorInstance) then
    GlLoadMonitorInstance := TLoadMonitor.Create;
  Result := GlLoadMonitorInstance;
end;

function IsLoadMonitorAvailable: Boolean;
begin
  Result := Assigned(GlLoadMonitorInstance) and GlLoadMonitorInstance.Enabled;
end;

{ TLoadMonitor }

constructor TLoadMonitor.Create;
begin
  inherited Create(True); // Create suspended

  FCurrentPerformanceLevel := PERFLEVEL_DEFAULT;
  FCurrentCPUUsageTotal := 0;
  FCurrentCPUUsageWorker := 0;
  FCurrentWorkerQueueSize := 0;
  FHistoryPosition := 0;
  FHistoryCount := 0;
  FEnabled := False;  // Will be enabled by StartMonitoring

  FListeners := TInterfaceList.Create;
  FLock := TSlCriticalSection2.Create('LoadMonitor');

  // Initialize CPU timing
  FLastCPUCheck := Now;
  {$IFDEF MSWINDOWS}
  FLastIdleTime := 0;
  FLastTotalTime := 0;
  {$ELSE}
  FLastIdleJiffies := 0;
  FLastTotalJiffies := 0;
  {$ENDIF}

  // Initialize history array
  FillChar(FCPUHistory, SizeOf(FCPUHistory), 0);

  Debug(dpMessage, section, 'LoadMonitor created with %dms interval, %ds history',
    [MONITOR_INTERVAL_MS, HISTORY_LENGTH_SECONDS]);
end;

destructor TLoadMonitor.Destroy;
begin
  Debug(dpMessage, section, 'LoadMonitor shutting down...');

  StopMonitoring;

  FreeAndNil(FListeners);
  FreeAndNil(FLock);

  inherited Destroy;
end;

procedure TLoadMonitor.Execute;
var
  shouldRun: Boolean;
begin
  Debug(dpMessage, section, 'LoadMonitor thread started');

  repeat
    // Thread-safe check if we should continue running
    FLock.Enter('Execute-Check');
    try
      shouldRun := FEnabled and not Terminated;
    finally
      FLock.Leave;
    end;

    if not shouldRun then
      Break;

    try
      // Update CPU usage
      UpdateCPUUsage;

      // Update performance level based on CPU
      UpdatePerformanceLevel;

      // Sleep for monitor interval
      Sleep(MONITOR_INTERVAL_MS);

    except
      on E: Exception do
      begin
        Debug(dpError, section, 'LoadMonitor exception: %s', [E.Message]);
        Sleep(1000); // Longer sleep on error
      end;
    end;
  until False;

  Debug(dpMessage, section, 'LoadMonitor thread stopped');
end;

procedure TLoadMonitor.UpdateCPUUsage;
var
  cpuPercent: Integer;
begin
  try
    cpuPercent := CalculateCPUPercentage;

    FLock.Enter('UpdateCPUUsage');
    try
      // Always update queue size, even if CPU calculation is pending
      FCurrentWorkerQueueSize := GetCurrentQueueSize;

      // Only update CPU values if we have a valid measurement
      if cpuPercent >= 0 then
      begin
        FCurrentCPUUsageTotal := cpuPercent;
        FCurrentCPUUsageWorker := cpuPercent; // For now, treat as same

        // Add to history
        AddCPUHistoryValue(cpuPercent);
      end;

    finally
      FLock.Leave;
    end;

    Debug(dpSpam, section, 'CPU: %d%%, Queue: %d, PerfLevel: %d',
      [cpuPercent, FCurrentWorkerQueueSize, FCurrentPerformanceLevel]);

  except
    on E: Exception do
      Debug(dpError, section, 'UpdateCPUUsage error: %s', [E.Message]);
  end;
end;

function TLoadMonitor.CalculateCPUPercentage: Integer;
{$IFDEF MSWINDOWS}
var
  idleTime, kernelTime, userTime: TFileTime;
  idle, kernel, user, total, deltaIdle, deltaTotal: Int64;
{$ELSE}
var
  statFile: TextFile;
  line: String;
  parts: TStringList;
  userJ, niceJ, systemJ, idleJ, iowaitJ, irqJ, softirqJ, stealJ: Int64;
  currentIdle, currentTotal, deltaIdle, deltaTotal: Int64;
{$ENDIF}
begin
  Result := 0;

  try
    {$IFDEF MSWINDOWS}
    if EnsureGetSystemTimesLoaded and GetSystemTimesFunc(@idleTime, @kernelTime, @userTime) then
    begin
      idle := (Int64(idleTime.dwHighDateTime) shl 32) or idleTime.dwLowDateTime;
      kernel := (Int64(kernelTime.dwHighDateTime) shl 32) or kernelTime.dwLowDateTime;
      user := (Int64(userTime.dwHighDateTime) shl 32) or userTime.dwLowDateTime;

      total := kernel + user;

      if FLastTotalTime = 0 then
      begin
        // First measurement: just store values, don't update history yet
        FLastTotalTime := total;
        FLastIdleTime := idle;
        Result := -1;  // Signal that CPU measurement is not ready yet
        Exit;
      end
      else
      begin
        deltaTotal := total - FLastTotalTime;
        deltaIdle := idle - FLastIdleTime;

        if (deltaTotal > 0) and (deltaIdle >= 0) and (deltaIdle <= deltaTotal) then
          Result := EnsureRange(Round((deltaTotal - deltaIdle) * 100 / deltaTotal), 0, 100)
        else
          Result := FCurrentCPUUsageTotal;  // Use previous value on error

        FLastTotalTime := total;
        FLastIdleTime := idle;
      end;
    end
    else
      Exit;
    {$ELSE}
    AssignFile(statFile, '/proc/stat');
    try
      Reset(statFile);
      ReadLn(statFile, line); // first line: cpu ...
      CloseFile(statFile);
    except
      on E: Exception do
      begin
        Debug(dpError, section, 'Linux CPU read error: %s', [E.Message]);
        Exit;
      end;
    end;

    parts := TStringList.Create;
    try
      parts.Delimiter := ' ';
      parts.StrictDelimiter := False;
      parts.DelimitedText := Trim(line);

      if parts.Count >= 8 then
      begin
        userJ := StrToInt64Def(parts[1], 0);
        niceJ := StrToInt64Def(parts[2], 0);
        systemJ := StrToInt64Def(parts[3], 0);
        idleJ := StrToInt64Def(parts[4], 0);
        iowaitJ := StrToInt64Def(parts[5], 0);
        irqJ := StrToInt64Def(parts[6], 0);
        softirqJ := StrToInt64Def(parts[7], 0);
        if parts.Count > 8 then
          stealJ := StrToInt64Def(parts[8], 0)
        else
          stealJ := 0;

        currentIdle := idleJ + iowaitJ;
        currentTotal := userJ + niceJ + systemJ + idleJ + iowaitJ + irqJ + softirqJ + stealJ;

        if FLastTotalJiffies = 0 then
        begin
          // First measurement: just store values, don't update history yet
          FLastTotalJiffies := currentTotal;
          FLastIdleJiffies := currentIdle;
          Result := -1;  // Signal that CPU measurement is not ready yet
          Exit;
        end
        else
        begin
          deltaTotal := currentTotal - FLastTotalJiffies;
          deltaIdle := currentIdle - FLastIdleJiffies;

          if (deltaTotal > 0) and (deltaIdle >= 0) and (deltaIdle <= deltaTotal) then
            Result := EnsureRange(Round((deltaTotal - deltaIdle) * 100 / deltaTotal), 0, 100)
          else
            Result := FCurrentCPUUsageTotal;  // Use previous value on error

          FLastTotalJiffies := currentTotal;
          FLastIdleJiffies := currentIdle;
        end;
      end;
    finally
      parts.Free;
    end;
    {$ENDIF}

  except
    on E: Exception do
    begin
      Debug(dpError, section, 'CPU calculation error: %s', [E.Message]);
      Result := 0;
    end;
  end;
end;

procedure TLoadMonitor.UpdatePerformanceLevel;
var
  currCPUTotal, currCPUWorker: Integer;
  newLevel: Integer;
  throttleTop, throttleBottom: Integer;
begin
  FLock.Enter('UpdatePerformanceLevel');
  try
    // Use current CPU values instead of average for immediate response
    currCPUTotal := FCurrentCPUUsageTotal;
    currCPUWorker := FCurrentCPUUsageWorker;

    // Get thresholds from config (with defaults)
    throttleTop := config.ReadInteger('performance_monitor', 'cpu_threshold_high', CPU_THRESHOLD_HIGH) + CPU_THRESHOLD_TOLERANCE;
    throttleBottom := config.ReadInteger('performance_monitor', 'cpu_threshold_low', CPU_THRESHOLD_LOW) - CPU_THRESHOLD_TOLERANCE;

    newLevel := FCurrentPerformanceLevel;

    // Performance level adjustment logic: immediate response to current CPU measurements
    // If CPU total OR worker is above top threshold, reduce performance level
    if ((currCPUTotal > throttleTop) or (currCPUWorker > throttleTop)) and (FCurrentPerformanceLevel > PERFLEVEL_MIN) then
    begin
      newLevel := FCurrentPerformanceLevel - 1; // Reduce performance level
      Debug(dpMessage, section, 'CPU high (total:%d%% worker:%d%% > %d%%), reducing performance level to %d',
        [currCPUTotal, currCPUWorker, throttleTop, newLevel]);
    end
    // If CPU total OR worker is below bottom threshold, increase performance level
    else if ((currCPUTotal < throttleBottom) or (currCPUWorker < throttleBottom)) and (FCurrentPerformanceLevel < PERFLEVEL_MAX) then
    begin
      newLevel := FCurrentPerformanceLevel + 1; // Increase performance level
      Debug(dpMessage, section, 'CPU low (total:%d%% worker:%d%% < %d%%), increasing performance level to %d',
        [currCPUTotal, currCPUWorker, throttleBottom, newLevel]);
    end;

    // Update and notify if changed
    if newLevel <> FCurrentPerformanceLevel then
    begin
      FCurrentPerformanceLevel := newLevel;
      NotifyListeners(newLevel);
    end;

  finally
    FLock.Leave;
  end;
end;

procedure TLoadMonitor.AddCPUHistoryValue(Value: Integer);
begin
  // Add value to circular buffer
  FCPUHistory[FHistoryPosition] := Value;
  FHistoryPosition := (FHistoryPosition + 1) mod HISTORY_SLOTS;

  if FHistoryCount < HISTORY_SLOTS then
    Inc(FHistoryCount);
end;

function TLoadMonitor.GetAverageCPUUsage(Seconds: Integer): Integer;
var
  samples: Integer;
  total: Int64;
  i, pos: Integer;
begin
  Result := 0;

  FLock.Enter('GetAverageCPUUsage');
  try
    // Calculate how many samples we need
    samples := Min(Seconds * (1000 div MONITOR_INTERVAL_MS), FHistoryCount);

    if samples <= 0 then
    begin
      // No history yet, use current value or default performance level equivalent
      if FCurrentCPUUsageTotal > 0 then
        Result := FCurrentCPUUsageTotal
      else
        Result := 50;  // Assume moderate CPU usage (50%) if no data yet
      Exit;
    end;

    total := 0;
    for i := 0 to samples - 1 do
    begin
      pos := (FHistoryPosition - 1 - i + HISTORY_SLOTS) mod HISTORY_SLOTS;
      total := total + FCPUHistory[pos];
    end;

    Result := total div samples;

  finally
    FLock.Leave;
  end;
end;

function TLoadMonitor.GetCPUHistory(Seconds: Integer): TArray<Integer>;
var
  samples: Integer;
  i, pos: Integer;
begin
  FLock.Enter('GetCPUHistory');
  try
    samples := Min(Seconds * (1000 div MONITOR_INTERVAL_MS), FHistoryCount);
    SetLength(Result, samples);

    for i := 0 to samples - 1 do
    begin
      pos := (FHistoryPosition - samples + i + HISTORY_SLOTS) mod HISTORY_SLOTS;
      Result[i] := FCPUHistory[pos];
    end;

  finally
    FLock.Leave;
  end;
end;

procedure TLoadMonitor.NotifyListeners(NewLevel: Integer);
var
  i: Integer;
  listener: IPerformanceLevelListener;
begin
  Debug(dpMessage, section, 'Performance level changed to %d', [NewLevel]);

  // Notify event handler
  if Assigned(FOnPerformanceLevelChange) then
  begin
    try
      FOnPerformanceLevelChange(Self, NewLevel);
    except
      on E: Exception do
        Debug(dpError, section, 'Event handler error: %s', [E.Message]);
    end;
  end;

  // Notify interface listeners
  for i := 0 to FListeners.Count - 1 do
  begin
    try
      listener := FListeners[i] as IPerformanceLevelListener;
      if Assigned(listener) then
        listener.OnPerformanceLevelChanged(NewLevel);
    except
      on E: Exception do
        Debug(dpError, section, 'Listener notification error: %s', [E.Message]);
    end;
  end;
end;

procedure TLoadMonitor.AddListener(const Listener: IPerformanceLevelListener);
begin
  if Assigned(Listener) then
  begin
    FLock.Enter('AddListener');
    try
      FListeners.Add(Listener);
      Debug(dpSpam, section, 'Added performance level listener');
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TLoadMonitor.RemoveListener(const Listener: IPerformanceLevelListener);
begin
  if Assigned(Listener) then
  begin
    FLock.Enter('RemoveListener');
    try
      FListeners.Remove(Listener);
      Debug(dpSpam, section, 'Removed performance level listener');
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TLoadMonitor.StartMonitoring;
var
  wasEnabled: Boolean;
begin
  FLock.Enter('StartMonitoring');
  try
    wasEnabled := FEnabled;
    if not wasEnabled then
    begin
      FEnabled := True;
      Debug(dpMessage, section, 'Starting LoadMonitor...');
    end;
  finally
    FLock.Leave;
  end;

  // Start thread if it was created suspended
  if not wasEnabled then
    Start;
end;

procedure TLoadMonitor.StopMonitoring;
var
  wasEnabled: Boolean;
  timeout: Integer;
begin
  FLock.Enter('StopMonitoring');
  try
    wasEnabled := FEnabled;
    if wasEnabled then
    begin
      Debug(dpMessage, section, 'Stopping LoadMonitor...');
      FEnabled := False;
    end;
  finally
    FLock.Leave;
  end;

  if wasEnabled then
  begin
    Terminate;

    // Wait for thread to finish with timeout (5 seconds)
    timeout := 0;
    while not Finished do
    begin
      Sleep(100);
      Inc(timeout);
      if timeout > 50 then  // 5 seconds
      begin
        Debug(dpError, section, 'LoadMonitor thread did not terminate gracefully within 5 seconds');
        Break;
      end;
    end;
  end;
end;

initialization
  // nothing to initialize explicitly

// Cleanup when unit is finalized
finalization
  if Assigned(GlLoadMonitorInstance) then
  begin
    GlLoadMonitorInstance.StopMonitoring;
    FreeAndNil(GlLoadMonitorInstance);
  end;

end.
