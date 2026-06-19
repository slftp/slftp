unit cbftpevents;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, Math,
  mormot.core.base, mormot.core.json, mormot.core.buffers,
  slcriticalsection2, slstack,
  uLkJSON;

type
  TCbftpEventType = (
    cetRaceStarted,
    cetRaceProgress,
    cetRaceCompleted,
    cetRaceDone,
    cetSpeedSample,
    cetNfoAvailable,
    cetHeartbeat,
    cetSiteStatus
  );

  TCbftpEvent = record
    EventType: TCbftpEventType;
    Name: string;
    Section: string;
    Site: string;
    SrcSite: string;
    DstSite: string;
    FilesTotal: Integer;
    FilesDone: Integer;
    FilesUp: Integer;
    BytesTotal: Int64;
    BytesDone: Int64;
    BytesUp: Int64;
    SpeedMbps: Double;
    TimeSpentSeconds: Double;
    Status: string;
    FileSize: Int64;
    Timestamp: Int64;
    Filename: string;
    Disabled: Boolean;
    HasUpFields: Boolean;
    MaxLogins: Integer;
    CurrentLogins: Integer;
    HasSlotFields: Boolean;
  end;

  TCbftpEventCallback = procedure(const aEvent: TCbftpEvent);

  { Thread-safe event queue for cbftp events }
  TCbftpEventQueue = class
  private
    FLock: TSLCriticalSection2;
    FEvents: TList<TCbftpEvent>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const aEvent: TCbftpEvent);
    function Dequeue(out aEvent: TCbftpEvent): Boolean;
    function Count: Integer;
    procedure Clear;
  end;

  { Background thread that listens for cbftp events via UDP push }
  TCbftpUdpEventThread = class(TThread)
  private
    FBindIp: String;
    FPort: Integer;
    FQueue: TCbftpEventQueue;
    FOnEvent: TCbftpEventCallback;
    procedure ProcessEvents;
  protected
    procedure Execute; override;
  public
    constructor Create(const aBindIp: String; aPort: Integer);
    destructor Destroy; override;
    property Queue: TCbftpEventQueue read FQueue;
    property OnEvent: TCbftpEventCallback read FOnEvent write FOnEvent;
  end;

var
  GlCbftpUdpEventThread: TCbftpUdpEventThread = nil;
  GlCbftpEventThreadLock: TSLCriticalSection2;
  GlCbftpEventHandler: TCbftpEventCallback;

procedure CbftpUdpEventsStart(const aBindIp: String; aPort: Integer);
procedure CbftpUdpEventsStop;
function CbftpUdpEventsRunning: Boolean;

{ Register a global event handler. Will be applied to current and future event threads. }
procedure CbftpEventsSetHandler(const aHandler: TCbftpEventCallback);

implementation

uses
  debugunit;

const
  section = 'cbftpevents';
  MAX_RECONNECT_DELAY_MS = 60000;
  INITIAL_RECONNECT_DELAY_MS = 1000;

{ TCbftpEventQueue }

constructor TCbftpEventQueue.Create;
begin
  inherited;
  FLock := TSLCriticalSection2.Create('CbftpEventQueue');
  FEvents := TList<TCbftpEvent>.Create;
end;

destructor TCbftpEventQueue.Destroy;
begin
  FreeAndNil(FEvents);
  FreeAndNil(FLock);
  inherited;
end;

procedure TCbftpEventQueue.Enqueue(const aEvent: TCbftpEvent);
begin
  FLock.Enter('Enqueue');
  try
    FEvents.Add(aEvent);
  finally
    FLock.Leave;
  end;
end;

function TCbftpEventQueue.Dequeue(out aEvent: TCbftpEvent): Boolean;
begin
  FLock.Enter('Dequeue');
  try
    Result := FEvents.Count > 0;
    if Result then
    begin
      aEvent := FEvents[0];
      FEvents.Delete(0);
    end;
  finally
    FLock.Leave;
  end;
end;

function TCbftpEventQueue.Count: Integer;
begin
  FLock.Enter('Count');
  try
    Result := FEvents.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TCbftpEventQueue.Clear;
begin
  FLock.Enter('Clear');
  try
    FEvents.Clear;
  finally
    FLock.Leave;
  end;
end;



function CbftpParseEvent(const aJson: RawUtf8): TCbftpEvent;

  function _GetStr(const aObj: TlkJSONObject; const aKey: String): String;
  var
    f: TlkJSONbase;
  begin
    if aObj = nil then
    begin
      Result := '';
      Exit;
    end;
    f := aObj.Field[aKey];
    if (f <> nil) and (f.SelfType <> jsNull) then
      Result := f.Value
    else
      Result := '';
  end;

  function _GetInt(const aObj: TlkJSONObject; const aKey: String): Int64;
  var
    f: TlkJSONbase;
  begin
    if aObj = nil then
    begin
      Result := 0;
      Exit;
    end;
    f := aObj.Field[aKey];
    if (f <> nil) and (f.SelfType <> jsNull) then
      Result := StrToInt64Def(f.Value, 0)
    else
      Result := 0;
  end;

  function _GetDouble(const aObj: TlkJSONObject; const aKey: String): Double;
  var
    f: TlkJSONbase;
  begin
    if aObj = nil then
    begin
      Result := 0;
      Exit;
    end;
    f := aObj.Field[aKey];
    if (f <> nil) and (f.SelfType <> jsNull) then
      Result := StrToFloatDef(f.Value, 0)
    else
      Result := 0;
  end;

  function _GetBool(const aObj: TlkJSONObject; const aKey: String): Boolean;
  var
    f: TlkJSONbase;
  begin
    Result := False;
    if aObj = nil then
      Exit;
    f := aObj.Field[aKey];
    if (f <> nil) and (f.SelfType <> jsNull) then
    begin
      Result := (f.Value = True) or (f.Value = 'true') or (f.Value = '1');
    end;
  end;

var
  js: TlkJSONbase;
  obj: TlkJSONObject;
  eventType: String;
begin
  FillChar(Result, SizeOf(Result), 0);

  try
    js := TlkJSON.ParseText(AnsiString(aJson));
    if js = nil then
    begin
      Debug(dpError, section, 'Failed to parse event JSON');
      Exit;
    end;
    if not (js is TlkJSONObject) then
    begin
      js.Free;
      Debug(dpError, section, 'Event JSON is not an object');
      Exit;
    end;
    obj := TlkJSONObject(js);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('JSON parse error: %s', [E.Message]));
      Exit;
    end;
  end;

  try
    // cbftp >= e477d2c sends: {"event": "...", ...} (flat, no "data" wrapper)
    eventType := _GetStr(obj, 'event');
    if eventType = '' then
      eventType := _GetStr(obj, 'type');

    if eventType = 'race_started' then
    begin
      Result.EventType := cetRaceStarted;
      Result.Name := _GetStr(obj, 'name');
      Result.Section := _GetStr(obj, 'section');
      Result.Timestamp := _GetInt(obj, 'timestamp');
    end
    else if eventType = 'race_progress' then
    begin
      Result.EventType := cetRaceProgress;
      Result.Name := _GetStr(obj, 'name');
      Result.Site := _GetStr(obj, 'site');
      Result.FilesTotal := _GetInt(obj, 'files_total');
      Result.FilesDone := _GetInt(obj, 'files_done');
      Result.BytesTotal := _GetInt(obj, 'bytes_total');
      Result.BytesDone := _GetInt(obj, 'bytes_done');
      Result.SpeedMbps := _GetDouble(obj, 'speed_mbps');
      Result.Timestamp := _GetInt(obj, 'timestamp');
      Result.HasUpFields := (obj.Field['files_up'] <> nil);
      if Result.HasUpFields then
      begin
        Result.FilesUp := _GetInt(obj, 'files_up');
        Result.BytesUp := _GetInt(obj, 'bytes_up');
      end;
    end
    else if eventType = 'race_completed' then
    begin
      Result.EventType := cetRaceCompleted;
      Result.Name := _GetStr(obj, 'name');
      Result.Site := _GetStr(obj, 'site');
      Result.TimeSpentSeconds := _GetDouble(obj, 'time_spent_seconds');
      Result.FilesDone := _GetInt(obj, 'files_done');
      Result.BytesDone := _GetInt(obj, 'bytes_done');
      Result.Timestamp := _GetInt(obj, 'timestamp');
      Result.HasUpFields := (obj.Field['files_up'] <> nil);
      if Result.HasUpFields then
      begin
        Result.FilesUp := _GetInt(obj, 'files_up');
        Result.BytesUp := _GetInt(obj, 'bytes_up');
      end;
    end
    else if eventType = 'race_done' then
    begin
      Result.EventType := cetRaceDone;
      Result.Name := _GetStr(obj, 'name');
      Result.Status := _GetStr(obj, 'status');
      Result.Timestamp := _GetInt(obj, 'timestamp');
    end
    else if eventType = 'speed_sample' then
    begin
      Result.EventType := cetSpeedSample;
      Result.Name := _GetStr(obj, 'job_name');
      Result.SrcSite := _GetStr(obj, 'src_site');
      Result.DstSite := _GetStr(obj, 'dst_site');
      Result.Filename := _GetStr(obj, 'filename');
      Result.SpeedMbps := _GetDouble(obj, 'speed_mbps');
      Result.FileSize := _GetInt(obj, 'file_size');
      Result.Timestamp := _GetInt(obj, 'timestamp');
    end
    else if eventType = 'nfo_available' then
    begin
      Result.EventType := cetNfoAvailable;
      Result.Name := _GetStr(obj, 'release');
      Result.Site := _GetStr(obj, 'site');
      Result.Section := _GetStr(obj, 'path');
      Result.FileSize := _GetInt(obj, 'size');
      Result.Timestamp := _GetInt(obj, 'timestamp');
    end
    else if eventType = 'heartbeat' then
    begin
      Result.EventType := cetHeartbeat;
    end
    else if eventType = 'site_status' then
    begin
      Result.EventType := cetSiteStatus;
      Result.Site := _GetStr(obj, 'name');
      Result.Disabled := _GetBool(obj, 'disabled');
      Result.HasSlotFields := (obj.Field['max_logins'] <> nil);
      if Result.HasSlotFields then
      begin
        Result.MaxLogins := _GetInt(obj, 'max_logins');
        Result.CurrentLogins := _GetInt(obj, 'current_logins');
      end;
    end;
  finally
    obj.Free;
  end;
end;

{ TCbftpUdpEventThread }

constructor TCbftpUdpEventThread.Create(const aBindIp: String; aPort: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FBindIp := aBindIp;
  FPort := aPort;
  FQueue := TCbftpEventQueue.Create;
end;

destructor TCbftpUdpEventThread.Destroy;
begin
  Terminate;
  WaitFor;
  FreeAndNil(FQueue);
  inherited;
end;

procedure TCbftpUdpEventThread.ProcessEvents;
var
  event: TCbftpEvent;
begin
  while FQueue.Dequeue(event) do
  begin
    if Assigned(FOnEvent) then
    begin
      try
        FOnEvent(event);
      except
        on E: Exception do
          DebugException(dpError, section, Format('UDP event callback error (eventType=%d, name=%s)', [Ord(event.EventType), event.Name]), E);
      end;
    end;
  end;
end;

procedure TCbftpUdpEventThread.Execute;
var
  slSock: TslSocket;
  error: String;
  buf: array[0..8191] of AnsiChar;
  recvLen: Integer;
  jsonStr: RawUtf8;
  event: TCbftpEvent;
begin
  if not slGetSocket(slSock, True, error) then
  begin
    Debug(dpError, section, Format('UDP socket create failed: %s', [error]));
    Exit;
  end;
  try
    if not slBind(slSock, FBindIp, FPort, error) then
    begin
      Debug(dpError, section, Format('UDP bind failed on port %d: %s', [FPort, error]));
      Exit;
    end;

    Debug(dpMessage, section, Format('UDP event server listening on port %d', [FPort]));

    while not Terminated do
    begin
      if not slSelect(slSock, 500, True, False, error) then
        Continue;

      recvLen := slRecv(slSock, buf, SizeOf(buf) - 1, error);
      if recvLen <= 0 then
        Continue;

      buf[recvLen] := #0;
      jsonStr := RawUtf8(PAnsiChar(@buf));

      event := CbftpParseEvent(jsonStr);
      if event.EventType <> cetHeartbeat then
      begin
        FQueue.Enqueue(event);
        Synchronize(ProcessEvents);
      end;
    end;
  finally
    slClose(slSock);
  end;
end;

{ Global helpers }



procedure CbftpUdpEventsStart(const aBindIp: String; aPort: Integer);
begin
  GlCbftpEventThreadLock.Enter('CbftpUdpEventsStart');
  try
    if GlCbftpUdpEventThread <> nil then
      Exit;
    GlCbftpUdpEventThread := TCbftpUdpEventThread.Create(aBindIp, aPort);
    if Assigned(GlCbftpEventHandler) then
      GlCbftpUdpEventThread.OnEvent := GlCbftpEventHandler;
    Debug(dpMessage, section, Format('cbftp UDP event thread started on %s:%d', [aBindIp, aPort]));
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

procedure CbftpUdpEventsStop;
begin
  GlCbftpEventThreadLock.Enter('CbftpUdpEventsStop');
  try
    if GlCbftpUdpEventThread = nil then
      Exit;
    GlCbftpUdpEventThread.Terminate;
    GlCbftpUdpEventThread.WaitFor;
    FreeAndNil(GlCbftpUdpEventThread);
    Debug(dpMessage, section, 'cbftp UDP event thread stopped');
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

function CbftpUdpEventsRunning: Boolean;
begin
  GlCbftpEventThreadLock.Enter('CbftpUdpEventsRunning');
  try
    Result := (GlCbftpUdpEventThread <> nil) and not GlCbftpUdpEventThread.Terminated;
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

procedure CbftpEventsSetHandler(const aHandler: TCbftpEventCallback);
begin
  GlCbftpEventThreadLock.Enter('CbftpEventsSetHandler');
  try
    GlCbftpEventHandler := aHandler;
    if Assigned(GlCbftpUdpEventThread) then
      GlCbftpUdpEventThread.OnEvent := aHandler;
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

initialization
  GlCbftpEventThreadLock := TSLCriticalSection2.Create('CbftpEventThread');

finalization
  CbftpUdpEventsStop;
  FreeAndNil(GlCbftpEventThreadLock);

end.
