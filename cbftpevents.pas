unit cbftpevents;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, Math,
  mormot.core.base, mormot.core.json, mormot.core.buffers,
  mormot.net.client, mormot.net.http,
  slcriticalsection2,
  uLkJSON;

type
  TCbftpEventType = (
    cetRaceStarted,
    cetRaceProgress,
    cetRaceCompleted,
    cetRaceDone,
    cetSpeedSample,
    cetNfoAvailable,
    cetHeartbeat
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
    BytesTotal: Int64;
    BytesDone: Int64;
    SpeedMbps: Double;
    TimeSpentSeconds: Integer;
    Status: string;
    FileSize: Int64;
    Timestamp: Int64;
    Filename: string;
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

  { Background thread that maintains persistent connection to cbftp /events endpoint }
  TCbftpEventThread = class(TThread)
  private
    FHost: RawUtf8;
    FPort: Integer;
    FPassword: RawUtf8;
    FHttpClient: THttpClientSocket;
    FLock: TCriticalSection;
    FRunning: Boolean;
    FReconnectDelay: Integer;
    FQueue: TCbftpEventQueue;
    FOnEvent: TCbftpEventCallback;

    function GetAuthHeader: RawUtf8;
    function ParseEvent(const aJson: RawUtf8): TCbftpEvent;
    procedure ProcessEvents;
  protected
    procedure Execute; override;
  public
    constructor Create(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
    destructor Destroy; override;
    procedure Stop;
    property Queue: TCbftpEventQueue read FQueue;
    property OnEvent: TCbftpEventCallback read FOnEvent write FOnEvent;
  end;

var
  GlCbftpEventThread: TCbftpEventThread = nil;
  GlCbftpEventThreadLock: TSLCriticalSection2;
  GlCbftpEventHandler: TCbftpEventCallback;

procedure CbftpEventsStart(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
procedure CbftpEventsStop;
function CbftpEventsRunning: Boolean;

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

{ TCbftpEventThread }

constructor TCbftpEventThread.Create(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FHost := aHost;
  FPort := aPort;
  FPassword := aPassword;
  FRunning := True;
  FReconnectDelay := INITIAL_RECONNECT_DELAY_MS;
  FQueue := TCbftpEventQueue.Create;
  FLock := TCriticalSection.Create;
  FHttpClient := nil;
end;

destructor TCbftpEventThread.Destroy;
begin
  Stop;
  WaitFor;
  FreeAndNil(FQueue);
  FreeAndNil(FLock);
  if FHttpClient <> nil then
    FreeAndNil(FHttpClient);
  inherited;
end;

procedure TCbftpEventThread.Stop;
begin
  FLock.Acquire;
  try
    FRunning := False;
  finally
    FLock.Release;
  end;
end;

function TCbftpEventThread.GetAuthHeader: RawUtf8;
var
  credentials: RawUtf8;
begin
  credentials := BinToBase64(':' + FPassword);
  Result := 'Basic ' + credentials;
end;

function TCbftpEventThread.ParseEvent(const aJson: RawUtf8): TCbftpEvent;

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
    end
    else if eventType = 'race_completed' then
    begin
      Result.EventType := cetRaceCompleted;
      Result.Name := _GetStr(obj, 'name');
      Result.Site := _GetStr(obj, 'site');
      Result.TimeSpentSeconds := _GetInt(obj, 'time_spent_seconds');
      Result.Timestamp := _GetInt(obj, 'timestamp');
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
    end;
  finally
    obj.Free;
  end;
end;

procedure TCbftpEventThread.ProcessEvents;
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
          Debug(dpError, section, Format('Event callback error: %s', [E.Message]));
      end;
    end;
  end;
end;

procedure TCbftpEventThread.Execute;

  function _ReadLine(out aLine: RawUtf8): Boolean;
  var
    buf: array[0..4095] of AnsiChar;
    len: PtrInt;
  begin
    Result := False;
    if FHttpClient = nil then
      Exit;
    len := FHttpClient.SockInReadLn(@buf, SizeOf(buf));
    if len > 0 then
    begin
      SetString(aLine, PAnsiChar(@buf), len);
      Result := True;
    end;
  end;

  function _ReadLongPollResponse(const aContentLen: Integer; out aJson: RawUtf8): Boolean;
  var
    body: RawUtf8;
    buf: array[0..8191] of AnsiChar;
    len: PtrInt;
    totalRead: Integer;
  begin
    Result := False;
    aJson := '';

    if aContentLen <= 0 then
      Exit;

    // read body directly — _ReadHttpHeaders already consumed all headers
    SetLength(body, aContentLen);
    totalRead := 0;
    while totalRead < aContentLen do
    begin
      len := FHttpClient.SockInRead(@buf[0], Min(SizeOf(buf), aContentLen - totalRead));
      if len <= 0 then
        Exit;
      Move(buf[0], body[totalRead + 1], len);
      Inc(totalRead, len);
    end;

    aJson := body;
    Result := True;
  end;

  procedure _SendLongPollRequest(const aBaseUrl: RawUtf8);
  var
    host: RawUtf8;
  begin
    host := FHost;
    if FPort <> 443 then
      host := FormatUtf8('%:%', [FHost, FPort], []);
    FHttpClient.SockSendLine(['GET /events HTTP/1.1']);
    FHttpClient.SockSendLine(['Host: ' + host]);
    FHttpClient.SockSendLine(['Authorization: ' + GetAuthHeader]);
    FHttpClient.SockSendLine(['Accept: application/json']);
    FHttpClient.SockSendLine(['Cache-Control: no-cache']);
    FHttpClient.SockSendCRLF;
    FHttpClient.SockSendFlush;
  end;

  function _ReadHttpHeaders(out aContentLen: Integer): Integer;
  var
    line: RawUtf8;
    codeStr: RawUtf8;
    p: Integer;
  begin
    Result := 0;
    aContentLen := 0;
    // first line: HTTP/1.1 200 OK
    if not _ReadLine(line) then
      Exit;
    p := Pos(' ', line);
    if p > 0 then
    begin
      codeStr := Copy(line, p + 1, 3);
      Result := StrToIntDef(string(codeStr), 0);
    end;
    // read remaining headers until empty line
    repeat
      if not _ReadLine(line) then
        Break;
      if Pos('CONTENT-LENGTH:', UpperCase(string(line))) = 1 then
        aContentLen := StrToIntDef(Trim(Copy(string(line), 16, MaxInt)), 0);
    until line = '';
  end;

var
  running: Boolean;
  baseUrl: RawUtf8;
  eventJson: RawUtf8;
  httpStatus: Integer;
  contentLen: Integer;
begin
  baseUrl := FormatUtf8('https://%:%', [FHost, FPort], []);

  while not Terminated do
  begin
    FLock.Acquire;
    try
      running := FRunning;
    finally
      FLock.Release;
    end;

    if not running then
      Break;

    try
      if FHttpClient = nil then
      begin
        FHttpClient := THttpClientSocket.Create;
        FHttpClient.TLS.IgnoreCertificateErrors := True;
      end;

      if (not FHttpClient.SockIsDefined) or (not FHttpClient.SockConnected) then
        FHttpClient.ConnectUri(baseUrl);

      _SendLongPollRequest(baseUrl);
      httpStatus := _ReadHttpHeaders(contentLen);

      if httpStatus = 200 then
      begin
        FReconnectDelay := INITIAL_RECONNECT_DELAY_MS;
        if _ReadLongPollResponse(contentLen, eventJson) then
        begin
          // cbftp returns heartbeat on timeout — wait briefly to avoid log spam
          if (contentLen <= 2) or (Pos('"heartbeat"', string(eventJson)) > 0) then
          begin
            Sleep(1000);
            Continue;
          end;
          FQueue.Enqueue(ParseEvent(eventJson));
          Synchronize(ProcessEvents);
          // Long-polling: immediately make next request on success
          Continue;
        end
        else
        begin
          Debug(dpError, section, 'Failed to read cbftp event response body');
          FreeAndNil(FHttpClient);
        end;
      end
      else
      begin
        Debug(dpError, section, Format('cbftp events poll failed: %d', [httpStatus]));
        FreeAndNil(FHttpClient);
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('cbftp events exception: %s | JSON=%s', [E.Message, string(eventJson)]));
        FreeAndNil(FHttpClient);
      end;
    end;

    if Terminated then
      Break;

    // Exponential backoff before reconnect on error
    Debug(dpMessage, section, Format('cbftp events reconnect in %dms', [FReconnectDelay]));
    Sleep(FReconnectDelay);
    FReconnectDelay := Min(FReconnectDelay * 2, MAX_RECONNECT_DELAY_MS);
  end;
end;

{ Global helpers }

procedure CbftpEventsStart(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
begin
  GlCbftpEventThreadLock.Enter('CbftpEventsStart');
  try
    if GlCbftpEventThread <> nil then
      Exit;
    GlCbftpEventThread := TCbftpEventThread.Create(aHost, aPort, aPassword);
    if Assigned(GlCbftpEventHandler) then
      GlCbftpEventThread.OnEvent := GlCbftpEventHandler;
    Debug(dpMessage, section, Format('cbftp event thread started: %s:%d', [aHost, aPort]));
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

procedure CbftpEventsStop;
begin
  GlCbftpEventThreadLock.Enter('CbftpEventsStop');
  try
    if GlCbftpEventThread = nil then
      Exit;
    GlCbftpEventThread.Stop;
    GlCbftpEventThread.WaitFor;
    FreeAndNil(GlCbftpEventThread);
    Debug(dpMessage, section, 'cbftp event thread stopped');
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

function CbftpEventsRunning: Boolean;
begin
  GlCbftpEventThreadLock.Enter('CbftpEventsRunning');
  try
    Result := (GlCbftpEventThread <> nil) and not GlCbftpEventThread.Terminated;
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

procedure CbftpEventsSetHandler(const aHandler: TCbftpEventCallback);
begin
  GlCbftpEventThreadLock.Enter('CbftpEventsSetHandler');
  try
    GlCbftpEventHandler := aHandler;
    if Assigned(GlCbftpEventThread) then
      GlCbftpEventThread.OnEvent := aHandler;
  finally
    GlCbftpEventThreadLock.Leave;
  end;
end;

initialization
  GlCbftpEventThreadLock := TSLCriticalSection2.Create('CbftpEventThread');

finalization
  CbftpEventsStop;
  FreeAndNil(GlCbftpEventThreadLock);

end.
