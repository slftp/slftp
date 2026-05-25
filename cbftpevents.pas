unit cbftpevents;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, Math,
  mormot.core.base, mormot.core.variants, mormot.core.json, mormot.core.buffers,
  mormot.net.client, mormot.net.http,
  slcriticalsection2;

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
var
  doc: TDocVariantData;
  eventType: RawUtf8;
  data: PDocVariantData;
  dv: TDocVariantData;
begin
  FillChar(Result, SizeOf(Result), 0);

  if not doc.InitJson(aJson) then
  begin
    Debug(dpError, section, 'Failed to parse event JSON');
    Exit;
  end;

  eventType := doc.U['type'];
  data := doc.A_['data'];
  if data = nil then
    Exit;
  dv := data^;

  if eventType = 'race_started' then
  begin
    Result.EventType := cetRaceStarted;
    Result.Name := dv.U['name'];
    Result.Section := dv.U['section'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'race_progress' then
  begin
    Result.EventType := cetRaceProgress;
    Result.Name := dv.U['name'];
    Result.Site := dv.U['site'];
    Result.FilesTotal := dv.I['files_total'];
    Result.FilesDone := dv.I['files_done'];
    Result.BytesTotal := dv.I['bytes_total'];
    Result.BytesDone := dv.I['bytes_done'];
    Result.SpeedMbps := dv.D['speed_mbps'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'race_completed' then
  begin
    Result.EventType := cetRaceCompleted;
    Result.Name := dv.U['name'];
    Result.Site := dv.U['site'];
    Result.TimeSpentSeconds := dv.I['time_spent_seconds'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'race_done' then
  begin
    Result.EventType := cetRaceDone;
    Result.Name := dv.U['name'];
    Result.Status := dv.U['status'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'speed_sample' then
  begin
    Result.EventType := cetSpeedSample;
    Result.Name := dv.U['job_name'];
    Result.SrcSite := dv.U['src_site'];
    Result.DstSite := dv.U['dst_site'];
    Result.Filename := dv.U['filename'];
    Result.SpeedMbps := dv.D['speed_mbps'];
    Result.FileSize := dv.I['file_size'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'nfo_available' then
  begin
    Result.EventType := cetNfoAvailable;
    Result.Name := dv.U['release'];
    Result.Site := dv.U['site'];
    Result.Section := dv.U['path'];
    Result.FileSize := dv.I['size'];
    Result.Timestamp := dv.I['timestamp'];
  end
  else if eventType = 'heartbeat' then
  begin
    Result.EventType := cetHeartbeat;
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

  function _ReadSseEvent(out aJson: RawUtf8): Boolean;
  var
    line: RawUtf8;
    data: RawUtf8;
  begin
    Result := False;
    data := '';
    while True do
    begin
      if not _ReadLine(line) then
        Exit;
      if line = '' then
      begin
        // empty line marks end of event
        if data <> '' then
        begin
          aJson := data;
          Result := True;
        end;
        Exit;
      end;
      if Pos('data: ', line) = 1 then
      begin
        if data <> '' then
          data := data + #10;
        data := Copy(line, 7, MaxInt);
      end;
    end;
  end;

  procedure _SendSseRequest(const aBaseUrl: RawUtf8);
  var
    host: RawUtf8;
  begin
    host := FHost;
    if FPort <> 443 then
      host := FormatUtf8('%:%', [FHost, FPort], []);
    FHttpClient.SockSendLine(['GET /events HTTP/1.1']);
    FHttpClient.SockSendLine(['Host: ' + host]);
    FHttpClient.SockSendLine(['Authorization: ' + GetAuthHeader]);
    FHttpClient.SockSendLine(['Accept: text/event-stream']);
    FHttpClient.SockSendLine(['Cache-Control: no-cache']);
    FHttpClient.SockSendCRLF;
    FHttpClient.SockSendFlush;
  end;

  function _ReadHttpHeaders: Integer;
  var
    line: RawUtf8;
    codeStr: RawUtf8;
    p: Integer;
  begin
    Result := 0;
    // first line: HTTP/1.1 200 OK
    if not _ReadLine(line) then
      Exit;
    p := Pos(' ', line);
    if p > 0 then
    begin
      codeStr := Copy(line, p + 1, 3);
      Result := StrToIntDef(string(codeStr), 0);
    end;
    // skip remaining headers
    repeat
      if not _ReadLine(line) then
        Break;
    until line = '';
  end;

var
  running: Boolean;
  baseUrl: RawUtf8;
  eventJson: RawUtf8;
  httpStatus: Integer;
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

      Debug(dpMessage, section, 'Connecting to cbftp SSE stream...');
      _SendSseRequest(baseUrl);
      httpStatus := _ReadHttpHeaders;

      if httpStatus = 200 then
      begin
        FReconnectDelay := INITIAL_RECONNECT_DELAY_MS;
        Debug(dpMessage, section, 'cbftp SSE stream connected');
        // read events until connection drops or stopped
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

          if _ReadSseEvent(eventJson) then
          begin
            FQueue.Enqueue(ParseEvent(eventJson));
            Synchronize(ProcessEvents);
          end
          else
          begin
            // connection dropped or read error
            Break;
          end;
        end;
      end
      else
      begin
        Debug(dpError, section, Format('cbftp SSE connect failed: %d', [httpStatus]));
        FreeAndNil(FHttpClient);
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('cbftp SSE exception: %s', [E.Message]));
        FreeAndNil(FHttpClient);
      end;
    end;

    if Terminated then
      Break;

    // Exponential backoff before reconnect
    Debug(dpMessage, section, Format('cbftp SSE reconnect in %dms', [FReconnectDelay]));
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
