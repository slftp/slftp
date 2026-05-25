unit cbftpevents;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections,
  mormot.core.base, mormot.core.json, mormot.net.client, mormot.net.http,
  slcriticalsection2;

type
  TCbftpEventType = (
    cetRaceStarted,
    cetRaceProgress,
    cetRaceCompleted,
    cetRaceDone,
    cetSpeedSample,
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
  end;

  TCbftpEventCallback = procedure(const aEvent: TCbftpEvent) of object;

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
    function ParseEvent(const aJson: string): TCbftpEvent;
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

procedure CbftpEventsStart(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
procedure CbftpEventsStop;
function CbftpEventsRunning: Boolean;

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

function TCbftpEventThread.ParseEvent(const aJson: string): TCbftpEvent;
var
  doc: TDocVariantData;
  eventType: string;
  data: PDocVariantData;
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

  if eventType = 'race_started' then
  begin
    Result.EventType := cetRaceStarted;
    Result.Name := data^.U['name'];
    Result.Section := data^.U['section'];
    Result.Timestamp := data^.I['timestamp'];
  end
  else if eventType = 'race_progress' then
  begin
    Result.EventType := cetRaceProgress;
    Result.Name := data^.U['name'];
    Result.Site := data^.U['site'];
    Result.FilesTotal := data^.I['files_total'];
    Result.FilesDone := data^.I['files_done'];
    Result.BytesTotal := data^.I['bytes_total'];
    Result.BytesDone := data^.I['bytes_done'];
    Result.SpeedMbps := data^.D['speed_mbps'];
    Result.Timestamp := data^.I['timestamp'];
  end
  else if eventType = 'race_completed' then
  begin
    Result.EventType := cetRaceCompleted;
    Result.Name := data^.U['name'];
    Result.Site := data^.U['site'];
    Result.TimeSpentSeconds := data^.I['time_spent_seconds'];
    Result.Timestamp := data^.I['timestamp'];
  end
  else if eventType = 'race_done' then
  begin
    Result.EventType := cetRaceDone;
    Result.Name := data^.U['name'];
    Result.Status := data^.U['status'];
    Result.Timestamp := data^.I['timestamp'];
  end
  else if eventType = 'speed_sample' then
  begin
    Result.EventType := cetSpeedSample;
    Result.SrcSite := data^.U['src_site'];
    Result.DstSite := data^.U['dst_site'];
    Result.SpeedMbps := data^.D['speed_mbps'];
    Result.FileSize := data^.I['file_size'];
    Result.Timestamp := data^.I['timestamp'];
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
var
  url: RawUtf8;
  headers: RawUtf8;
  status: Integer;
  response: string;
  running: Boolean;
  baseUrl: RawUtf8;
begin
  baseUrl := FormatUtf8('https://%:%', [FHost, FPort]);

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
        FHttpClient := THttpClientSocket.Create(35000); // 35s timeout (slightly above 30s long-poll)
        FHttpClient.TLS.IgnoreCertificateErrors := True;
      end;

      if (not FHttpClient.SockIsDefined) or (not FHttpClient.SockConnected) then
        FHttpClient.ConnectUri(baseUrl);

      url := '/events?timeout=30';
      headers := 'Authorization: ' + GetAuthHeader + #13#10;

      Debug(dpMessage, section, 'Polling cbftp events...');
      status := FHttpClient.Request(url, 'GET', 0, headers, '', '', False);
      response := string(FHttpClient.Content);

      if status = 200 then
      begin
        FReconnectDelay := INITIAL_RECONNECT_DELAY_MS;
        if response <> '' then
        begin
          FQueue.Enqueue(ParseEvent(response));
          Synchronize(ProcessEvents);
        end;
      end
      else if status <> 0 then
      begin
        Debug(dpError, section, Format('cbftp events poll failed: %d', [status]));
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('cbftp events exception: %s', [E.Message]));
        FreeAndNil(FHttpClient);
      end;
    end;

    // Exponential backoff on error, short sleep on success
    if FReconnectDelay > INITIAL_RECONNECT_DELAY_MS then
    begin
      Sleep(FReconnectDelay);
      FReconnectDelay := Min(FReconnectDelay * 2, MAX_RECONNECT_DELAY_MS);
    end
    else
    begin
      Sleep(100); // Brief pause between polls
    end;
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

initialization
  GlCbftpEventThreadLock := TSLCriticalSection2.Create('CbftpEventThread');

finalization
  CbftpEventsStop;
  FreeAndNil(GlCbftpEventThreadLock);

end.
