unit slapi;

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.log,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.net.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.net.http,
  slapi.types,
  slapi.services,
  slapi.services.impl,
  slapi.issues,
  slapi.issueshook,
  slapi.cbftp,
  configunit,
  debugunit,
  globals,
  StrUtils;

type
  { REST API Server for slftp }
  TSlftpApiServer = class
  private
  FHttpServer: TRestHttpServer;
  FRestServer: TRestServerFullMemory;
  FEnabled: boolean;
  FPort: integer;
  FHost: string;
  FApiKey: string;
  procedure RegisterServices;
  function CheckApiKey(Ctxt: TRestServerUriContext): integer;
  function OnApiError(Ctxt: TRestServerUriContext; E: Exception): boolean;
  procedure OnApiAfter(Ctxt: TRestServerUriContext);
  function OnBeforeUri(Ctxt: TRestServerUriContext): boolean;
  function DoOnCustomRequest(var Call: TRestUriParams): boolean;
  function ServeIndexHtml(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    { Initializes and starts the API server
      @returns(True if server started successfully) }
    function Start: boolean;

    { Stops the API server }
    procedure Stop;

    { Returns whether server is running }
    function IsRunning: boolean;

    property Enabled: boolean read FEnabled write FEnabled;
    property Port: integer read FPort write FPort;
    property Host: string read FHost write FHost;
    property ApiKey: string read FApiKey write FApiKey;
  end;

{ Global API server instance }
function GetApiServer: TSlftpApiServer;

{ Initialize API server from config }
procedure ApiInit;

{ Start API server listening }
procedure ApiStart;

{ Shutdown API server }
procedure ApiUninit;

implementation

{$I ../slftp.inc}

const
  rsection = 'slapi';

var
  ApiServer: TSlftpApiServer = nil;

threadvar
  ApiExceptionLogged: boolean;

function GetApiServer: TSlftpApiServer;
begin
  Result := ApiServer;
end;

{ TSlftpApiServer }

constructor TSlftpApiServer.Create;
begin
  inherited Create;
  FHttpServer := nil;
  FRestServer := nil;
  FEnabled := False;
  FPort := 8089;
  FHost := '127.0.0.1';
  FApiKey := '';

  Debug(dpMessage, rsection, 'API Server created');
end;

destructor TSlftpApiServer.Destroy;
begin
  Stop;
  Debug(dpMessage, rsection, 'API Server destroyed');
  inherited Destroy;
end;

function TSlftpApiServer.CheckApiKey(Ctxt: TRestServerUriContext): integer;
var
  authHeader: RawUTF8;
  providedKey: RawUTF8;
  targetRoot: RawUTF8;
begin
  Result := 0; // 0 = continue processing

  // Skip auth check if no API key is configured
  if FApiKey = '' then
    Exit;

  // Protect only the API root
  targetRoot := Ctxt.Call^.Url;
  if (Length(targetRoot) < 4) or (Copy(UpperCase(targetRoot), 1, 4) <> '/API') then
    Exit;

  // Check Authorization header
  authHeader := Ctxt.InHeader['Authorization'];
  if authHeader = '' then
  begin
    Ctxt.Error('Missing Authorization header', HTTP_FORBIDDEN);
    Result := HTTP_FORBIDDEN;
    Exit;
  end;

  // Expected format: "Bearer YOUR_API_KEY" or just "YOUR_API_KEY"
  if (Length(authHeader) >= 7) and (UpperCase(Copy(authHeader, 1, 7)) = 'BEARER ') then
    providedKey := Copy(authHeader, 8, Length(authHeader))
  else
    providedKey := authHeader;

  if providedKey <> UTF8Encode(FApiKey) then
  begin
    Ctxt.Error('Invalid API key', HTTP_FORBIDDEN);
    Result := HTTP_FORBIDDEN;
    Debug(dpError, rsection, 'API authentication failed');
  end;
end;

function TSlftpApiServer.OnApiError(Ctxt: TRestServerUriContext; E: Exception): boolean;
var
  url: string;
begin
  ApiExceptionLogged := True;
  url := '';
  if (Ctxt <> nil) and (Ctxt.Call <> nil) then
    url := UTF8ToString(Ctxt.Call^.Url);

  if url <> '' then
    Debug(dpError, rsection, Format('[API EXCEPTION] %s: %s', [url, E.Message]))
  else
    Debug(dpError, rsection, Format('[API EXCEPTION] %s', [E.Message]));

  Result := True;
end;

procedure TSlftpApiServer.OnApiAfter(Ctxt: TRestServerUriContext);
var
  url: string;
  status: integer;
begin
  try
    if (Ctxt = nil) or (Ctxt.Call = nil) then
      Exit;

    url := UTF8ToString(Ctxt.Call^.Url);
    if (Length(url) < 4) or (UpperCase(Copy(url, 1, 4)) <> '/API') then
      Exit;

    status := Ctxt.Call^.OutStatus;
    if not StatusCodeIsSuccess(status) then
    begin
      if not ApiExceptionLogged then
        Debug(dpError, rsection, Format('[API ERROR] %s status=%d', [url, status]));
    end;
  finally
    ApiExceptionLogged := False;
  end;
end;

function TSlftpApiServer.OnBeforeUri(Ctxt: TRestServerUriContext): boolean;
var
  sUrl: string;
  sUrlLower: string;
begin
  Result := True; // By default, let mORMot continue processing

  if (Ctxt = nil) or (Ctxt.Call = nil) then
    Exit;

  sUrl := UTF8ToString(Ctxt.Call^.Url);

  // Normalize leading slash
  if (sUrl <> '') and (sUrl[1] <> '/') then
    sUrl := '/' + sUrl;

  sUrlLower := LowerCase(sUrl);

  // Check for other interceptions if needed
  Result := True; // By default, let mORMot continue processing
end;

procedure TSlftpApiServer.RegisterServices;
begin
  // Register all service interfaces with mORMot2
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IApiSystemService),
    TypeInfo(IApiSitesService),
    TypeInfo(IApiQueueService),
    TypeInfo(IApiStatsService),
    TypeInfo(IApiRacesService),
    TypeInfo(IApiIrcService),
    TypeInfo(IApiRulesService),
    TypeInfo(IApiSpeedService),
    TypeInfo(IApiKnowledgeBaseService),
    TypeInfo(IApiPrecatcherService),
    TypeInfo(IApiSimulatorService),
    TypeInfo(IApiIssuesService),
    TypeInfo(IApiLogService),
    TypeInfo(IApiBrowserService),
    TypeInfo(IApiImdbService),
    TypeInfo(IApiTVService),
    TypeInfo(IApiConfigService),
    TypeInfo(IApiHelpService),
    TypeInfo(IApiNewsService)
  ]);

  // Register all service interfaces with their implementations
  FRestServer.ServiceDefine(TApiSystemServiceImpl, [IApiSystemService], sicShared);
  FRestServer.ServiceDefine(TApiSitesServiceImpl, [IApiSitesService], sicShared);
  FRestServer.ServiceDefine(TApiQueueServiceImpl, [IApiQueueService], sicShared);
  FRestServer.ServiceDefine(TApiStatsServiceImpl, [IApiStatsService], sicShared);
  FRestServer.ServiceDefine(TApiRacesServiceImpl, [IApiRacesService], sicShared);
  FRestServer.ServiceDefine(TApiIrcServiceImpl, [IApiIrcService], sicShared);
  FRestServer.ServiceDefine(TApiRulesServiceImpl, [IApiRulesService], sicShared);
  FRestServer.ServiceDefine(TApiSpeedServiceImpl, [IApiSpeedService], sicShared);
  FRestServer.ServiceDefine(TApiKnowledgeBaseServiceImpl, [IApiKnowledgeBaseService], sicShared);
  FRestServer.ServiceDefine(TApiPrecatcherServiceImpl, [IApiPrecatcherService], sicShared);
  FRestServer.ServiceDefine(TApiSimulatorServiceImpl, [IApiSimulatorService], sicShared);
  FRestServer.ServiceDefine(TApiIssuesServiceImpl, [IApiIssuesService], sicShared);
  FRestServer.ServiceDefine(TApiLogServiceImpl, [IApiLogService], sicShared);
  FRestServer.ServiceDefine(TApiBrowserServiceImpl, [IApiBrowserService], sicShared);
  FRestServer.ServiceDefine(TApiImdbServiceImpl, [IApiImdbService], sicShared);
  FRestServer.ServiceDefine(TApiTVServiceImpl, [IApiTVService], sicShared);
  FRestServer.ServiceDefine(TApiConfigServiceImpl, [IApiConfigService], sicShared);
  FRestServer.ServiceDefine(TApiHelpServiceImpl, [IApiHelpService], sicShared);
  FRestServer.ServiceDefine(TApiNewsServiceImpl, [IApiNewsService], sicShared);

  Debug(dpMessage, rsection, 'API Services registered');
end;

function TSlftpApiServer.ServeIndexHtml(Ctxt: THttpServerRequestAbstract): cardinal;
var
  FileName: TFileName;
begin
  FileName := ExtractFilePath(ParamStr(0)) + 'web' + PathDelim + 'index.html';
  if FileExists(FileName) then
  begin
    Ctxt.OutContent := StringFromFile(FileName);
    Ctxt.OutContentType := 'text/html';
    Result := HTTP_SUCCESS;
  end
  else
  begin
    Ctxt.OutContent := 'index.html not found';
    Ctxt.OutContentType := 'text/plain';
    Result := HTTP_NOTFOUND;
  end;
end;

function TSlftpApiServer.DoOnCustomRequest(var Call: TRestUriParams): boolean;
var
  FileName: TFileName;
  ContentType: RawUTF8;
  sUrl: string;
  sUrlLower: string;
  basePath: string;
  normalizedPath: string;
  isApiCall: Boolean;
  isCbftpCall: Boolean;
  isCbftpEnabledEndpoint: Boolean;
  isSlotsStreamCall: Boolean;
  isSlotsHistoryCall: Boolean;
  
  function GetMimeType(const FN: TFileName): RawUTF8;
  var
    Ext: string;
  begin
    Ext := LowerCase(ExtractFileExt(FN));
    if Ext = '.html' then Result := 'text/html'
    else if Ext = '.js' then Result := 'application/javascript'
    else if Ext = '.css' then Result := 'text/css'
    else if Ext = '.json' then Result := 'application/json'
    else if Ext = '.png' then Result := 'image/png'
    else if Ext = '.jpg' then Result := 'image/jpeg'
    else if Ext = '.gif' then Result := 'image/gif'
    else if Ext = '.svg' then Result := 'image/svg+xml'
    else if Ext = '.ico' then Result := 'image/x-icon'
    else Result := 'application/octet-stream';
  end;

  function RequireApiAuth: Boolean;
  var
    authHeader: RawUTF8;
    providedKey: RawUTF8;
    idx, crlfPos, colonPos: Integer;
    headerLine: string;
  begin
    Result := True;

    if FApiKey = '' then
      Exit(True);

    authHeader := '';
    if Pos('AUTHORIZATION:', UpperCase(Call.InHead)) > 0 then
    begin
      // Extract value after "Authorization: "
      idx := Pos('AUTHORIZATION:', UpperCase(Call.InHead));
      headerLine := Copy(Call.InHead, idx, MaxInt);
      crlfPos := Pos(#13, headerLine);
      if crlfPos > 0 then
        headerLine := Copy(headerLine, 1, crlfPos - 1);
      colonPos := Pos(':', headerLine);
      if colonPos > 0 then
        authHeader := Trim(Copy(headerLine, colonPos + 1, MaxInt));
    end;

    if authHeader = '' then
    begin
      Call.OutStatus := HTTP_FORBIDDEN;
      Call.OutBody := 'Missing Authorization header';
      Exit(False);
    end;

    // Expected format: "Bearer YOUR_API_KEY" or just "YOUR_API_KEY"
    if (Length(authHeader) >= 7) and (UpperCase(Copy(authHeader, 1, 7)) = 'BEARER ') then
      providedKey := Copy(authHeader, 8, Length(authHeader))
    else
      providedKey := authHeader;

    if providedKey <> UTF8Encode(FApiKey) then
    begin
      Call.OutStatus := HTTP_FORBIDDEN;
      Call.OutBody := 'Invalid API key';
      Debug(dpError, rsection, 'API authentication failed');
      Exit(False);
    end;
  end;

  function QueryParam(const aUrl, aName: string): string;
  var
    query: string;
    pairs: TStringList;
    i: integer;
    eqPos: integer;
    key: string;
  begin
    Result := '';
    if aName = '' then
      Exit;

    eqPos := Pos('?', aUrl);
    if eqPos <= 0 then
      Exit;

    query := Copy(aUrl, eqPos + 1, MaxInt);
    if query = '' then
      Exit;

    pairs := TStringList.Create;
    try
      pairs.StrictDelimiter := True;
      pairs.Delimiter := '&';
      pairs.DelimitedText := query;
      for i := 0 to pairs.Count - 1 do
      begin
        eqPos := Pos('=', pairs[i]);
        if eqPos <= 0 then
          Continue;
        key := LowerCase(Copy(pairs[i], 1, eqPos - 1));
        if key = LowerCase(aName) then
        begin
          Result := Copy(pairs[i], eqPos + 1, MaxInt);
          Exit;
        end;
      end;
    finally
      pairs.Free;
    end;
  end;

  function CalcFnv1a32Hex(const aData: RawUTF8): string;
  var
    i: integer;
    h: cardinal;
  begin
    h := 2166136261;
    for i := 1 to Length(aData) do
    begin
      h := h xor Ord(aData[i]);
      h := h * 16777619;
    end;
    Result := LowerCase(IntToHex(h, 8));
  end;

begin
  Result := False; // By default, let mORMot handle

  // Convert URL
  sUrl := UTF8ToString(Call.Url);

  // Normalize leading slash so comparisons work for both "/api/..." and "api/..."
  if sUrl = '' then
    sUrl := '/'
  else if sUrl[1] <> '/' then
    sUrl := '/' + sUrl;

  sUrlLower := LowerCase(sUrl);
  isCbftpCall :=
    ((Length(sUrlLower) >= 7) and (Copy(sUrlLower, 1, 7) = '/cbftp/')) or
    (sUrlLower = '/cbftp') or
    ((Length(sUrlLower) >= 11) and (Copy(sUrlLower, 1, 11) = '/api/cbftp/')) or
    (sUrlLower = '/api/cbftp');
  isCbftpEnabledEndpoint :=
    (sUrlLower = '/cbftp/enabled') or
    (Copy(sUrlLower, 1, 15) = '/cbftp/enabled?') or
    (sUrlLower = '/api/cbftp/enabled') or
    (Copy(sUrlLower, 1, 19) = '/api/cbftp/enabled?');
  isSlotsStreamCall :=
    (sUrlLower = '/api/sites/slots/stream') or
    (Copy(sUrlLower, 1, 24) = '/api/sites/slots/stream?');
  isSlotsHistoryCall :=
    (sUrlLower = '/api/sites/slots/history') or
    (Copy(sUrlLower, 1, 25) = '/api/sites/slots/history?');
  isApiCall := (Length(sUrl) >= 4) and (Copy(UpperCase(sUrl), 1, 4) = '/API');

  // Check for cbftp proxy requests first (allow /cbftp/ and /api/cbftp/)
  if isCbftpCall then
  begin
    // Keep /cbftp/enabled public for frontend capability checks.
    // All other cbftp proxy endpoints require the slftp API key.
    if (not isCbftpEnabledEndpoint) and (not RequireApiAuth) then
      Exit(True); // Rejected by auth

    Result := HandleCbftpRequest(Call);
    Exit(Result);
  end;

  // MONITORING ENDPOINTS DISABLED - Feature removed due to stability issues
  // The slot streaming and history endpoints caused memory leaks and race conditions
  // and the CurrentAction tracking was never properly implemented.
  if isSlotsStreamCall or isSlotsHistoryCall or
     (Pos('/api/monitoring/', sUrlLower) = 1) then
  begin
    if not RequireApiAuth then
      Exit(True);
    Call.OutStatus := HTTP_NOTFOUND;
    Call.OutBody := '{"error":"Monitoring feature disabled"}';
    Exit(True);
  end;

  // If it's an API call (/api/...), check authentication first
  if isApiCall then
  begin
    if not RequireApiAuth then
      Exit(True); // Rejected by auth

    // Auth passed (or not required), let mORMot handle the API call
    Exit(False); // mORMot handles it
  end;

  // Not an API call, try to serve static file
  Result := True; // We will handle it (serve file or 404)

  // Handle root path
  if (sUrl = '') or (sUrl = '/') then
    FileName := 'index.html'
  else
    FileName := Copy(sUrl, 2, MaxInt); // strip leading slash for path building

  // Remove query string if any
  if Pos('?', FileName) > 0 then
    FileName := Copy(FileName, 1, Pos('?', FileName) - 1);

  basePath := IncludeTrailingPathDelimiter(ExpandFileName(
    ExtractFilePath(ParamStr(0)) + 'web'));
  normalizedPath := ExpandFileName(basePath + FileName);

  // Prevent path traversal outside web root
  if Copy(normalizedPath, 1, Length(basePath)) <> basePath then
  begin
    Call.OutStatus := 404;
    Call.OutBody := 'File not found';
    Exit(True);
  end;

  FileName := normalizedPath;
  
  if DirectoryExists(FileName) then
    FileName := FileName + PathDelim + 'index.html';

  if FileExists(FileName) then
  begin
    Call.OutBody := StringFromFile(FileName);
    ContentType := GetMimeType(FileName);
    Call.OutHead := 'Content-Type: ' + ContentType;
    Call.OutStatus := 200;
    // Result already = True
  end
  else
  begin
      // Fallback for SPA (Single Page Application) routing:
      // If file not found and it's not an API call, serve index.html
      FileName := ExtractFilePath(ParamStr(0)) + 'web' + PathDelim + 'index.html';
      if FileExists(FileName) then
      begin
        Call.OutBody := StringFromFile(FileName);
        Call.OutHead := 'Content-Type: text/html';
        Call.OutStatus := 200;
        // Result already = True
      end
      else
      begin
        Call.OutStatus := 404;
        Call.OutBody := 'File not found';
        // Result already = True
      end;
  end;
end;

function TSlftpApiServer.Start: boolean;
var
  model: TOrmModel;
begin
  Result := False;

  if not FEnabled then
  begin
    Debug(dpMessage, rsection, 'API Server disabled in config');
    Exit;
  end;

  try
    // Create ORM model with empty root
    // This prevents mORMot from claiming / as API root
    model := TOrmModel.Create([]);
    try
      // Create REST server
      FRestServer := TRestServerFullMemory.Create(model);
      FRestServer.CreateMissingTables;
      FRestServer.OnErrorUri := OnApiError;
      FRestServer.OnAfterUri := OnApiAfter;
      FRestServer.OnBeforeUri := OnBeforeUri;

      // Register services
      RegisterServices;

      // Set URI root for services to /api
      FRestServer.Model.Root := 'api';

      // Create HTTP server
      FHttpServer := TRestHttpServer.Create(
        UTF8Encode(Format('%s:%d', [FHost, FPort])),
        [FRestServer],
        '+',
        useHttpAsync,
        32,
        secNone
      );

      // Serve index for root explicitly
      FHttpServer.Route.Get('/', ServeIndexHtml);
      // Static files are handled via OnCustomRequest (root + SPA fallback)
      FHttpServer.OnCustomRequest := DoOnCustomRequest;

      Debug(dpMessage, rsection, Format('API Server started on %s:%d', [FHost, FPort]));
      Result := True;

    except
      on E: Exception do
      begin
        Debug(dpError, rsection, Format('[EXCEPTION] Start failed: %s', [E.Message]));
        FreeAndNil(FHttpServer);
        FreeAndNil(FRestServer);
        Result := False;
      end;
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, rsection, Format('[EXCEPTION] Start: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

procedure TSlftpApiServer.Stop;
begin
  try
    if FHttpServer <> nil then
    begin
      FHttpServer.Shutdown;
      FreeAndNil(FHttpServer);
      Debug(dpMessage, rsection, 'HTTP Server stopped');
    end;

    if FRestServer <> nil then
    begin
      FreeAndNil(FRestServer);
      Debug(dpMessage, rsection, 'REST Server stopped');
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, rsection, Format('[EXCEPTION] Stop: %s', [E.Message]));
    end;
  end;
end;

function TSlftpApiServer.IsRunning: boolean;
begin
  Result := (FHttpServer <> nil) and (FRestServer <> nil);
end;

{ Global procedures }

procedure ApiInit;
begin
  try
    if ApiServer <> nil then
      Exit;

    ApiServer := TSlftpApiServer.Create;

    // Connect core issues hook -> API issues store
    GlIssueLogProc := @SlapiIssues_LogIssue;

    // Load config
    ApiServer.Enabled := config.ReadBool('api', 'enabled', False);
    ApiServer.Port := config.ReadInteger('api', 'port', 8089);
    ApiServer.Host := config.ReadString('api', 'host', '127.0.0.1');
    ApiServer.ApiKey := config.ReadString('api', 'apikey', '');

  except
    on E: Exception do
    begin
      Debug(dpError, rsection, Format('[EXCEPTION] ApiInit: %s', [E.Message]));
      FreeAndNil(ApiServer);
    end;
  end;
end;

procedure ApiStart;
begin
  try
    if ApiServer = nil then Exit;

    if ApiServer.Enabled then
    begin
      if not ApiServer.Start then
      begin
        Debug(dpError, rsection, 'Failed to start API server');
      end
      else
      begin
        Debug(dpMessage, rsection, Format('API available at http://%s:%d/api',
          [ApiServer.Host, ApiServer.Port]));
      end;
    end
    else
    begin
      Debug(dpMessage, rsection, 'API server disabled');
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, rsection, Format('[EXCEPTION] ApiStart: %s', [E.Message]));
    end;
  end;
end;

procedure ApiUninit;
begin
  try
    if ApiServer <> nil then
    begin
      ApiServer.Stop;
      FreeAndNil(ApiServer);
      Debug(dpMessage, rsection, 'API Server uninitialized');
    end;
    GlIssueLogProc := nil;
  except
    on E: Exception do
    begin
      Debug(dpError, rsection, Format('[EXCEPTION] ApiUninit: %s', [E.Message]));
    end;
  end;
end;

initialization

finalization
  ApiUninit;

end.
