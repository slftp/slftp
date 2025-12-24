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
  configunit,
  debugunit;

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

{ Shutdown API server }
procedure ApiUninit;

implementation

{$I ../slftp.inc}

const
  rsection = 'slapi';

var
  ApiServer: TSlftpApiServer = nil;

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
    TypeInfo(IApiBrowserService)
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
  basePath: string;
  normalizedPath: string;
  authHeader: RawUTF8;
  providedKey: RawUTF8;
  idx, crlfPos, colonPos: Integer;
  headerLine: string;
  
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

begin
  Result := False; // By default, let mORMot handle

  // Convert URL
  sUrl := UTF8ToString(Call.Url);

  // Normalize leading slash so comparisons work for both "/api/..." and "api/..."
  if sUrl = '' then
    sUrl := '/'
  else if sUrl[1] <> '/' then
    sUrl := '/' + sUrl;

  // If it's an API call (/api/...), check authentication first
  if (Length(sUrl) >= 4) and (Copy(UpperCase(sUrl), 1, 4) = '/API') then
  begin
    // Check API key if configured
    if FApiKey <> '' then
    begin
      // Parse Authorization header from InHead
      authHeader := '';
      if Pos('AUTHORIZATION:', UpperCase(Call.InHead)) > 0 then
      begin
        // Extract value after "Authorization: "
        idx := Pos('AUTHORIZATION:', UpperCase(Call.InHead));
        headerLine := Copy(Call.InHead, idx, MaxInt);
        crlfPos := Pos(#13, headerLine);
        if crlfPos > 0 then
          headerLine := Copy(headerLine, 1, crlfPos - 1);
        // Get value after "Authorization: "
        colonPos := Pos(':', headerLine);
        if colonPos > 0 then
          authHeader := Trim(Copy(headerLine, colonPos + 1, MaxInt));
      end;

      if authHeader = '' then
      begin
        Call.OutStatus := HTTP_FORBIDDEN;
        Call.OutBody := 'Missing Authorization header';
        Exit(True); // We handled it (rejected)
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
        Exit(True); // We handled it (rejected)
      end;
    end;

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
        FreeAndNil(model);
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

    if ApiServer.Enabled then
    begin
      if not ApiServer.Start then
      begin
        Debug(dpError, rsection, 'Failed to start API server');
        FreeAndNil(ApiServer);
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
      Debug(dpError, rsection, Format('[EXCEPTION] ApiInit: %s', [E.Message]));
      FreeAndNil(ApiServer);
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
