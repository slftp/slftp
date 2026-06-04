unit cbftpclient;

interface

uses
  Classes, SysUtils, SyncObjs, mormot.core.base, mormot.core.unicode, mormot.core.json, mormot.core.buffers,
  mormot.net.client, mormot.net.http;

type
  TCbftpHttpClient = class(THttpClientSocket)
  public
    procedure DisableProxy;
  end;

  { cbftp REST API Client
    Provides HTTP-based access to cbftp REST API endpoints }
  TCbftpClient = class
  private
    FHost: RawUtf8;
    FPort: Integer;
    FPassword: RawUtf8;
    FBaseUrl: RawUtf8;
    FHttpClient: THttpClientSocket;
    FLock: TCriticalSection;
    FLastStatus: Integer;
    FLastResponse: RawUtf8;
    FLastUrl: RawUtf8;
    FLastMethod: RawUtf8;
    FLastError: RawUtf8;
    FConnected: Boolean;

    function GetAuthHeader: RawUtf8;
    function DoRequest(const aMethod, aPath: RawUtf8; const aBody: RawUtf8 = ''): RawUtf8;
  public
    constructor Create(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
    destructor Destroy; override;
    property LastStatus: Integer read FLastStatus;
    property LastResponse: RawUtf8 read FLastResponse;
    property LastUrl: RawUtf8 read FLastUrl;
    property LastMethod: RawUtf8 read FLastMethod;
    property LastError: RawUtf8 read FLastError;
    property Connected: Boolean read FConnected;

    { Get list of all sites }
    function GetSites(const aFilters: RawUtf8 = ''): RawUtf8;

    { Get detailed info for a specific site }
    function GetSite(const aSiteName: RawUtf8): RawUtf8;

    { Create a new site }
    function CreateSite(const aBody: RawUtf8): Boolean;

    { Update an existing site }
    function UpdateSite(const aSiteName: RawUtf8; const aBody: RawUtf8): Boolean;

    { Delete a site }
    function DeleteSite(const aSiteName: RawUtf8): Boolean;

    { Get sections for a site }
    function GetSiteSections(const aSiteName: RawUtf8): RawUtf8;

    { Get a single section for a site }
    function GetSiteSection(const aSiteName, aSectionName: RawUtf8): RawUtf8;

    { Create a section on a site }
    function CreateSiteSection(const aSiteName: RawUtf8; const aBody: RawUtf8): Boolean;

    { Update a section on a site }
    function UpdateSiteSection(const aSiteName, aSectionName: RawUtf8; const aBody: RawUtf8): Boolean;

    { Delete a section from a site }
    function DeleteSiteSection(const aSiteName, aSectionName: RawUtf8): Boolean;

    { Get list of all sections }
    function GetSections: RawUtf8;

    { Get detailed info for a specific section }
    function GetSection(const aSectionName: RawUtf8): RawUtf8;

    { Create a new section }
    function CreateSection(const aBody: RawUtf8): Boolean;

    { Update an existing section }
    function UpdateSection(const aSectionName: RawUtf8; const aBody: RawUtf8): Boolean;

    { Delete a section }
    function DeleteSection(const aSectionName: RawUtf8): Boolean;

    { Get list of spread jobs }
    function GetSpreadJobs(const aFilters: RawUtf8 = ''): RawUtf8;

    { Get detailed info for a specific spread job }
    function GetSpreadJob(const aJobName: RawUtf8): RawUtf8;

    { Start a new spread job }
    function StartSpreadJob(const aBody: RawUtf8): Boolean;

    { Reset a spread job }
    function ResetSpreadJob(const aJobName: RawUtf8; const aBody: RawUtf8 = '{}'): Boolean;

    { Abort a spread job }
    function AbortSpreadJob(const aJobName: RawUtf8; const aBody: RawUtf8 = '{}'): Boolean;

    { Get list of transfer jobs }
    function GetTransferJobs(const aFilters: RawUtf8 = ''): RawUtf8;

    { Get detailed info for a specific transfer job }
    function GetTransferJob(const aNameOrId: RawUtf8; aById: Boolean = False): RawUtf8;

    { Start a new transfer job }
    function StartTransferJob(const aBody: RawUtf8): Boolean;

    { Reset a transfer job }
    function ResetTransferJob(const aNameOrId: RawUtf8; aById: Boolean = False): Boolean;

    { Abort a transfer job }
    function AbortTransferJob(const aNameOrId: RawUtf8; aById: Boolean = False): Boolean;

    { Send a raw FTP command to sites }
    function SendRawCommand(const aBody: RawUtf8): RawUtf8;

    { Get result of async raw command }
    function GetRawCommandResult(aId: Integer): RawUtf8;

    { Get cbftp info (version, uptime, stats) }
    function GetInfo: RawUtf8;

    { Start a spread job with explicit parameters }
    function StartSpreadJobEx(const aSection, aRelease: RawUtf8; const aSites: array of RawUtf8): Boolean;

    { Get speed stats from cbftp }
    function GetSpeedStats: RawUtf8;

    { Get completion stats from cbftp }
    function GetCompletionStats: RawUtf8;

    { Get hourly aggregate stats from cbftp }
    function GetHourlyStats: RawUtf8;

    { Get race history stats from cbftp }
    function GetRaceStats: RawUtf8;

    { Get events from cbftp (long-polling) }
    function GetEvents(const aQuery: RawUtf8 = ''): RawUtf8;

    { Download a file from a site via cbftp }
    function GetFile(const aSite, aPath: RawUtf8): RawUtf8;

    { List a directory on a site via cbftp }
    function GetPath(const aSite, aPath: RawUtf8; aTimeout: Integer = 60): RawUtf8;

  end;

  { Global cbftp client instance }
  var
    GlCbftpClient: TCbftpClient;

  { Initialize global cbftp client from config }
  procedure cbftpclient_Init(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);

  { Start a spread job using the global client }
  function cbftpclient_StartSpreadJob(const aSection, aRelease, aSitesCsv: String): Boolean;

implementation

uses
  debugunit, uLkJSON;

const
  section = 'cbftpclient';

function CollapseWhitespace(const s: RawUtf8): RawUtf8;
var
  i, len: Integer;
  lastWasSpace: Boolean;
  c: AnsiChar;
begin
  Result := '';
  len := Length(s);
  if len = 0 then
    Exit;
  SetLength(Result, len);
  lastWasSpace := False;
  len := 0;
  for i := 1 to Length(s) do
  begin
    c := s[i];
    if c in [#9, #10, #13, ' '] then
    begin
      if not lastWasSpace then
      begin
        Inc(len);
        Result[len] := ' ';
        lastWasSpace := True;
      end;
    end
    else
    begin
      Inc(len);
      Result[len] := c;
      lastWasSpace := False;
    end;
  end;
  SetLength(Result, len);
end;

function GetJsonErrorString(const aJson: RawUtf8): RawUtf8;
var
  js: TlkJSONbase;
  obj: TlkJSONObject;
begin
  Result := aJson;
  if aJson = '' then
    Exit;
  try
    js := TlkJSON.ParseText(AnsiString(aJson));
    if js <> nil then
    begin
      try
        if js is TlkJSONObject then
        begin
          obj := TlkJSONObject(js);
          if obj.Field['error'] <> nil then
            Result := RawUtf8(obj.getString('error'));
        end;
      finally
        js.Free;
      end;
    end;
  except
    // Fallback to original JSON on error
  end;
end;

{ TCbftpHttpClient }

procedure TCbftpHttpClient.DisableProxy;
begin
  fExtendedOptions.Proxy := 'none';
end;

{ TCbftpClient }

constructor TCbftpClient.Create(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
begin
  inherited Create;
  FHost := aHost;
  FPort := aPort;
  FPassword := aPassword;
  FBaseUrl := FormatUtf8('https://%:%', [aHost, aPort], [], False);
  FHttpClient := TCbftpHttpClient.Create(30000); // 30 second timeout
  // avoid OS/system proxy for localhost cbftp
  TCbftpHttpClient(FHttpClient).DisableProxy;
  FHttpClient.TLS.IgnoreCertificateErrors := True; // cbftp uses self-signed cert
  FLock := TCriticalSection.Create;
  FLastStatus := 0;
  FLastResponse := '';
  FLastUrl := '';
  FLastMethod := '';
  FLastError := '';
end;

destructor TCbftpClient.Destroy;
begin
  FLock.Free;
  FHttpClient.Free;
  inherited;
end;

function TCbftpClient.GetAuthHeader: RawUtf8;
var
  credentials: RawUtf8;
begin
  // HTTP Basic Auth with empty username
  credentials := BinToBase64(':' + FPassword);
  Result := 'Basic ' + credentials;
end;

function TCbftpClient.DoRequest(const aMethod, aPath: RawUtf8; const aBody: RawUtf8): RawUtf8;
var
  url: RawUtf8;
  headers: RawUtf8;
  status: Integer;
  path: RawUtf8;
begin
  Result := '';

  FLock.Acquire;
  try
    try
      path := aPath;
      if path = '' then
        path := '/'
      else if path[1] <> '/' then
        path := '/' + path;
      url := FBaseUrl + path;
      FLastUrl := url;
      FLastMethod := aMethod;

      if (not FHttpClient.SockIsDefined) or (not FHttpClient.SockConnected) then
        FHttpClient.ConnectUri(FBaseUrl);

      // Build headers
      headers := 'Authorization: ' + GetAuthHeader + #13#10;
      if aBody <> '' then
        headers := headers + 'Content-Type: application/json'#13#10;

      // Execute request
      Debug(dpMessage, section, Format('cbftp %s %s', [aMethod, aPath]));
      status := FHttpClient.Request(path, aMethod, 0, headers, aBody, '', False);
      
      FLastStatus := status;
      FLastResponse := FHttpClient.Content;
      FLastError := '';

      FConnected := True;

      if status in [200, 201, 204] then
      begin
        Result := FHttpClient.Content;
        if Result = '' then
          Result := ' ';
        Debug(dpMessage, section, Format('cbftp response: %d', [status]));
      end
      else
      begin
        if status = 404 then
          Debug(dpMessage, section, Format('cbftp request not found: %d %s', [status, url]))
        else if (status = 503) and (Pos('"error":', FHttpClient.Content) > 0) then
          Debug(dpMessage, section, Format('cbftp request ignored: %d %s - %s', [status, url, GetJsonErrorString(FHttpClient.Content)]))
        else
          Debug(dpError, section, Format('cbftp request failed: %d %s - %s', [status, url, GetJsonErrorString(FHttpClient.Content)]));
      end;
    except
      on E: Exception do
      begin
        FLastStatus := 0;
        FLastResponse := '';
        FLastError := Utf8Encode(E.Message);
        if FConnected then
        begin
          FConnected := False;
          Debug(dpError, section, 'cbftp disconnected');
        end;
        if (Pos('connect', LowerCase(E.Message)) > 0) or
           (Pos('refused', LowerCase(E.Message)) > 0) or
           (Pos('tls failed', LowerCase(E.Message)) > 0) or
           (Pos('econnrefused', LowerCase(E.Message)) > 0) then
          Debug(dpError, section, 'cbftp not reachable')
        else
          Debug(dpError, section, Format('cbftp request exception: %s', [E.Message]));
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TCbftpClient.GetSites(const aFilters: RawUtf8): RawUtf8;
var
  path: RawUtf8;
begin
  path := '/sites';
  if aFilters <> '' then
    path := path + '?' + aFilters;
  Result := DoRequest('GET', path);
end;

function TCbftpClient.GetSite(const aSiteName: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/sites/' + aSiteName);
end;

function TCbftpClient.CreateSite(const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/sites', aBody) <> '';
end;

function TCbftpClient.UpdateSite(const aSiteName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('PATCH', '/sites/' + aSiteName, aBody) <> '';
end;

function TCbftpClient.DeleteSite(const aSiteName: RawUtf8): Boolean;
begin
  Result := DoRequest('DELETE', '/sites/' + aSiteName) <> '';
end;

function TCbftpClient.GetSiteSections(const aSiteName: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/sites/' + aSiteName + '/sections');
end;

function TCbftpClient.GetSiteSection(const aSiteName, aSectionName: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/sites/' + aSiteName + '/sections/' + aSectionName);
end;

function TCbftpClient.CreateSiteSection(const aSiteName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/sites/' + aSiteName + '/sections', aBody) <> '';
end;

function TCbftpClient.UpdateSiteSection(const aSiteName, aSectionName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('PATCH', '/sites/' + aSiteName + '/sections/' + aSectionName, aBody) <> '';
end;

function TCbftpClient.DeleteSiteSection(const aSiteName, aSectionName: RawUtf8): Boolean;
begin
  Result := DoRequest('DELETE', '/sites/' + aSiteName + '/sections/' + aSectionName) <> '';
end;

function TCbftpClient.GetSections: RawUtf8;
begin
  Result := DoRequest('GET', '/sections');
end;

function TCbftpClient.GetSection(const aSectionName: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/sections/' + aSectionName);
end;

function TCbftpClient.CreateSection(const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/sections', aBody) <> '';
end;

function TCbftpClient.UpdateSection(const aSectionName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('PATCH', '/sections/' + aSectionName, aBody) <> '';
end;

function TCbftpClient.DeleteSection(const aSectionName: RawUtf8): Boolean;
begin
  Result := DoRequest('DELETE', '/sections/' + aSectionName) <> '';
end;

function TCbftpClient.GetSpreadJobs(const aFilters: RawUtf8): RawUtf8;
var
  path: RawUtf8;
begin
  path := '/spreadjobs';
  if aFilters <> '' then
    path := path + '?' + aFilters;
  Result := DoRequest('GET', path);
end;

function TCbftpClient.GetSpreadJob(const aJobName: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/spreadjobs/' + aJobName);
end;

function TCbftpClient.StartSpreadJob(const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/spreadjobs', aBody) <> '';
end;

function TCbftpClient.ResetSpreadJob(const aJobName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/spreadjobs/' + aJobName + '/reset', aBody) <> '';
end;

function TCbftpClient.AbortSpreadJob(const aJobName: RawUtf8; const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/spreadjobs/' + aJobName + '/abort', aBody) <> '';
end;

function TCbftpClient.GetTransferJobs(const aFilters: RawUtf8): RawUtf8;
var
  path: RawUtf8;
begin
  path := '/transferjobs';
  if aFilters <> '' then
    path := path + '?' + aFilters;
  Result := DoRequest('GET', path);
end;

function TCbftpClient.GetTransferJob(const aNameOrId: RawUtf8; aById: Boolean): RawUtf8;
var
  path: RawUtf8;
begin
  path := '/transferjobs/' + aNameOrId;
  if aById then
    path := path + '?id=true';
  Result := DoRequest('GET', path);
end;

function TCbftpClient.StartTransferJob(const aBody: RawUtf8): Boolean;
begin
  Result := DoRequest('POST', '/transferjobs', aBody) <> '';
end;

function TCbftpClient.ResetTransferJob(const aNameOrId: RawUtf8; aById: Boolean): Boolean;
var
  path: RawUtf8;
begin
  path := '/transferjobs/' + aNameOrId + '/reset';
  if aById then
    path := path + '?id=true';
  Result := DoRequest('POST', path, '{}') <> '';
end;

function TCbftpClient.AbortTransferJob(const aNameOrId: RawUtf8; aById: Boolean): Boolean;
var
  path: RawUtf8;
begin
  path := '/transferjobs/' + aNameOrId + '/abort';
  if aById then
    path := path + '?id=true';
  Result := DoRequest('POST', path, '{}') <> '';
end;

function TCbftpClient.SendRawCommand(const aBody: RawUtf8): RawUtf8;
begin
  Result := DoRequest('POST', '/raw', aBody);
end;

function TCbftpClient.GetRawCommandResult(aId: Integer): RawUtf8;
begin
  Result := DoRequest('GET', '/raw/' + IntToStr(aId));
end;

function TCbftpClient.GetInfo: RawUtf8;
begin
  Result := DoRequest('GET', '/info');
end;

function TCbftpClient.StartSpreadJobEx(const aSection, aRelease: RawUtf8; const aSites: array of RawUtf8): Boolean;
var
  body: RawUtf8;
  sitesJson: RawUtf8;
  i: Integer;
begin
  sitesJson := '';
  for i := Low(aSites) to High(aSites) do
  begin
    if sitesJson <> '' then
      sitesJson := sitesJson + ',';
    sitesJson := sitesJson + '"' + aSites[i] + '"';
  end;
  body := '{"section":"' + aSection + '","name":"' + aRelease + '","sites":[' + sitesJson + ']}';
  Result := DoRequest('POST', '/spreadjobs', body) <> '';
end;

function TCbftpClient.GetSpeedStats: RawUtf8;
begin
  Result := DoRequest('GET', '/stats/speeds');
end;

function TCbftpClient.GetCompletionStats: RawUtf8;
begin
  Result := DoRequest('GET', '/stats/completion');
end;

function TCbftpClient.GetHourlyStats: RawUtf8;
begin
  Result := DoRequest('GET', '/stats/hourly');
end;

function TCbftpClient.GetRaceStats: RawUtf8;
begin
  Result := DoRequest('GET', '/stats/races');
end;

function TCbftpClient.GetEvents(const aQuery: RawUtf8): RawUtf8;
var
  path: RawUtf8;
begin
  path := '/events';
  if aQuery <> '' then
    path := path + '?' + aQuery;
  Result := DoRequest('GET', path);
end;

function TCbftpClient.GetFile(const aSite, aPath: RawUtf8): RawUtf8;
var
  query: RawUtf8;
begin
  query := FormatUtf8('site=%&path=%', [aSite, aPath], []);
  Result := DoRequest('GET', '/file?' + query);
end;

function TCbftpClient.GetPath(const aSite, aPath: RawUtf8; aTimeout: Integer): RawUtf8;
var
  query: RawUtf8;
begin
  query := FormatUtf8('site=%&path=%&timeout=%', [aSite, aPath, aTimeout], []);
  Result := DoRequest('GET', '/path?' + query);
end;

{ Global helper functions }

procedure cbftpclient_Init(const aHost: RawUtf8; aPort: Integer; const aPassword: RawUtf8);
begin
  FreeAndNil(GlCbftpClient);
  GlCbftpClient := TCbftpClient.Create(aHost, aPort, aPassword);
end;

function cbftpclient_StartSpreadJob(const aSection, aRelease, aSitesCsv: String): Boolean;
var
  sitesArr: array of RawUtf8;
  siteList: TStringList;
  i: Integer;
begin
  Result := False;
  if GlCbftpClient = nil then
    Exit;

  siteList := TStringList.Create;
  try
    siteList.CommaText := aSitesCsv;
    SetLength(sitesArr, siteList.Count);
    for i := 0 to siteList.Count - 1 do
      sitesArr[i] := StringToUtf8(Trim(siteList[i]));
    Result := GlCbftpClient.StartSpreadJobEx(StringToUtf8(aSection), StringToUtf8(aRelease), sitesArr);
  finally
    FreeAndNil(siteList);
  end;
end;

end.
