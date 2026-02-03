unit cbftpclient;

interface

uses
  Classes, SysUtils, SyncObjs, mormot.core.base, mormot.core.json, mormot.core.buffers,
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
  end;

implementation

uses
  debugunit;

const
  section = 'cbftpclient';

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
  FHttpClient := TCbftpHttpClient.Create(5000); // 5 second timeout
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

      Debug(dpMessage, section, Format('cbftp %s %s', [aMethod, aPath]));

      // Execute request
      status := FHttpClient.Request(path, aMethod, 0, headers, aBody, '', False);
      FLastStatus := status;
      FLastResponse := FHttpClient.Content;
      FLastError := '';

      if status in [200, 201, 204] then
      begin
        Result := FHttpClient.Content;
        Debug(dpMessage, section, Format('cbftp response: %d', [status]));
      end
      else
      begin
        if status = 404 then
          Debug(dpMessage, section, Format('cbftp request not found: %d %s', [status, url]))
        else
          Debug(dpError, section, Format('cbftp request failed: %d %s - %s', [status, url, FHttpClient.Content]));
      end;
    except
      on E: Exception do
      begin
        FLastStatus := 0;
        FLastResponse := '';
        FLastError := Utf8Encode(E.Message);
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

end.
