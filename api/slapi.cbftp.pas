unit slapi.cbftp;

interface

uses
  mormot.core.base, mormot.rest.core, cbftpclient;

{ Handle cbftp proxy requests }
function HandleCbftpRequest(var Call: TRestUriParams): Boolean;

{ Check if cbftp integration is enabled }
function IsCbftpEnabled: Boolean;

{ Serve the public cbftp enabled response }
procedure ServeCbftpEnabledResponse(var aCall: TRestUriParams);

implementation

uses
  SysUtils, StrUtils, configunit, debugunit, mormot.core.unicode, mormot.core.json,
  slcriticalsection2, pazo,
  Classes;

function _QueryParam(const aUrl, aName: string): string;
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

const
  section = 'slapi.cbftp';

var
  GlobalCbftpClient: TCbftpClient = nil;
  GlobalCbftpClientLock: TSLCriticalSection2;

function IsCbftpEnabled: Boolean;
var
  s: string;
begin
  s := LowerCase(config.ReadString('UDPConfig', 'EnableUDP', '0'));
  Result := (s = 'true') or (s = '1') or (s = 'yes');
end;

procedure ServeCbftpEnabledResponse(var aCall: TRestUriParams);
begin
  aCall.OutStatus := 200;
  if IsCbftpEnabled then
    aCall.OutBody := '{"enabled":true}'
  else
    aCall.OutBody := '{"enabled":false}';
  aCall.OutHead := 'Content-Type: application/json';
end;

function GetCbftpClient: TCbftpClient;
var
  host: RawUtf8;
  port: Integer;
  password: RawUtf8;
begin
  if GlobalCbftpClient = nil then
  begin
    GlobalCbftpClientLock.Enter('GetCbftpClient');
    try
      if GlobalCbftpClient <> nil then
      begin
        Result := GlobalCbftpClient;
        Exit;
      end;

      host := StringToUtf8(config.ReadString('UDPConfig', 'IP', '127.0.0.1'));
      port := config.ReadInteger('UDPConfig', 'ApiPort', 0);
      password := StringToUtf8(config.ReadString('UDPConfig', 'Password', ''));

      if (port <= 0) then
      begin
        Debug(dpError, section, 'cbftp API port not configured in [UDPConfig] (ApiPort)');
        Exit(nil);
      end;

      if password = '' then
      begin
        Debug(dpError, section, 'cbftp password not configured in [UDPConfig]');
        Exit(nil);
      end;

      GlobalCbftpClient := TCbftpClient.Create(host, port, password);
      Debug(dpMessage, section, Format('cbftp client initialized: %s:%d', [host, port]));
    finally
      GlobalCbftpClientLock.Leave;
    end;
  end;

  Result := GlobalCbftpClient;
end;

function HandleCbftpRequest(var Call: TRestUriParams): Boolean;
var
  client: TCbftpClient;
  path: RawUtf8;
  method: RawUtf8;
  body: RawUtf8;
  response: RawUtf8;
  success: Boolean;
  query: RawUtf8;
  queryPos: Integer;
  byId: Boolean;
  detail: RawUtf8;
  detailLimit: Integer;
  errorPayload: RawUtf8;
  siteName: RawUtf8;
  sectionName: RawUtf8;

  function QueryHasIdTrue(const aQuery: RawUtf8): Boolean;
  var
    queryLower: string;
  begin
    Result := False;
    if aQuery = '' then
      Exit;
    queryLower := LowerCase(Utf8ToString(aQuery));
    Result := (Pos('id=true', queryLower) > 0) or (Pos('id=1', queryLower) > 0);
  end;

  function TruncateUtf8(const aText: RawUtf8; aMaxLen: Integer): RawUtf8;
  begin
    if (aMaxLen > 0) and (length(aText) > aMaxLen) then
      Result := Copy(aText, 1, aMaxLen) + '...'
    else
      Result := aText;
  end;

  function ExtractCbftpPath(const aUrl: RawUtf8): RawUtf8;
  begin
    Result := aUrl;
    if (Result <> '') and (Result[1] <> '/') then
      Result := '/' + Result;
    if StartsStr('/api/cbftp', Result) then
      Delete(Result, 1, Length('/api/cbftp'))
    else if StartsStr('/cbftp', Result) then
      Delete(Result, 1, Length('/cbftp'));
    // Remove query string for path check
    if Pos('?', Result) > 0 then
      Result := Copy(Result, 1, Pos('?', Result) - 1);
  end;

begin
  Result := False;

  // Handle /enabled endpoint first - always works regardless of config
  path := ExtractCbftpPath(Call.Url);
  if (Call.Method = 'GET') and (path = '/enabled') then
  begin
    ServeCbftpEnabledResponse(Call);
    Exit(True);
  end;

  // Handle /latencies endpoint - slftp internal latency data (PRE to UDP send)
  if (Call.Method = 'GET') and (path = '/latencies') then
  begin
    Call.OutStatus := 200;
    Call.OutBody := StringToUtf8(CbftpLatencyGetJson);
    Call.OutHead := 'Content-Type: application/json';
    Exit(True);
  end;

  // If cbftp is disabled, reject all other requests
  if not IsCbftpEnabled then
  begin
    Call.OutStatus := 503;
    Call.OutBody := '{"error":"cbftp integration is disabled","enabled":false}';
    Call.OutHead := 'Content-Type: application/json';
    Exit(True);
  end;

  client := GetCbftpClient;
  if client = nil then
  begin
    Call.OutStatus := 500;
    Call.OutBody := '{"error":"cbftp client not configured"}';
    Call.OutHead := 'Content-Type: application/json';
    Exit(True);
  end;

  // Extract path after /api/cbftp/ or /cbftp/
  path := Call.Url;
  if (path <> '') and (path[1] <> '/') then
    path := '/' + path;
  if StartsStr('/api/cbftp', path) then
    Delete(path, 1, Length('/api/cbftp'))
  else if StartsStr('/cbftp', path) then
    Delete(path, 1, Length('/cbftp'));

  query := '';
  queryPos := Pos('?', path);
  if queryPos > 0 then
  begin
    query := Copy(path, queryPos + 1, MaxInt);
    path := Copy(path, 1, queryPos - 1);
  end;

  method := Call.Method;
  body := Call.InBody;

  Debug(dpMessage, section, Format('HandleCbftpRequest: %s %s', [method, path]));

  success := False;
  response := '';

  try
    // Route to appropriate cbftp API endpoint
    if method = 'GET' then
    begin
      if path = '/info' then
        response := client.GetInfo
      else if StartsStr('/sites/', path) then
      begin
        Delete(path, 1, 7); // Remove '/sites/'
        if Pos('/sections/', path) > 0 then
        begin
          // /sites/{name}/sections/{sectionName} - get single section
          siteName := Copy(path, 1, Pos('/sections/', path) - 1);
          sectionName := Copy(path, Pos('/sections/', path) + 10, MaxInt);
          response := client.GetSiteSection(siteName, sectionName);
        end
        else if Pos('/sections', path) > 0 then
        begin
          // /sites/{name}/sections - get all sections
          Delete(path, Pos('/sections', path), MaxInt);
          response := client.GetSiteSections(path);
        end
        else
          response := client.GetSite(path);
      end
      else if path = '/sites' then
        response := client.GetSites(query)
      else if StartsStr('/sections/', path) then
      begin
        Delete(path, 1, 10); // Remove '/sections/'
        response := client.GetSection(path);
      end
      else if path = '/sections' then
        response := client.GetSections
      else if StartsStr('/spreadjobs/', path) then
      begin
        Delete(path, 1, 12); // Remove '/spreadjobs/'
        response := client.GetSpreadJob(path);
      end
      else if path = '/spreadjobs' then
        response := client.GetSpreadJobs(query)
      else if StartsStr('/transferjobs/', path) then
      begin
        Delete(path, 1, 14); // Remove '/transferjobs/'
        byId := QueryHasIdTrue(query);
        response := client.GetTransferJob(path, byId);
      end
      else if path = '/transferjobs' then
        response := client.GetTransferJobs(query)
      else if StartsStr('/raw/', path) then
      begin
        Delete(path, 1, 5); // Remove '/raw/'
        response := client.GetRawCommandResult(StrToIntDef(Utf8ToString(path), 0));
      end
      else if path = '/stats/speeds' then
        response := client.GetSpeedStats
      else if path = '/stats/completion' then
        response := client.GetCompletionStats
      else if path = '/stats/hourly' then
        response := client.GetHourlyStats
      else if path = '/stats/races' then
        response := client.GetRaceStats
      else if path = '/path' then
      begin
        response := client.GetPath(
          StringToUtf8(_QueryParam(Utf8ToString(Call.Url), 'site')),
          StringToUtf8(_QueryParam(Utf8ToString(Call.Url), 'path')),
          StrToIntDef(_QueryParam(Utf8ToString(Call.Url), 'timeout'), 60)
        );
      end;

      success := response <> '';
    end
    else if method = 'POST' then
    begin
      if StartsStr('/sites/', path) and (Pos('/sections', path) > 0) then
      begin
        // POST /sites/{name}/sections - create section on site
        Delete(path, 1, 7); // Remove '/sites/'
        siteName := Copy(path, 1, Pos('/sections', path) - 1);
        success := client.CreateSiteSection(siteName, body);
      end
      else if path = '/sites' then
        success := client.CreateSite(body)
      else if path = '/sections' then
        success := client.CreateSection(body)
      else if path = '/spreadjobs' then
        success := client.StartSpreadJob(body)
      else if StartsStr('/spreadjobs/', path) and EndsStr('/reset', path) then
      begin
        Delete(path, 1, 12); // Remove '/spreadjobs/'
        Delete(path, Pos('/reset', path), MaxInt);
        success := client.ResetSpreadJob(path, body);
      end
      else if StartsStr('/spreadjobs/', path) and EndsStr('/abort', path) then
      begin
        Delete(path, 1, 12); // Remove '/spreadjobs/'
        Delete(path, Pos('/abort', path), MaxInt);
        success := client.AbortSpreadJob(path, body);
      end
      else if path = '/transferjobs' then
        success := client.StartTransferJob(body)
      else if StartsStr('/transferjobs/', path) and EndsStr('/reset', path) then
      begin
        Delete(path, 1, 14); // Remove '/transferjobs/'
        Delete(path, Pos('/reset', path), MaxInt);
        byId := QueryHasIdTrue(query);
        success := client.ResetTransferJob(path, byId);
      end
      else if StartsStr('/transferjobs/', path) and EndsStr('/abort', path) then
      begin
        Delete(path, 1, 14); // Remove '/transferjobs/'
        Delete(path, Pos('/abort', path), MaxInt);
        byId := QueryHasIdTrue(query);
        success := client.AbortTransferJob(path, byId);
      end
      else if path = '/raw' then
      begin
        response := client.SendRawCommand(body);
        success := response <> '';
      end;

      // Only set default success response if we actually succeeded AND have no response body
      if success and (response = '') then
        response := '{"success":true}';
    end
    else if method = 'PATCH' then
    begin
      if StartsStr('/sites/', path) and (Pos('/sections/', path) > 0) then
      begin
        // PATCH /sites/{name}/sections/{sectionName} - update section on site
        Delete(path, 1, 7); // Remove '/sites/'
        siteName := Copy(path, 1, Pos('/sections/', path) - 1);
        sectionName := Copy(path, Pos('/sections/', path) + 10, MaxInt);
        success := client.UpdateSiteSection(siteName, sectionName, body);
      end
      else if StartsStr('/sites/', path) then
      begin
        Delete(path, 1, 7); // Remove '/sites/'
        success := client.UpdateSite(path, body);
      end
      else if StartsStr('/sections/', path) then
      begin
        Delete(path, 1, 10); // Remove '/sections/'
        success := client.UpdateSection(path, body);
      end;

      if success then
        response := '{"success":true}';
    end
    else if method = 'DELETE' then
    begin
      if StartsStr('/sites/', path) and (Pos('/sections/', path) > 0) then
      begin
        // DELETE /sites/{name}/sections/{sectionName} - delete section from site
        Delete(path, 1, 7); // Remove '/sites/'
        siteName := Copy(path, 1, Pos('/sections/', path) - 1);
        sectionName := Copy(path, Pos('/sections/', path) + 10, MaxInt);
        success := client.DeleteSiteSection(siteName, sectionName);
      end
      else if StartsStr('/sites/', path) then
      begin
        Delete(path, 1, 7); // Remove '/sites/'
        success := client.DeleteSite(path);
      end
      else if StartsStr('/sections/', path) then
      begin
        Delete(path, 1, 10); // Remove '/sections/'
        success := client.DeleteSection(path);
      end;

      if success then
        response := '{"success":true}';
    end;

    if success or (response <> '') then
    begin
      Call.OutStatus := 200;
      Call.OutBody := response;
      Call.OutHead := 'Content-Type: application/json';
    end
    else
    begin
      if (client <> nil) and (client.LastStatus <> 0) then
        Call.OutStatus := client.LastStatus
      else
        Call.OutStatus := 500;

      detailLimit := 512;
      if client <> nil then
      begin
        detail := TruncateUtf8(client.LastResponse, detailLimit);
        if (detail = '') and (client.LastError <> '') then
          detail := TruncateUtf8(client.LastError, detailLimit);
        errorPayload := JsonEncode([
          'error', 'cbftp request failed',
          'status', client.LastStatus,
          'url', client.LastUrl,
          'method', client.LastMethod,
          'detail', detail
        ]);
        Call.OutBody := errorPayload;
      end
      else
        Call.OutBody := '{"error":"cbftp request failed"}';
      Call.OutHead := 'Content-Type: application/json';
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('cbftp request error: %s', [E.Message]));
      Call.OutStatus := 500;
      Call.OutBody := Format('{"error":"%s"}', [E.Message]);
      Call.OutHead := 'Content-Type: application/json';
    end;
  end;

  Result := True;
end;

initialization
  GlobalCbftpClientLock := TSLCriticalSection2.Create('CbftpClient');

finalization
  FreeAndNil(GlobalCbftpClient);
  FreeAndNil(GlobalCbftpClientLock);

end.
