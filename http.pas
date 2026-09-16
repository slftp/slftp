unit http;

interface

{ Fetches HTML Sourcecode for @link(aUrl) with support for HTTP compression, random useragent, SSL and proxy.
  @param(aUrl complete url which should be fetched (gets automatically URL encoded))
  @param(aRecvStr Fetched HTML Sourcecode from given @link(aUrl))
  @param(aErrMsg Holds Exception text, webserver response text for occured failure code or a message if reply was empty)
  @param(aMaxTries Max. number of retries when http get failed - default 2)
  @returns(@true on success, @false on failure, exception or if response was empty) }
function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer = 2): boolean; overload;
function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer; out aOutStatus: Integer): boolean; overload;
{ Same as above, plus @param(aExtraHeaders) raw extra request headers (e.g. an auth token), appended after the User-Agent header. Use #13#10 to separate multiple headers. }
function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer; out aOutStatus: Integer; const aExtraHeaders: String): boolean; overload;
{ Posts @link(aJsonBody) as application/json to @link(aUrl) with support for SSL, random useragent and proxy.
  @param(aUrl complete url to post to)
  @param(aJsonBody raw JSON payload sent as request body)
  @param(aRecvStr Fetched response body from given @link(aUrl))
  @param(aErrMsg Holds Exception text, webserver response text for occured failure code or a message if reply was empty)
  @param(aMaxTries Max. number of retries when http post failed - default 2)
  @param(aExtraHeaders raw extra request headers (e.g. Origin/Referer), appended after the User-Agent header. Use #13#10 to separate multiple headers. }
function HttpPostJsonUrl(const aUrl: String; const aJsonBody: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer = 2; const aExtraHeaders: String = ''): boolean;

implementation

uses
  SysUtils, StrUtils, debugunit, math, configunit, mormot.core.base, mormot.core.data, mormot.core.os, mormot.net.client, mormot.net.sock, mormot.core.buffers;

const
  section = 'http';
  UserAgentsCount = 3;
  UserAgents: array[0..UserAgentsCount] of String = (
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:136.0) Gecko/20100101 Firefox/136.0',
    'Mozilla/5.0 (X11; Linux x86_64; rv:136.0) Gecko/20100101 Firefox/136.0',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.0',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36'
  );

function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer): boolean;
var
  fStatus: Integer;
begin
  Result := HttpGetUrl(aUrl, aRecvStr, aErrMsg, aMaxTries, fStatus);
end;

function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer; out aOutStatus: Integer): boolean;
label
  TryAgain;
var
  fNumErrors: Integer;
  fOutHeaders: RawUtf8;
  fInHeaders: RawUtf8;
  fRandomUserAgent: String;
begin
  Result := False;
  fNumErrors := 0;
  fOutHeaders := '';
  aOutStatus := 0;

  // Select random User-Agent
  fRandomUserAgent := UserAgents[Random(UserAgentsCount + 1)];
  fInHeaders := 'User-Agent: ' + fRandomUserAgent;

  TryAgain:
  Inc(fNumErrors);
  if fNumErrors <= aMaxTries then
  begin
    // reset buffers
    aErrMsg := '';
    // load website
    try
      aRecvStr := HttpGet(aUrl, fInHeaders, @fOutHeaders, {forceNotSocket:}False, @aOutStatus, {timeout:}0, {forcesocket:}False, {ignoreTlsCertError:}True);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('HTTP GET for %s failed due to error <--> %s.', [aUrl, Utf8ToString(fOutHeaders)]));
        Debug(dpError, section, Format('ClassName: %s <--> Exception: %s', [e.ClassName, e.Message]));
        aErrMsg := Format('HTTP GET failed with error <--> %s.', [e.Message]);
      end;
    end;

    if aErrMsg = '' then
    begin
      if aOutStatus = 404 then
      begin
        aErrMsg := Format('HTTP GET failed with 404 Not Found. (%s)', [aUrl]);
        exit;
      end;

      if ((aOutStatus >= 400) and (aOutStatus < 600)) then
      begin
        aErrMsg := Format('HTTP GET failed with status %d. (%s)', [aOutStatus, aUrl]);
        exit;
      end;
    end;

    if (Length(aRecvStr) = 0) and (aErrMsg = '') then
    begin
      Debug(dpError, section, Format('HTTP GET reply for %s is empty (%s / %d).', [aUrl, Utf8ToString(fOutHeaders), aOutStatus]));
      aErrMsg := Format('HTTP GET reply is empty. (%s / %d)', [Utf8ToString(fOutHeaders), aOutStatus]);
    end;

    if aErrMsg <> '' then
      goto TryAgain;
  end
  else
  begin
    Debug(dpError, section, Format('Too many errors while getting website content. URL: %s Error: %s', [aUrl, aErrMsg]));
    aErrMsg := Format('Too many errors while getting website content. (%s)', [aErrMsg]);
    exit;
  end;

  Result := True;
end;

function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer; out aOutStatus: Integer; const aExtraHeaders: String): boolean;
label
  TryAgain;
var
  fNumErrors: Integer;
  fOutHeaders: RawUtf8;
  fInHeaders: RawUtf8;
  fRandomUserAgent: String;
begin
  Result := False;
  fNumErrors := 0;
  fOutHeaders := '';
  aOutStatus := 0;

  // Select random User-Agent
  fRandomUserAgent := UserAgents[Random(UserAgentsCount + 1)];
  fInHeaders := 'User-Agent: ' + fRandomUserAgent;
  if aExtraHeaders <> '' then
    fInHeaders := fInHeaders + #13#10 + aExtraHeaders;

  TryAgain:
  Inc(fNumErrors);
  if fNumErrors <= aMaxTries then
  begin
    // reset buffers
    aErrMsg := '';
    // load website
    try
      aRecvStr := HttpGet(aUrl, fInHeaders, @fOutHeaders, {forceNotSocket:}False, @aOutStatus, {timeout:}0, {forcesocket:}False, {ignoreTlsCertError:}True);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('HTTP GET for %s failed due to error <--> %s.', [aUrl, Utf8ToString(fOutHeaders)]));
        Debug(dpError, section, Format('ClassName: %s <--> Exception: %s', [e.ClassName, e.Message]));
        aErrMsg := Format('HTTP GET failed with error <--> %s.', [e.Message]);
      end;
    end;

    if aErrMsg = '' then
    begin
      if aOutStatus = 404 then
      begin
        aErrMsg := Format('HTTP GET failed with 404 Not Found. (%s)', [aUrl]);
        exit;
      end;

      if ((aOutStatus >= 400) and (aOutStatus < 600)) then
      begin
        aErrMsg := Format('HTTP GET failed with status %d. (%s)', [aOutStatus, aUrl]);
        exit;
      end;
    end;

    if (Length(aRecvStr) = 0) and (aErrMsg = '') then
    begin
      Debug(dpError, section, Format('HTTP GET reply for %s is empty (%s / %d).', [aUrl, Utf8ToString(fOutHeaders), aOutStatus]));
      aErrMsg := Format('HTTP GET reply is empty. (%s / %d)', [Utf8ToString(fOutHeaders), aOutStatus]);
    end;

    if aErrMsg <> '' then
      goto TryAgain;
  end
  else
  begin
    Debug(dpError, section, Format('Too many errors while getting website content. URL: %s Error: %s', [aUrl, aErrMsg]));
    aErrMsg := Format('Too many errors while getting website content. (%s)', [aErrMsg]);
    exit;
  end;

  Result := True;
end;

function HttpPostJsonUrl(const aUrl: String; const aJsonBody: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer; const aExtraHeaders: String): boolean;
label
  TryAgain;
var
  fNumErrors: Integer;
  fStatus: Integer;
  fOutHeaders: RawUtf8;
  fInHeaders: RawUtf8;
  fRandomUserAgent: String;
  fUri: TUri;
  fClient: THttpClientSocket;
begin
  Result := False;
  fNumErrors := 0;

  if not fUri.From(aUrl) then
  begin
    aErrMsg := Format('HTTP POST failed with invalid URL. (%s)', [aUrl]);
    Debug(dpError, section, aErrMsg);
    exit;
  end;

  TryAgain:
  Inc(fNumErrors);
  if fNumErrors <= aMaxTries then
  begin
    // reset buffers
    aErrMsg := '';
    aRecvStr := '';
    fClient := THttpClientSocket.Create({timeout:}0);
    try
      fClient.TLS.IgnoreCertificateErrors := True;
      // Select random User-Agent (set via property, otherwise the socket sends
      // its own default UA in addition to the raw header)
      fRandomUserAgent := UserAgents[Random(UserAgentsCount + 1)];
      fClient.UserAgent := fRandomUserAgent;
      fInHeaders := aExtraHeaders;

      // load website
      try
        fClient.ConnectUri(aUrl);
        fStatus := fClient.Request(fUri.Address, 'POST', {KeepAlive:}0, fInHeaders, aJsonBody, 'application/json', {AsRetry:}False);
        aRecvStr := Utf8ToString(fClient.Content);
        fOutHeaders := fClient.Headers;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('HTTP POST for %s failed due to error <--> %s.', [aUrl, e.Message]));
          Debug(dpError, section, Format('ClassName: %s <--> Exception: %s', [e.ClassName, e.Message]));
          aErrMsg := Format('HTTP POST failed with error <--> %s.', [e.Message]);
        end;
      end;

      if aErrMsg = '' then
      begin
        if ((fStatus >= 400) and (fStatus < 600)) then
        begin
          aErrMsg := Format('HTTP POST failed with status %d. (%s)', [fStatus, aUrl]);
          exit;
        end;

        if (Length(aRecvStr) = 0) and (aErrMsg = '') then
        begin
          Debug(dpError, section, Format('HTTP POST reply for %s is empty (%s / %d).', [aUrl, Utf8ToString(fOutHeaders), fStatus]));
          aErrMsg := Format('HTTP POST reply is empty. (%s / %d)', [Utf8ToString(fOutHeaders), fStatus]);
        end;
      end;
    finally
      fClient.Free;
    end;

    if aErrMsg <> '' then
      goto TryAgain;
  end
  else
  begin
    Debug(dpError, section, Format('Too many errors while posting website content. URL: %s Error: %s', [aUrl, aErrMsg]));
    aErrMsg := Format('Too many errors while posting website content. (%s)', [aErrMsg]);
    exit;
  end;

  Result := True;
end;

end.

