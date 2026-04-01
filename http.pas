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

implementation

uses
  SysUtils, StrUtils, debugunit, math, configunit, mormot.core.base, mormot.core.data, mormot.core.os, mormot.net.client, mormot.core.buffers;

const
  section = 'http';
  UserAgentsCount = 3;
  UserAgents: array[0..UserAgentsCount] of String = (
    'Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:65.0) Gecko/20100101 Firefox/65.0',
    'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:65.0) Gecko/20100101 Firefox/65.0',
    'Mozilla/5.0 (Windows NT 10.0; WOW64) Gecko/20100101 Firefox/64.0',
    'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:64.0) Gecko/20100101 Firefox/64.0'
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

    if (Length(aRecvStr) = 0) and (aErrMsg = '') then
    begin
      if aOutStatus = 404 then
      begin
        aErrMsg := Format('HTTP GET failed with 404 Not Found. (%s)', [aUrl]);
        exit; // don't retry on 404
      end;

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

end.

