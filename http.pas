unit http;

interface

{ Fetches HTML Sourcecode for @link(aUrl) with support for HTTP compression, random useragent, SSL and proxy.
  @param(aUrl complete url which should be fetched (gets automatically URL encoded))
  @param(aRecvStr Fetched HTML Sourcecode from given @link(aUrl))
  @param(aErrMsg Holds Exception text, webserver response text for occured failure code or a message if reply was empty)
  @param(aMaxTries Max. number of retries when http get failed - default 2)
  @returns(@true on success, @false on failure, exception or if response was empty) }
function HttpGetUrl(const aUrl: String; out aRecvStr: String; out aErrMsg: String; aMaxTries: Integer = 2): boolean;

implementation

uses
  SysUtils, StrUtils, debugunit, math, IdHTTP, IdURI, IdSSLOpenSSL, IdCompressorZLib, IdSocks, configunit,
  mslproxys, {$IFNDEF UNICODE}IdGlobal,{$ENDIF} IdExceptionCore;

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
label
  TryAgain;
var
  fIdHTTP: TIdHTTP;
  fIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
  fIdSocksInfo: TIdSocksInfo;
  fEncodedUrl: String;
  fProxyname: String;
  fSocks5: TmSLSocks5;
  fNumErrors: Integer;
begin
  Result := False;
  fIdSocksInfo := nil;
  fNumErrors := 0;

  // encodes input URL with % and other defined characters for Uniform Resource Identifier as needed for TIdHTTP
  fEncodedUrl := TIdURI.URLEncode(aUrl);

  try
    fIdHTTP := TIdHTTP.Create(nil);
    try
      with fIdHTTP do
      begin
        Request.UserAgent := UserAgents[RandomRange(0, UserAgentsCount)];

        // enable handling of redirects
        HandleRedirects := True;
        // maximum number of redirects
        RedirectMaximum := 3;

        // time we will wait for beeing connected (milliseconds)
        ConnectTimeout := 3000;
        // time until all data should be read from server (milliseconds)
        ReadTimeout := 3000;

        // needed for using compression (e.g. gzip), automatically requested if Compressor is not nil
        Compressor := TIdCompressorZLib.Create(nil);
      end;

      // socks5 configuration
      if config.ReadInteger('http', 'enabled', 0) <> 0 then
      begin
        fProxyname := config.ReadString('http', 'proxyname', '');
        if (fProxyname <> '') then
        begin
          fSocks5 := FindProxyByName(fProxyname);
          if fSocks5 = nil then
          begin
            Debug(dpError, section, Format('There is no Proxy with name %s for HTTP.', [fProxyname]));
            aErrMsg := Format('There is no Proxy with name %s for HTTP. Check your settings.', [fProxyname]);
            exit;
          end;

          if not fSocks5.Enabled then
          begin
            Debug(dpError, section, Format('Proxy %s for HTTP is disabled.', [fSocks5.Name]));
            aErrMsg := Format('Proxy for HTTP is set to proxy %s but this one is disabled. Skipping HTTP.', [fSocks5.Name]);
            exit;
          end;

          fIdSocksInfo := TIdSocksInfo.Create(nil);
          with fIdSocksInfo do
          begin
            Host := fSocks5.Host;
            Port := fSocks5.Port;
            Version := svSocks5; // we only support socks5

            if (fSocks5.Username <> '') or (fSocks5.Password <> '') then
            begin
              Authentication := saUsernamePassword;
              Username := fSocks5.Username;
              Password := fSocks5.Password;
            end
            else
            begin
              Authentication := saNoAuthentication;
              Username := '';
              Password := '';
            end;

            Enabled := True;
          end;
        end
        else
        begin
          Debug(dpError, section, 'Proxy for HTTP is enabled but Proxyname is empty.');
          aErrMsg := 'Proxy for HTTP is enabled but Proxyname is empty.';
          exit;
        end;
      end;

      // secure connection setup
      fIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      // negotiate highest possible SSL version between client and server
      fIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvSSLv23];

      // set socks5 proxy if configured to use one
      if fIdSocksInfo <> nil then
      begin
        fIdSSLIOHandlerSocketOpenSSL.TransparentProxy := fIdSocksInfo;
      end;
      fIdHTTP.IOHandler := fIdSSLIOHandlerSocketOpenSSL;

      TryAgain:
      Inc(fNumErrors);
      if fNumErrors <= aMaxTries then
      begin
        // reset buffers
        aErrMsg := '';
        // load website
        with fIdHTTP do
        begin
          try
            {$IFNDEF UNICODE}
              // in case of utf8 response content it gets converted correctly to unicode
              // but the string in FPC is still ansistring and thus the unicode is converted
              // back by Indy defaults user encoding Ascii => so set UTF8 encoding explicitly
              aRecvStr := Get(fEncodedUrl, IndyTextEncoding_UTF8);
            {$ELSE}
              aRecvStr := Get(fEncodedUrl);
            {$ENDIF}
          except
            on e: EIdReadTimeout do
            begin
              Debug(dpMessage, section, Format('HTTP GET for %s failed: %s.', [fEncodedUrl, e.Message]));
              aErrMsg := Format('HTTP GET for %s failed: %s.', [fEncodedUrl, e.Message]);
              Debug(dpSpam, section, Format('HTTP RawHeaders: %s', [fIdHTTP.Response.RawHeaders.CommaText]));
            end;
            on e: Exception do
            begin
              Debug(dpError, section, Format('HTTP GET for %s failed due to %d error code <--> %s.', [fEncodedUrl, ResponseCode, ResponseText]));
              Debug(dpError, section, Format('ClassName: %s <--> Exception: %s', [e.ClassName, e.Message]));
              aErrMsg := Format('HTTP GET failed with %d error code <--> %s.', [ResponseCode, ResponseText]);
              Debug(dpSpam, section, Format('HTTP RawHeaders: %s', [fIdHTTP.Response.RawHeaders.CommaText]));
            end;
          end;

          Debug(dpSpam, section, Format('HTTP RawHeaders: %s', [fIdHTTP.Response.RawHeaders.CommaText]));

          if (Length(aRecvStr) = 0) then
          begin
            Debug(dpMessage, section, Format('HTTP GET reply for %s is empty.', [fEncodedUrl]));
            aErrMsg := 'HTTP GET reply is empty.';
          end;
        end;

        if aErrMsg <> '' then
          goto TryAgain;
      end
      else
      begin
        Debug(dpMessage, section, Format('Too many errors while getting website content. URL: %s', [fEncodedUrl]));
        aErrMsg := 'Too many errors while getting website content.';
        exit;
      end;

      Result := True;

    finally
      fIdHTTP.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] HttpGetUrl %s : %s', [fEncodedUrl, e.Message]));
      aErrMsg := Format('[EXCEPTION] HttpGetUrl %s : %s', [fEncodedUrl, e.Message]);
    end;
  end;
end;

end.

