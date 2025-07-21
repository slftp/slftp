unit slssl;

interface

uses
mormot.lib.openssl11, mormot.core.os, mormot.crypt.openssl;

{ Get the full OpenSSL version string including version, compiler flags, built date and platform info
  @returns(OpenSSL version string + additional info) }
function GetOpenSSLVersion: String;

{ Get the Information if openssl is available
  @returns(OpenSSL version string + additional info) }
function GetOpenSSLAvailable: boolean;

{ Get the full OpenSSL version
  @returns(OpenSSL version string) }
function GetOpenSSLShortVersion: String;

{ Initialize the SSL_CTX object with default settings
  @param(aError Error message from OpenSSL in case it couldn't initialize)
  @returns(@true on success, @false otherwise) }
function InitOpenSSLConnectionContext(out aError: String): Boolean;

{ Uninitialize the SSL_CTX object with default settings and unload }
procedure UninitOpenSSLConnectionContext;

{ Get the SSL_CTX object as framework to establish TLS/SSL connections
  @returns(SSL_CTX object with some default configuration) }
function GetOpenSSLConnectionContext: PSSL_CTX;

{ Return string representing error result code of TLS/SSL I/O operation @br
  @note(also checks the current thread's OpenSSL error queue)
  @param(aSSL SSL which did the call)
  @param(aErrCode Error code of previous OpenSSL API call)
  @returns(String that indicates the error) }
function GetLastSSLError(const aSSL: PSSL; const aErrCode: Integer): String;

{ Loads OpenSSL.
  @param(aError In case an error occurs, this out parameter will contains some info.)
  @returns(True, in case of success. False otherwise.) }
function InitOpenSSL(out aError: String): boolean;

implementation

uses
  SysUtils;

//const
//  gSSL_Default_Cipher_List = 'ALL:!EXP';
//  gSSL_Default_Cipher_List = 'HIGH'; // probably more secure

var
  gSSLContextSettings: PSSL_CTX = nil; // default SSL/TLS context used for all connections

// returns the earliest error code from the thread's error queue and removes the entry
// can be called repeatedly until there are no more error codes to return.
function _GetEarliestOpenSSLErrorCode: String;
var
  fErrStr: String;
  fErrors: Integer;
  fErrCode: Cardinal;
begin
  Result := '';
  SetLength(fErrStr, 255);
  fErrors := 0;
  while (true) do
  begin
    fErrCode := ERR_get_error();
    if fErrCode = 0 then
      Break;
    ERR_error_string_n(fErrCode, @fErrStr[1], Length(fErrStr));
    Inc(fErrors);

    if Result <> '' then
      Result := Result + ' / ';

    Result := Result + fErrStr;
  end;

  if fErrors = 0 then
    Result := 'NO SSL ERROR, THIS CALL SHOULD HAVE NOT HAPPEN!';
end;

function GetOpenSSLVersion: String;
begin
  if OpenSslIsLoaded then
  begin
    // Get and display the OpenSSL version
    Result := OpenSslVersionText;
  end;

end;

function GetOpenSSLAvailable: boolean;
begin
  Result := OpenSslIsLoaded;
end;

function initOpenSsl(out aError: String): boolean;
var
  fLoadedProvider: POSSL_PROVIDER;
begin
  Result := false;
  aError := '';

  if not OpenSslIsLoaded then
  begin
    OpenSslDefaultPath := ExtractFilePath(ParamStr(0));
    Result := OpenSslInitialize('','');
    if Result then
      RegisterOpenSsl
    else
    begin
      aError := 'OpenSslInitialize failed! can not load openssl!';
      exit;
    end;

    if OpenSslVersion >= OPENSSL3_VERNUM then
    begin
      // on windows to be able to load the legacy.dll we need to OSSL_PROVIDER_set_default_search_path
      {$IFDEF MSWINDOWS}
        {$IFDEF UNICODE}
          OSSL_PROVIDER_set_default_search_path(NIL, PAnsiChar(Pointer(AnsiString(ExtractFilePath(ParamStr(0))))));
        {$ELSE}
          OSSL_PROVIDER_set_default_search_path(NIL, PAnsiChar(ExtractFilePath(ParamStr(0))));
        {$ENDIF}
      {$ENDIF}

      fLoadedProvider := OSSL_PROVIDER_load(NIL, 'default');
      if fLoadedProvider = NIL THEN
      begin
        aError := 'default ssl provider not loaded!';
        Result := False;
        exit;
      end;
      fLoadedProvider := OSSL_PROVIDER_load(NIL, 'legacy');
      if fLoadedProvider = NIL THEN
      begin
        aError := 'legacy ssl provider not loaded!';
        Result := False;
        exit;
      end;

      Result := True;
      Exit;
    end;
    if OpenSslVersion < OPENSSL1_VERNUM then
    begin
      aError := 'Openssl-Version is too old! Please Update!';
      Result := False;
      exit;
    end;
  end
  else
  begin
    Result := true;
  end;
end;

function GetOpenSSLShortVersion: String;
begin
    Result := Int64(High(OpenSSL_version_num)).ToString();
end;

function InitOpenSSLConnectionContext(out aError: String): Boolean;
begin
  Result := False;

  if OpenSslIsAvailable then
  begin
  gSSLContextSettings := SSL_CTX_new(TLS_client_method());
    if (gSSLContextSettings = nil) then
    begin
      aError := _GetEarliestOpenSSLErrorCode;
      exit;
    end;

    SSL_CTX_set_default_verify_paths(gSSLContextSettings);
  end;

  Result := True;
end;

procedure UninitOpenSSLConnectionContext;
begin
  if gSSLContextSettings <> nil then
  begin
    SSL_CTX_free(gSSLContextSettings);
  end;
end;

function GetOpenSSLConnectionContext: PSSL_CTX;
begin
  Result := gSSLContextSettings;
end;

function GetLastSSLError(const aSSL: PSSL; const aErrCode: Integer): String;
var
  fErrorCode: Integer;
begin
  fErrorCode := SSL_get_error(aSSL, aErrCode);
  case fErrorCode of
    SSL_ERROR_NONE: Result := 'no error';
    SSL_ERROR_ZERO_RETURN: Result := 'zero return';
    SSL_ERROR_WANT_READ: Result := 'want read';
    SSL_ERROR_WANT_WRITE: Result := 'want write';
    SSL_ERROR_WANT_CONNECT: Result := 'want connect';
    SSL_ERROR_WANT_ACCEPT: Result := 'want accept';
    SSL_ERROR_WANT_X509_LOOKUP: Result := 'x509 lookup wanted';
    SSL_ERROR_WANT_ASYNC: Result := 'want async';
    SSL_ERROR_WANT_ASYNC_JOB: Result := 'want async job';
    SSL_ERROR_WANT_CLIENT_HELLO_CB: Result := 'want client hello callback';
    SSL_ERROR_SYSCALL: Result := 'syscall error';
    SSL_ERROR_SSL: Result := 'ssl error';
  else
    Result := 'unknown error';
  end;
end;

end.
