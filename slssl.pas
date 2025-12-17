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
  @param(aSslReturnCode Return code of previous OpenSSL API call)
  @returns(String that indicates the error) }
function GetLastSSLError(const aSSL: PSSL; const aSslReturnCode: Integer): String;

{ Get the error code of the latest error from OpenSSL.
  @param(aSSL SSL which did the call)
  @param(aSslReturnCode Return code of previous OpenSSL API call)
  @returns(The OpenSSL error code) }
function GetLastSSLErrorCode(const aSSL: PSSL; const aSslReturnCode: Integer): Integer;

{ Get the corresponding error message to the given OpenSSL error code.
  @param(aSSL SSL which did the call)
  @param(aSslErrorCode The error code to get the message for)
  @returns(String that indicates the error) }
function GetSSLErrorMessageFromErrorCode(const aSSL: PSSL; const aSslErrorCode: Integer): String;


{ Loads OpenSSL.
  @param(aError In case an error occurs, this out parameter will contains some info.)
  @returns(True, in case of success. False otherwise.) }
function InitOpenSSL(out aError: String): boolean;

implementation

uses
  SysUtils, mormot.core.base;

var
  gSSLContextSettings: PSSL_CTX = nil; // default SSL/TLS context used for all connections

const
  // SSL session cache mode constants (not defined in mormot.lib.openssl11)
  SSL_SESS_CACHE_OFF    = $0000;
  SSL_SESS_CACHE_CLIENT = $0001;
  SSL_SESS_CACHE_SERVER = $0002;
  SSL_SESS_CACHE_BOTH   = SSL_SESS_CACHE_CLIENT or SSL_SESS_CACHE_SERVER;

  // SSL mode constants for SSL_CTX_set_mode (not defined in mormot.lib.openssl11)
  SSL_MODE_ENABLE_PARTIAL_WRITE         = $00000001;  // Allow partial writes
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER   = $00000002;  // Avoid unnecessary buffer copying
  SSL_MODE_AUTO_RETRY                   = $00000004;  // Auto-retry on non-fatal errors
  SSL_MODE_RELEASE_BUFFERS              = $00000010;  // Free read/write buffers when idle

{ Helper function to set SSL session cache size via SSL_CTX_ctrl }
function SSL_CTX_sess_set_cache_size(ctx: PSSL_CTX; size: integer): integer; inline;
begin
  Result := SSL_CTX_ctrl(ctx, 42 {SSL_CTRL_SET_SESS_CACHE_SIZE}, size, nil);
end;

{ Helper function to set SSL modes via SSL_CTX_ctrl }
function SSL_CTX_set_mode(ctx: PSSL_CTX; mode: cardinal): cardinal; inline;
begin
  Result := SSL_CTX_ctrl(ctx, 33 {SSL_CTRL_MODE}, mode, nil);
end;

// returns the earliest error code from the thread's error queue and removes the entry
// can be called repeatedly until there are no more error codes to return.
function _GetEarliestOpenSSLErrorCode: String;
var
  fErrStr: RawUtf8;
  fErrors: Integer;
  fErrCode: integer;
begin
  Result := '';
  try
    fErrors := 0;
    while (true) do
    begin
      fErrCode := ERR_get_error();
      if fErrCode = 0 then
        Break;
      OpenSSL_error(fErrCode, fErrStr);
      Inc(fErrors);

      if Result <> '' then
        Result := Result + ' / ';

    Result := Result + UTF8ToString(fErrStr);
    end;

    if fErrors = 0 then
      Result := 'NO SSL ERROR, THIS CALL SHOULD HAVE NOT HAPPEN!';
  except
    on e: Exception do
    begin
      Result := 'Error while getting OpenSSL error: ' + e.Message;
    end;
  end;
end;

function GetOpenSSLVersion: String;
begin
  Result := ''; // Initialize to prevent uninitialized variable warning
  if OpenSslIsLoaded then
  begin
    // Get and display the OpenSSL version
    Result := UTF8ToString(OpenSslVersionText);
  end;
end;

function GetOpenSSLAvailable: boolean;
begin
  Result := OpenSslIsLoaded;
end;

function initOpenSsl(out aError: String): boolean;
var
  fLoadedProvider: POSSL_PROVIDER;
  i: integer;
begin
  Result := True;
  aError := '';

  if not OpenSslIsLoaded then
  begin
    {$IFNDEF MSWINDOWS}
    // the libinstaller used to install the files named libcrypto.so and libssl.so which the mormot loader does not
    // find, because it expects libcrypto.so.3 / libcrypto.so.1. Therefore tell mormot to load those files explicitly
    // if they exist.
    if FileExists(ExtractFilePath(ParamStr(0)) + 'libcrypto.so') and FileExists(ExtractFilePath(ParamStr(0)) + 'libssl.so') then
      Result := OpenSslInitialize(ExtractFilePath(ParamStr(0)) + 'libcrypto.so', ExtractFilePath(ParamStr(0)) + 'libssl.so')
    else
    {$ENDIF}
    Result := OpenSslInitialize;

    if Result then
      RegisterOpenSsl
    else
    begin
      aError := 'OpenSslInitialize failed! can not load openssl! ' + _GetEarliestOpenSSLErrorCode;
      exit;
    end;

    if OpenSslVersion >= OPENSSL3_VERNUM then
    begin
      {$IFDEF UNICODE}
        i := OSSL_PROVIDER_set_default_search_path(NIL, PAnsiChar(Pointer(AnsiString(ExtractFilePath(ParamStr(0))))));
      {$ELSE}
        i := OSSL_PROVIDER_set_default_search_path(NIL, PAnsiChar(ExtractFilePath(ParamStr(0))));
      {$ENDIF}

      if i <> 1 then
      begin
        aError := 'OSSL_PROVIDER_set_default_search_path error ' + _GetEarliestOpenSSLErrorCode;
        Result := False;
        exit;
      end;

      fLoadedProvider := OSSL_PROVIDER_load(NIL, 'default');
      if fLoadedProvider = NIL THEN
      begin
        aError := 'default ssl provider not loaded! ' + _GetEarliestOpenSSLErrorCode;
        Result := False;
        exit;
      end;

      fLoadedProvider := OSSL_PROVIDER_load(NIL, 'legacy');
      if fLoadedProvider = NIL THEN
      begin
        aError := 'legacy ssl provider not loaded! ' + _GetEarliestOpenSSLErrorCode;
        Result := False;
        exit;
      end;
    end;
    if OpenSslVersion < OPENSSL1_VERNUM then
    begin
      aError := 'Openssl-Version is too old! Please Update!';
      Result := False;
      exit;
    end;
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

    // Enable client-side SSL session caching for faster reconnects
    // This eliminates full handshake (3-5 RTTs) on repeated connections
    // Cache size: 2048 sessions (enough for 100 sites × 10 slots × 2 for overhead)
    SSL_CTX_set_session_cache_mode(gSSLContextSettings, SSL_SESS_CACHE_CLIENT);
    SSL_CTX_sess_set_cache_size(gSSLContextSettings, 2048);
    SSL_CTX_set_timeout(gSSLContextSettings, 300); // 5 minutes session lifetime

    // Performance optimizations
    SSL_CTX_set_options(gSSLContextSettings,
      SSL_OP_NO_COMPRESSION or                                // Disable TLS compression (CRIME attack + performance)
      SSL_OP_NO_TICKET or                                     // Disable session tickets (use session IDs instead)
      SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION);        // Security: don't resume on renegotiation

    // SSL modes for better performance
    SSL_CTX_set_mode(gSSLContextSettings,
      SSL_MODE_ENABLE_PARTIAL_WRITE or                       // Allow partial writes (don't require full buffer)
      SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER or                 // Avoid unnecessary buffer copying
      SSL_MODE_RELEASE_BUFFERS);                             // Free buffers when idle (memory optimization)

    // Prioritize fast cipher suites (AES-NI hardware acceleration)
    // Modern scene FTP servers support these ciphers
    // AES-GCM provides best performance on CPUs with AES-NI instructions
    SSL_CTX_set_cipher_list(gSSLContextSettings,
      'ECDHE-RSA-AES128-GCM-SHA256:' +                       // TLS 1.2+, ECDHE, AES-GCM (fastest)
      'ECDHE-RSA-AES256-GCM-SHA384:' +                       // TLS 1.2+, stronger variant
      'ECDHE-RSA-AES128-SHA256:' +                           // TLS 1.2+, fallback without GCM
      'ECDHE-RSA-AES256-SHA384:' +                           // TLS 1.2+, fallback
      'AES128-GCM-SHA256:' +                                 // TLS 1.2+, no perfect forward secrecy
      'AES256-GCM-SHA384');                                  // TLS 1.2+, fallback

    // Certificate verification - scene FTP sites typically use self-signed certs
    // Explicit SSL_VERIFY_NONE (no verification) - don't load CA paths
    SSL_CTX_set_verify(gSSLContextSettings, SSL_VERIFY_NONE, nil);
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

function GetLastSSLError(const aSSL: PSSL; const aSslReturnCode: Integer): String;
var
  fErrorCode: Integer;
begin
  // first try to get the error via mormot
  try
    EOpenSsl.Check(aSslReturnCode, '', aSSL);
  except
    on e: Exception do
    begin
      Result := e.Message;
      exit;
    end;
  end;

  // if mormot does not extract the error, use the old way
  fErrorCode := SSL_get_error(aSSL, aSslReturnCode);
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

function GetLastSSLErrorCode(const aSSL: PSSL; const aSslReturnCode: Integer): Integer;
begin
  Result := SSL_get_error(aSSL, aSslReturnCode);
end;

function GetSSLErrorMessageFromErrorCode(const aSSL: PSSL; const aSslErrorCode: Integer): String;
var
  fErrorMessage: RawUTF8;
begin
  SSL_get_error_text(aSslErrorCode, fErrorMessage);
  Result := UTF8ToString(fErrorMessage);
end;

end.
