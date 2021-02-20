unit slssl;

interface

uses IdOpenSSLHeaders_ossl_typ, IdOpenSSLHeaders_err, IdOpenSSLHeaders_crypto, IdOpenSSLHeaders_ssl, IdOpenSSLContext, IdOpenSSLLoader;

function OpenSSLVersion: String;
function OpenSSLShortVersion: String;
function slSSL_LastError(): String; overload;
function slSSL_LastError(ssl: PSSL; ec: Integer): String; overload;

const
  slssl_default_cipher_list = 'ALL:!EXP';

var slssl_inited: Boolean = False;
    slssl_error: String;
    slssl_ctx_tls_client: PSSL_CTX = nil;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  dynlibs,
  {$IFNDEF MSWINDOWS}
    pthreads,
  {$ENDIF}
{$ELSE}
  {$IFNDEF MSWINDOWS}
    Libc,
  {$ENDIF}
{$ENDIF}
  SysUtils
;

{$I slftp.inc}

function OpenSSLVersion: String;
begin
  Result:= Format('%s %s %s %s',[
    OpenSSL_version(OPENSSL_VERSION_CONST),
    OpenSSL_version(OPENSSL_CFLAGS),
    OpenSSL_version(OPENSSL_BUILT_ON),
    OpenSSL_version(OPENSSL_PLATFORM)]);
end;

function OpenSSLShortVersion: String;
begin
  Result:= Copy(OpenSSL_version(OPENSSL_VERSION_CONST), 9, 6);
end;


procedure slSslInit;
var
  fSslLoader: IOpenSSLLoader;
  fOpenSSLVersion: String;
begin
  if slssl_inited then exit;

  fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
  fSslLoader.OpenSSLPath := '.';

  try
    if not fSslLoader.Load then
    begin
      slssl_error := Format('Failed to load OpenSSL from slftp dir:%s %s', [sLineBreak, fSslLoader.FailedToLoad.CommaText]);
      exit;
    end;
  except
    on e: Exception do
    begin
      slssl_error := Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]);
      exit;
    end;
  end;

  fOpenSSLVersion := Copy(OpenSSL_version(OPENSSL_VERSION_CONST), 9, 5);
  if (fOpenSSLVersion <> lib_OpenSSL) then
  begin
    slssl_error := Format('OpenSSL version %s is not supported! OpenSSL %s needed.', [fOpenSSLVersion, lib_OpenSSL]);
    exit;
  end;


  slSSL_CTX_tls_client:= SSL_CTX_new(TLS_client_method());
  if (slSSL_CTX_tls_client = nil) then
  begin
    slssl_error:= slssl_LastError();
    exit;
  end;

  SSL_CTX_set_default_verify_paths(slSSL_CTX_tls_client);
  SSL_CTX_set_options(slSSL_CTX_tls_client,SSL_OP_ALL);
  SSL_CTX_set_mode(slSSL_CTX_tls_client, SSL_MODE_AUTO_RETRY);
  SSL_CTX_set_mode(slSSL_CTX_tls_client, SSL_SESS_CACHE_OFF);
  SSL_CTX_set_cipher_list(slSSL_CTX_tls_client, slssl_default_cipher_list);

  slssl_error:= '';
  slssl_inited:= True;
end;

procedure slSslUnInit;
begin
  if not slssl_inited then exit;

  if slSSL_CTX_tls_client <> nil then
  begin
    SSL_CTX_free(slSSL_CTX_tls_client);
    slSSL_CTX_tls_client:= nil;
  end;

  IdOpenSSLLoader.GetOpenSSLLoader().Unload();

  slssl_inited:= False;
end;

function slSSL_LastError(): String;
var
  s: String;
  db: Integer;
  i: Cardinal;
begin
  Result := '';
  SetLength(s, 255);
  db := 0;
  while (true) do
  begin
    i := ERR_get_error();
    if i = 0 then
      Break;
    ERR_error_string(i, @s[1]);
    inc(db);

    if Result <> '' then
      Result := Result + ' / ';

    Result := Result + s;
  end;

  if db = 0 then
    Result := 'NO SSL ERROR, THIS CALL SHOULD HAVE NOT HAPPEN!';
end;
function slSSL_LastError(ssl: PSSL; ec: Integer): String;
begin
  ec := SSL_get_error(ssl, ec);
  case ec of
    SSL_ERROR_NONE: Result := 'no error';
    SSL_ERROR_SSL: Result := 'ssl error';
    SSL_ERROR_SYSCALL: Result := 'syscall error';
    SSL_ERROR_WANT_CONNECT: Result := 'want connect';
    SSL_ERROR_WANT_READ: Result := 'want read';
    SSL_ERROR_WANT_WRITE: Result := 'want write';
    SSL_ERROR_WANT_X509_LOOKUP: Result := 'x509 lookup wanted';
    SSL_ERROR_ZERO_RETURN: Result := 'zero return';
  else
    Result := 'unknown error';
  end;
end;

initialization
  slSslInit;
finalization
  slSslUninit;
end.
