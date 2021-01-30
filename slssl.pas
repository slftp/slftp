unit slssl;

interface

type
  PSSL_CTX        = Pointer;
  PSSL            = Pointer;
  PSSL_METHOD     = Pointer;


function OpenSSLVersion: String;
function OpenSSLShortVersion: String;
function slSSL_LastError(): String; overload;
function slSSL_LastError(ssl: PSSL; ec: Integer): String; overload;

const
  OPENSSL_SSL_ERROR_NONE = 0;
  OPENSSL_SSL_ERROR_SSL = 1;
  OPENSSL_SSL_ERROR_SYSCALL = 5;
  OPENSSL_SSL_ERROR_WANT_CONNECT = 7;
  OPENSSL_SSL_ERROR_WANT_READ = 2;
  OPENSSL_SSL_ERROR_WANT_WRITE = 3;
  OPENSSL_SSL_ERROR_WANT_X509_LOOKUP = 4;
  OPENSSL_SSL_ERROR_ZERO_RETURN = 6;

  OPENSSL_SSL_OP_ALL = $00000FFF;
  OPENSSL_SSL_OP_EPHEMERAL_RSA = $00200000;
  OPENSSL_SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER = $00000020;
  OPENSSL_SSL_OP_MICROSOFT_SESS_ID_BUG = $00000001;
  OPENSSL_SSL_OP_MSIE_SSLV2_RSA_PADDING = $00000040;
  OPENSSL_SSL_OP_NETSCAPE_CA_DN_BUG = $20000000;
  OPENSSL_SSL_OP_NETSCAPE_CHALLENGE_BUG = $00000002;
  OPENSSL_SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG = $40000000;
  OPENSSL_SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG = $00000008;
  OPENSSL_SSL_OP_NON_EXPORT_FIRST = $40000000;
  OPENSSL_SSL_OP_NO_SSLv2 = $01000000;
  OPENSSL_SSL_OP_NO_SSLv3 = $02000000;
  OPENSSL_SSL_OP_NO_TLSv1 = $04000000;
  OPENSSL_SSL_OP_PKCS1_CHECK_1 = $00;
  OPENSSL_SSL_OP_PKCS1_CHECK_2 = $00;
  OPENSSL_SSL_OP_SINGLE_DH_USE = $00100000;
  OPENSSL_SSL_OP_SSLEAY_080_CLIENT_DH_BUG = $00000080;
  OPENSSL_SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG = $00000010;
  OPENSSL_SSL_OP_TLS_BLOCK_PADDING_BUG = $00000200;
  OPENSSL_SSL_OP_TLS_D5_BUG = $00000100;
  OPENSSL_SSL_OP_TLS_ROLLBACK_BUG = $00800000;

  OPENSSL_SSL_MODE_ENABLE_PARTIAL_WRITE = $00000001;
  OPENSSL_SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER = $00000002;
  OPENSSL_SSL_MODE_AUTO_RETRY = $00000004;

  OPENSSL_SSL_SESS_CACHE_CLIENT = $0001;
  OPENSSL_SSL_SESS_CACHE_SERVER = $0002;
  OPENSSL_SSL_SESS_CACHE_BOTH = OPENSSL_SSL_SESS_CACHE_CLIENT or OPENSSL_SSL_SESS_CACHE_SERVER;
  OPENSSL_SSL_SESS_CACHE_NO_AUTO_CLEAR = $0080;
  OPENSSL_SSL_SESS_CACHE_NO_INTERNAL_LOOKUP = $0100;
  OPENSSL_SSL_SESS_CACHE_OFF = $0000;

  OPENSSL_X509_FILETYPE_PEM = 1;
  OPENSSL_SSL_FILETYPE_PEM = OPENSSL_X509_FILETYPE_PEM;

  OPENSSL_V_ASN1_APPLICATION = $40;
  OPENSSL_V_ASN1_APP_CHOOSE = -2;
  OPENSSL_V_ASN1_BIT_STRING = 3;
  OPENSSL_V_ASN1_BMPSTRING = 30;
  OPENSSL_V_ASN1_BOOLEAN = 1;
  OPENSSL_V_ASN1_CONSTRUCTED = $20;
  OPENSSL_V_ASN1_CONTEXT_SPECIFIC = $80;
  OPENSSL_V_ASN1_ENUMERATED = 10;
  OPENSSL_V_ASN1_EOC = 0;
  OPENSSL_V_ASN1_EXTERNAL = 8;
  OPENSSL_V_ASN1_GENERALIZEDTIME = 24;
  OPENSSL_V_ASN1_GENERALSTRING = 27;
  OPENSSL_V_ASN1_GRAPHICSTRING = 25;
  OPENSSL_V_ASN1_IA5STRING = 22;
  OPENSSL_V_ASN1_INTEGER = 2;
  OPENSSL_V_ASN1_ISO64STRING = 26;
  OPENSSL_V_ASN1_NEG_ENUMERATED = 10+$100;
  OPENSSL_V_ASN1_NEG_INTEGER = 2+$100;
  OPENSSL_V_ASN1_NULL = 5;
  OPENSSL_V_ASN1_NUMERICSTRING = 18;
  OPENSSL_V_ASN1_OBJECT = 6;
  OPENSSL_V_ASN1_OBJECT_DESCRIPTOR = 7;
  OPENSSL_V_ASN1_OCTET_STRING = 4;
  OPENSSL_V_ASN1_PRIMATIVE_TAG = $1f;
  OPENSSL_V_ASN1_PRIMITIVE_TAG = $1f;
  OPENSSL_V_ASN1_PRINTABLESTRING = 19;
  OPENSSL_V_ASN1_PRIVATE = $c0;
  OPENSSL_V_ASN1_REAL = 9;
  OPENSSL_V_ASN1_SEQUENCE = 16;
  OPENSSL_V_ASN1_SET = 17;
  OPENSSL_V_ASN1_T61STRING = 20;
  OPENSSL_V_ASN1_TELETEXSTRING = 20;
  OPENSSL_V_ASN1_UNDEF = -1;
  OPENSSL_V_ASN1_UNIVERSAL = $00;
  OPENSSL_V_ASN1_UNIVERSALSTRING = 28;
  OPENSSL_V_ASN1_UTCTIME = 23;
  OPENSSL_V_ASN1_UTF8STRING = 12;
  OPENSSL_V_ASN1_VIDEOTEXSTRING = 21;
  OPENSSL_V_ASN1_VISIBLESTRING = 26;

  OPENSSL_CRYPTO_LOCK     = $01;
  OPENSSL_CRYPTO_UNLOCK   = $02;
  OPENSSL_CRYPTO_READ     = $04;
  OPENSSL_CRYPTO_WRITE    = $08;

  slssl_default_cipher_list = 'ALL:!EXP';

var slssl_inited: Boolean = False;
    slssl_error: String;
    slssl_ctx_tls_client: PSSL_CTX = nil;


  slRAND_Screen : procedure cdecl = nil;
  slOpenSSL_add_all_ciphers : procedure cdecl = nil;
  slSSLeay_version: function(vertype: Integer): PAnsiChar cdecl = nil;

  slSSL_CTX_set_cipher_list : function(arg0: PSSL_CTX; str: PAnsiChar):Integer cdecl = nil;
  slSSL_CTX_new : function(meth: PSSL_METHOD):PSSL_CTX cdecl = nil;
  slSSL_CTX_free : procedure(arg0: PSSL_CTX) cdecl = nil;
  slSSL_set_fd : function(s: PSSL; fd: Integer):Integer cdecl = nil;

  slSSL_new : function(ctx: PSSL_CTX):PSSL cdecl = nil;
  slSSL_free : procedure(ssl: PSSL) cdecl = nil;
  slSSL_accept : function(ssl: PSSL):Integer cdecl = nil;
  slSSL_connect : function(ssl: PSSL):Integer cdecl = nil;
  slSSL_read : function(ssl: PSSL; buf: PAnsiChar; num: Integer):Integer cdecl = nil;
  slSSL_peek : function(ssl: PSSL; buf: PAnsiChar; num: Integer):Integer cdecl = nil;
  slSSL_write : function(ssl: PSSL; const buf: PAnsiChar; num: Integer):Integer cdecl = nil;
  slSSL_shutdown : function(s: PSSL):Integer cdecl = nil;
  slSSL_get_error : function(s: PSSL; ret_code: Integer):Integer cdecl = nil;

  slTLS_method : function:PSSL_METHOD cdecl = nil;
  slTLS_server_method : function:PSSL_METHOD cdecl = nil;
  slTLS_client_method : function:PSSL_METHOD cdecl = nil;


  slSSL_library_init: procedure cdecl = nil;
  slSSL_crypto_init: procedure cdecl = nil;
  slENGINE_load_builtin_engines: procedure cdecl = nil;
  slERR_get_error: function: Cardinal cdecl = nil;
  slERR_error_string: function(e: Cardinal; buf: PAnsiChar): PAnsiChar cdecl = nil;

  slSSL_CTX_set_default_verify_paths: function(ctx: PSSL_CTX): Integer; cdecl = nil;
  slSSL_CTX_set_options: function(ctx: PSSL_CTX; op: Longint):Longint cdecl = nil;
  slSSL_CTX_set_mode: function(ctx: PSSL_CTX; mode: Longint): Longint cdecl = nil;
  slSSL_CTX_set_session_cache_mode: function(ctx: PSSL_CTX; mode: LongInt): Longint cdecl = nil;


  // pem generalashoz fuggvenyek
  slBIO_new_file: function(filename, mode: PAnsiChar): Pointer cdecl = nil;
  slRSA_generate_key: function(num: Integer; e: Cardinal; callback: Pointer; cb_arg: Pointer): Pointer cdecl = nil;
  slX509_REQ_new: function: Pointer cdecl = nil;
  slRSA_free: procedure(rsa: Pointer) cdecl = nil;
  slBIO_free: function(bio: Pointer): Integer cdecl = nil;
  slX509_NAME_new: function: Pointer cdecl = nil;
  slX509_REQ_free: procedure(req: Pointer) cdecl = nil;
  slX509_NAME_ENTRY_create_by_txt: function(ne: Pointer; field: PAnsiChar; ntype: Integer; name: PAnsiChar; namelen: Integer): Pointer cdecl = nil;
  slX509_NAME_free: procedure(n: Pointer) cdecl = nil;
  slX509_NAME_add_entry: function(xn, xne: Pointer; nloc, nset: Integer): Integer cdecl = nil;
  slX509_REQ_set_subject_name: function(xr, xn: Pointer): Integer cdecl = nil;
  slEVP_PKEY_new: function: Pointer cdecl = nil;
  slEVP_PKEY_free: procedure(key: Pointer) cdecl = nil;
  slEVP_PKEY_set1_RSA: function(pkey, key: Pointer): Integer cdecl = nil;
  slX509_REQ_set_pubkey: function(x, pkey: Pointer):Integer cdecl = nil;
  slX509_REQ_sign: function(x, pkey, md: Pointer): Integer cdecl = nil;
  slX509_REQ_to_X509: function(r: Pointer; day: Integer; pkey: Pointer): Pointer cdecl = nil;
  slX509_free: procedure(a: Pointer) cdecl = nil;
  slPEM_write_bio_RSAPrivateKey: function(bp, x, enc: Pointer; kstr: PAnsiChar; klen: Integer; cb, u: Pointer): Integer cdecl = nil;
  slPEM_write_bio_X509: function(bp, x: Pointer): integer cdecl = nil;
  slEVP_sha256: function: Pointer cdecl = nil;

  slSSL_CTX_use_certificate_chain_file : function(ctx: PSSL_CTX; const _file: PAnsiChar):Integer cdecl = nil;
  slSSL_CTX_use_PrivateKey_file: function(ctx: PSSL_CTX; const _file: PAnsiChar; _type: Integer):Integer cdecl = nil;
  slSSL_CTX_check_private_key: function(ctx: PSSL_CTX):Integer cdecl = nil;



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


const
  {$IFDEF MSWINDOWS}
  slSsl_libssl_name         = 'libssl-1_1-x64.dll';  {Do not localize}
  slSsl_libcrypto_name      = 'libcrypto-1_1-x64.dll';  {Do not localize}
  {$ELSE}
  slSsl_libssl_name         = 'libssl.so'; {Do not localize}
  slSsl_libcrypto_name      = 'libcrypto.so'; {Do not localize}
  {$ENDIF}
var
  h_libssl    : {$IFDEF MSWINDOWS}Int64{$ELSE}Integer{$ENDIF} = 0;
  h_libcrypto : {$IFDEF MSWINDOWS}Int64{$ELSE}Integer{$ENDIF} = 0;


const
  OPENSSL_SSLEAY_BUILT_ON = 3;
  OPENSSL_SSLEAY_CFLAGS = 2;
  OPENSSL_SSLEAY_PLATFORM = 4;
  OPENSSL_SSLEAY_VERSION = 0;


  fn_SSL_shutdown = 'SSL_shutdown';  {Do not localize}
  fn_RAND_screen = 'RAND_screen';  {Do not localize}

  fn_SSL_CTX_set_cipher_list = 'SSL_CTX_set_cipher_list';  {Do not localize}
  fn_SSL_CTX_new = 'SSL_CTX_new';  {Do not localize}
  fn_SSL_CTX_free = 'SSL_CTX_free';  {Do not localize}
  fn_SSL_set_fd = 'SSL_set_fd';  {Do not localize}

  fn_SSL_new = 'SSL_new';  {Do not localize}
  fn_SSL_free = 'SSL_free';  {Do not localize}
  fn_SSL_accept = 'SSL_accept';  {Do not localize}
  fn_SSL_connect = 'SSL_connect';  {Do not localize}
  fn_SSL_read = 'SSL_read';  {Do not localize}
  fn_SSL_peek = 'SSL_peek';  {Do not localize}
  fn_SSL_write = 'SSL_write';  {Do not localize}
  fn_SSL_get_error = 'SSL_get_error';  {Do not localize}

  fn_OpenSSL_version = 'OpenSSL_version';  {Do not localize}

  fn_TLS_method = 'TLS_method';   {Do not localize}
  fn_TLS_server_method = 'TLS_server_method';  {Do not localize}
  fn_TLS_client_method = 'TLS_client_method';  {Do not localize}



  fn_SSL_CTX_set_default_verify_paths = 'SSL_CTX_set_default_verify_paths';  {Do not localize}
  fn_SSL_CTX_set_options = 'SSL_CTX_set_options';  {Do not localize}
  fn_SSL_CTX_set_mode = 'SSL_CTX_set_mode';  {Do not localize}
  fn_SSL_CTX_set_session_cache_mode = 'SSL_CTX_set_session_cache_mode';

  fn_OPENSSL_init_ssl = 'OPENSSL_init_ssl';
  fn_OPENSSL_init_crypto = 'OPENSSL_init_crypto';
  fn_ENGINE_load_builtin_engines = 'ENGINE_load_builtin_engines';
  fn_ERR_error_string = 'ERR_error_string';
  fn_ERR_get_error = 'ERR_get_error';


  fn_BIO_new_file = 'BIO_new_file';  {Do not localize}
  fn_RSA_generate_key = 'RSA_generate_key';  {Do not localize}
  fn_BIO_free = 'BIO_free';
  fn_X509_REQ_new = 'X509_REQ_new';
  fn_RSA_free = 'RSA_free';
  fn_X509_NAME_new = 'X509_NAME_new';
  fn_X509_REQ_free = 'X509_REQ_free';
  fn_X509_NAME_ENTRY_create_by_txt = 'X509_NAME_ENTRY_create_by_txt';
  fn_X509_NAME_free = 'X509_NAME_free';
  fn_X509_NAME_add_entry = 'X509_NAME_add_entry';
  fn_X509_REQ_set_subject_name = 'X509_REQ_set_subject_name';
  fn_EVP_PKEY_new = 'EVP_PKEY_new';
  fn_EVP_PKEY_set1_RSA = 'EVP_PKEY_set1_RSA';
  fn_EVP_PKEY_free = 'EVP_PKEY_free';
  fn_X509_REQ_set_pubkey = 'X509_REQ_set_pubkey';
  fn_X509_REQ_sign = 'X509_REQ_sign';
  fn_X509_REQ_to_X509 = 'X509_REQ_to_X509';
  fn_X509_free = 'X509_free';
  fn_PEM_write_bio_X509 = 'PEM_write_bio_X509';
  fn_PEM_write_bio_RSAPrivateKey = 'PEM_write_bio_RSAPrivateKey';
  fn_EVP_sha256 = 'EVP_sha256';

  fn_SSL_CTX_use_certificate_chain_file = 'SSL_CTX_use_certificate_chain_file';  {Do not localize}
  fn_SSL_CTX_use_PrivateKey_file = 'SSL_CTX_use_PrivateKey_file';
  fn_SSL_CTX_check_private_key = 'SSL_CTX_check_private_key';


function OpenSSLVersion: String;
begin
  Result:= Format('%s %s %s %s',[
    slSSLeay_version(OPENSSL_SSLEAY_VERSION),
    slSSLeay_version(OPENSSL_SSLEAY_CFLAGS),
    slSSLeay_version(OPENSSL_SSLEAY_BUILT_ON),
    slSSLeay_version(OPENSSL_SSLEAY_PLATFORM)]);
end;

function OpenSSLShortVersion: String;
begin
  Result:= Copy(slSSLeay_version(OPENSSL_SSLEAY_VERSION), 9, 6);
end;


function slSsl_LoadProc(handle: {$IFDEF MSWINDOWS}Int64{$ELSE}Integer{$ENDIF}; const fnName: String; var fn: Pointer): Boolean;
var fceName: String;
begin
  Result:= False;
  FceName := fnName+#0;
{$IFDEF FPC}
  fn := GetProcAddress(handle, fceName);
{$ELSE}
  {$IFDEF UNICODE}
    fn := GetProcAddress(handle, PAnsiChar(AnsiString(fceName)));
  {$ELSE}
    fn := GetProcAddress(handle, @fceName[1]);
  {$ENDIF}
{$ENDIF}
  if fn = nil then
  begin
    slssl_error:= 'Cannot load '+fnName
  end
  else
    Result:= True;
end;


procedure slSslInit;
begin
  if slssl_inited then exit;

{$IFDEF FPC}
  // Workaround that is requered under Linux
  if h_libcrypto = 0 then h_libcrypto := LoadLibrary(ExtractFilePath(ParamStr(0))+slSsl_libcrypto_name);
  if h_libcrypto = 0 then h_libcrypto := LoadLibrary(slSsl_libcrypto_name);
  if h_libcrypto = 0 then
  begin
    slssl_error:= 'Couldnt load libcrypto';
    exit;
  end;

  If h_libssl = 0 Then h_libssl := LoadLibrary(ExtractFilePath(ParamStr(0))+slSsl_libssl_name);
  If h_libssl = 0 Then h_libssl := LoadLibrary(slSsl_libssl_name);
  if h_libssl = 0 then
  begin
    slssl_error:= 'Couldnt load libssl';
    exit;
  end;
{$ELSE}
  {$IFDEF LINUX}
  // Workaround that is requered under Linux
  if h_libcrypto = 0 then h_libcrypto := HMODULE(dlopen(PAnsiChar(ExtractFilePath(ParamStr(0))+slSsl_libcrypto_name), RTLD_GLOBAL));
  if h_libcrypto = 0 then h_libcrypto := HMODULE(dlopen(slSsl_libcrypto_name, RTLD_GLOBAL));
  if h_libcrypto = 0 then
  begin
    slssl_error:= 'Couldnt load libcrypto';
    exit;
  end;

  If h_libssl = 0 Then h_libssl := HMODULE(dlopen(PAnsiChar(ExtractFilePath(ParamStr(0))+slSsl_libssl_name), RTLD_GLOBAL));
  If h_libssl = 0 Then h_libssl := HMODULE(dlopen(slSsl_libssl_name, RTLD_GLOBAL));
  if h_libssl = 0 then
  begin
    slssl_error:= 'Couldnt load libssl';
    exit;
  end;
  {$ELSE}
  if h_libcrypto = 0 then
    h_libcrypto := LoadLibrary(slSsl_libcrypto_name);
  if h_libcrypto = 0 then
  begin
    slssl_error:= 'Couldnt load libcrypto: '+SysErrorMessage(GetLastError) ;
    exit;
  end;

  If h_libssl = 0 Then h_libssl := LoadLibrary(slSsl_libssl_name);
  if h_libssl = 0 then
  begin
    slssl_error:= 'Couldnt load libssl: '+SysErrorMessage(GetLastError) ;
    exit;
  end;
  {$ENDIF}
{$ENDIF}

  if not slSsl_LoadProc(h_libssl, fn_SSL_shutdown, @slSSL_shutdown) then exit;
  slSsl_LoadProc(h_libcrypto, fn_RAND_screen, @slRAND_screen);
  if not slSsl_LoadProc(h_libcrypto, fn_OpenSSL_version, @slSSLeay_version) then exit;

  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_set_cipher_list, @slSSL_CTX_set_cipher_list) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_new, @slSSL_CTX_new) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_free, @slSSL_CTX_free) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_set_fd, @slSSL_set_fd) then exit;

  if not slSsl_LoadProc(h_libssl, fn_SSL_new, @slSSL_new) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_free, @slSSL_free) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_accept, @slSSL_accept) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_connect, @slSSL_connect) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_read, @slSSL_read) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_peek, @slSSL_peek) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_write, @slSSL_write) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_get_error, @slSSL_get_error) then exit;

  if not slSsl_LoadProc(h_libssl, fn_TLS_method, @slTLS_method) then exit;
  if not slSsl_LoadProc(h_libssl, fn_TLS_server_method, @slTLS_server_method) then exit;
  if not slSsl_LoadProc(h_libssl, fn_TLS_client_method, @slTLS_client_method) then exit;

  slSsl_LoadProc(h_libcrypto, fn_ENGINE_load_builtin_engines, @slENGINE_load_builtin_engines);
  if not slSsl_LoadProc(h_libssl, fn_OPENSSL_init_ssl, @slSSL_library_init) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_OPENSSL_init_crypto, @slSSL_crypto_init) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_ERR_error_string, @slERR_error_string) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_ERR_get_error, @slERR_get_error) then exit;

  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_set_default_verify_paths,@slSSL_CTX_set_default_verify_paths) then exit;
  slSsl_LoadProc(h_libssl, fn_SSL_CTX_set_mode,@slSSL_CTX_set_mode);
  slSsl_LoadProc(h_libssl, fn_SSL_CTX_set_session_cache_mode,@slSSL_CTX_set_session_cache_mode);
  slSsl_LoadProc(h_libssl, fn_SSL_CTX_set_options,@slSSL_CTX_set_options);


  // cert generalo szarcsimbokok
  if not slSsl_LoadProc(h_libcrypto, fn_BIO_new_file, @slBIO_new_file) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_RSA_generate_key, @slRSA_generate_key) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_BIO_free, @slBIO_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_new, @slX509_REQ_new) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_RSA_free, @slRSA_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_NAME_new, @slX509_NAME_new) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_free, @slX509_REQ_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_NAME_ENTRY_create_by_txt, @slX509_NAME_ENTRY_create_by_txt) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_NAME_free, @slX509_NAME_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_NAME_add_entry, @slX509_NAME_add_entry) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_set_subject_name, @slX509_REQ_set_subject_name) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_EVP_PKEY_new, @slEVP_PKEY_new) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_EVP_PKEY_set1_RSA, @slEVP_PKEY_set1_RSA) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_EVP_PKEY_free, @slEVP_PKEY_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_set_pubkey, @slX509_REQ_set_pubkey) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_sign, @slX509_REQ_sign) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_REQ_to_X509, @slX509_REQ_to_X509) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_X509_free, @slX509_free) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_PEM_write_bio_X509, @slPEM_write_bio_X509) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_PEM_write_bio_RSAPrivateKey, @slPEM_write_bio_RSAPrivateKey) then exit;
  if not slSsl_LoadProc(h_libcrypto, fn_EVP_sha256, @slEVP_sha256) then exit;

  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_use_certificate_chain_file, @slSSL_CTX_use_certificate_chain_file) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_use_PrivateKey_file, @slSSL_CTX_use_PrivateKey_file) then exit;
  if not slSsl_LoadProc(h_libssl, fn_SSL_CTX_check_private_key, @slSSL_CTX_check_private_key) then exit;

  slSSL_library_init();
  slSSL_crypto_init();

  if @slENGINE_load_builtin_engines <> nil then
    slENGINE_load_builtin_engines();
    {
      * might miss a ENGINE_cleanup() call on uninit
      If no engine API functions are called at all in an application, then there are no inherent memory leaks to worry about from the engine functionality,
      however if any ENGINEs are loaded, even if they are never registered or used, it is necessary to use the ENGINE_cleanup() function to correspondingly
      cleanup before program exit, if the caller wishes to avoid memory leaks.
    }

  if @slRAND_screen <> nil then
    slRAND_screen();



//----------------- tls start
  slSSL_CTX_tls_client:= slSSL_CTX_new(slTLS_client_method());
  if (slSSL_CTX_tls_client = nil) then
  begin
    slssl_error:= slssl_LastError();
    exit;
  end;

  slSSL_CTX_set_default_verify_paths(slSSL_CTX_tls_client);
  if @slSSL_CTX_set_options <> nil then
    slSSL_CTX_set_options(slSSL_CTX_tls_client,OPENSSL_SSL_OP_ALL);
  if @slSSL_CTX_set_mode <> nil then
    slSSL_CTX_set_mode(slSSL_CTX_tls_client,OPENSSL_SSL_MODE_AUTO_RETRY);
  if @slSSL_CTX_set_session_cache_mode <> nil then
    slSSL_CTX_set_session_cache_mode(slSSL_CTX_tls_client,OPENSSL_SSL_SESS_CACHE_OFF);

  slSSL_CTX_set_cipher_list( slSSL_CTX_tls_client, slssl_default_cipher_list );
//----------------- tls end

  slssl_error:= '';
  slssl_inited:= True;
end;

procedure slSslUnInit;
begin
  if not slssl_inited then exit;

  if slSSL_CTX_tls_client <> nil then
  begin
    slSSL_CTX_free(slSSL_CTX_tls_client);
    slSSL_CTX_tls_client:= nil;
  end;

  if h_libssl > 0 then FreeLibrary(h_libssl);
  h_libssl := 0;

  if h_libcrypto > 0 then FreeLibrary(h_libcrypto);
  h_libcrypto := 0;


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
    i := slERR_get_error();
    if i = 0 then
      Break;
    slERR_error_string(i, @s[1]);
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
  ec := slSSL_get_error(ssl, ec);
  case ec of
    OPENSSL_SSL_ERROR_NONE: Result := 'no error';
    OPENSSL_SSL_ERROR_SSL: Result := 'ssl error';
    OPENSSL_SSL_ERROR_SYSCALL: Result := 'syscall error';
    OPENSSL_SSL_ERROR_WANT_CONNECT: Result := 'want connect';
    OPENSSL_SSL_ERROR_WANT_READ: Result := 'want read';
    OPENSSL_SSL_ERROR_WANT_WRITE: Result := 'want write';
    OPENSSL_SSL_ERROR_WANT_X509_LOOKUP: Result := 'x509 lookup wanted';
    OPENSSL_SSL_ERROR_ZERO_RETURN: Result := 'zero return';
  else
    Result := 'unknown error';
  end;
end;

initialization
  slSslInit;
finalization
  slSslUninit;
end.
