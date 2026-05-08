unit mycrypto;

interface

uses
  Classes, slmd5;

procedure MyCryptoInit;
procedure MycryptoStart(pp: TslMD5Data);
procedure MycryptoStop;
function DecryptUDP(const s: String): String;
function EncryptUDP(const s: String): String;

{ AES-256-CBC encryption compatible with cbftp's Crypto::encrypt format.
  Output: "Salted__" (8 bytes) + random salt (8 bytes) + AES-256-CBC encrypted data.
  Key derivation: PBKDF2-HMAC-SHA256 with 10000 iterations using aPassword and the random salt.
  Returns empty string on error. }
function CbftpEncryptAES(const aData: RawByteString; const aPassword: RawByteString): RawByteString;

implementation

uses
  SysUtils, delphiblowfish, configunit, debugunit, Math, mystrings,
  mormot.lib.openssl11,
  {$IFDEF FPC}
  DynLibs,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  slcriticalsection2;

const
  section = 'crypto';
  UDP_MIN_PADDING = 8;
  UDP_MAX_PADDING = 32;
  MAX_UDP_PACKET = 16384;
  CBFTP_SALT_LENGTH = 8;
  CBFTP_SALT_HEADER_LENGTH = 16;
  CBFTP_KDF_ITERATIONS = 10000;
  AES256_BLOCK_SIZE = 16;
  AES256_KEY_LENGTH = 32;
  AES256_IV_LENGTH = 16;

type
  TEVP_EncryptInit_ex = function(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; eng: Pointer; key: PByte; iv: PByte): Integer; cdecl;
  TEVP_EncryptFinal_ex = function(ctx: PEVP_CIPHER_CTX; out_: PByte; outl: PInteger): Integer; cdecl;
  TPKCS5_PBKDF2_HMAC = function(pass: PAnsiChar; passlen: Integer; salt: PByte; saltlen: Integer; iter: Integer; digest: PEVP_MD; keylen: Integer; out_: PByte): Integer; cdecl;

{$IFNDEF FPC}
type
  TLibHandle = HMODULE;
const
  NilHandle = HMODULE(0);
{$ENDIF}

var
  KeyData: TBlowfishData;
  _EVP_EncryptInit_ex: TEVP_EncryptInit_ex = nil;
  _EVP_EncryptFinal_ex: TEVP_EncryptFinal_ex = nil;
  _PKCS5_PBKDF2_HMAC: TPKCS5_PBKDF2_HMAC = nil;
  _CryptoLibLoaded: Boolean = False;
  _CryptoLoadLock: TSlCriticalSection2 = nil;

procedure LoadCryptoFunctions;
var
  lib: TLibHandle;
begin
  _CryptoLoadLock.Enter('LoadCryptoFunctions');
  try
    if _CryptoLibLoaded then
      Exit;
    _CryptoLibLoaded := True;
    lib := LoadLibrary('libcrypto.so.3');
    if lib = NilHandle then
      lib := LoadLibrary('libcrypto.so.1.1');
    if lib = NilHandle then
      lib := LoadLibrary('libcrypto.so');
    if lib = NilHandle then
    begin
      Debug(dpError, section, 'CbftpEncryptAES: could not load libcrypto');
      Exit;
    end;
    _EVP_EncryptInit_ex := TEVP_EncryptInit_ex(GetProcAddress(lib, 'EVP_EncryptInit_ex'));
    _EVP_EncryptFinal_ex := TEVP_EncryptFinal_ex(GetProcAddress(lib, 'EVP_EncryptFinal_ex'));
    _PKCS5_PBKDF2_HMAC := TPKCS5_PBKDF2_HMAC(GetProcAddress(lib, 'PKCS5_PBKDF2_HMAC'));
  finally
    _CryptoLoadLock.Leave;
  end;
end;

procedure MyCryptoInit;
begin
  Randomize;
end;

procedure MycryptoStart(pp: TslMD5Data);
const
  IV: array[0..7] of Byte = (0,0,0,0,0,0,0,0);
begin
  BlowfishInit(KeyData, @pp, SizeOf(pp), @IV);
end;

procedure MycryptoStop;
begin
  BlowfishBurn(KeyData);
end;

function DecryptUDP(const s: String): String;
var
  p: Byte;
  l: Integer;
begin
  Result := '';

  l := length(s);
  if l > MAX_UDP_PACKET then
    exit;

  BlowfishReset(KeyData);
  BlowfishDecryptCFB(KeyData, @s[1], @s[1], l);

  p := Ord(s[1]);
  if (p >= UDP_MIN_PADDING) and (p <= UDP_MAX_PADDING) then
    Result := Copy(s, p + 1, l - p - 1);
end;

function EncryptUDP(const s: String): String;
var
  p: Byte;
  block: array[0..MAX_UDP_PACKET-1] of AnsiChar;
begin
  Result := '';

  if Length(s) + UDP_MAX_PADDING > MAX_UDP_PACKET then
    exit;

  p := Byte(RandomRange(UDP_MIN_PADDING, UDP_MAX_PADDING));
  block[0] := AnsiChar(p);
  Move(s[1], block[p], length(s));

  BlowfishReset(KeyData);
  BlowfishEncryptCFB(KeyData, @block, @block, p+1+length(s));
  SetLength(Result, p + Length(s) + 1);
  Move(block[0], Result[1], p + 1 + Length(s));
end;

function CbftpEncryptAES(const aData: RawByteString; const aPassword: RawByteString): RawByteString;
var
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  md: PEVP_MD;
  tmpKeyIV: array[0..AES256_KEY_LENGTH + AES256_IV_LENGTH - 1] of Byte;
  outLen, finalLen: Integer;
  salt: array[0..CBFTP_SALT_LENGTH - 1] of Byte;
begin
  Result := '';
  if (aData = '') or (aPassword = '') then
    Exit;

  LoadCryptoFunctions;
  if not Assigned(_EVP_EncryptInit_ex) or not Assigned(_EVP_EncryptFinal_ex) or not Assigned(_PKCS5_PBKDF2_HMAC) then
  begin
    Debug(dpError, section, 'CbftpEncryptAES: required OpenSSL functions not available');
    Exit;
  end;

  cipher := EVP_aes_256_cbc;
  md := EVP_sha256;
  if (cipher = nil) or (md = nil) then
  begin
    Debug(dpError, section, 'CbftpEncryptAES: OpenSSL AES-256-CBC or SHA256 not available');
    Exit;
  end;

  if RAND_bytes(@salt[0], CBFTP_SALT_LENGTH) <> 1 then
  begin
    Debug(dpError, section, 'CbftpEncryptAES: RAND_bytes failed');
    Exit;
  end;

  _PKCS5_PBKDF2_HMAC(PAnsiChar(aPassword), Length(aPassword),
    @salt[0], CBFTP_SALT_LENGTH, CBFTP_KDF_ITERATIONS, md,
    AES256_KEY_LENGTH + AES256_IV_LENGTH, @tmpKeyIV[0]);

  SetLength(Result, CBFTP_SALT_HEADER_LENGTH + Length(aData) + AES256_BLOCK_SIZE);

  Move(PAnsiChar('Salted__')^, Result[1], 8);
  Move(salt[0], Result[9], CBFTP_SALT_LENGTH);

  ctx := EVP_CIPHER_CTX_new;
  if ctx = nil then
  begin
    Debug(dpError, section, 'CbftpEncryptAES: EVP_CIPHER_CTX_new failed');
    Result := '';
    Exit;
  end;
  try
    if _EVP_EncryptInit_ex(ctx, cipher, nil, @tmpKeyIV[0], @tmpKeyIV[AES256_KEY_LENGTH]) <> 1 then
    begin
      Debug(dpError, section, 'CbftpEncryptAES: EVP_EncryptInit_ex failed');
      Result := '';
      Exit;
    end;

    outLen := 0;
    if EVP_EncryptUpdate(ctx, @Result[CBFTP_SALT_HEADER_LENGTH + 1], @outLen,
      @aData[1], Length(aData)) <> 1 then
    begin
      Debug(dpError, section, 'CbftpEncryptAES: EVP_EncryptUpdate failed');
      Result := '';
      Exit;
    end;

    finalLen := 0;
    if _EVP_EncryptFinal_ex(ctx, @Result[CBFTP_SALT_HEADER_LENGTH + 1 + outLen], @finalLen) <> 1 then
    begin
      Debug(dpError, section, 'CbftpEncryptAES: EVP_EncryptFinal_ex failed');
      Result := '';
      Exit;
    end;

    SetLength(Result, CBFTP_SALT_HEADER_LENGTH + outLen + finalLen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

initialization
  _CryptoLoadLock := TSlCriticalSection2.Create('mycrypto_load');

finalization
  FreeAndNil(_CryptoLoadLock);

end.