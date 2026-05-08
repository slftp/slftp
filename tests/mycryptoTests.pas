unit mycryptoTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}
  slftpUnitTestsSetupIndyOpenSSL;

type
  TTestCbftpEncryptAES = class(TTestIndyOpenSSL)
  published
    procedure TestEmptyDataReturnsEmpty;
    procedure TestEmptyPasswordReturnsEmpty;
    procedure TestOutputStartsWithSaltedHeader;
    procedure TestOutputLengthIsMultipleOf16;
    procedure TestSaltIsRandomized;
    procedure TestRoundtripShortMessage;
    procedure TestRoundtripExactBlockBoundary;
    procedure TestRoundtripRaceCommandFormat;
  end;

implementation

uses
  SysUtils,
  {$IFDEF FPC}
  DynLibs,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  mormot.lib.openssl11, mycrypto;

{ --- Decrypt helper (mirrors cbftp Crypto::decrypt) ----------------------- }

type
  TTestDecryptInit = function(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER;
    eng: Pointer; key: PByte; iv: PByte): Integer; cdecl;
  TTestDecryptFinal = function(ctx: PEVP_CIPHER_CTX; out_: PByte;
    outl: PInteger): Integer; cdecl;
  TTestPBKDF2 = function(pass: PAnsiChar; passlen: Integer; salt: PByte;
    saltlen: Integer; iter: Integer; digest: PEVP_MD; keylen: Integer;
    out_: PByte): Integer; cdecl;

const
  CBFTP_HEADER_LEN  = 16;
  CBFTP_SALT_OFFSET = 8;
  CBFTP_SALT_LEN    = 8;
  CBFTP_KEY_LEN     = 32;
  CBFTP_IV_LEN      = 16;
  CBFTP_BLOCK_SIZE  = 16;
  CBFTP_ITERATIONS  = 10000;

function DecryptForTest(const aCiphertext: RawByteString;
  const aPassword: RawByteString): RawByteString;
var
  lib: TLibHandle;
  decInit: TTestDecryptInit;
  decFinal: TTestDecryptFinal;
  pbkdf2: TTestPBKDF2;
  ctx: PEVP_CIPHER_CTX;
  cipher: PEVP_CIPHER;
  md: PEVP_MD;
  tmpKeyIV: array[0..CBFTP_KEY_LEN + CBFTP_IV_LEN - 1] of Byte;
  writelen, finalwritelen: Integer;
begin
  Result := '';
  if Length(aCiphertext) < CBFTP_HEADER_LEN + CBFTP_BLOCK_SIZE then
    Exit;
  if Copy(aCiphertext, 1, 8) <> 'Salted__' then
    Exit;

  lib := LoadLibrary('libcrypto.so.3');
  if lib = NilHandle then
    lib := LoadLibrary('libcrypto.so.1.1');
  if lib = NilHandle then
    lib := LoadLibrary('libcrypto.so');
  if lib = NilHandle then
    Exit;

  decInit  := TTestDecryptInit(GetProcAddress(lib, 'EVP_DecryptInit_ex'));
  decFinal := TTestDecryptFinal(GetProcAddress(lib, 'EVP_DecryptFinal_ex'));
  pbkdf2   := TTestPBKDF2(GetProcAddress(lib, 'PKCS5_PBKDF2_HMAC'));
  if not Assigned(decInit) or not Assigned(decFinal) or not Assigned(pbkdf2) then
    Exit;

  cipher := EVP_aes_256_cbc;
  md     := EVP_sha256;
  if (cipher = nil) or (md = nil) then
    Exit;

  pbkdf2(PAnsiChar(aPassword), Length(aPassword),
    @aCiphertext[CBFTP_SALT_OFFSET + 1], CBFTP_SALT_LEN,
    CBFTP_ITERATIONS, md,
    CBFTP_KEY_LEN + CBFTP_IV_LEN, @tmpKeyIV[0]);

  SetLength(Result, Length(aCiphertext) - CBFTP_HEADER_LEN + CBFTP_BLOCK_SIZE);
  ctx := EVP_CIPHER_CTX_new;
  if ctx = nil then
  begin
    Result := '';
    Exit;
  end;
  try
    if decInit(ctx, cipher, nil, @tmpKeyIV[0], @tmpKeyIV[CBFTP_KEY_LEN]) <> 1 then
    begin
      Result := '';
      Exit;
    end;
    writelen := 0;
    if EVP_DecryptUpdate(ctx, @Result[1], @writelen,
      @aCiphertext[CBFTP_HEADER_LEN + 1],
      Length(aCiphertext) - CBFTP_HEADER_LEN) <> 1 then
    begin
      Result := '';
      Exit;
    end;
    finalwritelen := 0;
    if decFinal(ctx, @Result[1 + writelen], @finalwritelen) <> 1 then
    begin
      Result := '';
      Exit;
    end;
    SetLength(Result, writelen + finalwritelen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

{ --- Tests ---------------------------------------------------------------- }

procedure TTestCbftpEncryptAES.TestEmptyDataReturnsEmpty;
begin
  CheckEquals('', CbftpEncryptAES('', 'secret'),
    'Empty data must return empty string');
end;

procedure TTestCbftpEncryptAES.TestEmptyPasswordReturnsEmpty;
begin
  CheckEquals('', CbftpEncryptAES('hello', ''),
    'Empty password must return empty string');
end;

procedure TTestCbftpEncryptAES.TestOutputStartsWithSaltedHeader;
var
  fResult: RawByteString;
begin
  fResult := CbftpEncryptAES('test payload', 'mypass');
  CheckTrue(fResult <> '', 'Encrypt must not return empty for valid inputs');
  CheckEquals('Salted__', Copy(fResult, 1, 8),
    'Output must start with Salted__ header');
end;

procedure TTestCbftpEncryptAES.TestOutputLengthIsMultipleOf16;
var
  fResult: RawByteString;
begin
  // Header (16) + AES-CBC ciphertext (always a multiple of 16).
  // PKCS7 adds at least 1 byte so ciphertext >= 16; total >= 32.
  fResult := CbftpEncryptAES('short', 'pass');
  CheckTrue(fResult <> '', 'Encrypt must not return empty');
  CheckTrue(Length(fResult) >= 32,
    'Output must be at least 32 bytes (header + one AES block)');
  CheckEquals(0, Length(fResult) mod 16,
    'Total output length must be a multiple of 16');
end;

procedure TTestCbftpEncryptAES.TestSaltIsRandomized;
var
  fFirst, fSecond: RawByteString;
begin
  fFirst  := CbftpEncryptAES('identical plaintext', 'samepass');
  fSecond := CbftpEncryptAES('identical plaintext', 'samepass');
  CheckTrue(fFirst  <> '', 'First encrypt must not return empty');
  CheckTrue(fSecond <> '', 'Second encrypt must not return empty');
  CheckTrue(fFirst <> fSecond,
    'Two encryptions of the same plaintext must produce different ciphertext (random salt)');
end;

procedure TTestCbftpEncryptAES.TestRoundtripShortMessage;
const
  fPlaintext = 'hello world';
  fPassword  = 'testpassword';
var
  fCiphertext, fDecrypted: RawByteString;
begin
  fCiphertext := CbftpEncryptAES(fPlaintext, fPassword);
  CheckTrue(fCiphertext <> '', 'Encrypt must not return empty');
  fDecrypted := DecryptForTest(fCiphertext, fPassword);
  CheckEquals(fPlaintext, fDecrypted,
    'Decrypted output must match original plaintext');
end;

procedure TTestCbftpEncryptAES.TestRoundtripExactBlockBoundary;
const
  // 16 bytes exactly — PKCS7 adds a full extra block, so ciphertext = 32 bytes
  fPlaintext = '1234567890abcdef';
  fPassword  = 'blocktest';
var
  fCiphertext, fDecrypted: RawByteString;
begin
  fCiphertext := CbftpEncryptAES(fPlaintext, fPassword);
  CheckTrue(fCiphertext <> '', 'Encrypt must not return empty');
  CheckEquals(0, (Length(fCiphertext) - 16) mod 16,
    'Ciphertext part must be a multiple of AES block size');
  fDecrypted := DecryptForTest(fCiphertext, fPassword);
  CheckEquals(fPlaintext, fDecrypted,
    'Decrypted output must match plaintext at exact block boundary');
end;

procedure TTestCbftpEncryptAES.TestRoundtripRaceCommandFormat;
const
  fPassword = 'udpSecret123';
  fSection  = 'MP3';
  fRelease  = 'Artist-Album-2025-GRP';
  fSites    = 'SITE1,SITE2,SITE3';
var
  fPlaintext, fCiphertext, fDecrypted: RawByteString;
begin
  // Mirrors the exact udpMessage construction in pazo.pas
  fPlaintext  := RawByteString(fPassword + ' race ' + fSection + ' ' + fRelease + ' ' + fSites);
  fCiphertext := CbftpEncryptAES(fPlaintext, RawByteString(fPassword));
  CheckTrue(fCiphertext <> '', 'Encrypt must not return empty for race command');
  fDecrypted := DecryptForTest(fCiphertext, RawByteString(fPassword));
  CheckEquals(string(fPlaintext), string(fDecrypted),
    'Decrypted race command must match original');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('CbftpEncryptAES', TTestCbftpEncryptAES.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestCbftpEncryptAES);
  {$ENDIF}
end.
