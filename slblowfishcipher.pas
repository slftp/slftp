unit slblowfishcipher;

interface

uses
  mormot.lib.openssl11;

type
  TslBlowfishMode = (
    sbmECB, //< independent eight-byte blocks
    sbmCBC, //< cipher-block chaining
    sbmCFB  //< 64-bit cipher feedback
  );

  { OpenSSL EVP context for one Blowfish operation. }
  TslBlowfishCipher = class
  private
    FContext: PEVP_CIPHER_CTX;
  public
    { Initializes one Blowfish cipher context.
      @param(aKey Blowfish key bytes)
      @param(aIV eight-byte initialization vector for CBC and CFB; empty for ECB)
      @param(aMode Blowfish mode)
      @param(aEncrypt @true to encrypt, @false to decrypt) }
    constructor Create(const aKey, aIV: array of Byte; aMode: TslBlowfishMode;
      aEncrypt: Boolean);
    { Releases the OpenSSL cipher context. }
    destructor Destroy; override;

    { Transforms input bytes into an equally sized output buffer.
      @param(aInput input buffer)
      @param(aOutput output buffer; may equal aInput)
      @param(aLength input length; ECB and CBC require a multiple of eight)
      @returns(number of output bytes) }
    function Update(aInput, aOutput: PByte; aLength: Integer): Integer;

    { Finalizes the context and raises an exception if finalization fails. }
    procedure Final;
  end;

implementation

uses
  SysUtils, mormot.core.base, slssl;

constructor TslBlowfishCipher.Create(const aKey, aIV: array of Byte;
  aMode: TslBlowfishMode; aEncrypt: Boolean);
var
  fCipherName: RawUtf8;
  fCipher: PEVP_CIPHER;
  fKey: PByte;
  fIV: PByte;
  fError: String;
  fDirection: Integer;
begin
  inherited Create;
  FContext := nil;
  if (Length(aKey) = 0) or (Length(aKey) > 56) then
    raise Exception.Create('Blowfish key length must be between 1 and 56 bytes');
  if (aMode = sbmECB) and (Length(aIV) <> 0) then
    raise Exception.Create('Blowfish ECB does not use an IV');
  if (aMode <> sbmECB) and (Length(aIV) <> 8) then
    raise Exception.Create('Blowfish CBC/CFB requires an eight-byte IV');

  fError := '';
  if not InitOpenSSL(fError) then
    raise Exception.Create('OpenSSL initialization failed: ' + fError);

  case aMode of
    sbmECB: fCipherName := 'BF-ECB';
    sbmCBC: fCipherName := 'BF-CBC';
  else
    fCipherName := 'BF-CFB';
  end;
  fCipher := EVP_get_cipherbyname(PUtf8Char(fCipherName));
  if fCipher = nil then
    raise Exception.Create('OpenSSL Blowfish cipher unavailable: ' +
      String(fCipherName));

  fKey := @aKey[0];
  fIV := nil;
  if Length(aIV) > 0 then
    fIV := @aIV[0];
  fDirection := Ord(aEncrypt);

  FContext := EVP_CIPHER_CTX_new();
  if FContext = nil then
    raise Exception.Create('Could not create OpenSSL Blowfish context');
  try
    if EVP_CipherInit_ex(FContext, fCipher, nil, nil, nil, fDirection) <> 1 then
      raise Exception.Create('Could not initialize OpenSSL Blowfish cipher');
    if EVP_CIPHER_CTX_set_key_length(FContext, Length(aKey)) <> 1 then
      raise Exception.Create('OpenSSL rejected Blowfish key length');
    if EVP_CIPHER_CTX_set_padding(FContext, 0) <> 1 then
      raise Exception.Create('Could not disable OpenSSL Blowfish padding');
    if EVP_CipherInit_ex(FContext, nil, nil, fKey, fIV, fDirection) <> 1 then
      raise Exception.Create('Could not set OpenSSL Blowfish key and IV');
  except
    EVP_CIPHER_CTX_free(FContext);
    FContext := nil;
    raise;
  end;
end;

destructor TslBlowfishCipher.Destroy;
begin
  if FContext <> nil then
    EVP_CIPHER_CTX_free(FContext);
  inherited Destroy;
end;

function TslBlowfishCipher.Update(aInput, aOutput: PByte;
  aLength: Integer): Integer;
begin
  Result := 0;
  if aLength < 0 then
    raise Exception.Create('Blowfish input length cannot be negative');
  if aLength = 0 then
    Exit;
  if (aInput = nil) or (aOutput = nil) then
    raise Exception.Create('Blowfish input and output buffers must be assigned');
  if EVP_CipherUpdate(FContext, aOutput, @Result, aInput, aLength) <> 1 then
    raise Exception.Create('OpenSSL Blowfish transform failed');
  if Result <> aLength then
    raise Exception.CreateFmt('OpenSSL Blowfish returned %d bytes for %d input bytes',
      [Result, aLength]);
end;

procedure TslBlowfishCipher.Final;
var
  fOutput: array[0..15] of Byte;
  fOutputLength: Integer;
begin
  fOutputLength := 0;
  if EVP_CipherFinal_ex(FContext, @fOutput[0], @fOutputLength) <> 1 then
    raise Exception.Create('OpenSSL Blowfish finalization failed');
  if fOutputLength <> 0 then
    raise Exception.CreateFmt('OpenSSL Blowfish produced unexpected final bytes: %d',
      [fOutputLength]);
end;

end.
