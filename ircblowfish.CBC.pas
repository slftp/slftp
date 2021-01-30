unit ircblowfish.CBC;

interface

uses
  ircchansettings, SysUtils;

type
  { @abstract(CBC de/encryption for IRC Channel) }
  TIrcBlowkeyCBC = class(TIrcChannelSettings)
  const
    IV: array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00); //< standardized IV for CBC
  private
    FBlowkey: TBytes; //< blowkey for channel
    FBlowkeyLength: integer; //< blowkey length

    property BlowkeyLength: integer read FBlowkeyLength;
  public
    { Creates a new IrcBlowkey entry }
    constructor Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
    { Sets @link(FBlowkey) and @link(FBlowkeyLength)
      @param(aBlowkey blowkey) }
    procedure UpdateKey(const aBlowkey: String); override;
    { Encrypts decrypted input text, adds the '+OK *' to returned result
      @param(dText decrypted message)
      @returns(encrypted text) }
    function EncryptMessage(const dText: String): String; override;
    { Decrypts encrypted input text, caller is responsible to remove '+OK *'
      @param(eText encrypted message)
      @returns(decrypted text) }
    function DecryptMessage(const eText: String): String; override;

    property Blowkey: TBytes read FBlowkey;
  end;

implementation

uses
  debugunit, IdSSLOpenSSLHeaders, IdOpenSSLHeaders_ossl_typ, IdOpenSSLHeaders_evp, IdOpenSSLHeaders_evperr, IdOpenSSLHeaders_rand, {$IFDEF UNICODE}NetEncoding,{$ENDIF} mystrings;

const
  section = 'ircblowfish.CBC';

{ functions for CBC de-/encryption }

function BlowfishCipherWalk(aCTX: PEVP_CIPHER_CTX; aBufIn: PByte; aInSize: integer; out aOut: TBytes): boolean;
var
  fSuccess: boolean;
  fBytesLeft: integer;
  fBufPtr: PByte;
  fInSize: integer;
  fTmpBuf: array[0..255] of Byte;
  fOutLen: integer;
  fArgOutLen: integer;
begin
  Result := False;
  // init
  SetLength(aOut, 0);
  fSuccess := False;
  fOutLen := 0;
  fBytesLeft := aInSize;
  fBufPtr := aBufIn;

  while (fBytesLeft > 0) do
  begin
    if (fBytesLeft > 256) then
    begin
      fInSize := 256;
    end
    else
    begin
      fInSize := fBytesLeft;
    end;

    if (EVP_CipherUpdate(aCTX, @fTmpBuf[0], @fOutLen, fBufPtr, fInSize) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] EVP_CipherUpdate failed for CBC!');
      exit;
    end;

    // zero in the first loop
    fArgOutLen := Length(aOut);

    SetLength(aOut, fArgOutLen + fOutLen);
    Move(fTmpBuf[0], aOut[fArgOutLen], fOutLen);

    Dec(fBytesLeft, fInSize);
    Inc(fBufPtr, fInSize);
  end;

  fSuccess := (EVP_CipherFinal_ex(aCTX, @fTmpBuf[0], @fOutLen) = 1);

  if ((fSuccess) and (fOutLen > 0)) then
  begin
    fArgOutLen := Length(aOut);
    SetLength(aOut, fArgOutLen + fOutLen);
    Move(fTmpBuf[0], aOut[fArgOutLen + 1], fOutLen);
  end;

  Result := fSuccess;
end;

{ TIrcBlowkeyCBC }

constructor TIrcBlowkeyCBC.Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
begin
  inherited Create(aNetname, aChannel, aChanRoles, aChankey, aInviteOnly);

  UpdateKey(aBlowkey);
end;

procedure TIrcBlowkeyCBC.UpdateKey(const aBlowkey: String);
begin
  {$IFDEF UNICODE}
    FBlowkey := TEncoding.UTF8.GetBytes(aBlowkey);
  {$ELSE}
    SetLength(FBlowkey, Length(aBlowkey));
    move(aBlowkey[1], FBlowkey[0], Length(aBlowkey));
  {$ENDIF}

  FBlowkeyLength := Length(aBlowkey);

  if FBlowkeyLength = 0 then
  begin
    Debug(dpError, section, Format('Empty CBC blowkey for %s-%s does not make sense!', [Netname, Channel]));
    exit;
  end;

  if (FBlowkeyLength >= 56) then
    FBlowkeyLength := 56;
end;

function TIrcBlowkeyCBC.EncryptMessage(const dText: String): String;
var
  fCTX: PEVP_CIPHER_CTX;
  fInBufSize: integer;
  fInBufSizeMod: integer;
  fDTextLength: Integer;
  fInBuf: TBytes;
  fRealIV: array[0..7] of Byte;
  fSuccess: boolean;
  eText: TBytes;
  eTextBase64: String;
  dTextHelper: TBytes;
begin
  Result := '';
  fDTextLength := Length(dText);

  {$IFDEF UNICODE}
    dTextHelper := TEncoding.UTF8.GetBytes(dText);
  {$ELSE}
    SetLength(dTextHelper, fDTextLength);
    move(dText[1], dTextHelper[0], fDTextLength);
  {$ENDIF}

  FillChar(fRealIV[0], Length(fRealIV), 0);

  {* init struct for encryption *}
  fCTX := EVP_CIPHER_CTX_new();
  try
    if (EVP_CipherInit_ex(fCTX, EVP_bf_cbc(), nil, nil, nil, 1) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC encryption!');
      exit;
    end;

    {* set options *}
    EVP_CIPHER_CTX_set_key_length(fCTX, BlowkeyLength);
    // disable auto padding. Required for Mircryption compatibility.
    EVP_CIPHER_CTX_set_padding(fCTX, 0);

    {* actually initialize session context *}
    // 1 for encryption mode
    if (EVP_CipherInit_ex(fCTX, nil, nil, @Blowkey[0], @IV[0], 1) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC encryption!');
      exit;
    end;

    // prepare buffers
    fInBufSize := Length(dTextHelper);
    fInBufSizeMod := fInBufSize mod 8;
    if (fInBufSizeMod <> 0) then
    begin
      Inc(fInBufSize, 8 - fInBufSizeMod);
    end;
    Inc(fInBufSize, 8); // for the IV data

    SetLength(fInBuf, fInBufSize);
    // important for padding
    FillChar(fInBuf[0], fInBufSize, 0);

    { for some f*cked up reason, Mircryption's CBC blowfish does not use an
      explicit IV, but prepends 8 bytes of random data to the actual string
      instead, so we have to do this too...
      generate IV using input string }
    // RAND_add() may be called with sensitive data such as user entered passwords.
    // The seed values cannot be recovered from the PRNG output.
    RAND_seed(@dText, fDTextLength);

    if (RAND_bytes(@fRealIV[0], Length(fRealIV)) <> 1) then
    begin
      // fallback but deprecated in OpenSSL
      if (RAND_pseudo_bytes(@fRealIV[0], Length(fRealIV)) <> 1) then
      begin
        Debug(dpError, section, '[FiSH] Can not get random numbers for CBC encryption!');
        exit;
      end;
    end;
    // got an IV
    move(fRealIV[0], fInBuf[0], 8);

    // append unencrypted msg to buffer
    move(dTextHelper[0], fInBuf[8], Length(dTextHelper));

    {* encrypt... *}
    fSuccess := BlowfishCipherWalk(fCTX, @fInBuf[0], fInBufSize, eText);
    if not fSuccess then
      exit;

  finally
    EVP_CIPHER_CTX_free(fCTX);
  end;

  {* do base64 encoding *}
  eTextBase64 := DoBase64Encode(eText);
  if (eTextBase64.Length = 0) then
  begin
    Debug(dpError, section, Format('[FiSH] Base64 Encode for %s failed!', [dText]));
    exit;
  end;

  // append * to mark it as CBC message
  Result := '+OK *' + eTextBase64;
end;

function TIrcBlowkeyCBC.DecryptMessage(const eText: String): String;
var
  fCTX: PEVP_CIPHER_CTX;
  B64Text: TBytes;
  LengthB64Text: integer;
  LengthB64TextMod: integer;
  fBeenCut: boolean;
  fSuccess: boolean;
  DecryptedText: TBytes;
  i, fLength: integer;
  j: Byte;
  fCleanedBytes: TBytes;
begin
  Result := eText;

  {* init struct for decryption *}
  fCTX := EVP_CIPHER_CTX_new();
  try
    if (EVP_CipherInit_ex(fCTX, EVP_bf_cbc(), nil, nil, nil, 0) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC decryption!');
      exit;
    end;

    {* set options *}
    EVP_CIPHER_CTX_set_key_length(fCTX, BlowkeyLength);
    // MUST be the same setting used during encryption
    EVP_CIPHER_CTX_set_padding(fCTX, 0);

    {* actually initialize session context *}
    // 0 for decryption mode
    if (EVP_CipherInit_ex(fCTX, nil, nil, @Blowkey[0], @IV[0], 0) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] Second EVP_CipherInit_ex failed for CBC decryption!');
      exit;
    end;

    {* do base64 decoding *}
    B64Text := DoBase64DecodeToBytes(eText);
    LengthB64Text := Length(B64Text);
    if (LengthB64Text = 0) then
    begin
      Debug(dpError, section, Format('[FiSH] Base64 Decode for %s failed!', [eText]));
      exit;
    end;

    LengthB64TextMod := LengthB64Text mod 8;
    fBeenCut := (LengthB64TextMod <> 0);
    if fBeenCut then
    begin
      SetLength(B64Text, LengthB64Text - LengthB64TextMod);
      LengthB64Text := Length(B64Text);
    end;

    {* decrypt... *}
    fSuccess := BlowfishCipherWalk(fCTX, @B64Text[0], LengthB64Text, DecryptedText);
    if not fSuccess then
      exit;

  finally
    EVP_CIPHER_CTX_free(fCTX);
  end;

  // even if the decryption was not successful, there might be *some* data in
  // the out buffer, so we should always do this
  Delete(DecryptedText, 0, 8); // remove IV data

  // remove bad \x00 and \x0d\x0a chars
  // see https://github.com/flakes/mirc_fish_10/blob/master/fish_10/src/util.cpp#L211
  SetLength(fCleanedBytes, Length(DecryptedText));
  fLength := 0;
  for i := 0 to Length(DecryptedText) - 1 do
  begin
    j := DecryptedText[i];
    if not ((j = 0) or (j = 10) or (j = 13)) then
    begin
      fCleanedBytes[fLength] := DecryptedText[i];
      Inc(fLength);
    end;
  end;
  SetLength(fCleanedBytes, fLength);

  {$IFDEF UNICODE}
    Result := TEncoding.UTF8.GetString(fCleanedBytes);
  {$ELSE}
    SetLength(Result, Length(fCleanedBytes));
    move(fCleanedBytes[0], Result[1], Length(fCleanedBytes));
  {$ENDIF}
end;

end.
