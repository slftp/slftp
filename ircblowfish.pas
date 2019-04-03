unit ircblowfish;

interface

uses
  Contnrs, delphiblowfish
  {$IFDEF MSWINDOWS}
    , Windows
  {$ENDIF};

type
  TIrcBlowkey = class
  private
    KeyData: TBlowfishData; //< generated blowfish data for @link(blowkey) in ECB mode
    fBlowkey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; //< { blowkey for @link(channel) }
    fCBC: Boolean; //< @true if channel is CBC encrypted, @false otherwise.
  public
    netname: String; //< netname of IRC network
    channel: String; //< IRC channelname
    chankey: String; //< chankey for @link(channel) which is needed to join it
    names: String; // funkcionalitasa a csatornanak - Functionality of the channel
    inviteonly: Boolean; //< @true if channel is invite only (you have to invite yourself first), @false otherwise.
    property cbc: Boolean read fCBC;
    property blowkey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF} read fBlowkey;

    { Updates IrcBlowkey and CBC mode value }
    procedure UpdateKey(const aBlowkey: String; aCBCMode: Boolean);

    { Creates a new IrcBlowkey entry }
    constructor Create(const netname, channel, blowkey: String; chankey: String = ''; inviteonly: Boolean = True; cbc: Boolean = False);
    function HasKey(key: String): Boolean;
  end;

{ function to decide which encryption mode should be used @br
  For more infos: http://blog.bjrn.se/2009/01/proposal-for-better-irc-encryption.html }
function irc_encrypt(const netname, channel, dText: String; include_ok: Boolean = False): String;

{ Encrypt decrypted dText in CBC mode }
function irc_cbc_encrypt(const dText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; const BlowfishKey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
{ Decrypt encrypted eText in CBC mode }
function irc_cbc_decrypt(const netname, channel, eText: String): String;

{ Encrypt decrypted dText in ECB mode }
function irc_ecb_encrypt(dText: String; BlowfishKeyData: TBlowfishData): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
{ Decrypt encrypted eText in ECB mode }
function irc_ecb_decrypt(const netname, channel, eText: String): String;

function irc_RegisterChannel(const netname, channel, blowkey: String; chankey: String = ''; inviteonly: Boolean = False; cbc: Boolean = False): TIrcBlowkey;
function FindIrcBlowfish(const netname, channel: String; acceptdefault: Boolean = True): TIrcBlowkey;
procedure IrcblowfishInit;
procedure IrcblowfishUnInit;

var
  chankeys: TObjectList;

implementation

uses
  SysUtils, Classes, console, debugunit, IdSSLOpenSSLHeaders, Base64OpenSSL;

const
  B64: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF} = './0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  section: String = 'ircblowfish';

{ functions for ECB de-/encryption }

// perl compatible index, cause delphis pos works different
function PCindex(const w: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): Cardinal;
begin
  Result := Pos(w, B64);
  if Result > 0 then dec(Result);
end;

function PCsubstr(const w: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; i: Integer): AnsiChar;
var
  s: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
begin
  Result := #0;
  s := Copy(w, i + 1, 1);
  if (length(s) > 0) then
    Result := s[1];
end;

{ non-standard base64 implementation for ECB mode }
function bytetoB64(const ec: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
var
  dc: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  left, right: Cardinal;
  i, k : Integer;
begin
  dc := '';
  k := -1;

  while (k < (length(ec) - 1)) do
  begin
    inc(k);
    left := (ord(PCsubstr(ec,k)) shl 24);
    inc(k);
    inc(left,(ord(PCsubstr(ec,k)) shl 16));
    inc(k);
    inc(left, (ord(PCsubstr(ec,k)) shl 8));
    inc(k);
    inc(left, ord(PCsubstr(ec,k)));

    inc(k);
    right := (ord(PCsubstr(ec,k)) shl 24);
    inc(k);
    inc(right,(ord(PCsubstr(ec,k)) shl 16));
    inc(k);
    inc(right,(ord(PCsubstr(ec,k)) shl 8));
    inc(k);
    inc(right, ord(PCsubstr(ec,k)));

    for i := 0 to 5 do
    begin
      dc := dc + PCsubstr(B64, right and $3F);
      right := right shr 6;
    end;

    for i := 0 to 5 do
    begin
      dc := dc + PCsubstr(B64, left and $3F);
      left := left shr 6;
    end;
  end;

  Result := dc;
end;

{ non-standard base64 implementation for ECB mode }
function B64tobyte(const ec: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
var
  dc: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  k: Integer;
  i, right, left: Cardinal;
begin
  dc := '';
  k := -1;

  while (k < (length(ec) - 1)) do
  begin
     right := 0;
     left := 0;

     for i := 0 to 5 do
     begin
       inc(k);
       right := right or (PCindex(PCsubstr(ec, k)) shl (i * 6));
     end;

     for i := 0 to 5 do
     begin
       inc(k);
       left := left or (PCindex(PCsubstr(ec, k)) shl (i * 6));
     end;

     for i := 0 to 3 do
     begin
       dc := dc + AnsiChar(Chr((left and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8)));
     end;

     for i := 0 to 3 do
     begin
       dc := dc + AnsiChar(Chr((right and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8)));
     end;
  end;

  Result := dc;
end;


function set_key(const key: String): String;
var
  i, keyLen: Integer;
  newkey: String;
begin
  Result := key;

  if (length(key) < 8) then
  begin
    keyLen := length(key);
    i := 8 div keyLen;
    if (8 mod keyLen > 0) then inc(i);

    newkey := '';

    while (i > 0) do
    begin
      newkey := newkey + key;
      dec(i);
    end;

    Result := newkey;
  end;
end;

function FindIrcBlowfish(const netname, channel: String; acceptdefault: Boolean = True): TIrcBlowkey;
var
  i: Integer;
  bf: TIrcBlowkey;
begin
  Result := nil;

  if ((acceptdefault) and (chankeys.Count = 0)) then
  begin
    Debug(dpMessage, section, Format('No default chankey for %s@%s registered.', [channel, netname]));
    exit;
  end;

  for i := 0 to chankeys.Count - 1 do
  begin
    bf := TIrcBlowkey(chankeys[i]);
    if ((AnsiSameText(bf.channel, channel)) and (AnsiSameText(bf.netname, netname))) then
    begin
      Result := bf;
      exit;
    end;
  end;

  if acceptdefault then
    Result := chankeys[0] as TIrcBlowkey
  else
    Result := nil;
end;

function irc_encrypt(const netname, channel, dText: String; include_ok: Boolean = False): String;
var
  bf: TIrcBlowkey;
begin
  Result := '';

  if (dText = '') then
    exit;

  bf := FindIrcBlowfish(netname, channel);
  if ((bf = nil) or (bf.blowkey = '')) then
  begin
    Result := dText;
    exit;
  end;

  if bf.cbc then
  begin
    // do CBC encryption
    Result := irc_cbc_encrypt(dText, bf.blowkey);
  end
  else
  begin
    // do ECB encryption
    Result := irc_ecb_encrypt(dText, bf.KeyData);
  end;

  if include_ok then
    {$IFDEF UNICODE}
      Result := UTF8ToString('+OK ' + Result)
    {$ELSE}
      Result := '+OK ' + Result
    {$ENDIF}
  else
    {$IFDEF UNICODE}
      Result := UTF8ToString(Result);
    {$ELSE}
      Result := Result;
    {$ENDIF}
end;

{ CBC mode }
function BlowfishCipherWalk(aCTX: PEVP_CIPHER_CTX; aBufIn: PAnsiChar; aInSize: integer; out aOut: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): boolean;
var
  fSuccess: boolean;
  fBytesLeft: integer;
  fBufPtr: PAnsiChar;
  fInSize: integer;
  fTmpBuf: array[0..255] of Byte;
  fOutLen: integer;
  fArgOutLen: integer;
begin
  Result := False;

  fSuccess := False;
  fOutLen := 255;

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

    aOut := '';
    fArgOutLen := Length(aOut);

    SetLength(aOut, fArgOutLen + fOutLen);
    Move(fTmpBuf[0], aOut[fArgOutLen + 1], fOutLen);

    Dec(fBytesLeft, fInSize);
    Inc(fBufPtr, fInSize);
  end;

  fSuccess := (EVP_CipherFinal_ex(aCTX, @fTmpBuf[0], @fOutLen) = 1);

  if (fSuccess) then
  begin
    fArgOutLen := Length(aOut);

    SetLength(aOut, fArgOutLen + fOutLen);
    Move(fTmpBuf[0], aOut[fArgOutLen + 1], fOutLen);
  end;

  Result := fSuccess;
end;

function irc_cbc_encrypt(const dText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; const BlowfishKey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
const
  IV: array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00);
var
  fKeyLen: integer;
  fCTX: EVP_CIPHER_CTX;
  fInBufSize: integer;
  fInBufSizeMod: integer;
  fInBuf: array of Byte;
  fRealIV: array[0..7] of Byte;
  fSuccess: boolean;
  eText, eTextBase64: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
begin
  Result := '';
  eText := '';
  eTextBase64 := '';

  FillChar(fRealIV[0], Length(fRealIV), 0);

  fKeyLen := Length(BlowfishKey);
  if (fKeyLen >= 56) then
    fKeyLen := 56;

  {* init struct for encryption *}
  EVP_CIPHER_CTX_init(@fCTX);
  try
    if (EVP_CipherInit_ex(@fCTX, EVP_bf_cbc(), nil, nil, nil, 1) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC encryption!');
      exit;
    end;

    {* set options *}
    EVP_CIPHER_CTX_set_key_length(@fCTX, fKeyLen);
    // disable auto padding. Required for Mircryption compatibility.
    EVP_CIPHER_CTX_set_padding(@fCTX, 0);

    {* actually initialize session context *}
    // 1 for encryption mode
    if (EVP_CipherInit_ex(@fCTX, nil, nil, @BlowfishKey[1], @IV[0], 1) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC encryption!');
      exit;
    end;

    // prepare buffers
    fInBufSize := Length(dText);
    fInBufSizeMod := fInBufSize mod 8;
    if (fInBufSizeMod <> 0) then
    begin
      Inc(fInBufSize, 8 - fInBufSizeMod);
    end;
    Inc(fInBufSize, 8); // for the IV data

    SetLength(fInBuf, fInBufSize);
    // important for padding
    FillChar(fInBuf[0], fInBufSize, 0);

    // for some f*cked up reason, Mircryption's CBC blowfish does not use an
    // explicit IV, but prepends 8 bytes of random data to the actual string
    // instead, so we have to do this too... }
    {$IFDEF MSWINDOWS}
      // generate IV using screen/user input
      RAND_screen();
    {$ELSE}
      // TODO: Add new stuff to random bytes from time to time
    {$ENDIF}

    if (RAND_bytes(@fRealIV[0], 8) <> 1) then
    begin
      // fallback but deprecated in OpenSSL
      if (RAND_pseudo_bytes(@fRealIV[0], 8) <> 1) then
      begin
        Debug(dpError, section, '[FiSH] Can not get random numbers for CBC encryption!');
        exit;
      end;
    end;
    // got an IV
    move(fRealIV[0], fInBuf[0], 8);

    // append unencrypted msg to buffer
    move(dText[1], fInBuf[8], Length(dText));

    {* encrypt... *}
    fSuccess := BlowfishCipherWalk(@fCTX, @fInBuf[0], fInBufSize, eText);
    if not fSuccess then
      exit;

  finally
    if (EVP_CIPHER_CTX_cleanup(@fCTX) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] EVP_CIPHER_CTX_cleanup failed for CBC encryption!');
      // we can't do something...
    end;
  end;

  if (DoBase64Encode(eText, eTextBase64) < 1) then
  begin
    Debug(dpError, section, Format('[FiSH] Base64 Encode for %s failed!', [dText]));
    exit;
  end;

  // append * to mark it as CBC message
  Result := '*' + eTextBase64;
end;

function irc_cbc_decrypt(const netname, channel, eText: String): String;
const
  IV: array[0..7] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00);
var
  bf: TIrcBlowkey;
  bfKey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fKeyLen: integer;
  fCTX: EVP_CIPHER_CTX;
  B64Text: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  LengthB64Text: integer;
  LengthB64TextMod: integer;
  fBeenCut: boolean;
  fSuccess: boolean;
  DecryptedText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  i, j, fLength: integer;
  fCleanedStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
begin
  Result := eText;
  bf := FindIrcBlowfish(netname, channel);
  if ((bf = nil) or (bf.blowkey = '')) then
  begin
    Result := eText;
    exit;
  end;

  if not bf.cbc then
  begin
    Debug(dpError, section, Format('[FiSH] Fishkey is for ECB but encrypted text is in CBC mode! %s@%s : %s', [channel, netname, eText]));
    exit;
  end;

  // get used blowkey and length of it
  bfKey := bf.blowkey;
  fKeyLen := Length(bfKey);

  {* init struct for decryption *}
  EVP_CIPHER_CTX_init(@fCTX);
  try
    if (EVP_CipherInit_ex(@fCTX, EVP_bf_cbc(), nil, nil, nil, 0) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] First EVP_CipherInit_ex failed for CBC decryption!');
      exit;
    end;

    {* set options *}
    EVP_CIPHER_CTX_set_key_length(@fCTX, fKeyLen);
    // MUST be the same setting used during encryption
    EVP_CIPHER_CTX_set_padding(@fCTX, 0);

    {* actually initialize session context *}
    // 0 for decryption mode
    if (EVP_CipherInit_ex(@fCTX, nil, nil, @bfKey[1], @IV[0], 0) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] Second EVP_CipherInit_ex failed for CBC decryption!');
      exit;
    end;

    {* do base64 decryption *}
    LengthB64Text := DoBase64Decode(eText, B64Text);
    if (LengthB64Text < 1) then
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
    fSuccess := BlowfishCipherWalk(@fCTX, @B64Text[1], LengthB64Text, DecryptedText);
    if not fSuccess then
      exit;

  finally
    if (EVP_CIPHER_CTX_cleanup(@fCTX) <> 1) then
    begin
      Debug(dpError, section, '[FiSH] EVP_CIPHER_CTX_cleanup failed for CBC decryption!');
      // we can't do something...
    end;
  end;

  // even if the decryption was not successful, there might be *some* data in
  // the out buffer, so we should always do this
  Delete(DecryptedText, 1, 8); // remove IV data

  // remove bad \x00 and \x0d\x0a chars
  // see https://github.com/flakes/mirc_fish_10/blob/master/fish_10/src/util.cpp#L211
  SetLength(fCleanedStr, Length(DecryptedText));
  fLength := 0;
  for i := 1 to Length(DecryptedText) do
  begin
    j := Ord(DecryptedText[i]);
    if not ((j = 0) or (j = 10) or (j = 13)) then
    begin
      Inc(fLength);
      fCleanedStr[fLength] := DecryptedText[i];
    end;
  end;
  SetLength(fCleanedStr, fLength);

  {$IFDEF UNICODE}
    Result := UTF8ToString(fCleanedStr);
  {$ELSE}
    Result := fCleanedStr;
  {$ENDIF}
end;


{ ECB mode }
function irc_ecb_encrypt(dText: String; BlowfishKeyData: TBlowfishData): {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
var
  temp: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  i: Integer;
begin
  temp := '';
  // encrypted text
  Result := '';

  // Each message is split into blocks of 8 bytes, encrypted individually.
  // A block shorter than 8 bytes is padded with zeroes.
  if (length(dText) mod 8 > 0) then
    for i := 1 to 8 - (length(dText) mod 8) do
      dText := dText + #0;

  SetLength(temp, 8);

  for i := 1 to length(dText) div 8 do
  begin
    temp := Copy(dText, 1 + (i - 1) * 8, 8);
    BlowfishEncryptECB(BlowfishKeyData, PAnsiChar(temp), PAnsiChar(temp));
    Result := Result + bytetoB64(temp);
  end;
end;

function irc_ecb_decrypt(const netname, channel, eText: String): String;
var
  temp, dText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  i : Integer;
  bf: TIrcBlowkey;
begin
  Result := eText;
  bf := FindIrcBlowfish(netname, channel);
  if ((bf = nil) or (bf.blowkey = '')) then
  begin
    Result := eText;
    exit;
  end;

  if bf.cbc then
  begin
    Debug(dpError, section, Format('[FiSH] Fishkey is for CBC but encrypted text is in ECB mode! %s@%s : %s', [channel, netname, eText]));
    exit;
  end;

  dtext := '';
  for i := 1 to length(eText) div 12 do
  begin
    temp := B64tobyte(Copy(eText, 1 + (i - 1) * 12, 12));
    SetLength(temp, 8);
    BlowfishDecryptECB(bf.KeyData, PAnsiChar(temp), PAnsiChar(temp));
    dText := dtext + temp;
  end;

  Result := dText;
end;


{ TIrcBlowkey }

constructor TIrcBlowkey.Create(const netname, channel, blowkey: String; chankey: String = ''; inviteonly: Boolean = True; cbc: Boolean = False);
begin
  self.channel := channel;
  self.chankey := chankey;
  self.netname := netname;
  self.inviteonly := inviteonly;
  self.fCBC := cbc;
  UpdateKey(blowkey, cbc);
end;

function TIrcBlowkey.HasKey(key: String): Boolean;
begin
  Result := False;
  key := ' ' + UpperCase(key) + ' ';
  if Pos(key, names) > 0 then
    Result := True;
end;

procedure TIrcBlowKey.UpdateKey(const aBlowkey: String; aCBCMode: Boolean);
const
  IV: array[0..7] of byte = ($11, $22, $33, $44, $55, $66, $77, $88);
var
  myKey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
begin
  fBlowkey := {$IFDEF UNICODE}UTF8Encode(aBlowkey){$ELSE}aBlowkey{$ENDIF};
  if blowkey <> '' then
  begin
    myKey := {$IFDEF UNICODE}set_key(blowkey){$ELSE}set_key(blowkey){$ENDIF};
    SetLength(myKey, length(myKey));
    BlowfishInit(KeyData, PAnsiChar(myKey), Length(myKey), @iv);
    fCBC := aCBCMode;
  end
  else
    myKey := '';
end;

procedure IrcBlowfishInit;
begin
  chankeys := TObjectList.Create;
end;

procedure IrcBlowfishUninit;
var
  i: Integer;
begin
  Debug(dpSpam, section, 'Uninit1');

  for i := 0 to chankeys.Count - 1 do
    BlowfishBurn(TIrcBlowkey(chankeys[i]).KeyData);

  chankeys.Free;

  Debug(dpSpam, section, 'Uninit2');
end;

function irc_RegisterChannel(const netname, channel, blowkey: String; chankey: String = ''; inviteonly: Boolean= False; cbc: Boolean = False): TIrcBlowkey;
begin
  Result := FindIrcBlowfish(netname, channel, False);
  if Result = nil then
  begin
    console_add_ircwindow(netname + ' ' + channel);
    Result := TIrcBlowkey.Create(netname, channel, blowkey, chankey, inviteonly, cbc);
    chankeys.Add(Result);
  end;
end;

end.
