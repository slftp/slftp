unit ircblowfish.ECB;

interface

uses
  SysUtils, ircchansettings;

type
  {$IFDEF FPC}
    TBlowfishKeyBytes = array of Byte;
  {$ELSE}
    TBlowfishKeyBytes = TBytes;
  {$ENDIF}

  { @abstract(ECB de/encryption for IRC Channel) }
  TIrcBlowkeyECB = class(TIrcChannelSettings)
  private
    FKeyBytes: TBlowfishKeyBytes; //< Blowfish key bytes for @link(blowkey)
    FBlowkey: String; //< blowkey for channel
  public
    { Creates a new IrcBlowkey entry }
    constructor Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
    { Sets the key used for IRC Blowfish ECB encryption.
      @param(aBlowkey channel Blowfish key) }
    procedure UpdateKey(const aBlowkey: String); override;
    { Encrypts decrypted input text, adds the '+OK ' to returned result
      @param(dText decrypted message)
      @returns(encrypted text) }
    function EncryptMessage(const dText: String): String; override;
    { Decrypts encrypted input text, caller is responsible to remove '+OK '
      @param(eText encrypted message)
      @returns(decrypted text) }
    function DecryptMessage(const eText: String): String; override;

    property Blowkey: String read FBlowkey;
  end;

implementation

uses
  debugunit, slblowfishcipher;

const
  B64: RawByteString = './0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  section = 'ircblowfish.ECB';

{ functions for ECB de-/encryption }

// perl compatible index: 0-based position of w in B64, but 0 instead of -1 if not found
function PCindex(const w: AnsiChar): Cardinal;
var
  i: Integer;
begin
  // a loop instead of Pos: Pos needs a temporary string for the char, which costs an allocation per char
  Result := 0;
  for i := 1 to Length(B64) do
    if B64[i] = w then
      Exit(i - 1);
end;

function PCsubstr(const w: RawByteString; const i: Integer): AnsiChar;
begin
  // index the string directly instead of copying a substring, which costs an allocation per char
  Result := #0;
  if (i >= 0) and (i < Length(w)) then
    Result := w[i + 1];
end;

{ non-standard base64 implementation for ECB mode }
function bytetoB64(const ec: RawByteString): RawByteString;
var
  left, right: Cardinal;
  i, k, fPos: Integer;
begin
  // each started block of 8 bytes gives 12 chars; allocate the result at once instead of growing it char by char
  SetLength(Result, ((Length(ec) + 7) div 8) * 12);
  fPos := 0;
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
      inc(fPos);
      Result[fPos] := PCsubstr(B64, right and $3F);
      right := right shr 6;
    end;

    for i := 0 to 5 do
    begin
      inc(fPos);
      Result[fPos] := PCsubstr(B64, left and $3F);
      left := left shr 6;
    end;
  end;
end;

{ non-standard base64 implementation for ECB mode }
function B64tobyte(const ec: RawByteString): RawByteString;
var
  k, fPos: Integer;
  i, right, left: Cardinal;
begin
  // each started block of 12 chars gives 8 bytes; allocate the result at once instead of growing it byte by byte
  SetLength(Result, ((Length(ec) + 11) div 12) * 8);
  fPos := 0;
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
       inc(fPos);
       Result[fPos] := AnsiChar(Chr((left and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8)));
     end;

     for i := 0 to 3 do
     begin
       inc(fPos);
       Result[fPos] := AnsiChar(Chr((right and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8)));
     end;
  end;
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

{ TIrcBlowkeyECB }

constructor TIrcBlowkeyECB.Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
begin
  inherited Create(aNetname, aChannel, aChanRoles, aChankey, aInviteOnly);

  UpdateKey(aBlowkey);
end;

procedure TIrcBlowkeyECB.UpdateKey(const aBlowkey: String);
var
  {$IFNDEF UNICODE}
    fStrHelper: String;
  {$ELSE}
    fKeyBytesHelper: TBytes;
  {$ENDIF}
begin
  FBlowkey := aBlowkey;

  if Blowkey <> '' then
  begin
    {$IFDEF UNICODE}
      fKeyBytesHelper := TEncoding.UTF8.GetBytes(set_key(Blowkey));
      FKeyBytes := fKeyBytesHelper;
    {$ELSE}
      fStrHelper := set_key(Blowkey);
      SetLength(FKeyBytes, Length(fStrHelper));
      move(fStrHelper[1], FKeyBytes[0], Length(fStrHelper));
    {$ENDIF}
  end
  else
  begin
    Debug(dpError, section, Format('Empty ECB blowkey for %s-%s does not make sense!', [Netname, Channel]));
    SetLength(FKeyBytes, 0);
    exit;
  end;
end;

function TIrcBlowkeyECB.EncryptMessage(const dText: String): String;
var
  fStrHelper, fResultHelper: RawByteString;
  fLength, fSplitLength: Integer;
  fCipher: TslBlowfishCipher;
  fIV: TBlowfishKeyBytes;
begin
  {$IFDEF UNICODE}
    fStrHelper := UTF8Encode(dText);
  {$ELSE}
    SetLength(fStrHelper, Length(dText));
    move(dText[1], fStrHelper[1], Length(dText));
  {$ENDIF}

  // Each message is split into blocks of 8 bytes, encrypted individually.
  // A block shorter than 8 bytes is padded with zeroes.
  fLength := Length(fStrHelper);
  fSplitLength := (fLength + 7) div 8;
  SetLength(fStrHelper, fSplitLength * 8);
  if fSplitLength * 8 > fLength then
    FillChar(fStrHelper[fLength + 1], fSplitLength * 8 - fLength, 0);

  // Encrypt all ECB blocks through one OpenSSL context.
  if fSplitLength > 0 then
  begin
    fCipher := TslBlowfishCipher.Create(FKeyBytes, fIV, sbmECB, True);
    try
      fCipher.Update(PByte(PAnsiChar(fStrHelper)), PByte(PAnsiChar(fStrHelper)),
        Length(fStrHelper));
      fCipher.Final;
    finally
      fCipher.Free;
    end;
  end;
  fResultHelper := bytetoB64(fStrHelper);

  {$IFDEF UNICODE}
    Result := UTF8ToString(fResultHelper);
  {$ELSE}
    SetLength(Result, Length(fResultHelper));
    move(fResultHelper[1], Result[1], Length(fResultHelper));
  {$ENDIF}

  Result := '+OK ' + Result;
end;

function TIrcBlowkeyECB.DecryptMessage(const eText: String): String;
var
  fStrHelper, dText: RawByteString;
  fSplitLength, fPriorNullByte: Integer;
  fCipher: TslBlowfishCipher;
  fIV: TBytes;
begin
  Result := eText;
  if Blowkey = '' then
    exit;

  dText := '';
  fSplitLength := Length(eText) div 12;

  if fSplitLength > 0 then
  begin
    {$IFDEF UNICODE}
      fStrHelper := UTF8Encode(Copy(eText, 1, fSplitLength * 12));
    {$ELSE}
      fStrHelper := Copy(eText, 1, fSplitLength * 12);
    {$ENDIF}
    dText := B64tobyte(fStrHelper);
    SetLength(dText, fSplitLength * 8);
    fCipher := TslBlowfishCipher.Create(FKeyBytes, fIV, sbmECB, False);
    try
      fCipher.Update(PByte(PAnsiChar(dText)), PByte(PAnsiChar(dText)), Length(dText));
      fCipher.Final;
    finally
      fCipher.Free;
    end;
  end;

  // remove (possible) null terminator(s) at end of string
  fPriorNullByte := Pos(#0, dText) - 1;
  if fPriorNullByte > 0 then
    SetLength(dText, fPriorNullByte)
  else
    fPriorNullByte := Length(dText);

  {$IFDEF UNICODE}
    Result := UTF8ToString(dText);
  {$ELSE}
    SetLength(Result, fPriorNullByte);
    move(dText[1], Result[1], fPriorNullByte);
  {$ENDIF}
end;

end.
