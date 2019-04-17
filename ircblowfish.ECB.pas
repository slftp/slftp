unit ircblowfish.ECB;

interface

uses
  ircchansettings, delphiblowfish;

type
  TIrcBlowkeyECB = class(TIrcChannelSettings)
  private
    FKeyData: TBlowfishData; //< generated blowfish data for @link(blowkey)
    FBlowkey: String; //< blowkey for channel
  public
    { Creates a new IrcBlowkey entry }
    constructor Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
    { Burns the IrcBlowkey @link(FKeyData) data }
    destructor Destroy; override;
    { Generates TBlowfishData for given irc blowkey
      @param(aBlowkey generates @link(FKeyData) data for @link(aBlowkey)) }
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
  SysUtils, debugunit;

const
  B64: RawByteString = './0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  section = 'ircblowfish.ECB';

{ functions for ECB de-/encryption }

// perl compatible index, cause delphis pos works different
function PCindex(const w: RawByteString): Cardinal;
begin
  Result := Pos(w, B64);
  if Result > 0 then dec(Result);
end;

function PCsubstr(const w: RawByteString; const i: Integer): AnsiChar;
var
  s: RawByteString;
begin
  Result := #0;
  s := Copy(w, i + 1, 1);
  if (length(s) > 0) then
    Result := s[1];
end;

{ non-standard base64 implementation for ECB mode }
function bytetoB64(const ec: RawByteString): RawByteString;
var
  dc: RawByteString;
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
function B64tobyte(const ec: RawByteString): RawByteString;
var
  dc: RawByteString;
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

{ TIrcBlowkeyECB }

constructor TIrcBlowkeyECB.Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
begin
  inherited Create(aNetname, aChannel, aChanRoles, aChankey, aInviteOnly);

  UpdateKey(aBlowkey);
end;

destructor TIrcBlowkeyECB.Destroy;
begin
  BlowfishBurn(FKeyData);
  inherited;
end;

procedure TIrcBlowkeyECB.UpdateKey(const aBlowkey: String);
const
  IV: array[0..7] of byte = ($11, $22, $33, $44, $55, $66, $77, $88);
var
  fKey: TBytes;
  {$IFNDEF UNICODE}
    fStrHelper: String;
  {$ENDIF}
begin
  FBlowkey := aBlowkey;

  if Blowkey <> '' then
  begin
    {$IFDEF UNICODE}
      fKey := TEncoding.UTF8.GetBytes(set_key(Blowkey));
    {$ELSE}
      fStrHelper := set_key(Blowkey);
      SetLength(fKey, Length(fStrHelper));
      move(fStrHelper[1], fKey[0], Length(fStrHelper));
    {$ENDIF}
    SetLength(fKey, Length(fKey));
    BlowfishInit(FKeyData, PAnsiChar(fKey), Length(fKey), @IV);
  end
  else
  begin
    Debug(dpError, section, 'Empty ECB blowkey does not make sense!');
    exit;
  end;
end;

function TIrcBlowkeyECB.EncryptMessage(const dText: String): String;
var
  fStrHelper, fResultHelper, temp: RawByteString;
  i: Integer;
begin
  fStrHelper := UTF8Encode(dText);
  fResultHelper := '';

  // Each message is split into blocks of 8 bytes, encrypted individually.
  // A block shorter than 8 bytes is padded with zeroes.
  if (length(fStrHelper) mod 8 > 0) then
    for i := 1 to 8 - (length(fStrHelper) mod 8) do
      fStrHelper := fStrHelper + #0;

  SetLength(temp, 8);

  for i := 1 to length(fStrHelper) div 8 do
  begin
    temp := Copy(fStrHelper, 1 + (i - 1) * 8, 8);
    BlowfishEncryptECB(FKeyData, PAnsiChar(temp), PAnsiChar(temp));
    fResultHelper := fResultHelper + bytetoB64(temp);
  end;

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
  fStrHelper, temp, dText: RawByteString;
  i, fSplitLength, fPriorNullByte: Integer;
begin
  Result := eText;
  if Blowkey = '' then
    exit;

  dText := '';
  fSplitLength := Length(eText) div 12;

  for i := 1 to fSplitLength do
  begin
    fStrHelper := UTF8Encode(Copy(eText, 1 + (i - 1) * 12, 12));
    temp := B64tobyte(fStrHelper);
    SetLength(temp, 8);
    BlowfishDecryptECB(FKeyData, PAnsiChar(temp), PAnsiChar(temp));
    dText := dText + temp;
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
