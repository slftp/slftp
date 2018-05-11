unit ircblowfish;

interface

uses
  Contnrs, delphiblowfish
  {$IFDEF MSWINDOWS}
    ,Windows
  {$ENDIF};

type
  TIrcBlowkey = class
  private
    KeyData: TBlowfishData;
  public
    netname: String; //< { netname of IRC network }
    channel: String; //< { IRC channelname }
    blowkey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; //< { blowkey for @value(channel) }
    chankey: String; //< { chankey for @value(channel) which is needed to join it }
    names: String; // funkcionalitasa a csatornanak - Functionality of the channel
    cbc: Boolean; //< { @true if channel is CBC encrypted, @false otherwise. }
    inviteonly: Boolean; //< { @true if channel is invite only (you have to invite yourself first), @false otherwise. }
    procedure UpdateKey(const blowkey: String);

    { Creates a new IrcBlowkey entry }
    constructor Create(const netname, channel, blowkey: String; chankey: String = ''; inviteonly: Boolean = True; cbc: Boolean = False);
    function HasKey(key: String): Boolean;
  end;

function irc_encrypt(netname, channel, dText: String; include_ok: Boolean = False): String;
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
  SysUtils, console, debugunit;

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

function irc_encrypt(netname, channel, dText: String; include_ok: Boolean = False): String;
var
  temp, eText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  i: Integer;
  bf: TIrcBlowkey;
begin
  Result := '';
  eText := '';

  if (dText = '') then
    exit;

  bf := FindIrcBlowfish(netname, channel);
  if ((bf = nil) or (bf.blowkey = '')) then
  begin
    Result := dText;
    exit;
  end;

  temp := '';
  if (length(dText) mod 8 > 0) then
    for i := 1 to 8 - (length(dText) mod 8) do
      dText:= dText+ #0;

  SetLength(temp, 8);

  for i := 1 to length(dText) div 8 do
  begin
    temp := Copy(dText, 1+(i-1)*8,8);
    if bf.cbc then
      BlowfishEncryptCBC(bf.KeyData, PAnsiChar(temp), PAnsiChar(temp))
    else
      BlowfishEncryptECB(bf.KeyData, PAnsiChar(temp), PAnsiChar(temp));

    eText := eText + bytetoB64(temp);
  end;

  if bf.cbc then
    eText := '*' + eText + '==';

  if include_ok then
    Result := UTF8Decode('+OK ' + eText)
  else
    Result := UTF8Decode(eText);
end;



function irc_ecb_decrypt(const netname, channel, eText: String): String;
var
  temp, dText: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  i : Integer;
  bf: TIrcBlowkey;
begin
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
  self.cbc := cbc;
  UpdateKey(blowkey);
end;

function TIrcBlowkey.HasKey(key: String): Boolean;
begin
  Result := False;
  key := ' ' + UpperCase(key) + ' ';
  if Pos(key, names) > 0 then
    Result := True;
end;

procedure TIrcBlowKey.UpdateKey(const blowkey: String);
const
  IV: array[0..7] of byte = ($11, $22, $33, $44, $55, $66, $77, $88);
var
  myKey: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
begin
  self.blowkey := {$IFDEF UNICODE}UTF8Encode(blowkey){$ELSE}blowkey{$ENDIF};
  if blowkey <> '' then
  begin
    myKey := {$IFDEF UNICODE}set_key(UTF8Encode(blowkey)){$ELSE}set_key(blowkey){$ENDIF};
    SetLength(myKey, length(myKey));
    BlowfishInit(KeyData, PAnsiChar(myKey), Length(myKey), @iv);
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
