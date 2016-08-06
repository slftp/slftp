unit ircblowfish;

interface

uses Contnrs, delphiblowfish{$IFDEF MSWINDOWS},Windows{$ENDIF};
type
  TIrcBlowkey = class
  private
    KeyData: TBlowfishData;
  public
    netname: AnsiString;
    channel: AnsiString;
    blowkey: AnsiString;
    chankey: AnsiString;
    names: AnsiString; // funkcionalitasa a csatornanak
    inviteonly: Boolean;
    procedure UpdateKey(blowkey: AnsiString);
    constructor Create(netname, channel, blowkey: AnsiString; chankey: AnsiString = ''; inviteonly: Boolean = True);

    function HasKey(key: AnsiString): Boolean;
  end;

function irc_encrypt(netname, channel, dText: AnsiString; include_ok: Boolean = False): AnsiString;
function irc_decrypt(netname, channel, eText: AnsiString): AnsiString;
function irc_RegisterChannel(netname, channel, blowkey: AnsiString; chankey: AnsiString = ''; inviteonly: Boolean= False): TIrcBlowkey;
function FindIrcBlowfish(netname, channel: AnsiString; acceptdefault: Boolean = True): TIrcBlowkey;
procedure IrcblowfishInit;
procedure IrcblowfishUnInit;

var chankeys: TObjectList;

implementation

uses SysUtils, console, debugunit;

const B64: AnsiString = './0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  section: AnsiString = 'ircblowfish';


//perl compatible index, cause delphis pos works different
function PCindex(w: AnsiString): Cardinal;
begin
  Result:= Pos(w, B64);
  if Result > 0 then dec(Result);
end;

function PCsubstr(w: AnsiString; i: Integer): AnsiChar;
var s: AnsiString;
begin
  Result := #0;
  s:= Copy(w, i+1, 1);
  if (length(s) > 0) then
    Result:= s[1];
end;

function bytetoB64(ec: AnsiString): AnsiString;
var dc: AnsiString;
    left, right: Cardinal;
    i, k : Integer;
begin
  dc := '';

  k := -1;

  while(k < (length(ec)-1)) do
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
  Result:= dc;
end;

function B64tobyte(ec: AnsiString): AnsiString;
var dc: AnsiString;
    k: Integer;
    i, right, left: Cardinal;
begin
  dc:= '';
  k := -1;

  while(k < (length(ec)-1)) do
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
       dc := dc + Chr((left and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8));
     end;

     for i := 0 to 3 do
     begin
       dc := dc + Chr((right and ($FF shl ((3 - i) * 8))) shr ((3 - i) * 8));
     end;

  end;

  Result:= dc;
end;


function set_key(key: AnsiString): AnsiString;
var i, keyLen: Integer;
    newkey: AnsiString;
begin
  Result := key;
  if(length(key) < 8) then
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

function FindIrcBlowfish(netname, channel: AnsiString; acceptdefault: Boolean = True): TIrcBlowkey;
var i: Integer;
    bf: TIrcBlowkey;
begin
  if ((acceptdefault) and (chankeys.Count = 0)) then
    raise Exception.Create('No default chankey is registered');

  for i:= 0 to chankeys.Count -1 do
  begin
    bf:= TIrcBlowkey(chankeys[i]);
    if ((AnsiSameText(bf.channel, channel)) and (AnsiSameText(bf.netname, netname))) then
    begin
      Result:= bf;
      exit;
    end;
  end;
  if acceptdefault then
    Result:= chankeys[0] as TIrcBlowkey
  else
    Result:= nil;
end;

function irc_encrypt(netname, channel, dText: AnsiString; include_ok: Boolean = False): AnsiString;
var temp, eText: AnsiString;
    i: Integer;
    bf: TIrcBlowkey;
begin
  eText := '';

  if(dText<>'') then
  begin
    bf:= FindIrcBlowfish(netname, channel);
    if ((bf = nil) or (bf.blowkey = '')) then
    begin
      Result:= dText;
      exit;
    end;

    temp := '';
    if (length(dText) mod 8 > 0) then
      for i:= 1 to 8 - (length(dText) mod 8) do
        dText:= dText+ #0;

    SetLength(temp, 8);

    for i:= 1 to length(dText) div 8 do
    begin
      temp:= Copy(dText, 1+(i-1)*8,8);
      BlowfishEncryptECB(bf.KeyData, PAnsiChar(temp), PAnsiChar(temp));
      eText := eText + bytetoB64(temp);
    end;

  end;

  if include_ok then
    Result:= '+OK '+eText
  else
    Result:= eText;
end;


function irc_decrypt(netname, channel, eText: AnsiString): AnsiString;
var temp, dText: AnsiString;
    i : Integer;
    bf: TIrcBlowkey;
begin
  bf:= FindIrcBlowfish(netname, channel);
  if ((bf = nil) or (bf.blowkey = '')) then
  begin
    Result:= eText;
    exit;
  end;

  dtext:= '';
  for i:= 1 to length(eText) div 12 do
  begin
     temp := B64tobyte(Copy(eText,1+(i-1)*12,12));
     SetLength(temp, 8);
     BlowfishDecryptECB(bf.KeyData, PAnsiChar(temp), PAnsiChar(temp));
     dText := dtext + temp;
  end;

  Result:= dText;
end;


{ TIrcBlowkey }

constructor TIrcBlowkey.Create(netname, channel, blowkey: AnsiString; chankey: AnsiString = ''; inviteonly: Boolean = True);
begin
  self.channel:= channel;
  self.chankey:= chankey;
  self.netname:= netname;
  self.inviteonly:= inviteonly;
  UpdateKey(blowkey);
end;

function TIrcBlowkey.HasKey(key: AnsiString): Boolean;
begin
  Result:= False;
  key:= ' '+UpperCase(key)+' ';
  if Pos(key, names) > 0 then
    Result:= True;
end;

procedure TIrcBlowKey.UpdateKey(blowkey: AnsiString);
const
    IV: array[0..7] of byte= ($11, $22, $33, $44, $55, $66, $77, $88);
var myKey: AnsiString;
begin
  self.blowkey:= blowkey;
  if blowkey <> '' then
  begin
    myKey:= set_key(blowkey);
    SetLength(myKey, length(myKey));
    BlowfishInit(KeyData, PAnsiChar(myKey), Length(myKey), @iv);
  end
  else
    myKey:= '';
end;

procedure IrcBlowfishInit;
begin
  chankeys:= TObjectList.Create;
end;
procedure IrcBlowfishUninit;
var i: Integer;
begin
  Debug(dpSpam, section, 'Uninit1');
  for i:= 0 to chankeys.Count -1 do
    BlowfishBurn(TIrcBlowkey(chankeys[i]).KeyData);
  chankeys.Free;
  Debug(dpSpam, section, 'Uninit2');  
end;

// ezt a fuggvenyt csak irc_lock mellett szabad hivni!
function irc_RegisterChannel(netname, channel, blowkey: AnsiString; chankey: AnsiString = ''; inviteonly: Boolean= False): TIrcBlowkey;
begin
  Result:= FindIrcBlowfish(netname, channel, False);
  if nil = Result then
  begin
    console_add_ircwindow(netname+' '+channel);
    Result:= TIrcBlowkey.Create(netname, channel, blowkey, chankey, inviteonly);
    chankeys.Add(Result);
  end;
end;

end.
