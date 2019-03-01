unit mycrypto;

interface

uses
  Classes, slmd5;

procedure MyCryptoInit;
procedure MycryptoStart(pp: TslMD5Data);
procedure MycryptoStop;
function DecryptUDP(const s: String): String;
function EncryptUDP(const s: String): String;

implementation

uses
  SysUtils, delphiblowfish, slssl, helper, configunit, debugunit, Math, mystrings;

const
  section = 'crypto';
  UDP_MIN_PADDING = 8;
  UDP_MAX_PADDING = 32;
  MAX_UDP_PACKET = 16384;

var
  KeyData: TBlowfishData;

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

end.