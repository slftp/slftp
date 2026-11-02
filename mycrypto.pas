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
  SysUtils, configunit, debugunit, Math, mystrings, slblowfishcipher, blowfishdiag;

const
  section = 'crypto';
  UDP_MIN_PADDING = 8;
  UDP_MAX_PADDING = 32;
  MAX_UDP_PACKET = 16384;

var
  glBlowfishKey: TBytes;

procedure MyCryptoInit;
begin
  Randomize;
end;

procedure MycryptoStart(pp: TslMD5Data);
begin
  SetLength(glBlowfishKey, SizeOf(pp));
  Move(pp, glBlowfishKey[0], SizeOf(pp));
end;

procedure MycryptoStop;
begin
  if Length(glBlowfishKey) > 0 then
    FillChar(glBlowfishKey[0], Length(glBlowfishKey), 0);
  SetLength(glBlowfishKey, 0);
end;

function DecryptUDP(const s: String): String;
var
  p: Byte;
  l: Integer;
  fCipher: TslBlowfishCipher;
  fIV: array[0..7] of Byte;
begin
  Result := '';

  l := length(s);
  if l > MAX_UDP_PACKET then
    exit;

  RecordBlowfishDiagnosticUse(bdmUdpCfbDecrypt, Length(glBlowfishKey), l);

  FillChar(fIV, SizeOf(fIV), 0);
  fCipher := TslBlowfishCipher.Create(glBlowfishKey, fIV, sbmCFB, False);
  try
    fCipher.Update(PByte(@s[1]), PByte(@s[1]), l);
    fCipher.Final;
  finally
    fCipher.Free;
  end;

  p := Ord(s[1]);
  if (p >= UDP_MIN_PADDING) and (p <= UDP_MAX_PADDING) then
    Result := Copy(s, p + 1, l - p - 1);
end;

function EncryptUDP(const s: String): String;
var
  p: Byte;
  block: array[0..MAX_UDP_PACKET-1] of AnsiChar;
  fCipher: TslBlowfishCipher;
  fIV: array[0..7] of Byte;
begin
  Result := '';

  if Length(s) + UDP_MAX_PADDING > MAX_UDP_PACKET then
    exit;

  p := Byte(RandomRange(UDP_MIN_PADDING, UDP_MAX_PADDING));
  RecordBlowfishDiagnosticUse(bdmUdpCfbEncrypt, Length(glBlowfishKey), p + 1 + Length(s));
  block[0] := AnsiChar(p);
  Move(s[1], block[p], length(s));

  FillChar(fIV, SizeOf(fIV), 0);
  fCipher := TslBlowfishCipher.Create(glBlowfishKey, fIV, sbmCFB, True);
  try
    fCipher.Update(PByte(@block[0]), PByte(@block[0]), p + 1 + Length(s));
    fCipher.Final;
  finally
    fCipher.Free;
  end;
  SetLength(Result, p + Length(s) + 1);
  Move(block[0], Result[1], p + 1 + Length(s));
end;

end.
