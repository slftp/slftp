unit slmd5;

interface

type
  TslMD5Data = array[0..15] of Byte;

function slMD5DigestToStr(v: TslMD5Data): String;
function slMD5String(const s: String): TslMD5Data;

implementation

uses
  SysUtils,
  {$IFDEF FPC}
    md5
  {$ELSE}
    mormot.crypt.core
  {$ENDIF};

function slMD5DigestToStr(v: TslMD5Data): String;
begin
  {$IFDEF FPC}
    Result := UpperCase(MD5Print(TMDDigest(v)));
  {$ELSE}
    Result := UpperCase(String(Md5DigestToString(TMd5Digest(v))));
  {$ENDIF}
end;

function slMD5String(const s: String): TslMD5Data;
{$IFNDEF FPC}
var
  RawPassphrase: RawByteString;
  Digest: TMd5Digest;
  MD5: TMd5;
{$ENDIF}
begin
  {$IFDEF FPC}
    Result := TslMD5Data(MD5String(s));
  {$ELSE}
    RawPassphrase := RawByteString(s);
    MD5.Full(Pointer(RawPassphrase), Length(RawPassphrase), Digest);
    Move(Digest, Result[0], SizeOf(Result));
  {$ENDIF}
end;

end.
