unit slmd5;

interface

uses
{$IFDEF FPC}
  md5
{$ELSE}
  delphimd5
{$ENDIF}
 ;


type
  TslMD5Data = array[0..15] of Byte;


function slMD5DigestToStr(v: TslMD5Data): string;
function slMD5String(const s: string): TslMD5Data;

implementation

uses SysUtils;


function slMD5DigestToStr(v: TslMD5Data): string;
begin
{$IFDEF FPC}
  Result:= UpperCase( MD5Print(TMDDigest(v)) );
{$ELSE}
  Result:= MD5DigestToStr(TMD5Digest(v));
{$ENDIF}
end;
function slMD5String(const s: string): TslMD5Data;
begin
  Result:= TslMD5Data(MD5String(s));
end;


end.
