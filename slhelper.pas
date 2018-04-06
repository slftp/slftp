unit slhelper;

interface


function Elsosor(var osszes: String): String;
function IsIP(AIP: String): boolean;
function TInAddrToString(var AInAddr): String;
procedure TranslateStringToTInAddr(AIP: String; var AInAddr);
function FetchSL(var osszes: String; const Args: array of Char): String;

implementation

uses
  SysUtils, IdGlobal,
{$IFDEF FPC}
  sockets
{$ELSE}
  {$IFDEF MSWINDOWS}
    slWinSock2
  {$ELSE}
    Libc
  {$ENDIF}
{$ENDIF}
;


function FetchSL(var osszes: String; const Args: array of Char): String;
var elso, utolso: Integer;
    i,j: Integer;
    megvolt: Boolean;
begin
  elso:= 0;
  utolso:= 0;
  for i:= 1 to length(osszes) do
  begin
    megvolt:= False;
    for j:= Low(Args) to High(Args) do
      if osszes[i] = Args[j] then
      begin
        if elso = 0 then
          elso:= i;
        utolso:= i;
        megvolt:= True;
        Break;
      end;

    if not megvolt then
      if utolso <> 0 then Break;
  end;

  if (elso = 0) or (utolso = 0) then
  begin
    Result:= osszes;
    osszes:= '';
    exit;
  end;

  Result:= Copy(osszes, 1, elso-1);
  Delete(osszes, 1, utolso);
end;

function Elsosor(var osszes: String): String;
begin
  Result:= FetchSL(osszes, [#13,#10]);
end;

function IsIP(AIP: String): boolean;
var
  s1, s2, s3, s4: String;

  function ByteIsOk(const AByte: String): boolean;
  begin
    result := (StrToIntDef(AByte, -1) > -1) and (StrToIntDef(AByte, 256) < 256);
  end;

begin
  s1 := Fetch(AIP, '.', True, False);    {Do not Localize}
  s2 := Fetch(AIP, '.', True, False);    {Do not Localize}
  s3 := Fetch(AIP, '.', True, False);    {Do not Localize}
  s4 := AIP;
  result := ByteIsOk(s1) and ByteIsOk(s2) and ByteIsOk(s3) and ByteIsOk(s4);
end;

{$IFDEF FPC}
function TInAddrToString(var AInAddr): String;
begin
  with TInAddr(AInAddr) do begin
    result := IntToStr(s_bytes[1]) + '.' + IntToStr(s_bytes[2]) + '.' + IntToStr(s_bytes[3]) + '.'    {Do not Localize}
     + IntToStr(s_bytes[4]);
  end;
end;
{$ELSE}
function TInAddrToString(var AInAddr): String;
begin
  with TInAddr(AInAddr).S_un_b do begin
    result := IntToStr(s_b1) + '.' + IntToStr(s_b2) + '.' + IntToStr(s_b3) + '.'    {Do not Localize}
     + IntToStr(s_b4);
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TranslateStringToTInAddr(AIP: String; var AInAddr);
begin
  with TInAddr(AInAddr) do
  begin
    s_bytes[1] := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_bytes[2] := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_bytes[3] := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_bytes[4] := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
  end;
end;
{$ELSE}
procedure TranslateStringToTInAddr(AIP: String; var AInAddr);
begin
  with TInAddr(AInAddr).S_un_b do begin
    s_b1 := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_b2 := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_b3 := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
    s_b4 := Byte(StrToInt(Fetch(AIP, '.', True, False)));    {Do not Localize}
  end;
end;
{$ENDIF}

end.
