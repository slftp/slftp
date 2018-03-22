unit slhelper;

interface


function Elsosor(var osszes: AnsiString): AnsiString;
function IsIP(AIP: AnsiString): boolean;
function TInAddrToString(var AInAddr): AnsiString;
procedure TranslateStringToTInAddr(AIP: AnsiString; var AInAddr);
function Fetch(var osszes: AnsiString; const Args: array of AnsiChar): AnsiString; overload;
function Fetch(var osszes: AnsiString; sep: AnsiChar): AnsiString; overload;
function UrlEncode(const DecodedStr: AnsiString; Pluses: Boolean): AnsiString; overload;
function UrlEncode(const DecodedStr: AnsiString): AnsiString; overload;

implementation

uses
  SysUtils,
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


function Fetch(var osszes: AnsiString; const Args: array of AnsiChar): AnsiString;
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

function Fetch(var osszes: AnsiString; sep: AnsiChar): AnsiString;
begin
  Result:= Fetch(osszes, [sep]);
end;

function Elsosor(var osszes: AnsiString): AnsiString;
begin
  Result:= Fetch(osszes, [#13,#10]);
end;

(*
function Fetch(var s: string; delim: string): string;
var i: Integer;
begin
  Result:= s;
  i:= Pos(delim, s);
  if i > 0 then
  begin
    Result:= Copy(s, 1, i-1);
    Delete(s, 1, i-1+length(delim));
  end;
end;
*)

function IsIP(AIP: AnsiString): boolean;
var
  s1, s2, s3, s4: AnsiString;

  function ByteIsOk(const AByte: AnsiString): boolean;
  begin
    result := (StrToIntDef(AByte, -1) > -1) and (StrToIntDef(AByte, 256) < 256);
  end;

begin
  s1 := Fetch(AIP, '.');    {Do not Localize}
  s2 := Fetch(AIP, '.');    {Do not Localize}
  s3 := Fetch(AIP, '.');    {Do not Localize}
  s4 := AIP;
  result := ByteIsOk(s1) and ByteIsOk(s2) and ByteIsOk(s3) and ByteIsOk(s4);
end;

{$IFDEF FPC}
function TInAddrToString(var AInAddr): AnsiString;
begin
  with TInAddr(AInAddr) do begin
    result := IntToStr(s_bytes[1]) + '.' + IntToStr(s_bytes[2]) + '.' + IntToStr(s_bytes[3]) + '.'    {Do not Localize}
     + IntToStr(s_bytes[4]);
  end;
end;
{$ELSE}
function TInAddrToString(var AInAddr): AnsiString;
begin
  with TInAddr(AInAddr).S_un_b do begin
    result := IntToStr(s_b1) + '.' + IntToStr(s_b2) + '.' + IntToStr(s_b3) + '.'    {Do not Localize}
     + IntToStr(s_b4);
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TranslateStringToTInAddr(AIP: AnsiString; var AInAddr);
begin
  with TInAddr(AInAddr) do
  begin
    s_bytes[1] := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_bytes[2] := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_bytes[3] := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_bytes[4] := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
  end;
end;
{$ELSE}
procedure TranslateStringToTInAddr(AIP: AnsiString; var AInAddr);
begin
  with TInAddr(AInAddr).S_un_b do begin
    s_b1 := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_b2 := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_b3 := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
    s_b4 := Byte(StrToInt(Fetch(AIP, '.')));    {Do not Localize}
  end;
end;
{$ENDIF}


(*
function IsIP(s: string): Boolean;
var i, j: Integer;
    aktszam: string;
    db: Integer;
    szamdb: Integer;
begin
  Result:= False;

  aktszam:= '';
  db:= 0;
  szamdb:= 0;
  for i:= 1 to length(s) do
  begin
    if (s[i] = '.') then
    begin
      j:= StrToIntDef(aktszam, -1);
      if (j > 255) or (j < 0) then exit;

      inc(szamdb);
      db:= 0;
      aktszam:= '';
    end
    else
    if (s[i] in ['0'..'9']) then
    begin
      aktszam:= aktszam + s[i];
      inc(db);

      if db > 3 then exit;

    end else
      exit;
  end;

  if szamdb <> 3 then exit;

  j:= StrToIntDef(aktszam, -1);
  if (j > 255) or (j < 0) then exit;

  Result:= True;
end;
*)


function UrlEncode(const DecodedStr: AnsiString; Pluses: Boolean): AnsiString;
var
  I: Integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z','A'..'Z', ' ','+']) then
        Result := Result + '%' + IntToHex(Ord(DecodedStr[I]), 2)
      else if not (DecodedStr[I] = ' ') then
        Result := Result + DecodedStr[I]
      else
        begin
          if not Pluses then
            Result := Result + '%20'
          else
            Result := Result + '+';
        end;
    end;
end;

function UrlEncode(const DecodedStr: AnsiString): AnsiString;
begin
  Result:= URLEncode(DecodedStr, True);
end;


end.
