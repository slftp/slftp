{*****************************************************************************

 - Soulless robotic engine aka SLFTP
 - Version 1.3

 - Remarks:          Freeware, Copyright must be included

 - Original Author:  believe

 - Modifications:    aKRAUT aka dOH

 - Last change:      27/06/2010 - Added DateAsString(secs) give a pretime known result

 - Description:      Just a copy of the some std. delphi strings i guess........


 ****************************************************************************

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS       *
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE       *
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE        *
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR      *
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF     *
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,    *
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE     *
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,        *
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       *

*****************************************************************************}

unit mystrings;

interface

uses Classes;

{ Remove all non/special characters from String
  @param(S String which should be cleaned)
  @returns(String which only contains characters as [a-z] or [A-Z]) }
function onlyEnglishAlpha(const S: String): String;

function DateTimeAsString(const aThen: TDateTime; padded: boolean = False): String;

function LeftStr(const Source: String; Count: integer): String;
function RightStr(const Source: String; Count: integer): String;
function SubString(const s, seperator: String; index: integer): String;
function Csere(const Source, old, new: String): String;
function Count(const mi, miben: String): integer;
function RPos(const SubStr: Char; const Str: String): integer;
function MyStrToTime(const x: String): TDateTime;
function MyDateToStr(x: TDateTime): String;
function MyStrToDate(x: String): TDateTime;
function myStrToFloat(s: String): double; overload;
function myStrToFloat(s: String; def: double): double; overload;
function MyCopy(b: array of byte; index, len: integer): String;
function ParseResponseCode(s: String): integer;

{ Adds '/' to end of dir if it's missing or returns '/' if empty string
  @param(s input dir)
  @returns(Input dir with appended '/' if it was missing or just '/' if empty string) }
function MyIncludeTrailingSlash(const s: String): String;

{ extracts data from ftpd PASV reply
  @param(s (ip,ip,ip,ip,port,port) reply)
  @param(host extracted host IP)
  @param(port extracted port)
  @returns(@true if host and port successful extracted, @false otherwise) }
function ParsePASVString(s: String; out host: String; out port: integer): boolean;

{ extracts data from ftpd EPSV reply
  @param(s (|mode|ip.ip.ip.ip|port|) reply (| is a variable delimiter))
  @param(host extracted host IP)
  @param(port extracted port)
  @param(IPv4Transfermode @true if IP adress is IPv4, @false if IPv6)
  @returns(@true if host, port and transfermode successful extracted, @false otherwise) }
function ParseEPSVString(s: String; out host: String; out port: integer; out IPv4Transfermode: boolean): boolean;

{ checks if c is a letter (case-insensitive)
  @param(c Character which should be checked)
  @returns(@true if it's a letter: [a-z] or [A-Z], @false otherwise) }
function IsALetter(const c: Char): boolean;
{ checks if c is a number
  @param(c Character which should be checked)
  @returns(@true if it's a number: [0-9], @false otherwise) }
function IsANumber(const c: Char): boolean;

{ Counts the number of occurrences of numbers in String
  @param(S String which should be used to search in)
  @returns(Count of occurrences of numbers) }
function OccurrencesOfNumbers(const S: string): Integer;

function GetFileContents(const fn: String): String;
function FetchSL(var osszes: String; const Args: array of Char): String;
function Elsosor(var osszes: String): String;
function todaycsere(const s: String; datum: TDateTime = 0): String;

procedure splitString(const Source: String; const Delimiter: String; const Dest: TStringList);

implementation

uses
  SysUtils, Math
  {$IFDEF MSWINDOWS}
    , registry, Windows
  {$ENDIF}
  , DateUtils, IdGlobal;

function Count(const mi, miben: String): integer;
var
  s: String;
  i: integer;
begin
  s      := '';
  Result := 0;
  for i := 1 to length(miben) do
  begin
    s := s + miben[i];
    if 0 < Pos(mi, s) then
    begin
      Inc(Result);
      s := '';
    end;
  end;
end;

function LeftStr(const Source: String; Count: integer): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + Source[i];
end;

function RightStr(const Source: String; Count: integer): String;
var
  i: integer;
begin
  Result := '';
  for i := Count + 1 to length(Source) do
    Result := Result + Source[i];
end;


(*
function SubString (const s, seperator: string; index: integer): string;
var ok: boolean;
    i: integer;
    szamlalo: integer;
begin
  ok:= false; i:= 0; Result:= ''; szamlalo:= 0;

  while not ok do
  begin
    if i = length(s) then ok:= True
    else
    begin
      inc (i);
      Result:= Result + s[i];
      if ((length(Result) - length(Seperator)+1) <> 0) and (length(Result) - length(Seperator) +1 = Pos (Seperator, Result)) then //szoveg vegen
      begin
        inc (szamlalo);
        if szamlalo = index then
        begin
          ok:= True;
          Delete (Result, (length(Result) - length(Seperator)+1),length(Seperator));
        end else
          Result:= '';
      end;
    end;
  end;

end;
*)

function SubString(const s, seperator: String; index: integer): String;
var
  akts: String;
  sz:   integer;
  i, l: integer;
begin
  akts   := s;
  sz     := 0;
  Result := '';
  l      := length(seperator);
  repeat
    i := Pos(seperator, akts);
    if i <> 0 then
    begin
      if sz + 1 = index then
      begin
        // ezt kerestuk
        Result := Copy(akts, 1, i - 1);
        exit;
      end;

      akts := Copy(akts, i + l, 100000);
      Inc(sz);
    end
    else
    begin
      // nincs tobb talalat.
      if sz + 1 = index then
        Result := akts;
      exit;
    end;
  until False;
end;

function Csere(const Source, old, new: String): String;
begin
  Result := StringReplace(Source, old, new, [rfReplaceAll, rfIgnoreCase]);
end;

function MyDateToStr(x: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', x);
end;

function MyStrToTime(const x: String): TDateTime;
var
  h, m: integer;
begin
  h      := StrToIntDef(Copy(x, 1, 2), 0);
  m      := StrToIntDef(Copy(x, 4, 2), 0);
  Result := EncodeTime(h, m, 0, 0);
end;

function MyStrToDate(x: String): TDateTime;
var
  y, m, d, h, mm, s: integer;
begin
  y  := StrToIntDef(Copy(x, 1, 4), 0);
  m  := StrToIntDef(Copy(x, 6, 2), 0);
  d  := StrToIntDef(Copy(x, 9, 2), 0);
  h  := StrToIntDef(Copy(x, 12, 2), 0);
  mm := StrToIntDef(Copy(x, 15, 2), 0);
  s  := StrToIntDef(Copy(x, 18, 2), 0);
  if not TryEncodeDateTime(y, m, d, h, mm, s, 0, Result) then
    Result := 0;
end;

function myStrToFloat(s: String; def: double): double;
var
  x: String;
  d: integer;
  e: integer;
begin
  Result := def;
  if s = '' then
    exit;
  s := Csere(s, ',', '.');
  d := Count('.', s);
  if (d <= 1) then
  begin
    Result := StrToIntDef(SubString(s, '.', 1), 0);
    if d = 1 then
    begin
      x := SubString(s, '.', 2);
      if Result < 0 then
        e := -1
      else
        e := 1;
      Result := Result + e * StrToIntDef(x, 0) / Power(10, length(x));
    end;
  end;
end;

function myStrToFloat(s: String): double;
begin
  Result := myStrToFloat(s, -1);
end;

function MyCopy(b: array of byte; index, len: integer): String;
var
  i: integer;
begin
  Result := '';
  for i := index to index + len - 1 do
    Result := Result + Chr(b[i]);
end;

function RPos(const SubStr: Char; const Str: String): integer;
var
  m, i: integer;
begin
  Result := 0;
  m      := length(Str);
  for i := m downto 1 do
    if Str[i] = SubStr then
    begin
      Result := i;
      exit;
    end;
end;

function RTrimCRLF(const s: String): String;
var
  db, i, j: integer;
begin
  j  := length(s);
  db := 0;
  for i := j downto 1 do
  begin
    if not (s[i] in [#13, #10]) then
      break;
    Inc(db);
  end;
  Result := Copy(s, 1, j - db);
end;

function ParseResponseCode(s: String): integer;
var
  p, l: integer;
begin
  Result := 0;
  s      := RTrimCRLF(s);
  p      := RPos(#13, s);
  l      := length(s);
  if (p <= l - 3) then
  begin
    Inc(p);
    if (s[p] in [#13, #10]) then
      Inc(p);

    Result := StrToIntDef(Copy(s, p, 3), 0);
    if ((l > 3) and (s[p + 3] <> ' ')) then
      Inc(Result, 1000);// and (p + 3 <= l)
  end;
end;

function MyIncludeTrailingSlash(const s: String): String;
var
  fLength: Integer;
begin
  fLength := Length(s);
  if fLength > 0 then
  begin
    Result := s;
    if Result[fLength] <> '/' then
      Result := Result + '/';
  end
  else
    Result := '/';
end;

function ParsePASVString(s: String; out host: String; out port: integer): boolean;
begin
  Result := False;

  {
  * PASV
  * 227 Entering Passive Mode (ip,ip,ip,ip,port,port)
  }
  s := Copy(s, Pos('(', s) + 1, 100000);
  s := Copy(s, 1, Pos(')', s) - 1);
  if s = '' then
    exit;

  host := Fetch(s, ',', True, False) + '.' + Fetch(s, ',', True, False) + '.' + Fetch(s, ',', True, False) + '.' + Fetch(s, ',', True, False);
  if s = '' then
    exit;

  port := StrToIntDef(Fetch(s, ',', True, False), 0) * 256 + StrToIntDef(Fetch(s, ',', True, False), 0);
  if port = 0 then
    exit;

  Result := True;
end;

// TODO: Implement based on FEAT response, as we need to know if it's supported by site before using it
// for more infos see: https://glftpd.eu/beta/docs/README.IPv6
function ParseEPSVString(s: String; out host: String; out port: integer; out IPv4Transfermode: boolean): boolean;
var
  helper, delimiter: String;
  ipdots: Char;
begin
  Result := False;
  ipdots := '.';

  {
  * CEPR on
  * 200 Custom Extended Passive Reply enabled
  * EPSV
  * 229 Entering Extended Passive Mode (|mode|ip.ip.ip.ip|port|)
  }
  s := Copy(s, Pos('(', s) + 2, 100000);
  s := Copy(s, 1, Pos(')', s) - 2);
  if s = '' then
    exit;

  // s is now: mode|ip.ip.ip.ip|port
  delimiter := s[2]; // delimiter in the range of ASCII 33-126

  // protocol family as defined by IANA (1 for IPv4, 2 for IPv6)
  IPv4Transfermode := Boolean(StrToInt(s[1]) = 1);
  if not IPv4Transfermode then
  begin
    ipdots := ':';
  end;

  helper := Copy(s, Pos(delimiter, s) + 1, Pos(delimiter, s) - 1);
  host := Fetch(helper, ipdots, True, False) + ipdots + Fetch(helper, ipdots, True, False) + ipdots + Fetch(helper, ipdots, True, False) + ipdots + Fetch(helper, ipdots, True, False);

  port := StrToIntDef(Copy(s, LastDelimiter(delimiter, s) + 1, 100000), 0);
  if port = 0 then
    exit;

  Result := True;
end;

function IsALetter(const c: Char): boolean;
begin
  Result := (((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')));
end;

function IsANumber(const c: Char): boolean;
begin
  Result := ((c >= '0') and (c <= '9'));
end;

function OccurrencesOfNumbers(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
  begin
    if IsANumber(s[i]) then
      Inc(Result);
  end;
end;

function GetFileContents(const fn: String): String;
var
  x: TextFile;
  s: String;
begin
  if FileExists(fn) then
  begin
    Result := '';
    AssignFile(x, fn);
    Reset(x);
    while not EOF(x) do
    begin
      ReadLn(x, s);
      Result := Result + s + #13#10;
    end;

  end
  else
    Result := '';
end;

function FetchSL(var osszes: String; const Args: array of Char): String;
var
  elso, utolso: integer;
  i, j:    integer;
  megvolt: boolean;
begin
  elso   := 0;
  utolso := 0;
  for i := 1 to length(osszes) do
  begin
    megvolt := False;
    for j := Low(Args) to High(Args) do
      if osszes[i] = Args[j] then
      begin
        if elso = 0 then
          elso := i;
        utolso := i;
        megvolt := True;
        Break;
      end;

    if not megvolt then
      if utolso <> 0 then
        Break;
  end;

  if (elso = 0) or (utolso = 0) then
  begin
    Result := osszes;
    osszes := '';
    exit;
  end;

  Result := Copy(osszes, 1, elso - 1);
  Delete(osszes, 1, utolso);
end;

function Elsosor(var osszes: String): String;
begin
  Result := FetchSL(osszes, [#13, #10]);
end;


function todaycsere(const s: String; datum: TDateTime = 0): String;
var
  yyyy, yy, mm, dd, ww: String;
begin
  if datum = 0 then
    datum := Now();

  yyyy := Format('%.4d', [YearOf(datum)]);
  yy   := Copy(yyyy, 3, 2);
  mm   := Format('%.2d', [MonthOf(datum)]);
  dd   := Format('%.2d', [DayOf(datum)]);
  ww   := Format('%.2d', [WeekOf(datum)]);


  Result := s;
  Result := Csere(Result, '<yyyy>', yyyy);
  Result := Csere(Result, '<yy>', yy);
  Result := Csere(Result, '<mm>', mm);
  Result := Csere(Result, '<dd>', dd);
  Result := Csere(Result, '<ww>', ww);
end;

{$WARNINGS OFF}
function DateTimeAsString(const aThen: TDateTime; padded: boolean = False): String;
var
  i, seci, mini, houri, dayi, weeki, monthi, yeari:    int64;
  imsg, secs, mins, hours, days, weeks, months, years: String;
begin
  Result := '-1';
  if (aThen = 0) then
    exit;

  seci   := SecondsBetween(now, aThen);
  mini   := MinutesBetween(now, aThen);
  houri  := HoursBetween(now, aThen);
  dayi   := DaysBetween(now, aThen);
  weeki  := WeeksBetween(now, aThen);
  monthi := MonthsBetween(now, aThen);
  //yeari:=YearsBetween(now,aThen);
  try
    //sec
    if seci >= 60 then
    begin
      mini := seci div 60;
      seci := seci - (mini * 60);
    end
    else
      seci := seci;
    //min
    if mini > 60 then
    begin
      houri := mini div 60;
      mini  := mini - (houri * 60);
    end
    else
      mini := mini;
    if mini = 60 then
      mini := 0;

    //hour
    if houri > 24 then
    begin
      dayi  := houri div 24;
      houri := houri - (dayi * 24);
    end
    else
      houri := houri;
    if houri = 24 then
      houri := 0;

    //day
(*
if dayi > 7 then begin
weeki:=dayi div 7;
dayi:=dayi-(weeki * 7);
end else dayi:=dayi;
*)
    if dayi > 7 then
    begin
      weeki := dayi div 7;
      i     := weeki * 7;
      if dayi <> i then
        dayi := dayi - (weeki * 7)
      else
        dayi := 0;
    end
    else
      dayi := dayi;
    if dayi = 7 then
      dayi := 0;

    //week
    if weeki > 4 then
    begin
      monthi := weeki div 4;
      i      := monthi * 4;
      if weeki <> i then
        weeki := weeki - (monthi * 4)
      else
        weeki := 0;
    end
    else
      weeki := weeki;
    if weeki = 4 then
      weeki := 0;

    //month
    if monthi >= 12 then
    begin
      yeari  := monthi div 12;
      monthi := monthi - (yeari * 12);
    end
    else
      monthi := monthi;
    if monthi = 12 then
      monthi := 0;
    //year
    yeari := monthi div 12;

    if padded then
    begin
      if seci = 1 then
        secs := Format('%2d second', [seci])
      else
        secs := Format('%2d seconds', [seci]);
      if mini = 1 then
        mins := Format('%2d minute ', [mini])
      else
        mins := Format('%2d minutes ', [mini]);
      if houri = 1 then
        hours := Format('%2d hour ', [houri])
      else
        hours := Format('%2d hours ', [houri]);
      if dayi = 1 then
        days := Format('%d day ', [dayi])
      else
        days := Format('%d days ', [dayi]);
      if weeki = 1 then
        weeks := Format('%d week ', [weeki])
      else
        weeks := Format('%d weeks ', [weeki]);
      if monthi = 1 then
        months := Format('%2d month ', [monthi])
      else
        months := Format('%2d months ', [monthi]);
      if yeari = 1 then
        years := Format('%d year ', [yeari])
      else
        years := Format('%d years ', [yeari]);
    end
    else
    begin
      if seci = 1 then
        secs := Format('%d second', [seci])
      else
        secs := Format('%2d seconds', [seci]);
      if mini = 1 then
        mins := Format('%d minute ', [mini])
      else
        mins := Format('%2d minutes ', [mini]);
      if houri = 1 then
        hours := Format('%d hour ', [houri])
      else
        hours := Format('%2d hours ', [houri]);
      if dayi = 1 then
        days := Format('%d day ', [dayi])
      else
        days := Format('%d days ', [dayi]);
      if weeki = 1 then
        weeks := Format('%d week ', [weeki])
      else
        weeks := Format('%d weeks ', [weeki]);
      if monthi = 1 then
        months := Format('%d month ', [monthi])
      else
        months := Format('%2d months ', [monthi]);
      if yeari = 1 then
        years := Format('%d year ', [yeari])
      else
        years := Format('%d years ', [yeari]);
    end;
    imsg := '';
    if yeari > 0 then
      imsg := imsg + years;
    if monthi > 0 then
      imsg := imsg + months;
    if weeki > 0 then
      imsg := imsg + weeks;
    if dayi > 0 then
      imsg := imsg + days;
    if houri > 0 then
      imsg := imsg + hours;
    if mini > 0 then
      imsg := imsg + mins;
    if seci > 0 then
      imsg := imsg + secs;
  finally
    Result := imsg;
  end;
end;

{$WARNINGS ON}

procedure splitString(const Source: String; const Delimiter: String; const Dest: TStringList);
var
  Count: integer;
  LStartpos, LEndepos, LSourcelength: integer;
  LDelimiterLength: integer;
begin
  Dest.Clear;
  Count     := 1;
  LStartpos := 0;
  LEndepos  := 0;
  LSourcelength := length(Source);
  LDelimiterLength := Length(Delimiter);
  while Count <= LSourcelength do
  begin
    if copy(Source, Count, LDelimiterLength) = Delimiter then
    begin
      LEndepos := Count;
      dest.Add(Trim(copy(Source, LStartpos + 1, LEndepos - LStartpos - 1)));
      LStartpos := Count + LDelimiterLength - 1;
      Inc(Count, LDelimiterLength);
    end
    else
    begin
      Inc(Count);
    end;
  end;
  if LEndePos <> Count - LDelimiterLength then
    dest.Add(Trim(copy(Source, LStartpos + 1, Count - LStartpos - 1)));
end;

function onlyEnglishAlpha(const S: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    if IsALetter(S[i]) then
    begin
      Result := Result + S[i];
    end;
  end;
end;

end.

