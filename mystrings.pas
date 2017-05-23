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

function onlyEnglishAlpha(s: AnsiString): AnsiString;
function ArrayTextContainesIndexText(const AText, AValues: array of ansistring): Boolean;

function DateTimeAsString(const aThen: TDateTime; padded: boolean = False): AnsiString;

function WhenDidThisHappen(SecondsElapsed: int64): TDateTime;

function LeftStr(const Source: AnsiString; Count: integer): AnsiString;
function RightStr(const Source: AnsiString; Count: integer): AnsiString;
function MinMax(aValue, minimal, maximum: integer): integer;
function SubString(const s, seperator: AnsiString; index: integer): AnsiString;
function Csere(const Source, old, new: AnsiString): AnsiString;
function AtConvert(Source: AnsiString; style: integer): AnsiString;
function myEncode(what: AnsiString): AnsiString;//spaceket  csereli at
function myDecode(what: AnsiString): AnsiString;
function CleanString(mit: AnsiString): AnsiString;
function Count(const mi, miben: AnsiString): integer;
function RPos(SubStr: AnsiChar; Str: AnsiString): integer;
function FillCharL(what, t: integer; whit: AnsiChar): AnsiString;
function GetLastDir(g: AnsiString): AnsiString;
function ExtractUrlFileName(url: AnsiString): AnsiString;
function ExtractFileNameWithoutExt(fname: AnsiString): AnsiString;
function BufStr(B: array of byte; APos, ALen: integer): AnsiString;
procedure StrBuf(var B: array of byte; APos, ALen: integer; const AStr: AnsiString);
function FillCharR(w: AnsiString; ig: integer; withc: AnsiChar): AnsiString;
function ReadBetweenSeperators(s, sep1, sep2: AnsiString; var from: integer): AnsiString;
function validemail(em: AnsiString): boolean;
function MakeStringLeft(mi, mivel: AnsiString; c: integer): AnsiString;
function MakeStringCenter(mi, mivel: AnsiString; c: integer): AnsiString;
function MakeStringRight(mi, mivel: AnsiString; c: integer): AnsiString;
function MakeFullPath(s: AnsiString): AnsiString;
function GetByte(i, b: longword): byte;
function GetInteger(b1, b2, b3, b4: byte): longword;
function UrlEncode(const DecodedStr: AnsiString; Pluses: boolean): AnsiString; overload;
function UrlEncode(const DecodedStr: AnsiString): AnsiString; overload;
function UrlDecode(const EncodedStr: AnsiString): AnsiString;
function MyToday: AnsiString;
function MyTimeToStr(x: TDateTime): AnsiString;
function MyStrToTime(x: AnsiString): TDateTime;
function MyDateToStr(x: TDateTime): AnsiString;
function MyStrToDate(x: AnsiString): TDateTime;
function NoToTime(x: integer): AnsiString; overload;
function NoToTime(s: AnsiString): AnsiString; overload;
function Szovegge(szam: integer): AnsiString; overload;
function Szovegge(d: double): AnsiString; overload;
function myStrToFloat(s: AnsiString): double; overload;
function myStrToFloat(s: AnsiString; def: double): double; overload;
function CheckTAXNumber(TAxNumber: AnsiString; BornDate: TDateTime = 0): boolean;
function CheckCompanyTaxNumber(TaxNumber: AnsiString): integer;
function IsValidEmail(const Value: AnsiString): boolean;
procedure MyWriteLn(s: AnsiString);
function MyCopy(b: array of byte; index, len: integer): AnsiString;
function myRand(mini, maxi: integer): integer;
function ParseResponseCode(s: AnsiString): integer;

{$IFDEF MSWINDOWS}
function GetWinDir: AnsiString;
function GetTempDir: AnsiString;
function GetContentType(fname: AnsiString): AnsiString;
{$ENDIF}
function MyIncludeTrailingSlash(s: AnsiString): AnsiString;
function CombineDirectories(dir1, dir2: AnsiString): AnsiString;
function ParsePasvString(s: AnsiString; out host: AnsiString; out port: integer): boolean;

function IsALetter(const c: AnsiChar): boolean; // { returns with true if it's a letter: [a-z] or [A-Z] }
function IsANumber(const c: AnsiChar): boolean; // { returns with true if it's a number: [0-9] }
function Szamokszama(s: AnsiString): integer;
function CsakSzamok(s: AnsiString): AnsiString;

function GetFileContents(fn: AnsiString): AnsiString;
function Fetch(var osszes: AnsiString; const Args: array of AnsiChar): AnsiString; overload;
function Fetch(var osszes: AnsiString; sep: AnsiChar): AnsiString; overload;
function Elsosor(var osszes: AnsiString): AnsiString;
function todaycsere(const s: AnsiString; datum: TDateTime = 0): AnsiString;
function InArray(const s: AnsiString; const d: array of AnsiString;
  casesensitive: boolean = True): boolean;

function BoolToStr(Value: boolean; const TS, FS: AnsiString): AnsiString; overload;
function BoolToStr(Value: boolean): AnsiString; overload;


procedure splitString(const Source: AnsiString; const Delimiter: AnsiString;
  const Dest: TStringList);

implementation

uses SysUtils, Math
{$IFDEF MSWINDOWS}
  , registry, Windows
{$ENDIF}
  , DateUtils;

procedure StrBuf(var B: array of byte; APos, ALen: integer; const AStr: AnsiString);
var
  Len: integer;
begin
  Len := Length(AStr);
  if Len > ALen then
    Len := ALen;
  Move(AStr[1], b[APos], Len);
  if Len < ALen then
    FillChar(b[APos + Len], ALen - Len, $00);
end;

function BufStr(B: array of byte; APos, ALen: integer): AnsiString;
begin
  SetString(Result, nil, ALen);
  Move(b[APos], Result[1], ALen);
  if Pos(#0, Result) > 0 then
    Result := copy(Result, 1, Pos(#0, Result) - 1);
  Result := TrimRight(Result);
end;

{$IFDEF MSWINDOWS}
function GetWinDir: AnsiString;
var
  a: array[1..255] of byte;
  i: integer;
begin
  i      := GetWindowsDirectory(@a, 255);
  Result := BufStr(a, 0, i);
end;

function GetTempDir: AnsiString;
var
  a: array[1..255] of byte;
  i: integer;
begin
  i      := GetTempPath(255, @a);
  Result := BufStr(a, 0, i);
end;

{$ENDIF}

function Count(const mi, miben: AnsiString): integer;
var
  s: AnsiString;
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

function LeftStr(const Source: AnsiString; Count: integer): AnsiString;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + Source[i];
end;

function RightStr(const Source: AnsiString; Count: integer): AnsiString;
var
  i: integer;
begin
  Result := '';
  for i := Count + 1 to length(Source) do
    Result := Result + Source[i];
end;

function MinMax(aValue, minimal, maximum: integer): integer;
begin
  if aValue < minimal then
    Result := minimal
  else
  if aValue > maximum then
    Result := maximum
  else
    Result := aValue;
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

function SubString(const s, seperator: AnsiString; index: integer): AnsiString;
var
  akts: AnsiString;
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

function Csere(const Source, old, new: AnsiString): AnsiString;
begin
  Result := StringReplace(Source, old, new, [rfReplaceAll, rfIgnoreCase]);
end;

function AtConvert(Source: AnsiString; style: integer): AnsiString;
var
  i: integer;
  nemkell: boolean;
begin
  Result := Source;
  case style of
    1: Result := AnsiLowerCase(Source);
    2:
    begin
      Result  := '';
      nemkell := False;
      for i := 1 to length(Source) do
      begin
        if (i + 1 <= length(Source)) and (Source[i] in
          [' ', '-', '.', '_', '(', '?', '!']) then
        begin
          Result  := Result + Source[i];
          Source[i + 1] := AnsiUpperCase(Source[i + 1])[1];
          nemkell := True;
        end
        else
        if (i = 1) then
          Result := Result + AnsiUpperCase(Source[i])
        else
        begin
          if nemkell then
          begin
            Result  := Result + Source[i];
            nemkell := False;
          end
          else
            Result := Result + AnsiLowerCase(Source[i]);
        end;
      end;
    end;
    3: Result := AnsiUpperCase(Source);
    4:
    begin
      Result := AnsiLowerCase(Source);
      if length(Result) > 0 then
        Result[1] := AnsiUpperCase(Result)[1];
    end;
  end;
end;

function myDecode(what: AnsiString): AnsiString;
begin
  Result := Csere(what, ' ', '\ia{{ai/');
end;

function myEncode(what: AnsiString): AnsiString;
begin
  Result := Csere(what, '\ia{{ai/', ' ');
end;

//lecsereli az osszes nemfajlnevkaraktert
function CleanString(mit: AnsiString): AnsiString;
begin
  mit    := Csere(mit, '/', '-');
  mit    := Csere(mit, ':', '-');
  mit    := Csere(mit, '?', '');
  mit    := Csere(mit, '<', '');
  mit    := Csere(mit, '>', '');
  mit    := Csere(mit, '"', '');
  mit    := Csere(mit, '*', '');
  mit    := Csere(mit, #0, '');
  Result := mit;
end;



function FillCharL(what, t: integer; whit: AnsiChar): AnsiString;
var
  i: integer;
begin
  Result := IntToStr(what);
  for i := 0 to t - length(Result) - 1 do
    Result := whit + Result;
end;

function GetLastDir(g: AnsiString): AnsiString;
begin
  if ((length(g) > 0) and (g[length(g)] = '\')) then
    Delete(g, length(g), 1);
  Result := RightStr(g, RPos('\', g));
end;

function ExtractUrlFileName(url: AnsiString): AnsiString;
var
  i: integer;
begin
  Result := '';
  i      := RPos('/', url);
  if i > 5 then
    Result := Copy(url, i + 1, 200);
end;

function ExtractFileNameWithoutExt(fname: AnsiString): AnsiString;
var
  tmp: AnsiString;
begin
  tmp    := ExtractFileName(fname);
  Result := Copy(tmp, 1, length(tmp) - length(ExtractFileExt(fname)));
end;

function FillCharR(w: AnsiString; ig: integer; withc: AnsiChar): AnsiString;
var
  i: integer;
begin
  Result := w;
  for i := length(w) to ig do
    Result := Result + withc;
end;

function ReadBetweenSeperators(s, sep1, sep2: AnsiString; var from: integer): AnsiString;
var
  tmp, tmp2: AnsiString;
  k, tmpv, v: integer;
  ok: boolean;
begin
  tmp := Copy(s, from, length(s));
  if sep1 <> '' then
    k := Pos(sep1, tmp) + length(sep1)
  else
    k := 1;
  if k = 0 then
    k := 1;
  v := 0;
  tmpv := 0;
  ok   := True;
  tmp2 := tmp;
  while ok do
  begin
    tmp2 := Copy(tmp, v + 1, length(tmp));
    if sep2 <> '' then
      tmpv := Pos(sep2, tmp2)
    else
    begin
      v  := length(tmp) + 1;
      ok := False;
    end;
    if tmpv = 0 then
    begin
      ok := False;
      v  := length(tmp) + 1;
    end;
    Inc(v, tmpv);
    if v >= k then
      ok := False;
  end;

  from   := v;
  Result := Copy(tmp, k, v - k);
end;



function validemail(em: AnsiString): boolean;
var
  i1, i2: integer;
begin
  i1 := Pos('@', em);
  i2 := RPos('.', em);
  if ((i1 = 0) or (i2 = 0) or (i1 + 1 >= i2)) then
    Result := False
  else
    Result := True;
end;


function MakeStringLeft(mi, mivel: AnsiString; c: integer): AnsiString;
var
  i: integer;
begin
  Result := Copy(mi, 1, c);
  for i := length(Result) + 1 to c do
    Result := Result + mivel;
end;

function MakeStringCenter(mi, mivel: AnsiString; c: integer): AnsiString;
var
  s: integer;
begin
  Result := Copy(mi, 1, c);
  s      := length(Result);
  Result := MakeStringLeft(Result, ' ', (c - s) div 2 + s);
  Result := MakeStringRight(Result, ' ', c);
end;

function MakeStringRight(mi, mivel: AnsiString; c: integer): AnsiString;
var
  i: integer;
begin
  Result := Copy(mi, 1, c);
  for i := 1 to (c - length(Result)) do
    Result := mivel + Result;
end;


function MakeFullPath(s: AnsiString): AnsiString;
var
  x: integer;
begin
  Result := s;
  x      := length(Result);
  if x <> 0 then
    if s[x] <> '\' then
      Result := s + '\';
end;

//visszaadja i b-ik bajtjat
function GetByte(i, b: longword): byte;
var
  mask: longword;
begin
  mask   := Round(IntPower(2, b * 8) - 1 - ((IntPower(2, (b - 1) * 8)) - 1));
  Result := (i and mask) shr ((b - 1) * 8);
end;

//a negy megadott bajtbol keszit egy integert
function GetInteger(b1, b2, b3, b4: byte): longword;
begin
  Result := b4;
  Result := Result shl 8;
  Result := Result + b3;
  Result := Result shl 8;
  Result := Result + b2;
  Result := Result shl 8;
  Result := Result + b1;
end;

function UrlEncode(const DecodedStr: AnsiString; Pluses: boolean): AnsiString;
var
  I: integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
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
  Result := URLEncode(DecodedStr, True);
end;


function HexToInt(HexStr: AnsiString): int64;
var
  RetVar: int64;
  i:      byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0'..'9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else
    if HexStr[i] in ['A'..'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      Retvar := 0;
      break;
    end;
  end;

  Result := RetVar;
end;


function UrlDecode(const EncodedStr: AnsiString): AnsiString;
var
  I: integer;
begin
  Result := '';
  if Length(EncodedStr) > 0 then
  begin
    I := 1;
    while I <= Length(EncodedStr) do
    begin
      if EncodedStr[I] = '%' then
      begin
        Result := Result + Chr(
          HexToInt(EncodedStr[I + 1] + EncodedStr[I + 2]));
        I      := Succ(Succ(I));
      end
      else if EncodedStr[I] = '+' then
        Result := Result + ' '
      else
        Result := Result + EncodedStr[I];

      I := Succ(I);
    end;
  end;
end;



{$IFDEF MSWINDOWS}
function GetContentType(fname: AnsiString): AnsiString;
var
  x: TRegistry;
begin
  x      := TRegistry.Create;
  x.RootKey := HKEY_CLASSES_ROOT;
  Result := 'application/octet-stream';
  try
    x.OpenKey('\' + ExtractFileExt(fname), False);
    Result := x.ReadString('Content Type');
  finally
    x.Free;
  end;
end;

{$ENDIF}

function MyToday: AnsiString;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  Result := Format('%.4d-%.2d-%.2d', [y, m, d]);
end;

function MyDateToStr(x: TDateTime): AnsiString;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', x);
end;

function MyTimeToStr(x: TDateTime): AnsiString;
begin
  Result := FormatDateTime('hh:nn', x);
end;

function MyStrToTime(x: AnsiString): TDateTime;
var
  h, m: integer;
begin
  h      := StrToIntDef(Copy(x, 1, 2), 0);
  m      := StrToIntDef(Copy(x, 4, 2), 0);
  Result := EncodeTime(h, m, 0, 0);
end;


function MyStrToDate(x: AnsiString): TDateTime;
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


function NoToTime(x: integer): AnsiString;
begin
  Result := IntToStr(8 + (x div 2)) + ':';
  if (x mod 2 = 0) then
    Result := Result + '00'
  else
    Result := Result + '30';
end;

function NoToTime(s: AnsiString): AnsiString;
begin
  Result := NoToTime(StrToInt(s)) + '-' + NoToTime(StrToInt(s) + 1);
end;


procedure betuzz(var s: AnsiString; number: integer);
const
  kicsik: array[0..8] of AnsiString =
    ('egy', 'kettu', 'harom', 'nigy', 'vt', 'hat', 'hit', 'nyolc', 'kilenc');
var
  num: integer;
begin

  num := number;
  if (num div 100 <> 0) then
  begin
    if (num div 100 <> 1) then
    begin
      s := s + kicsik[(num div 100) - 1];
    end;
    s   := s + 'szaz';
    num := num mod 100;
  end;

  if (num div 10 <> 0) then
  begin
    case (num div 10) of
      9: s := s + 'kilencven';
      8: s := s + 'nyolcvan';
      7: s := s + 'hetven';
      6: s := s + 'hatvan';
      5: s := s + 'vtven';
      4: s := s + 'negyven';
      3: s := s + 'harminc';
      2:
        if (num mod 10 <> 0) then
          s := s + 'huszon'
        else
          s := s + 'hzsz';
      1:
        if (num mod 10 <> 0) then
          s := s + 'tizen'
        else
          s := s + 'tmz';
    end; //end of case
  end;

  if (num mod 10 <> 0) then
    s := s + kicsik[(num mod 10) - 1];

end;


function Szovegge(szam: integer): AnsiString;
const
  SZMAX = 4;
type
  TCuccok = record
    ertek: integer;
    s:     AnsiString
  end;
  TTablazat = array[1..SZMAX] of TCuccok;

const
  ertekek: TTablazat = (
    (ertek: 1000000000; s: 'milliard'),
    (ertek: 1000000; s: 'millis'),
    (ertek: 1000; s: 'ezer'),
    (ertek: 1; s: '')
    );
var
  orig, i:  integer;
  betukkel: AnsiString;
begin
  Result := '';
  if szam < 0 then
  begin
    szam   := szam * -1;
    Result := 'mmnusz ';
  end;

  orig := szam;
  for i := 1 to SZMAX do
  begin
    if (szam div ertekek[i].ertek <> 0) then
    begin
      betukkel := '';
      betuzz(betukkel, szam div ertekek[i].ertek);
      Result := Result + betukkel + ertekek[i].s;
      szam   := szam mod ertekek[i].ertek;
      if (i <> SZMAX) and (szam > 0) and (orig > 2000) then
        Result := Result + '-';
    end;
  end;

end;

function Szovegge(d: double): AnsiString;
var
  a: integer;
begin
  a      := Round(d);
  Result := Szovegge(a);
end;


function myStrToFloat(s: AnsiString; def: double): double;
var
  x: AnsiString;
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

function myStrToFloat(s: AnsiString): double;
begin
  Result := myStrToFloat(s, -1);
end;

function CheckTAXNumber(TAxNumber: AnsiString; BornDate: TDateTime = 0): boolean;
var
  index, napok_szama, crc: integer;
begin
  try
    if (Length(TaxNumber) <> 10) then
      Result := False
    else
    begin
      Result := True;
      if BornDate <> 0 then
      begin
        napok_szama := Trunc(BornDate - EncodeDate(1867, 1, 1));
        if (StrToInt(copy(TaxNumber, 2, 5)) <> napok_szama) then
          Result := False;
      end;
      if Result then
      begin
        crc   := 0;
        index := 1;
        while (index < Length(TaxNumber)) do
        begin
          crc   := crc + (StrToInt(copy(TaxNumber, index, 1)) * index);
          index := index + 1;
        end;
        crc    := (crc - StrToInt(copy(TaxNumber, 10, 1))) mod 11;
        Result := crc = 0;
      end;
    end;
  except
    Result := False;
  end;
end;

function CheckCompanyTaxNumber(TaxNumber: AnsiString): integer;
{************************************************
* Adsszam ellenurzise
* Visszatirisi irtik:
* - 0: Js adsszam
* - -1: Rossz a kapott irtik hossza (csak 11 /elvalasztas nilk|l/ vagy 13 /elvalasztassal/ karakter lehet)
* - -2: A kapott irtik nem csak szamjegyet tartalmaz (kivive: elvalasztas)
* - -3: A 9. helyen nem 1,2 vagy 3 szerepel (adsmentes, adskvteles,EVA)
* - -4: Az utolss kit szamjegy nem a kvvetkezuk egyike: 02-20, 22-44, 41
* - -5: A kapott irtik CDV hibas
************************************************}
const
  aCDV: array[1..4] of integer = (9, 7, 3, 1);
var
  i:     int64;
  j:     integer;
  nCDV:  integer;
  cTemp: AnsiString;
begin
  if not (length(TaxNumber) in [11, 13]) then
  begin
    Result := -1;
    exit;
  end;
  if Length(TaxNumber) = 11 then
  begin
    if not TryStrToInt64(TaxNumber, i) then
    begin
      Result := -2;
      exit;
    end;
    cTemp := TaxNumber;
  end
  else
  begin
    cTemp := copy(TaxNumber, 1, 8) + copy(TaxNumber, 10, 1) + copy(TaxNumber, 12, 2);
    if not TryStrToInt64(cTemp, i) then
    begin
      Result := -2;
      exit;
    end;
  end;
  if not (cTemp[9] in ['1', '2', '3']) then
  begin
    Result := -3;
    exit;
  end;
  nCDV := StrToInt(copy(cTemp, 10, 2));
  if not (((nCDV > 1) and (nCDV < 21)) or ((nCDV > 21) and (nCDV < 45)) or
    (nCDV = 51)) then
  begin
    Result := -4;
    exit;
  end;
  nCDV := 0;
  for j := 1 to 7 do
  begin
    nCDV := nCDV + StrToInt(cTemp[j]) * aCDV[((j - 1) mod 4) + 1];
  end;
  if StrToInt(cTemp[8]) <> ((10 - (nCDV mod 10)) mod 10) then
  begin
    Result := -5;
    exit;
  end;
  Result := 0;
end;

function IsValidEmail(const Value: AnsiString): boolean;

  function CheckAllowed(const s: AnsiString): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '.']) then
        Exit;
    end;
    Result := True;
  end;

var
  i: integer;
  namePart, serverPart: AnsiString;
begin // of IsValidEmail
  Result := False;
  i      := Pos('@', Value);
  if (i = 0) or (pos('..', Value) > 0) then
    Exit;
  namePart   := Copy(Value, 1, i - 1);
  serverPart := Copy(Value, i + 1, Length(Value));
  if (Length(namePart) = 0)         // @ or name missing
    or ((Length(serverPart) < 4))   // name or server missing or
  then
    Exit;                      // too short
  i := Pos('.', serverPart);
  // must have dot and at least 3 places from end
  if (i < 2) or (i > (Length(serverPart) - 2)) then
    Exit;
  Result := CheckAllowed(namePart) and CheckAllowed(serverPart);
end;

procedure MyWriteLn(s: AnsiString);
{$IFDEF DEBUG}var f: TextFile;{$ENDIF}
begin
{$IFDEF DEBUG}
  s:= FormatDateTime('hh:nn:ss.zzz', Now())+': '+s;
  AssignFile(f, 'szamlazo.log');
  if (FileExists('szamlazo.log')) then Append(f) else Rewrite(f);
  WriteLn(f,s);
  CloseFile(f);
{$ENDIF}
end;


function MyCopy(b: array of byte; index, len: integer): AnsiString;
var
  i: integer;
begin
  Result := '';
  for i := index to index + len - 1 do
    Result := Result + Chr(b[i]);
end;

function myRand(mini, maxi: integer): integer;
begin
  Result := Random(maxi - mini + 1) + mini;
end;

function RPos(SubStr: AnsiChar; Str: AnsiString): integer;
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

function RTrimCRLF(const s: AnsiString): AnsiString;
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

function ParseResponseCode(s: AnsiString): integer;
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

function CombineDirectories(dir1, dir2: AnsiString): AnsiString;
begin
  if dir1 <> '' then
    Result := MyIncludeTrailingSlash(dir1) + dir2
  else
    Result := dir2;
end;

function MyIncludeTrailingSlash(s: AnsiString): AnsiString;
begin
  if length(s) > 0 then
  begin
    Result := s;
    if Result[length(s)] <> '/' then
      Result := Result + '/';
  end
  else
    Result := '/';
end;

function ParsePasvString(s: AnsiString; out host: AnsiString; out port: integer): boolean;
begin
  Result := False;

  s := Copy(s, Pos('(', s) + 1, 100000);
  s := Copy(s, 1, Pos(')', s) - 1);
  if s = '' then
    exit;

  host := Fetch(s, ',') + '.' + Fetch(s, ',') + '.' + Fetch(s, ',') +
    '.' + Fetch(s, ',');
  if s = '' then
    exit;

  port := StrToIntDef(Fetch(s, ','), 0) * 256 + StrToIntDef(Fetch(s, ','), 0);
  if port = 0 then
    exit;
  Result := True;
end;

function IsALetter(const c: AnsiChar): boolean;
begin
  Result := (((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')));
end;

function IsANumber(const c: AnsiChar): boolean;
begin
  Result := ((c >= '0') and (c <= '9'));
end;

function Szamokszama(s: AnsiString): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(s) do
    if (IsANumber(s[i])) then
      Inc(Result);
end;

function CsakSzamok(s: AnsiString): AnsiString;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
    if not (s[i] in ['/', '-', '0'..'9']) then
      exit
    else if (IsANumber(s[i])) then
      Result := Result + s[i];
end;

function GetFileContents(fn: AnsiString): AnsiString;
var
  x: TextFile;
  s: AnsiString;
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



function Fetch(var osszes: AnsiString; const Args: array of AnsiChar): AnsiString;
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

function Fetch(var osszes: AnsiString; sep: AnsiChar): AnsiString;
begin
  Result := Fetch(osszes, [sep]);
end;

function Elsosor(var osszes: AnsiString): AnsiString;
begin
  Result := Fetch(osszes, [#13, #10]);
end;


function todaycsere(const s: AnsiString; datum: TDateTime = 0): AnsiString;
var
  yyyy, yy, mm, dd, ww: AnsiString;
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

function InArray(const s: AnsiString; const d: array of AnsiString;
  casesensitive: boolean = True): boolean;
var
  i: integer;
begin
  Result := True;

  for i := low(d) to high(d) do
  begin
    if casesensitive then
    begin
      if d[i] = s then
        exit;
    end
    else
    if AnsiSameText(d[i], s) then
      exit;
  end;

  Result := False;
end;

{$WARNINGS OFF}
function DateTimeAsString(const aThen: TDateTime; padded: boolean = False): AnsiString;
var
  i, seci, mini, houri, dayi, weeki, monthi, yeari:    int64;
  imsg, secs, mins, hours, days, weeks, months, years: AnsiString;
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

function WhenDidThisHappen(SecondsElapsed: int64): TDateTime;
var
  dtsec:   TDateTime;
  SecFrac: real;
begin
  SecFrac := (1 / 86400);
  dtsec   := SecondsElapsed * SecFrac;
  Result  := Now - dtsec;
end;

function BoolToStr(Value: boolean; const TS, FS: AnsiString): AnsiString; overload;
begin
  if Value then
    Result := TS
  else
    Result := FS;
end;

function BoolToStr(Value: boolean): AnsiString; overload;
begin
  Result := BoolToStr(Value, 'TRUE', 'FALSE');
end;




procedure splitString(const Source: AnsiString; const Delimiter: AnsiString;
  const Dest: TStringList);
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




function onlyEnglishAlpha(s: AnsiString): AnsiString;
var
  i: integer;
begin
  s      := LowerCase(s);
  Result := '';
  for i := 1 to length(s) do
    if ((s[i] >= 'a') and (s[i] <= 'z')) then
    begin
      Result := Result + s[i];
    end;
end;

function ArrayTextContainesIndexText(const AText, AValues: array of ansistring): Boolean;
var
ati,avi: Integer;
begin
  Result := false;
  for ati := Low(AText) to High(AText) do begin
    for avi := Low(AValues) to High(AValues) do
    if AnsiSameText(AText[ati], AValues[avi]) then
    begin
      Result := true;
      Break;
    end;
  end;
end;

end.

