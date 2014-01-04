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

function DateTimeAsString(const aThen:TDateTime; padded: Boolean = False):string;

function WhenDidThisHappen(SecondsElapsed:int64):TDateTime;

function LeftStr (const source: string; count: integer): string;
function RightStr (const source: string; count: integer): string;
function MinMax(aValue, minimal, maximum: integer): integer;
function SubString (const s, seperator: string; index: integer): string;
function Csere (const source, mit, mire:string):string;
function AtConvert(source: string; style: integer): string;
function RightStrv2 (const source: string; count: integer): string;
function myEncode (what: string): String;//spaceket  csereli at
function myDecode (what: string): String;
function CleanString (mit: String):String;
function Count(const mi, miben: string):Integer;
function RPos(SubStr: Char; Str: String): Integer;
function FillCharL(what, t: Integer;whit: Char): String;
function GetLastDir(g: string): string;
function ExtractUrlFileName(url:string):string;
function ExtractFileNameWithoutExt(fname:string):string;
function BufStr (B: array of byte; APos, ALen : integer) : string;
procedure StrBuf (var B: array of byte; APos, ALen : integer; const AStr : string);
function FillCharR(w: string; ig: Integer; withc: Char): string;
function ReadBetweenSeperators(s, sep1,sep2: string; var from: Integer):string;
function validemail(em: string): Boolean;
function MakeStringLeft(mi, mivel: string; c: Integer): string;
function MakeStringCenter(mi, mivel: string; c: Integer): string;
function MakeStringRight(mi, mivel: string; c: Integer): string;
function MakeFullPath(s: string):string;
function GetByte(i, b: LongWord): Byte;
function GetInteger(b1,b2,b3,b4:Byte): LongWord;
function UrlEncode(const DecodedStr: String; Pluses: Boolean): String; overload;
function UrlEncode(const DecodedStr: String): String; overload;
function UrlDecode(const EncodedStr: String): String;
function MyToday: string;
function MyTimeToStr(x: TDateTime): string;
function MyStrToTime(x: string): TDateTime;
function MyDateToStr(x: TDateTime): string;
function MyStrToDate(x: string): TDateTime;
function NoToTime(x: Integer): string; overload;
function NoToTime(s: string): string; overload;
function Szovegge(szam: Integer): string; overload;
function Szovegge(d: Double): string; overload;
function myStrToFloat(s: string): Double; overload;
function myStrToFloat(s: string; def: Double): Double; overload;
function CheckTAXNumber(TAxNumber: string; BornDate: TDateTime = 0): Boolean;
function CheckCompanyTaxNumber(TaxNumber: string):Integer;
function IsValidEmail(const Value: string): boolean;
procedure MyWriteLn(s: string);
function MyCopy(b: array of byte; index, len: Integer): string;
function myRand(mini, maxi: Integer): Integer;
function ParseResponseCode(s: string): Integer;

{$IFDEF MSWINDOWS}
function GetWinDir: string;
function GetTempDir:string;
function GetContentType(fname: string): string;
{$ENDIF}
function MyIncludeTrailingSlash(s: string): string;
function CombineDirectories(dir1, dir2: string): string;
function ParsePasvString(s: string; var host: string; var port: Integer): Boolean;

function Szam(c: char): Boolean;
function Szamokszama(s: string): Integer;
function CsakSzamok(s: string): string;

function GetFileContents(fn: string):string;
function Fetch(var osszes: string; const Args: array of char): string; overload;
function Fetch(var osszes: string; sep: char): string; overload;
function Elsosor(var osszes: string): string;
function todaycsere(const s: string; datum: TDateTime = 0): string;
function InArray(const s: string ; const d: array of string; casesensitive: Boolean = True): Boolean;

function BoolToStr(Value: Boolean; const TS, FS: String): String; overload;
function BoolToStr(Value: Boolean): String; overload;


procedure splitString(const Source: String; const Delimiter: String; const Dest: TStringlist);

implementation

uses SysUtils, math
{$IFDEF MSWINDOWS}
, registry, Windows
{$ENDIF}
, DateUtils;

procedure StrBuf (var B: array of byte; APos, ALen : integer; const AStr : string);
  var
   Len : integer;
  begin
   Len := Length (AStr);
   if Len > ALen
    then Len := ALen;
   Move (AStr [1], b [APos], Len);
   if Len < ALen
    then FillChar (b [APos + Len], ALen - Len, $00);
  end;

function BufStr (B: array of byte; APos, ALen : integer) : string;
begin
   SetString (Result, nil, ALen);
   Move (b [APos], Result [1], ALen);
   if Pos (#0, Result) > 0 then
     Result := copy (Result, 1, Pos (#0, Result) - 1);
   Result := TrimRight (Result);
end;

{$IFDEF MSWINDOWS}
function GetWinDir:string;
var a: array[1..255] of Byte;
    i: Integer;
begin
  i:= GetWindowsDirectory(@a,255);
  Result:=BufStr(a,0,i);
end;

function GetTempDir:string;
var a: array[1..255] of Byte;
    i: Integer;
begin
  i:= GetTempPath(255,@a);
  Result:=BufStr(a,0,i);
end;
{$ENDIF}

function Count(const mi, miben: string):Integer;
var s: string;
    i: Integer;
begin
  s:= '';
  Result:= 0;
  for i:= 1 to length(miben) do
  begin
    s:= s + miben[i];
    if 0 < Pos(mi,s) then
    begin
      inc(Result);
      s:= '';
    end;
  end;
end;

function LeftStr (const source: string; count: integer): string;
var i: integer;
begin
  Result:= '';
  for i:= 1 to count do
    Result:= Result + source[i];
end;

function RightStr (const source: string; count: integer): string;
var i: integer;
begin
  Result:= '';
  for i:= length(source)-count+1 to length(source) do
    Result:= Result + source[i];
end;

function RightStrv2 (const source: string; count: integer): string;
var i: integer;
begin
  Result:= '';
  for i:= count+1 to length(source) do
    Result:= Result + source[i];
end;


function MinMax(aValue, minimal, maximum: integer): integer;
begin
  if aValue < minimal then
    Result:= minimal else
  if aValue > maximum then
    Result:= maximum else
    Result:= aValue;
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

function SubString (const s, seperator: string; index: integer): string;
var akts: string;
    sz: integer;
    i,l: Integer;
begin
  akts:= s;
  sz:= 0;
  Result:= '';
  l:= length(seperator);
  repeat
    i:= Pos(seperator, akts);
    if i <> 0 then
    begin
      if sz + 1 = index then
      begin
        // ezt kerestuk
        Result:= Copy(akts, 1, i-1);
        exit;
      end;

      akts:= Copy(akts, i+l, 100000);
      inc(sz);
    end else
    begin
      // nincs tobb talalat.
      if sz + 1 = index then
        Result:= akts;
      exit;
    end;
  until False;
end;

function Csere (const source, mit, mire:string):string;
begin
  Result:= StringReplace(source,mit, mire, [rfReplaceAll, rfIgnoreCase]);
end;

function AtConvert(source: string; style: integer): string;
var i: integer;
    nemkell: boolean;
begin
  Result:= source;
  case style of
    1: Result:= AnsiLowerCase (source);
    2: begin
         Result:= '';
         nemkell:= false;
         for i:= 1 to length (source) do
         begin
           if (i+1 <= length(source)) and (source[i] in [' ', '-', '.', '_','(','?','!']) then
           begin
             Result:= Result + source[i];
             source[i+1]:= AnsiUpperCase(source[i+1])[1];
             nemkell:= true;
           end else
           if (i = 1) then
             Result:= Result + AnsiUpperCase(source[i]) 
           else
           begin
             if nemkell then
             begin
               Result:= Result + source[i];
               nemkell:= false;
             end else
               Result:= Result + AnsiLowerCase(source[i]);
           end;
         end;
       end;
    3: Result:= AnsiUpperCase (source);
    4: begin
         Result:= AnsiLowerCase (source);
         if length(Result) > 0 then
           Result[1]:= AnsiUpperCase (Result)[1];
       end;
  end;
end;

function myDecode (what: string): String;
begin
  Result:= Csere (what, ' ', '\éáûûáé/');
end;

function myEncode (what: string): String;
begin
  Result:= Csere (what,'\éáûûáé/' ,' ');
end;

//lecsereli az osszes nemfajlnevkaraktert
function CleanString (mit: String):String;
begin
  mit:= Csere (mit, '/', '-');
  mit:= Csere (mit, ':', '-');
  mit:= Csere (mit, '?', '');
  mit:= Csere (mit, '<', '');
  mit:= Csere (mit, '>', '');
  mit:= Csere (mit, '"', '');
  mit:= Csere (mit, '*', '');
  mit:= Csere (mit, #0, '');
  Result:= mit;
end;



function FillCharL(what, t: Integer;whit: Char): String;
var i: Integer;
begin
  Result:= IntToStr(what);
  for i:= 0 to t - length(Result) - 1 do
    Result:= whit+Result;
end;

function GetLastDir(g: string): string;
begin
  if ((length(g) > 0) and (g[length(g)] = '\')) then
    Delete(g,length(g),1);
  Result:= RightStrv2(g,RPos('\',g));
end;

function ExtractUrlFileName(url:string):string;
var i: Integer;
begin
  Result:= '';
  i:= RPos('/', url);
  if i > 5 then
    Result:= Copy(url, i+1, 200);
end;

function ExtractFileNameWithoutExt(fname:string):string;
var tmp: string;
begin
  tmp:= ExtractFileName(fname);
  Result:= Copy(tmp,1,length(tmp)-length(ExtractFileExt(fname)));
end;

function FillCharR(w: string; ig: Integer; withc: Char):string;
var i: Integer;
begin
  Result:= w;
  for i:= length(w) to ig do
    Result:= Result + withc;
end;

function ReadBetweenSeperators(s, sep1,sep2: string; var from: Integer):string;
var tmp, tmp2: string;
    k, tmpv, v: Integer;
    ok: Boolean;
begin
  tmp:= Copy(s,from,length(s));
  if sep1 <> '' then
    k:= Pos(sep1,tmp) + length(sep1)
  else
    k:= 1;
  if k = 0 then k:= 1;
  v:= 0;
  tmpv:= 0;
  ok:= True;
  tmp2:= tmp;
  while ok do
  begin
    tmp2:= Copy(tmp,v+1,length(tmp));
    if sep2 <> '' then
      tmpv:= Pos(sep2,tmp2)
    else
    begin
      v:= length(tmp) + 1;
      ok:= False;
    end;
    if tmpv = 0 then
    begin
      ok:= False;
      v:= length(tmp) + 1;
    end;
    inc(v, tmpv);
    if v >= k then
      ok:= False;
  end;

  from:= v;
  Result:= Copy(tmp, k, v-k);
end;



function validemail(em: string): Boolean;
var i1, i2: Integer;
begin
  i1:= Pos('@',em);
  i2:= RPos('.',em);
  if ((i1 = 0) or (i2 = 0) or (i1 + 1 >= i2)) then
    Result:= False
  else
    Result:= True;
end;


function MakeStringLeft(mi, mivel: string; c: Integer): string;
var i: Integer;
begin
  Result:= Copy(mi,1,c);
  for i:= length(Result)+1 to c do
    Result:= Result + mivel;
end;

function MakeStringCenter(mi, mivel: string; c: Integer): string;
var s: Integer;
begin
  Result:= Copy(mi,1,c);
  s:= length(Result);
  Result:= MakeStringLeft(Result, ' ',(c -s) div 2 + s);
  Result:= MakeStringRight(Result, ' ',c);
end;

function MakeStringRight(mi, mivel: string; c: Integer): string;
var i: Integer;
begin
  Result:= Copy(mi,1,c);
  for i:= 1 to (c-length(Result)) do
    Result:= mivel + Result;
end;


function MakeFullPath(s: string):string;
var x: Integer;
begin
  Result:= s;
  x:= length(Result);
  if x <> 0 then
    if s[x] <> '\' then
      Result:= s+'\';
end;

//visszaadja i b-ik bajtjat
function GetByte(i, b: LongWord): Byte;
var mask: LongWord;
begin
  mask:= Round(IntPower(2,b*8)-1 - ((IntPower(2,(b-1)*8)) - 1));
  Result:= (i and mask) shr ((b -1)*8);
end;

//a negy megadott bajtbol keszit egy integert
function GetInteger(b1,b2,b3,b4:Byte): LongWord;
begin
  Result:= b4;
  Result:= Result shl 8;
  Result:= Result + b3;
  Result:= Result shl 8;
  Result:= Result + b2;
  Result:= Result shl 8;
  Result:= Result + b1;
end;

function UrlEncode(const DecodedStr: String; Pluses: Boolean): String;
var
  I: Integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z',
                                       'A'..'Z', ' ']) then
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

function UrlEncode(const DecodedStr: String): String;
begin
  Result:= URLEncode(DecodedStr, True);
end;


function HexToInt(HexStr: String): Int64;
var RetVar : Int64;
    i : byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
     Delete(HexStr,length(HexStr),1);
  RetVar := 0;

  for i := 1 to length(HexStr) do begin
      RetVar := RetVar shl 4;
      if HexStr[i] in ['0'..'9'] then
         RetVar := RetVar + (byte(HexStr[i]) - 48)
      else
         if HexStr[i] in ['A'..'F'] then
            RetVar := RetVar + (byte(HexStr[i]) - 55)
         else begin
            Retvar := 0;
            break;
         end;
  end;

  Result := RetVar;
end;


function UrlDecode(const EncodedStr: String): String;
var
  I: Integer;
begin
  Result := '';
  if Length(EncodedStr) > 0 then
  begin
    I := 1;
    while I <= Length(EncodedStr) do
    begin
      if EncodedStr[I] = '%' then
        begin
          Result := Result + Chr(HexToInt(EncodedStr[I+1]
                                       + EncodedStr[I+2]));
          I := Succ(Succ(I));
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
function GetContentType(fname: string): string;
var x: TRegistry;
begin
  x:= TRegistry.Create;
  x.RootKey:= HKEY_CLASSES_ROOT;
  Result:= 'application/octet-stream';
  try
    x.OpenKey('\'+ExtractFileExt(fname),false);
    Result:= x.ReadString('Content Type');
  finally
    x.Free;
  end;
end;

{$ENDIF}

function MyToday: string;
var y,m,d:Word;
begin
  DecodeDate(Now, y, m, d);
  Result:= Format('%.4d-%.2d-%.2d', [y, m, d]);
end;

function MyDateToStr(x: TDateTime): string;
begin
  Result:= FormatDateTime('yyyy-mm-dd hh:nn:ss', x)
end;

function MyTimeToStr(x: TDateTime): string;
begin
  Result:= FormatDateTime('hh:nn', x)
end;

function MyStrToTime(x: string): TDateTime;
var h, m: Integer;
begin
  h:= StrToIntDef(Copy(x, 1, 2), 0);
  m:= StrToIntDef(Copy(x, 4, 2), 0);
  Result:= EncodeTime(h, m, 0, 0)
end;


function MyStrToDate(x: string): TDateTime;
var y, m, d,h, mm, s: Integer;
begin
  y:= StrToIntDef(Copy(x, 1, 4), 0);
  m:= StrToIntDef(Copy(x, 6, 2), 0);
  d:= StrToIntDef(Copy(x, 9, 2), 0);
  h:= StrToIntDef(Copy(x, 12, 2), 0);
  mm:= StrToIntDef(Copy(x, 15, 2), 0);
  s:= StrToIntDef(Copy(x, 18, 2), 0);
  if not TryEncodeDateTime(y,m,d, h, mm, s, 0, Result) then
    Result:= 0;
end;


function NoToTime(x: Integer): string;
begin
  Result:= IntToStr(8 + (x div 2))+':';
  if (x mod 2 = 0) then
    Result:= Result + '00'
  else
    Result:= Result + '30';
end;

function NoToTime(s: string): string;
begin
  Result:= NoToTime(StrToInt(s))+'-'+NoToTime(StrToInt(s)+1);
end;


procedure betuzz(var s: string; number: Integer);
const kicsik: array[0..8] of string = ('egy','kettõ','három','négy','öt','hat','hét','nyolc','kilenc');
var num: Integer;
begin

  num:= number;
  if (num div 100 <> 0) then
  begin
     if (num div 100 <> 1) then
		 begin
       s:= s + kicsik[(num div 100) - 1];
     end;
     s:= s + 'száz';
     num:= num mod 100;
	end;
  
  if (num div 10 <> 0) then
  begin
    case (num div 10) of
      9: s:= s+'kilencven';
			8: s:= s+'nyolcvan';
			7: s:= s+'hetven';
			6: s:= s+'hatvan';
			5: s:= s+'ötven';
			4: s:= s+'negyven';
			3: s:= s+'harminc';
			2:
			         if (num mod 10 <> 0) then
                  s:= s+ 'huszon'
               else
                  s:= s+ 'húsz';
			1:
			         if (num mod 10 <> 0) then
                  s:= s + 'tizen'
           	 	 else
                  s:= s + 'tíz';
    end; //end of case
	end;

  if (num mod 10 <> 0) then s:= s + kicsik[(num mod 10) - 1];

end;


function Szovegge(szam: Integer): string;
const SZMAX = 4;
type
  TCuccok = record
    ertek: Integer;
    s: string
  end;
  TTablazat = array[1..SZMAX] of TCuccok;

const ertekek : TTablazat = (
                      (ertek:1000000000; s:'milliárd'),
                      (ertek:1000000;   s:'millió'),
                      (ertek:1000;     s:'ezer'),
                      (ertek:1; s:'')
                             );
var orig, i: Integer;
    betukkel: string;
begin
 Result:= '';
 if szam < 0 then
 begin
   szam:= szam * -1;
   Result:= 'mínusz ';
 end;

 orig:= szam;
 for i:=1 to SZMAX do
 begin
   if (szam div ertekek[i].ertek <> 0) then
   begin
     betukkel:= '';
  	 betuzz(betukkel, szam div ertekek[i].ertek);
     Result:= Result + betukkel + ertekek[i].s;
  	 szam:= szam mod ertekek[i].ertek;
     if (i <> SZMAX) and (szam > 0) and (orig > 2000) then
       Result:= Result + '-';
   end;
 end;

end;

function Szovegge(d: Double): string;
var a: Integer;
begin
  a:= Round(d);
  Result:= Szovegge(a);
end;


function myStrToFloat(s: string; def: Double): Double;
var x: string;
    d: Integer;
    e: Integer;
begin
  Result:= def;
  if s = '' then exit;
  s:= Csere(s, ',', '.');
  d:= Count('.', s);
  if (d <= 1) then
  begin
    Result:= StrToIntDef(SubString(s, '.', 1), 0);
    if d = 1 then
    begin
      x:= SubString(s, '.', 2);
      if Result < 0 then e:= -1 else e:= 1;
      Result:= Result+ e * StrToIntDef(x,0)/Power(10,length(x));
    end;
  end;
end;

function myStrToFloat(s: string): Double;
begin
  Result:= myStrToFloat(s, -1);
end;

function CheckTAXNumber(TAxNumber: string; BornDate: TDateTime = 0): Boolean;
var index, napok_szama, crc: integer;
begin
try
if (Length(TaxNumber) <> 10) then Result := false
else begin
Result:=True;
if BornDate<>0 then
begin
napok_szama := Trunc(BornDate - EncodeDate(1867, 1, 1));
if (StrToInt(copy(TaxNumber, 2, 5)) <> napok_szama) then Result := False;
end;
if Result then
begin
crc := 0;
index := 1;
while (index < Length(TaxNumber)) do begin
crc := crc + (StrToInt(copy(TaxNumber, index, 1)) * index);
index := index + 1;
end;
crc := (crc - StrToInt(copy(TaxNumber, 10, 1))) mod 11;
result := crc = 0;
end;
end;
except
Result := False;
end;
end;

function CheckCompanyTaxNumber(TaxNumber: string):Integer;
{************************************************
* Adószám ellenõrzése
* Visszatérési érték:
* - 0: Jó adószám
* - -1: Rossz a kapott érték hossza (csak 11 /elválasztás nélkül/ vagy 13 /elválasztással/ karakter lehet)
* - -2: A kapott érték nem csak számjegyet tartalmaz (kivéve: elválasztás)
* - -3: A 9. helyen nem 1,2 vagy 3 szerepel (adómentes, adóköteles,EVA)
* - -4: Az utolsó két számjegy nem a következõk egyike: 02-20, 22-44, 41
* - -5: A kapott érték CDV hibás
************************************************}
const
aCDV:array[1..4] of integer = (9,7,3,1);
var i: int64;
j: integer;
nCDV: integer;
cTemp: string;
begin
if not (length(TaxNumber) in [11,13]) then
begin
Result := -1;
exit;
end;
if Length(TaxNumber)=11 then
begin
if not TryStrToInt64(TaxNumber,i) then
begin
Result := -2;
exit;
end;
cTemp := TaxNumber;
end
else
begin
cTemp := copy(TaxNumber,1,8) + copy(TaxNumber,10,1) + copy(TaxNumber,12,2);
if not TryStrToInt64(cTemp,i) then
begin
Result := -2;
exit;
end;
end;
if not (cTemp[9] in ['1','2','3']) then
begin
Result := -3;
exit;
end;
nCDV := StrToInt(copy(cTemp,10,2));
if not(((nCDV>1) and (nCDV<21)) or ((nCDV>21) and (nCDV<45)) or (nCDV=51)) then
begin
Result := -4;
exit;
end;
nCDV := 0;
for j:=1 to 7 do
begin
nCDV := nCDV + StrToInt(cTemp[j])*aCDV[((j-1) mod 4)+1];
end;
if StrToInt(cTemp[8]) <> ((10-(nCDV mod 10)) mod 10) then
begin
Result := -5;
exit;
end;
Result := 0;
end;

function IsValidEmail(const Value: string): boolean;
  function CheckAllowed(const s: string): boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= 1 to Length(s) do
    begin
      // illegal char in s -> no valid address
      if not (s[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then
        Exit;
    end;
    Result:= true;
  end;
var
  i: integer;
  namePart, serverPart: string;
begin // of IsValidEmail
  Result:= false;
  i:= Pos('@', Value);
  if (i = 0) or (pos('..', Value) > 0) then
    Exit;
  namePart:= Copy(Value, 1, i - 1);
  serverPart:= Copy(Value, i + 1, Length(Value));
  if (Length(namePart) = 0)         // @ or name missing
    or ((Length(serverPart) < 4))   // name or server missing or
    then Exit;                      // too short
  i:= Pos('.', serverPart);
  // must have dot and at least 3 places from end
  if (i < 2) or (i > (Length(serverPart) - 2)) then
    Exit;
  Result:= CheckAllowed(namePart) and CheckAllowed(serverPart);
end;

procedure MyWriteLn(s: string);
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


function MyCopy(b: array of byte; index, len: Integer): string;
var i: Integer;
begin
  Result:= '';
  for i:= index to index+len -1 do
    Result:= Result+Chr(b[i]);
end;

function myRand(mini, maxi: Integer): Integer;
begin
  Result:= Random(maxi-mini+1)+mini;
end;

function RPos(SubStr: Char; Str: String): Integer;
var m, i: Integer;
begin
  Result:= 0;
  m:= length(Str);
  for i:= m downto 1 do
    if Str[i] = SubStr Then
    begin
      Result:= i;
      exit;
    end;
end;

function RTrimCRLF(const s: string): string;
var db, i, j: Integer;
begin
  j:= length(s);
  db:= 0;
  for i:= j downto 1 do
  begin
    if not (s[i] in [#13,#10]) then
      break;
    inc(db)
  end;
  Result:= Copy(s, 1, j-db);
end;
function ParseResponseCode(s: string): Integer;
var p, l: Integer;
begin
  Result:= 0;
  s:= RTrimCRLF(s);
  p:= RPos(#13, s);
  l:= length(s);
  if (p <= l-3) then
  begin
    inc(p);
    if (s[p] in [#13, #10]) then inc(p);

    Result:= StrToIntDef(Copy(s, p, 3), 0);
    if ((l > 3) and (s[p+3] <> ' ')) then inc(Result, 1000);// and (p + 3 <= l)
  end;
end;

function CombineDirectories(dir1, dir2: string): string;
begin
  if dir1 <> '' then
    Result:= MyIncludeTrailingSlash(dir1)+dir2
  else
    Result:= dir2;
end;
function MyIncludeTrailingSlash(s: string): string;
begin
  if length(s) > 0 then
  begin
    Result:= s;
    if Result[length(s)] <> '/' then
      Result:= Result + '/';
  end else
    Result:= '/';
end;

function ParsePasvString(s: string; var host: string; var port: Integer): boolean;
begin
  Result:= False;

  s:= Copy(s, Pos('(', s)+1, 100000);
  s:= Copy(s, 1, Pos(')', s)-1);
  if s = '' then exit;
  
  host:= Fetch(s, ',')+'.'+
         Fetch(s, ',')+'.'+
         Fetch(s, ',')+'.'+
         Fetch(s, ',');
  if s = '' then exit;

  port:= StrToIntDef(Fetch(s, ','), 0)*256+StrToIntDef(Fetch(s, ','), 0);
  if port = 0 then exit;
  Result:= True;
end;

function Szam(c: char): Boolean;
begin
  Result:= ((c >= '0') and (c <= '9'))
end;
function Szamokszama(s: string): Integer;
var i: Integer;
begin
  Result:= 0;
  for i:= 1 to length(s) do
    if(Szam(s[i])) then
      inc(Result);
end;
function CsakSzamok(s: string): string;
var i: Integer;
begin
  Result:= '';
  for i:= 1 to length(s) do
    if not (s[i] in ['/','-','0'..'9']) then
      exit
    else
    if(Szam(s[i])) then
      Result:= Result + s[i];
end;

function GetFileContents(fn: string):string;
var x: TextFile;
    s: string;
begin
  if FileExists(fn) then
  begin
    Result:= '';
    AssignFile(x, fn);
    Reset(x);
    while not eof(x) do
    begin
      ReadLn(x, s);
      Result:= Result + s + #13#10;
    end;

  end else
    Result:= '';
end;



function Fetch(var osszes: string; const Args: array of char): string; 
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

function Fetch(var osszes: string; sep: char): string;
begin
  Result:= Fetch(osszes, [sep]);
end;

function Elsosor(var osszes: string): string;
begin
  Result:= Fetch(osszes, [#13,#10]);
end;


function todaycsere(const s: string; datum: TDateTime = 0): string;
var yyyy,yy,mm,dd, ww: string;
begin
  if datum = 0 then
    datum:= Now();
    
    yyyy:= Format('%.4d', [YearOf(datum)]);
    yy:= Copy(yyyy, 3, 2);
    mm:= Format('%.2d', [MonthOf(datum)]);
    dd:= Format('%.2d', [DayOf(datum)]);
    ww:= Format('%.2d', [WeekOf(datum)]);


     Result:= s;
     Result:= Csere(Result, '<yyyy>', yyyy);
     Result:= Csere(Result, '<yy>', yy);
     Result:= Csere(Result, '<mm>', mm);
     Result:= Csere(Result, '<dd>', dd);
     Result:= Csere(Result, '<ww>', ww);
end;

function InArray(const s: string ; const d: array of string; casesensitive: Boolean = True): Boolean;
var i: Integer;
begin
  Result:= True;
  
  for i:= low(d) to high(d) do
  begin
    if casesensitive then
    begin
      if d[i] = s then
        exit;
    end else
    if AnsiSameText(d[i], s) then
      exit;
  end;

  Result:= False;
end;

{$WARNINGS OFF}
function DateTimeAsString(const aThen:TDateTime; padded: Boolean = False):string;
var
i,seci,mini,houri,dayi,weeki,monthi,yeari:Int64;
imsg,secs,mins,hours,days,weeks,months,years:string;
begin
result:='-1';
if (aThen = 0) then
  exit;

seci:=SecondsBetween(now,aThen);
mini:=MinutesBetween(now,aThen);
houri:=HoursBetween(now,aThen);
dayi:=DaysBetween(now,aThen);
weeki:=WeeksBetween(now,aThen);
monthi:=MonthsBetween(now,aThen);
//yeari:=YearsBetween(now,aThen);
try
//sec
if seci >= 60 then begin
mini:=seci div 60;
seci:=seci-(mini * 60);
end else seci:=seci;
//min
if mini > 60 then begin
houri:=mini div 60;
mini:=mini-(houri * 60);
end else mini:=mini;
if mini = 60 then mini:=0;

//hour
if houri > 24 then begin
dayi:=houri div 24;
houri:=houri-(dayi * 24);
end else houri:=houri;
if houri = 24 then houri:=0;

//day
(*
if dayi > 7 then begin
weeki:=dayi div 7;
dayi:=dayi-(weeki * 7);
end else dayi:=dayi;
*)
if dayi > 7 then begin
weeki:=dayi div 7;
i:=weeki * 7;
if dayi <> i then dayi:=dayi-(weeki * 7) else dayi:=0;
end else dayi:=dayi;
if dayi = 7 then dayi:=0;

//week
if weeki > 4 then begin
monthi:=weeki div 4;
i:=monthi * 4;
if weeki <> i then
weeki:=weeki-(monthi * 4) else weeki:=0;
end else weeki:=weeki;
if weeki = 4 then weeki:=0;

//month
if monthi >= 12 then begin
yeari:=monthi div 12;
monthi:=monthi-(yeari * 12);
end else monthi:=monthi;
if monthi = 12 then monthi:=0;
//year
yeari:=monthi div 12;

if padded then
begin
if seci = 1 then secs:=Format('%2d second',[seci]) else secs:=Format('%2d seconds',[seci]);
if mini = 1 then mins:=Format('%2d minute ',[mini]) else mins:=Format('%2d minutes ',[mini]);
if houri = 1 then hours:=Format('%2d hour ',[houri]) else hours:=Format('%2d hours ',[houri]);
if dayi = 1 then days:=Format('%d day ',[dayi]) else days:=Format('%d days ',[dayi]);
if weeki = 1 then weeks:=Format('%d week ',[weeki]) else weeks:=Format('%d weeks ',[weeki]);
if monthi = 1 then months:=Format('%2d month ',[monthi]) else months:=Format('%2d months ',[monthi]);
if yeari = 1 then years:=Format('%d year ',[yeari]) else years:=Format('%d years ',[yeari]);
end else begin
if seci = 1 then secs:=Format('%d second',[seci]) else secs:=Format('%2d seconds',[seci]);
if mini = 1 then mins:=Format('%d minute ',[mini]) else mins:=Format('%2d minutes ',[mini]);
if houri = 1 then hours:=Format('%d hour ',[houri]) else hours:=Format('%2d hours ',[houri]);
if dayi = 1 then days:=Format('%d day ',[dayi]) else days:=Format('%d days ',[dayi]);
if weeki = 1 then weeks:=Format('%d week ',[weeki]) else weeks:=Format('%d weeks ',[weeki]);
if monthi = 1 then months:=Format('%d month ',[monthi]) else months:=Format('%2d months ',[monthi]);
if yeari = 1 then years:=Format('%d year ',[yeari]) else years:=Format('%d years ',[yeari]);
end;
imsg:='';
if yeari > 0 then imsg:=imsg+years;
if monthi > 0 then imsg:=imsg+months;
if weeki > 0 then imsg:=imsg+weeks;
if dayi > 0 then imsg:=imsg+days;
if houri > 0 then imsg:=imsg+hours;
if mini > 0 then imsg:=imsg+mins;
if seci > 0 then imsg:=imsg+secs;
finally
result:=imsg;
end;
end;
{$WARNINGS ON}

function WhenDidThisHappen(SecondsElapsed:int64):TDateTime;
var dtsec : TDateTime;
SecFrac : real;
begin
SecFrac :=(1/86400);
dtsec := SecondsElapsed * SecFrac;
Result := Now - dtsec;
end;

function BoolToStr(Value: Boolean; const TS, FS: String): String; overload;
begin
  if Value then Result := TS else Result := FS;
end;

function BoolToStr(Value: Boolean): String; overload;
begin
  Result := BoolToStr( Value, 'TRUE', 'FALSE' );
end;




procedure splitString(const Source: String; const Delimiter: String; const Dest: TStringlist);
var
  count: Integer;
  LStartpos, LEndepos, LSourcelength: Integer;
  LDelimiterLength : Integer;
begin
  Dest.Clear;
  count := 1;
  LStartpos := 0;
  LEndepos := 0;
  LSourcelength := length(Source);
  LDelimiterLength := Length(Delimiter);
  while count <= LSourcelength do
  begin
    if copy(Source, count, LDelimiterLength) = Delimiter then
    begin
      LEndepos := count;
      dest.Add(Trim(copy(Source, LStartpos + 1, LEndepos - LStartpos - 1)));
      LStartpos := count + LDelimiterLength - 1;
      inc(count,LDelimiterLength);
    end else
    begin
      inc(count);
    end;
  end;
  if LEndePos <> Count - LDelimiterLength then
    dest.Add(Trim(copy(Source, LStartpos + 1, count - LStartpos - 1)));
end;

end.
