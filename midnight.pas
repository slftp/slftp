unit midnight;

interface

procedure MidnightInit;
procedure MidnightUninit;
procedure MidnightStart;
function IsMidnight(section: AnsiString): Boolean;

implementation

uses DateUtils, mystrings, SysUtils, Classes, configunit;

var ms: TStringList;
    m_starts: TDateTime;
    m_ends: TDateTime;

const rsections='midnight';

function IsMidnight(section: AnsiString): Boolean;
var m: TDateTime;
begin
  Result:= False;
  if ms.IndexOf(section) = -1 then exit;

  m:= Timeof(Now);

  if ((m >= m_ends) and (m <= m_starts)) then exit;

  Result:= True;
end;

procedure MidnightInit;
begin
  ms:= TStringList.Create;
  ms.Delimiter:=',';
end;

procedure MidnightUninit;
begin
  ms.Free;
end;

procedure MidnightStart;
begin
  ms.DelimitedText:= config.ReadString(rsections, 'sections', '');
  m_starts:= MyStrToTime(config.ReadString(rsections, 'starts', '23:30'));
  m_ends:= MyStrToTime(config.ReadString(rsections, 'ends', '00:30'));
end;

end.
