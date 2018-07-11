unit midnight;

interface

{ Just a helper function to init @value(ms) }
procedure MidnightInit;
{ Just a helper function to free @value(ms) }
procedure MidnightUninit;
{ Load values from inifile into corresponding variables }
procedure MidnightStart;
{ Checks if section is a midnight section and if current time is between midnight time values from inifile
  @param(section Sectionname)
  @returns(@true If section is a midnight section and time is between midnight values, @false otherwise) }
function IsMidnight(const section: String): Boolean;

implementation

uses
  DateUtils, mystrings, SysUtils, Classes, configunit;

var
  ms: TStringList; //< midnight sections from inifile
  m_starts: TDateTime; //< Starttime of midnight
  m_ends: TDateTime; //< Endtime of midnight

const
  rsections = 'midnight';

procedure MidnightInit;
begin
  ms := TStringList.Create;
  ms.Delimiter := ',';
end;

procedure MidnightUninit;
begin
  if Assigned(ms) then
  begin
    ms.Free;
  end;
end;

procedure MidnightStart;
begin
  ms.DelimitedText := config.ReadString(rsections, 'sections', '');
  m_starts := MyStrToTime(config.ReadString(rsections, 'starts', '23:45'));
  m_ends := MyStrToTime(config.ReadString(rsections, 'ends', '00:15'));
end;

function IsMidnight(const section: String): Boolean;
var
  m: TDateTime;
begin
  Result := False;

  // is midnight section?
  // TODO: allow usage of masks like MP3*
  if ms.IndexOf(section) = -1 then
    exit;

  m := Timeof(Now);

  if ((m >= m_ends) and (m <= m_starts)) then
    exit;

  Result := True;
end;

end.
