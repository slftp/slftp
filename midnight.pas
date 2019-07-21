unit midnight;

interface

{ Just a helper function to init @link(ms) }
procedure MidnightInit;
{ Just a helper function to free @link(ms) }
procedure MidnightUninit;
{ Load values from inifile into corresponding variables }
procedure MidnightStart;
{ Checks if section is a midnight section and if current time is between midnight time values from inifile
  @param(aSection Sectionname)
  @returns(@true If section is a midnight section and time is between midnight values, @false otherwise) }
function IsMidnight(const aSection: String): Boolean;

implementation

uses
  DateUtils, mystrings, SysUtils, Classes, configunit, debugunit, Generics.Collections, slmasks;

var
  MidnightSections: TObjectList<TslMask>; //< midnight sections (masks) from inifile
  m_starts: TDateTime; //< Starttime of midnight
  m_ends: TDateTime; //< Endtime of midnight

const
  rsections = 'midnight';

procedure MidnightInit;
begin
  MidnightSections := TObjectList<TslMask>.Create;
end;

procedure MidnightUninit;
begin
  if Assigned(MidnightSections) then
  begin
    MidnightSections.Free;
  end;
end;

procedure MidnightStart;
var
  x: TStringList;
  i: Integer;
  fMidnightSection: String;
begin
  x := TStringList.Create;
  try
    x.CaseSensitive := False;
    x.Delimiter := ',';
    // ignore dupe entries
    x.Duplicates := dupIgnore;

    x.DelimitedText := config.ReadString(rsections, 'sections', 'MP3-*, 0DAY, PDA, /(A|E)BOOK/i, XXXPIX, MV');

    for i := 0 to x.Count - 1 do
    begin
      fMidnightSection := x[i];
      MidnightSections.Add(TslMask.Create(fMidnightSection));
    end;
  finally
    x.Free;
  end;
  Debug(dpSpam, rsections, Format('Loaded %d sections', [MidnightSections.Count]));

  m_starts := MyStrToTime(config.ReadString(rsections, 'starts', '23:45'));
  m_ends := MyStrToTime(config.ReadString(rsections, 'ends', '00:15'));
end;

function IsMidnight(const aSection: String): Boolean;
var
  m: TDateTime;
  fMidnightMask: TslMask;
begin
  Result := False;

  m := Timeof(Now);
  if ((m >= m_ends) and (m <= m_starts)) then
    exit;

  for fMidnightMask in MidnightSections do
  begin
    if fMidnightMask.Matches(aSection) then
    begin
      // it's midnight
      Result := True;
      exit;
    end;
  end;
end;

end.
