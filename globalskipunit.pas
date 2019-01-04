unit globalskipunit;

interface

uses
  IniFiles;

{ Just a helper function to initialize @value(globalgroupskip) and calls Rehashglobalskiplist afterwards }
procedure Initglobalskiplist;

{ Just a helper function to free @value(globalgroupskip) }
procedure Uninitglobalskiplist;

{ Reloads entries from skipgroups file and sets it to @value(globalgroupskip)
  @returns(@true on success, @false otherwise) }
function Rehashglobalskiplist: boolean;

{ Extracts groupname from @value(rls) and checks if it's in global skipped group list
  @param(rls Releasename which should be checked against skipped group lists)
  @returns(@true if in global skipped group list, @false otherwise) }
function CheckIfGlobalSkippedGroup(const rls: String): boolean;

var
  globalgroupskip: THashedStringList; //< hashed list of all global skipped groups

implementation

uses
  SysUtils, Classes, debugunit, Regexpr;

const
  section = 'globalskip';

procedure Initglobalskiplist;
begin
  Debug(dpSpam, section, 'Loading up global group skiplist...');
  globalgroupskip := THashedStringList.Create;
  Rehashglobalskiplist;
end;

procedure Uninitglobalskiplist;
begin
  if Assigned(globalgroupskip) then
  begin
    FreeAndNil(globalgroupskip);
  end;
end;

function Rehashglobalskiplist: boolean;
var
  x: TStringlist;
begin
  try
    x := TStringlist.Create;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.skipgroups');
      globalgroupskip.Clear;
      globalgroupskip.Delimiter := ' ';
      globalgroupskip.DelimitedText := x.text;
      Result := True;
    finally
      x.free;
    end;
  except on E: Exception do
    begin
      Debug(dpError, section, Format('Exception in Rehashglobalskiplist: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function CheckIfGlobalSkippedGroup(const rls: String): boolean;
var
  r: TRegexpr;
begin
  Result := False;
  r := TRegexpr.Create;
  try
    r.Expression := '\-([^\-]+)$';
    if r.Exec(rls) then
    begin
      if globalgroupskip.IndexOf(r.Match[1]) <> -1 then
        Result := True
      else
        Result := False;
    end;
  finally
    r.free;
  end;
end;

end.