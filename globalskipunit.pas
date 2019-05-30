unit globalskipunit;

interface

uses
  IniFiles;

{ Just a helper function to create @link(globalgroupskip) object and calls Rehashglobalskiplist afterwards }
procedure Initglobalskiplist;

{ Just a helper function to free @link(globalgroupskip) }
procedure Uninitglobalskiplist;

{ Reloads entries from skipgroups file, clears @link(globalgroupskip) and adds the skipgroups afterwards
  @returns(@true on success, @false otherwise) }
function Rehashglobalskiplist: boolean;

{ Extracts groupname from @link(aRls) and checks if it's in global skipped group list
  @param(aRls Releasename which should be checked against skipped group list)
  @returns(@true if in global skipped group list, @false otherwise) }
function CheckIfGlobalSkippedGroup(const aRls: String): boolean;

var
  globalgroupskip: THashedStringList; //< hashed list of all global skipped groups

implementation

uses
  SysUtils, Classes, StrUtils, debugunit, kb;

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
      globalgroupskip.DelimitedText := x.Text;

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

function CheckIfGlobalSkippedGroup(const aRls: String): boolean;
var
  fGroupname: String;
begin
  Result := False;

  fGroupname := GetGroupname(aRls);
  fGroupname := ReplaceText(fGroupname, '_INT', '');

  if globalgroupskip.IndexOf(fGroupname) <> -1 then
    Result := True
  else
    Result := False;
end;

end.