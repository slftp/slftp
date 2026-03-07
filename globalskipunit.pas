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
  SysUtils, Classes, StrUtils, debugunit, kb, mygrouphelpers;

const
  section = 'globalskip';

procedure Initglobalskiplist;
begin
  Debug(dpSpam, section, 'Loading up global group skiplist...');
  globalgroupskip := THashedStringList.Create;
  globalgroupskip.CaseSensitive := False;
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
  i, j: Integer;
  s: String;
  y: TStringList;
begin
  try
    x := TStringlist.Create;
    y := TStringList.Create;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.skipgroups');

      globalgroupskip.Clear;
      for i := 0 to x.Count - 1 do
      begin
        s := Trim(x[i]);
        if (s = '') or (s[1] = '#') or (s[1] = ';') then
          continue;

        y.Delimiter := ' ';
        y.DelimitedText := s;
        for j := 0 to y.Count - 1 do
          if Trim(y[j]) <> '' then
            globalgroupskip.Add(Trim(y[j]));
      end;

      Result := True;
    finally
      x.free;
      y.free;
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
  i: Integer;
  s: String;
begin
  Result := False;

  // Method 1: Extraction based matching (standard names)
  fGroupname := GetGroupname(aRls);
  fGroupname := RemoveINT(fGroupname);
  fGroupname := RemoveWEB(fGroupname);

  if globalgroupskip.IndexOf(fGroupname) <> -1 then
  begin
    Result := True;
    Exit;
  end;

  // Method 2: Suffix based matching (robust for group names with hyphens/underscores)
  // Strip tags from end of release name to match base group name
  s := RemoveINT(aRls);
  s := RemoveWEB(s);

  for i := 0 to globalgroupskip.Count - 1 do
  begin
    fGroupname := globalgroupskip[i];
    if (fGroupname <> '') then
    begin
      // Check if release name is exactly the group name, or ends with -group, _group or .group
      if (SameText(s, fGroupname)) or
         (EndsText('-' + fGroupname, s)) or
         (EndsText('_' + fGroupname, s)) or
         (EndsText('.' + fGroupname, s)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

end.