unit globalskipunit;

interface

uses classes, Regexpr;

procedure Initglobalskiplist;
procedure Uninitglobalskiplist;

function Rehashglobalskiplist: boolean;

function CheckForBadAssGroup(rls: string): boolean;

var
  globalgroupskip: TStringlist;

implementation

uses debugunit, sysutils, irc;

procedure Initglobalskiplist;
begin
  Debug(dpSpam, 'global_skip_group', 'Loading up Global group skiplist..');
  globalgroupskip := TStringlist.Create;
  Rehashglobalskiplist;
end;

procedure Uninitglobalskiplist;
begin
  if globalgroupskip <> nil then
  begin
    globalgroupskip.free;
    globalgroupskip := nil;
  end;
end;

function Rehashglobalskiplist: boolean;
var
  x: TStringlist;
begin
  result := False;
  try
    x := TStringlist.Create;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.skipgroups');
      globalgroupskip.Clear;
      globalgroupskip.Delimiter := ' ';
      globalgroupskip.DelimitedText := x.text;
      result := True;
    finally
      x.free;
    end;
  except on E: Exception do
    begin
      Debug(dpError, section, format('Exception in Rehashglobalskiplist: %s',
        [E.Message]));
      result := False;
    end;

  end;
end;

function CheckForBadAssGroup(rls: string): boolean;
var
  r: TRegexpr; //i:integer;
begin
  result := False;
  r := TRegexpr.Create;
  r.Expression := '\-([^\-]+)$';
  if r.Exec(rls) then
  begin
    if globalgroupskip.IndexOf(r.Match[1]) <> -1 then
      result := True
    else
      result := False;
  end;
  r.free;
end;

end.

