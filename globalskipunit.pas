unit globalskipunit;

interface

uses classes, Regexpr;

procedure Initglobalskiplist;
procedure Uninitglobalskiplist;

function Rehashglobalskiplist: boolean;

function CheckForBadAssGroup(const rls: string): boolean;

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
      Debug(dpError, 'globalskip', format('Exception in Rehashglobalskiplist: %s',
        [E.Message]));
      result := False;
    end;
  end;
end;

function CheckForBadAssGroup(const rls: string): boolean;
var
  r: TRegexpr;
begin
  result := False;
  r := TRegexpr.Create;
  try
    r.Expression := '\-([^\-]+)$';
    if r.Exec(rls) then
    begin
      if globalgroupskip.IndexOf(r.Match[1]) <> -1 then
        result := True
      else
        result := False;
    end;
  finally
    r.free;
  end;
end;

end.

