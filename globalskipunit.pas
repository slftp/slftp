unit globalskipunit;

interface

uses classes, Regexpr;

procedure Initglobalskiplist;
procedure Uninitglobalskiplist;

function Rehashglobalskiplist: boolean;

function CheckForBadAssGroup(const rls: String): boolean;

var
  globalgroupskip: TStringlist;

implementation

uses debugunit, sysutils;

const
  section = 'globalskip';

procedure Initglobalskiplist;
begin
  Debug(dpSpam, section, 'Loading up Global group skiplist..');
  globalgroupskip := TStringlist.Create;
  Rehashglobalskiplist;
end;

procedure Uninitglobalskiplist;
begin
  if globalgroupskip <> nil then
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

function CheckForBadAssGroup(const rls: String): boolean;
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

