unit mygrouphelpers;

interface

{ Extracts groupname from release
  @param(aRlz releasename)
  @returns(Groupname including _(INT|WEB) etc from input releasename) }
function GetGroupname(const aRlz: String): String;

{ Removes '-groupname*' from release
  @param(aRlz releasename)
  @returns(Releasename without '-groupname*') }
function RemoveGroupname(const aRlz: String): String;

{ Removes the internal marker from groupname
  @param(aGroupname Groupname)
  @returns(Releasename without groupname internal marker) }
function RemoveINT(const aGroupname: String): String;

{ Removes the WEB marker from groupname
  @param(aGroupname Groupname)
  @returns(Releasename without groupname WEB marker) }
function RemoveWEB(const aGroupname: String): String;

implementation

uses
  Classes, SysUtils, StrUtils, regexpr;

function GetGroupname(const aRlz: String): String;
var
  x: TStringList;
  s: String;
begin
  s := ReplaceText(aRlz, '(', '');
  s := ReplaceText(s, ')', '');
  s := ReplaceText(s, '.', ' ');
  s := ReplaceText(s, '-', ' ');
  s := ReplaceText(s, '_', ' ');

  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := s;
    if UpperCase(x.Strings[x.Count - 1]) = 'INT' then
      Result := x.Strings[x.Count - 2] + '_' + x.Strings[x.Count - 1]
    else
      Result := x.Strings[x.Count - 1];
  finally
    x.Free;
  end;
end;

function RemoveGroupname(const aRlz: String): String;
var
  fGroup: String;
begin
  fGroup := GetGroupname(aRlz);
  Result := ReplaceText(aRlz, '-' + fGroup, '');
end;

function RemoveINT(const aGroupname: String): String;
var
  r: TRegexpr;
begin
  Result := aGroupname;
  r := TRegexpr.Create;
  try
    r.ModifierI := True;
    r.Expression := '[\-\_]int$';
    Result := r.Replace(aGroupname, '', False);
  finally
    r.free;
  end;
end;

function RemoveWEB(const aGroupname: String): String;
var
  r: TRegexpr;
begin
  Result := aGroupname;
  r := TRegexpr.Create;;
  try
    r.ModifierI := True;
    r.Expression := '[\-\_]web$';
    Result := r.Replace(aGroupname, '', False);
  finally
    r.free;
  end;
end;

end.
