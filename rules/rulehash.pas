unit rulehash;

interface

function HashRuleLine(const Line: String; IgnoreCase, IgnoreBlanks: Boolean): Cardinal;

implementation

uses
  SysUtils, mormot.core.base, mormot.core.unicode, mormot.lib.z;

function HashRuleLine(const Line: String; IgnoreCase, IgnoreBlanks: Boolean): Cardinal;
var
  i, j: Integer;
  Normalized: String;
  Utf8Line: RawUtf8;
begin
  Normalized := Line;
  if IgnoreBlanks then
  begin
    j := 1;
    for i := 1 to Length(Line) do
      if not (Line[i] in [#9, #32]) then
      begin
        Normalized[j] := Line[i];
        Inc(j);
      end;
    SetLength(Normalized, j - 1);
  end;
  if IgnoreCase then
    Normalized := AnsiLowerCase(Normalized);
  Utf8Line := StringToUtf8(Normalized);
  Result := crc32(0, Pointer(Utf8Line), Length(Utf8Line));
end;

end.

