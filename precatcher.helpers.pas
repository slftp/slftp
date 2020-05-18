unit precatcher.helpers;

interface

function RemoveSpecialCharsAndBareIt(const s: String): String;

{ Replaces all characters from input which are not listed in ValidChars array with whitespace
  @param(aInput String to be cleaned)
  @returns(Cleaned string) }
function StripNoValidChars(const aInput: String): String;

{ Checks if the given line of text is commented out by starting with '#' or '//'
  @param(aSingleLine Single line of string)
  @returns(@true if line is commented out, @false otherwise.) }
function IsLineCommentedOut(const aSingleLine: String): Boolean;

implementation

uses
  SysUtils, Regexpr, mystrings, precatcher;

const
  rsections = 'precatcher.helpers';
  ValidChars: set of Char = ['0'..'9', 'A'..'Z', 'a'..'z', '?', '.', '>', '<', '+', '-', '~', '!', '@', '#', '$', '%', '&', '*', '(', ')', '_', '=', '{', '}', '[', ']', '|', '\', '/', ':', ';', ' '];
  StrippingChars: set of Char = ['(', ')', '_', '-', '.', '&', '*', '<', '>'];


function RemoveSpecialCharsAndBareIt(const s: String): String;
var
  i: integer;
  skip: integer;
begin
  Result := '';
  skip := 0;
  for i := 1 to length(s) do
    if (skip = 0) then
    begin
      if (Ord(s[i]) >= 32) then
      begin
        if (Ord(s[i]) <> 255) then
        begin
          if (IsALetter(s[i]) or IsANumber(s[i]) or (s[i] in StrippingChars)) then
            Result := Result + s[i]
          else
            Result := Result + ' ';
        end
        else
          Result := Result + ' ';
      end
      else
      begin
        if ((s[i] = #3) and (i < length(s) - 2)) then
        begin
          if IsANumber(s[i + 1]) then
          begin
            if IsANumber(s[i + 2]) then
              skip := 2
            else
              skip := 1;
          end;
        end;
      end;
    end
    else
      Dec(skip);
end;

function StripNoValidChars(const aInput: String): String;
var
  I: integer;
begin
  Result := aInput;
  for I := 1 to Length(Result) do
  begin
    if not (Result[I] in ValidChars) then
      Result[I] := ' ';
  end;
end;

function IsLineCommentedOut(const aSingleLine: String): Boolean;
var
  rx: TRegExpr;
begin
  Result := False;

  rx := TRegExpr.Create;
  try
    rx.ModifierI := True;
    rx.ModifierM := True;
    rx.Expression := '^(\#|\/\/)';

    if rx.Exec(aSingleLine) then
    begin
      exit(True);
    end;
  finally
    rx.Free;
  end;
end;

end.

