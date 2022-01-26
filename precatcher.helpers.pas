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

{ Check if the sitebot announce contains the MP3 genre for the release and extract it if available
  @param(aChanAnnounce Announce text from sitebot that might contain the MP3 Genre)
  @returns(MP3 Genre if found, otherwise empty) }
function TryToExtractMP3GenreFromSitebotAnnounce(const aChanAnnounce: String): String;

implementation

uses
  SysUtils, StrUtils, Regexpr, mystrings, precatcher, kb.releaseinfo;

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

function TryToExtractMP3GenreFromSitebotAnnounce(const aChanAnnounce: String): String;
var
  i: Integer;
  fGenreVariant1, fGenreVariant2: String;
begin
  Result := '';
  for i := 0 to GlMP3Genres.Count - 1 do
  begin
  {
  * TODO
  * only useful if we add an extra event for GENRE (need to pass ts_data to this function then)
    * [info][mp3] Keller_Williams_Kwahtro-Sync-WEB-2017-ENTiTLED remaining(122.4MB) Rock(2017)
    * ( MP3 )-( Presk_-_2BXPRZD-(SOHASOMRGWLD01)-WEB-2017-HQEM )-( Expecting 4F of 320kbps Techno from 2017 )
    x := ts_data.IndexOf(GlMP3Genres[i]);
    if x <> -1 then
    begin
      Result := GlMP3Genres[i];
      Debug(dpError, rsections, Format('_findMP3GenreOnAnnounce TStringList %s %s', [text, Result]));
    end;

    note: Can't we !catchadd a line with event UPDATE to handle this line ?
    for cleanliness its better to have an extra event for GENRE but not sure how much extra work this is compared to the UPDATE
  }

    fGenreVariant1 := GlMP3Genres[i];
    // replace possible whitespaces
    fGenreVariant2 := ReplaceText(GlMP3Genres[i], ' ', '');

    if (ContainsText(aChanAnnounce, fGenreVariant1) or ContainsText(fGenreVariant2, aChanAnnounce)) then
    begin
      Result := GlMP3Genres[i];
      break;
    end;
  end;
end;

end.

