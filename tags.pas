unit tags;

interface

type
  {
  @abstract(Return types for TagComplete function)
  @value(tctUNMATCHED percent dir matching and regex matching failed)
  @value(tctINCOMPLETE it's an incomplete dir tag)
  @value(tctCOMPLETE it's a complete dir tag)
  }
  TTagCompleteType = (tctUNMATCHED, tctINCOMPLETE, tctCOMPLETE);

{ Just a helper function to init and validate user regex }
procedure TagsInit;
{ Just a helper function to uninit regex }
procedure TagsUninit;
{ Tries to find complete status of given @link(aFilename)
  @param(aFilename complete dir/file)
  @returns(@link(tctCOMPLETE) if complete, @link(tctINCOMPLETE) if incomplete, otherwise @link(tctUNMATCHED).) }
function TagComplete(const aFilename: String): TTagCompleteType;
{ Extracts the numeric percentage from a tag string (e.g. "95% Complete")
  @param(aFilename complete dir/file tag)
  @param(aPercent extracted percentage value between 0 and 100)
  @returns(@true if a percentage was found, @false otherwise.) }
function TagExtractPercent(const aFilename: String; out aPercent: Integer): Boolean;

{ Frees the thread vars of the current thread (call this when a thread terminates). }
procedure CleanupTagsThreadVars;

implementation

uses
  Classes, SysUtils, StrUtils, mystrings, configunit, debugunit, FLRE;

const
  section = 'tags';

var
  glCompleteRegex: RawByteString;
  glIncompleteRegex: RawByteString;

threadvar
  glCompleteRegexInstance, glIncompleteRegexInstance: TFLRE; //< complete and incomplete regex object

{ Case-insensitive search for '% complete' without string allocations
  (UpperCase() temporaries per checked file showed up hot in CPU profiling).
  @param(aFilename filename to search in)
  @returns(1-based position of '% complete' in aFilename, 0 if not found) }
function _PosPercentComplete(const aFilename: String): Integer;
const
  fNeedle = '% COMPLETE';
var
  i, j: Integer;
begin
  Result := 0;
  if Length(aFilename) < Length(fNeedle) then
    exit;

  for i := 1 to Length(aFilename) - Length(fNeedle) + 1 do
  begin
    Result := i;
    for j := 1 to Length(fNeedle) do
    begin
      if (UpCase(aFilename[i + j - 1]) <> fNeedle[j]) then
      begin
        Result := 0;
        break;
      end;
    end;
    if Result <> 0 then
      exit;
  end;
end;

{ Fast search for '% complete' in given @link(aFilename) and determines the percentage if found
  @param(aFilename complete dir/file)
  @returns(tctUNMATCHED if '% complete' not found, tctCOMPLETE if it's done (100%), otherwise tctINCOMPLETE.) }
function _CheckStandardPercentDir(const aFilename: String): TTagCompleteType;
var
  i, j: Integer;
  fFoundNumber: Boolean;
begin
  Result := tctUNMATCHED;

  i := _PosPercentComplete(aFilename);
  if i > 4 then
  begin
    fFoundNumber := False;
    for j := 1 to 4 do
    begin
      if ((not fFoundNumber) and (aFilename[i-j] = ' ')) then
        Continue
      else
      begin
        fFoundNumber := True;
        if (aFilename[i-j] < '0') or (aFilename[i-j] > '9') then
        begin
          i := StrToIntDef(Trim(Copy(aFilename, i-j+1, j-1)), -1);
          break;
        end;
      end;
    end;

    if i = 100 then
    begin
      Result := tctCOMPLETE;
      exit;
    end
    else
    begin
      Result := tctINCOMPLETE;
      exit;
    end;
  end;
end;

function GetCompleteRegexInstance: TFLRE;
begin
  // single threadvar read per call - every threadvar access goes through
  // pthread TLS on Linux and showed up hot in CPU profiling
  Result := glCompleteRegexInstance;
  if Result = nil then
  begin
    Result := TFLRE.Create(glCompleteRegex, [rfIGNORECASE]);
    glCompleteRegexInstance := Result;
  end;
end;

function GetIncompleteRegexInstance: TFLRE;
begin
  // single threadvar read per call - every threadvar access goes through
  // pthread TLS on Linux and showed up hot in CPU profiling
  Result := glIncompleteRegexInstance;
  if Result = nil then
  begin
    Result := TFLRE.Create(glIncompleteRegex, [rfIGNORECASE]);
    glIncompleteRegexInstance := Result;
  end;
end;

function TagComplete(const aFilename: String): TTagCompleteType;
begin
  // check if the dir is a percent dir
  Result := _CheckStandardPercentDir(aFilename);
  if Result <> tctUNMATCHED then
    exit;

  // single try/except for both regex checks - an exception frame per checked
  // file showed up hot in CPU profiling
  try
    // is the file/dir a complete tag
    if GetCompleteRegexInstance.Find(RawByteString(aFilename)) <> 0 then
    begin
      Debug(dpSpam, section, 'TagComplete By FLRE %s', [aFilename]);
      Result := tctCOMPLETE;
      exit;
    end;

    // is the file/dir an incomplete tag
    if GetIncompleteRegexInstance.Find(RawByteString(aFilename)) <> 0 then
    begin
      Debug(dpSpam, section, 'TagIncomplete By FLRE %s', [aFilename]);
      Result := tctINCOMPLETE;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TagComplete: Exception : %s', [e.Message]));
    end;
  end;
end;

function TagExtractPercent(const aFilename: String; out aPercent: Integer): Boolean;
var
  idx, startIdx, endIdx: Integer;
  numStr: String;
  tempPercent: Integer;
begin
  Result := False;
  aPercent := -1;
  if aFilename = '' then
    exit;

  // get the position of the first '%'
  idx := Pos('%', aFilename);
  while idx > 1 do
  begin
    startIdx := idx - 1;
    while (startIdx >= 1) and (aFilename[startIdx] = ' ') do
      Dec(startIdx);

    if (startIdx < 1) or not (aFilename[startIdx] in ['0'..'9']) then
    begin
      // we reached the start of the string or a char that is not a number, try to find another
      // '%' at a higher position inside of the string.
      idx := PosEx('%', aFilename, idx + 1);
      Continue;
    end;

    // find the beginning of the actual percent number
    endIdx := startIdx;
    while (startIdx >= 1) and (aFilename[startIdx] in ['0'..'9']) do
      Dec(startIdx);
    Inc(startIdx);

    // get the number from the string with the start and end index we just determined
    numStr := Trim(Copy(aFilename, startIdx, endIdx - startIdx + 1));
    if TryStrToInt(numStr, tempPercent) and (tempPercent >= 0) and (tempPercent <= 100) then
    begin
      // we were able to parse and it's a valid percent number
      aPercent := tempPercent;
      Result := True;
      exit;
    end;

    // if we reach this place, no valid percent number was found before the '%' char
    // try to find another '%' at a higher position inside of the string.
    idx := PosEx('%', aFilename, idx + 1);
  end;
end;

procedure TagsInit;
var
  complete_regex_default, incomplete_regex_default: String;
  dummy_string: RawByteString;
  fTestingRegexInstance: TFLRE;
begin
  Debug(dpSpam, section, 'Init %s begins', [section]);

  complete_regex_default := '([^\w]*100%[^\w]*)|([^\w]*-\sCOMPLETE\s\)[^\w]*)|([^\w]*-\sCOMPLETE\s-[^\w]*)|([^\w].*DONE\s\-\>\s\d+F[^\w]*)|((\dM\s*\dF.*?|\d+[\w]*[^\w]*\d+[\w]*[^\w]*)(DONE|COMPLETE$|FINISH)|(\d+F[^\w].*DONE)|COMPLETE\-\d+M\_\d+F|Completed!.*?\d+)';
  incomplete_regex_default := '(\d{1,2}\s*%\s*Complete|incomplete|\d{1,2}%|\-\s*\d{1,2}DONE)';

  dummy_string := '[xy] - ( 19M 4F - COMPLETE ) - [xy]';

  // check custom slftp.ini complete_regex
  glCompleteRegex := RawByteString(config.ReadString(section, 'complete_regex', complete_regex_default));

  fTestingRegexInstance := TFLRE.Create(glCompleteRegex, [rfIGNORECASE]);
  try
    fTestingRegexInstance.Test(dummy_string);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('TagComplete: slftp.ini complete_regex is invalid. Falling back to default. (Exception :%s)', [e.Message]));
      glCompleteRegex := RawByteString(complete_regex_default);
    end;
  end;

  if Assigned(fTestingRegexInstance) then
    FreeAndNil(fTestingRegexInstance);

  // check custom slftp.ini incomplete_regex
  glIncompleteRegex := RawByteString(config.ReadString(section, 'incomplete_regex', incomplete_regex_default));

  fTestingRegexInstance := TFLRE.Create(glIncompleteRegex, [rfIGNORECASE]);
  try
    fTestingRegexInstance.Test(dummy_string);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('TagComplete: slftp.ini incomplete_regex is invalid. Falling back to default. (Exception :%s)', [e.Message]));
      glIncompleteRegex := RawByteString(incomplete_regex_default);
    end;
  end;

  if Assigned(fTestingRegexInstance) then
    FreeAndNil(fTestingRegexInstance);

  Debug(dpSpam, section, 'Init %s done', [section]);
end;

procedure TagsUninit;
begin
  Debug(dpSpam, section, 'Uninit %s begins', [section]);

  Debug(dpSpam, section, 'Uninit %s done', [section]);
end;

procedure CleanupTagsThreadVars;
begin
  if glCompleteRegexInstance <> nil then
    FreeAndNil(glCompleteRegexInstance);
  if glIncompleteRegexInstance <> nil then
    FreeAndNil(glIncompleteRegexInstance);
end;

end.
