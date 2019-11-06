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

implementation

uses
  Classes, SysUtils, mystrings, configunit, debugunit, FLRE, SyncObjs;

const
  section = 'tags';

var
  cs: TCriticalSection; // lock to avoid concurrent access to regex objects
  crc, cri: TFLRE; //< complete and incomplete regex object

{ Fast search for '% complete' in given @link(aFilename) and determines the percentage if found
  @param(aFilename complete dir/file)
  @returns(tctUNMATCHED if '% complete' not found, tctCOMPLETE if it's done (100%), otherwise tctINCOMPLETE.) }
function _CheckStandardPercentDir(const aFilename: String): TTagCompleteType;
var
  i, j, max: Integer;
  fFoundNumber: Boolean;
begin
  Result := tctUNMATCHED;

  i := Pos(UpperCase('% complete'), UpperCase(aFilename));
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

function TagComplete(const aFilename: String): TTagCompleteType;
begin
  // check if the dir is a percent dir
  Result := _CheckStandardPercentDir(aFilename);
  if Result <> tctUNMATCHED then
    exit;

  // regex matching
  cs.Enter;
  try
    // is the file/dir a complete tag
    try
      if crc.Find(aFilename) <> 0 then
      begin
        Debug(dpSpam, section, 'TagComplete By FLRE %s', [aFilename]);
        Result := tctCOMPLETE;
        exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TagComplete(crc): Exception : %s', [e.Message]));
      end;
    end;

    // is the file/dir an incomplete tag
    try
      if cri.Find(aFilename) <> 0 then
      begin
        Debug(dpSpam, section, 'TagIncomplete By FLRE %s', [aFilename]);
        Result := tctINCOMPLETE;
        exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TagComplete(cri): Exception : %s', [e.Message]));
      end;
    end;
  finally
    cs.Leave;
  end;
end;

procedure TagsInit;
var
  complete_regex, incomplete_regex: String;
  complete_regex_default, incomplete_regex_default: String;
  dummy_string: String;
begin
  Debug(dpSpam, section, 'Init %s begins', [section]);

  complete_regex_default := '([^\w]*100%[^\w]*)|([^\w]*-\sCOMPLETE\s\)[^\w]*)|([^\w]*-\sCOMPLETE\s-[^\w]*)|([^\w].*DONE\s\-\>\s\d+F[^\w]*)|((\dM\s*\dF.*?|\d+[\w]*[^\w]*\d+[\w]*[^\w]*)(DONE|COMPLETE$|FINISH)|(\d+F[^\w].*DONE)|COMPLETE\-\d+M\_\d+F|Completed!.*?\d+)';
  incomplete_regex_default := '(\d{1,2}\s*%\s*Complete|incomplete|\d{1,2}%|\-\s*\d{1,2}DONE)';

  dummy_string := '[xy] - ( 19M 4F - COMPLETE ) - [xy]';

  cs := TCriticalSection.Create;

  // check custom slftp.ini complete_regex
  complete_regex := config.ReadString(section, 'complete_regex', complete_regex_default);

  crc := TFLRE.Create(complete_regex, [rfIGNORECASE]);
  try
    crc.Test(dummy_string);
  except
    on e: Exception do
    begin
      if Assigned(crc) then
        crc.Free;

      Debug(dpError, section, Format('TagComplete: slftp.ini complete_regex is invalid. Falling back to default. (Exception :%s)', [e.Message]));
      crc := TFLRE.Create(complete_regex_default, [rfIGNORECASE]);
    end;
  end;

  // check custom slftp.ini incomplete_regex
  incomplete_regex := config.ReadString(section, 'incomplete_regex', incomplete_regex_default);

  cri := TFLRE.Create(incomplete_regex, [rfIGNORECASE]);
  try
    cri.Test(dummy_string);
  except
    on e: Exception do
    begin
      if Assigned(cri) then
        cri.Free;

      Debug(dpError, section, Format('TagComplete: slftp.ini incomplete_regex is invalid. Falling back to default. (Exception :%s)', [e.Message]));
      cri := TFLRE.Create(incomplete_regex_default, [rfIGNORECASE]);
    end;
  end;

  Debug(dpSpam, section, 'Init %s done', [section]);
end;

procedure TagsUninit;
begin
  Debug(dpSpam, section, 'Uninit %s begins', [section]);

  cs.Enter;
  try
    FreeAndNil(crc);
    FreeAndNil(cri);
  finally
    cs.Leave;
  end;
  cs.Free;

  Debug(dpSpam, section, 'Uninit %s done', [section]);
end;

end.
