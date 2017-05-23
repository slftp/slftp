unit tags;

interface

procedure TagsInit;
procedure TagsUninit;
function TagComplete(const filename: AnsiString): Integer;
function CheckStandardPercentDir(const filename: AnsiString): Integer;

implementation

uses Classes, SysUtils, mystrings, configunit, debugunit, FLRE, SyncObjs;

const
  section = 'tags';

var
  cs: TCriticalSection;
  crc, cri: TFLRE;

function TagComplete(const filename: AnsiString): Integer;
begin
  (*
    Result:
       0 if nothing matched (normal file/dir)
       1 if file/dir is a complete tag
      -1 if file/dir is an incomplete tag
  *)

  // check if the dir is a percent dir
  Result := CheckStandardPercentDir(filename);
  if Result <> 0 then
    exit;

  // regex matching
  cs.Enter;
  try
    // is the file/dir a complete tag
    try
      if crc.Find(filename) <> 0 then
      begin
        Debug(dpSpam, section, 'TagComplete By FLRE %s', [filename]);
        Result := 1;
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
      if cri.Find(filename) <> 0 then
      begin
        Debug(dpSpam, section, 'TagIncomplete By FLRE %s', [filename]);
        Result := -1;
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

function CheckStandardPercentDir(const filename: AnsiString): Integer;
var
  i, j: Integer;
  voltszam: Boolean;
begin
  Result := 0;

  i := AnsiPos(AnsiUpperCase('% complete'), AnsiUpperCase(filename));
  if i > 4 then
  begin
    voltszam := False;
    for j := 1 to 4 do
      if ((not voltszam) and (filename[i-j] = ' ')) then
        Continue
      else
      begin
        voltszam := True;
        if (filename[i-j] < '0') or (filename[i-j] > '9') then
        begin
          i := StrToIntDef(Trim(Copy(filename, i-j+1, j-1)), -1);
          break;
        end;
      end;
    if i = 100 then
    begin
      Result := 1;
      exit;
    end else
    begin
      Result := -1;
      exit;
    end;
  end;
end;

procedure TagsInit;
var
  complete_regex, incomplete_regex: String;
  complete_regex_default, incomplete_regex_default: String;
  dummy_string: String;
begin
  Debug(dpSpam, section, 'Init %s begins', [section]);

  complete_regex_default := '([^\w]*100%[^\w]*)|([^\w]*-\sCOMPLETE\s\)[^\w]*)|([^\w]*-\sCOMPLETE\s-[^\w]*)';
  incomplete_regex_default := '(\d{1,2}\s*%\s*Complete|incomplete)';

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
