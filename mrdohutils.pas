unit mrdohutils;

interface

uses
  encinifile;

procedure InitmRdOHConfigFiles;
procedure UninitmRdOHConfigFiles;

{ Creates a TFileStream for @value(FileName) and returns the size of it
  @param(FileName Name of the file from which you want to know the size)
  @returns(filesize on success, else -1) }
function GetLocalFileSize(const FileName: String): Int64;
{ Checks if each file from @link(common.inc) exist and does also some size checks }
function CommonFileCheck: String;

var
  preurls: TEncStringlist;
  spamcfg: TEncIniFile;

implementation

uses
  SysUtils, Classes;

  {$I common.inc}

procedure InitmRdOHConfigFiles;
begin
  spamcfg := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.spamconf', '');
end;

procedure UninitmRdOHConfigFiles;
begin
  if Assigned(spamcfg) then
  begin
    FreeAndNil(spamcfg);
  end;
end;

function GetLocalFileSize(const FileName: String): Int64;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    try
      Result := FileStream.Size;
    except
      Result := -1; // file not found, better than 0 as 0 could mean empty file
    end;
  finally
    FileStream.Free;
  end;
end;

function CommonFileCheck: String;
var
  fPath: String;
  i, fsize: Integer;
  finiFound: boolean;
begin
  Result := '';
  finiFound := false;
  fsize := 0;
  fPath := ExtractFilePath(ParamStr(0));

  for i := 0 to iFileCount do
  begin
    if FileExists(fPath + iniFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + iniFiles[i]);
      if (fsize > 0) then
      begin
        finiFound := true;
      end;
    end;
  end;

  if not finiFound then
  begin
    Result := Format('slftp inifile (missing or 0 byte) %s',[#10#13]);
  end;

  for i := 0 to cFilecount do
  begin
    if FileExists(fPath + commonFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + commonFiles[i]);
      if (fsize <= 0) then
      begin
        Result := Format('%s %s (0 byte) %s',[Result, commonFiles[i], #10#13]);
      end;
    end
    else
    begin
      if commonFiles[i] = 'mirktrade.conf' then
        continue;

      Result := Format('%s %s (missing) %s',[Result, commonFiles[i], #10#13]);
    end;
  end;

  // no special case for missing files as it will be created when needed
  for i := 0 to gFilecount do
  begin
    if FileExists(fPath + generatedFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + generatedFiles[i]);
      if (fsize <= 0) then
      begin
        Result := Format('%s %s (0 byte) %s',[Result, generatedFiles[i], #10#13]);
      end;
    end;
  end;

  if Result <> '' then
  begin
    Result := Format('slFtp can not start with/without following files: %s%s',[#10#13, Result]);
  end;
end;

end.

