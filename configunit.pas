unit configunit;

interface

uses
  encinifile, slmd5;

procedure ReadPass;
function config_io_timeout: Integer;
function config_connect_timeout: Integer;
function ConfigInit(var p: String): Boolean;
procedure ConfigUninit;

var
  config: TEncIniFile;
  passphrase: TslMD5Data;

implementation

uses
  SysUtils, helper;

const
  timeout = 'timeout';
  section = 'config';

var
  cfgloaded: boolean = False;

procedure ReadPass;
var
  pw: String;
begin
  pw := MyGetPass('Password: ');
  if pw = '' then
    halt;
  passphrase := slMD5String(pw);
end;

procedure WipePass(var p: String);
var
  i: Integer;
begin
  SetLength(p, 100);
  for i := 1 to 100 do
    p[i] := 'x';
end;

function config_connect_timeout: Integer;
begin
  if not cfgloaded then
    result := 20
  else
    result := config.ReadInteger(timeout, 'connect', 20);
end;

function config_io_timeout: Integer;
begin
  if not cfgloaded then
    result := 20
  else
    result := config.ReadInteger(timeout, 'io', 20);
end;

function ConfigInit(var p: String): Boolean;
begin
  Result := True;

  passphrase := slMD5String(p);
  WipePass(p);
  try
    if FileExists(ExtractFilePath(ParamStr(0))+'slftp.cini') then
      config := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.cini', passphrase)
    else
      config := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.ini', '');
  except
    Result := False;
  end;

  cfgloaded := result;
end;

procedure ConfigUninit;
begin
  config.Free;
  cfgloaded := False;
end;

end.
