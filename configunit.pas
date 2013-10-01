unit configunit;

interface

uses encinifile, slmd5;

procedure ReadPass;
function ConfigInit(var p: string): Boolean;
procedure ConfigUninit;
//function ConfigRehash:boolean;

var config: TEncIniFile;
    passphrase: TslMD5Data;


function config_io_timeout: Integer;
function config_connect_timeout: Integer;

implementation

uses SysUtils, helper;

const timeout = 'timeout';
      section = 'config';

var cfgloaded:boolean = False;


procedure ReadPass;
var pw: string;
begin
  pw:= MyGetPass('Password: ');
  if pw = '' then halt;
  passphrase:= slMD5String(pw);
end;

procedure WipePass(var p: string);
var i: Integer;
begin
  SetLength(p, 100);
  for i:= 1 to 100 do
    p[i]:= 'x';
end;

function config_connect_timeout: Integer;
begin
if not cfgloaded then result:=20 else
result:=config.ReadInteger(timeout, 'connect', 20);
end;
function config_io_timeout: Integer;
begin
if not cfgloaded then result:=20 else
result:=config.ReadInteger(timeout, 'io', 20);
end;

function ConfigInit(var p: string): Boolean;
begin
  Result:= True;
  passphrase:= slMD5String(p);
  WipePass(p);
  try
    if FileExists(ExtractFilePath(ParamStr(0))+'slftp.cini') then
    config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'slftp.cini', passphrase)
//    if FileExists(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.cini') then
//      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.cini', passphrase)
    else
      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'slftp.ini', '');
//      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.ini', '');

  except
    Result:= False;
  end;
    cfgloaded:=result;
  end;
procedure ConfigUninit;
begin
  config.Free;
  cfgloaded:=False;
end;
(*
function ConfigRehash:boolean;
begin
  result:=False;
try
  ConfigUninit;
    if FileExists(ExtractFilePath(ParamStr(0))+'slftp.cini') then
    config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'slftp.cini', passphrase)
//    if FileExists(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.cini') then
//      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.cini', passphrase)
    else
      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'slftp.ini', '');
//      config:= TEncIniFile.Create(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'slftp.ini', '');
finally
  result:=True;
end;
end;
  *)
end.
