unit configunit;

interface

uses
  encinifile, slmd5;

function config_io_timeout: Integer;
function config_connect_timeout: Integer;
{ Creates the MD5 password from decrypting password and loads settings from slftp.cini/slftp.ini if existing,
  it also sets @link(cfgloaded) to @true if ini file loading was successful.
  @param(aPassword Decryption password as string)
  @returns(@true on successful loading of ini file, @false otherwise) }
function ConfigInit(var aPassword: String): Boolean;
{ Just a helper function to free @link(config) and reset @link(cfgloaded) }
procedure ConfigUninit;

var
  config: TEncIniFile; //< config values from loaded slftp.cini/slftp.ini
  passphrase: TslMD5Data; //< MD5 hash of decryption password for slftp

implementation

uses
  SysUtils;

const
  timeout = 'timeout';
  section = 'config';

var
  cfgloaded: boolean = False; //< @true if slftp.cini/slftp.ini is decrypted and loaded, @false if not existing or failed to load

{ Sets the length of @param(p) to 100 and fills all of it with char 'x'
  @param(aWipeString String which should be overwritten with value 'x')
}
procedure WipePass(var aWipeString: String);
begin
  SetLength(aWipeString, 100);
  aWipeString := StringOfChar('x', Length(aWipeString));
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

function ConfigInit(var aPassword: String): Boolean;
begin
  Result := True;

  passphrase := slMD5String(aPassword);
  WipePass(aPassword);

  try
    if FileExists(ExtractFilePath(ParamStr(0)) + 'slftp.cini') then
      config := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.cini', passphrase)
    else
      config := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.ini', '');
  except
    Result := False;
  end;

  cfgloaded := Result;
end;

procedure ConfigUninit;
begin
  cfgloaded := False;
  config.Free;
end;

end.
