unit configunit;

{ @abstract(Config Unit - Global Configuration Management)
  
  This unit handles loading and accessing the slFTP configuration file
  (slftp.cini for encrypted or slftp.ini for plain text).
  
  It provides getter functions for various configuration values with
  default fallbacks if the config file is not loaded or values are missing.
  
  The config file is loaded once at startup by @link(ConfigInit) and
  accessed globally via the @link(config) variable throughout the application.
  
  Usage:
  - Call ConfigInit(password) at startup to load the config
  - Use the config_* functions to get typed values with defaults
  - Call ConfigUninit at shutdown to cleanup
}

interface

uses
  encinifile, slmd5;

{ @section Timeout Configuration Functions }

{ Returns the connection timeout in seconds from [timeout] section.
  Default: 20 seconds if config not loaded or value not set.
  Used when establishing TCP connections to FTP/IRC servers. }
function config_connect_timeout: Integer;

{ Returns the I/O timeout in seconds from [timeout] section.
  Default: 20 seconds if config not loaded or value not set.
  Used for read/write operations on established connections. }
function config_io_timeout: Integer;

{ @section IRC Configuration Functions }

{ Returns the delay between IRC messages in milliseconds from [irc] section.
  Default: 333ms (~3 messages per second) if config not loaded.
  
  Purpose: Flood protection for IRC servers. Prevents the bot from being
  kicked for "Excess Flood" when sending many messages quickly.
  
  Note: Only applies when direct_echo=0. With direct_echo=1, messages
  are sent immediately without any delay.
  
  Typical values:
  - 333 = Conservative (default, works with most servers)
  - 100 = Aggressive (~10 msg/sec, for servers with high flood limits)
  - 0   = No delay (only for local servers or unlimited connections)
  
  @returns(Delay in milliseconds between IRC messages) }
function config_irc_spamchan_delay: Integer;

{ @section Config File Lifecycle }

{ Creates the MD5 password from decrypting password and loads settings 
  from slftp.cini/slftp.ini if existing.
  
  Loading order:
  1. First tries to load slftp.cini (encrypted) using the MD5 of aPassword
  2. Falls back to slftp.ini (plain text) if .cini doesn't exist
  
  It also sets @link(cfgloaded) to @true if ini file loading was successful.
  
  @param(aPassword Decryption password as string for .cini file)
  @returns(@true on successful loading of ini file, @false otherwise) }
function ConfigInit(var aPassword: String): Boolean;

{ Just a helper function to free @link(config) and reset @link(cfgloaded).
  Should be called during application shutdown to properly release resources. }
procedure ConfigUninit;

var
  { Global config object - provides access to all config values.
    Created by ConfigInit, destroyed by ConfigUninit.
    Use ReadString/ReadInteger/ReadBool methods to access values. }
  config: TEncIniFile;
  
  { MD5 hash of the decryption password for slftp.cini.
    Used internally by TEncIniFile to decrypt/encrypt values. }
  passphrase: TslMD5Data;

implementation

uses
  SysUtils;

const
  { Section name for timeout-related settings in config file }
  timeout = 'timeout';
  { Section name for general configuration settings }
  section = 'config';

var
  { Internal flag: @true if slftp.cini/slftp.ini loaded successfully,
    @false if file not found or decryption failed. }
  cfgloaded: boolean = False;

{ Security helper: Overwrites password string with 'x' characters
  to prevent it from remaining in memory after use.
  @param(aWipeString String which should be overwritten with value 'x') }
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

function config_irc_spamchan_delay: Integer;
begin
  if not cfgloaded then
    result := 333  // Conservative default: ~3 messages/second
  else
    result := config.ReadInteger('irc', 'spamchan_delay', 333);
end;

function ConfigInit(var aPassword: String): Boolean;
begin
  Result := True;

  // Hash the password for decryption of .cini file
  passphrase := slMD5String(aPassword);
  // Security: wipe the plaintext password from memory
  WipePass(aPassword);

  try
    // Try encrypted config first (slftp.cini)
    if FileExists(ExtractFilePath(ParamStr(0)) + 'slftp.cini') then
      config := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.cini', passphrase)
    else
      // Fall back to plain text config (slftp.ini)
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
