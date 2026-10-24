unit configunit;

interface

uses
  encinifile, slmd5;

{ Returns the cached I/O timeout, or the default if the cache is not initialized. }
function config_io_timeout: Integer;
{ Returns the cached connection timeout, or the default if the cache is not initialized. }
function config_connect_timeout: Integer;
{ Initializes cached configuration values after config and spamconf are loaded. }
procedure ConfigCacheInit;
{ Frees cached configuration values. }
procedure ConfigCacheUninit;
{ Returns the cached boolean setting, or aDefault if the setting is not in the cache. }
function config_cached_bool(const aSection, aName: String; const aDefault: Boolean): Boolean;
{ Returns the cached integer setting, or aDefault if the setting is not in the cache. }
function config_cached_integer(const aSection, aName: String; const aDefault: Integer): Integer;
{ Returns the cached string setting, or aDefault if the setting is not in the cache. }
function config_cached_string(const aSection, aName, aDefault: String): String;
{ Returns the cached spamconf boolean setting, or aDefault if the setting is not in the cache. }
function spam_config_cached_bool(const aSection, aName: String; const aDefault: Boolean): Boolean;
{ Loads settings from slftp.cini/slftp.ini and returns @true on success. }
function ConfigInit(var aPassword: String): Boolean;
{ Frees the loaded configuration and resets the loaded state. }
procedure ConfigUninit;

var
  config: TEncIniFile; //< config values from loaded slftp.cini/slftp.ini
  passphrase: TslMD5Data; //< MD5 hash of decryption password for slftp

implementation

uses
  SysUtils, Classes, mrdohutils;

const
  timeout = 'timeout';
  section = 'config';

var
  glIniBools, glIniIntegers, glIniStrings, glSpamBools: TStringList;
  cfgloaded: boolean = False; //< @true if slftp.cini/slftp.ini is decrypted and loaded, @false if not existing or failed to load

{ Sets the length of @param(p) to 100 and fills all of it with char 'x'
  @param(aWipeString String which should be overwritten with value 'x')
}
procedure WipePass(var aWipeString: String);
begin
  SetLength(aWipeString, 100);
  aWipeString := StringOfChar('x', Length(aWipeString));
end;

function _CacheKey(const aSection, aName: String): String;
begin
  Result := UpperCase(aSection) + '.' + UpperCase(aName);
end;

procedure _PutCache(aList: TStringList; const aSection, aName, aValue: String);
begin
  aList.Values[_CacheKey(aSection, aName)] := aValue;
end;

function _GetCache(aList: TStringList; const aSection, aName: String; out aValue: String): Boolean;
var
  fIndex: Integer;
begin
  Result := False;
  if aList = nil then
    Exit;
  fIndex := aList.IndexOfName(_CacheKey(aSection, aName));
  if fIndex < 0 then
    Exit;
  aValue := Copy(aList[fIndex], Pos('=', aList[fIndex]) + 1, MaxInt);
  Result := True;
end;

function config_cached_bool(const aSection, aName: String; const aDefault: Boolean): Boolean;
var fValue: String;
begin
  if _GetCache(glIniBools, aSection, aName, fValue) then Result := fValue = '1'
  else Result := aDefault;
end;

function config_cached_integer(const aSection, aName: String; const aDefault: Integer): Integer;
var fValue: String;
begin
  if _GetCache(glIniIntegers, aSection, aName, fValue) then Result := StrToIntDef(fValue, aDefault)
  else Result := aDefault;
end;

function config_cached_string(const aSection, aName, aDefault: String): String;
begin
  if not _GetCache(glIniStrings, aSection, aName, Result) then Result := aDefault;
end;

function spam_config_cached_bool(const aSection, aName: String; const aDefault: Boolean): Boolean;
var fValue: String;
begin
  if _GetCache(glSpamBools, aSection, aName, fValue) then Result := fValue = '1'
  else Result := aDefault;
end;

procedure _CacheBool(const aSection, aName: String; const aDefault: Boolean);
begin _PutCache(glIniBools, aSection, aName, IntToStr(Ord(config.ReadBool(aSection, aName, aDefault)))); end;

procedure _CacheInteger(const aSection, aName: String; const aDefault: Integer);
begin _PutCache(glIniIntegers, aSection, aName, IntToStr(config.ReadInteger(aSection, aName, aDefault))); end;

procedure _CacheString(const aSection, aName, aDefault: String);
begin _PutCache(glIniStrings, aSection, aName, config.ReadString(aSection, aName, aDefault)); end;

procedure _CacheSpamBool(const aSection, aName: String; const aDefault: Boolean);
begin _PutCache(glSpamBools, aSection, aName, IntToStr(Ord(spamcfg.ReadBool(aSection, aName, aDefault)))); end;

procedure ConfigCacheInit;
var
  fSections, fKeys: TStringList;
  fSection, fKey: String;
begin
  ConfigCacheUninit;
  glIniBools := TStringList.Create;
  glIniIntegers := TStringList.Create;
  glIniStrings := TStringList.Create;
  glSpamBools := TStringList.Create;
  glIniBools.Sorted := True;
  glIniIntegers.Sorted := True;
  glIniStrings.Sorted := True;
  glSpamBools.Sorted := True;

  _CacheInteger('config', 'connect', 20);
  _CacheInteger('config', 'io', 20);
  _CacheBool('speedstats', 'reduced_speedstat_weight', False);
  _CacheInteger('dirlist', 'skip_being_uploaded_files', 0);
  _CacheInteger('sites', 'idleinterval', 25);
  _CacheInteger('sites', 'maxidle', 60);
  _CacheBool('sites', 'set_down_on_out_of_space', False);
  _CacheBool('sites', 'set_down_on_out_of_credits', False);
  _CacheString('ident', 'response', 'slftpuser');
  _CacheBool('autodirlist', 'use_site_search_on_reqfill', False);
  _CacheBool('autodirlist', 'only_use_routable_sites_on_reqfill', False);
  _CacheBool('autodirlist', 'create_already_on_site_in_directory', True);
  _CacheBool('autodirlist', 'fill_already_on_site', False);
  _CacheInteger('autodirlist', 'dropolder', 86400);
  _CacheBool('autodirlist', 'compare_files_for_reqfilled_fallback', True);
  _CacheInteger('autodirlist', 'reqfill_delay', 60);
  _CacheBool('kb', 'skip_rip_older_then_one_month', False);
  _CacheBool('dbaddimdb', 'skip_tv_releases', False);
  _CacheBool('dbaddimdb', 'post_lookup_infos', False);
  _CacheBool('tasktvinfo', 'post_lookup_infos', False);
  _CacheBool('tasktvinfo', 'use_new_announce_style', True);
  _CacheInteger('tasktvinfo', 'days_between_last_update', 6);
  _CacheBool('tasktvinfolookup', 'stop_on_englishcheck', True);
  _CacheString('tasktvinfolookup', 'addcmd', '!addtvmaze');
  _CacheBool('indexer', 'use_custom_dirlist_command', False);
  _CacheString('indexer', 'custom_dirlist_command', 'list -al');
  _CacheString('indexer', 'expect_nfo_files', '');
  _CacheInteger('indexer', 'max_deep', 5);
  _CacheInteger('ranks', 'percent_of_sites_to_score', 30);
  _CacheInteger('backup', 'keep_backups', 10);
  _CacheBool('console', 'show_uptime', False);
  _CacheBool('console', 'show_infos', False);
  _CacheInteger('taskimdb', 'readd_attempts', 5); _CacheInteger('taskimdb', 'readd_interval', 60);
  _CacheInteger('taskmvid', 'readd_attempts', 5); _CacheInteger('taskmvid', 'readd_interval', 60);
  _CacheInteger('taskgenrenfo', 'readd_attempts', 5); _CacheInteger('taskgenrenfo', 'readd_interval', 60);
  _CacheInteger('taskgenredirlist', 'readd_attempts', 5); _CacheInteger('taskgenredirlist', 'readd_interval', 60);
  _CacheInteger('taskgame', 'readd_attempts', 5); _CacheInteger('taskgame', 'readd_interval', 60);
  _CacheInteger('tasktvinfo', 'readd_attempts', 5); _CacheInteger('tasktvinfo', 'readd_interval', 60);

  _CacheSpamBool('kb', 'new_rls', True); _CacheSpamBool('kb', 'pre_rls', True);
  _CacheSpamBool('kb', 'spread_rls', True); _CacheSpamBool('kb', 'updated_rls', True);
  _CacheSpamBool('kb', 'dont_match_rls', True); _CacheSpamBool('kb', 'skip_rls', True);
  _CacheSpamBool('precatcher', 'global_skip_group', True);
  _CacheSpamBool('addinfo', 'imdbupdate', True); _CacheSpamBool('addinfo', 'tvinfoupdate', True);
  _CacheSpamBool('taskrace', 'filename_not_allowed', True);
  _CacheSpamBool('taskrace', 'cant_create_dir', True);
  _CacheSpamBool('taskrace', 'denying_creation_of', True);

  fSections := TStringList.Create;
  fKeys := TStringList.Create;
  try
    spamcfg.ReadSections(fSections);
    fKeys.CommaText := 'incomplete,no_such_file_or_directory,src_regression,dst_uploader_switch,dst_regression,cant_open_data_connection,reached_max_sim_up,permission_denied,reached_max_sim_down,turn_on_sslfxp';
    for fSection in fSections do
      for fKey in fKeys do
        _CacheSpamBool(fSection, fKey, True);
  finally
    fKeys.Free;
    fSections.Free;
  end;
end;

procedure ConfigCacheUninit;
begin
  FreeAndNil(glIniBools);
  FreeAndNil(glIniIntegers);
  FreeAndNil(glIniStrings);
  FreeAndNil(glSpamBools);
end;

function config_connect_timeout: Integer;
begin
  Result := config_cached_integer(timeout, 'connect', 20);
end;

function config_io_timeout: Integer;
begin
  Result := config_cached_integer(timeout, 'io', 20);
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
  ConfigCacheUninit;
  cfgloaded := False;
  config.Free;
end;

end.
