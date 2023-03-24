{*****************************************************************************

 - Soulless robotic engine aka SLFTP
 - Version 1.3

 - Remarks:          Freeware, Copyright must be included

 - Original Author:  believe

 - Modifications:    aKRAUT aka dOH

 - Last change:      DD/MM/2010 - adding socks5 proxylist

 - Description:      Mainpool, Init/Uninit, Start/Stop and handle the timer

 ****************************************************************************

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS       *
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE       *
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE        *
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR      *
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF     *
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,    *
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE     *
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,        *
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       *

*****************************************************************************}

{ @abstract(Code for initialization, runtime checks and uninit) }

unit mainthread;

interface

uses SysUtils;

function Main_Init: String;
procedure Main_Run;
procedure Main_Iter;
procedure Main_Stop;
procedure Main_Uninit;

function Main_Restart: boolean;

var
  slshutdown: boolean;
  started: TDateTime;

implementation

uses
  identserver, tasksunit, dirlist, ircchansettings, sltcp, slssl, kb, fake, console, sllanguagebase, irc, mycrypto, queueunit,
  sitesunit, versioninfo, pazo, rulesunit, skiplists, DateUtils, configunit, precatcher, notify, tags, taskidle, knowngroups, slvision, nuke,
  mslproxys, speedstatsunit, socks5, taskspeedtest, indexer, statsunit, ranksunit, dbaddpre, dbaddimdb, dbaddnfo, dbaddurl,
  dbaddgenre, globalskipunit, backupunit, debugunit, midnight, irccolorunit, mrdohutils, dbtvinfo, taskhttpimdb, {$IFNDEF MSWINDOWS}slconsole,{$ENDIF}
  StrUtils, news, dbhandler, SynSQLite3, ZPlainMySqlDriver, SynDBZeos, SynDB, irccommands.prebot, IdOpenSSLLoader, IdOpenSSLHeaders_crypto;

{$I slftp.inc}

const
  section = 'mainthread';

var
  queue_fire: integer;
  queueclean_interval: integer;
  ranks_save_interval: integer;
  recalc_ranks_interval: integer;
  speedstats_save_interval: integer;
  speedstats_recalc_routes_interval: integer;
  backup_interval: integer;
  new_news_announce_interval: integer;

function kilepescsekker(socket: TslTCPSocket): boolean;
begin
  Result := slshutdown;
end;

function Main_Init: String;
var
  fSslLoader: IOpenSSLLoader;
  fOpenSSLVersion, fSSLErrorMsg: String;
  fHost, fPort, fUser, fPass, fDbName, fDBMS, fLibName: String;
  fError: String;
begin
  Result := '';

  if not sltcp_inited then
  begin
    Result := 'Couldnt init TCP library! TCP Error: ' + sltcp_error;
    exit;
  end;

  console_addline('Admin', 'Load OpenSSL', True);
  { load OpenSSL }
  fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
  fSslLoader.OpenSSLPath := '.';
  try
    if not fSslLoader.Load then
    begin
      Result := Format('Failed to load OpenSSL from slftp dir:%s %s', [sLineBreak, fSslLoader.FailedToLoad.CommaText]);
      exit;
    end;
  except
    on e: Exception do
    begin
      Result := Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]);
      exit;
    end;
  end;

  // check if we at least have OpenSSL 1.1.0
  if not Assigned(OpenSSL_version) then
  begin
    Result := Format('OpenSSL %s needed.', [lib_OpenSSL]);
    exit;
  end;

  { verify that it loaded the correct OpenSSL version }
  fOpenSSLVersion := GetOpenSSLShortVersion;
  // remove letter from version
  SetLength(fOpenSSLVersion, Length(fOpenSSLVersion) - 1);
  if (fOpenSSLVersion <> lib_OpenSSL) then
  begin
    Result := Format('OpenSSL version %s is not supported! OpenSSL %s needed.', [GetOpenSSLVersion, lib_OpenSSL]);
    exit;
  end;

  { init default SSL/TLS connection context }
  if not InitOpenSSLConnectionContext(fSSLErrorMsg) then
  begin
    Result := Format('Could not initialize default OpenSSL TLS/SSL context: %s%s', [sLineBreak, fSSLErrorMsg]);
    exit;
  end;

  console_addline('Admin', 'Load SQLite', True);
  // initialize global SQLite3 object for API calls (only load from current dir)
  try
    sqlite3 := TSQLite3LibraryDynamic.Create({$IFDEF MSWINDOWS}SQLITE_LIBRARY_DEFAULT_NAME{$ELSE}'./libsqlite3.so'{$ENDIF});
  except
    on e: Exception do
    begin
      Result := Format('Failed to load SQLite3: %s%s', [sLineBreak, e.Message]);
      exit;
    end;
  end;

  if sqlite3.VersionText < lib_SQLite3 then
  begin
    result := Format('SQLite3 version %s is too old! %sVersion %s or newer needed.', [sqlite3.VersionText, sLineBreak, lib_SQLite3]);
    exit;
  end;


  // initialize global MySQL/MariaD object
  fHost := config.ReadString('mysql', 'host', '0');
  if fHost <> '0' then
  begin
    console_addline('Admin', 'Load MYSQL/MariaDB', True);
    fPort := IntToStr(config.ReadInteger('mysql', 'port', 3306));
    fUser := config.ReadString('mysql', 'user', 'dbuser');
    fPass := config.ReadString('mysql', 'pass', 'dbpass');
    fDbName := config.ReadString('mysql', 'dbname', 'slftp-addpre');
    fDBMS := UpperCase(config.ReadString('mysql', 'dbms', ''));

    try
      // differentiate between db software, maybe not compatible in future
      if fDBMS = 'MYSQL' then
      begin
        fLibName := {$IFDEF MSWINDOWS}WINDOWS_DLL_LOCATION{$ELSE}LINUX_DLL_LOCATION{$ENDIF};
      end
      else if fDBMS = 'MARIADB' then
      begin
        fLibName := MARIADB_LOCATION;
      end
      else
      begin
        Result := 'Please set DBMS entry for MySQL/MariaDB in config.';
        exit;
      end;

      // create connection
      MySQLCon := TSQLDBZEOSConnectionProperties.Create(TSQLDBZEOSConnectionProperties.URI(dMySQL, fHost + ':' + fPort, fLibName), fDbName, fUser, fPass);
    except
      on e: Exception do
      begin
        Result := Format('Failed to load MySQL/MariaDB: %s%s', [sLineBreak, e.Message]);
        exit;
      end;
    end;

    if not Assigned(MySQLCon) then
    begin
      Result := Format('Failed to load MySQL/MariaDB: %s%s', [sLineBreak, fLibName]);
      exit;
    end;

    Debug(dpSpam, section, 'MySQL/MariaDB library initialised.');
  end;

  console_addline('Admin', 'Check Ncurses version', True);
  {$IFNDEF MSWINDOWS}
    if Ncurses_Version < lib_Ncurses then
    begin
      Result := Format('ncurses version is too old! %s%s or newer needed.', [sLineBreak, lib_Ncurses]);
      exit;
    end;
  {$ENDIF}

  console_addline('Admin', 'Init Ident server', True);
  fError := IdentServerInit;
  if fError <> '' then
  begin
    Result := Format('Unable to start ident server! %s%s', [sLineBreak, fError]);
    exit;
  end;

  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    console_addline('Admin', 'Set split_site_data directories', True);
    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim);
  end;

  sltcp_onwaitingforsocket := @kilepescsekker;

  console_addline('Admin', 'Init Stats DB', True);
  StatsInit;
  console_addline('Admin', 'Init indexer', True);
  IndexerInit;
  console_addline('Admin', 'Init Socks5', True);
  Socks5Init;
  console_addline('Admin', 'Init Crypto', True);
  MyCryptoInit;
  console_addline('Admin', 'Init Proxies', True);
  InitProxys;
  console_addline('Admin', 'Init RdOHConfig', True);
  InitmRdOHConfigFiles;
  console_addline('Admin', 'Init Addpre DB', True);
  dbaddpreInit;
  console_addline('Admin', 'Init NFO DB', True);
  dbaddnfoInit;
  console_addline('Admin', 'Init URL DB', True);
  dbaddurlInit;
  console_addline('Admin', 'Init Genre DB', True);
  dbaddgenreInit;
  console_addline('Admin', 'Init IMDB DB', True);
  dbaddimdbInit;
  console_addline('Admin', 'Init TV DB', True);
  dbtvinfoInit;
  console_addline('Admin', 'Init Console', True);
  ConsoleInit;
  console_addline('Admin', 'Init Tasks', True);
  Tasks_Init;
  console_addline('Admin', 'Init Sites', True);
  SitesInit;
  console_addline('Admin', 'Init Queue', True);
  QueueInit;
  console_addline('Admin', 'Init KB', True);
  kb_Init;
  console_addline('Admin', 'Init Idle tasks', True);
  taskidleinit;
  console_addline('Admin', 'Init Dirlist', True);
  DirlistInit;
  console_addline('Admin', 'Init Fakes', True);
  FakesInit;
  console_addline('Admin', 'Init Knowngroups', True);
  KnowngroupsInit;
  console_addline('Admin', 'Init Midnight', True);
  MidnightInit;
  console_addline('Admin', 'Init IRC', True);
  IrcInit;
  console_addline('Admin', 'Init IRC chans', True);
  IrcChannelSettingsInit;
  console_addline('Admin', 'Init Notify', True);
  NotifyInit;
  console_addline('Admin', 'Init Racing', True);
  PazoInit;
  console_addline('Admin', 'Init Prebot', True);
  PrebotInit;
  console_addline('Admin', 'Init Precatcher', True);
  Precatcher_Init;
  console_addline('Admin', 'Init Rules', True);
  RulesInit;
  console_addline('Admin', 'Init Skiplists', True);
  SkiplistsInit;
  console_addline('Admin', 'Init Languages', True);
  SLLanguagesInit;
  console_addline('Admin', 'Init Tags', True);
  TagsInit;
  //  EPrecatcherInit;
  console_addline('Admin', 'Init Nuke', True);
  NukeInit;
  console_addline('Admin', 'Init News', True);
  NewsInit;
  console_addline('Admin', 'Init SpeedStats', True);
  SpeedStatsInit;
  console_addline('Admin', 'Init Ranks', True);
  RanksInit;
  console_addline('Admin', 'Init Speedtest', True);
  SpeedTestInit;
  console_addline('Admin', 'Init Global Skiplist', True);
  Initglobalskiplist;

  queue_fire := config.readInteger('queue', 'queue_fire', 900);
  queueclean_interval := config.ReadInteger('queue', 'queueclean_interval', 1800);
  ranks_save_interval := config.readInteger('ranks', 'save_interval', 900);
  recalc_ranks_interval := config.readInteger('ranks', 'recalc_ranks_interval', 1800);
  speedstats_save_interval := config.readInteger('speedstats', 'save_interval', 900);
  speedstats_recalc_routes_interval := config.readInteger('speedstats', 'recalc_routes_interval', 3600);
  backup_interval := config.ReadInteger('backup', 'backup_interval', 0); //< time value in seconds for automatic backup
  new_news_announce_interval := config.ReadInteger('news', 'new_news_announce_interval', 3); //< time value in hours for announcing unread news count
end;

procedure Main_Iter;
begin
  if slshutdown then
  begin
    slapp.shouldquit := True;
    exit;
  end;

  // fire queue scheduler
  if ((queue_fire > 0) and (MilliSecondsBetween(Now, queue_last_run) >= queue_fire)) then
  begin
    try
      QueueFire;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(QueueFire): %s', [e.Message]);
      end;
    end;
  end;

  // clean queue scheduler
  if ((queueclean_interval > 0) and (SecondsBetween(Now, queueclean_last_run) >= queueclean_interval)) then
  begin
    try
      QueueClean;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(QueueClean): %s', [e.Message]);
        queueclean_last_run := Now;
      end;
    end;
  end;

  // ranks save scheduler
  if ((ranks_save_interval > 0) and (SecondsBetween(Now, ranks_last_save) >= ranks_save_interval)) then
  begin
    try
      RanksSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(RanksSave): %s', [e.Message]);
        ranks_last_save := Now;
      end;
    end;
  end;

  // ranks recalc scheduler
  if ((recalc_ranks_interval > 0) and (SecondsBetween(Now, ranks_last_process) >= recalc_ranks_interval)) then
  begin
    try
      RanksRecalc('', '');
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(RanksRecalc): %s', [e.Message]);
        ranks_last_process := Now;
      end;
    end;
  end;

  // speedstats save scheduler
  if ((speedstats_save_interval > 0) and (SecondsBetween(Now, speedstats_last_save) >= speedstats_save_interval)) then
  begin
    try
      SpeedStatsSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(SpeedStatsSave): %s', [e.Message]);
        speedstats_last_save := Now;
      end;
    end;
  end;

  // routes recalc scheduler
  if ((speedstats_recalc_routes_interval > 0) and (SecondsBetween(Now, speedstats_last_recalc) >= speedstats_recalc_routes_interval)) then
  begin
    try
      SpeedStatsRecalc('CONSOLE', 'SPEEDSTATS');
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(SpeedStatsRecalc): %s', [e.Message]);
        speedstats_last_recalc := Now;
      end;
    end;
  end;

  // backup scheduler
  if ((backup_interval > 0) and (SecondsBetween(Now, backup_last_backup) >= backup_interval)) then
  begin
    try
      BackupBackup;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Iter(BackupBackup): %s', [e.Message]);
        backup_last_backup := Now;
      end;
    end;
  end;

  // announce unread news count
  if ((new_news_announce_interval > 0) and (HoursBetween(Now, last_news_announce) >= new_news_announce_interval)) then
  begin
    if (Pos('<b>0</b> unread', SlftpNewsStatus) = 0) then
    begin
      try
        irc_AddAdmin(Format('%s', [SlftpNewsStatus]));
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] Main_Iter(SlftpNewsStatus): %s', [e.Message]);
          last_news_announce := Now;
        end;
      end;
    end;
  end;
end;

procedure Main_Run;
begin
  Debug(dpError, section, '%s started', [GetFullVersionString]);
  Debug(dpMessage, section, Format('OpenSSL version: %s', [GetOpenSSLVersion]));
  Debug(dpMessage, section, Format('SQLite3 version: %s', [sqlite3.Version]));
  {$IFNDEF MSWINDOWS}
    Debug(dpMessage, section, Format('ncurses version: %s', [Ncurses_Version]));
  {$ENDIF}

  started := Now();

  // Decrypt sites.dat
  console_addline('Admin', 'Decrypt sites.dat', True);
  MycryptoStart(passphrase);

  // Run backup
  if config.ReadBool('backup', 'run_backup_on_startup', True) then
  begin
    try
      console_addline('Admin', 'Create backup', True);
      BackupBackup;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Run(BackupBackup): %s', [e.Message]);
        backup_last_backup := Now;
      end;
    end;
  end;

  console_addline('Admin', 'Start Proxies', True);
  StartProxys;
  console_addline('Admin', 'Start Addpre DB', True);
  dbaddpreStart;
  console_addline('Admin', 'Start NFO DB', True);
  dbaddnfoStart;
  console_addline('Admin', 'Start URL DB', True);
  dbaddurlStart;
  console_addline('Admin', 'Start Genre DB', True);
  dbaddgenreStart;
  console_addline('Admin', 'Start IMDB DB', True);
  dbaddimdbStart;
  console_addline('Admin', 'Start TV DB', True);
  dbtvinfoStart;
  console_addline('Admin', 'Start Ranks', True);
  RanksStart;
  console_addline('Admin', 'Start SpeedStats', True);
  SpeedStatsStart;
  console_addline('Admin', 'Start Nuke', True);
  NukeStart;
  console_addline('Admin', 'Start Midnight', True);
  MidnightStart;
  console_addline('Admin', 'Start Skiplists', True);
  SkiplistStart;
  console_addline('Admin', 'Start Knowngroups', True);
  KnowngroupsStart;
  console_addline('Admin', 'Start Rules', True);
  RulesStart();
  console_addline('Admin', 'Start Fake', True);
  FakeStart();
  console_addline('Admin', 'Start KB', True);
  kb_Start();
  console_addline('Admin', 'Start Indexer', True);
  indexerStart;
  console_addline('Admin', 'Start Sites', True);
  SitesStart;
  console_addline('Admin', 'Start IRC', True);
  IrcStart();
  console_addline('Admin', 'Start Precatcher', True);
  PrecatcherStart();
  //  EPrecatcherStart();
  console_addline('Admin', 'Start Sites Auto Tasks', True);
  SiteAutoStart;
  slshutdown := False;
  console_addline('Admin', 'Start Queue', True);
  QueueStart();
end;

procedure Main_Stop;
begin
  // this is just a matter of putting the right shit on the kitty,
  // uninitialization will be in Main_Uninit
  Debug(dpSpam, section, 'Main_Stop begin');
  NukeSave;
  SpeedStatsSave;
  //  EPrecatcherStop;
  IdentServerStop;
  IrcStop();
  kb_Save();
  kb_Stop;
  QueueFire();
  Debug(dpSpam, section, 'Main_Stop end');
end;

procedure Main_Uninit;
var
  fSslLoader: IOpenSSLLoader;
begin
  Debug(dpSpam, section, 'Uninit1');
  (*
    // Looks like this was an attempt to ensure a clean exit when everything is shut down
    while
      (kb_thread <> nil)
      or
      (myIrcThreads.Count <> 0)
      do Sleep(500);
    Debug(dpSpam, section, 'Uninit2');
  *)
  ConsoleUnInit;
  RanksUnInit;
  SpeedStatsUnInit;
  NukeUninit;
  //EPrecatcherUninit;
  TagsUnInit;
  SkiplistsUnInit;
  RulesUnInit;
  Precatcher_UnInit;
  PrebotUnInit;
  NotifyUnInit;
  IrcChannelSettingsUninit;
  IrcUnInit;
  FakesUnInit;
  kb_UnInit;
  taskidleuninit;
  SitesUnInit;
  QueueUnInit;
  KnowngroupsUnInit;
  MidnightUninit;
  Tasks_UnInit;
  IndexerUnInit;
  StatsUninit;
  UnInitProxys;
  UninitmRdOHConfigFiles;
  SLLanguagesUninit;
  UnInitglobalskiplist;
  dbaddpreUnInit;
  dbaddnfoUnInit;
  dbaddurlUnInit;
  dbaddgenreUnInit;
  dbaddimdbUnInit;
  dbtvinfoUnInit;
  NewsUnInit;

  // TSQLite3LibraryDynamic
  if Assigned(sqlite3) then
    FreeAndNil(sqlite3);

  // MySQL/MariaDB connection
  if Assigned(MySQLCon) then
    FreeAndNil(MySQLCon);

  { unload OpenSSL }
  UninitOpenSSLConnectionContext;

  try
    fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
    fSslLoader.Unload;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] Unexpected error while unloading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;

  Debug(dpSpam, section, 'Uninit3');
  Debug(dpError, section, 'Clean exit');
end;

function Main_Restart: boolean;
begin
  Result := False;
  (*
  // Looks like this was an attempt for a restart command but stopping without
  // exception would be already a good begining.
  try
      Main_Stop;
      Main_Uninit;
      sleep(500);
      Main_Init;
      Main_Stop;
      result:=True;
  except
    on e: Exception do
      Debug(dpError, section, '[EXCEPTION] MainThreadRestart: %s', [e.Message]);
  end;
  *)
end;

end.

