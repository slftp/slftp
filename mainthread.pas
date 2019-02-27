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
  ident, tasksunit, dirlist, ircblowfish, sltcp, slssl, kb, fake, helper, console, xmlwrapper, sllanguagebase, irc, mycrypto, queueunit,
  sitesunit, versioninfo, pazo, rulesunit, skiplists, DateUtils, configunit, precatcher, notify, tags, taskidle, knowngroups, slvision, nuke,
  mslproxys, speedstatsunit, socks5, taskspeedtest, indexer, statsunit, ranksunit, IdSSLOpenSSL, IdSSLOpenSSLHeaders, dbaddpre, dbaddimdb, dbaddnfo, dbaddurl,
  dbaddgenre, globalskipunit, backupunit, debugunit, midnight, irccolorunit, mrdohutils, dbtvinfo, taskhttpimdb, {$IFNDEF MSWINDOWS}slconsole,{$ENDIF}
  StrUtils, news, dbhandler, SynSQLite3, ZPlainMySqlDriver, SynDBZeos, SynDB, irccommands.prebot;

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
  fHost, fPort, fUser, fPass, fDbName, fDBMS, fLibName: String;
begin
  Result := '';

  if not sltcp_inited then
  begin
    Result := 'Couldnt init TCP library! TCP Error: ' + sltcp_error;
    exit;
  end;


  if not slssl_inited then
  begin
    Result := 'Could not load OpenSSL!' + #10#13;
    {$IFDEF MSWINDOWS}
      Result := Result + 'Install it from:' + #13#10 + 'http://www.slproweb.com/products/Win32OpenSSL.html';
    {$ELSE}
      Result := Result + 'Try to copy the libssl.so and libcrypto.so libs into slftp dir!';
    {$ENDIF}
    exit;
  end;

  if (OpenSSLShortVersion() < lib_OpenSSL) then
  begin
    result := Format('OpenSSL version %s is deprecated! %s or newer needed.', [OpenSSLVersion, lib_OpenSSL]);
    exit;
  end;


  // Tell Indy OpenSSL to load libs from current dir
  //IdOpenSSLSetLibPath('.');
  // note for Indy 5457: "Failed to load ./libcrypto.so."

  {$IFDEF UNIX}
    // do not try to load sym links first
    IdOpenSSLSetLoadSymLinksFirst(False);
  {$ENDIF}

  try
    IdSSLOpenSSLHeaders.Load;
  except
    on e: EIdOSSLCouldNotLoadSSLLibrary do
    begin
      Result := Format('Failed to load OpenSSL: %s %s', [sLineBreak, IdSSLOpenSSLHeaders.WhichFailedToLoad]);
      exit;
    end;
    on e: Exception do
    begin
      Result := Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]);
      exit;
    end;
  end;

  // TODO: add a check for OpenSSL version


  //< initialize global SQLite3 object for API calls (only load from current dir)
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


  {$IFNDEF MSWINDOWS}
    if Ncurses_Version < lib_Ncurses then
    begin
      Result := Format('ncurses version is too old! %s%s or newer needed.', [sLineBreak, lib_Ncurses]);
      exit;
    end;
  {$ENDIF}


  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim);
  end;

  sltcp_onwaitingforsocket := @kilepescsekker;

  InitXMLWeapper;

  StatsInit;
  IndexerInit;
  Socks5Init;
  MyCryptoInit;

  InitProxys;
  SLLanguages_Init;
  InitmRdOHConfigFiles;

  dbaddpreInit;
  dbaddnfoInit;
  dbaddurlInit;
  dbaddgenreInit;
  dbaddimdbInit;
  dbtvinfoInit;

  ConsoleInit;
  Tasks_Init;
  QueueInit;
  SitesInit;
  kb_Init;
  taskidleinit;
  DirlistInit;
  FakesInit;
  KnowngroupsInit;
  MidnightInit;
  IrcInit;
  IrcblowfishInit;
  NotifyInit;
  PazoInit;
  PrebotInit;
  Precatcher_Init;
  RulesInit;
  SkiplistsInit;
  TagsInit;
  //  EPrecatcherInit;
  NukeInit;
  NewsInit;
  SpeedStatsInit;
  RanksInit;
  SpeedTestInit;
  TaskHttpImdbInit;
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
  Debug(dpMessage, section, Format('OpenSSL version: %s', [OpenSSLVersion()]));
  Debug(dpMessage, section, Format('SQLite3 version: %s', [sqlite3.Version]));
  {$IFNDEF MSWINDOWS}
    Debug(dpMessage, section, Format('ncurses version: %s', [Ncurses_Version]));
  {$ENDIF}

  started := Now();

  // Decrypt sites.dat
  MycryptoStart(passphrase);

  // Run backup
  if config.ReadBool('backup', 'run_backup_on_startup', True) then
  begin
    try
      BackupBackup;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] Main_Run(BackupBackup): %s', [e.Message]);
        backup_last_backup := Now;
      end;
    end;
  end;

  StartProxys;
  dbaddpreStart;
  dbaddnfoStart;
  dbaddurlStart;
  dbaddgenreStart;
  dbaddimdbStart;
  dbtvinfoStart;
  RanksStart;
  SpeedStatsStart;
  NukeStart;
  MidnightStart;
  SkiplistStart;
  KnowngroupsStart;
  IdentStart();
  RulesStart();
  FakeStart();
  kb_Start();
  indexerStart;
  StatsStart;
  SitesStart;
  IrcStart();
  PrecatcherStart();
  //  EPrecatcherStart();
  SiteAutoStart;
  slshutdown := False;
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
  IdentStop();
  IrcStop();
  kb_Save();
  kb_Stop;
  QueueFire();
  Debug(dpSpam, section, 'Main_Stop end');
end;

procedure Main_Uninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  (*
    // Looks like this was an attempt to ensure a clean exit when everything is shut down
    while
      (myIdentserver <> nil)
      or
      (kb_thread <> nil)
      or
      (myIrcThreads.Count <> 0)
      do Sleep(500);
    Debug(dpSpam, section, 'Uninit2');
  *)
  ConsoleUnInit;
  UninitXMLWeapper;
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
  IrcblowfishUnInit;
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
  SLLanguages_Uninit;
  UnInitglobalskiplist;
  TaskHttpImdbUnInit;
  dbaddpreUnInit;
  dbaddnfoUnInit;
  dbaddurlUnInit;
  dbaddgenreUnInit;
  dbaddimdbUnInit;
  dbtvinfoUnInit;
  NewsUnInit;

  // TSQLite3LibraryDynamic
  if Assigned(sqlite3) then
    sqlite3.Free;

  // MySQL/MariaDB connection
  if Assigned(MySQLCon) then
    MySQLCon.Free;

  try
    IdSSLOpenSSLHeaders.Unload;
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

