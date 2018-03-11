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

function Main_Init: AnsiString;
procedure Main_Run;
procedure Main_Iter;
procedure Main_Stop;
procedure Main_Uninit;

function Main_Restart: boolean;

var
  slshutdown: boolean;
  started: TDateTime;

implementation

uses pretimeunit, ident, slmysql2, mysqlutilunit, tasksunit, dirlist, ircblowfish, sltcp, slssl, kb, fake, helper, console, slsqlite, xmlwrapper,
  sllanguagebase, irc, mycrypto, queueunit, sitesunit, versioninfo, pazo, rulesunit, skiplists, DateUtils, irccommandsunit, configunit, precatcher,
  notify, tags, taskidle, knowngroups, slvision, nuke, mslproxys, prebot, speedstatsunit, socks5, taskspeedtest, indexer, statsunit, ranksunit,
  dbaddpre, dbaddimdb, dbaddnfo, dbaddurl, dbaddgenre, globalskipunit, backupunit, taskautocrawler, debugunit, midnight, irccolorunit, mrdohutils, dbtvinfo,
{$IFNDEF MSWINDOWS}
  slconsole,
{$ENDIF}
  StrUtils, news;

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

function kilepescsekker(socket: TslTCPSocket): boolean;
begin
  Result := slshutdown;
end;

function Main_Init: AnsiString;
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

  if config.ReadString('mysql', 'host', '0') <> '0' then
  begin
    if InitialiseMysql then
    begin
      Debug(dpSpam, section, 'MYSQL libs initialised..');
    end
    else
    begin
      Debug(dpError, section, 'Could not initialize MYSQL libs!');
      Result := 'Cant initialize MYSQL libs!';
      Exit;
    end;
  end;

  if slsqlite_inited then
    Debug(dpMessage, section, 'SQLITE: ' + slSqliteVersion)
  else
  begin
    Debug(dpError, section, 'Could not initialize sqlite: ' + slsqlite_error);
    Result := slsqlite_error;
    Exit;
  end;

  {$IFNDEF MSWINDOWS}
    if Ncurses_Version < lib_Ncurses then
    begin
      Result := Format('Ncurses version is unsupported! %s+ needed.',[lib_Ncurses]);
      exit;
    end;
  {$ENDIF}

  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim);
  end;

  sltcp_onwaitingforsocket := @kilepescsekker;
  //  AutoCrawlerInit;

  InitXMLWeapper;

  StatsInit;
  IndexerInit;
  Socks5Init;
  MyCryptoInit;

  InitProxys;
  MySQLInit;
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
  IrcCommandInit;
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

  Initglobalskiplist;
  //  DupeDBInit;
  //  RehashIrcColor;

  queue_fire := config.readInteger('queue', 'queue_fire', 900);
  queueclean_interval := config.ReadInteger('queue', 'queueclean_interval', 1800);
  ranks_save_interval := config.readInteger('ranks', 'save_interval', 900);
  recalc_ranks_interval := config.readInteger('ranks', 'recalc_ranks_interval', 1800);
  speedstats_save_interval := config.readInteger('speedstats', 'save_interval', 900);
  speedstats_recalc_routes_interval := config.readInteger('speedstats', 'recalc_routes_interval', 3600);
  backup_interval := config.ReadInteger('backup', 'backup_interval', 0);
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

  // announce unread news count every 1h
  if (SecondsBetween(Now, last_news_announce) >= 3600) then
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
  Debug(dpError, section, '%s started', [Get_VersionString(ParamStr(0))]);

  Debug(dpMessage, section, OpenSSLVersion());

  {$IFNDEF MSWINDOWS}
    Debug(dpMessage, section, 'Ncurses: %s', [Ncurses_Version]);
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
  MySQLInit;
  SitesStart;
  IrcStart();
  PrecatcherStart();
  //  EPrecatcherStart();
  SiteAutoStart;
  AutoCrawlerStart;
  slshutdown := False;
  QueueStart();
end;

procedure Main_Stop;
begin
  // ez a fuggveny csak kiadja a megfelelo tobbszalu szaroknak a kilepesre vonatkozo dolgokat,
  // a tenyleges felszabaditasok/uninicializaciok a main_uninitben lesznek
  Debug(dpSpam, section, 'Main_Stop begin');
  AutoCrawlerStop;
  NukeSave;
  SpeedStatsSave;
  //  EPrecatcherStop;
  IdentStop();
  IrcStop();
  kb_Save();
  kb_Stop;
  QueueFire();
  MySQLUninit();
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
  PazoUnInit;
  NotifyUnInit;
  IrcCommandUnInit;
  IrcblowfishUnInit;
  IrcUnInit;
  FakesUnInit;
  DirlistUnInit;
  kb_UnInit;
  taskidleuninit;
  SitesUnInit;
  QueueUnInit;
  KnowngroupsUnInit;
  MidnightUninit;
  Tasks_UnInit;
  MyCryptoUnInit;
  IndexerUnInit;
  StatsUninit;
  AutoCrawlerUnInit;
  //  nWoMYSQLUNinit;
  UnInitProxys;
  UninitmRdOHConfigFiles;
  SLLanguages_Uninit;
  UnInitglobalskiplist;
  //  DupeDBUninit;

  dbaddpreUnInit;
  dbaddnfoUnInit;
  dbaddurlUnInit;
  dbaddgenreUnInit;
  dbaddimdbUnInit;
  dbtvinfoUnInit;
  NewsUnInit;

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

