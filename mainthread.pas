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
  dbaddpre, dbaddimdb, dbaddnfo, dbaddurl, dbaddgenre, globalskipunit, slhttp, backupunit, taskautocrawler, debugunit, midnight, irccolorunit, mrdohutils, dbtvinfo
{$IFNDEF MSWINDOWS}
  , slconsole
{$ENDIF}
  , StrUtils;

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
var
  ss, s: AnsiString;
begin
  Result := '';

  if not sltcp_inited then
  begin
    Result := 'Couldnt init TCP library!';
    exit;
  end;
  if not slssl_inited then
  begin
    ss := 'Could not load OpenSSL! Try to copy the libssl/libcrypto libs into slftp dir!' + #10#13;
{$IFDEF MSWINDOWS}
    ss := ss + 'Or install it from:' + #13#10 +
      'http://www.slproweb.com/products/Win32OpenSSL.html';
{$ENDIF}
    Result := ss;
    exit;
  end;

  s := OpenSSLShortVersion();
  if (s < lib_OpenSSL) then
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
  s := Ncurses_Version;
  if s < lib_Ncurses then
  begin
    Result := Format('Ncurses version is unsupported! %s+ needed.',[lib_Ncurses]);
    exit;
  end;
{$ENDIF}

  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    s := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;
    ForceDirectories(s);
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
  speedstats_recalc_routes_interval :=
    config.readInteger('speedstats', 'recalc_routes_interval', 3600);
  backup_interval := config.ReadInteger('backup', 'backup_interval', 0);
end;

procedure Main_Iter;
begin
  if slshutdown then
  begin
    slapp.shouldquit := True;
    exit;
  end;

  if ((queue_fire > 0) and (MilliSecondsBetween(Now, queue_last_run) >= queue_fire)) then
  begin
    try
      QueueFire;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in QueueFire: %s', [e.Message]));
      end;
    end;
  end;

  if ((queueclean_interval > 0) and (SecondsBetween(Now, queueclean_last_run) >=
    queueclean_interval)) then
  begin
    try
      QueueClean;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in QueueClean: %s', [e.Message]));
        queueclean_last_run := Now;
      end;
    end;
  end;

  if ((ranks_save_interval > 0) and (SecondsBetween(Now, ranks_last_save) >=
    ranks_save_interval)) then
  begin
    try
      RanksSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in RanksSave: %s', [e.Message]));
        ranks_last_save := Now;
      end;
    end;
  end;

  if ((recalc_ranks_interval > 0) and (SecondsBetween(Now, ranks_last_process) >=
    recalc_ranks_interval)) then
  begin
    try
      RanksRecalc('', '');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in RanksRecalc: %s', [e.Message]));
        ranks_last_process := Now;
      end;
    end;
  end;

  if ((speedstats_save_interval > 0) and
    (SecondsBetween(Now, speedstats_last_save) >= speedstats_save_interval)) then
  begin
    try
      SpeedStatsSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in SpeedStatsSave: %s', [e.Message]));
        speedstats_last_save := Now;
      end;
    end;
  end;

  if ((speedstats_recalc_routes_interval > 0) and
    (SecondsBetween(Now, speedstats_last_recalc) >= speedstats_recalc_routes_interval)) then
  begin
    try
      SpeedStatsRecalc('CONSOLE', 'SPEEDSTATS');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in SpeedStatsRecalc: %s',
          [e.Message]));
        speedstats_last_recalc := Now;
      end;
    end;
  end;

  if ((backup_interval > 0) and (SecondsBetween(Now, backup_last_backup) >=
    backup_interval)) then
  begin
    try
      BackupBackup;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in BackupBackup: %s', [e.Message]));
        irc_Adderror(Format('<c4>[Exception]</c> in BackupBackup: %s', [e.Message]));
        backup_last_backup := Now;
      end;
    end;
  end;

  //CustomBackup(s);

  //    if s <> '' then Debug(dpError, section, 'backup create failed: %s ', [s]);

  // Looks like a good spot to handle the rec. uptime ..
  //if SecondsBetween(Now, last_max_uptime_check) >= 3 then CheckForNewMaxUpTime;// <- broken!

end;

procedure Main_Run;
begin
  Debug(dpError, section, '%s started', [Get_VersionString(ParamStr(0))]);

  Debug(dpMessage, section, OpenSSLVersion());
{$IFNDEF MSWINDOWS}
  Debug(dpMessage, section, 'Ncurses: %s', [Ncurses_Version]);
{$ENDIF}

  started := Now();
  MycryptoStart(passphrase);
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
    // ez a legutolsonak betoltott unit, kilepesnel varni fog a tobbi cucc befejezodesere
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

  Debug(dpSpam, section, 'Uninit3');
  Debug(dpError, section, 'Clean exit');
end;

function Main_Restart: boolean;
begin
  Result := False;
  (*  broken anyway!
  try
      Main_Stop;
      Main_Uninit;
      sleep(500);
      Main_Init;
      Main_Stop;
      result:=True;
  except on e: Exception do Debug(dpError, 'MainThread', '[EXCEPTION] MainThreadRestart: %s', [e.Message]);end;
  *)
end;

end.

