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

function Main_Init: string;
procedure Main_Run;
procedure Main_Iter;
procedure Main_Stop;
procedure Main_Uninit;

function Main_Restart:boolean;

var kilepes: Boolean;
    started: TDateTime;

implementation

uses pretimeunit, ident,slmysql2, mysqlutilunit, eprecatcher, tasksunit,
     dirlist, ircblowfish, sltcp, slssl, kb, fake, helper, console, slsqlite,
     sllanguagebase, irc, mycrypto, queueunit, sitesunit, versioninfo, pazo,
     rulesunit, skiplists, DateUtils, irccommandsunit, configunit, precatcher,
     notify, tags, taskidle, knowngroups, slvision, nuke, mslproxys, prebot,
     speedstatsunit, socks5, taskspeedtest, indexer, statsunit, ranksunit,
     backupunit,taskautocrawler, debugunit, midnight, irccolorunit, mrdohutils,
     dbaddpre, dbaddnfo, dbaddurl, dbaddimdb, dbaddtvrage, globalskipunit, slhttp, dbaddgenre
{$IFNDEF MSWINDOWS}
     , slconsole
{$ENDIF}
     , StrUtils;

const section = 'mainthread';

var queue_fire: Integer;
    queueclean_interval: Integer;
    ranks_save_interval: Integer;
    recalc_ranks_interval: Integer;
    speedstats_save_interval: Integer;
    speedstats_recalc_routes_interval: Integer;
    backup_interval: Integer;


function kilepescsekker(socket: TslTCPSocket): Boolean;
begin
  Result:= kilepes;
end;

function Main_Init: string;
var ss, s: string;
begin
  Result:= '';

(*
{$IFDEF MSWINDOWS}
  if (not FileExists(ExtractFilePath(ParamStr(0))+'ssleay32.dll')) or
     (not FileExists(ExtractFilePath(ParamStr(0))+'libeay32.dll'))
  then
  begin
    WriteLn('OpenSSL dlls are needed! (libeay32.dll ssleay32.dll)');
    halt;
    exit;
  end;
{$ENDIF}
*)

  if not sltcp_inited then
  begin
    Result:= 'Couldnt init TCP library!';
    exit;
  end;
  if not slssl_inited then
  begin
  	ss:='Couldnt load OpenSSL! Try to copy the libssl/libcrypto libs in slftp dir!'+#10#13;
{$IFDEF MSWINDOWS}
    ss:=ss+ 'Or install it from:'+#13#10+'http://www.slproweb.com/products/Win32OpenSSL.html';
{$ENDIF}
{$IFDEF LINUX}
    ss:=ss+'try sudo apt-get -y install openssl libssl-dev libssl0.9.8 libssl0.9.8-dbg';
    ss:=ss+#10#13+'Check the wiki for more infos about openssl +1.*.*';
{$ENDIF}
    result:=ss;
    exit;
  end;


  s:= OpenSSLShortVersion();
  if (s < '0.9.8') then
  begin
    Result:= 'OpenSSL version is unsupported! 0.9.8+ needed.';
    exit;
  end;


(*
  if InitialiseMysql then Debug(dpMessage,section,'MYSQL libs initialised..')
  else begin
    //Debug(dpError,section,'Cant initialize MYSQL libs!');
    result:='Cant initialize MYSQL libs!'+#10#13;
    {$IFNDEF MSWINDOWS}
    //Debug(dpError,section,'Copy libmysql.dll into your slftp/nwo directory.');
    result:=result+'Copy libmysql.dll into your slftp/nwo directory.';
    {$ENDIF}
    {$IFDEF LINUX}
    //Debug(dpError,section,'do as root: apt-get install -y mysql-client-5.0 mysql-client libmysqlclient15off libmysqlclient15-dev');
    result:=result+'sudo apt-get install -y mysql-client-5.0 mysql-client libmysqlclient15off libmysqlclient15-dev';
    {$ENDIF}
    //result:=result+#10#13+'Cant initialize MYSQL libs!';
    exit;
  end;
*)


{$IFNDEF MSWINDOWS}
  s:= Ncurses_Version;
  if s < 'ncurses 5.5.' then
  begin
    Result:= 'Ncurses version is unsupported! 5.5+ needed.';
    exit;
  end;
{$ENDIF}

  sltcp_onwaitingforsocket:= @kilepescsekker;
//  AutoCrawlerInit;
  StatsInit;
  IndexerInit;
  Socks5Init;
  MyCryptoInit;

(* mR dOH mOD...*)
  InitProxys;
//nWoMYSQLInit;
  SLLanguages_Init;
  InitmRdOHConfigFiles;

  dbaddpreInit;
  dbaddnfoInit;
  dbaddurlInit;
  dbaddgenreInit;
  dbaddimdbInit;
  dbaddtvrageInit;

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
  EPrecatcherInit;
  NukeInit;
  SpeedStatsInit;
  RanksInit;
  SpeedTestInit;
  
  Initglobalskiplist;
//  DupeDBInit;
//  RehashIrcColor;

  queue_fire:= config.readInteger('queue', 'queue_fire', 900);
  queueclean_interval:= config.ReadInteger('queue', 'queueclean_interval', 1800);
  ranks_save_interval:= config.readInteger('ranks', 'save_interval', 900);
  recalc_ranks_interval:= config.readInteger('ranks', 'recalc_ranks_interval', 1800);
  speedstats_save_interval:= config.readInteger('speedstats', 'save_interval', 900);
  speedstats_recalc_routes_interval:= config.readInteger('speedstats', 'recalc_routes_interval', 3600);
  backup_interval:= config.ReadInteger('backup', 'backup_interval', 0);
end;

procedure Main_Iter;
begin
  if kilepes then
  begin
    slapp.shouldquit:= True;
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
  
  if ((queueclean_interval > 0) and (SecondsBetween(Now, queueclean_last_run) >= queueclean_interval)) then
  begin
    try
      QueueClean;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in QueueClean: %s', [e.Message]));
        queueclean_last_run:= Now;
      end;
    end;
  end;

  if ((ranks_save_interval > 0) and (SecondsBetween(Now, ranks_last_save) >= ranks_save_interval)) then
  begin
    try
      RanksSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in RanksSave: %s', [e.Message]));
        ranks_last_save:= Now;
      end;
    end;
  end;

  if ((recalc_ranks_interval > 0) and (SecondsBetween(Now, ranks_last_process) >= recalc_ranks_interval)) then
  begin
    try
      RanksRecalc('','');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in RanksRecalc: %s', [e.Message]));
        ranks_last_process:= Now;
      end;
    end;
  end;

  if ((speedstats_save_interval > 0) and (SecondsBetween(Now, speedstats_last_save) >= speedstats_save_interval)) then
  begin
    try
      SpeedStatsSave;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in SpeedStatsSave: %s', [e.Message]));
        speedstats_last_save:= Now;
      end;
    end;
  end;

  if ((speedstats_recalc_routes_interval > 0) and (SecondsBetween(Now, speedstats_last_recalc) >= speedstats_recalc_routes_interval)) then
  begin
    try
      SpeedStatsRecalc('CONSOLE', 'SPEEDSTATS');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in SpeedStatsRecalc: %s', [e.Message]));
        speedstats_last_recalc:= Now;
      end;
    end;
  end;

  if ((backup_interval > 0) and (SecondsBetween(Now, backup_last_backup) >= backup_interval)) then
  begin
    try
      BackupBackup;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in BackupBackup: %s', [e.Message]));
        irc_Adderror(Format('<c4>[Exception]</c> in BackupBackup: %s', [e.Message]));
        backup_last_backup:= Now;
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

  if slsqlite_inited then
    Debug(dpMessage, section, 'SQLITE: '+slSqliteVersion)
  else
    Debug(dpError, section, 'Could not init sqlite: '+slsqlite_error);

  started:= Now();
  MycryptoStart(passphrase);
  StartProxys;
//  nWoMYSQLStart;
//RehashPreurls;

  dbaddpreStart;
  dbaddnfoStart;
  dbaddurlStart;
  dbaddgenreStart;
  dbaddimdbStart;
  dbaddtvrageStart;

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
  EPrecatcherStart();

  SiteAutoStart;
  AutoCrawlerStart;

  kilepes:= False;

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
  EPrecatcherStop;
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
  RanksUnInit;
  SpeedStatsUnInit;
  NukeUninit;
  EPrecatcherUninit;
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
  dbaddtvrageUnInit;

  Debug(dpSpam, section, 'Uninit3');
  Debug(dpError, section, 'Clean exit');
end;


function Main_Restart:boolean;
begin
  result:=False;
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
