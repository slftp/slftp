unit irccommandsunit;

interface

uses Classes, dirlist, irc, prebot;

type
  TIrcCommandHandler = function(const netname, channel: AnsiString;
    params: AnsiString): boolean;
  //TIrcCommandHandler = function (const netname, channel: string; params: string; nickname:string = ''): Boolean;
  TIrcCommand = record
    cmd: AnsiString;
    hnd: TIrcCommandHandler;
    minparams: integer;
    maxparams: integer;
    hlpgrp: AnsiString;
  end;

  TIRCCommandThread = class(TThread)
    c: TIRCCommandHandler;
    th: TMyIrcThread;
    netname, channel, cmd, params: AnsiString;
    constructor Create(c: TIRCCommandHandler; netname, channel, params: AnsiString;
      cmd: AnsiString = '');
    //constructor Create(c: TIRCCommandHandler; netname, channel, params: string; nickname:string = '');
    procedure Execute; override;
  end;

procedure IrcCommandInit;
procedure IrcCommandUninit;

function IrcNope(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcHelpHeader(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcHelpSeperator(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcHelpv2(const Netname, Channel: AnsiString; params: AnsiString): boolean;

function FindIrcCommand(cmd: AnsiString): integer; // overload;
//function FindIrcCommand(cmd: string): boolean; overload;
function IrcDie(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcHelp(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUptime(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRaw(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcManageUser(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcInvite(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcBnctest(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKill(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSites(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSite(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcBnc(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetdown(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcNope(const netname, channel: string;params: string; nickname:string = ''): Boolean;

function IrcQueue(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcMaxUpDn(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcMaxUpPerRip(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcMaxIdle(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcTimeout(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelsite(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSlots(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSlotsShow(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcAddSite(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcAddSiteInfos(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcAddBnc(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelBnc(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSiteUser(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSitePass(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcNetAddServer(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNetDelServer(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcNetAddPerform(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNetDelPerform(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNetListPerform(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNetDoPerform(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetdir(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSslmethod(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSslfxp(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcLegacyCwd(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcRankLock(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRanks(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRank(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRankRecalc(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcNoannouncesite(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSpeeds(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetSpeed(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcLockSpeed(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcInroutes(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcOutroutes(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcDirlist(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcLatest(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcDelrelease(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelAllrelease(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSpread(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcCStop(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcTransfer(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcStatus(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcChannels(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcChanAdd(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetBlowkey(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetChankey(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetChanName(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcSetChanInvite(const netname, channel: string;params: string): Boolean;
function IrcShowNet(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAddnet(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcModnet(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcModesNet(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelnet(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelchan(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcJump(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSay(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSitechan(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPrereload(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPrelist(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreadd(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPredel(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreCatchtest(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreCatchDebug(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcRuleAdd(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRuleIns(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRuleMod(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRuleDel(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAllRuleDel(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRuleHelp(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRuleList(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRules(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRulesLoad(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRulesReload(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcAffils(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcSetAffils(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUsers(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcCountry(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcInfo(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcName(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSize(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcLink(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNotes(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcLeechers(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcTraders(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUserslots(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFreeslots(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFindAffil(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFindCountry(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFindSection(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFindUser(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAuto(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcCrawler(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcConfirmerAnnounce(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcCrawl(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoLogin(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoBncTest(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoRules(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoNuke(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoDirlist(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcAutoCrawler(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAutoIndex(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKbShow(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKbList(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKbExtra(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKbAdd(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSkipReload(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcNoHelp(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcIdent(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNoSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcLookup(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcKnowngroups(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcShowWindow(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcShowWindows(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelWindow(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRepaint(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcIrcNames(const netname, channel: AnsiString; params: AnsiString): boolean;

function DirlistB(const netname, channel: AnsiString; sitename, dir: AnsiString; SpeedTest:
  boolean = False): TDirList;
procedure RawB(const netname, channel: AnsiString; sitename, dir, command: AnsiString;
  AnnounceSitename: boolean = False);
function RawC(const Netname, Channel: AnsiString; sitename, dir, command: AnsiString;
  AnnounceSitename: boolean = False): AnsiString;

function IrcNuke(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUnnuke(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcOper(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcNews(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNewsAdd(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNewsDel(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSpeedStats(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSpeedRecalc(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSpeedTestLocal(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSpeedTestCleanup(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSpeedTestIn(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSpeedTestOut(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcIndexStat(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcIndexQuery(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcIndexDropSection(const netname, channel: AnsiString; params: AnsiString): boolean;

//function IrcSetSpeedtestToPredir(const netname, channel: string;params: string): Boolean;

function IrcStatSites(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatSitesByGroup(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatSitesByUser(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatRaces(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcStatGroups(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatGroupsBySite(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcStatUsers(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatUsersByGroup(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatUsersBySite(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcStatUsersByGroupBySite(const netname, channel: AnsiString; params: AnsiString):
  boolean;

function IrcDelayLeech(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelayUpload(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcTweak(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcCatchMod(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcShowAllRules(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcKillAll(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcNetNoSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetMYIrcNick(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcInviteMyIRCNICK(const netname, channel: AnsiString; params: AnsiString): boolean;
//function IrcNetBotNick(const netname, channel: string;params: string): Boolean;

//Site_stuff
function IrcNoLoginMSG(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUseForNFOdownload(const Netname, Channel: AnsiString; params: AnsiString): boolean;
function IrcSkipBeingUploadedFiles(const Netname, Channel: AnsiString; params: AnsiString): boolean;

//function IrcCustomDelrelease(const netname, channel: string;params: string): Boolean;

function IrcCreateBackup(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcLanguageBaseReload(const netname, channel: AnsiString; params: AnsiString):
  boolean;
function IrcTestLanguageBase(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSpamConfig(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSLFTPConfig(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcRuleCopy(const netname, channel: AnsiString; params: AnsiString): boolean;

(* PreURLs *)
function IrcPreURLAdd(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreURLDel(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreURLMod(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcPreURLList(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetupOffset(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetupPretimeMode(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetupPretimeMode2(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetupADDPreMode(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcFindPretime(const netname, channel: AnsiString; params: AnsiString): boolean;

//function IrcReloadoffset(const netname, channel: string;params: string): Boolean;
function Irctestoffset(const netname, channel: AnsiString; params: AnsiString): boolean;

//function IrcSetMYSQLData(const netname, channel: string;params: string): Boolean;
//function IrcViewMYSQLValue(const netname, channel: string;params: string): Boolean;
//function IrcTweakMYSQL(const netname, channel: string;params: string): Boolean;
//function IrcMYSQLStatus(const netname, channel: string;params: string): Boolean;
(* SOCKS5 *)
function IrcAddSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDisplaySocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcTweakSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcSetSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRehashSocks5(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcDisplayMappings(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcReloadGlobalSkipGrouplist(const netname, channel: AnsiString; params: AnsiString):
  boolean;

function IrcShowCredits(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcShowAppStatus(const netname, channel: AnsiString; params: AnsiString): boolean;

function Ircaddknowngroup(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcChanSetSitename(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetSitePermdown(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcAnnounceIMDBInfo(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcShowSiteNukes(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetAutoInvite(const netname, channel: AnsiString; params: AnsiString): boolean;

//function IrcMain_Restart(const netname, channel: string;params: string): Boolean;

function IrcDWherePred(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcDelPart(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcFakeReload(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetPretime(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcRebuildSlot(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcRecalcFreeslots(const netname, channel: AnsiString; params: AnsiString): boolean;

function IrcSetDebugverbosity(const Netname, Channel: AnsiString; params: AnsiString): boolean;

{        Sections                   }
function IrcInsSection(const Netname, Channel: string; params: string): boolean;
function IrcSections(const netname, channel: AnsiString; params: AnsiString): boolean;
{        Test functions             }
function IrcTestColors(const Netname, Channel: AnsiString; params: AnsiString): boolean;

{ TVInfo aka TTVRelease aka TVMaze  }
function IrcAnnounceTVInfo(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcAddTVMazeToDb(const netname, channel: AnsiString; params: AnsiString): boolean;
function IrcUpdateTVMazeInfo(const Netname, Channel: AnsiString; params: AnsiString): boolean;
function IrcDelTheTVDbInfo(const Netname, Channel: AnsiString; params: AnsiString): boolean;
function IrcSetTheTVDbID(const Netname, Channel: AnsiString; params: AnsiString): boolean;
function IrcSetTVRageID(const netname, channel: AnsiString; params: AnsiString): boolean;

const
  helpCommands: array[0..22] of AnsiString = ('general', 'site', 'auto', 'route',
    'rank', 'speed', 'work', 'rip', 'stats', 'slots', 'misc', 'news', 'irc',
    'rules', 'indexer', 'info', 'reload', 'socks5', 'pretime', 'imdb', 'tv', 'test',
    'section');

  irccommands: array[1..246] of TIrcCommand = (
    (cmd: 'GENERAL'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$general'),
    (cmd: 'help'; hnd: IrcHelp; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'die'; hnd: IrcDie; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'uptime'; hnd: IrcUptime; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'status'; hnd: IrcShowAppStatus; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'nhelp'; hnd: IrcHelpv2; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'queue'; hnd: IrcQueue; minparams: 0; maxparams: 2; hlpgrp: 'general'),
    (cmd: 'logverbosity'; hnd: IrcSetDebugverbosity; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'backup'; hnd: IrcCreateBackup; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'auto'; hnd: IrcAuto; minparams: 0; maxparams: 1; hlpgrp: 'general'),

    (cmd: 'SITES'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$site'),
    (cmd: 'sites'; hnd: IrcSites; minparams: 0; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'site'; hnd: IrcSite; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'addsite'; hnd: IrcAddsite; minparams: 4; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'delsite'; hnd: IrcDelsite; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'addbnc'; hnd: IrcAddBnc; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'siteuser'; hnd: IrcSiteUser; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'sitepass'; hnd: IrcSitePass; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'delbnc'; hnd: IrcDelBnc; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'setdown'; hnd: IrcSetdown; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'setpermdown'; hnd: IrcSetSitePermdown; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: ''),
    (cmd: 'setdir'; hnd: IrcSetDir; minparams: 2; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'slots'; hnd: IrcSlots; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'maxupdn'; hnd: IrcMaxUpDn; minparams: 3; maxparams: 4; hlpgrp: 'site'),
    (cmd: 'maxupperrip'; hnd: IrcMaxUpPerRip; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    //(cmd: 'setspeedtesttopredir'; hnd: IrcSetSpeedtesttoPredir; minparams: 0; maxparams: 1; hlpgrp:'site'),
    (cmd: 'maxidle'; hnd: IrcMaxIdle; minparams: 2; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'timeout'; hnd: IrcTimeout; minparams: 3; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'sslfxp'; hnd: IrcSslfxp; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'sslmethod'; hnd: IrcSslmethod; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'legacycwd'; hnd: IrcLegacycwd; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'nologinmsg'; hnd: IrcNoLoginMSG; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'skipinc'; hnd: IrcSkipBeingUploadedFiles; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'usefornfodownload'; hnd: IrcUseForNFOdownload; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'autologin'; hnd: IrcAutoLogin; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'autobnctest'; hnd: IrcAutoBnctest; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'stats'),
    (cmd: 'credits'; hnd: IrcShowCredits; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'siteinfo'; hnd: IrcAddSiteInfos; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'slotsshow'; hnd: IrcSlotsShow; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'bnc'; hnd: IrcBnc; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'bnctest'; hnd: IrcBnctest; minparams: 0; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'ghost'; hnd: IrcKill; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'rebuildslot'; hnd: IrcRebuildSlot; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'recalcfreeslots'; hnd: IrcRecalcFreeslots; minparams: 1; maxparams: 1; hlpgrp: 'site'),

    (cmd: 'ROUTES'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$route'),
    (cmd: 'routes'; hnd: IrcSpeeds; minparams: 1; maxparams: 1; hlpgrp: 'route'),
    (cmd: 'routeset'; hnd: IrcSetspeed; minparams: 3; maxparams: -1 ; hlpgrp: 'route'),
    (cmd: 'routelock'; hnd: IrcLockspeed; minparams: 3; maxparams: 3; hlpgrp: 'route'),
    (cmd: 'routesin'; hnd: IrcInroutes; minparams: 0; maxparams: 1; hlpgrp: 'route'),
    (cmd: 'routesout'; hnd: IrcOutroutes; minparams: 0; maxparams: 1; hlpgrp: 'route'),
    (cmd: 'speedstats'; hnd: IrcSpeedStats; minparams: 1; maxparams: 4; hlpgrp: 'route'),
    (cmd: 'speedrecalc'; hnd: IrcSpeedRecalc; minparams: 0; maxparams: 0; hlpgrp: 'route'),

    (cmd: 'RANKS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$rank'),
    (cmd: 'ranks'; hnd: IrcRanks; minparams: 0; maxparams: 1; hlpgrp: 'rank'),
    (cmd: 'rank'; hnd: IrcRank; minparams: 2; maxparams: 3; hlpgrp: 'rank'),
    (cmd: 'ranklock'; hnd: IrcRankLock; minparams: 2; maxparams: 3; hlpgrp: 'rank'),
    (cmd: 'rankrecalc'; hnd: IrcRankRecalc; minparams: 0; maxparams: 0; hlpgrp: 'rank'),

    (cmd: 'SPEEDTEST'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$speedtest'),
    (cmd: 'speedtestlocal'; hnd: IrcSpeedTestLocal; minparams: 1; maxparams: 1; hlpgrp: 'speed'),
    (cmd: 'speedtestout'; hnd: IrcSpeedTestOut; minparams: 2; maxparams: - 1; hlpgrp: 'speed'),
    (cmd: 'speedtestin'; hnd: IrcSpeedTestIn; minparams: 2; maxparams: - 1; hlpgrp: 'speed'),
    (cmd: 'speedtestcleanup'; hnd: IrcSpeedTestCleanup; minparams: 0; maxparams: - 1; hlpgrp: 'speed'),

    (cmd: 'WORK'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$work'),
    (cmd: 'dirlist'; hnd: IrcDirlist; minparams: 1; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'autodirlist'; hnd: IrcAutoDirlist; minparams: 1; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'latest'; hnd: IrcLatest; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'lame'; hnd: IrcLame; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'spread'; hnd: IrcSpread; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'transfer'; hnd: IrcTransfer; minparams: 5; maxparams: 5; hlpgrp: 'work'),
    (cmd: 'stop'; hnd: IrcCStop; minparams: 1; maxparams: 1; hlpgrp: 'work'),
    (cmd: 'lookup'; hnd: IrcLookup; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'nuke'; hnd: IrcNuke; minparams: 4; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'unnuke'; hnd: IrcUnNuke; minparams: 3; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'autonuke'; hnd: IrcAutoNuke; minparams: 1; maxparams: 2; hlpgrp: 'work'),
    (cmd: 'checkforrip'; hnd: IrcCheckForExistsRip; minparams: 1; maxparams: 1; hlpgrp: 'work'),

    (cmd: 'RIPS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$rip'),
    (cmd: 'setprecmd'; hnd: IrcPrecmd; minparams: 3; maxparams: - 1; hlpgrp: 'rip'),
    (cmd: 'setpredir'; hnd: IrcPredir; minparams: 2; maxparams: 2; hlpgrp: 'rip'),
    (cmd: 'check'; hnd: IrcCheck; minparams: 2; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'pre'; hnd: IrcPre; minparams: 1; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'pretest'; hnd: IrcPretest; minparams: 2; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'batch'; hnd: IrcBatchAdd; minparams: 2; maxparams: 4; hlpgrp: 'rip'),
    (cmd: 'batchdel'; hnd: IrcBatchDel; minparams: 2; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'delrelease'; hnd: IrcDelrelease; minparams: 2; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'delallrelease'; hnd: IrcDelallrelease; minparams: 2; maxparams: 3; hlpgrp: 'rip'),
    (cmd: 'prelist'; hnd: IrcListPreContent; minparams: 0; maxparams: 1; hlpgrp: 'rip'),
    (cmd: 'prechecktime'; hnd: IrcSetReexamineTime; minparams: 0; maxparams: 1; hlpgrp: 'rip'),
    (cmd: 'skippre'; hnd: IrcSetSkipPre; minparams: 1; maxparams: 2; hlpgrp: 'rip'),

    (cmd: 'RACE STATS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$stats'),
    (cmd: 'statsites'; hnd: IrcStatSites; minparams: 0; maxparams: 2; hlpgrp: 'stats'),
    (cmd: 'statsitesbygroup'; hnd: IrcStatSitesByGroup; minparams: 1; maxparams: 3; hlpgrp: 'stats'),
    (cmd: 'statsitesbyuser'; hnd: IrcStatSitesByUser; minparams: 1; maxparams: 3; hlpgrp: 'stats'),
    (cmd: 'statgroups'; hnd: IrcStatGroups; minparams: 0; maxparams: 2; hlpgrp: 'stats'),
    (cmd: 'statgroupsbysite'; hnd: IrcStatGroupsBySite; minparams: 1; maxparams: 3; hlpgrp: 'stats'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'stats'),
    (cmd: 'statusers'; hnd: IrcStatUsers; minparams: 0; maxparams: 2; hlpgrp: 'stats'),
    (cmd: 'statusersbysite'; hnd: IrcStatUsersBySite; minparams: 1; maxparams: 3; hlpgrp: 'stats'),
    (cmd: 'statusersbygroup'; hnd: IrcStatUsersByGroup; minparams: 1; maxparams: 3; hlpgrp: 'stats'),
    (cmd: 'statusersbygroupbysite'; hnd: IrcStatUsersByGroupBySite; minparams: 2; maxparams: 4; hlpgrp: 'stats'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'stats'),
    (cmd: 'statrace'; hnd: IrcStatRaces; minparams: 1; maxparams: 3; hlpgrp: 'stats'),

    (cmd: 'LEECH SLOTS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$slots'),
    (cmd: 'delayleech'; hnd: IrcDelayLeech; minparams: 1; maxparams: 4; hlpgrp: 'slots'),
    (cmd: 'delayupload'; hnd: IrcDelayUpload; minparams: 1; maxparams: 4; hlpgrp: 'slots'),

    (cmd: 'MISC'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$misc'),
    (cmd: 'raw'; hnd: IrcRaw; minparams: 1; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'manageuser'; hnd: IrcManageUser; minparams: 2; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'invite'; hnd: IrcInvite; minparams: 1; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'sitechan'; hnd: IrcSiteChan; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'autoinvite'; hnd: IrcSetAutoInvite; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'tweak'; hnd: IrcTweak; minparams: 2; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'misc'),
    (cmd: 'ident'; hnd: IrcIdent; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'nosocks5'; hnd: IrcNoSocks5; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'noannouncesite'; hnd: IrcNoAnnounceSite; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'nohelp'; hnd: IrcNohelp; minparams: 0; maxparams: 0; hlpgrp: 'misc'),
    (cmd: 'testlanguagebase'; hnd: IrcTestLanguageBase; minparams: 1; maxparams: 1; hlpgrp: 'misc'),
    (cmd: 'killall'; hnd: IrcKillAll; minparams: 0; maxparams: 0; hlpgrp: 'misc'),
    (cmd: 'spamconf'; hnd: IrcSpamConfig; minparams: 0; maxparams: 3; hlpgrp: 'misc'),
    (cmd: 'addknowngroup'; hnd: Ircaddknowngroup; minparams: 1; maxparams: - 1; hlpgrp: 'misc'),

    (cmd: 'NEWS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$news'),
    (cmd: 'news'; hnd: IrcNews; minparams: 0; maxparams: 0; hlpgrp: 'news'),
    (cmd: 'newsadd'; hnd: IrcNewsAdd; minparams: 1; maxparams: - 1; hlpgrp: 'news'),
    (cmd: 'newsdel'; hnd: IrcNewsDel; minparams: 1; maxparams: 1; hlpgrp: 'news'),

    (cmd: 'WINDOWS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$windows'),
    (cmd: 's'; hnd: IrcShowWindow; minparams: 1; maxparams: - 1; hlpgrp: 'windows'),
    (cmd: 'windows'; hnd: IrcShowWindows; minparams: 0; maxparams: 0; hlpgrp: 'windows'),
    (cmd: 'delwindow'; hnd: IrcDelWindow; minparams: 1; maxparams: - 1; hlpgrp: 'windows'),
    (cmd: 'names'; hnd: IrcIrcNames; minparams: 2; maxparams: 2; hlpgrp: 'windows'),
    (cmd: 'repaint'; hnd: IrcRepaint; minparams: 0; maxparams: 0; hlpgrp: 'windows'),

    (cmd: 'IRC'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$irc'),
    (cmd: 'ircstatus'; hnd: IrcStatus; minparams: 0; maxparams: 0; hlpgrp: 'irc'),
    (cmd: 'ircsay'; hnd: IrcSay; minparams: 3; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircjump'; hnd: IrcJump; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircoper'; hnd: IrcOper; minparams: 1; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircnetnosocks5'; hnd: IrcNetNoSocks5; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'irc'),
    (cmd: 'ircnet'; hnd: IrcShownet; minparams: 1; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetadd'; hnd: IrcAddnet; minparams: 3; maxparams: 7; hlpgrp: 'irc'),
    (cmd: 'ircnetmod'; hnd: IrcModnet; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircnetmodes'; hnd: IrcModesNet; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdel'; hnd: IrcDelnet; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircnetaddserver'; hnd: Ircnetaddserver; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetdelserver'; hnd: Ircnetdelserver; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'irc'),
    (cmd: 'ircnetaddperform'; hnd: Ircnetaddperform; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdelperform'; hnd: Ircnetdelperform; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetlistperform'; hnd: Ircnetlistperform; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdoperform'; hnd: Ircnetdoperform; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'irc'),
    (cmd: 'ircchannels'; hnd: IrcChannels; minparams: 0; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircchanadd'; hnd: IrcChanAdd; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircchandel'; hnd: IrcDelchan; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircchanblow'; hnd: IrcSetBlowkey; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircchankey'; hnd: IrcSetChankey; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircchanrole'; hnd: IrcSetChanName; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircchanpart'; hnd: IrcDelPart; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnick'; hnd: IrcSetMYIrcNick; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    //(cmd: 'ircnetbotnick'; hnd: IrcNetBotNick; minparams: 2; maxparams:2; hlpgrp:'')
    (cmd: 'inviteme'; hnd: IrcInviteMyIRCNICK; minparams: 1; maxparams: - 1; hlpgrp: 'irc'),

    (cmd: 'PRECATCHER'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$precatcher'),
    (cmd: 'catchlist'; hnd: IrcPrelist; minparams: 0; maxparams: 2; hlpgrp: 'precatcher'),
    (cmd: 'catchadd'; hnd: IrcPreadd; minparams: 6; maxparams: 7; hlpgrp: 'precatcher'),
    (cmd: 'catchdel'; hnd: IrcPredel; minparams: 1; maxparams: 1; hlpgrp: 'precatcher'),
    (cmd: 'catchtest'; hnd: IrcPreCatchtest; minparams: 5; maxparams: - 1; hlpgrp: 'precatcher'),
    (cmd: 'catchmod'; hnd: IrcCatchMod; minparams: 7; maxparams: 8; hlpgrp: 'precatcher'),
    (cmd: 'catchdebug'; hnd: IrcPreCatchDebug; minparams: 0; maxparams: 1; hlpgrp: 'precatcher'),
    (cmd: 'mappings'; hnd: IrcDisplayMappings; minparams: 0; maxparams: 1; hlpgrp: 'precatcher'),

    (cmd: 'RULES'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$rules'),
    (cmd: 'ruleadd'; hnd: IrcRuleAdd; minparams: 6; maxparams: - 1; hlpgrp: 'rules'),
    (cmd: 'ruledel'; hnd: IrcRuleDel; minparams: 1; maxparams: 1; hlpgrp: 'rules'),
    (cmd: 'rulemod'; hnd: IrcRuleMod; minparams: 7; maxparams: - 1; hlpgrp: 'rules'),
    (cmd: 'ruleins'; hnd: IrcRuleIns; minparams: 7; maxparams: - 1; hlpgrp: 'rules'),
    (cmd: 'allrules'; hnd: IrcShowAllRules; minparams: 0; maxparams: - 1; hlpgrp: 'rules'),
    (cmd: 'delallrules'; hnd: IrcAllRuleDel; minparams: 1; maxparams: 2; hlpgrp: 'rules'),
    (cmd: 'rules'; hnd: IrcRules; minparams: 2; maxparams: 2; hlpgrp: 'rules'),
    (cmd: 'rulelist'; hnd: IrcRuleList; minparams: 0; maxparams: 1; hlpgrp: 'rules'),
    (cmd: 'rulehelp'; hnd: IrcRuleHelp; minparams: 1; maxparams: 1; hlpgrp: 'rules'),
    (cmd: 'rulesload'; hnd: IrcRulesLoad; minparams: 2; maxparams: 2; hlpgrp: 'rules'),
    (cmd: 'rulecp'; hnd: IrcRuleCopy; minparams: 3; maxparams: 3; hlpgrp: 'rules'),
    (cmd: 'autorules'; hnd: IrcAutoRules; minparams: 1; maxparams: 2; hlpgrp: 'rules'),

    (cmd: 'KB'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$kb'),
    (cmd: 'kbshow'; hnd: IrcKbShow; minparams: 2; maxparams: 2; hlpgrp: 'kb'),
    (cmd: 'kblist'; hnd: IrcKbList; minparams: 0; maxparams: 2; hlpgrp: 'kb'),
    (cmd: 'kbextra'; hnd: IrcKbExtra; minparams: 3; maxparams: - 1; hlpgrp: 'kb'),
    (cmd: 'kbadd'; hnd: IrcKbAdd; minparams: 4; maxparams: - 1; hlpgrp: 'kb'),

    (cmd: 'INDEXER'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$indexer'),
    (cmd: 'indexstat'; hnd: IrcIndexStat; minparams: 0; maxparams: 0; hlpgrp: 'indexer'),
    (cmd: 'indexquery'; hnd: IrcIndexQuery; minparams: 1; maxparams: - 1; hlpgrp: 'indexer'),
    (cmd: 'indexdropsection'; hnd: IrcIndexDropSection; minparams: 2; maxparams: 2; hlpgrp: 'indexer'),
    (cmd: 'autoindex'; hnd: IrcAutoIndex; minparams: 1; maxparams: - 1; hlpgrp: 'indexer'),

    //rename to INFO?
    (cmd: 'AFFILS/USERS/SHIT'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$info'),
    (cmd: 'info'; hnd: IrcInfo; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'name'; hnd: IrcName; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'link'; hnd: IrcLink; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'affils'; hnd: IrcAffils; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'size'; hnd: IrcSize; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'country'; hnd: IrcCountry; minparams: 2; maxparams: 2; hlpgrp: 'info'),
    (cmd: 'notes'; hnd: IrcNotes; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'info'),
    (cmd: 'users'; hnd: IrcUsers; minparams: 0; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'leechers'; hnd: IrcLeechers; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'traders'; hnd: IrcTraders; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'userslots'; hnd: IrcUserslots; minparams: 3; maxparams: 3; hlpgrp: 'info'),
    (cmd: 'freeslots'; hnd: IrcFreeslots; minparams: 0; maxparams: 0; hlpgrp: 'info'),
    (cmd: '-'; hnd: IrcHelpSeperator; minparams: 0; maxparams: 0; hlpgrp: 'info'),
    (cmd: 'findaffil'; hnd: IrcFindAffil; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'findcountry'; hnd: IrcFindCountry; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'findsection'; hnd: IrcFindSection; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'finduser'; hnd: IrcFindUser; minparams: 1; maxparams: 1; hlpgrp: 'info'),

    (cmd: 'RELOAD'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$reload'),
    (cmd: 'catchreload'; hnd: IrcPrereload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'skipreload'; hnd: IrcSkipReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'languagereload'; hnd: IrcLanguageBaseReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'socks5reload'; hnd: IrcRehashSocks5; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'fakereload'; hnd: IrcFakeReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'rulesreload'; hnd: IrcRulesReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'reloadglobalskip'; hnd: IrcReloadGlobalSkipGrouplist; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'knowngroupreload'; hnd: IrcKnowngroups; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    //(cmd: 'restart'; hnd: IrcMain_Restart; minparams: 0; maxparams: 0; hlpgrp:''),

    (cmd: 'SOCKS5'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$socks5'),
    (cmd: 'addsocks5'; hnd: IrcAddSocks5; minparams: 3; maxparams: 5; hlpgrp: 'socks5'),
    (cmd: 'delsocks5'; hnd: IrcDelSocks5; minparams: 2; maxparams: 2; hlpgrp: 'socks5'),
    (cmd: 'listsocks5'; hnd: IrcDisplaySocks5; minparams: 0; maxparams: 0; hlpgrp: 'socks5'),
    (cmd: 'tweaksocks5'; hnd: IrcTweakSocks5; minparams: 3; maxparams: 3; hlpgrp: 'socks5'),
    (cmd: 'setsocks5'; hnd: IrcSetSocks5; minparams: 3; maxparams: 3; hlpgrp: 'socks5'),

    (cmd: 'PRETIME'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$pretime'),
    (cmd: 'pretimemode'; hnd: IrcSetupPretimeMode; minparams: 0; maxparams: 1; hlpgrp: 'pretime'),
    (cmd: 'pretimemode2'; hnd: IrcSetupPretimeMode2; minparams: 0; maxparams: 1; hlpgrp: 'pretime'),
    (cmd: 'addpremode'; hnd: IrcSetupADDPreMode; minparams: 0; maxparams: 1; hlpgrp: 'pretime'),
    (cmd: 'pretime'; hnd: IrcFindPretime; minparams: 1; maxparams: 1; hlpgrp: 'pretime'),
    (cmd: 'setpretime'; hnd: IrcSetPretime; minparams: 2; maxparams: 3; hlpgrp: 'pretime'),
    (cmd: 'setoffset'; hnd: IrcSetupOffset; minparams: 0; maxparams: 1; hlpgrp: 'pretime'),

    (cmd: 'IMDB'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$imdb'),
    (cmd: 'imdbinfo'; hnd: IrcAnnounceIMDBInfo; minparams: 1; maxparams: 1; hlpgrp: 'imdb'),

    (cmd: 'TVINFO'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$tv'),
    (cmd: 'tvinfo'; hnd: IrcAnnounceTVInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'addtvinfo'; hnd: IrcAddTVMazeToDb; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'settvdbid'; hnd: IrcSetTheTVDBID; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'settvrageid'; hnd: IrcSetTVRageID; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'updatetvinfo'; hnd: IrcUpdateTVMazeInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'deltvinfo'; hnd: IrcDelTheTVDbInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),

    (cmd: 'SECTIONS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$section'),
    (cmd: 'sections'; hnd: IrcSections; minparams: 0; maxparams: - 1; hlpgrp: 'section'),
    (cmd: 'sectionins'; hnd: IrcInsSection; minparams: 1; maxparams: - 1; hlpgrp: 'section'),

    (*
      // Disabled - probably need some refactoring
      (cmd: 'PREBOT'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$$$'),
      (cmd: 'preurls'; hnd: IrcPreURLList; minparams: 0; maxparams:0; hlpgrp:'doh_preurls'),
      (cmd: 'preurladd'; hnd: IrcPreURLAdd; minparams: 2; maxparams:2; hlpgrp:'doh_preurls'),
      (cmd: 'preurldel'; hnd: IrcPreURLDel; minparams: 1; maxparams:1; hlpgrp:'doh_preurls'),
      (cmd: 'preurlmod'; hnd: IrcPreURLMod; minparams: 3; maxparams:3; hlpgrp:'doh_preurls'),
    *)

    (*
      // Disabled - probably need some refactoring
      (cmd: 'MYSQL'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp:'$$$'),
      (cmd: 'setmysql'; hnd: IrcSetMYSQLData; minparams: 5; maxparams:5; hlpgrp:''),
      (cmd: 'mysqlvalue'; hnd: IrcViewMYSQLValue; minparams: 0; maxparams:0; hlpgrp:''),
      (cmd: 'tweakmysql'; hnd: IrcTweakMYSQL; minparams: 2; maxparams:2; hlpgrp:''),
      (cmd: 'mysql'; hnd: IrcTweakMYSQL; minparams: 1; maxparams:1; hlpgrp:''),
    *)

    (cmd: 'TESTING'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$test'),
    (cmd: 'irccolors'; hnd: IrcTestColors; minparams: 0; maxparams: 0; hlpgrp: 'test')
  );

procedure IrcLineBreak(const Netname, Channel: AnsiString; const commatext: AnsiString;
  QuoteChar: AnsiChar = '"'; fronttext: AnsiString = ''; breakafter: integer = 16);

implementation

uses sltcp, SysUtils, DateUtils, Math, versioninfo, knowngroups, encinifile, speedstatsunit,
  debugunit, queueunit, tasksunit, mystrings, sitesunit, notify, taskraw, tasklogin,
  indexer, taskdirlist, taskdel, tasklame, taskcwd, taskrace, pazo, configunit, console,
  slconsole, uintlist, nuke, kb, helper, ircblowfish, precatcher, rulesunit, mainthread,
  taskspeedtest, taskfilesize, statsunit, skiplists, slssl, ranksunit, taskautocrawler,
  RegExpr, mslproxys, slhttp, strUtils, inifiles, rcmdline,
  mysqlutilunit, backupunit, sllanguagebase, irccolorunit, mrdohutils, fake, taskpretime,
  dbaddpre, dbaddurl, dbaddnfo, dbaddimdb, dbtvinfo, globalskipunit, xmlwrapper,
  tasktvinfolookup, uLkJSON;

{$I common.inc}

const
  section = 'irccommands';

procedure IrcLineBreak(const Netname, Channel: AnsiString; const commatext: AnsiString;
  QuoteChar: AnsiChar = '"'; fronttext: AnsiString = ''; breakafter: integer = 16);
var
  xs: TStringList;
  i, ii: integer;
  s, ss: AnsiString;
begin
  xs := TStringList.Create;
  xs.QuoteChar := QuoteChar;
  ii := 1;
  s := '';
  try
    xs.commatext := commatext;
    for i := 0 to xs.Count - 1 do
    begin
      if ii > breakafter then
      begin
        s := s + #13#10 + fronttext;
        ii := 1;
      end; // if ii > 9 then begin
      s := s + xs.Strings[i] + ', ';
      Inc(ii);
    end; // for I := 0 to xs.Count - 1 do begin
    if xs.Text <> '' then
    begin
      s := fronttext + s;
      for i := 1 to 1000 do
      begin
        ss := SubString(s, #13#10, i);
        if ss = '' then
          break;
        Delete(ss, length(ss) - 1, 2);
        irc_addtext(Netname, Channel, '%s', [ss]);
      end; // if xs.Text <> '' then begin
    end; // for i:= 1 to 1000 do begin
  finally
    xs.Free;
  end;
end;

function FindIrcCommand(cmd: AnsiString): integer;
var
  i: integer;
begin
  Result := 0;
  if ((cmd <> '') and (cmd[1] = '-')) then
    exit;

  for i := Low(irccommands) to High(irccommands) do
    if irccommands[i].cmd = lowercase(cmd) then
    begin
      Result := i;
      exit;
    end;
end;

function IrcSections(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss, sitename, secs: AnsiString;
  s: TSite;
  i: integer;
begin

  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  secs := UpperCase(mystrings.RightStr(params, length(sitename) + 1));

  if ((sitename = '') and (secs = '')) then
  begin
    IrcLineBreak(Netname, Channel, kb_sections.commatext, AnsiChar('"'),
      '<b>Global Sections</b>: ');
    Result := True;
    exit;
  end;

  //  Irc_AddText(Netname,channel,'%d ---',[kb_sections.count]);

  if kb_sections.IndexOf(sitename) > -1 then
  begin
    ss := '';
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if s.IsSection(sitename) then
        ss := ss + s.Name + ',';
    end;
    delete(ss, length(ss), 1);
    Irc_AddText(Netname, channel, 'Sites with section %s', [sitename]);
    IrcLineBreak(Netname, Channel, ss, '"', '<b>' + sitename + '</b>: ', 9);
    result := true;
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  ss := s.SetSections(secs, True);
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), '<b>' + sitename + ' Sections</b>: ');
  Result := True;
end;

function IrcInsSection(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  section, toadd: AnsiString;
  nsecs, osecs: TStringList;
  ini: TInifile;
  x:TStringList;
  i: integer;
begin
  section := UpperCase(SubString(params, ' ', 1));
  toadd := mystrings.RightStr(params, length(section) + 1);
  x:=TStringList.Create;
  x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
//  ini := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  osecs := TStringList.Create;
  nsecs := TStringList.Create;
  try
    osecs.Delimiter := ',';
    osecs.Sorted := True;
    osecs.Duplicates := dupIgnore;
    osecs.DelimitedText := x.Values[section];
//    osecs.DelimitedText := ini.ReadString('sections', section, '');

    if toadd = '' then
    begin
      IrcLineBreak(Netname, Channel, osecs.DelimitedText, ',', section + ': ');
      Result := True;
      Exit;
    end;

    if AnsiContainsText(toadd, ',') then
    begin
      Irc_addText(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      Result := True;
      Exit;
    end;

    nsecs.Delimiter := ' ';
    nsecs.DelimitedText := toadd;

    //avoid dupes...
    for i := 0 to nsecs.Count - 1 do
      osecs.Add(nsecs.Strings[i]);

    x.Values[section]:=osecs.DelimitedText;
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  //  ini.WriteString('sections', section, osecs.DelimitedText);
  //  ini.UpdateFile;
    osecs.Clear;
    osecs.DelimitedText := x.Values[section];
    irc_addText(Netname, Channel, PrecatcherReload);
    IrcLineBreak(Netname, Channel, osecs.DelimitedText, ',', section + ': ');

  finally
  //  ini.free;
    x.free;
    osecs.free;
    nsecs.free;
  end;
  result := True;
end;

function IrcSetdir(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, section: AnsiString;
  s: TSite;
  dir: AnsiString;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);

  if ((section = '*') or (section = '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if ((dir <> '') and (dir[1] <> '/')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if section = 'REQUESTS' then
  begin
    irc_addtext(Netname, Channel,
      '<c4><b>Syntax error</b></c>, use REQUEST as section name.');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Site <b>%s</b> not found.',
      [sitename]);
    exit;
  end;

  if (section <> 'SPEEDTEST') and (section <> 'REQUEST') and (AnsiPos('ARCH-', section) = 0)
    then
  begin
    if (kb_sections.IndexOf(section) = -1) and (dir <> '') then
    begin
      irc_addtext(Netname, Channel,
        '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].',
        [section, section]);
      exit;
    end;
  end;

  s.sectiondir[section] := dir;
  s.SetSections(section, False);

  if dir = '' then // el kell tavolitani a rulejait is
  begin
    s.SetSections(section, True);
    RulesRemove(sitename, section);
  end;

  Result := True;
end;

procedure Outroutes(const sitename, Netname, Channel: AnsiString);
var
  x: TStringList;
  ii, i: integer;
  ss: AnsiString;
begin
  x := TStringList.Create;
  try
    x.Sorted := True;
    sitesdat.ReadSection('speed-from-' + sitename, x);
    ss := '';
    ii := x.Count;
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      ss := ss + x[i] + ' ' + sitesdat.ReadString('speed-from-' + sitename, x[i], '');
    end;
  finally
    x.Free;
  end;
  if ss <> '' then
    irc_addtext(Netname, Channel, '<b>%s (%d)</b> -> <b>%s</b>', [sitename, ii, ss]);

end;

procedure OutroutesB(const Netname, Channel: AnsiString; const sitename: AnsiString);
var
  x: TStringList;
  ii, i: integer;
  ss: AnsiString;
begin
  x := TStringList.Create;
  try
    x.Sorted := True;
    sitesdat.ReadSection('speed-from-' + sitename, x);
    ss := '';
    ii := x.Count;
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      if (sitesdat.ReadString('speedlock-from-' + sitename, x[i], '') <> '') then
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speedlock-from-' + sitename, x[i],
          '') + '(L)' + '"';
      end
      else
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speed-from-' + sitename, x[i], '') +
          '"';
      end;
    end;
  finally
    x.Free;
  end;
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), format('<b>%s (%d)</b> -> ', [sitename,
      ii]));
  // LineBreak(ss,format('<b>%s (%d)</b> -> ',[sitename,ii]),lines);
  // irc_addtext(netname, channel, '<b>%s (%d)</b> -> <b>%s</b>', [sitename, ii, ss]);
end;

procedure Inroutes(const sitename, Netname, Channel: AnsiString);
var
  x: TStringList;
  i: integer;
  ss: AnsiString;
begin
  x := TStringList.Create;
  try
    x.Sorted := True;
    sitesdat.ReadSection('speed-to-' + sitename, x);
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      ss := ss + x[i] + ' ' + sitesdat.ReadString('speed-to-' + sitename, x[i], '');
    end;
    if ss <> '' then
    begin
      irc_addtext(Netname, Channel, '<b>%s (%d)</b> <- <b>%s</b>', [sitename, x.Count, ss]);
    end;
  finally
    x.Free;
  end;

end;

procedure InroutesB(const Netname, Channel: AnsiString; const sitename: AnsiString);
var
  x: TStringList;
  ii, i: integer;
  ss: AnsiString;
begin
  x := TStringList.Create;
  try
    x.Sorted := True;
    sitesdat.ReadSection('speed-to-' + sitename, x);
    ss := '';
    ii := x.Count;
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      if (sitesdat.ReadString('speedlock-to-' + sitename, x[i], '') <> '') then
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speedlock-to-' + sitename, x[i], '')
          + '(L)' + '"';
      end
      else
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speed-to-' + sitename, x[i], '') +
          '"';
      end;
    end;
  finally
    x.Free;
  end;
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), format('<b>%s (%d)</b> <- ', [sitename,
      ii]));
  // LineBreak(ss,format('<b>%s (%d)</b> -> ',[sitename,ii]),lines);
  // irc_addtext(netname, channel, '<b>%s (%d)</b> -> <b>%s</b>', [sitename, ii, ss]);
end;

function IrcSpeeds(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  OutroutesB(Netname, Channel, sitename);
  InroutesB(Netname, Channel, sitename);

  Result := True;
end;

function IrcSetSpeed(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  source, dest, admin_site: AnsiString;
  rcmd: TCommandLineReader;
  c1, c2, sw1, sw2: AnsiString;
  i,j: integer;
  apply: Boolean;
  source_sites, dest_sites: TStringList;
  site: TSite;
  speed: integer;

begin
  Result := False;

  rcmd := TCommandLineReader.create();

  try
    try
      rcmd.allowDOSStyle := False;
      rcmd.automaticalShowError := False;
      rcmd.declareString('c1','','s1');
      rcmd.declareString('c2','','s2');
      rcmd.declareString('sw1','','s3');
      rcmd.declareString('sw2','','s4');
      rcmd.declareFlag('apply','Apply changes');
      rcmd.addAbbreviation('a', 'apply');
      rcmd.parse(params);

    except
      on e: Exception do
      begin
        irc_addtext(Netname, Channel, '<c4><b>EXCEPTION</c></b> IrcSetSpeed(rcmd.parse): %s', [e.Message]);
        Debug(dpError, section, '[EXCEPTION] IrcSetSpeed(rcmd.parse): %s', [e.Message]);
        exit;
      end;
    end;

    source := rcmd.readNamelessString()[0];
    dest := rcmd.readNamelessString()[1];
    speed := rcmd.readNamelessInt()[0];
    c1 := rcmd.readString('c1');
    c2 := rcmd.readString('c2');
    sw1 := rcmd.readString('sw1');
    sw2 := rcmd.readString('sw2');
    apply := rcmd.readFlag('apply');

    irc_addtext(Netname, Channel, '[debug] source: %s | dest: %s | speed :%d | c1: %s | c2: %s | sw1: %s | sw2: %s | apply: %s',
      [source, dest, speed, c1, c2, sw1, sw2, BoolToStr(apply)]);

  finally
    rcmd.Free;
  end;

  exit;

  admin_site := UpperCase(config.ReadString('sites', 'admin_sitename', 'SLFTP'));

  // sanity check for first args
  if (source <> '*') and (AnsiContainsText(source, '-')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>First argument must be a site name or *</b>.</c>');
    exit;
  end;
  if (dest <> '*') and (AnsiContainsText(dest, '-')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Second argument must be a site name or *</b>.</c>');
    exit;
  end;
  if (source = admin_site) or (dest = admin_site) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>You can not use admin site with this function</b>.</c>');
    exit;
  end;
  if ((speed >= 10) or (speed < 0)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Third argument must be a speed between 0 and 9</b>.</c>');
    exit;
  end;

  // lookup optional filters from params line


  if (source = '*') or (dest = '*') then
    apply := False;

  // here we go with the real stuff
  source_sites := TStringList.Create;
  dest_sites := TStringList.Create;
  site := nil;
  try
    // lookup source site(s)
    if source = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := nil;
        site := TSite(sites[i]);

        // site not found (shouldn't happen)
        if site = nil then
          Continue;
        if site.Name = admin_site then
          Continue;

        // here add filters handling

        // add site to the source sites list
        source_sites.Add(site.Name);
      end;
    end
    else
    begin
      site := FindSiteByName(Netname, source);
      if site = nil then
      begin
        irc_addtext(Netname, Channel, 'Source site <b>%s</b> not found.', [source]);
        exit;
      end;
      source_sites.Add(site.Name);
    end;

    // lookup destination site(s)
    if dest = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := nil;
        site := TSite(sites[i]);

        // site not found (shouldn't happen)
        if site = nil then
          Continue;
        if site.Name = admin_site then
          Continue;

        // here add filters handling

        // add site to the destination sites list
        dest_sites.Add(site.Name);
      end;
    end
    else
    begin
      site := FindSiteByName(Netname, dest);
      if site = nil then
      begin
        irc_addtext(Netname, Channel, 'Destination site <b>%s</b> not found.', [dest]);
        exit;
      end;
      dest_sites.Add(site.Name);
    end;

    // Check if we have work to do
    if source_sites.Count < 1 then
    begin
      irc_addtext(Netname, Channel, 'No source site match your criterias.');
      exit;
    end;
    if dest_sites.Count < 1 then
    begin
      irc_addtext(Netname, Channel, 'No destination site match your criterias.');
      exit;
    end;

    for i := 0 to source_sites.Count - 1 do
    begin
      for j := 0 to dest_sites.Count - 1 do
      begin
        // Source and dest is the same. Skipping.
        if (source_sites[i] = dest_sites[j]) then
          Continue;

        irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> set.', [source_sites[i], dest_sites[j]]);

        // When using wildcards apply changes only if -apply has been specified (to avoid unwanted changes)
        if Apply then
        begin
          if speed > 0 then
          begin
            sitesdat.WriteInteger('speed-from-' + source_sites[i], dest_sites[j], speed);
            sitesdat.WriteInteger('speed-to-' + dest_sites[j], source_sites[i], speed);
          end
          else
            sitesdat.DeleteKey('speed-from-' + source_sites[i], dest_sites[j]);
            sitesdat.DeleteKey('speed-to-' + dest_sites[j], source_sites[i]);
          end;
        end;
      end;

    if not Apply then
      irc_addtext(Netname, Channel, 'Route were not really added. Check if you are satisfied and add -apply to the command.');

  finally
    FreeAndNil(source_sites);
    FreeAndNil(dest_sites);
  end;

  Result := True;
end;

function IrcLockSpeed(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename1, sitename2: AnsiString;
  speed: integer;
  s1, s2: TSite;
begin
  Result := False;
  sitename1 := UpperCase(SubString(params, ' ', 1));
  sitename2 := UpperCase(SubString(params, ' ', 2));
  speed := StrToIntDef(SubString(params, ' ', 3), -1);

  if ((speed >= 10) or (speed < 0)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s1 := FindSiteByName(Netname, sitename1);
  if s1 = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename1]);
    exit;
  end;
  s2 := FindSiteByName(Netname, sitename2);
  if s2 = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename2]);
    exit;
  end;

  if speed > 0 then
  begin
    sitesdat.WriteInteger('speed-from-' + sitename1, sitename2, speed);
    sitesdat.WriteInteger('speed-to-' + sitename2, sitename1, speed);
    sitesdat.WriteInteger('speedlock-from-' + sitename1, sitename2, speed);
    sitesdat.WriteInteger('speedlock-to-' + sitename2, sitename1, speed);
  end
  else
  begin
    sitesdat.DeleteKey('speedlock-from-' + sitename1, sitename2);
    sitesdat.DeleteKey('speedlock-to-' + sitename2, sitename1);
  end;

  Result := True;
end;

function IrcInroutes(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
begin
  if params <> '' then
  begin
    s := FindSiteByName('', UpperCase(params));
    if s <> nil then
    begin
      InroutesB(Netname, Channel, s.Name);
    end
    else
      irc_addtext(Netname, Channel, '<c4>Site: <b>%s</b> not found!</c>', [params]);
    Result := True;
    exit;
  end;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    InroutesB(Netname, Channel, s.Name);
  end;
  Result := True;
end;

function IrcOutroutes(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
begin
  if params <> '' then
  begin
    s := FindSiteByName('', UpperCase(params));
    if s <> nil then
    begin
      OutroutesB(Netname, Channel, s.Name);
      // irc_addtext(netname, channel,outs.Text);
    end
    else
      irc_addtext(Netname, Channel, '<c4>Site: <b>%s</b> not found!</c>', [params]);
    Result := True;
    exit;
  end;

  for i := 0 to sites.Count - 1 do
  begin

    s := TSite(sites[i]);
    OutroutesB(Netname, Channel, s.Name);
    // irc_addtext(netname, channel,outs.Text);
  end;
  Result := True;
end;

function DirlistB(const Netname, Channel: AnsiString; sitename, dir: AnsiString;
  SpeedTest: boolean = False): TDirList;
var
  r: TDirlistTask;
  tn: TTaskNotify;
  s: AnsiString;
begin
  Result := nil;

  r := TDirlistTask.Create(Netname, Channel, sitename, dir, true);
  tn := AddNotify;
  tn.tasks.Add(r);
  AddTask(r);
  QueueFire;

  tn.event.WaitFor($FFFFFFFF);

  s := '';
  if tn.responses.Count = 1 then
    s := TSiteResponse(tn.responses[0]).response;

  RemoveTN(tn);

  if s <> '' then
  begin
    Result := TDirList.Create(sitename, nil, nil, s, SpeedTest, true);
    if Result <> nil then
      Result.SetFullPath(dir);
  end;
end;

function IrcDirlist(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, predir, dir: AnsiString;
  d: TDirList;
  de: TDirListEntry;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;
  predir := s.sectiondir[section];

  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);
  if ((dir = '') and (predir = '')) then
  begin
    section := 'PRE';
    predir := s.sectiondir[section];
    dir := mystrings.RightStr(params, length(sitename) + 1);
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.',
      [sitename, section]);
    exit;
  end;

  predir := todaycsere(predir);

  d := DirlistB(Netname, Channel, sitename, MyIncludeTrailingSlash(predir) + dir);
  try
    if d <> nil then
    begin
      for i := 0 to d.entries.Count - 1 do
      begin
        de := TDirListEntry(d.entries[i]);

        if de.directory then
          irc_addtext(Netname, Channel, '<b>%s</b>', [de.filename])
        else
          irc_addtext(Netname, Channel, '%s (%d)', [de.filename, de.filesize]);
      end;
    end;

  finally
    d.Free;
  end;

  Result := True;
end;

function IrcLatest(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, predir: AnsiString;
  d: TDirList;
  de: TDirListEntry;
  amount: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  amount := StrToIntDef(SubString(params, ' ', 3), 10);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;
  predir := s.sectiondir[section];

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section %s.', [sitename,
      section]);
    exit;
  end;

  if (amount <= 0) then
  begin
    irc_addtext(Netname, Channel, 'Invalid amount');
    exit;
  end;

  predir := todaycsere(predir);

  d := DirlistB(Netname, Channel, sitename, predir);
  try
    if d <> nil then
    begin
      d.SortByModify;
      for i := 0 to d.entries.Count - 1 do
      begin
        if i >= amount then
          break;
        de := TDirListEntry(d.entries[i]);

        if de.directory then
        begin
          irc_addtext(Netname, Channel, '<b>%s</b>', [de.filename]);
        end
        else
          irc_addtext(Netname, Channel, '%s (%d)', [de.filename, de.filesize]);
      end;
    end;
  finally
    d.Free;
  end;

  Result := True;
end;

function IrcDelrelease(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  rlsname, sitename, section, predir, dir: AnsiString;
  r: TDelreleaseTask;
  tn: TTaskNotify;
  i: integer;
  p: TPazo;
begin
  Result := False;

  rlsname := SubString(params, ' ', 3);
  section := UpperCase(SubString(params, ' ', 2));
  sitename := UpperCase(SubString(params, ' ', 1));
  //xsites:=TStringlist.Create;
  try
    p := FindPazoByName(section, rlsname);
    if p <> nil then
    begin
      p.stopped := True;
      RemovePazo(p.pazo_id);
    end;
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;

  dir := rlsname;

  try
    if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end; // if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then begin
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;

  try
    if dir = '' then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end; // if dir = '' then begin
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c><b>ERROR</c></b>: %s', [E.Message]);
  end;

  // sitename:= UpperCase( mystrings.RightStr(params, length(section)+length(rlsname)+2));

  if ((sitename = '*') or (sitename = '')) then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      s := FindSiteByName(Netname, TSite(sites.Items[i]).Name);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
          [TSite(sites.Items[i]).Name]);
        Continue;
      end; // if s = nil then begin

      try
        predir := s.sectiondir[section];
        if (predir = '') then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> has no predir set.', [s.Name]);
          Continue;
        end; // if (predir = '') then begin
      except
        on E: Exception do
          irc_addtext(Netname, Channel, '<c4<b>ERROR</c></b>: %s', [E.Message]);
      end;

      if s.working <> sstUp then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> is not marked as up.', [sitename]);
        Continue;
      end;

      irc_addtext(Netname, Channel, 'Adding Task for: %s', [s.Name]);
      try

        r := TDelreleaseTask.Create(Netname, Channel, s.Name, MyIncludeTrailingSlash(predir) +
          dir);
        tn := AddNotify;
        tn.tasks.Add(r);
        AddTask(r);
        QueueFire;

        irc_addtext(Netname, Channel, 'Fireing %s @ %s ... hang a sec bro!', [dir, s.Name]);
        tn.event.WaitFor($FFFFFFFF);
        // r.devent.WaitFor($FFFFFFFF);

        RemoveTN(tn);
        irc_addtext(Netname, Channel, 'Site %s are done!', [s.Name]);

      except
        on E: Exception do
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
      end;
    end;
  end
  else
  begin

    s := FindSiteByName(Netname, sitename);

    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end; // if s = nil then begin

    try
      predir := s.sectiondir[section];
      if (predir = '') then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> has no predir set.', [sitename]);
        exit;
      end; // if (predir = '') then begin
    except
      on E: Exception do
        irc_addtext(Netname, Channel, '<c><b>ERROR</c></b>: %s', [E.Message]);
    end;

    if s.working <> sstUp then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> is not marked as up.', [sitename]);
      exit;
    end;

    irc_addtext(Netname, Channel, 'Adding Task for: %s', [s.Name]);

    try
      r := TDelreleaseTask.Create(Netname, Channel, sitename, MyIncludeTrailingSlash(predir) +
        dir);
      tn := AddNotify;
      tn.tasks.Add(r);
      AddTask(r);
      QueueFire;
      irc_addtext(Netname, Channel, 'Fireing %s @ %s ... hang a sec bro!', [dir, s.Name]);
      tn.event.WaitFor($FFFFFFFF);

      RemoveTN(tn);
      irc_addtext(Netname, Channel, 'Site %s are done!', [s.Name]);
    except
      on E: Exception do
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
    end;
  end;
  Result := True;
end;

function IrcDelallrelease(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  s: TSite;
  predir, section, sitename, dir: AnsiString;
  r: TDelreleaseTask;
  tn: TTaskNotify;
  added: boolean;
  i: integer;
  pazo_id: integer;
  p: TPazo;
  ps: TPazoSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  predir := s.sectiondir[section];

  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);
  if ((dir = '') and (predir = '')) then
  begin
    section := 'PRE';
    predir := s.sectiondir[section];
    dir := mystrings.RightStr(params, length(sitename) + 1);
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if dir = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel,
      'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  pazo_id := kb_Add(Netname, Channel, sitename, section, '', 'NEWDIR', dir, '', True);
  if pazo_id = -1 then
  begin
    exit;
  end;
  p := TPazo(kb_list.Objects[pazo_id]);
  p.Clear;
  p.AddSites;

  FireRules(p, p.FindSite(sitename));

  for i := 0 to p.sites.Count - 1 do
  begin
    ps := TPazoSite(p.sites[i]);

    if (ps.Name <> sitename) then
    begin
      s := FindSiteByName(Netname, ps.Name);
      if s <> nil then
      begin
        if (s.sectiondir[section] <> '') and (s.working = sstUnknown) then
        begin
          irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is unknown.', [s.Name]);
          exit;
        end;
      end;
    end;
  end;

  added := False;
  tn := AddNotify;
  for i := 0 to p.sites.Count - 1 do
  begin
    ps := TPazoSite(p.sites[i]);

    if (ps.Name <> sitename) then
    begin
      if (ps.status <> rssNotAllowed) then
      begin
        (* ps.Clear; *)

        r := TDelreleaseTask.Create(Netname, Channel, ps.Name,
          MyIncludeTrailingSlash(ps.maindir) + dir);
        tn.tasks.Add(r);
        AddTask(r);
        added := True;
      end;
    end;
  end;

  QueueFire;

  if added then
    tn.event.WaitFor($FFFFFFFF)
  else
    irc_addtext(Netname, Channel, 'No sites found...');

  RemoveTN(tn);

  Result := True;
end;

// y-ba belepakolja az osszes olyan siteot amibe el lehet jutni honnanbol...   -- y into it packs all of the site into which you can reach honnanbol ...

procedure Routeable(honnan: AnsiString; y: TStringList);
var
  x: TStringList;
  i: integer;
  s: TSite;
begin
  if - 1 = y.IndexOf(honnan) then
  begin
    y.Add(honnan);
    x := TStringList.Create;
    try
      sitesdat.ReadSection('speed-from-' + honnan, x);
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName('', x[i]);
        if ((s <> nil) and (s.working = sstUp)) then
          Routeable(x[i], y);
      end;
    finally
      x.Free;
    end;
  end;
end;

function mySpeedComparer(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareValue(StrToIntDef(List.ValueFromIndex[Index2], 0),
    StrToIntDef(List.ValueFromIndex[Index1], 0));
end;

function IrcSpread(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sp, s: TSite;
  ps: TPazoSite;
  ssite, predir, sitename, section, dir: AnsiString;
  lastAnn: TDateTime;
  ann: integer;
  pazo_id: integer;
  p: TPazo;
  y: TStringList;
  sdone, ssss, ss, si, sj, sss: AnsiString;
  added: boolean;
  ii, i, addednumber: integer;
  dd: double;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  ssite := sitename;
  section := UpperCase(SubString(params, ' ', 2));

  s := FindSiteByName(Netname, sitename);

  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  predir := s.sectiondir[section];

  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);
  if ((dir = '') and (predir = '')) then
  begin
    section := 'PRE';
    predir := s.sectiondir[section];
    dir := mystrings.RightStr(params, length(sitename) + 1);
  end;

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel,
      'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  (* Now we check the routing *)
  added := True;
  addednumber := 0;
  if 1 = Pos('PRE', section) then
    pazo_id := kb_Add(Netname, Channel, sitename, section, '', 'PRE', dir, '',
      True)
  else
    pazo_id := kb_Add(Netname, Channel, sitename, section, '', 'NEWDIR',
      dir, '', True);
  if pazo_id = -1 then
  begin
    Irc_AddText(Netname, Channel, 'Pazoid = %d', [pazo_id]);
    exit;
  end;

  p := TPazo(kb_list.Objects[pazo_id]);
  p.Clear;

  // p.AddSites; // ha kozben valamelyik site up lett...
  try
    p.AddSitesForSpread; // with skippre check.
  except
    on E: Exception do
    begin
      Irc_AddText(Netname, Channel,
        '<c4><b>ERROR</c></b>: IrcSpread.AddSitesForSpread: %s',
        [e.Message]);
      Debug(dpError, section,
        Format('[EXCEPTION] IrcSpread.AddSitesForSpread: %s',
        [e.Message]));
    end;
  end;

  try
    FireRules(p, p.FindSite(sitename));
  except
    on E: Exception do
    begin
      Irc_AddText(Netname, Channel,
        '<c4><b>ERROR</c></b>: IrcSpread.FireRules: %s',
        [e.Message]);
      Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRules: %s',
        [e.Message]));
    end;
  end;

  y := TStringList.Create;
  try

    // recurrere run, so we can use y.text to check! or?
    try
      Routeable(sitename, y);
    except
      on E: Exception do
      begin
        Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcSpread.Routeable: %s', [e.Message]);
        Debug(dpError, section, Format('[EXCEPTION] IrcSpread.Routeable: %s', [e.Message]));
      end;
    end;

    if y.Text = '' then
    begin
      irc_addtext(Netname, Channel, 'No Routeable sites found!');
      exit;
    end;

    for i := 0 to p.sites.Count - 1 do
    begin
      ps := TPazoSite(p.sites[i]);
      sp := FindSiteByName('', ps.Name);

      if sp.SkipPre then
      begin
        irc_addtext(Netname, Channel,
          '<c8><b>INFO</c></b>: we skip %s for spread ',
          [TSite(p.sites[i]).Name]);
        Continue;
      end;

      try
        FireRuleSet(p, ps);
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel,
            '<c4><b>ERROR</c></b>: IrcSpread.FireRuleSet: %s',
            [e.Message]);
          Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRuleSet: %s',
            [e.Message]));
        end;
      end;

      try
        FireRules(p, ps);
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel,
            '<c4><b>ERROR</c></b>: IrcSpread.FireRules: %s',
            [e.Message]);
          Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRules: %s',
            [e.Message]));
        end;
      end;

      try
        s := FindSiteByName(Netname, ps.Name);
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel,
            '<c4><b>ERROR</c></b>: IrcSpread.FindSiteByName: %s',
            [e.Message]);
          Debug(dpError, section,
            Format('[EXCEPTION] IrcSpread.FindSiteByName: %s',
            [e.Message]));
        end;
      end;

      if s.working <> sstUp then
      begin

        if s.working = sstUnknown then
          sss := 'unknown';
        if s.working = sstDown then
          sss := 'down';
        if s.working = sstMarkedDown then
          sss := 'marked down';
        if s.working = sstOutOfCreds then
          sss := 'out of creds';
        if s.working = sstOutOfSpace then
          sss := 'out of space';
        irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is %s.',
          [s.Name, sss]);
      end;

      if s.working = sstUnknown then
      begin
        irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is unknown.',
          [s.Name]);
        added := False;
        break;
      end;

      if ((ps.Name <> sitename) and (s.working = sstUp)) then
      begin
        Inc(addednumber);
        if y.IndexOf(ps.Name) = -1 then
        begin
          irc_addtext(Netname, Channel, '<b>%s</b> -> <b>%s</b> is not routeable.', [sitename, ps.Name]);
          added := False;
          break;
        end;
      end;
    end;

    if (addednumber = 0) then
    begin
      irc_addtext(Netname, Channel, 'There are no sites up to spread to...');
      added := False;
    end;

    if not added then
    begin
      exit;
    end;

    if 1 = Pos('PRE', section) then
      pazo_id := kb_Add(Netname, Channel, sitename, section, '', 'PRE',
        dir, '', False, True)
    else
      pazo_id := kb_Add(Netname, Channel, sitename, section, '', 'NEWDIR',
        dir, '', False, True);
    if pazo_id = -1 then
    begin
      irc_addtext(Netname, Channel, 'Is it allowed anywhere at all?');
      exit;
    end;

    irc_addtext(Netname, Channel,
      'Spread has started. Type %sstop <b>%d</b> if you want.',
      [irccmdprefix, pazo_id]);

    si := '-1';
    sj := '-1';
    sdone := '-1';

    ann := config.ReadInteger('spread', 'announcetime', 40);
    lastAnn := now();
    while (True) do
    begin
      if (slshutdown) then
        exit;
      Sleep(500);

      p := FindPazoById(pazo_id);
      if p = nil then
      begin
        irc_addtext(Netname, Channel, 'No valid Pazo found for %s', [dir]);
        exit; // ez a szituacio nem nagyon fordulhat elo
      end;

      if p.stopped then
      begin
        if RemovePazo(p.pazo_id) then
          irc_addtext(Netname, Channel, 'DEBUG - Pazo Removed!')
        else
          irc_addtext(Netname, Channel, 'DEBUG - Pazo NOT Removed!');
        irc_addtext(Netname, Channel,
          'Spreading of <b>%s</b> has been stopped.', [dir]);
        Result := True;
        exit;
      end;

      if ((p.ready) or (p.readyerror)) then
      begin
        ssss := 'successfully finished.';
        if p.readyerror then
        begin
          if p.errorreason = '' then
            irc_addtext(Netname, Channel,
              '<b>%s</b> ERROR: <c4>NO ERROR MSG FOUND, SORRY!</c>', [dir])
          else
            irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>%s</c>',
              [dir, p.errorreason]);
          ssss := 'stopped!';
          RemovePazo(p.pazo_id);
          Result := True;
        end
        else
          Result := True;
        irc_addtext(Netname, Channel, 'Spreading of %s has been %s', [dir, ssss]);
        break;

      end;

      if ((ann <> 0) and (SecondsBetween(now, lastAnn) > ann)) then
      begin

        ps := p.FindSite(sitename);

        if ps = nil then
          irc_addtext(Netname, Channel,
            '<c4>DEBUG<b></c></b>: %s is not a valid pazo site.', [sitename]);
        if ps.dirlist = nil then
          irc_addtext(Netname, Channel,
            '<c4>DEBUG<b></c></b>: %s have no dirlist.', [sitename]);
        if ((ps <> nil) and (ps.dirlist <> nil)) then
          si := IntToStr(ps.dirlist.Done)
        else
          si := '?';

        sss := '';

        for ii := 0 to p.sites.Count - 1 do
        begin
          sj := '?';
          ps := TPazoSite(p.sites[ii]);
          if ps = nil then
          begin
            irc_addtext(Netname, Channel,
              '<c8>DEBUG<b></c></b>: %s is not a valid pazo site.',
              [TPazoSite(p.sites[ii]).Name]);
            Continue;
          end;

          if ps.Name = ssite then
            Continue;
          if ps.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
            Continue;

          if ps.dirlist = nil then
            irc_addtext(Netname, Channel,
              '<c7>DEBUG<b></c></b>: %s have no dirlist.', [ps.Name]);

          if ((ps <> nil) and (ps.dirlist <> nil)) then
          begin
            sj := IntToStr(ps.dirlist.RacedByMe);
            sdone := IntToStr(ps.dirlist.Done);

            dd := ps.dirlist.SizeRacedByMe;

(*
            ssss := 'byte';
            if dd > 1024 then
            begin
              dd := dd / 1024;
              ssss := 'KB';
            end;
            if dd > 1024 then
            begin
              dd := dd / 1024;
              ssss := 'MB';
            end;
            if dd > 1024 then
            begin
              dd := dd / 1024;
              ssss := 'GB';
            end;
*)

            RecalcSizeValueAndUnit(dd, ssss, 0);

            if sdone = si then
            begin
              ss := format('<c3>%s</c>', [ps.Name]);
              if sss = '' then
                sss := ss
              else
                sss := sss + ', ' + ss;
              Continue;
            end;

            if si = sj then
            begin
              ss := format('<c3>%s</c>', [ps.Name]);
              if sss = '' then
                sss := ss
              else
                sss := sss + ', ' + ss;
              Continue;
            end;

            if dd = 0 then
              ss := format('"<b>%s</b> (%s/%sF)"', [ps.Name, sj, si])
            else
              ss := format('"<b>%s</b> (%s/%sF in %.2f%s)"',
                [ps.Name, sj, si, dd, ssss]);

            if sss = '' then
              sss := ss
            else
              sss := sss + ', ' + ss;
          end;

        end;
        IrcLineBreak(Netname, Channel, sss, AnsiChar('"'), '<b>STATUS</b>: ', 5);
        lastAnn := now();
      end;
    end;

  finally
    y.Free;
  end;
end;

function IrcTransfer(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  srcsitename, dstsitename, srcdir, dstdir, rlsname, ftpsrcdir, ftpdstdir: AnsiString;
  srcsite, dstsite: TSite;
  p: TPazo;
  ps_src, ps_dst: TPazoSite;
  rc: TCRelease;
  rls: TRelease;
  //  pazo_id: integer;
  pd: TPazoDirlistTask;
  lastAnn: TDateTime;

  ann: integer;
  i, j, k: AnsiString;
begin
  Result := False;

  //!transfer srcsite dstsite srcdir dstdir rlsname

  srcsitename := UpperCase(SubString(params, ' ', 1));
  dstsitename := UpperCase(SubString(params, ' ', 2));
  //uppercase don't work with path...so use uppercase later in code
  srcdir := SubString(params, ' ', 3);
  dstdir := SubString(params, ' ', 4);
  rlsname := SubString(params, ' ', 5);

  if ((srcsitename = '') or (dstsitename = '') or (srcdir = '') or (dstdir = '') or (rlsname =
    '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  srcsite := FindSiteByName(Netname, srcsitename);
  if srcsite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [srcsitename]);
    exit;
  end;
  if srcsite.working = sstDown then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is down.', [srcsitename]);
    exit;
  end;

  dstsite := FindSiteByName(Netname, dstsitename);
  if dstsite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [dstsitename]);
    exit;
  end;
  if dstsite.working = sstDown then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is down.', [dstsitename]);
    exit;
  end;

  if ((1 = AnsiPos('/', srcdir)) or (length(srcdir) = LastDelimiter('/', srcdir))) then
  begin
    if ((1 = AnsiPos('/', srcdir)) and (length(srcdir) = LastDelimiter('/', srcdir))) then
    begin
      ftpsrcdir := srcdir;
      irc_addtext(Netname, Channel, '<c14><b>srcdir is a path</b>.</c>');
    end
    else
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b> - Sure if it is a path?</c>');
      exit;
    end;
  end
  else
  begin
    srcdir := UpperCase(srcdir);
    ftpsrcdir := srcsite.sectiondir[srcdir];
    irc_addtext(Netname, Channel, '<c14><b>srcdir is a slftp section</b>.</c>');
  end;

  if ((1 = AnsiPos('/', dstdir)) or (length(dstdir) = LastDelimiter('/', dstdir))) then
  begin
    if ((1 = AnsiPos('/', dstdir)) and (length(dstdir) = LastDelimiter('/', dstdir))) then
    begin
      ftpdstdir := dstdir;
      irc_addtext(Netname, Channel, '<c14><b>dstdir is a path</b>.</c>');
    end
    else
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b> - Sure if it is a path?</c>');
      exit;
    end;
  end
  else
  begin
    dstdir := UpperCase(dstdir);
    ftpdstdir := dstsite.sectiondir[dstdir];
    irc_addtext(Netname, Channel, '<c14><b>dstdir is a slftp section</b>.</c>');
  end;

  //for the case if a slftp section is used but no dir is set
  if (ftpsrcdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section %s.',
      [srcsitename, srcdir]);
    exit;
  end;
  if (ftpdstdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section %s.',
      [dstsitename, dstdir]);
    exit;
  end;

  rc := FindSectionHandler(srcdir); //srcdir is our "section"
  rls := rc.Create(rlsname, srcdir);
  p := PazoAdd(rls);
  //  pazo_id := p.pazo_id;
  kb_list.AddObject('TRANSFER-' + IntToStr(RandomRange(10000000, 99999999)), p);

  p.AddSite(srcsite.Name, ftpsrcdir, False);
  p.AddSite(dstsite.Name, ftpdstdir, False);

  ps_src := p.FindSite(srcsite.Name);
  ps_src.AddDestination(dstsite.Name, 200);

  ps_dst := p.FindSite(dstsite.Name);
  ps_dst.status := rssAllowed;

  ps_src := TPazoSite(p.sites[0]);
  ps_src.dirlist.dirlistadded := True;

  pd := TPazoDirlistTask.Create(Netname, Channel, ps_src.Name, p, '', False);
  AddTask(pd);
  QueueFire;

  irc_addtext(Netname, Channel,
    'File Transfer has started. Type <c4>%sstop <b>%d</b></c> if you need.', [irccmdprefix,
    p.pazo_id]);

  ann := config.ReadInteger('spread', 'announcetime', 60);
  lastAnn := Now();

  while (True) do
  begin
    if (slshutdown) then
    begin
      Result := False;
      exit;
    end;

    Sleep(1000);

    //[STATS] output was initiated by dirlist
    if p.ready then
    begin

      if ps_dst.dirlist.RacedByMe <> 0 then
      begin
        // do nothing
      end
      else
      begin
        irc_addtext(netname, channel, '%s was already <c10>COMPLETE</c> on %s', [rlsname,
          dstsite.Name]);
      end;

      break;
    end;

    if p.stopped then
    begin
      irc_addtext(Netname, Channel, 'File Transfer of <b>%s</b> has <c4>stopped</c>.',
        [rlsname]);
      Result := True;
      exit;
    end;

    if p.readyerror then
    begin
      if p.errorreason = '' then
        irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>NO ERROR MSG FOUND, SORRY!</c>',
          [rlsname])
      else
        irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>%s</c>', [rlsname, p.errorreason]);

      irc_addtext(netname, channel, '%s STATS until ERROR: %s - %s', [rlsname, p.Stats(TRUE),
        p.StatusText]);
      Result := False;
      exit;
    end;

    if ((ann <> 0) and (SecondsBetween(Now, lastAnn) > ann)) then
    begin
      i := '?';
      if ((ps_src <> nil) and (ps_src.dirlist <> nil)) then
        i := IntToStr(ps_src.dirlist.Done);

      j := '?';
      k := '?';
      if ((ps_dst <> nil) and (ps_dst.dirlist <> nil)) then
      begin
        j := IntToStr(ps_dst.dirlist.RacedByMe);
        k := IntToStr(ps_dst.dirlist.Done);
      end;

      irc_addtext(Netname, Channel,
        '%s: %s (%s)/%s files done. Type <c4>%sstop <b>%d</b></c> if you want.', [rlsname, j,
        k,
          i, irccmdprefix, p.pazo_id]);
      lastAnn := Now();
    end;

  end;

  Result := True;
end;

function IrcCStop(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  p: TPazo;
  pazo_id: integer;
begin
  Result := True; // ezutan nem akarunk ok-et
  pazo_id := StrToIntDef(params, -1);
  if pazo_id = -1 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  p := FindPazoById(pazo_id);
  if p <> nil then
  begin
    p.stopped := True;
    Result := RemovePazo(p.pazo_id);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'No Pazo found for id: <b>%d</b>', [pazo_id]);
    Result := True;
    exit;
  end;
end;

function IrcSslmethod(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  method, sitename: AnsiString;
  s: TSite;
  i, v: integer;
  x: TStringList;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  method := SubString(params, ' ', 2);
  i := StrToIntDef(method, -1);

  if ((method <> '') and ((i < 0) or (i > Integer(High(TSSLMethods))))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</c></b>: %s is not valid SSL method.',
      [method]);
    Result := True;
    Exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;
      if s.PermDown then
        Continue;
      if method <> '' then s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));
      irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename,
        sslMethodToSTring(s)]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    x.commatext := sitename;
    for i := 0 to x.Count - 1 do
    begin
      s := FindSiteByName(Netname, x.Strings[i]);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
          [x.Strings[i]]);
        Continue;
      end;
     if method <> '' then s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));
      irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename,
        sslMethodToSTring(s)]);
    end;
  end;
  Result := True;
end;

function IrcSslfxp(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: AnsiString;
  sname: AnsiString;
  site: TSite;
  sslfxp: TSSLReq;
  i: integer;
  x: TStringList;
begin
  //  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  s := SubString(params, ' ', 2);
  sslfxp := TSSLReq(StrToIntDef(s, 0));

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      site := TSite(sites.Items[i]);
      if (site.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;
      if s <> '' then
        site.sslfxp := sslfxp;
      if site.sslfxp = srNone then
        irc_addtext(Netname, Channel, '%s SSLFXP: False', [site.Name]);
      if site.sslfxp = srNeeded then
        irc_addtext(Netname, Channel, '%s SSLFXP: True', [site.Name]);
      if site.sslfxp = srUnsupported then
        irc_addtext(Netname, Channel, '%s SSLFXP: Unsupported', [site.Name]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sname;
      for i := 0 to x.Count - 1 do
      begin
        site := FindSiteByName('', x.Strings[i]);
        if site = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
            [x.Strings[i]]);
          Continue;
        end;
        if s <> '' then
          site.sslfxp := sslfxp;
        if site.sslfxp = srNone then
          irc_addtext(Netname, Channel, '%s SSLFXP: False', [site.Name]);
        if site.sslfxp = srNeeded then
          irc_addtext(Netname, Channel, '%s SSLFXP: True', [site.Name]);
        if site.sslfxp = srUnsupported then
          irc_addtext(Netname, Channel, '%s SSLFXP: Unsupported',
            [site.Name]);
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcLegacycwd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  cwd: integer;
  i: integer;
  x: TStringList;
begin
  //  Result   := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  cwd := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites',
        'admin_sitename', 'SLFTP')) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      if cwd = 1 then
        TSite(sites.Items[i]).legacydirlist := True;
      if cwd = 0 then
        TSite(sites.Items[i]).legacydirlist := False;
      irc_addtext(Netname, Channel, 'Legacy dirlisting of site <b>%s</b> is %d',
        [TSite(sites.Items[i]).Name,
        integer(TSite(sites.Items[i]).legacydirlist)]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
            [x.Strings[i]]);
          Continue;
        end;
        if cwd = 1 then
          s.legacydirlist := True;
        if cwd = 0 then
          s.legacydirlist := False;
        // s.legacydirlist:= s.legacydirlist;
        irc_addtext(Netname, Channel,
          'Legacy dirlisting of site <b>%s</b> is %d',
          [sitename, integer(s.legacydirlist)]);
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcRank(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, section: AnsiString;
  rank: integer;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rank := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if s.sectiondir[section] = '' then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.',
      [sitename, section]);
    exit;
  end;

  if (rank = -1) then
  begin
    irc_addtext(Netname, Channel, 'Section %s on site %s is ranked %d.',
      [section, sitename, s.GetRank(section)]);
    Result := True;
    exit;
  end;

  if ((rank >= 0) and (rank <= 9)) then
  begin
    s.SetRank(section, rank);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Rank must be >= 0 and <= 9.', []);
    exit;
  end;

  if rank > 0 then
    irc_addtext(Netname, Channel, 'Section %s on site %s is ranked %d.',
      [section, sitename, s.GetRank(section)])
  else
    irc_addtext(Netname, Channel, 'Section %s on site %s is not ranked',
      [section, sitename]);

  Result := True;
end;

function IrcRanks(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  section: AnsiString;
  i, j: integer;
  s: TSite;
  x: TStringList;
  ss: AnsiString;
  //  outs: TStringList;
begin
  section := UpperCase(params);
  x := TStringList.Create;
  try
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if section <> '' then
      begin
        if s.sectiondir[section] <> '' then
        begin
          j := s.RCInteger('ranklock-' + section, 0);
          if j <> 0 then
          begin
            x.Add(s.Name + '(L)' + '=' + s.RCString('ranklock-' + section, '1'));
          end
          else
          begin
            x.Add(s.Name + '=' + s.RCString('rank-' + section, '1'));
          end;
        end;
      end
      else
      begin
        j := s.RCInteger('ranklock', 0);
        if j <> 0 then
          x.Add(s.Name + '=' + IntToStr(j));
      end;
    end;

    x.CustomSort(mySpeedComparer);

    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      ss := ss + '"' + x.Names[i] + ' ' + x.ValueFromIndex[i] + '"';
      if (i + 1 mod 10 = 0) then
      begin
        irc_addtext(Netname, Channel, ss);
        ss := '';
      end;
    end;
    if ss <> '' then
      IrcLineBreak(Netname, Channel, ss);
  finally
    x.Free;
  end;
  Result := True;
end;

function IrcRankLock(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, section: AnsiString;
  rank: integer;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rank := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if ((section <> '*') and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel,
      'Site %s has no section %s. Use * to set a global ranklock',
      [sitename, section]);
    exit;
  end;

  if (rank = -1) then
  begin
    irc_addtext(Netname, Channel, 'Section %s on site %s is rank locked %d.',
      [section, sitename, s.GetRankLock(section)]);
    Result := True;
    exit;
  end;

  if ((rank >= 0) and (rank <= 9)) then
  begin
    s.SetRankLock(section, rank);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Rank must be >= 0 and <= 9.', []);
    exit;
  end;

  if rank > 0 then
    irc_addtext(Netname, Channel, 'Section %s on site %s is rank locked %d.',
      [section, sitename, s.GetRankLock(section)])
  else
    irc_addtext(Netname, Channel, 'Section %s on site %s is not rank locked',
      [section, sitename]);

  Result := True;
end;

function IrcNoannouncesite(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  s: TSite;
  cwd: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  cwd := StrToIntDef(SubString(params, ' ', 2), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if cwd = 1 then
    s.noannounce := True
  else if cwd = 0 then
    s.noannounce := False;

  irc_addtext(Netname, Channel, 'Noannounce setting of site <b>%s</b> is %d',
    [sitename, integer(s.noannounce)]);

  Result := True;
end;

function IrcAddSite(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, username, password: AnsiString;
  s: TSite;
  bnc: AnsiString;
  bnchost: AnsiString;
  bncport: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);
  password := SubString(params, ' ', 3);
  bnc := SubString(params, ' ', 4);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (sitename = '*') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (0 < Pos('-', sitename)) then
  begin
    irc_addtext(Netname, Channel, 'Sitename cant contain -.');
    exit;
  end;

  if (username = '') or (password = '') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s <> nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> already added.', [sitename]);
    exit;
  end;

  i := 4;
  while (True) do
  begin
    bnc := SubString(params, ' ', i);
    bnchost := SubString(bnc, ':', 1);
    bncport := StrToIntDef(SubString(bnc, ':', 2), 0);
    if ((bnchost = '') or (bncport = 0)) then
      break;

    sitesdat.WriteString('site-' + sitename, 'bnc_host-' + IntToStr(i - 4), bnchost);
    sitesdat.WriteInteger('site-' + sitename, 'bnc_port-' + IntToStr(i - 4), bncport);

    Inc(i);
  end;

  sites.Add(TSite.Create(sitename));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Adding Site <b>%s</b> failed.', [sitename]);
    exit;
  end;

  s.UserName := username;
  s.PassWord := password;

  Result := True;
end;

function IrcAddBnc(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  aktbnc, bnc: AnsiString;
  bnchost: AnsiString;
  bncport: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktbnc := s.RCString('bnc_host-' + IntToStr(i), '');
    if (aktbnc = '') then
      break;
    Inc(i);
  end;
  s.WCString('bnc_host-' + IntToStr(i), bnchost);
  s.WCInteger('bnc_port-' + IntToStr(i), bncport);

  Result := True;
end;

function IrcSiteUser(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  username, sname: AnsiString;
  s: TSite;
begin
  Result := False;

  // parse parameters
  sname := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);

  // lookup site
  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: Site %s not found!', [sname]);
    exit;
  end;

  // not for admin site
  if s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: This command is not allowed on internal site <b>%s</b>!', [sname]);
    exit;
  end;

  // no username has been specified
  if Trim(username) = '' then
  begin
    irc_addtext(Netname, Channel, 'Current username on <b>%s</b>: <b>%s</b>', [sname, s.username]);
    Result := True;
    exit;
  end;

  // set the new username
  s.username := username;
  irc_addtext(Netname, Channel, 'Username on <b>%s</b> set to: <b>%s</b>', [sname, s.username]);
   Result := True;
end;

function IrcSitePass(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  password, sname: AnsiString;
  s: TSite;
begin
  Result := False;

  // parse parameters
  sname := UpperCase(SubString(params, ' ', 1));
  password := SubString(params, ' ', 2);

  // lookup site
  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: Site %s not found!', [sname]);
    exit;
  end;

  // not for admin site
  if s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: This command is not allowed on internal site <b>%s</b>!', [sname]);
    exit;
  end;

  // no password has been specified
  if Trim(password) = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: No password specified!');
    exit;
  end;

  // set the new password
  s.password := password;
  irc_addtext(Netname, Channel, 'Password on <b>%s</b> set to: <b>%s</b>', [sname, '<censored>']);
  Result := True;
end;

function IrcNetAddServer(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  aktbnc, bnc: AnsiString;
  bnchost: AnsiString;
  bncport: integer;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktbnc := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' + IntToStr(i),
      '');
    if (aktbnc = '') then
      break;
    Inc(i);
  end;
  sitesdat.WriteString('ircnet-' + nn, 'bnc_host-' + IntToStr(i), bnchost);
  sitesdat.WriteInteger('ircnet-' + nn, 'bnc_port-' + IntToStr(i), bncport);

  Result := True;
end;

function IrcNetDelServer(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  bnc: AnsiString;
  aktbnchost, bnchost: AnsiString;
  aktbncport, bncport: integer;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC network <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    aktbnchost := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' +
      IntToStr(i), '');
    aktbncport := sitesdat.ReadInteger('ircnet-' + nn, 'bnc_port-' +
      IntToStr(i), 0);
    if (aktbnchost = '') then
      break;

    if (not megvan) then
    begin
      if (aktbnchost = bnchost) and (aktbncport = bncport) then
      begin
        megvan := True;
        sitesdat.DeleteKey('ircnet-' + nn, 'bnc_host-' + IntToStr(i));
        sitesdat.DeleteKey('ircnet-' + nn, 'bnc_port-' + IntToStr(i));
      end;
    end
    else
    begin
      sitesdat.DeleteKey('ircnet-' + nn, 'bnc_host-' + IntToStr(i));
      sitesdat.DeleteKey('ircnet-' + nn, 'bnc_port-' + IntToStr(i));
      sitesdat.WriteString('ircnet-' + nn, 'bnc_host-' + IntToStr(i - 1),
        aktbnchost);
      sitesdat.WriteInteger('ircnet-' + nn, 'bnc_port-' + IntToStr(i - 1),
        aktbncport);
    end;

    Inc(i);
  end;
  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Bnc not found.');
    exit;
  end;

  Result := True;
end;

function IrcNetAddPerform(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  aktperform, Perform: AnsiString;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  Perform := mystrings.RightStr(params, length(nn) + 1);

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    Inc(i);
  end;
  sitesdat.WriteString('ircnet-' + nn, 'perform-' + IntToStr(i), Perform);

  Result := True;
end;

function IrcNetDelPerform(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  aktperform: integer;
  Perform: AnsiString;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  aktperform := StrToIntDef(SubString(params, ' ', 2), -1);

  if (aktperform = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC network <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    Perform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (Perform = '') then
      break;

    if (not megvan) then
    begin
      if (aktperform = i) then
      begin
        megvan := True;
        sitesdat.DeleteKey('ircnet-' + nn, 'perform-' + IntToStr(i));
      end;
    end
    else
    begin
      sitesdat.DeleteKey('ircnet-' + nn, 'perform-' + IntToStr(i));
      sitesdat.WriteString('ircnet-' + nn,
        'perform-' + IntToStr(i - 1), Perform);
    end;

    Inc(i);
  end;
  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Perform not found.');
    exit;
  end;

  Result := True;
end;

function IrcNetListPerform(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  aktperform: AnsiString;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    irc_addtext(Netname, Channel, 'Perform <b>%s</b>(%d) : %s',
      [nn, i, aktperform]);
    Inc(i);
  end;

  Result := True;
end;

function IrcNetDoPerform(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  nn: AnsiString;
  nnth: TMyIrcThread;
  aktperform: AnsiString;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  nnth := FindIrcnetwork(nn);

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    if not nnth.IrcWrite(aktperform) then
    begin
      irc_addtext(Netname, Channel, 'Error Perform <b>%s</b>(%d) : %s',
        [nn, i, aktperform]);
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Perform <b>%s</b>(%d) : %s',
        [nn, i, aktperform]);
    end;
    Inc(i);
  end;

  Result := True;
end;

function IrcDelBnc(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  bnc: AnsiString;
  aktbnchost, bnchost: AnsiString;
  aktbncport, bncport: integer;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    aktbnchost := s.RCString('bnc_host-' + IntToStr(i), '');
    aktbncport := s.RCInteger('bnc_port-' + IntToStr(i), 0);
    if (aktbnchost = '') then
      break;

    if (not megvan) then
    begin
      if (aktbnchost = bnchost) and (aktbncport = bncport) then
      begin
        megvan := True;
        sitesdat.DeleteKey('site-' + sitename, 'bnc_host-' + IntToStr(i));
        sitesdat.DeleteKey('site-' + sitename, 'bnc_port-' + IntToStr(i));
      end; // if(aktbnchost = bnchost) and (aktbncport = bncport) then
    end
    else
    begin // if(not megvan) then
      sitesdat.DeleteKey('site-' + sitename, 'bnc_host-' + IntToStr(i));
      sitesdat.DeleteKey('site-' + sitename, 'bnc_port-' + IntToStr(i));
      s.WCString('bnc_host-' + IntToStr(i - 1), aktbnchost);
      s.WCInteger('bnc_port-' + IntToStr(i - 1), aktbncport);
    end; // end else begin    //    if(not megvan) then
    Inc(i);
  end;
  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Bnc not found.');
    exit;
  end; // if(not megvan) then begin

  Result := True;
end;

function IrcMaxUpDn(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  x: TStringList;
  s: TSite;
  up, dn, pre_dn: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  up := StrToIntDef(SubString(params, ' ', 2), 0);
  dn := StrToIntDef(SubString(params, ' ', 3), 0);
  // optional setting, if empty it well be set to dn
  pre_dn := StrToIntDef(SubString(params, ' ', 4), 0);

  if (up < 0) or (dn < 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (pre_dn = 0) then
    pre_dn := dn;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      TSite(sites.Items[i]).max_dn := dn;
      TSite(sites.Items[i]).max_pre_dn := pre_dn;
      TSite(sites.Items[i]).max_up := up;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        s.max_dn := dn;
        s.max_pre_dn := pre_dn;
        s.max_up := up;
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcMaxUpPerRip(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  s: TSite;
  upperrip: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  upperrip := StrToIntDef(SubString(params, ' ', 2), 0);

  if (upperrip < 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      TSite(sites.Items[i]).WCInteger('maxupperrip', upperrip);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        s.WCInteger('maxupperrip', upperrip);
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcMaxIdle(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  maxidle, idleinterval: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  maxidle := StrToIntDef(SubString(params, ' ', 2), -1);
  idleinterval := StrToIntDef(SubString(params, ' ', 3), 0);

  if (maxidle = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      TSite(sites.Items[i]).maxidle := maxidle;
      if idleinterval <> 0 then
        TSite(sites.Items[i]).idleinterval := idleinterval;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        s.maxidle := maxidle;
        if idleinterval <> 0 then
          s.idleinterval := idleinterval;
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcTimeout(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  iotimeout, connnecttimeout: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  connnecttimeout := StrToIntDef(SubString(params, ' ', 2), 0);
  iotimeout := StrToIntDef(SubString(params, ' ', 3), 0);

  if (connnecttimeout = 0) or (iotimeout = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      TSite(sites.Items[i]).io_timeout := iotimeout;
      TSite(sites.Items[i]).connect_timeout := connnecttimeout;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        s.io_timeout := iotimeout;
        s.connect_timeout := connnecttimeout;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcDelsite(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(params);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  s.Stop;

  try

    try
      s.DeleteKey('autodirlist');
      s.DeleteKey('autodirlistsections');
      s.DeleteKey('nextautodirlist');
      s.RemoveAutoDirlist;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autodirlist</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autonuke');
      s.DeleteKey('nextautonuke');
      s.RemoveAutoNuke;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autonuke</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autobnctest');
      s.RemoveAutoBnctest;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autobnctest</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autorules');
      s.RemoveAutoRules;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autorules</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autoindex');
      s.DeleteKey('autoindexsections');
      s.DeleteKey('nextautoindex');
      s.RemoveAutoIndex;
      indexerRemoveSiteSection(s.Name, '');
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autoindex</b> failed : %s', [E.Message]);
    end;

    try
      x := TStringList.Create;
      try
        sitesdat.ReadSection('site-' + sitename, x);
        for i := 0 to x.Count - 1 do
          sitesdat.DeleteKey('site-' + sitename, x.Strings[i]);
      finally
        x.Free;
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Wipeing section SITE-%s failed : %s', [sitename,
          E.Message]);
    end;

    try
      sitesdat.EraseSection('speed-from-' + sitename);
      sitesdat.EraseSection('speed-to-' + sitename);

      for i := 0 to sites.Count - 1 do
      begin
        sitesdat.DeleteKey('speed-from-' + TSite(sites.Items[i]).Name, sitename);
        sitesdat.DeleteKey('speed-to-' + TSite(sites.Items[i]).Name, sitename);
      end;

    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>routes</b> failed : %s', [E.Message]);
    end;

    try
      RulesRemove(sitename, '');
      RulesSave;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, '<b>Rules remove</b> failed : %s', [E.Message]);
    end;

    try
      RemoveRanks(sitename);
      RanksSave;
      RanksReload;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>ranks</b> failed : %s', [E.Message]);
    end;

    try
      Precatcher_DelSiteChans(sitename);
      PrecatcherRebuild;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>catches</b> failed : %s', [E.Message]);
    end;

    try
      sitesdat.EraseSection('site-' + sitename);
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>site section</b> failed : %s', [E.Message]);
    end;

    try
      sites.Delete(sites.IndexOf(s));

    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>TSite Object</b> failed : %s', [E.Message]);
    end;
    Result := True;

  finally
    sitesdat.UpdateFile;
  end;
end;

function IrcSlotsShow(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  ss: TSiteSlot;
  i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  irc_addtext(Netname, Channel, 'Slots for : ' + sitename + ' : ' +
    IntToStr(s.slots.Count) + 'T-' + IntToStr(s.freeslots) + 'F-' +
    IntToStr(s.num_dn) + '/' + IntToStr(s.max_dn) + 'D-' + IntToStr(s.num_up) +
    '/' + IntToStr(s.max_up) + 'U');
  for i := 0 to s.slots.Count - 1 do
  begin
    try
      ss := TSiteSlot(s.slots[i]);
      if ((s.slots[i] = nil) or (ss = nil)) then
      begin
        irc_addtext(Netname, Channel, sitename + '/' + IntToStr(i) + ': ERROR');
      end
      else
      begin
        if ss.todotask = nil then
        begin
          irc_addtext(Netname, Channel, ss.Name + ': NIL');
        end
        else
        begin
          irc_addtext(Netname, Channel, ss.Name + ': ' + ss.todotask.Name +
            ' - A:' + TimeToStr(ss.lastactivity) + ' I/O:' +
            TimeToStr(ss.lastio));
        end;
      end;
    except
      on E: Exception do
      begin
        irc_addtext(Netname, Channel, sitename + '/' + IntToStr(i) + ': ERROR');
        Continue;
      end;
    end;
  end;
  Result := True;
  exit;
end;

function IrcSlots(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  ss: TStringList;
  s: TSite;
  oldslots, newslots: integer;
  ii, i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  newslots := StrToIntDef(SubString(params, ' ', 2), 0);

  if newslots <= 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  ss := TStringList.Create;
  try

    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites',
          'admin_sitename', 'SLFTP')) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        s := TSite(sites.Items[i]);

        oldslots := s.slots.Count;

        sitesdat.WriteInteger('site-' + s.Name, 'slots', newslots);
        if oldslots > newslots then
        begin
          // nehany slotot torolni kell
          for ii := 1 to oldslots - newslots do
          begin
            TSiteSlot(s.slots[s.slots.Count - 1]).Stop;
            s.slots.Delete(s.slots.Count - 1);
          end;
        end
        else if oldslots < newslots then
        begin
          // uj slotokat kell addolni
          for ii := 1 to newslots - oldslots do
          begin
            s.slots.Add(TSiteSlot.Create(s, s.slots.Count));
          end;
        end; // else fuckup

        s.RecalcFreeslots;
      end;

    end
    else
    begin
      ss.commatext := sitename;
      for i := 0 to ss.Count - 1 do
      begin
        s := FindSiteByName(Netname, ss.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
            [ss.Strings[i]]);
          Continue;
        end;

        oldslots := s.slots.Count;

        sitesdat.WriteInteger('site-' + s.Name, 'slots', newslots);
        if oldslots > newslots then
        begin
          // nehany slotot torolni kell
          for ii := 1 to oldslots - newslots do
          begin
            TSiteSlot(s.slots[s.slots.Count - 1]).Stop;
            s.slots.Delete(s.slots.Count - 1);
          end;
        end
        else if oldslots < newslots then
        begin
          // uj slotokat kell addolni
          for ii := 1 to newslots - oldslots do
          begin
            s.slots.Add(TSiteSlot.Create(s, s.slots.Count));
          end;
        end; // else fuckup

        s.RecalcFreeslots;

      end;
    end;

    Result := True;

  finally
    ss.Free;
  end;
end;

function IrcQueue(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, ii: integer;
  show_tasks: integer;
  show_all: boolean;
  rr: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    show_tasks := 10;
    rr.Expression := '-c\:([\d]+)';
    if rr.Exec(params) then
    begin
      show_tasks := StrToIntDef(rr.Match[1], 10);
    end;

    show_all := False;
    rr.Expression := '--all';
    if rr.Exec(params) then
    begin
      show_tasks := tasks.Count;
      show_all := True;
    end;

    ii := 0;
    irc_addtext(Netname, Channel, 'Tasks in queue: %d displaycount: %d',
      [tasks.Count, Min(show_tasks, tasks.Count)]);

    for i := 0 to tasks.Count - 1 do
    begin
      try

        if show_all then
        begin
          irc_addtext(Netname, Channel, TTask(tasks[i]).Fullname);
          Continue;
          //        Inc(ii);
        end
        else
        begin

          if (ii > show_tasks) then
            break;

          rr.Expression := '(AUTO(LOGIN|INDEX|NUKE|RULES))';
          if ((not rr.Exec(TTask(tasks[i]).Fullname)) and
            (not TTask(tasks[i]).ready) and (not TTask(tasks[i]).readyerror)) then
          begin
            irc_addtext(Netname, Channel, TTask(tasks[i]).Fullname);
            Inc(ii);
          end;
        end;
      except
        break;
      end;
    end;

  finally
    rr.Free;
  end;

  Result := True;
end;

function RawC(const Netname, Channel: AnsiString; sitename, dir, command: AnsiString;
  AnnounceSitename: boolean = False): AnsiString;
var
  r: TRawTask;
  tn: TTaskNotify;
  i: integer;
  ss: AnsiString;

begin
  r := TRawTask.Create(Netname, Channel, sitename, dir, command);
  tn := AddNotify;
  tn.tasks.Add(r);
  AddTask(r);
  QueueFire;

  tn.event.WaitFor($FFFFFFFF);

  Result := '';
  if tn.responses.Count = 1 then
  begin
    i := 1;
    while (True) do
    begin
      ss := SubString(TSiteResponse(tn.responses[0]).response, slEOL, i);
      if ss = '' then
        break;
      if AnnounceSitename then
        Result := Result + Format('<b>%s</b>: %s %s', [sitename, ss, #10#13])
      else
        Result := Result + ss + #10#13;
      Inc(i);
    end;
  end;
  RemoveTN(tn);

end;

procedure RawB(const Netname, Channel: AnsiString; sitename, dir, command: AnsiString;
  AnnounceSitename: boolean = False);
var
  r: TRawTask;
  tn: TTaskNotify;
  i: integer;
  ss: AnsiString;

begin
  r := TRawTask.Create(Netname, Channel, sitename, dir, command);
  tn := AddNotify;
  tn.tasks.Add(r);
  AddTask(r);
  QueueFire;

  tn.event.WaitFor($FFFFFFFF);

  if tn.responses.Count = 1 then
  begin
    i := 1;
    while (True) do
    begin
      ss := SubString(TSiteResponse(tn.responses[0]).response, slEOL, i);
      if ss = '' then
        break;
      if AnnounceSitename then
        irc_addtext(Netname, Channel, '<b>%s</b>: %s', [sitename, ss])
      else
        irc_addtext(Netname, Channel, ss);
      Inc(i);
    end;

  end;
  RemoveTN(tn);

end;

function IrcInvite(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  xl: TStringList;
  i: integer;
begin
  sitename := UpperCase(params);
  xl := TStringList.Create;
  xl.Delimiter := AnsiChar(44);
  xl.DelimitedText := sitename;
  sitename := '';
  try
    for i := 0 to xl.Count - 1 do
    begin
      sitename := xl.Strings[i];
      s := FindSiteByName(Netname, sitename);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
        Continue;
      end;
      if s.PermDown then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> is set permdown.', [sitename]);
        continue;
      end;
      RawB(Netname, Channel, sitename, '', 'SITE INVITE ' + mynickname);
    end;
  finally
    xl.Free;
  end;
  Result := True;
end;

function IrcRaw(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  command, sitename: AnsiString;
  s: TSite;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  command := mystrings.RightStr(params, length(sitename) + 1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites',
        'admin_sitename', 'SLFTP')) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      s := TSite(sites.Items[i]);
      if (s.PermDown) then
        Continue;
      RawB(Netname, Channel, s.Name, '', command, True);

    end;
    Result := True;
  end
  else
  begin
    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;
    if not s.PermDown then
    begin
      RawB(Netname, Channel, sitename, '', command);
      Result := True;
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.',
        [sitename]);
      Result := False;
    end;
  end;
end;

function IrcManageUser(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  command, username: AnsiString;
  s: TSite;
  i, j: integer;
  x, y: TStringList;
begin
  Result := False;
  username := UpperCase(SubString(params, ' ', 1));
  command := mystrings.RightStr(params, length(username) + 1);

  x := TStringList.Create;
  y := TStringList.Create;
  try

    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if s.markeddown then
      begin
        irc_addtext(Netname, Channel, 'Skipping site %s, cause its marked down.', [s.Name]);
        Continue;
      end;
      if (s.PermDown) then
      begin
        irc_addtext(Netname, Channel, 'Skipping site %s, cause its perm down.', [s.Name]);
        Continue;
      end;
      x.DelimitedText := s.leechers;
      if x.IndexOf(username) <> -1 then
      begin
        y.Add(s.Name);
        Continue;
      end;

      x.DelimitedText := s.traders;
      j := x.IndexOf(username);
      if j <> -1 then
      begin
        y.Add(s.Name);
        Continue;
      end;
    end;

    if y.Count = 0 then
    begin
      irc_addtext(Netname, Channel, 'User %s not found on any sites', [username]);
      exit;
    end;

    for i := 0 to y.Count - 1 do
      RawB(Netname, Channel, y[i], '', command);

  finally
    x.Free;
    y.Free;
  end;

  Result := True;
end;

function Bnctest(const Netname, Channel: AnsiString; s: TSite; tn: TTaskNotify; kill: boolean = False): boolean;
var
  l: TLoginTask;
begin
  if UpperCase(s.Name) <> uppercase(config.ReadString(
    'sites', 'admin_sitename', 'SLFTP')) then
  begin
    l := TLoginTask.Create(Netname, Channel, s.Name, kill, False);
    if tn <> nil then
      tn.tasks.Add(l);

    l.startat := GiveSiteLastStart;
    AddTask(l);
  end;

  Result := True;
end;

procedure SitesB(const Netname, Channel: AnsiString);
var
  up, down, unk: AnsiString;
  i: integer;
  s: TSite;
begin
  up := '';
  down := '';
  unk := '';
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    if ((Netname <> 'CONSOLE') and (Netname <> '') and (s.noannounce)) then
      Continue;
    if UpperCase(s.Name) = Uppercase(config.ReadString(
      'sites', 'admin_sitename', 'SLFTP')) then
      Continue;
    case s.working of
      sstUp:
        begin
          if up <> '' then
            up := up + ', ';
          up := up + '<b>' + s.Name + '</b>' + ' (<b>' +
            IntToStr(s.ffreeslots) + '</b>/' + IntToStr(s.slots.Count) + ')';
        end;
      sstDown:
        begin
          if down <> '' then
            down := down + ', ';
          down := down + '<b>' + s.Name + '</b>';
        end;
      sstUnknown:
        begin
          if unk <> '' then
            unk := unk + ', ';
          unk := unk + '<b>' + s.Name + '</b>';
        end;
    end;
  end;

  if up <> '' then
    irc_addtext(Netname, Channel, 'UP: ' + up);
  if down <> '' then
    irc_addtext(Netname, Channel, 'DN: ' + down);
  if unk <> '' then
    irc_addtext(Netname, Channel, '??: ' + unk);
end;

function IrcKill(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if Bnctest(Netname, Channel, s, nil, True) then
    QueueFire;

  Result := True;
end;

function IrcBnctest(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  x: TStringList;
  tn: TTaskNotify;
  added: boolean;
  i: integer;
  db: integer;
begin
  Result := False;
  added := False;
  s := nil;
  db := 0;
  x := TStringList.Create;
  x.Delimiter := ' ';
  x.DelimitedText := UpperCase(params);
  try

    if x.Count > 0 then
    begin
      db := x.Count;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site %s not found', [x[i]]);
          exit;
        end;
        if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        begin
          Continue;
        end;
      end;
      tn := AddNotify;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x[i]);
        if s.PermDown then
          Continue;
        if Bnctest(Netname, Channel, s, tn) then
          added := True;
      end;
    end
    else
    begin
      tn := AddNotify;
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites[i]);
        if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        begin
          Continue;
        end;
        Inc(db);

        if s.PermDown then
          Continue;

        if Bnctest(Netname, Channel, s, tn) then
          added := True;
      end;
    end;
    if added then
      QueueFire;

    if added then
      tn.event.WaitFor($FFFFFFFF);

    if (db > 1) then
      SitesB(Netname, Channel);

    s.RemoveAutoIndex;
    s.RemoveAutoBnctest;
    s.RemoveAutoNuke;
    s.RemoveAutoDirlist;
    s.RemoveAutoRules;
    // s.RemoveAutoCrawler;

    if s.RCInteger('autonuke', 0) <> 0 then
      s.AutoNuke;
    if s.RCInteger('autoindex', 0) <> 0 then
      s.AutoIndex;
    if s.RCInteger('autorules', 0) <> 0 then
      s.AutoRules;
    // if s.RCString('autologin','-1') <> '-1' then
    if s.RCInteger('autobnctest', 0) <> 0 then
      s.AutoBnctest;

  finally
    x.Free;
  end;

  RemoveTN(tn);

  Result := True;
end;

function IrcShownet(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, host: AnsiString;
  x: TStringList;
  i: integer;
  trigger: AnsiString;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  trigger := SubString(params, ' ', 2);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s does not exists!', [nn]);
    exit;
  end;

  irc_addtext(Netname, Channel, 'IRC network: ' + nn);
  x := TStringList.Create;
  try
    sitesdat.ReadSection('ircnet-' + nn, x);
    x.Sort;
    for i := 0 to x.Count - 1 do
    begin
      if Copy(x[i], 1, 3) <> 'bnc' then
      begin
        if ((x[i] = 'password') and (trigger <> '--plain')) then
          Continue;
        irc_addtext(Netname, Channel, ' %s: %s', [x[i], sitesdat.ReadString('ircnet-' + nn,
            x[i], '')]);
      end;
    end;
  finally
    x.Free;
  end;
  i := 0;
  while (not slshutdown) do
  begin
    host := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;
    irc_addtext(Netname, Channel, ' bnc: %s:%d', [host, sitesdat.ReadInteger('ircnet-' + nn,
        'bnc_port-' + IntToStr(i), 0)]);
    Inc(i);
  end;
  Result := True;
end;

function IrcAddnet(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, host, password, user, ident, nick: AnsiString;
  port: integer;
  ssl: integer;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  if (0 < Pos('-', nn)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if (nn = 'CONSOLE') then
  begin
    irc_addtext(Netname, Channel, 'You cant add a network called CONSOLE');
    exit;
  end;

  host := SubString(params, ' ', 2);
  port := StrToIntDef(SubString(host, ':', 2), 0);
  if port <= 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  host := SubString(host, ':', 1);
  ssl := StrToIntDef(SubString(params, ' ', 3), -1);
  if ((ssl < 0) or (ssl > 1)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if nil <> FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s already exists!', [nn]);
    exit;
  end;

  password := SubString(params, ' ', 4);
  nick := SubString(params, ' ', 5);
  ident := SubString(params, ' ', 6);
  user := SubString(params, ' ', 7);

  if nick = '' then
    nick := config.ReadString('irc', 'nickname', 'slftp');
  if user = '' then
    user := config.ReadString('irc', 'username', 'slftp');
  if ident = '' then
    ident := config.ReadString('irc', 'realname', 'slftp');

  sitesdat.WriteString('ircnet-' + nn, 'host', host);
  sitesdat.WriteInteger('ircnet-' + nn, 'port', port);
  sitesdat.WriteBool('ircnet-' + nn, 'ssl', boolean(ssl));
  sitesdat.WriteString('ircnet-' + nn, 'password', password);
  sitesdat.WriteString('ircnet-' + nn, 'nick', nick);
  sitesdat.WriteString('ircnet-' + nn, 'anick', '_' + nick);
  sitesdat.WriteString('ircnet-' + nn, 'ident', ident + '@soulless.ftp');
  sitesdat.WriteString('ircnet-' + nn, 'username', user);

  myIrcThreads.Add(TMyIrcThread.Create(nn));

  Result := True;

end;

function IrcModesNet(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  mode, n_modes, nn: AnsiString;
  mlist: TStringlist;
  I: Integer;
  ircn: TMyIrcThread;
begin
  result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  n_modes := mystrings.RightStr(params, length(nn) + 1);

  ircn := FindIrcnetwork(nn);
  if nil = ircn then
  begin
    irc_addtext(Netname, Channel,
      '<c5><b>ERROR</b></c>: Network with name <b>%s</b> doesnt exists!', [nn]);
    exit;
  end;

  //some more checks to inform the user what slftp have changed?

  mlist := TStringlist.Create;
  try
    mlist.Delimiter := ' ';
    mlist.DelimitedText := n_modes;
    for I := 0 to mlist.Count - 1 do
    begin

      mode := mlist.Strings[i];

      if length(mode) = 2 then
      begin

        if ((mode[1] = '+') and (mode[2] = 'h')) then
          ircn.MangleHost := True;
        if ((mode[1] = '-') and (mode[2] = 'h')) then
          ircn.MangleHost := False;

        if ((mode[1] = '+') and (mode[2] = 'i')) then
          ircn.Invisible := True;
        if ((mode[1] = '-') and (mode[2] = 'i')) then
          ircn.Invisible := False;

      end;
    end;
  finally
    mlist.free;
  end;
  result := True;
end;

function IrcModnet(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, password: AnsiString;
  ssl: integer;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  if (0 < Pos('-', nn)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  ssl := StrToIntDef(SubString(params, ' ', 2), -1);
  if ((ssl < 0) or (ssl > 1)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  password := SubString(params, ' ', 3);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s doesnt exists!', [nn]);
    exit;
  end;

  sitesdat.WriteBool('ircnet-' + nn, 'ssl', boolean(ssl));
  sitesdat.WriteString('ircnet-' + nn, 'password', password);

  IrcJump(Netname, Channel, nn);

  Result := True;

end;

function IrcDelnet(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, ii: integer;
  s: TSite;
  ircth: TMyIrcThread;
  b: TIrcBlowkey;
  x: TStringList;
begin
  params := UpperCase(trim(params));
  ircth := FindIrcnetwork(params);
  x := TStringList.Create;
  try
    if ircth <> nil then
    begin
      try
        sitesdat.ReadSection('ircnet-' + params, x);
        for ii := 0 to x.Count - 1 do
          sitesdat.DeleteKey('ircnet-' + params, x.Strings[ii]);
        sitesdat.EraseSection('ircnet-' + params);
        myIrcThreads.Remove(ircth);
      except
        on E: Exception do
          irc_addtext(Netname, Channel, 'Erase <b>irc-net</b> failed : %s', [E.Message]);
      end;
    end;

    // most meg le kell wipeolnunk a siteokrol is ezt a networkot
    try
      for i := 0 to sites.Count - 1 do
      begin
        s := sites[i] as TSite;
        if s.RCString('ircnet', '') = params then
        begin
          s.DeleteKey('ircnet');
        end;
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>ircnet from sites</b> failed : %s',
          [E.Message]);
    end;

    // most meg le kell torolnunk a chanjait
    i := 0;
    try
      while (i < chankeys.Count) do
      begin
        b := chankeys[i] as TIrcBlowkey;
        if b.Netname = params then
        begin
          sitesdat.EraseSection('channel-' + b.Netname + '-' + b.Channel);
          chankeys.Remove(b);
          Dec(i);
        end;
        Inc(i);
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>channels</b> failed : %s', [E.Message]);
    end;
  finally
    x.Free;
  end;
  Result := True;
end;

function IrcJump(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ircth: TMyIrcThread;
begin
  params := UpperCase(trim(params));

  ircth := FindIrcnetwork(params);
  if ircth <> nil then
  begin
    ircth.shouldrestart := True;
    myIrcThreads.Remove(ircth);
    myIrcThreads.Add(TMyIrcThread.Create(params));
  end;

  Result := True;

end;

function IrcStatus(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  th: TMyIrcThread;
begin
  for i := 0 to myIrcThreads.Count - 1 do
  begin
    th := TMyIrcThread(myIrcThreads[i]);
    // channel-FREE-#SM
    irc_addtext(Netname, Channel, format('%s (%s:%d): %s',
      [th.Netname, th.host, th.port, th.status]));
  end;
  Result := True;
end;

function IrcChannels(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  b: TIrcBlowkey;
  nn: AnsiString;
begin
  nn := UpperCase(trim(params));
  for i := 0 to chankeys.Count - 1 do
  begin
    b := chankeys[i] as TIrcBlowkey;
    if ((nn = '') or (nn = b.Netname)) then
    begin
      irc_addtext_b(Netname, Channel, format('%s@%s -> blowkey(%s) chankey(%s)',
        [b.Channel, b.Netname, b.blowkey, b.chankey]));
      // inviteonly(%s) BoolToStr(b.inviteonly, True)
    end;
  end;
  Result := True;
end;

function IrcSay(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel, tosay: AnsiString;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  tosay := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);
  if nil = FindIrcBlowfish(nn, blowchannel, False) then
  begin
    irc_addtext(Netname, Channel, 'Cant find channel.');
    exit;
  end;

  irc_addtext(nn, blowchannel, tosay);
  Result := True;
end;

function Check_For_Vailed_Chanrole(Name: AnsiString): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to integer(irc_chanroleindex) do
    if irc_chanroles[i] = Name then
    begin
      Result := True;
      break;
    end;
end;

function IrcSetChanName(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel, Names: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
  y: TStringList;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  Names := UpperCase(mystrings.RightStr(params, length(nn) + length(blowchannel) + 2));

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>ERROR</c>:</b> Cant find network -> %s', [nn]);
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);

  if b = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find Channel -> %s', [blowchannel]);
    exit;
  end;

  if Names = '' then
  begin
    irc_addtext_b(Netname, Channel, format('Channel name(s): %s', [trim(b.Names)]));
    Result := True;
    exit;
  end;

  if Names = '-' then
  begin
    b.Names := '';
    sitesdat.DeleteKey('channel-' + nn + '-' + blowchannel, 'names');
    Result := True;
    exit;
  end;

  y := TStringList.Create;
  try
    y.Delimiter := ' ';
    y.DelimitedText := Names;

    for i := 0 to y.Count - 1 do

      if not Check_For_Vailed_Chanrole(y.Strings[i]) then
      begin
        irc_addtext(Netname, Channel,
          '<c4><b>ERROR</c>:</b> %s is not a valid chanrole.', [y.Strings[i]]);
        Result := False;
        exit;
      end;

    b := FindIrcBlowfish(nn, blowchannel, False);
    if b <> nil then
    begin
      if Names = '' then
      begin
        irc_addtext_b(Netname, Channel, format('Channel name(s): %s',
          [trim(b.Names)]));
      end
      else if Names = '-' then
      begin
        b.Names := '';
        sitesdat.DeleteKey('channel-' + nn + '-' + blowchannel, 'names');
      end
      else
      begin
        b.Names := ' ' + Names + ' ';
        sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'names', Names);
      end;
    end
    else
      irc_addtext_b(Netname, Channel, format('Channel %s@%s not found',
        [blowchannel, nn]));

  finally
    y.Free;
  end;

  Result := True;
end;

function IrcSetChankey(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel, key: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  key := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b <> nil then
  begin
    b.chankey := key;
    sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'chankey', key);
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found',
      [blowchannel, nn]));

  Result := True;
end;

function IrcDelchan(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Network not found.');
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b <> nil then
  begin
    ircth.chanpart(blowchannel, ircth.BotNick);
    chankeys.Remove(b);
    sitesdat.EraseSection('channel-' + nn + '-' + blowchannel);
    ircth.shouldjoin := True;
    // console_delwindow(nn+' '+blowchannel);
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found',
      [blowchannel, nn]));

  Result := True;
end;

function IrcSetBlowkey(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel, key: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
  cbc:boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  key := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);
  cbc:= False;

 if AnsiStartsStr('cbc',key) then begin
   Delete(key,1,4);
   cbc:=True;
 end;

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b <> nil then
  begin
    b.UpdateKey(key);
    sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'blowkey', key);
    sitesdat.WriteBool('channel-' + nn + '-' + blowchannel, 'cbc', cbc);
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found',
      [blowchannel, nn]));

  Result := True;
end;

function IrcChanAdd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b = nil then
  begin
    sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'blowkey', '');
    irc_RegisterChannel(nn, blowchannel, '');
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s is already added',
      [blowchannel, Netname]));

  Result := True;
end;

function IrcSitechan(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, nn: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  nn := UpperCase(SubString(params, ' ', 2));
  if nn <> '' then
  begin
    if nil = FindIrcnetwork(nn) then
    begin
      irc_addtext(Netname, Channel, 'Cant find network.');
      exit;
    end;
  end;

  s := FindSiteByName(Netname, sitename);
  if nil = s then
  begin
    irc_addtext(Netname, Channel, 'Cant find site.');
    exit;
  end;
  if nn <> '' then
    s.WCString('ircnet', nn)
  else
    s.DeleteKey('ircnet');

  Result := True;
end;

function IrcRuleAdd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  r: TRule;
  sitename, rule, section, error: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rule := params;

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  r := AddRule(rule, error);
  if ((r = nil) or (error <> '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> %s', [error]);
    exit;
  end;

  rules.Add(r);
  RulesSave;

  irc_addtext(Netname, Channel, '<b>Added<b>: %d %s', [rules.Count - 1, r.AsText(True)]);

  Result := True;
end;

function IrcRuleIns(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, error: AnsiString;
  s: TSite;
begin
  Result := False;
  id := StrToIntDef(SubString(params, ' ', 1), -1);
  sitename := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rule := Copy(params, length(IntToStr(id)) + 2, 1000);

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  if ((id < 0) or (id >= rules.Count)) then
  begin
    irc_addtext(Netname, Channel, 'Incorrect rule ID!');
    exit;
  end;

  r := AddRule(rule, error);
  if ((r = nil) or (error <> '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> %s', [error]);
    exit;
  end;

  rules.Insert(id, r);
  RulesSave;

  irc_addtext(Netname, Channel, '<b>Inserted<b>: %d %s', [id, r.AsText(True)]);

  Result := True;
end;

function IrcRuleMod(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, error: AnsiString;
  s: TSite;
begin
  Result := False;
  id := StrToIntDef(SubString(params, ' ', 1), -1);
  sitename := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rule := Copy(params, length(IntToStr(id)) + 2, 1000);

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  if ((id < 0) or (id >= rules.Count)) then
  begin
    irc_addtext(Netname, Channel, 'Incorrect rule id');
    exit;
  end;

  r := AddRule(rule, error);
  if ((r = nil) or (error <> '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> %s', [error]);
    exit;
  end;

  irc_addtext(Netname, Channel, '<b>Modified<b>: %d %s <u><b>to</b></u> %s', [id,
    TRule(rules[id]).AsText(True), r.AsText(True)]);

  rules.Delete(id);
  rules.Insert(id, r);
  RulesSave;

  Result := True;
end;

function IrcRuleDel(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  id: integer;
begin
  Result := False;
  id := StrToIntDef(params, -1);

  if ((id < 0) or (id >= rules.Count)) then
  begin
    irc_addtext(Netname, Channel, 'Incorrect rule id (%s)', [params]);
    exit;
  end;

  Irc_AddText(netname, channel, '<c4><b>Deleted</b></c>: <b>%s</b> %s', [params,
    TRule(rules.Items[id]).AsText(true)]);

  rules.Delete(id);
  RulesSave;

  Result := True;
end;

function IrcRuleCopy(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  rr, r: TRule;
  rule, error, src_s, dst_s, src_section: AnsiString;
  ss: TSite;
  i: integer;
begin
  Result := False;
  src_s := UpperCase(SubString(params, ' ', 1));
  dst_s := UpperCase(SubString(params, ' ', 2));
  src_section := UpperCase(SubString(params, ' ', 3));

  ss := FindSiteByName('', src_s);
  if ss = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [src_s]);
    exit;
  end;

  ss := FindSiteByName('', dst_s);
  if ss = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [dst_s]);
    exit;
  end;

  // maybe needed to block inserting new rule while copying
  //queue_lock.Enter;
  //try

  for i := 0 to rules.Count - 1 do
  begin
    r := TRule(rules.Items[i]);
    if ((r.sitename = src_s) and (r.section = src_section)) then
    begin
      rule := dst_s + ' ' + src_section + ' ' + r.AsText(False);
      rr := nil;
      rr := AddRule(rule, error);
      if ((rr = nil) or (error <> '')) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> %s', [error]);
        Continue;
      end;
      rules.Add(rr);
    end;
  end;

  RulesSave;

  Irc_AddText(netname, channel, '<b>Copied</b>: %s to %s for section %s', [src_s, dst_s, src_section]);

  //finally
  //  queue_lock.Leave;
  //end;

  Result := True;
end;

function IrcRuleHelp(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s, ss: AnsiString;
begin

  if FindConditionClassByName(params) = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>Rule condition "<b>%s</b>" not found!</c>', [params]);
    exit;
  end;

  for i := 0 to conditions.Count - 1 do
  begin
    if TConditionClass(conditions[i]).Name = params then
    begin
      s := TConditionClass(conditions[i]).Description;
      while (True) do
      begin
        ss := elsosor(s);
        if ss = '' then
          break;
        irc_addtext(Netname, Channel, ss);
      end;

      if conditions[i] <> TBooleanCondition then
        irc_addtext(Netname, Channel, '<b>Accepted ops:</b> ' +
          TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      break;
    end;
  end;

  Result := True;
end;

function IrcRules(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  r: TRule;
  s: TSite;
  sitename, section: AnsiString;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));

  if sitename <> '*' then
  begin
    s := FindSiteByName('', sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
      exit;
    end;

    if section <> '*' then
    begin
      if not s.IsSection(section) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</b></c>: %s is not valid section!',
          [section]);
        exit;
      end;
    end;

  end;

  // display global rules
  if (((sitename <> '*') or (section <> '*')) or
    ((sitename = '*') and (section = '*'))) then
  begin
    for i := 0 to rtpl.Count - 1 do
    begin
      r := TRule(rtpl[i]);
      if ((r.sitename = '*') and (r.section = '*')) then
      begin
        irc_addtext(Netname, Channel, 'rtpl-%d %s', [i, r.AsText(True)]);
      end;
    end;
    for i := 0 to rules.Count - 1 do
    begin
      r := TRule(rules[i]);
      if ((r.sitename = '*') and (r.section = '*')) then
      begin
        irc_addtext(Netname, Channel, 'rule-%d %s', [i, r.AsText(True)]);
      end;
    end;
  end;

  // display global section rules
  if ((sitename <> '*') or ((sitename = '*') and (section <> '*'))) then
  begin
    for i := 0 to rtpl.Count - 1 do
    begin
      r := TRule(rtpl[i]);
      if ((r.sitename = '*') and (r.section = section)) then
      begin
        irc_addtext(Netname, Channel, 'rtpl-%d %s', [i, r.AsText(True)]);
      end;
    end;
    for i := 0 to rules.Count - 1 do
    begin
      r := TRule(rules[i]);
      if ((r.sitename = '*') and (r.section = section)) then
      begin
        irc_addtext(Netname, Channel, 'rule-%d %s', [i, r.AsText(True)]);
      end;
    end;
  end;

  // display global site rules
  if ((section <> '*') or ((sitename <> '*') and (section = '*'))) then
  begin
    for i := 0 to rtpl.Count - 1 do
    begin
      r := TRule(rtpl[i]);
      if ((r.sitename = sitename) and (r.section = '*')) then
      begin
        irc_addtext(Netname, Channel, 'rtpl-%d %s', [i, r.AsText(True)]);
      end;
    end;
    for i := 0 to rules.Count - 1 do
    begin
      r := TRule(rules[i]);
      if ((r.sitename = sitename) and (r.section = '*')) then
      begin
        irc_addtext(Netname, Channel, 'rule-%d %s', [i, r.AsText(True)]);
      end;
    end;
  end;

  // display site section rules
  if ((sitename <> '*') and (section <> '*')) then
  begin
    for i := 0 to rtpl.Count - 1 do
    begin
      r := TRule(rtpl[i]);
      if ((r.sitename = sitename) and (r.section = section)) then
      begin
        irc_addtext(Netname, Channel, 'rtpl-%d %s', [i, r.AsText(True)]);
      end;
    end;
    for i := 0 to rules.Count - 1 do
    begin
      r := TRule(rules[i]);
      if ((r.sitename = sitename) and (r.section = section)) then
      begin
        irc_addtext(Netname, Channel, 'rule-%d %s', [i, r.AsText(True)]);
      end;
    end;
  end;
  Result := True;
end;

function IrcRuleList(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  r: TRegExpr;
begin
  Result := False;
  r := TRegExpr.Create;
  r.ModifierI := True;
  try

    for i := 0 to conditions.Count - 1 do
    begin
      if UpperCase(params) = 'COMMON' then
      begin
        r.Expression := '^(MP3|0DAY|IMDB|NFO|TV|MVID|GAME|APP)[\w\d]+$';
        if not r.Exec(TConditionClass(conditions[i]).Name) then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      end
      else if params <> '' then
      begin
        r.Expression := format('^%s[\w\d]+$', [params]);
        if r.Exec(TConditionClass(conditions[i]).Name) then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      end
      else
      begin
        if conditions[i] <> TBooleanCondition then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText)
        else
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name);
      end;
    end;

  finally
    r.Free;
  end;

  Result := True;
end;

function IrcPrereload(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  vs: AnsiString;
begin
  vs := PrecatcherReload;
  if vs <> '' then
    irc_addtext(Netname, Channel, vs);
  Result := True;
end;

function IrcPreadd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, nn, channelname, botnicks, event, words, section: AnsiString;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  nn := UpperCase(SubString(params, ' ', 2));
  channelname := SubString(params, ' ', 3);
  botnicks := SubString(params, ' ', 4);
  event := UpperCase(SubString(params, ' ', 5));
  words := SubString(params, ' ', 6);
  section := SubString(params, ' ', 7);
  (*
    if event = '-' then event:= '';
    if words = '-' then words:= '';
    if section = '-' then section:= '';
  *)

  if ((event <> 'PRE') and (event <> 'COMPLETE') and (event <> 'NEWDIR') and
    (event <> 'NUKE') and (event <> 'REQUEST')) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, unknown event: ' + event);
    exit;
  end;

  if nil = FindSiteByName(Netname, sitename) then
  begin
    irc_addtext(Netname, Channel, 'Site not found');
    exit;
  end;

  if nil = FindIrcBlowfish(nn, channelname, False) then
  begin
    irc_addtext(Netname, Channel, 'Channel not found.');
    exit;
  end;

  catcherFile.Add(format('%s;%s;%s;%s;%s;%s;%s', [nn, channelname,
    botnicks, sitename, event, words, section]));
  PrecatcherRebuild;
  Result := True;
end;

function IrcPredel(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
begin
  Result := False;
  i := StrToIntDef(params, -1);
  if i < 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if catcherFile.Count > i then
    catcherFile.Delete(i);
  PrecatcherRebuild();
  Result := True;
end;

function IrcPrecatchtest(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  net, chan, nick, rest: AnsiString;
begin
  Result := False;

  net := UpperCase(SubString(params, ' ', 1));
  chan := SubString(params, ' ', 2);
  nick := SubString(params, ' ', 3);
  rest := mystrings.RightStr(params, length(net) + length(chan) + length(nick) + 3);

  if nil = FindIrcBlowfish(net, chan, False) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error: %s@%s not found', [net, chan]);
    exit;
  end;

  precatcher_debug := True;
  precatcher_debug_netname := Netname;
  precatcher_debug_channel := Channel;
  PrecatcherProcessB(net, chan, nick, rest);
  precatcher_debug := False;
  Result := True;
end;

function IrcPreCatchDebug(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  try
    if params <> '' then
    begin
      if params = '0' then
        precatcher_ircdebug := False;
      if params = '1' then
        precatcher_ircdebug := True;
      irc_addtext(Netname, Channel, 'CatchDebug is: ' +
        BoolToStr(precatcher_ircdebug));
    end
    else
      irc_addtext(Netname, Channel, 'CatchDebug is: ' +
        BoolToStr(precatcher_ircdebug));
  finally
    Result := True;
  end;
end;

function IrcPrelist(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s1, s2: AnsiString;
  mehetki: boolean;
  nn, aktchannel, sitename, nick, event, words, section: AnsiString;
begin
  Result := False;
  s1 := UpperCase(SubString(params, ' ', 1));
  s2 := SubString(params, ' ', 2);

  if ((s1 <> '') and (s2 <> '')) then
  begin
    if nil = FindIrcBlowfish(s1, s2, False) then
    begin
      irc_addtext(Netname, Channel, 'Cant find channel.');
      exit;
    end;
  end
  else if (s1 <> '') then
  begin
    if nil = FindSiteByName(Netname, s1) then
    begin
      irc_addtext(Netname, Channel, 'Cant find site.');
      exit;
    end;
  end;

  for i := 0 to catcherFile.Count - 1 do
  begin
    nn := SubString(catcherFile[i], ';', 1);
    aktchannel := SubString(catcherFile[i], ';', 2);
    nick := SubString(catcherFile[i], ';', 3);
    sitename := SubString(catcherFile[i], ';', 4);
    event := SubString(catcherFile[i], ';', 5);
    words := SubString(catcherFile[i], ';', 6);
    section := SubString(catcherFile[i], ';', 7);

    mehetki := False;
    if ((s1 <> '') and (s2 <> '')) then
    begin
      if ((s1 = nn) and (s2 = aktchannel)) then
        mehetki := True;
    end
    else if (s1 <> '') then
    begin
      if sitename = s1 then
        mehetki := True;
    end
    else
      mehetki := True;

    if mehetki then
      irc_addtext(Netname, Channel, '#%d %s-%s-%s <%s> [%s] {%s} (%s)',
        [i, sitename, nn, aktchannel, nick, event, words, section]);
  end;
  Result := True;
end;

function IrcUptime(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: AnsiString;
begin
  s := ReplaceThemeMSG(format('<b>%s</b> is up for [%s] <c7><b>%s</b></c>',
    [Get_VersionString, DatetimetoStr(started), DateTimeAsString(started)]));
  irc_addtext(Netname, Channel, s);
  Result := True;
end;

procedure _readHelpTXTFile(const Netname, Channel: AnsiString; filename: AnsiString);
var
  s, fn: AnsiString;
  f: TextFile;
begin
  fn := 'help' + PathDelim + filename + '.txt';
  if FileExists(fn) then
  begin
    try
      AssignFile(f, fn);
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
        s := trim(s);
        if s <> '' then
        begin
          s := Csere(s, '<prefix>', irccmdprefix);
          s := Csere(s, '<cmdprefix>', irccmdprefix);
          s := Csere(s, '<cmd>', irccmdprefix + filename);
          irc_addtext(Netname, Channel, s);
        end;
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

function IrcHelpv2(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  ss, s: AnsiString;
begin
  if (params = '') then
  begin
    _readHelpTXTFile(Netname, Channel, 'nhelp');
  end;

  //Show all commands Start
  if ((params = '--all') or (params = '-all') or (params = '--a') or (params = '-a')) then
  begin
    irc_addtext(Netname, Channel, '<b><u>Available commands are</b>:</u>');
    s := '';
    for i := Low(irccommands) to High(irccommands) do
    begin

      if ((irccommands[i].cmd[1] = '-') or (AnsiStartsText('$',irccommands[i].hlpgrp))) then
      begin
        if s <> '' then
          irc_addtext(Netname, Channel, s);
        if (AnsiStartsText('$',irccommands[i].hlpgrp)) then
          irc_addtext(Netname, Channel, ':: <u><c7><b>%s</c></u> :</b>', [irccommands[i].cmd]);
        s := '';
      end
      else
      begin
        if s <> '' then
          s := s + ', ';
        s := s + irccmdprefix + irccommands[i].cmd;
      end;
    end;
    if s <> '' then
      IrcLineBreak(Netname, Channel, s, ',', '', 9);
    irc_addtext(Netname, Channel,
      'Type <b>%shelp</b> command to get detailed info.', [irccmdprefix]);
    result := True;
    Exit;
    //Show all commands End
  end;

  //Find commands by hlpgrp Start
  if AnsiStartsText('-', params) then
  begin
    ss := params;
    Delete(ss, 1, 1);
    if AnsiIndexText(ss, helpCommands) > -1 then
    begin
      for i := Low(irccommands) to High(irccommands) do
      begin
        if ((irccommands[i].hlpgrp = ss) or (irccommands[i].hlpgrp = '$' + ss)) then
        begin
          if AnsiContainsText(irccommands[i].hlpgrp, '$') then
            irc_addtext(Netname, Channel, ':: <u><c7><b>%s</c></u> :</b>', [irccommands[i].cmd])
          else
          begin
            if (irccommands[i].cmd <> '-') then
            begin
              if s <> '' then
                s := s + ', ';
              s := s + irccmdprefix + irccommands[i].cmd;
            end;
          end;
        end;
      end;
      if s <> '' then
        IrcLineBreak(Netname, Channel, s, ',', '', 12);
      result := True;
      Exit;
    end;
    //Find commands by hlpgrp End
    irc_addtext(Netname, Channel, 'Help group <b>%s</b> not found.', [params]);
    result := True;
    Exit;
  end;
  //Display Textfile Start
  if params <> '' then
  begin
    if (1 = Pos(irccmdprefix, params)) then
      Delete(params, 1, 1);
    i := FindIrcCommand(params);
    if i <> 0 then
    begin
      if FileExists('help' + PathDelim + params + '.txt') then
      begin
        _readHelpTXTFile(Netname, Channel, params);
      end
      else // if FileExists(fn) then begin
        irc_addtext(Netname, Channel, '<c4>No help available on</c> ' + params);
    end
    else
      irc_addtext(Netname, Channel, 'Command <b>%s</b> not found.', [params]);
    result := True;
    Exit;
    //Display Textfile End
  end;
end;

function IrcHelp(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s: AnsiString;
  f: TextFile;
  fn: AnsiString;
  //  sl: TStringList;
  //  r:  TRegExpr;
begin

  //  r := TRegExpr.Create;
  if params <> '' then
  begin
    if (1 = Pos(irccmdprefix, params)) then
      // commandhandlerrel kezdodik     -- commandhandler rel begins
      params := Copy(params, length(irccmdprefix) + 1, 1000);

    i := FindIrcCommand(params);
    if i <> 0 then
    begin
      // fn:= 'help'+PathDelim+'irc'+PathDelim+params+'.txt';
      fn := 'help' + PathDelim + params + '.txt';
      if FileExists(fn) then
      begin
        AssignFile(f, fn);
        Reset(f);
        while not EOF(f) do
        begin
          ReadLn(f, s);
          s := trim(s);
          if s <> '' then
          begin
            s := Csere(s, '<prefix>', irccmdprefix);
            s := Csere(s, '<cmdprefix>', irccmdprefix);
            s := Csere(s, '<cmd>', irccmdprefix + params);
            irc_addtext(Netname, Channel, s);
          end; // if s <> '' then begin
        end; // while
        CloseFile(f);
      end
      else // if FileExists(fn) then begin
        irc_addtext(Netname, Channel, '<c4>No help available on</c> ' + params);
    end
    else
      irc_addtext(Netname, Channel, '<b>Command not found.</b>');
  end
  else
  begin

    irc_addtext(Netname, Channel, '<b><u>Available commands are:</b></u>');
    s := '';
    for i := Low(irccommands) to High(irccommands) do
    begin
      if AnsiContainsText(irccommands[i].hlpgrp, '$') then
      begin
        if s <> '' then
          irc_addtext(Netname, Channel, s);
        //
          irc_addtext(Netname, Channel, ':: <u><c7><b>%s</c></u> :</b>',
            [irccommands[i].cmd]);
        s := '';
      end
      else
      begin
        if s <> '' then
          s := s + ', ';
          if (irccommands[i].cmd <> '-') then
        s := s + irccmdprefix + irccommands[i].cmd;
      end;
    end;
    if s <> '' then
      irc_addtext(Netname, Channel, s);
    irc_addtext(Netname, Channel,
      '<b>Type %shelp command to get detailed info</b>.', [irccmdprefix]);
  end;

  Result := True;
end;

function IrcSites(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  spd, sup, sdn, suk: TStringList;
  scount: integer;
begin
  scount := sites.Count - 1;

  sup := TStringList.Create;
  spd := TStringList.Create;
  sdn := TStringList.Create;
  suk := TStringList.Create;
  try
    SitesWorkingStatusToStringlist(Netname, Channel, sup, sdn, suk, spd);

    // make it alphabetically
    sup.Sort;
    spd.Sort;
    sdn.Sort;
    suk.Sort;

    IrcLineBreak(Netname, Channel, sup.commatext, AnsiChar('"'),
      'UP(' + IntToStr(sup.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, sdn.commatext, AnsiChar('"'),
      'DN(' + IntToStr(sdn.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, suk.commatext, AnsiChar('"'),
      '??(' + IntToStr(suk.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, spd.commatext, AnsiChar('"'),
      'PD(' + IntToStr(spd.Count) + '/' + IntToStr(scount) + '): ');
  finally
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function IrcAddSiteInfos(const netname, channel: AnsiString; params: AnsiString):
  boolean;
var
  s: TSite;
  Text, sitename: AnsiString;
begin
  sitename := SubString(params, ' ', 1);
  Text := mystrings.RightStr(params, length(sitename) + 2);
  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    Result := False;
    exit;
  end;

  if Text = '' then
  begin
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39),
      s.Name]);
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39),
      s.SiteInfos]);
  end
  else
  begin
    s.SiteInfos := Text;
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39),
      s.Name]);
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39),
      s.SiteInfos]);
  end;
  Result := True;
end;

function IrcInfo(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s: TSite;
  sitename: AnsiString;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(params);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  x := TStringList.Create;
  try

    irc_addtext(Netname, Channel, '<b>Site</b> %s:', [s.Name]);
    irc_addtext(Netname, Channel, ' name/speed/location/size:B %s / %s / %s / %s',
      [s.RCString('name', '??'), s.RCString('link', '??'), s.Country, s.RCString('size', '??')]);
    irc_addtext(Netname, Channel, ' sections:B %s', [s.sections]);

    sitesdat.ReadSection('site-' + sitename, x);
    x.Sort;
    for i := 0 to x.Count - 1 do
    begin
      if x[i] = 'affils-PRE' then
        Continue;

      if 1 = Pos('affils-', x[i]) then
        irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
    end;

    x.DelimitedText := s.leechers;
    irc_addtext(Netname, Channel, ' leechers (%d/%d):B %s',
      [x.Count, s.RCInteger('maxleechers', -1), x.DelimitedText]);
    x.DelimitedText := s.traders;
    irc_addtext(Netname, Channel, ' traders (%d/%d):B %s',
      [x.Count, s.RCInteger('maxtraders', -1), x.DelimitedText]);

    if s.RCString('notes', '') <> '' then
      irc_addtext(Netname, Channel, ' notes: ' + s.RCString('notes', ''));

  finally
    x.Free;
  end;

  Result := True;
end;

function IrcSite(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, i_sec, j_sec: integer;
  s: TSite;
  host, sitename: AnsiString;
  x: TStringList;
  s_section, s_sections: AnsiString;
begin
  Result := False;
  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  x := TStringList.Create;
  try
    sitesdat.ReadSection('site-' + sitename, x);
    x.Sort;

    irc_addtext(Netname, Channel, 'Site <b>%s</b>:', [sitename]);
    for i := 0 to x.Count - 1 do
    begin
      if x[i] = 'password' then
        Continue;
      if (Copy(x[i], 1, 3) = 'bnc') then
        Continue;

      if x.Strings[i] = 'sslmethod' then
      begin
        irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), sslMethodToSTring(s)]);
        Continue;
      end;

      if x.Strings[i] = 'sw' then
      begin
        irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), SiteSoftWareToSTring(s)]);
        Continue;
      end;

      if (AnsiStartsStr('pretime', x.Strings[i]) or (x.Strings[i] = 'pretime-*')) then
      begin
        if s.RCInteger(x.Strings[i], 120) > 60 then
        begin
          case round(s.RCInteger(x.Strings[i], 120) / 60) of
            1:
              irc_addtext(Netname, Channel, ' %s: %s seconds (%d minute)', [x[i],
                s.RCString(x[i], ''), round(s.RCInteger(x.Strings[i], 120) / 60)]);
          else
            irc_addtext(Netname, Channel, ' %s: %s seconds (%d minutes)', [x[i],
              s.RCString(x[i], ''), round(s.RCInteger(x.Strings[i], 120) / 60)]);
          end;
        end
        else
          irc_addtext(Netname, Channel, ' %s: %s seconds', [x[i], s.RCString(x[i], '')]);
        Continue;
      end;

      if x.Strings[i] = 'affils' then
      begin
        IrcLineBreak(Netname, Channel, s.RCString(x.Strings[i], ''), ' ', ' affils: ');
        Continue;
      end;

      if x.Strings[i] = 'country' then
      begin
        if (s.RCString(x[i], '')[1] <> '.') then
        begin
          irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
          continue;
        end
        else
        begin
          //we can use j_sec because it's set to 0 below when used there - so no need to create a new integer variable!
          j_sec := AnsiIndexText(copy(s.RCString(x[i], ''), 2, length(s.RCString(x[i], ''))), CountryCodes);
          irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), CountryNames[j_sec]]);
          continue;
        end;
      end
      else
      begin
        if ((x.Strings[i] = 'sections') or (x.Strings[i] = 'autoindexsections')) then
        begin
          j_sec := 0;
          s_sections := '';
          for i_sec := 1 to 1000 do
          begin
            s_section := SubString(s.RCString(x[i], ''), ' ', i_sec);
            if s_section = '' then
              break;

            if s_sections <> '' then
            begin
              s_sections := s_sections + ', ' + s_section;
            end
            else
            begin
              s_sections := s_section;
            end;
            Inc(j_sec);

            if (j_sec >= 10) then
            begin
              //irc_addtext(Netname, Channel, ' %s: %s', [x[i], s_sections]);
              IrcLineBreak(Netname, Channel, s_sections, ',', ' ' + x.Strings[i] + ': ', 9);
              j_sec := 0;
              s_sections := '';
            end;
          end;
          if s_sections <> '' then
          begin
            IrcLineBreak(Netname, Channel, s_sections, ',', ' ' + x.Strings[i] + ': ', 9);
          end;
        end
        else
        begin
          irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
        end;
      end;
    end;

  finally
    x.Free;
  end;

  i := 0;
  while (not slshutdown) do
  begin
    host := s.RCString('bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;

    irc_addtext(Netname, Channel, ' bnc: %s:%d',
      [host, s.RCInteger('bnc_port-' + IntToStr(i), 0)]);

    Inc(i);
  end;

  Result := True;
end;

function IrcBnc(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s: TSite;
  host, sitename: AnsiString;
begin
  Result := False;
  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error:</b> </c>Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  irc_addtext(Netname, Channel, 'Site <b>%s</b>:', [sitename]);

  i := 0;
  while (not slshutdown) do
  begin
    host := s.RCString('bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;

    irc_addtext(Netname, Channel, ' bnc: %s:%d',
      [host, s.RCInteger('bnc_port-' + IntToStr(i), 0)]);

    Inc(i);
  end;

  Result := True;
end;

function IrcDie(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  try
    slshutdown := IrcSetdown(Netname, Channel, '!ALL!');
  finally
    Result := slshutdown;
  end;
end;

function IrcAffils(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  affils, ss, sitename: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  affils := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Site <b>%s</b> not found.',
      [sitename]);
    exit;
  end;

  if affils <> '' then
  begin
    s.siteAffils := affils;
  end;
  ss := s.SiteAffils;
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, ' ', Format('<b>%s</b>@%s : ', ['', sitename]), 12)
  else
    irc_addText(Netname, Channel, 'No affils available.');
  (*
    ss := s.siteaffils;
    if ss <> '' then
      IrcLineBreak(Netname, Channel, ss, ' ', Format('<b>%s</b>@%s : ',
        ['', sitename]), 12);
        *)
  Result := True;
end;

function IrcIdent(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss, sitename: AnsiString;
  s: TSite;
  ident: AnsiString;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  ident := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if ident <> '' then
    s.WCString('ident', ident)
  else
    s.DeleteKey('ident');
  ss := s.RCString('ident', config.ReadString(section, 'response', 'rsctm'));
  if ss <> '' then
    irc_addtext(Netname, Channel, 'Ident reply for %s is %s', [sitename, ss]);

  Result := True;
end;

function IrcKnowngroups(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  KnownGroupsStart();
  Result := True;
end;

function IrcNoSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  q: integer;
  s2: AnsiString;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  s2 := mystrings.RightStr(params, length(sitename) + 1);
  q := StrToIntDef(s2, -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if q = 1 then
    s.WCInteger('nosocks5', q)
  else if q = 0 then
    s.DeleteKey('nosocks5');
  q := s.RCInteger('nosocks5', 0);

  irc_addtext(Netname, Channel, 'Nosocks5 for %s is %d', [sitename, q]);

  Result := True;
end;

function IrcLookup(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, section, dir: AnsiString;
  p: TPazo;
  ps: TPazoSite;
  i: integer;
begin
  Result := False;
  i := -1;
  section := UpperCase(SubString(params, ' ', 1));

  if kb_sections.IndexOf(section) <> -1 then
  begin
    sitename := '';
    dir := SubString(params, ' ', 2);
  end
  else
  begin
    sitename := section;
    section := UpperCase(SubString(params, ' ', 2));

    if kb_sections.IndexOf(section) = -1 then
    begin
      irc_addtext(Netname, Channel,
        '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].');
      exit;
    end;

    dir := SubString(params, ' ', 3);
  end;

  if ((dir = '') or (dir = section) or (sitename = dir)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: No valid Rip found!');
    exit;
  end;

  if CheckForBadAssGroup(dir) then
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Skipped group found...');

  try
    // i:= kb_add(netname, channel, '', section, '', 'NEWDIR', dir, '', True);
    i := kb_Add(Netname, Channel, sitename, section, '', 'NEWDIR', dir, '', True);

  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, format('[EXCEPTION] IrcLookup_kb_add : %s', [E.Message]));
      exit;
    end;
  end;

  if i <> -1 then
  begin
    p := TPazo(kb_list.Objects[i]);
    p.AddSites;
    p.rls.aktualizalva := False;
    if sitename <> '' then
    begin
      ps := p.FindSite(sitename);
      if ps <> nil then
        ps.lookupforcedhere := True;
    end;
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Cant find');
    exit;
  end;

  Result := True;
end;

function IrcLeechers(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss, sitename, users: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  users := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  ss := s.SetLeechers(users, True);
  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcCountry(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, country, countrywithoutdot: AnsiString;
  i: Integer;
  s: TSite;
begin
  Result := False;
  sitename := AnsiUpperCase(SubString(params, ' ', 1));
  country := AnsiUpperCase(mystrings.RightStr(params, length(sitename) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  if (country[1] <> '.') then
  begin
    irc_addtext(Netname, Channel, 'The location/country need to begin with a dot!');
    exit;
  end;

  countrywithoutdot := copy(country, 2, length(country));
  i := AnsiIndexText(countrywithoutdot, CountryCodes);
  if not (i > -1) then
  begin
    irc_addtext(Netname, Channel, 'Country %s is not a valid country! Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements', [country]);
    exit;
  end;

  s.Country := country;
  irc_addtext(Netname, Channel, 'Country for %s set to %s (%s)', [sitename, s.Country, CountryNames[i]]);

  Result := True;
end;

function IrcLink(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, link: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  link := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('link', link);
  irc_addtext(Netname, Channel, link);

  Result := True;
end;

function IrcNotes(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, notes: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  notes := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('notes', notes);
  irc_addtext(Netname, Channel, notes);

  Result := True;
end;

function IrcSize(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, size: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  size := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('size', size);
  irc_addtext(Netname, Channel, size);

  Result := True;
end;

function IrcName(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, Name: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  Name := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('name', Name);
  irc_addtext(Netname, Channel, Name);

  Result := True;
end;

function IrcTraders(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss, sitename, users: AnsiString;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  users := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  ss := s.SetTraders(users, True);
  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcUserslots(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  s: TSite;
  leechslots, ratioslots: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  leechslots := StrToIntDef(SubString(params, ' ', 2), -1);
  ratioslots := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  s.WCInteger('maxleechers', leechslots);
  s.WCInteger('maxtraders', ratioslots);

  Result := True;
end;

function IrcFreeslots(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  db: integer;
  ss: AnsiString;
begin

  db := 0;
  ss := '';
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    ss := ss + format('<b>%s</b> (%d/%d) ', [s.Name, s.FreeTraderSlots,
      s.FreeLeechSlots]);
    Inc(db);
    if db >= 5 then
    begin
      irc_addtext(Netname, Channel, ss);
      ss := '';
      db := 0;
    end;
  end;

  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcFindAffil(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  db, i: integer;
  ss: AnsiString;
  affil: AnsiString;
begin
  affil := SubString(params, ' ', 1);

  ss := '';
  db := 0;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsAffil(affil) then
    begin
      ss := ss + format('<b>%s</b> (%d %d) ', [s.Name, s.FreeTraderSlots,
        s.FreeLeechSlots]);

      Inc(db);
      if db >= 5 then
      begin
        irc_addtext(Netname, Channel, ss);
        db := 0;
        ss := '';
      end;
    end;

  end;

  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcFindCountry(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  site_found: boolean;
  country, ss: AnsiString;
begin
  country := SubString(params, ' ', 1);
  site_found := False;
  ss := format('Site(s) with Country %s:', [Country]);

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if country = s.Country then
    begin
      ss := ss + format(' <b>%s</b>', [s.Name]);
      site_found := True;
    end;

  end;

  if site_found then
  begin
    irc_addtext(Netname, Channel, ss);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'No sites with this Country found!');
  end;

  Result := True;
end;

function IrcFindSection(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  s: TSite;
  i: integer;
  section: AnsiString;
begin
  section := UpperCase(SubString(params, ' ', 1));

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsSection(section) then
      irc_addtext(Netname, Channel, '<b>%s</b>: %d %d',
        [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
  end;

  Result := True;
end;

function IrcAuto(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
  try
    if params <> '' then
    begin
      if params = '0' then
      begin
        sitesdat.WriteBool('precatcher', 'auto', False);
        irc_addtext(Netname, Channel, Format('Auto is disabled (%s) now!',
          [IntToStr(integer(precatcherauto))]));
      end;

      if params = '1' then
      begin
        sitesdat.WriteBool('precatcher', 'auto', True);
        irc_addtext(Netname, Channel, Format('Auto is enabled (%s) now!',
          [IntToStr(integer(precatcherauto))]));
      end;
    end
    else
      irc_addtext(Netname, Channel,
        Format('Precatcher auto is: %s [1 means enabled - 0 means disabled]',
        [IntToStr(integer(precatcherauto))]));
  finally
    Result := True;
  end;
end;
(*

function IrcAutoCrawler(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: AnsiString;
  ss: AnsiString;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1 +
    length(IntToStr(status)) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;
  if ((status > -1) and (status <> 0)) then
  begin
    // hitelesitjuk a szekciokat
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        break;

      if s.sectiondir[ss] = '' then
      begin
        irc_addtext(Netname, Channel, 'Site %s has no %s section',
          [sitename, ss]);
        exit;
      end;
    end;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.RCInteger('autocrawler', 0) <= 0 then
        kell := True;
      s.WCInteger('autocrawler', status);
      s.WCString('autocrawlersections', sections);
    end
    else
    begin
      s.DeleteKey('autocrawler');
      s.DeleteKey('autocrawlersections');
      s.DeleteKey('nextautocrawler');
      s.RemoveAutoCrawler;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autocrawler of %s is: %d (%s)',
    [sitename, s.RCInteger('autocrawler', 0), s.RCString('autocrawlersections',
      '')]);

  if kell then
    s.AutoCrawler;

  Result := True;
end;

function IrcCrawler(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  if params <> '' then
  begin
    crawler_enabled := boolean(StrToIntDef(params, 0));
    sitesdat.WriteBool('crawler', 'enabled', crawler_enabled);
  end;
  irc_addtext(Netname, Channel, 'Crawler is: ' +
    IntToStr(integer(crawler_enabled)));

  Result := True;
end;

function IrcConfirmerAnnounce(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  if params <> '' then
  begin
    confirmer_announce := boolean(StrToIntDef(params, 0));
    sitesdat.WriteBool('crawler', 'confirmer_announce', confirmer_announce);
  end;
  irc_addtext(Netname, Channel, 'Confirmer announce is: ' +
    IntToStr(integer(confirmer_announce)));

  Result := True;
end;

function IrcCrawl(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  y, m, d: integer;
  dd: TDateTime;
  sitename, section: AnsiString;
  i: integer;
  s: TSite;
  asc, sc: AnsiString;
begin
  Result := False;

  y := StrToIntDef(SubString(params, ' ', 1), -1);
  m := StrToIntDef(SubString(params, ' ', 2), -1);
  d := StrToIntDef(SubString(params, ' ', 3), -1);

  if not TryEncodeDate(y, m, d, dd) then
  begin
    irc_addtext(Netname, Channel, 'Invalid date');
    exit;
  end;

  sitename := UpperCase(SubString(params, ' ', 4));
  section := UpperCase(SubString(params, ' ', 5));

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    if (sitename = '') or (sitename = s.Name) then
    begin
      sc := s.RCString('autocrawlersections', '');
      while (True) do
      begin
        asc := Fetch(sc, ' ');
        if ((asc = '') and (sc = '')) then
          break;

        if ((section = '') or (section = asc)) then
        begin
          // task hozzaadasa
          AddTask(TAutoCrawlerTask.Create(Netname, Channel, sitename,
            section, dd));
        end;
      end;
    end;
  end;

  Result := True;
end;
*)

function IrcAutoLogin(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  i: integer;
  x: TStringList;
begin
  Result := True;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;
      TSite(sites.Items[i]).WCInteger('autologin', status);
      irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [TSite(sites.Items[i]).Name,
        integer(TSite(sites.Items[i]).RCBool('autologin', False))]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);

        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        if (s.PermDown) then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set to PermDown.', [x.Strings[i]]);
          Continue;
        end;

        if status > -1 then
        begin
          s.WCInteger('autologin', status);
        end;
        irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [sitename,
          integer(s.RCBool('autologin', False))]);
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcAutoBnctest(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
  i: integer;
  x: TStringList;
begin
  //  Result   := True;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites',
        'admin_sitename', 'SLFTP')) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      kell := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).RCInteger('autobnctest', 0) <= 0 then
            kell := True;
          TSite(sites.Items[i]).WCInteger('autobnctest', status);
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autobnctest');
          TSite(sites.Items[i]).RemoveAutoBnctest;
        end;
      end;
      irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d',
        [TSite(sites.Items[i]).Name, TSite(sites.Items[i])
        .RCInteger('autobnctest', 0)]);

      if kell then
        TSite(sites.Items[i]).AutoBnctest;
    end;
    //    Result := True;
  end
  else
  begin

    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
            [x.Strings[i]]);

          Continue;
        end;

        if (s.PermDown) then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set to PermDown.',
            [x.Strings[i]]);
          Continue;
        end;

        kell := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.RCInteger('autobnctest', 0) <= 0 then
              kell := True;
            s.WCInteger('autobnctest', status);
          end
          else
          begin
            s.DeleteKey('autobnctest');
            s.RemoveAutoBnctest;
          end;
        end;
        irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d',
          [sitename, s.RCInteger('autobnctest', 0)]);

        if kell then
          s.AutoBnctest;
      end;

    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcAutoRules(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
  i: integer;
  x: TStringList;
begin
  //  Result   := True;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites',
        'admin_sitename', 'SLFTP')) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;
      kell := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).RCInteger('autorules', 0) <= 0 then
            kell := True;
          TSite(sites.Items[i]).WCInteger('autorules', status);
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autorules');
          TSite(sites.Items[i]).RemoveAutoRules;
        end;
      end;
      irc_addtext(Netname, Channel, 'Autorules of %s is: %d',
        [TSite(sites.Items[i]).Name, TSite(sites.Items[i])
        .RCInteger('autorules', 0)]);

      if kell then
        TSite(sites.Items[i]).AutoRules;
    end;
    //    Result := True;
  end
  else
  begin

    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.',
            [x.Strings[i]]);
          Continue;
        end;
        if s.PermDown then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.',
            [sitename]);
          continue;
        end;

        kell := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.RCInteger('autorules', 0) <= 0 then
              kell := True;
            s.WCInteger('autorules', status);
          end
          else
          begin
            s.DeleteKey('autorules');
            s.RemoveAutoRules;
          end;
        end;
        irc_addtext(Netname, Channel, 'Autorules of %s is: %d',
          [sitename, s.RCInteger('autorules', 0)]);

        if kell then
          s.AutoRules;
      end;
    finally
      x.Free;
    end;
  end;
  Result := True;
end;

function IrcAutoDirlist(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: AnsiString;
  ss: AnsiString;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1 +
    length(IntToStr(status)) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;

  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;

  if ((status > -1) and (status <> 0)) then
  begin
    // hitelesitjuk a szekciokat
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        break;

      if s.sectiondir[ss] = '' then
      begin
        irc_addtext(Netname, Channel, 'Site %s has no %s section',
          [sitename, ss]);
        exit;
      end;
    end;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.RCInteger('autodirlist', 0) <= 0 then
        kell := True;
      s.WCInteger('autodirlist', status);
      s.WCString('autodirlistsections', sections);
    end
    else
    begin
      s.DeleteKey('autodirlist');
      s.DeleteKey('autodirlistsections');
      s.DeleteKey('nextautodirlist');
      s.RemoveAutoDirlist;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autodirlist of %s is: %d (%s)',
    [sitename, s.RCInteger('autodirlist', 0), s.RCString('autodirlistsections',
      '')]);

  if kell then
    s.AutoDirlist;

  Result := True;
end;

function IrcAutoIndex(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: AnsiString;
  ss: AnsiString;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1 +
    length(IntToStr(status)) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;
  if ((status > -1) and (status <> 0)) then
  begin
    // hitelesitjuk a szekciokat
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        break;

      if s.sectiondir[ss] = '' then
      begin
        irc_addtext(Netname, Channel, 'Site %s has no %s section',
          [sitename, ss]);
        exit;
      end;
    end;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.RCInteger('autoindex', 0) <= 0 then
        kell := True;
      s.WCInteger('autoindex', status);
      s.WCString('autoindexsections', sections);
    end
    else
    begin
      s.DeleteKey('autoindex');
      s.DeleteKey('autoindexsections');
      s.DeleteKey('nextautoindex');
      s.RemoveAutoIndex;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autoindex of %s is: %d (%s)',
    [sitename, s.RCInteger('autoindex', 0), s.RCString('autoindexsections',
      '')]);

  if kell then
    s.AutoIndex;

  Result := True;
end;

function IrcAutoNuke(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  status: integer;
  s: TSite;
  kell: boolean;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.RCInteger('autonuke', 0) <= 0 then
        kell := True;
      s.WCInteger('autonuke', status);
    end
    else
    begin
      s.DeleteKey('autonuke');
      s.DeleteKey('nextautonuke');
      s.RemoveAutoNuke;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autonuke of %s is: %d',
    [sitename, s.RCInteger('autonuke', 0)]);

  if kell then
    s.AutoNuke;

  Result := True;
end;

function IrcKbShow(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  section, rls: AnsiString;
  p: TPazo;
  i: integer;
  s, ss: AnsiString;
begin
  section := UpperCase(SubString(params, ' ', 1));
  rls := SubString(params, ' ', 2);
  i := kb_list.IndexOf(section + '-' + rls);
  if i <> -1 then
  begin
    p := TPazo(kb_list.Objects[i]);
    s := p.AsText;
    for i := 1 to 1000 do
    begin
      ss := SubString(s, #13#10, i);
      if ss = '' then
        break;
      irc_addtext(Netname, Channel, '%s', [ss]);
    end;
  end
  else
    irc_addtext(Netname, Channel, 'Cant find');

  Result := True;
end;

function IrcKbList(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  p: TPazo;
  i, db: integer;
  section: AnsiString;
  hits: integer;

begin
  section := SubString(params, ' ', 1);

  if kb_list.Count <= 0 then
  begin
    irc_addtext(Netname, Channel, 'No Infos in knowbase!');
    Result := True;
    exit;
  end;

  if section <> '' then
  begin
    i := kb_sections.IndexOf(section);
    if i <> -1 then
      section := kb_sections[i]
    else
      section := '';
  end;

  if section <> '' then
    hits := StrToIntDef(SubString(params, ' ', 2), 10)
  else
    hits := StrToIntDef(SubString(params, ' ', 1), 10);

  db := 0;
  for i := kb_list.Count - 1 downto 0 do
  begin
    if (db > hits) then
      break;

    p := TPazo(kb_list.Objects[i]);
    if p <> nil then
    begin
      if ((section = '') or (p.rls.section = section)) then
      begin
        irc_addtext(Netname, Channel,
          '#%d %s %s %s [QueueNumber: %d (R:%d D:%d M:%d)]',
          [p.pazo_id, p.rls.section, p.rls.rlsname, p.rls.ExtraInfo,
          p.queuenumber.ActValue, p.racetasks.ActValue, p.dirlisttasks.ActValue,
            p.mkdirtasks.ActValue]);
        Inc(db);
      end;
    end
    else
    begin
      irc_addtext(Netname, Channel,
        'Whops, Pazo is nil! anythingh screwed up!');
    end;
  end;

  Result := True;
end;

function IrcKbExtra(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  section, rls, extra: AnsiString;
begin
  section := UpperCase(SubString(params, ' ', 1));
  rls := SubString(params, ' ', 2);
  extra := mystrings.RightStr(params, length(section) + length(rls) + 2);
  kb_Add(Netname, Channel, '', section, extra, 'NEWDIR', rls, '', True);

  Result := True;
end;

function IrcKbAdd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, event, section, rls_section, rls: AnsiString;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  event := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rls := SubString(params, ' ', 4);

  // section correct
  section := ProcessDoReplace(section);
  rls_section := '';
  rls_section := KibontasSection(' ' + section + ' ', '');
  rls_section := PrecatcherSectionMapping(rls, rls_section);
  if ((rls_section = '') or (rls_section = 'TRASH')) then
  begin
    irc_addtext(Netname, Channel, 'No valid section found (%s)', [rls_section]);
    Result := False;
    exit;
  end;

  // add to kb
  kb_Add(Netname, Channel, sitename, rls_section, '', event, rls, '');
  if event = 'NEWDIR' then
    irc_addtext(Netname, Channel, format('<c2>-> [KB]</c> %s %s %s @ %s',
      [event, rls_section, rls, '<b>' + sitename + '</b>']));
  if event = 'PRE' then
    irc_addtext(Netname, Channel, format('<c3>-> [KB]</c> %s %s %s @ %s',
      [event, rls_section, rls, '<b>' + sitename + '</b>']));
  if event = 'ADDPRE' then
    irc_addtext(Netname, Channel, format('<c3>-> [KB]</c> %s %s %s @ %s',
      [event, rls_section, rls, '<b>' + sitename + '</b>']));
  if event = 'COMPLETE' then
    irc_addtext(Netname, Channel, format('<c7><- [KB]</c> %s %s %s @ %s',
      [event, rls_section, rls, '<b>' + sitename + '</b>']));
  if event = 'NUKE' then
    irc_addtext(Netname, Channel, format('<c4>-- [KB]</c> %s %s %s @ %s',
      [event, rls_section, rls, '<b>' + sitename + '</b>']));

  Result := True;
end;

function IrcSkipReload(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  try
    Result := SkiplistRehash;
  finally
    irc_addtext(Netname, Channel, 'Skiplist reloaded... (%d entries)', [SkiplistCount]);
  end;
end;

function IrcNoHelp(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  Count, i: integer;
begin
  Count := 0;
  for i := Low(irccommands) to High(irccommands) do
    if ((length(irccommands[i].cmd) > 0) and (irccommands[i].cmd[1] <> '-')) then
      if not FileExists(IncludeTrailingPathDelimiter('help') +
        irccommands[i].cmd + '.txt') then
      begin
        irc_addtext(Netname, Channel, 'Command %s has no help yet.',
          [irccommands[i].cmd]);
        Inc(Count);
      end;
  if Count = 0 then
    irc_addtext(Netname, Channel, 'No help is missing.');

  Result := True;
end;

function IrcFindUser(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  user: AnsiString;
  leech_up: AnsiString;
  leech_dn: AnsiString;
  ratio_up: AnsiString;
  ratio_dn: AnsiString;
begin
  user := SubString(params, ' ', 1);

  leech_up := '';
  leech_dn := '';
  ratio_up := '';
  ratio_dn := '';

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsLeecher(user) then
    begin
      if s.working = sstUp then
        leech_up := leech_up + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots])
      else
        leech_dn := leech_dn + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
    end
    else if s.IsTrader(user) then
    begin
      if s.working = sstUp then
        ratio_up := ratio_up + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots])
      else
        ratio_dn := ratio_dn + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
    end;

  end;

  if leech_up <> '' then
    irc_addtext(Netname, Channel, 'Leech up: %s', [leech_up]);
  if leech_dn <> '' then
    irc_addtext(Netname, Channel, 'Leech dn: %s', [leech_dn]);
  if ratio_up <> '' then
    irc_addtext(Netname, Channel, 'Ratio up: %s', [ratio_up]);
  if ratio_dn <> '' then
    irc_addtext(Netname, Channel, 'Ratio dn: %s', [ratio_dn]);

  Result := True;
end;

function IrcUsers(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss, sitename: AnsiString;
  s: TSite;
  x, y: TStringList;
  i, j: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  if sitename <> '' then
  begin
    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;
    ss := s.users;
  end
  else
  begin
    x := TStringList.Create;
    y := TStringList.Create;
    try
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites[i]);
        x.DelimitedText := s.leechers;
        for j := 0 to x.Count - 1 do
          if y.IndexOf(x[j]) = -1 then
            y.Add(x[j]);
        x.DelimitedText := s.traders;
        for j := 0 to x.Count - 1 do
          if y.IndexOf(x[j]) = -1 then
            y.Add(x[j]);
      end;
      //x.Free;

      y.Sort;

      ss := '';
      for i := 0 to y.Count - 1 do
      begin
        if ss <> '' then
          ss := ss + ' ';
        if (((i + 1) mod 10) = 0) then
        begin
          irc_addtext(Netname, Channel, ss);
          ss := '';
        end;

        ss := ss + y[i];
      end;

    finally
      x.Free;
      y.Free;
    end;

  end;
  if (trim(ss) <> '') then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcShowWindow(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := console_showwindow(params);
end;

function IrcShowWindows(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  Windows, s: AnsiString;
begin
  Windows := console_windows;
  while (True) do
  begin
    s := elsosor(Windows);
    if s = '' then
      break;

    irc_addtext(Netname, Channel, s);
  end;
  Result := True;
end;

function IrcDelWindow(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  if uppercase(params) = uppercase('ADMIN') then
  begin
    irc_addtext(Netname, Channel, 'Smartass.');
    Result := False;
    exit;
  end;
  console_delwindow(params);
  Result := True;
end;

function IrcIrcNames(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  th: TMyIrcThread;
  nn, ch: AnsiString;
  i: integer;
  s: AnsiString;
  x: TStringList;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  ch := SubString(params, ' ', 2);
  th := FindIrcnetwork(nn);
  if ((nil = FindIrcBlowfish(nn, ch, False)) or (th = nil)) then
  begin
    irc_addtext(Netname, Channel, 'Channel %s@%s not found.', [ch, nn]);
    exit;
  end;

  x := TStringList.Create;
  try
    x.DelimitedText := th.ChanNicks(ch);
    s := '';
    for i := 0 to x.Count - 1 do
    begin
      if (i + 1) mod 10 = 0 then
      begin
        irc_addtext(Netname, Channel, s);
        s := '';
      end;
      if s <> '' then
        s := s + ', ';
      s := s + x[i];
    end;
    if s <> '' then
      irc_addtext(Netname, Channel, s);
  finally
    x.Free;
  end;

  Result := True;
end;

function IrcRepaint(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  console_repaint;
  Result := True;
end;

function IrcNuke(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, t, h, multiplier: integer;
  datestamp: AnsiString;
  reason, sitename, rip, section, yyyy, yy, mm, dd: AnsiString;
  n: TNukeQueueItem;
begin
  Result := False;
  h := 0;
  sitename := UpperCase(SubString(params, ' ', 1));

  if nil <> FindSiteByName(Netname, sitename) then
  begin
    i := 3;
    Inc(h, length(sitename) + 1);
    section := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    i := 2;
    section := sitename;
    sitename := '';
  end;

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section %s not found', [section]);
    exit;
  end;

  Inc(h, length(section) + 1);

  dd := '';
  datestamp := SubString(params, ' ', i);
  if length(datestamp) = 10 then
  begin
    if ((datestamp[5] = '-') and (datestamp[8] = '-')) then
    begin
      yyyy := Copy(datestamp, 1, 4);
      if StrToIntDef(yyyy, 1990) > 1990 then
      begin
        yy := Copy(yyyy, 3, 2);
        mm := Copy(datestamp, 6, 2);
        t := StrToIntDef(mm, 0);
        if ((t >= 1) and (t <= 12)) then
        begin
          dd := Copy(datestamp, 9, 2);
          t := StrToIntDef(dd, 0);
          if (t >= 1) and (t <= 31) then
          begin
            Inc(h, 10 + 1);
            Inc(i);
          end
          else
            dd := '';
        end;
      end;
    end;
  end;

  if dd = '' then
  begin
    yyyy := IntToStr(YearOf(now));
    yy := Copy(yyyy, 3, 2);
    mm := format('%.2d', [MonthOf(now)]);
    dd := format('%.2d', [DayOf(now)]);
  end;

  rip := SubString(params, ' ', i);
  Inc(i);
  Inc(h, length(rip) + 1);
  multiplier := StrToIntDef(SubString(params, ' ', i), 0);
  Inc(h, length(SubString(params, ' ', i)) + 1);
  reason := Copy(params, h + 1, 1000);

  try
    for i := 0 to sites.Count - 1 do
    begin
      if TSite(sites[i]).IsAffil(GotGroupname(rip)) then
      begin
        irc_addtext(Netname, Channel,
          '<b>%s</b> is affil on %s we dont nuke affil!',
          [GotGroupname(rip), TSite(sites[i]).Name]);
        Continue;
      end;

      if ((sitename = '') or (TSite(sites[i]).Name = sitename)) then
      begin

        n := TNukeQueueItem.Create;
        n.site := TSite(sites[i]).Name;
        n.section := section;
        n.yyyy := yyyy;
        n.yy := yy;
        n.mm := mm;
        n.dd := dd;
        n.rip := rip;
        n.multiplier := multiplier;
        n.reason := reason;

        nukequeue.Add(n);

        if sitename <> '' then
          break;

      end;
    end;
    NukeSave;
  finally
  end;
  Result := True;
end;

function IrcUnnuke(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, t, h: integer;
  datestamp: AnsiString;
  reason, sitename, rip, section, yyyy, yy, mm, dd: AnsiString;
  n: TNukeQueueItem;
begin
  Result := False;
  h := 0;
  sitename := UpperCase(SubString(params, ' ', 1));
  if nil <> FindSiteByName(Netname, sitename) then
  begin
    i := 3;
    Inc(h, length(sitename) + 1);
    section := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    i := 2;
    section := sitename;
    sitename := '';
  end;

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section %s not found', [section]);
    exit;
  end;

  Inc(h, length(section) + 1);

  dd := '';
  datestamp := SubString(params, ' ', i);
  if length(datestamp) = 10 then
  begin
    if ((datestamp[5] = '-') and (datestamp[8] = '-')) then
    begin
      yyyy := Copy(datestamp, 1, 4);
      if StrToIntDef(yyyy, 1990) > 1990 then
      begin
        yy := Copy(yyyy, 3, 2);
        mm := Copy(datestamp, 6, 2);
        t := StrToIntDef(mm, 0);
        if ((t >= 1) and (t <= 12)) then
        begin
          dd := Copy(datestamp, 9, 2);
          t := StrToIntDef(dd, 0);
          if (t >= 1) and (t <= 31) then
          begin
            Inc(h, 10 + 1);
            Inc(i);
          end
          else
            dd := '';
        end;
      end;
    end;
  end;

  if dd = '' then
  begin
    yyyy := IntToStr(YearOf(now));
    yy := Copy(yyyy, 3, 2);
    mm := format('%.2d', [MonthOf(now)]);
    dd := format('%.2d', [DayOf(now)]);
  end;

  rip := SubString(params, ' ', i);
  Inc(h, length(rip) + 1);
  reason := Copy(params, h + 1, 1000);

  for i := 0 to sites.Count - 1 do
  begin
    if ((sitename = '') or (TSite(sites[i]).Name = sitename)) then
    begin
      n := TNukeQueueItem.Create;
      n.site := TSite(sites[i]).Name;
      n.section := section;
      n.yyyy := yyyy;
      n.yy := yy;
      n.mm := mm;
      n.dd := dd;
      n.rip := rip;
      n.multiplier := -1;
      n.reason := reason;

      nukequeue.Add(n);
    end;
  end;

  NukeSave;

  Result := True;
end;

function IrcOper(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, userid, pass: AnsiString;

begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  userid := SubString(params, ' ', 2);
  pass := SubString(params, ' ', 3);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'IRC Net %s not found!', [nn]);
    exit;
  end;

  if userid = '' then
  begin
    // query mode
    userid := sitesdat.ReadString('ircnet-' + nn, 'oper_userid', '');
    pass := sitesdat.ReadString('ircnet-' + nn, 'oper_password', '');
    if userid <> '' then
      irc_addtext(Netname, Channel, 'IRC oper settings for %s are: %s %s',
        [nn, userid, pass])
    else
      irc_addtext(Netname, Channel,
        'IRC oper settings for %s are turned off.', [nn]);
  end
  else if userid = '-' then
  begin
    // delete mode
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_userid');
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_password');
    irc_addtext(Netname, Channel,
      'IRC oper settings for %s are now deleted.', [nn]);
  end
  else
  begin
    // set mode
    sitesdat.WriteString('ircnet-' + nn, 'oper_userid', userid);
    sitesdat.WriteString('ircnet-' + nn, 'oper_password', pass);
  end;
  Result := True;
end;

function IrcNews(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  x: TEncStringList;
  i: integer;
begin
  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.news');
    for i := 0 to x.Count - 1 do
      irc_addtext(Netname, Channel, '[%d:] %s', [i + 1, x[i]]);
    Result := True;
  finally
    x.Free;
  end;
end;

function IrcNewsAdd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  x: TEncStringList;
  fn: AnsiString;
begin
  fn := ExtractFilePath(ParamStr(0)) + 'slftp.news';
  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(fn);

    x.Insert(0, FormatDateTime('yyyy-mm-dd', now) + ' ' + params);

    x.SaveToFile(fn);
    Result := True;
  finally
    x.Free;
  end;
end;

function IrcNewsDel(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  x: TEncStringList;
  i: integer;
  fn: AnsiString;
begin
  Result := False;

  if params = '*' then
  begin
    fn := ExtractFilePath(ParamStr(0)) + 'slftp.news';
    x := TEncStringList.Create(passphrase);
    try
      x.LoadFromFile(fn);
      x.Clear;
      x.SaveToFile(fn);
    finally
      x.Free;
    end;
    Result := True;
  end
  else
  begin
    i := StrToIntDef(params, 0);
    if i < 1 then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end;

    fn := ExtractFilePath(ParamStr(0)) + 'slftp.news';
    x := TEncStringList.Create(passphrase);
    try
      x.LoadFromFile(fn);
      x.BeginUpdate;
      if x.Count >= i then
        x.Delete(i - 1);
      x.EndUpdate;
      x.SaveToFile(fn);
    finally
      x.Free;
    end;
    Result := True;
  end;
end;

function IrcSpeedStats(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, section, rip: AnsiString;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rip := SubString(params, ' ', 3);

  if ((section = '') and (rip = '')) then
    SpeedStatsShowStats(Netname, Channel, sitename);

  if ((section <> '') and (rip = '')) then
    SpeedStatsShowStats(Netname, Channel, sitename, section);

  if ((section <> '') and (rip <> '')) then
    SpeedStatsShowStats(Netname, Channel, sitename, section, rip);

  Result := True;
end;

function IrcSpeedRecalc(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  // SpeedStatsRecalc(netname, channel);
  SpeedStatsRecalc('CONSOLE', 'ADMIN');
  Result := True;
end;

function IrcRankRecalc(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  RanksRecalc(Netname, Channel);
  Result := True;
end;

function IrcSpeedTestLocal(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  s: TSite;
  dir: AnsiString;
  t: TUploadSpeedtestFileTask;
  tn: TTaskNotify;
begin
  Result := False;
  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if nil = s then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  dir := s.sectiondir['SPEEDTEST'];
  if dir = '' then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.',
      [sitename]);
    exit;
  end;
  tn := AddNotify;
  t := TUploadSpeedtestFileTask.Create(Netname, Channel, sitename);
  tn.tasks.Add(t);
  AddTask(t);

  tn.event.WaitFor($FFFFFFFF);

  RemoveTN(tn);

  Result := True;
end;

function IrcSpeedTestCleanup(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  ss: AnsiString;
  s: TSite;
  tn: TTaskNotify;
  t: TDelSpeedtestFileTask;
  i: integer;
begin
  Result := False;
  params := trim(UpperCase(params));

  if params = '' then
  begin
    tn := AddNotify;
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if '' <> s.sectiondir['SPEEDTEST'] then
      begin
        t := TDelSpeedtestFileTask.Create(Netname, Channel, s.Name);
        tn.tasks.Add(t);
        AddTask(t);
      end;
    end;
  end
  else
  begin
    // specified sites only
    tn := AddNotify;

    while (True) do
    begin
      ss := Fetch(params, ' ');
      if ss = '' then
        break;

      s := FindSiteByName(Netname, ss);
      if s = nil then
      begin
        RemoveTN(tn);
        irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
        exit;
      end;
      if '' = s.sectiondir['SPEEDTEST'] then
      begin
        RemoveTN(tn);
        irc_addtext(Netname, Channel,
          'Site %s has no SPEEDTEST section.', [ss]);
        exit;
      end;
      t := TDelSpeedtestFileTask.Create(Netname, Channel, ss);
      tn.tasks.Add(t);
      AddTask(t);
    end;
  end;

  if tn.tasks.Count = 0 then
  begin
    RemoveTN(tn);
    irc_addtext(Netname, Channel,
      'ERROR: No sites to add del speedtest file tasks to.');
    exit;
  end;

  irc_addtext(Netname, Channel, '%d del speedtest file tasks added.',
    [tn.tasks.Count]);
  tn.event.WaitFor($FFFFFFFF);

  RemoveTN(tn);
  Result := True;
end;

procedure PickupSpeedtestFile(d: TDirList; var fsfilename: AnsiString;
  var fsfilesize: Int64);
var
  de: TDirListEntry;
  i: integer;
begin
  fsfilename := '';
  fsfilesize := 0;
  for i := 0 to d.entries.Count - 1 do
  begin
    de := TDirListEntry(d.entries[i]);
    if ((de.filesize > fsfilesize) and (de.filesize >=
      config.ReadInteger('speedtest', 'min_filesize', 15) * 1024 * 1024) and
      (de.filesize <= config.ReadInteger('speedtest', 'max_filesize', 120) *
      1024 * 1024)) then
    begin
      fsfilename := de.filename;
      fsfilesize := de.filesize;
    end;
  end;
end;

function IrcSpeedTestIn(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  oparams, ss: AnsiString;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  ds: TDirlistTask;
  fssitename: AnsiString;
  d1, d2: double;
  added: integer;
  d: TDirList;
  fsfilename: AnsiString;
  fsfilesize: Int64;
  fsfilesizemb: double;
  speedtestsites: TStringList;
  speedtestfilenames: TStringList;
  speedtestfilesizes: TIntList;
begin
  Result := False;
  fssitename := '';
  // eloszor validaljuk az osszes parametert... we first validate all the parameters ...
  params := trim(UpperCase(params));
  oparams := params;
  while (True) do
  begin
    ss := Fetch(params, ' ');
    if ss = '' then
      break;

    if fssitename = '' then
      fssitename := ss;

    s := FindSiteByName(Netname, ss);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
      exit;
    end;

    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [s.Name]);
      Exit;
    end;

    if s.working = sstDown then
    begin
      irc_addtext(Netname, Channel, 'Site %s is down.', [ss]);
      exit;
    end;

    if '' = s.sectiondir['SPEEDTEST'] then
    begin
      irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.', [ss]);
      exit;
    end;
  end;

  // most megnezzuk, van e mar speedtest file a forras siteokon
  // Now look, this is already the source file Speedtest siteokon
  added := 0;
  tn := AddNotify;
  params := oparams;

  while (True) do
  begin
    ss := Fetch(params, ' ');
    if ss = '' then
      break;

    if fssitename = ss then
      Continue;

    s := FindSiteByName(Netname, ss);
    ds := TDirlistTask.Create(Netname, Channel, s.Name, s.sectiondir['SPEEDTEST'], True);
    tn.tasks.Add(ds);
    AddTask(ds);
    Inc(added);
  end;

  if added = 0 then
  begin
    irc_addtext(Netname, Channel, 'wtf?');
    exit;
  end;

  tn.event.WaitFor($FFFFFFFF);

  speedtestsites := TStringList.Create;
  speedtestfilenames := TStringList.Create;
  speedtestfilesizes := TIntList.Create;
  try
    if tn.responses.Count <> added then
    begin
      RemoveTN(tn);
      irc_addtext(Netname, Channel, 'ERROR: Incorrect number of responses?!');
    end;

    for i := 0 to tn.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn.responses[i]);
      d := TDirList.Create(sr.sitename, nil, nil, sr.response, True);
      if d <> nil then
        d.SetFullPath(s.sectiondir['SPEEDTEST']);
      try
        PickupSpeedtestFile(d, fsfilename, fsfilesize);
      finally
        d.Free;
      end;

      if ((fsfilename = '') or (fsfilesize = 0)) then
      begin
        RemoveTN(tn);

        irc_addtext(Netname, Channel,
          'Site %s has no suitable file for speedtesting, check slftp.ini', [ss]);
        exit;
      end;

      speedtestsites.Add(sr.sitename);
      speedtestfilenames.Add(fsfilename);
      speedtestfilesizes.Add(fsfilesize);
    end;

    RemoveTN(tn);

    // es most kezdodik a moka, megcsinaljuk a pazot meg a szarjait
    // And now the fun begins, you do make a shit pazot

    firstsite := nil;
    params := oparams;

    p := PazoAdd(nil);

    kb_list.AddObject('TRANSFER-speedtest-' + IntToStr(p.pazo_id), p);
    while (True) do
    begin
      ss := Fetch(params, ' ');
      if ss = '' then
        break;

      s := FindSiteByName(Netname, ss);
      ps := p.AddSite(ss, s.sectiondir['SPEEDTEST']);
      if p.sites.Count > 1 then
        ps.AddDestination(firstsite, 1)
      else
        firstsite := ps;
    end;

    if firstsite = nil then
    begin
      irc_addtext(Netname, Channel, 'wtf?');
      exit;
    end;

    for i := 1 to p.sites.Count - 1 do
    begin

      ps := TPazoSite(p.sites[i]);

      if not IrcSpeedTestCleanup(Netname, Channel, firstsite.Name) then
      begin
        irc_addtext(Netname, Channel,
          'ERROR: cant remove speedtest file on site %s', [firstsite.Name]);
        exit;
      end;

      j := speedtestsites.IndexOf(ps.Name);
      if j = -1 then
        Continue; // wtf?
      fsfilename := speedtestfilenames[j];
      fsfilesize := speedtestfilesizes[j];
      fsfilesizemb := fsfilesize / 1024 / 1024;

      irc_addtext(Netname, Channel, 'Speedtesting %s -> %s (using %s / %d bytes)', [ps.Name,
        firstsite.Name, fsfilename, fsfilesize]);

      tn := AddNotify;

      t := TPazoRaceTask.Create(Netname, Channel, ps.Name, firstsite.Name, p, '', fsfilename,
        fsfilesize, 1);
      t.storfilename := speedtestfilename;
      tn.tasks.Add(t);
      AddTask(t);

      tn.event.WaitFor($FFFFFFFF);

      if tn.responses.Count = 1 then
      begin
        sr := TSiteResponse(tn.responses[0]);
        j := StrToIntDef(sr.response, 0);
        if j <> 0 then
        begin
          d2 := j;
          d2 := d2 / 1000;
          d1 := j;
          d1 := fsfilesize / d1;
          j := SpeedStatsScale(d1);
          if ((j >= 1) and (j <= 9)) then
            irc_addtext(Netname, Channel,
              '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs) : %srouteset %s %s %d',
              [ps.Name, firstsite.Name, d1, fsfilesizemb, d2, irccmdprefix,
              ps.Name, firstsite.Name, j])
          else
            irc_addtext(Netname, Channel,
              '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs)',
              [ps.Name, firstsite.Name, d1, fsfilesizemb, d2]);
        end
        else
          irc_addtext(Netname, Channel, '%s -> %s failed.',
            [ps.Name, firstsite.Name]);
      end
      else
        irc_addtext(Netname, Channel, '%s -> %s failed, site responses is:%d',
          [ps.Name, firstsite.Name, tn.responses.Count]);
      RemoveTN(tn);
    end;

  finally
    speedtestsites.Free;
    speedtestfilenames.Free;
    speedtestfilesizes.Free;
  end;

  Result := True;
end;

function IrcSpeedTestOut(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  oparams, ss: AnsiString;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  fs: TFileSizeTask;
  fssitename, fsfilename: AnsiString;
  fsfilesize: Int64;
  fsfilesizemb: double;
  todel: AnsiString;
  d1, d2: double;
  d: TDirList;
begin
  Result := False;
  tn := nil;

  // First, validate all the parameters ...
  params := trim(UpperCase(params));
  fssitename := '';
  fsfilename := '';
  oparams := params;
  while (True) do
  begin
    ss := Fetch(params, ' ');
    if ss = '' then
      break;

    if fssitename = '' then
      fssitename := ss;

    s := FindSiteByName(Netname, ss);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
      exit;
    end;

    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [s.Name]);
      Exit;
    end;

    if s.working = sstDown then
    begin
      irc_addtext(Netname, Channel, 'Site %s is down.', [ss]);
      exit;
    end;

    if '' = s.sectiondir['SPEEDTEST'] then
    begin
      irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.', [ss]);
      exit;
    end;
    if fsfilename = '' then
      fsfilename := s.sectiondir['SPEEDTEST'];
  end;

  // Getting files from SPEEDTEST directory
  d := DirlistB(Netname, Channel, fssitename, fsfilename, True);
  try
    if d = nil then
    begin
      irc_addtext(Netname, Channel, 'Can''t dirlist %s in %s.', [fsfilename, fssitename]);
      exit;
    end;
    // now we pick a file
    PickupSpeedtestFile(d, fsfilename, fsfilesize);
  finally
    d.Free;
  end;

  if ((fsfilesize = 0) or (fsfilename = '')) then
  begin
    irc_addtext(Netname, Channel,'No suitable file found on site %s for speedtesting, check slftp.ini. Speedtest aborted.', [ss]);
    exit;
  end;

  fsfilesizemb := fsfilesize / 1024 / 1024;
  irc_addtext(Netname, Channel, 'Testing outgoing speed with file %s (%d bytes)', [fsfilename, fsfilesize]);

  // Checking if the file already exists on destinations
  try
    tn := AddNotify;
  except
    on e: Exception do
    begin
      irc_addtext(Netname, Channel, '<c4>[EXCEPTION]</c> in SpeedTestOut-AddNotify: %s', [e.Message]);
      exit;
    end;
  end;

  params := oparams;

  while (True) do
  begin
    ss := Fetch(params, ' ');
    if ss = '' then
      break;

    if fssitename = ss then
      Continue;

    s := FindSiteByName(Netname, ss);
    fs := TFileSizeTask.Create(Netname, Channel, s.Name,
      MyIncludeTrailingSlash(s.sectiondir['SPEEDTEST']) + speedtestfilename);
    tn.tasks.Add(fs);

    AddTask(fs);
  end;

  if tn.tasks.Count = 0 then
  begin
    irc_addtext(Netname, Channel, 'Failed to check if speedtest file %s already exists on destination sites. Speedtest aborted.', [speedtestfilename]);
    exit;
  end;

  tn.event.WaitFor($FFFFFFFF);

  // deleting existing destination files
  todel := '';
  for i := 0 to tn.responses.Count - 1 do
  begin
    sr := TSiteResponse(tn.responses[i]);
    if StrToIntDef(sr.response, -1) > 0 then
      todel := todel + sr.sitename + ' ';
  end;
  RemoveTN(tn);
  todel := trim(todel);
  if todel <> '' then
  begin
    irc_addtext(Netname, Channel, 'Removing existing speedtest files from %s', [todel]);
    if not IrcSpeedTestCleanup(Netname, Channel, todel) then
    begin
      irc_addtext(Netname, Channel, 'Removing existing speedtest files failed. Speedtest aborted.');
      exit;
    end;
  end;

  // Creating the pazo entries for the speedtest
  firstsite := nil;
  params := oparams;

  p := PazoAdd(nil);

  kb_list.AddObject('TRANSFER-speedtest-' + IntToStr(p.pazo_id), p);

  while (True) do
  begin
    ss := Fetch(params, ' ');
    if ss = '' then
      break;

    s := FindSiteByName(Netname, ss);
    ps := p.AddSite(ss, s.sectiondir['SPEEDTEST']);
    if p.sites.Count > 1 then
      TPazoSite(p.sites[0]).AddDestination(ps, 1)
    else
      firstsite := ps;
  end;

  if firstsite = nil then
  begin
    irc_addtext(Netname, Channel, 'wtf?');
    exit;
  end;

  for i := 1 to p.sites.Count - 1 do
  begin
    ps := TPazoSite(p.sites[i]);
    irc_addtext(Netname, Channel, 'Speedtesting %s -> %s  ->> %s', [firstsite.Name, ps.Name, ps.maindir]);
    tn := AddNotify;
    t := TPazoRaceTask.Create(Netname, Channel, firstsite.Name, ps.Name, p, '', fsfilename, fsfilesize, 1);
    t.storfilename := speedtestfilename;

    tn.tasks.Add(t);

    AddTask(t);

    tn.event.WaitFor($FFFFFFFF);

    if tn.responses.Count = 1 then
    begin
      sr := TSiteResponse(tn.responses[0]);
      j := StrToIntDef(sr.response, 0);

      if j <> 0 then
      begin
        d2 := j;
        d2 := d2 / 1000;
        d1 := j;
        d1 := fsfilesize / d1;
        j := SpeedStatsScale(d1);
        if ((j >= 1) and (j <= 9)) then
          irc_addtext(Netname, Channel,
            '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs) : %srouteset %s %s %d',
            [firstsite.Name, ps.Name, d1, fsfilesizemb, d2, irccmdprefix,
            firstsite.Name, ps.Name, j])
        else
          irc_addtext(Netname, Channel,
            '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs)',
            [firstsite.Name, ps.Name, d1, fsfilesizemb, d2]);
      end
      else
        irc_addtext(Netname, Channel, '%s -> %s failed.', [firstsite.Name, ps.Name]);
    end
    else
      irc_addtext(Netname, Channel, '%s -> %s failed.', [firstsite.Name, ps.Name]);

    RemoveTN(tn);
  end;

  Result := True;
end;

function IrcIndexQuery(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s, ss: AnsiString;
begin
  s := indexerQueryPartially(params);

  if s <> '' then
  begin
    while (True) do
    begin
      ss := elsosor(s);
      if ss = '' then
        break;

      irc_addtext(Netname, Channel, ss);
    end;
  end
  else
    irc_addtext(Netname, Channel, 'Cant find rip %s indexed.', [params]);
  Result := True;
end;

function IrcIndexDropSection(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  params := UpperCase(params);

  indexerRemoveSiteSection(SubString(params, ' ', 1),
    SubString(params, ' ', 2));

  Result := True;
end;

function IrcIndexStat(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s, ss: AnsiString;
begin
  s := indexerStat;

  while (True) do
  begin
    ss := elsosor(s);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;
  Result := True;
end;

function IrcStatRaces(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, period: AnsiString;
  detailed: Boolean;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  period := UpperCase(SubString(params, ' ', 2));
  detailed := StrToBoolDef(SubString(params, ' ', 3), True);

  if (sitename <> '*') then
  begin
    if FindSiteByName(Netname, sitename) = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;
  end;

  if ((period <> 'YEAR') and (period <> 'MONTH')) then
  begin
    period := 'DAY';
  end;

  StatRaces(Netname, Channel, sitename, period, detailed);

  Result := True;
end;

function IrcStatSites(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  q, ss: AnsiString;
  sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(params, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatSitesByGroup(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sss, q, ss: AnsiString;
  groupname, groupfilter, sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  d := 0;
  sectionfilter := '';

  groupname := UpperCase(SubString(params, ' ', 1));
  groupfilter := ' AND groupname like "' + groupname + '" ' + #13#10;

  sectionname := UpperCase(SubString(params, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 3), 0);
      sectionfilter := ' AND section like "' + sectionname + '" ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME("now",-' + IntToStr(d) + ' day") ' + #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  sss := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(sss);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatSitesByUser(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  q, ss: AnsiString;
  username, userfilter, sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  d := 0;
  sectionfilter := '';

  username := UpperCase(SubString(params, ' ', 1));
  userfilter := ' AND username like ''' + username + ''' ' + #13#10;

  sectionname := UpperCase(SubString(params, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + userfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'User: %s', [username]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatGroups(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  q, ss: AnsiString;
  sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(params, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT groupname, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY groupname ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of groups:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatGroupsBySite(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  q, ss: AnsiString;
  sitename, sitefilter, sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  sitename := UpperCase(SubString(params, ' ', 1));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := SubString(params, ' ', 2);
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT groupname, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY groupname ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of groups:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsers(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  q, ss: AnsiString;
  sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(params, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersBySite(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  q, ss: AnsiString;
  sitename, sitefilter, sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  sitename := UpperCase(SubString(params, ' ', 1));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(params, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersByGroup(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  q, ss: AnsiString;
  groupname, groupfilter, sectionname, sectionfilter: AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  groupname := UpperCase(SubString(params, ' ', 1));
  groupfilter := ' AND groupname like ''' + groupname + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(params, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersByGroupBySite(const Netname, Channel: AnsiString;
  params: AnsiString): boolean;
var
  q, ss: AnsiString;
  groupname, groupfilter, sitename, sitefilter, sectionname, sectionfilter:
    AnsiString;
  d: integer;
begin
  // stupid and safe anti sql injection
  params := Csere(params, '"', '');
  params := Csere(params, '''', '');

  groupname := UpperCase(SubString(params, ' ', 1));
  groupfilter := ' AND groupname like ''' + groupname + ''' ' + #13#10;
  sitename := UpperCase(SubString(params, ' ', 2));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(params, ' ', 3));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(params, ' ', 4), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' +
    #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' +
    #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s',
    [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

procedure DisplayDelay(Netname, Channel, s1, s2, s3: AnsiString);
var
  minv, maxv: integer;
begin
  minv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min',
    0);
  maxv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max',
    0);
  if (minv = 0) or (maxv = 0) then
    irc_addtext(Netname, Channel, 'Delaying ' + s2 +
      ' feature is disabled for site %s, section: %s', [s1, s3])
  else
    irc_addtext(Netname, Channel, 'Delay ' + s2 +
      ' on site %s, section %s: min delay %d, max delay: %d',
      [s1, s3, minv, maxv]);

end;

procedure DisplayAllDelay(Netname, Channel, s1, s2: AnsiString);
var
  x: TStringList;
  minv, maxv: integer;
  i: integer;
  s, s3: AnsiString;
begin
  x := TStringList.Create;
  try
    sitesdat.ReadSection('site-' + s1, x);
    for i := 0 to x.Count - 1 do
    begin
      if ((1 = Pos('delay' + s2, x[i])) and (0 <> Pos('-min', x[i]))) then
      begin
        s := x[i];
        Fetch(s, '-');
        s3 := Fetch(s, '-');
        minv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min', 0);
        maxv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max', 0);

        if (minv = 0) or (maxv = 0) then
          irc_addtext(Netname, Channel, 'Delaying ' + s2 +
            ' feature is disabled for site %s, section: %s', [s1, s3])
        else
          irc_addtext(Netname, Channel, 'Delay ' + s2 +
            ' on site %s, section %s: min delay %d, max delay: %d', [s1, s3, minv, maxv]);

      end;
    end;
  finally
    x.Free;
  end;
end;

procedure DeleteDelay(Netname, Channel, s1, s2, s3: AnsiString);
begin
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-min');
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-max');
  irc_addtext(Netname, Channel, s3 + ' ' + s2 +
    ' delay is deleted on site ' + s1 + '.');
end;

procedure SpecifyDelay(Netname, Channel, s1, s2, s3: AnsiString; minv, maxv:
  integer);
begin
  if minv < 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if maxv < minv then
  begin
    irc_addtext(Netname, Channel, 'Max is smaller than min.');
    exit;
  end;

  sitesdat.WriteInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min', minv);
  sitesdat.WriteInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max', maxv);
end;

function IrcDelayLeech(const Netname, Channel: AnsiString; params: AnsiString): boolean;
const
  tipus = 'leech';
var
  sitename, section, s: AnsiString;
  minv, maxv: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
    if nil = FindSiteByName(Netname, sitename) then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites',
          'admin_sitename', 'SLFTP')) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
          'global');
      end;
    end
    else
      DeleteDelay(Netname, Channel, sitename, tipus, 'global');
  end
  else if section = '' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites',
          'admin_sitename', 'SLFTP')) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        DisplayAllDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus);
      end;
    end
    else
      DisplayAllDelay(Netname, Channel, sitename, tipus);
  end
  else
  begin
    minv := StrToIntDef(section, -1);
    if minv > 0 then
    begin
      // specify global
      section := 'global';
      maxv := StrToIntDef(SubString(params, ' ', 3), -1);
      if sitename = '*' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          if (TSite(sites.Items[i]).Name = config.ReadString('sites',
            'admin_sitename', 'SLFTP')) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;
          SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
            section, minv, maxv);
        end;
      end
      else
        SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
    end
    else
    begin
      // section specified.
      s := SubString(params, ' ', 3);
      if s = '-' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = config.ReadString('sites',
              'admin_sitename', 'SLFTP')) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          DeleteDelay(Netname, Channel, sitename, tipus, section);
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = config.ReadString('sites',
              'admin_sitename', 'SLFTP')) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            DisplayDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          DisplayDelay(Netname, Channel, sitename, tipus, section);
      end
      else
      begin
        // set
        minv := StrToIntDef(s, -1);
        maxv := StrToIntDef(SubString(params, ' ', 4), -1);
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = config.ReadString('sites',
              'admin_sitename', 'SLFTP')) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
              section, minv, maxv);
          end;
        end
        else
          SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
      end;
    end;
  end;
  Result := True;
end;

function IrcDelayUpload(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
const
  tipus = 'upload';
var
  sitename, section, s: AnsiString;
  minv, maxv: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
    if nil = FindSiteByName(Netname, sitename) then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites',
          'admin_sitename', 'SLFTP')) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
          'global');
      end;
    end
    else
      DeleteDelay(Netname, Channel, sitename, tipus, 'global');
  end
  else if section = '' then
  begin

    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites',
          'admin_sitename', 'SLFTP')) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        DisplayAllDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus);
      end;
    end
    else
      DisplayAllDelay(Netname, Channel, sitename, tipus);
  end
  else
  begin
    minv := StrToIntDef(section, -1);
    if minv > 0 then
    begin
      // specify global
      section := 'global';
      maxv := StrToIntDef(SubString(params, ' ', 3), -1);
      if sitename = '*' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          if (TSite(sites.Items[i]).Name = config.ReadString('sites',
            'admin_sitename', 'SLFTP')) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;
          SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
            section, minv, maxv);
        end;
      end
      else
        SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
    end
    else
    begin
      // section specified.
      s := SubString(params, ' ', 3);
      if s = '-' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = config.ReadString('sites',
              'admin_sitename', 'SLFTP')) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          DeleteDelay(Netname, Channel, sitename, tipus, section);
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = config.ReadString('sites',
              'admin_sitename', 'SLFTP')) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            DisplayDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          DisplayDelay(Netname, Channel, sitename, tipus, section);
      end
      else
      begin
        // set
        minv := StrToIntDef(s, -1);
        maxv := StrToIntDef(SubString(params, ' ', 4), -1);

        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
            SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
              section, minv, maxv);
        end
        else
          SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
      end;

    end;

  end;

  Result := True;
end;

function IrcTweak(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ss1, ss2, s1, s2, s3: AnsiString;
  x: TRegExpr;
begin
  s1 := SubString(params, ' ', 1);
  s2 := lowercase(SubString(params, ' ', 2));
  s3 := mystrings.RightStr(params, length(s1) + 1 + length(s2) + 1);
  ss1 := SubString(s1, '-', 1);
  ss2 := SubString(s1, '-', 2);

  x := TRegExpr.Create;
  try
    x.ModifierI := True;
    x.Expression := '(site\-|ircnet\-|mysql\-)(.*?)$';
    if not x.Exec(s1) then
    begin
      irc_addtext(Netname, Channel,
        '<c4><b>Syntax error</b>. Need to start with (site\-|ircnet\-|mysql\-).</c>');
      exit;
    end
    else
      s1 := lowercase(ss1) + '-' + uppercase(ss2);

  finally
    x.Free;
  end;

  if s3 = '' then
    irc_addtext(Netname, Channel, 'Value is: ' + sitesdat.ReadString(s1, s2, ''))
  else
  begin
    try
      sitesdat.WriteString(s1, s2, s3);
    except
      on E: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] IrcTweak : %s', [e.Message]);
        Exit;
      end;
    end;

    irc_addtext(netname, channel, 'New value is: ' + sitesdat.ReadString(s1, s2, ''));
  end;

  Result := True;
end;

/// dOH mODz

function Irctestoffset(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  voctime, vctime, vnow, cnow: int64;
  response, url, ss, s: AnsiString;
  x: TRegExpr;
begin
  //  Result := False;

  voctime := -99;
  vctime := -99;
  vnow := -1;
  url := '';
  x := TRegExpr.Create;
  try
    x.Expression := '^(\S+) (\S+) (\S+) (\S+) (\S+)$';
    irc_addtext(Netname, Channel, 'Offset TEST');
    // Irc_AddText(netname,channel,'Trigger: %s : Value: %d ',[offset.Trigger,offset.OffSetValue]);

    if params <> '' then
    begin
      response := slUrlGet(format(url, [params]));
      if x.Exec(response) then
      begin
        voctime := strtoint64(x.Match[2]);
        // vctime:=offset.NewCtime(voctime);
      end;
      if ((voctime <> -99) and (vctime <> -99)) then
      begin
        s := format('[%d] %s', [voctime, DateTimeAsString(
            UnixToDateTime(voctime))]) + #13#10;
        ss := format('[%d] %s', [vctime, DateTimeAsString(
            UnixToDateTime(vctime))]) + #13#10;
        s := s + '' + DatetimetoStr(UnixToDateTime(voctime));
        ss := ss + '' + DatetimetoStr(UnixToDateTime(vctime));
      end
      else
      begin
        s := 'No Pretime Found!';
        ss := 'No Pretime Found!';
      end; // if ((voctime <> -99) and (vctime <> -99)) then begin
      irc_addtext(Netname, Channel, 'Database Time:');
      irc_addtext(Netname, Channel, s);
      irc_addtext(Netname, Channel, 'Fixed Time:');
      irc_addtext(Netname, Channel, ss);

    end
    else
    begin // if params <> '' then begin
      cnow := DateTimeToUnix(now);

      irc_addtext(Netname, Channel, 'Offset TEST');
      // (%s%sh)',[vtrigger,offset.OffSet]);
      // Irc_AddText(netname,channel,'Trigger: %s : Value: %d ',[vtrigger,offset.OffSetValue]);
      irc_addtext(Netname, Channel, 'Realtime: %d (%s)',
        [cnow, DatetimetoStr(UnixToDateTime(cnow))]);
      irc_addtext(Netname, Channel, 'Fixxedtime: %d (%s)',
        [vnow, DatetimetoStr(UnixToDateTime(vnow))]);
    end;

  finally
    x.Free;
  end;

  Result := True;
end;

{ IrcKillAll }

function IrcKillAll(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  rx: TRegExpr;
begin
  Result := False;
  rx := TRegExpr.Create;
  try
    rx.ModifierI := False;
    rx.Expression := 'AUTOLOGIN';
    // Irc_AddText(Netname, Channel, 'Try to kill %s tasks plz w8...', [IntToStr(tasks.Count)]);
    for i := 0 to tasks.Count - 1 do
      if not rx.Exec(TPazoTask(tasks[i]).Fullname) then
      begin
        irc_addtext(Netname, Channel, 'Removing Task -> %s', [TPazoTask(tasks[i]).Fullname]);
        try
          tasks.Remove(TPazoTask(tasks[i]));
        except
          on E: Exception do
            Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcKillAll.tasks.Remove: %s',
              [e.Message]);
        end;

      end
      else
        Continue;

  finally
    rx.Free;
  end;
  Result := True;
end;

function IrcSetMYIrcNick(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  ircnick, sname: AnsiString;
  s: TSite;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  ircnick := SubString(params, ' ', 2);

  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Site %s not found!', [sname]);
    exit;
  end;
  s.ircnick := ircnick;

  Result := True;
end;

function IrcInviteMyIRCNICK(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  x: TStringList;
  i: Integer;
begin
  Result := False;

  if params = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if s = nil then
        Continue;
      if Uppercase(s.Name) = getAdminSiteName then
        Continue;
      if s.PermDown then
        Continue;

      if s.IRCNick = '' then
      begin
        Irc_AddText(Netname, Channel, 'Please set your irc nick first! Maybe %sircnick %s %s', [irccmdprefix, s.Name, s.UserName]);
      end
      else
      begin
        Irc_AddText(Netname, Channel, 'Invitation sent inquiry to %s with irc nick %s', [s.Name, s.IRCNick]);
        RawB(Netname, Channel, s.Name, '/', Format('SITE INVITE %s', [s.IRCNick]));
      end
    end;
    Result := True;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.Delimiter := ' ';
      x.DelimitedText := UpperCase(params);
      if x.Count > 0 then
      begin

        for i := 0 to x.Count - 1 do
        begin
          s := FindSiteByName(Netname, x[i]);
          if s = nil then
          begin
            irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Site %s not found!', [x[i]]);
            Continue;
          end
          else
          begin
            if Uppercase(s.Name) = getAdminSiteName then
              Continue;
            if s.PermDown then
              Continue;

            if s.IRCNick = '' then
            begin
              Irc_AddText(Netname, Channel, 'Please set your irc nick first! Maybe %sircnick %s %s', [irccmdprefix, s.Name, s.UserName]);
            end
            else
            begin
              Irc_AddText(Netname, Channel, 'Invitation sent inquiry to %s with irc nick %s', [s.Name, s.IRCNick]);
              RawB(Netname, Channel, s.Name, '/', Format('SITE INVITE %s', [s.IRCNick]));
            end;
          end;

        end;
        irc_addtext(Netname, Channel, 'All Done...');
      end;
    finally
      x.Free;
    end;
    Result := True;
  end;
end;

function IrcNetNoSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nname, Value: AnsiString;
  status: boolean;
begin
  nname := SubString(params, ' ', 1);
  Value := SubString(params, ' ', 2);
  status := boolean(StrToInt(Value));
  sitesdat.WriteBool('ircnet-' + nname, 'nosocks5', status);
  Result := True;
end;

function IrcTweakSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  fname, ftrigger, fvalue: AnsiString;
  s5: TmSLSocks5;
begin
  Result := False;

  fname := SubString(params, ' ', 1);
  ftrigger := AnsiLowerCase(SubString(params, ' ', 2));
  fvalue := SubString(params, ' ', 3);

  s5 := FindProxyByName(fname);
  if s5 = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find Proxy with name %s!', [fname]);
    exit;
  end;

  try
    if ftrigger = 'host' then
      s5.host := fvalue;
    if ftrigger = 'port' then
      s5.port := StrToInt(fvalue);
    if ftrigger = 'user' then
      s5.username := fvalue;
    if ftrigger = 'password' then
      s5.password := fvalue;
  except
    on e: Exception do
    begin
      Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcTweakSocks5 saving value %s', [e.Message]);
      exit;
    end;
  end;

  Result := True;
end;

function IrcAddSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  fhostport, fhost, fuser, fpass, fname: AnsiString;
  fport, fstatus: integer;
begin
  //  Result := False;
  fname := UpperCase(SubString(params, ' ', 1));
  if FindProxyByName(fname) <> nil then
  begin
    irc_addtext(Netname, Channel, 'Proxy with name %s already exists!',
      [fname]);
    Result := True;
    exit;
  end;
  fhostport := SubString(params, ' ', 2);
  fhost := SubString(fhostport, ':', 1);
  fport := StrToIntDef(SubString(fhostport, ':', 2), 0);
  fuser := SubString(params, ' ', 3);
  fstatus := StrToIntDef(fuser, -1);

  if fstatus = -1 then
  begin
    fpass := SubString(params, ' ', 4);
    fstatus := StrToIntDef(SubString(params, ' ', 5), 0);
  end
  else
  begin
    fuser := '';
    fpass := '';
    fstatus := StrToIntDef(SubString(params, ' ', 3), 0);
  end;

  socksini.WriteString(fname, 'Host', fhost);
  socksini.WriteInteger(fname, 'Port', fport);
  socksini.WriteString(fname, 'Username', fuser);
  socksini.WriteString(fname, 'Password', fpass);
  socksini.WriteBool(fname, 'Enabled', boolean(fstatus));
  socksini.UpdateFile;
  try
    proxys.Add(TmSLSocks5.Create(fname));
  except
    on E: Exception do
      Irc_AddText(Netname, Channel,
        '<c4><b>ERROR</c></b>: IrcAddSocks5.proxys.Add: %s',
        [e.Message]);
  end;
  Result := True;
end;

function IrcDelSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  trigger, Value: AnsiString;
  rx: TRegExpr;
begin
  Result := False;

  rx := TRegExpr.Create;
  try
    rx.ModifierI := True;
    rx.Expression := '[\-]{1,2}(name|index) ([^\s]+)';
    if rx.Exec(params) then
    begin
      trigger := AnsiUpperCase(rx.Match[1]);
      Value := rx.Match[2];
    end
    else
    begin
      irc_addtext(Netname, Channel, 'dOH! something wrong!');
      exit;
    end;
  finally
    rx.Free;
  end;

  if ((trigger <> 'NAME') and (trigger <> 'INDEX')) then
  begin
    irc_addtext(Netname, Channel,
      'Use delsocks5 --NAME <socks5 name> OR --INDEX <# in listsocks5>');
    exit;
  end;

  if trigger = 'NAME' then
    Result := RemoveProxy(Value);
  if trigger = 'INDEX' then
    Result := RemoveProxy(StrToInt(Value));

  Result := True;
end;

function IrcRehashSocks5(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  Result := RehashProxys;
end;

function IrcDisplaySocks5(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  i: integer;
begin
  irc_addtext(Netname, Channel, 'Listing all %d Proxys:', [proxys.Count]);
  for i := 0 to proxys.Count - 1 do
  begin
    irc_addtext(Netname, Channel, '%d) %s  %s:%d %s',
      [i, TmSLSocks5(proxys.Items[i]).Name, TmSLSocks5(proxys.Items[i]).host,
      TmSLSocks5(proxys.Items[i]).port, TmSLSocks5(proxys.Items[i]).username]);
  end;
  Result := True;
end;

function IrcSetSocks5(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  vname, vvalue, vtrigger: AnsiString;
  virc: TMyIrcThread;
  vsite: TSite;
  vsocks: TmSLSocks5;
begin
  Result := False;
  vtrigger := UpperCase(SubString(params, ' ', 1));
  vname := UpperCase(SubString(params, ' ', 2));
  vvalue := UpperCase(SubString(params, ' ', 3));
  vsocks := nil;

  if vtrigger = 'SITE' then
  begin
    vsite := FindSiteByName('', vname);
    if vsite = nil then
    begin
      irc_addtext(Netname, Channel, 'Cant find Site with name %s!', [vname]);
      exit;
    end;

    if vvalue = '-1' then
    begin
      vsite.ProxyName := '!!NOIN!!'; //means Proxy usage removed
      Result := True;
      exit;
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, 'Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;
      vsite.ProxyName := vvalue;
    end;
  end
  else if vtrigger = 'IRC' then
  begin
    virc := FindIrcnetwork(vname);
    if virc = nil then
    begin
      irc_addtext(Netname, Channel, 'Cant find IRCNetwork with name %s!', [vname]);
      exit;
    end;

    if vvalue = '-1' then
    begin
      virc.ProxyName := '!!NOIN!!'; //means Proxy usage removed
      Result := True;
      exit;
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, 'Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;
      virc.ProxyName := vvalue;
    end;
  end;

  Result := True;
end;

function IrcTestLanguageBase(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  irc_addtext(Netname, Channel, 'Read language for: ' + params +
    '    (only new languagebase supported right now...)');
  // if use_new_language_base then begin
  // irc_addtext(netname,channel,'New languagebase...');
  irc_addtext(Netname, Channel, 'language ->' +
    FindLanguageOnDirectory(params));
  Result := True;
  // end;

end;

function IrcLanguageBaseReload(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  irc_addtext(Netname, Channel, SLLanguagesReload);
  Result := True;
end;

function IrcShowAllRules(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename, sections: AnsiString;
  xs: TStringList;
  ii, i: integer;
  r: TRule;
begin
  xs := TStringList.Create;
  try
    sitename := UpperCase(SubString(params, ' ', 1));
    sections := mystrings.RightStr(params, length(sitename) + 1);

    if sections <> '' then
    begin
      xs.Delimiter := ' ';
      xs.DelimitedText := sections;
      for i := 0 to xs.Count - 1 do
      begin
        for ii := 0 to rules.Count - 1 do
        begin
          r := TRule(rules[ii]);
          if ((r.sitename = sitename) and (r.section = xs.Strings[i])) then
          begin
            irc_addtext(Netname, Channel, '%d %s', [ii, r.AsText(True)]);
          end;
        end;
      end;
    end
    else
    begin
      for ii := 0 to rules.Count - 1 do
      begin
        r := TRule(rules[ii]);
        if r.sitename = sitename then
        begin
          irc_addtext(Netname, Channel, '%d %s', [ii, r.AsText(True)]);
        end;
      end;
    end;
    Result := True;
  finally
    xs.Free;
  end;
end;

function IrcAllRuleDel(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitess, sectionss: TStringList;
  // s: TSite;
  sitename, section: AnsiString;
  ii, i: integer;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  // uppercase(mystrings.RightStr(params, length(sitename)+1));

  sitess := TStringList.Create;
  sectionss := TStringList.Create;
  try
    if sitename = '*' then
    begin
      if section = '' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename',
            'SLFTP')) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;

          RulesRemove(TSite(sites.Items[i]).Name, '');
        end;
      end
      else
      begin
        sectionss.commatext := section;
        for i := 0 to sites.Count - 1 do
        begin
          for ii := 0 to sectionss.Count - 1 do
          begin
            if TSite(sites.Items[i]).IsSection(sectionss.Strings[ii]) then
              RulesRemove(TSite(sites.Items[i]).Name, sectionss.Strings[ii]);
            // else irc_addtext(netname,channel,'Sections "%s" not found on site: %s',[sectionss.Strings[ii],TSite(sites.Items[i]).name])
          end;
        end;
      end;

    end;

    sitess.commatext := sitename;

    if section = '' then
      for i := 0 to sitess.Count - 1 do
        RulesRemove(sitess.Strings[i], '')
    else
    begin
      sectionss.commatext := section;
      for i := 0 to sitess.Count - 1 do
      begin
        for ii := 0 to sectionss.Count - 1 do
          RulesRemove(sitess.Strings[i], sectionss.Strings[ii]);
      end;
    end;

  finally
    sitess.Free;
    sectionss.Free;
  end;

  Result := True;
end;

function IrcSetMYSQLData(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  fhostport, fhost, fport, fuser, fpassw, fdbname, ftable: AnsiString;
begin
  //  Result := False;
  try
    fhostport := SubString(params, ' ', 1);
    fuser := SubString(params, ' ', 2);
    fpassw := SubString(params, ' ', 3);
    fdbname := SubString(params, ' ', 4);
    ftable := SubString(params, ' ', 5);
    fhost := SubString(fhostport, ':', 1);
    fport := SubString(fhostport, ':', 2);
    sitesdat.WriteString('MYSQL', 'Host', fhost);
    sitesdat.WriteString('MYSQL', 'Port', fport);
    sitesdat.WriteString('MYSQL', 'Username', fuser);
    sitesdat.WriteString('MYSQL', 'Password', fpassw);
    sitesdat.WriteString('MYSQL', 'dbname', fdbname);
    sitesdat.WriteString('MYSQL', 'tablename', ftable);
  finally
    sitesdat.UpdateFile;
    Result := True;
  end;
end;

function IrcViewMYSQLValue(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  Result := True;
end;

function IrcTweakMYSQL(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := True;
end;

function IrcMYSQLStatus(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

function IrcCreateBackup(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  error: AnsiString;
begin
  Result := ircBackup(error);
  if error <> '' then
    irc_addtext(Netname, Channel, '<b><c4>%s</b></c>', [error]);
end;

function IrcNoLoginMSG(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  svalue, sname: AnsiString;
  ss: TSite;
  i: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := SubString(params, ' ', 2);

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ss := TSite(sites.Items[i]);
      if (ss.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      if svalue = '' then
      begin
        if (ss.sw = sswGlftpd) then
        begin
          irc_addtext(Netname, Channel, '%s NoLogin MSG: %d', [ss.Name, Ord(ss.NoLoginMSG)]);
        end
        else
        begin
          irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name,
            SiteSoftWareToSTring(ss)]);
        end;
      end
      else if ((svalue = '1') or (svalue = '0')) then
      begin
        if (ss.sw = sswGlftpd) then
        begin
          ss.NoLoginMSG := StrToBoolDef(svalue, False);
          irc_addtext(Netname, Channel, '%s NoLogin MSG: %d', [ss.Name, Ord(ss.NoLoginMSG)]);
        end
        else
        begin
          irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name,
            SiteSoftWareToSTring(ss)]);
        end;
      end
      else
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
    end;
  end
  else
  begin
    ss := FindSiteByName('', sname);
    if ss = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Name]);
      Result := True;
      exit;
    end;

    if svalue = '' then
    begin
      if (ss.sw = sswGlftpd) then
      begin
        irc_addtext(Netname, Channel, '%s NoLogin MSG: %d', [ss.Name, Ord(ss.NoLoginMSG)]);
      end
      else
      begin
        irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name,
          SiteSoftWareToSTring(ss)]);
      end;
    end
    else if ((svalue = '1') or (svalue = '0')) then
    begin
      if (ss.sw = sswGlftpd) then
      begin
        ss.NoLoginMSG := StrToBoolDef(svalue, False);
        irc_addtext(Netname, Channel, '%s NoLogin MSG: %d', [ss.Name, Ord(ss.NoLoginMSG)]);
      end
      else
      begin
        irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name,
          SiteSoftWareToSTring(ss)]);
      end;
    end
    else
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
  end;

  Result := True;
end;

function IrcUseForNFOdownload(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sname: AnsiString;
  svalue: integer;
  ss: TSite;
  i: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := StrToIntDef(SubString(params, ' ', 2), -1);

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ss := TSite(sites.Items[i]);
      if (ss.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      if svalue = -1 then
      begin
        irc_addtext(Netname, Channel, '%s use for NFO download: %d', [ss.Name, ss.UseForNFOdownload]);
      end
      else if ((svalue = 1) or (svalue = 0)) then
      begin
        ss.UseForNFOdownload := svalue;
        irc_addtext(Netname, Channel, '%s use for NFO download: %d', [ss.Name, ss.UseForNFOdownload]);
      end
      else
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
    end;
  end
  else
  begin
    ss := FindSiteByName('', sname);
    if ss = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Name]);
      Result := True;
      exit;
    end;

    if svalue = -1 then
    begin
      irc_addtext(Netname, Channel, '%s use for NFO download: %d', [ss.Name, ss.UseForNFOdownload]);
    end
    else if ((svalue = 1) or (svalue = 0)) then
    begin
      ss.UseForNFOdownload := svalue;
      irc_addtext(Netname, Channel, '%s use for NFO download: %d', [ss.Name, ss.UseForNFOdownload]);
    end
    else
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
  end;

  Result := True;
end;

function IrcSkipBeingUploadedFiles(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  svalue, sname: AnsiString;
  ss: TSite;
  i: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := SubString(params, ' ', 2);

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ss := TSite(sites.Items[i]);
      if (ss.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;

      if svalue = '' then
        irc_addtext(Netname, Channel, '%s Skip incomplete files: %d', [ss.Name, Ord(ss.SkipBeingUploadedFiles)])
      else if ((svalue = '1') or (svalue = '0')) then
      begin
        ss.SkipBeingUploadedFiles := StrToBoolDef(svalue, False);
        irc_addtext(Netname, Channel, '%s Skip incomplete files: %d', [ss.Name, Ord(ss.SkipBeingUploadedFiles)]);
      end
      else
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
    end;
  end
  else
  begin
    ss := FindSiteByName('', sname);
    if ss = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Name]);
      Result := True;
      exit;
    end;

    if svalue = '' then
      irc_addtext(Netname, Channel, '%s Skip incomplete files: %d', [ss.Name, Ord(ss.SkipBeingUploadedFiles)])
    else if ((svalue = '1') or (svalue = '0')) then
    begin
      ss.SkipBeingUploadedFiles := StrToBoolDef(svalue, False);
      irc_addtext(Netname, Channel, '%s Skip incomplete files: %d', [ss.Name, Ord(ss.SkipBeingUploadedFiles)]);
    end
    else
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
  end;

  Result := True;
end;


(* PreURLs *)
function IrcPreURLAdd(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  url, offset: AnsiString;
begin
  //Result := False;
  url := SubString(params, ' ', 1);
  offset := SubString(params, ' ', 2);
  if preurls.IndexOf(url + ';' + offset) > -1 then
  begin
    irc_addtext(Netname, Channel, 'Error! url already added!');
    Result := True;
    exit;
  end;
  preurls.Add(url + ';' + offset);
  Result := True;
end;

function IrcPreURLDel(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: AnsiString;
  index: integer;
begin
  //  Result := False;
  s := SubString(params, ' ', 1);
  index := StrToInt(s);
  try
    preurls.BeginUpdate;
    preurls.Delete(index);
    preurls.EndUpdate;
  finally
    Result := True;
  end;

end;

function IrcPreURLMod(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s, url, offset: AnsiString;
  index: integer;
begin
  //Result := False;
  s := SubString(params, ' ', 1);
  url := SubString(params, ' ', 2);
  offset := SubString(params, ' ', 3);
  index := StrToInt(s);
  try
    preurls.BeginUpdate;
    preurls.Strings[index] := url + ';' + offset;
    preurls.EndUpdate;
  finally
    Result := True;
  end;
end;

function IrcPreURLList(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  url, offset: AnsiString;
  i: integer;
begin
  //Result := False;

  for i := 0 to preurls.Count - 1 do
  begin
    url := SubString(preurls.Strings[i], ' ', 1);
    offset := SubString(preurls.Strings[i], ' ', 2);
    irc_addtext(Netname, Channel, '#%d %s %s', [i, url, offset]);
  end;
  Result := True;
end;

function IrcFakeReload(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := FakesRehash;
end;

function IrcSpamConfig(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  vsecs, vsval: TStringList;
  i: integer;
  csec, ckey, cvalue: AnsiString;
begin
  //  Result := False;
  vsecs := TStringList.Create;
  try
    vsval := TStringList.Create;
    try

      if params = '' then
      begin
        spamcfg.ReadSections(vsecs);
        irc_addtext(Netname, Channel, '<b>Sections:</b> %s', [vsecs.commatext]);
        for i := 0 to vsecs.Count - 1 do
        begin
          vsval.Clear;
          spamcfg.ReadSection(vsecs.Strings[i], vsval);
          //spamcfg.ReadSectionValues(vsecs.Strings[i],vsval);
          //irc_addtext(Netname, Channel, '<b>%s:</b> %s',[vsecs.Strings[i], vsval.commatext]);

          IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>' +
            vsecs.Strings[i] + ':</b> ', 9);

        end;
        Result := True;
        exit;
      end;
      csec := SubString(params, ' ', 1);
      ckey := SubString(params, ' ', 2);
      cvalue := SubString(params, ' ', 3);

      if ckey = '' then
      begin
        vsval.Clear;
        //spamcfg.ReadSectionValues(csec, vsval);
        spamcfg.ReadSection(csec, vsval);
        //irc_addtext(Netname, Channel, ' %s',[vsval.commatext]);

        IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>valid keys:</b> ',
          9);

        Result := True;
        exit;
      end;

      if cvalue = '' then
      begin
        if spamcfg.ReadBool(csec, ckey, True) then
          irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 1 (Announce)', [csec, ckey])
        else
          irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 0 (Skip)', [csec, ckey]);

        Result := True;
        exit;
      end;

    finally
      vsval.Free;
    end;

  finally
    vsecs.Free;
  end;

  spamcfg.WriteInteger(csec, ckey, StrToIntDef(cvalue, 0));
  spamcfg.UpdateFile;

  if spamcfg.ReadBool(csec, ckey, True) then
    irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 1 (Announce)', [csec, ckey])
  else
    irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 0 (Skip)', [csec, ckey]);

  Result := True;
end;


function IrcSetupOffset(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  r: TRegexpr;
begin
  Result := False;

  if params <> '' then
  begin

    r := TRegexpr.Create;
    try
      r.Expression := '^(\+|\-)([\d]+)$';

      if r.Exec(params) then
      begin
        irc_addtext(Netname, Channel, 'Will change Offset value from %s to %s', [config.ReadString('taskpretime', 'offset', 'Not Found!'), params]);
        config.WriteString('taskpretime', 'offset', params);
        config.UpdateFile;
      end
      else
      begin
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
        exit;
      end;

    finally
      r.Free;
    end;

  end;

  irc_addtext(Netname, Channel, 'Offset value: %s', [config.ReadString('taskpretime', 'offset', 'Not Found!')]);

  Result := True;
end;

function IrcSetupPretimeMode(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  pmode: integer;
begin
  Result := False;

  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TPretimeLookupMOde))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0))), pretimeModeToString(TPretimeLookupMOde(pmode))]);
    config.WriteInteger('taskpretime', 'mode', pmode);
    setPretimeMode_One(TPretimeLookupMOde(pmode));
    config.UpdateFile;
  end;
  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('taskpretime', 'mode', 0), pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)))]);

  Result := True;
end;

function IrcSetupPretimeMode2(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  pmode: integer;
begin
  Result := False;

  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TPretimeLookupMOde))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0))), pretimeModeToString(TPretimeLookupMOde(pmode))]);
    config.WriteInteger('taskpretime', 'mode_2', pmode);
    setPretimeMode_Two(TPretimeLookupMOde(pmode));
    config.UpdateFile;
  end;
  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('taskpretime', 'mode_2', 0), pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0)))]);

  Result := True;
end;

function IrcSetupADDPreMode(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  pmode: integer;
begin
  Result := False;

  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TAddPreMode))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [addPreModeToString(TAddPreMode(config.ReadInteger('dbaddpre', 'mode', 0))), addPreModeToString(TAddPreMode(pmode))]);
    config.WriteInteger('dbaddpre', 'mode', pmode);
    setAddPretimeMode(TAddPreMode(pmode));
    config.UpdateFile;
  end;
  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('dbaddpre', 'mode', 0), addPreModeToString(TAddPreMode(config.ReadInteger('dbaddpre', 'mode', 0)))]);

  Result := True;
end;


function IrcFindPretime(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  pt: TDateTime;
  resu: TPretimeResult;
begin

  if config.ReadInteger('taskpretime', 'mode', 0) = 0 then
  begin
    Irc_AddText(Netname, Channel, '<c15>INFO</c>: Pretime_mode is set to None!');
    Result := True;
    Exit;
  end;

  resu := getPretime(params);
  pt := resu.pretime;
  if datetimetounix(pt) > 15 then
    irc_addtext(Netname, Channel, 'PRETIME %s ~ %s %s (%s)', [params,
      dbaddpre_GetPreduration(pt), FormatDateTime('yyyy-mm-dd hh:nn:ss', pt), resu.mode])
  else
  begin
    irc_addtext(Netname, Channel, 'No valid pretime -> ' +
      IntToStr(datetimetounix(pt)));
  end;
  Result := True;
end;

function IrcDisplayMappings(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  i: integer;
begin
  irc_addtext(Netname, Channel, 'Listing %d entries...', [mappingslist.Count]);
  try
    for i := 0 to mappingslist.Count - 1 do
      irc_addtext(Netname, Channel, '%s -> %s <c4>if</c> %s ',
        [TMap(mappingslist.Items[i]).origsection, TMap(mappingslist.Items[i])
        .newsection, TMap(mappingslist.Items[i]).mask.mask]);
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;
  Result := True;
end;

function IrcDelPart(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  nn, blowchannel: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Network not found.');
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b <> nil then
  begin
    ircth.chanpart(blowchannel, ircth.BotNick);
    chankeys.Remove(b);
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found',
      [blowchannel, nn]));
  Result := True;
end;

function IrcDWherePred(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

function IrcReloadGlobalSkipGrouplist(const Netname, Channel: AnsiString;
  params: AnsiString): boolean;
begin
  Result := Rehashglobalskiplist;
  if Result then
    irc_addtext(Netname, Channel, '%d groups in list.',
      [globalgroupskip.Count]);

end;

function IrcSetPretime(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  section: AnsiString;
  s_pretime: AnsiString;
  pretime: integer;
  site: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  s_pretime := SubString(params, ' ', 3);
  // mystrings.RightStr(params, length(sitename)+1+length(section)+1);

  if s_pretime = '-' then
    pretime := -10
  else
    pretime := StrToIntDef(s_pretime, -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      if ((pretime = -10) or (pretime >= 0)) then
        TSite(sites.Items[i]).sectionpretime[section] := pretime;
      if (TSite(sites.Items[i]).sectionpretime[section] <> -1) then
      begin
        irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is<c7> %d</c>',
          [TSite(sites.Items[i]).Name, section,
          TSite(sites.Items[i]).sectionpretime[section]]);
      end
      else
      begin
        irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is not set',
          [TSite(sites.Items[i]).Name, section]);
      end;

    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;

      for i := 0 to x.Count - 1 do
      begin
        site := FindSiteByName(Netname, x.Strings[i]);
        if site = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if ((pretime = -10) or (pretime >= 0)) then
          site.sectionpretime[section] := pretime;
        if (site.sectionpretime[section] <> -1) then
        begin
          irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is<c7> %d</c>', [sitename,
            section, site.sectionpretime[section]]);
        end
        else
        begin
          irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is not set', [sitename,
            section]);
        end;

      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcRulesLoad(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

function IrcSetSitePermdown(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  s: TSite;
  sname: AnsiString;
  svalue: AnsiString;
  ivalue: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := UpperCase(SubString(params, ' ', 2));
  if svalue = '' then
  begin
    ivalue := 1 // default value if no parameter is given
  end
  else
  begin
    ivalue := StrToIntDef(svalue, 0);
  end;

  if ((ivalue > 1) or (ivalue < 0)) then
  begin
    irc_AddText(Netname, Channel, '<c4><b>Syntax Error!</b></c> %d is not valid, 1 or 0',
      [ivalue]);
    exit;
  end;

  s := FindSiteByName('', sname);

  if s = nil then
  begin
    irc_AddText(Netname, Channel, '<c4><b>Site not found</b></c> with name: %s', [sname]);
    exit;
  end;

  if boolean(ivalue) then
  begin
    try
      s.RemoveAutoIndex;
      s.RemoveAutoBnctest;
      s.RemoveAutoRules;
      s.RemoveAutoNuke;
      s.RemoveAutoDirlist;
      s.RemoveAutoCrawler;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in remove auto tasks: %s',
          [E.Message]);
    end;

    try
      s.markeddown := True;
      s.working := sstDown;
    except on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in mark as down: %s', [E.Message]);
    end;

    try
      QueueEmpty(s.Name);
    except on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in QueueEmpty: %s', [E.Message]);
    end;
    try
      // rewrite config value
      s.WCInteger('disabled_autonuke', s.RCInteger('autonuke', 0));
      s.WCInteger('disabled_autoindex', s.RCInteger('autoindex', 0));
      s.WCInteger('disabled_autobnctest', s.RCInteger('autobnctest', 0));
      s.WCInteger('disabled_autorules', s.RCInteger('autorules', 0));
      s.WCInteger('disabled_autodirlist', s.RCInteger('autodirlist', 0));
      // s.WCInteger('disabled_autologin',s.RCInteger('autologin',0));
    except
      on E: Exception do
        irc_AddText(Netname, Channel,
          format('<c4>[Exception]</c> in rewrite value: %s', [E.Message]));
    end;

    try
      sitesdat.DeleteKey('site-' + s.Name, 'autonuke');
      sitesdat.DeleteKey('site-' + s.Name, 'autoindex');
      sitesdat.DeleteKey('site-' + s.Name, 'autobnctest');
      sitesdat.DeleteKey('site-' + s.Name, 'autorules');
      sitesdat.DeleteKey('site-' + s.Name, 'autodirlist');
      // sitesdat.DeleteKey('site-'+s.name,'autologin');
      // sitesdat.UpdateFile;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in delete old value: %s',
          [E.Message]);
    end;
  end
  else
  begin

    try
      // rewrite config value
      s.WCInteger('autonuke', s.RCInteger('disabled_autonuke', 0));
      s.WCInteger('autoindex', s.RCInteger('disabled_autoindex', 0));
      s.WCInteger('autobnctest', s.RCInteger('disabled_autobnctest', 0));
      s.WCInteger('autorules', s.RCInteger('disabled_autorules', 0));
      s.WCInteger('autodirlist', s.RCInteger('disabled_autodirlist', 0));
      // s.WCInteger('autologin',s.RCInteger('disabled_autologin',0));
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in rewrite orig. value: %s',
          [E.Message]);
    end;

    try
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autonuke');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autoindex');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autobnctest');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autorules');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autodirlist');
      // sitesdat.DeleteKey('site-'+s.name,'autologin');
      // sitesdat.UpdateFile;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in delete disabled value: %s',
          [E.Message]);
    end;

    try
      s.AutoIndex;
      s.AutoBnctest;
      s.AutoRules;
      s.AutoNuke;
      s.AutoDirlist;
      s.AutoCrawler;
    except
      on E: Exception do
        irc_AddText(Netname, Channel,
          format('<c4>[Exception]</c> in start auto tasks: %s', [E.Message]));
    end;
  end;

  s.PermDown := boolean(ivalue);
  Result := True;
end;

function IrcSetdown(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  s: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  x := TStringList.Create;
  try
    x.DelimitedText := UpperCase(params);

    if (x.Strings[0] = '!ALL!') or (x.Strings[0] = '*') then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
          then
          Continue;
        if (TSite(sites.Items[i]).PermDown) then
          Continue;

        s := TSite(sites[i]);
        s.markeddown := True;
        s.working := sstDown;
        s.markeddown := True;
        s.RemoveAutoIndex;
        s.RemoveAutoBnctest;
        s.RemoveAutoRules;
        QueueEmpty(s.Name);
      end;
    end
    else
    begin
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);

        if s = nil then
        begin
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Site <b>%s</b> not found.',
            [x.Strings[i]]);
          Continue;
        end;

        if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
          Continue;
        if (s.PermDown) then
          Continue;

        s.markeddown := True;
        s.working := sstDown;
        s.markeddown := True;
        s.RemoveAutoIndex;
        s.RemoveAutoBnctest;
        s.RemoveAutoRules;
        QueueEmpty(s.Name);
      end;
    end;

  finally
    x.Free;
  end;

  QueueFire; //to remove entries from queue
  Result := True;
end;

function IrcRulesReload(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
begin
  RulesReload;
  Result := True;
end;

function parseSTATLine(sitename, line: AnsiString; includeLastCredits: boolean = false):
  AnsiString;
var
  x: TRegExpr;
  ss, creds, ratio: AnsiString;
  minus: boolean;
  c: double;
begin
  x := TRegExpr.Create;
  try
    x.ModifierI := True;

    x.Expression := '\[?(R(atio)?|Shield|Health\s?)[\:\(]\s*(.*?)[)\]]'; // hardcoded for better result handling..
    if x.Exec(line) then
    begin
      if (AnsiContainsText(x.Match[3], 'Unlimited') or (x.Match[3] = '1:0.0')) then
        ratio := 'Unlimited'
      else
        ratio := x.Match[3];
    end;

    x.Expression := '\[?(C(redits|reds)?|Damage|Ha\-ooh\!)[:(]?\s?([\-\d\.\,]+)((M|G|T)B|(E|Z)P)[\]\)]?'; // hardcoded for better result handling..
    if x.Exec(line) then
    begin
      minus := False;
      ss := x.Match[3];
      if AnsiContainsText(ss, '-') then
      begin
        minus := True;
        ss := StringReplace(ss, '-', '', [rfReplaceAll, rfIgnoreCase]);
      end;
{$IFDEF FPC}
      ss := StringReplace(ss, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll,
        rfIgnoreCase]);
{$ELSE}
      ss := StringReplace(ss, '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
{$ENDIF}
      c := strtofloat(ss);
      ss := x.Match[4];
      if AnsiUpperCase(ss) = 'MB' then
      begin
        ss := 'MB';
        if c > 1024 then
        begin
          c := c / 1024;
          ss := 'GB';
        end;
        if c > 1024 then
        begin
          c := c / 1024;
          ss := 'TB';
        end;
      end;

      if minus then
        creds := Format('<c4> -%.2f %s </c>', [c, ss])
      else
        creds := Format('<c3> %.2f %s </c>', [c, ss]);
    end;

    result := Format('Credits on <b>%s</b>: %s (%s)', [sitename, creds, ratio]);
  finally
    x.free;
  end;

end;

function IrcShowCredits(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i: integer;
  s: TSite;
  r: TRawTask;
  tn: TTaskNotify;
  sitename: AnsiString;
begin
  Result := False;
  sitename := AnsiUpperCase(SubString(params, ' ', 1));

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if s = nil then
        Continue;
      if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;
      if (s.PermDown) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>Site %s is set permdown! </c></b>', [s.Name]);
        Continue;
      end;
      if (s.working <> sstUp) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>Site %s is temporarily offline! </c></b>', [s.Name]);
        Continue;
      end;

      tn := AddNotify;
      try
        try
          r := TRawTask.Create(Netname, Channel, s.Name, '', 'SITE STAT');
          tn.tasks.Add(r);
          AddTask(r);
          QueueFire;
          tn.event.WaitFor($FFFFFFFF);
        except on E: Exception do
          begin
            RemoveTN(tn);
            irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
            continue;
          end;
        end;
        irc_addtext(Netname, Channel, parseSTATLine(s.Name,
          TSiteResponse(tn.responses[0]).response));
      finally
        RemoveTN(tn);
      end;

    end;
  end
  else
  begin
    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;
    if (s.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
    begin
      exit;
    end;
    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> is perm down.', [sitename]);
      exit;
    end;

    tn := AddNotify;
    try
      try
        r := TRawTask.Create(Netname, Channel, s.Name, '', 'SITE STAT');
        tn.tasks.Add(r);
        AddTask(r);
        QueueFire;
        tn.event.WaitFor($FFFFFFFF);
      except on E: Exception do
        begin
          RemoveTN(tn);
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
          Exit;
        end;
      end;

      irc_addtext(Netname, Channel, parseSTATLine(s.Name,
        TSiteResponse(tn.responses[0]).response));
    finally
      RemoveTN(tn);
    end;
  end;
  Result := True;
end;

function IrcShowAppStatus(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  rx: TRegexpr;
  spd, sup, sdn, suk: TStringList;
begin
  irc_addtext(Netname, Channel, '<b>%s</b> with OpenSSL %s is up for [%s] <c7><b>%s</b></c>',
    [Get_VersionString, OpenSSLShortVersion, DatetimetoStr(started),
    DateTimeAsString(started)]);

  irc_addtext(Netname, Channel, '<b>Knowledge Base</b>: %d Rip''s in mind', [kb_list.Count]);
  irc_addtext(Netname, Channel, TheTVDbStatus);

  if TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)) = plmSQLITE then
    irc_addtext(Netname, Channel, dbaddpre_Status);

  irc_addtext(Netname, Channel, 'Other Stats: %s <b>-</b> %s <b>-</b> %s', [dbaddurl_Status, dbaddimdb_Status, dbaddnfo_Status]);

  rx := TRegexpr.Create;
  rx.ModifierI := True;
  sup := TStringList.Create;
  spd := TStringList.Create;
  sdn := TStringList.Create;
  suk := TStringList.Create;
  try
    SitesWorkingStatusToStringlist(Netname, Channel, sup, sdn, suk, spd);

    irc_addtext(Netname, Channel,
      '<b>Sites count</b>: %d | <b>Online</b> %d - <b>Offline</b> %d - <b>Unknown</b> %d - <b>Permanent offline</b> %d ',
      [sites.Count - 1, sup.Count, sdn.Count, suk.Count, spd.Count]);
    rx.Expression := 'QUEUE\:\s(\d+)\s\(Race\:(\d+)\sDir\:(\d+)\sAuto\:(\d+)\sOther\:(\d+)\)';
    if rx.Exec(ReadAppQueueCaption) then
      irc_addtext(Netname, Channel,
        '<b>Complete queue count</b>: %s | <b>Racetasks</b> %s - <b>Dirlisttasks</b> %s - <b>Autotasks</b> %s - <b>Other</b> %s',
        [rx.Match[1], rx.Match[2], rx.Match[3], rx.Match[4], rx.Match[5]]);
  finally
    rx.free;
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function Ircaddknowngroup(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  section, glist: AnsiString;
  y, x: TStringList;
  i: integer;
begin
  Result := False;
  section := UpperCase(SubString(params, ' ', 1));
  glist := mystrings.RightStr(params, length(section) + 1);

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section <b>%s</b> not found!', [section]);
    exit;
  end;
  x := TStringList.Create;
  y := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := glist;
    y.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.knowngroups');
    for i := 0 to x.Count - 1 do
    begin
      if IsKnownGroup(section, x.Strings[i]) <> grp_known then
        y.Values[section] := y.Values[section] + ' ' + x.Strings[i];
    end;
    y.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.knowngroups');
    KnownGroupsStart;
  finally
    x.Free;
    y.Free;
  end;
  Result := True;
end;

function IrcSLFTPConfig(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  x, y: TStringList;
  csec, ckey, cvalue, s: AnsiString;
  i, ii: integer;
begin
  Result := False;
  x := TStringList.Create;
  y := TStringList.Create;
  try
    s := '';
    ii := 0;
    // ok, we have no parameter, so we announce all sections.
    if params = '' then
    begin
      config.ReadSections(x);
      irc_addtext(Netname, Channel, '<b>Valid config sections</b>:');
      for i := 0 to x.Count - 1 do
      begin
        if ii = 7 then
        begin
          ii := 0;
          s := s + #10#13;
        end;
        s := s + x.Strings[i] + ',';
        Inc(ii);
      end;
      Delete(s, length(s), 1);
      irc_addtext(Netname, Channel, s);
      //x.Free;
      //y.Free;
      Result := True;
      exit;
    end;

    csec := SubString(params, ' ', 1);
    ckey := SubString(params, ' ', 2);
    cvalue := SubString(params, ' ', 3);

    // we have a section but no key, so we announce all keys we have.
    // should we add a ini-section check? x.indexof(csec) ??!
    if ((csec <> '') and (ckey = '')) then
    begin
      config.ReadSection(csec, x);
      irc_addtext(Netname, Channel, '<b>Valid config keys for section</b>: <b>%s</b>', [csec]);

      for i := 0 to x.Count - 1 do
      begin
        if ii = 7 then
        begin
          ii := 0;
          s := s + #10#13;
        end;
        s := s + x.Strings[i] + ',';
        Inc(ii);
      end;
      Delete(s, length(s), 1);
      irc_addtext(Netname, Channel, s);
      //x.Free;
      //y.Free;
      Result := True;
      exit;
    end;

    // ok over here we have a section and a key ...   over here we have 2 checks to not screw the ini file!
    config.ReadSections(x);
    if x.IndexOf(csec) = -1 then
    begin
      //x.Free;
      //y.Free;
      irc_addtext(Netname, Channel, 'Section %s was not found!', [csec]);
      exit;
    end;

    config.ReadSection(csec, y);

    if y.IndexOf(ckey) = -1 then
    begin
      //x.Free;
      //y.Free;
      irc_addtext(Netname, Channel, 'Key %s was not found in section %s!', [ckey, csec]);
      exit;
    end;

    // value is empty and we only announce current value...
    if cvalue = '' then
    begin
      s := config.ReadString(csec, ckey, 'nil');
      irc_addtext(Netname, Channel, '[%s]%s=<b>%s</b>', [csec, ckey, s]);
    end
    else
    begin
      // here the real deal start!
      try
        config.WriteString(csec, ckey, cvalue);
        config.UpdateFile;
        s := config.ReadString(csec, ckey, 'nil');
        irc_addtext(Netname, Channel, '[%s]%s=<b>%s</b>', [csec, ckey, s]);
      except
        on E: Exception do
        begin
          irc_addtext(Netname, Channel, 'Exception : %s', [E.Message]);
        end;
      end;
    end;

  finally
    x.Free;
    y.Free;
  end;

  Result := True;
end;

function IrcChanSetSitename(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sname, nname, chans: AnsiString;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
  y, x: TStringList;
  i: integer;
  s: TSite;
begin
  sname := UpperCase(SubString(params, ' ', 1));
  nname := UpperCase(SubString(params, ' ', 2));
  chans := SubString(params, ' ', 3);

  s := FindSiteByName(Netname, sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sname]);
    Result := False;
    exit;
  end;

  ircth := FindIrcnetwork(nname);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    Result := False;
    exit;
  end;

  x := TStringList.Create;
  y := TStringList.Create;
  try
    x.commatext := chans;

    for i := 0 to x.Count - 1 do
    begin
      b := FindIrcBlowfish(nname, x.Strings[i], False);
      if b <> nil then
        y.Add(x.Strings[i])
      else
        irc_addtext(Netname, Channel,
          format('<c4><b>ERROR</c></b>: Channel <b>%s@%s</b> not found!', [x.Strings[i],
          nname]));
    end;
    s.WCString('irc_channels', nname + ';' + y.commatext);
  finally
    y.Free;
    x.Free;
  end;
  Result := True;
end;

function IrcAnnounceIMDBInfo(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  //  rlzname: string;
  i: integer;
  imdbdata: TDbImdbData;
begin
  //  Result := False;

  i := last_imdbdata.IndexOf(params);
  if i = -1 then
  begin
    irc_addtext(Netname, Channel,
      format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    Result := True;
    exit;
  end
  else
  begin
    imdbdata := TDbImdbData(last_imdbdata.Objects[i]);
    imdbdata.PostResults(Netname, Channel, params);
  end;
  Result := True;
end;

{ The TV dB Function              }

function IrcAnnounceTVInfo(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  db_tvrage: TTVInfoDB;
begin

  db_tvrage := nil;

  try
    if StrToIntDef(params, -1) = -1 then
    begin
      try
        db_tvrage := getTVInfoByReleaseName(params);
      except
        on E: Exception do
        begin
          Debug(dpError, section,
            format('Exception in IrcAnnounceTheTVDbInfo.getTVInfoBbyShowName: %s', [E.Message]));
          irc_AddText(Netname, Channel,
            format('<c4>[Exception]</c> in IrcAnnounceTVInfo.getTheTVInfoByShowName: %s',
            [E.Message]));
          Result := True;
          exit;
        end;
      end;
    end
    else
    begin
      try
        db_tvrage := getTVInfoByShowID(params);
      except
        on E: Exception do
        begin
          //        db_tvrage := nil;
          Debug(dpError, section,
            format('Exception in IrcAnnounceTVInfo: %s',
            [E.Message]));
          irc_AddText(Netname, Channel,
            format('<c4>[Exception]</c> in IrcAnnounceTVInfo: %s',
            [E.Message]));
          Result := True;
          exit;
        end;
      end;
    end;

    if db_tvrage <> nil then
    begin
      db_tvrage.PostResults(db_tvrage.rls_showname, Netname, Channel);
      Result := True;
    end
    else
    begin
      irc_addtext(Netname, Channel,
        format('<c4>[<b>FAILED<b>]</c> Nothing found for <b>%s</b>', [params]));
      Result := True;
    end;
  finally
    db_tvrage.Free;
  end;
end;

function IrcDelTheTVDbInfo(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  return: Integer;
begin
  Result := False;
  if strtointdef(params, -1) > -1 then
    return := deleteTVInfoByID(params)
  else
    return := deleteTVInfoByRipName(params);

  //better error message system needed :/
  case return of
    1:
      begin
        //        Irc_AddText(Netname, Channel, '');
        result := true;
        exit;
      end;
    10:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete id:' + params);
        result := true;
        exit;
      end;
    11:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete id:' + params);
        result := true;
        exit;
      end;
    12:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete ' + params);
        result := true;
        exit;
      end;
    13:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete ' + params);
        result := true;
        exit;
      end;
  end;
  //we dont set result to true, to check if something went wrong...
end;

function IrcUpdateTVMazeInfo(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  respo, tvmaze_id, tv_showname: AnsiString;
  otvr, newtvi: TTVInfoDB;
begin
  Result := false;

  tvmaze_id := '';
  tv_showname := '';

  if strtointdef(params, -1) > -1 then
  begin
    tvmaze_id := params;
  end
  else
  begin
    otvr := getTVInfoByReleaseName(params);
    if otvr <> nil then
    begin
      try
        tvmaze_id := otvr.tvmaze_id;
        tv_showname := otvr.tv_showname;
      finally
        otvr.free;
      end;
    end
    else
      tv_showname := params;
  end;

  if tvmaze_id = '' then
  begin
    Irc_AddText(Netname, Channel, Format('<b><c4>Error</c></b>: Show named <b>%s</b> not found in our local database.', [tv_showname]));
    exit;
  end;

  respo := slUrlGet('http://api.tvmaze.com/shows/' + tvmaze_id +
    '?embed[]=nextepisode&embed[]=previousepisode');

  if respo = '' then
  begin
    if tv_showname <> '' then
      Irc_AddText(Netname, Channel,
        Format('<b><c4>Error</c></b>: HTTP response for %s (ID :%d) was empty.', [tv_showname,
        tvmaze_id]))
    else
      Irc_AddText(Netname, Channel,
        Format('<b><c4>Error</c></b>: HTTP response for ID %d was empty.', [tvmaze_id]));
    exit;
  end;

  try
    newtvi := parseTVMazeInfos(respo);
    if newtvi = nil then
      Exit;

    try
      newtvi.last_updated := DateTimeToUnix(now());
      if (newtvi.Update(True)) then
      begin
        Result := True;
        newtvi.PostResults(newtvi.tv_showname, Netname, Channel);
      end;
    finally
      newtvi.free;
    end;

  except on e: Exception do
    begin
      irc_AddText(Netname, Channel, Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s',
        [e.Message]));
      Exit;
    end;
  end;
end;

function IrcSetTVRageID(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  mazeid, tvrageid: integer;
  tvi: TTVInfoDB;
begin
  tvrageid := StrtoIntDef(SubString(params, ' ', 1), -1);
  mazeid := StrtoIntDef(SubString(params, ' ', 2), -1);
  result := False;

  if mazeid > -1 then
    tvi := getTVInfoByShowID(inttostr(mazeid))
  else
    tvi := getTVInfoByReleaseName(mystrings.RightStr(params, length(inttostr(tvrageid)) + 1));

  if tvi = nil then
  begin
    Irc_AddText(Netname, Channel, '<c15><b>Info</c></b>: No entry found..');
    Exit;
  end;
  tvi.setTVRageID(tvrageid);
  tvi.free;
  result := True;
end;

function IrcSetTheTVDBID(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  mazeid, thetvdbid: integer;
  tvi: TTVInfoDB;
begin
  thetvdbid := StrtoIntDef(SubString(params, ' ', 1), -1);
  mazeid := StrtoIntDef(SubString(params, ' ', 2), -1);
  result := False;

  if mazeid > -1 then
    tvi := getTVInfoByShowID(inttostr(mazeid))
  else
    tvi := getTVInfoByReleaseName(mystrings.RightStr(params, length(inttostr(thetvdbid)) + 1));

  if tvi = nil then
  begin
    Irc_AddText(Netname, Channel, '<c15><b>Info</c></b>: No entry found..');
    Exit;
  end;
  tvi.setTheTVDbID(thetvdbid);
  tvi.free;
  result := True;
end;

function IrcAddTVMazeToDb(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  resp, ssname, sid: AnsiString;
  tvr: TTVInfoDB;
  x: TRegExpr;
  i, sresMAXi: integer;
  res: TStringlist;
  showname: AnsiString;
begin
  result := False;
  sid := UpperCase(SubString(params, ' ', 1));
  ssname := mystrings.RightStr(params, length(sid) + 1);
  sresMAXi := strtointdef(config.ReadString('tasktvinfo', 'max_sid_lookup_results', '5'), 5);

  x := TRegExpr.Create;
  try
    x.ModifierI := True;
    x.ModifierM := True;
    x.Expression := '\s\-c\:(\d+)$';
    // \s is importent for the right announce later...
    if x.Exec(params) then
      sresMAXi := StrToIntDef(x.Match[1], sresMAXi);
    ssname := x.Replace(ssname, '', False);
  finally
    x.Free;
  end;

  if ((sid = '--SEARCH') or (sid = '--S') or (sid = '-SEARCH') or (sid = '-S')) then
  begin
    getShowValues(ssname, showname);
    resp := findTVMazeIDByName(showname, netname, channel);

    if resp <> 'FAILED' then
    begin
      res := TStringlist.Create;
      try
        res.CommaText := resp;
        for I := 0 to res.Count - 1 do
        begin
          if i >= sresMAXi then
            break;
          irc_addtext(netname, channel, res.Strings[i]);
        end;
      finally
        res.free;
      end;
    end
    else
      irc_addtext(Netname, Channel, '<b><c5>TVInfo</c></b>: No match for %s found.',
        [showname]);

    result := True;
    Exit;
  end;

  if StrToIntDef(sid, -1) > -1 then
  begin
    try
      resp := slUrlGet('http://api.tvmaze.com/shows/' + sid +
        '?embed[]=nextepisode&embed[]=previousepisode');
    except
      on E: Exception do
      begin
        irc_AddText(Netname, Channel, format(
          '<c4>[Exception]</c> in IrcAddTheTVDbToDb.add.slurlget: %s',
          [E.Message]));
        result := True;
        Exit;
      end;
    end;

    if ((resp = '') or (resp = '[]')) then
    begin
      irc_addtext(Netname, Channel, 'No info found for ' + ssname);
      Result := True;
      Exit;
    end;

    tvr := parseTVMazeInfos(resp, ssname);
    tvr.rls_showname := mystrings.RightStr(params, length(sid) + 1);
    try
      tvr.Save;
    except
      on E: Exception do
      begin
        irc_AddText(Netname, Channel, format(
          '<c4>[Exception]</c> in IrcAddTheTVDbToDb.save: %s',
          [E.Message]));
        tvr.free;
        result := True;
        Exit;
      end;
    end;
    tvr.PostResults(mystrings.RightStr(params, length(sid) + 1), Netname, Channel);
    tvr.Free;
  end
  else
    irc_Addtext(netname, channel,
      '<c4><b>Syntax Error!</b></c> no id found to add, you may want to search? use -s');
  Result := True;

end;

function IrcShowSiteNukes(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  sitename, ss: AnsiString;
  Count: integer;
  r: TRegexpr;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  Count := StrToIntDef(SubString(params, ' ', 2), 150);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if site.PermDown then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.', [site.Name]);
    exit;
  end;

  if ((site.working = sstUnknown) or (site.working = sstDown)) then
  begin
    TSiteSlot(site.slots.Items[site.slots.Count - 1]).ReLogin();
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is offline do a bnctest.... hand a sec!', [site.Name]);
  end;

  if site.GetSw <> sswGlftpd then
  begin
    irc_addtext(Netname, Channel, 'This command is currently only for GrayLine FTPD%ss.', [Chr(39)]);
    exit;
  end;

  try
    ss := RawC(Netname, Channel, site.Name, '', 'site nukes ' + IntToStr(Count));
  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, '<c4>[Exception]</c> in IrcShowSiteNukes; %s', [E.Message]);
      Exit;
    end;
  end;

  r := TRegexpr.Create;
  //  r.ModifierS := False;
  //  r.ModifierG := False;
  try
    r.Expression := 'foo nukes';
    r.ModifierI := True;

    if r.Exec(ss) then
    begin
      irc_addtext(Netname, Channel, 'Sorry not compatible with tur-nukes');
      Result := False;
    end
    else
    begin
      r.Expression := '200- ';
      ss := r.Replace(ss, '', False);

      r.Expression := Format(
        '\|\s*%s\s*\|\s*(\d+)[xX]\s*([\d,.]+[Mm]?)\s*\|(.*?)\|[\r\n\s]+.*?\|\s*Age\:(.*?)\|\s*Dir\:(.*?)\s*\|',
        [site.UserName]);

      if not r.Exec(ss) then
        irc_addtext(Netname, Channel, 'No Nukes found, good boy!')
      else
        repeat
          irc_addtext(Netname, Channel, '%s x%s for: %s (%sM) %s ago.',
            [Trim(r.Match[5]), Trim(r.Match[1]), Trim(r.Match[3]), Trim(r.Match[2]), Trim(r.Match[4])]);
        until not r.ExecNext;

      Result := True;
    end;

  finally
    r.Free;
  end;

end;

function IrcCatchMod(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  index, sitename, nn, channelname, botnicks, event, words, section: AnsiString;
begin
  Result := False;
  index := UpperCase(SubString(params, ' ', 1));
  sitename := UpperCase(SubString(params, ' ', 2));
  nn := UpperCase(SubString(params, ' ', 3));
  channelname := SubString(params, ' ', 4);
  botnicks := SubString(params, ' ', 5);
  event := UpperCase(SubString(params, ' ', 6));
  words := SubString(params, ' ', 7);
  section := SubString(params, ' ', 8);

  if ((index = '') or (StrToIntDef(index, -1) = -1)) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, index: ' + index);
    Exit;
  end;

  if ((event <> 'PRE') and (event <> 'COMPLETE') and (event <> 'NEWDIR') and
    (event <> 'NUKE') and (event <> 'REQUEST')) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, unknown event: ' + event);
    exit;
  end;

  if nil = FindSiteByName(Netname, sitename) then
  begin
    irc_addtext(Netname, Channel, 'Site not found');
    exit;
  end;

  if nil = FindIrcBlowfish(nn, channelname, False) then
  begin
    irc_addtext(Netname, Channel, 'Channel not found.');
    exit;
  end;
  try
    catcherFile.Delete(StrToInt(index));
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherFile.Delete: %s',
        [E.Message]));
      Exit;
    end;
  end;

  try
    catcherfile.Insert(StrToInt(index), format('%s;%s;%s;%s;%s;%s;%s',
      [nn, channelname, botnicks, sitename, event, words, section]));
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherfile.Insert: %s',
        [E.Message]));
      Exit;
    end;
  end;

  try
    PrecatcherRebuild();
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherfile.Insert: %s',
        [E.Message]));
      Exit;

    end;
  end;

  Result := True;
end;

function IrcSetDebugverbosity(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  val: integer;
begin
  val := StrToIntDef(params, -1);
  if val = -1 then
  begin

    case config.ReadInteger('debug', 'verbosity', 0) of
      0: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      1: irc_Addtext(Netname, Channel,
          'Only Logging Errors and common Messages.');
      2: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      3: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else if (val <= 3) then
  begin
    config.WriteInteger('debug', 'verbosity', val);
    config.UpdateFile;
    case config.ReadInteger('debug', 'verbosity', 0) of
      0: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      1: irc_Addtext(Netname, Channel,
          'Only Logging Errors and common Messages.');
      2: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      3: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else
  begin
    irc_Addtext(Netname, Channel, '<c4>Syntax error</c>, unknown verbosity.');
    Result := False;
    Exit;
  end;
  Result := True;
end;

/// dOH mODz  eNDz

function IrcRebuildSlot(const Netname, Channel: AnsiString; params: AnsiString):
  boolean;
var
  sitename: AnsiString;
  s_slot: AnsiString;
  slot: integer;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  s_slot := SubString(params, ' ', 2);
  slot := StrToIntDef(s_slot, -1);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if slot < 0 then
  begin
    irc_addtext(Netname, Channel, 'Slot %s/<b>%s</b> not found.',
      [sitename, s_slot]);
    exit;
  end;

  try
    if ((site.slots[slot] = nil) or (TSiteSlot(site.slots[slot]) = nil)) then
    begin
      irc_addtext(Netname, Channel, 'Slot %s/<b>%s</b> not found.',
        [sitename, s_slot]);
      exit;
    end;
    site.slots[slot] := nil;
    site.slots[slot] := TSiteSlot.Create(site, slot);
  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, 'Exception : %s', [E.Message]);
    end;
  end;

  Result := True;
end;

function IrcRecalcFreeslots(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  sitename: AnsiString;
  site: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP'))
        then
        Continue;
      TSite(sites.Items[i]).RecalcFreeslots;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;

      for i := 0 to x.Count - 1 do
      begin
        site := FindSiteByName(Netname, x.Strings[i]);
        if site = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        site.RecalcFreeslots;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSetAutoInvite(const netname, channel: AnsiString; params: AnsiString): boolean;
var
  sitename, value: AnsiString;
  site: TSite;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  value := UpperCase(SubString(params, ' ', 2));
  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    Result := True;
    Exit;
  end;
  if value = '' then
  begin
    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite)]);
    Result := True;
    Exit;
  end;

  if ((value = '1') or (value = '0')) then
  begin

    if value = '1' then
      site.UseAutoInvite := True;
    if value = '0' then
      site.UseAutoInvite := False;
    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite)]);
    Result := True;
    Exit;
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Syntax error.');
    Result := True;
    Exit;
  end;
end;

// Testing functions

function IrcTestColors(const Netname, Channel: AnsiString; params: AnsiString): boolean;
var
  i, colorscount: integer;
  colors: AnsiString;
begin
  colorscount := 15;
  colors := '';

  for i := 0 to colorscount do
  begin
    colors := colors + Format('<c%d>c%d</c> ', [i, i, i]);
  end;

  irc_addtext(Netname, Channel, 'Color test: %s', [colors]);
  Result := True;
end;

{ TIRCCommandThread }

constructor TIRCCommandThread.Create(c: TIRCCommandHandler;
  Netname, Channel, params: AnsiString; cmd: AnsiString = '');
begin
  self.c := c;
  self.Netname := Netname;
  self.th := th;
  self.Channel := Channel;
  self.params := params;
  self.cmd := cmd;
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TIRCCommandThread.Execute;
begin
  try
    if c(Netname, Channel, params) then
    begin
      if (cmd <> 'kbadd') then
      begin
        irc_addtext(Netname, Channel, 'Ok.');
      end;
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Failed.');
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section,
        format('[EXCEPTION] TIRCCommandThread.Execute: %s (%s %s %s %s)',
        [E.Message, Netname, Channel, cmd, params]));
    end;
  end;
end;

procedure IrcCommandInit;
begin
  // genres:= TStringList.Create;
end;

procedure IrcCommandUnInit;
begin
  Debug(dpSpam, section, 'Uninit1');
  // genres.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

{   Help section Handler    }

function IrcHelpHeader(const netname, channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

function IrcHelpSeperator(const netname, channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

function IrcNope(const Netname, Channel: AnsiString; params: AnsiString): boolean;
begin
  Result := False;
end;

end.

