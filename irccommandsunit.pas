unit irccommandsunit;

interface

uses
  Classes, dirlist, irc, prebot, sitesunit;

type
  TIrcCommandHandler = function(const netname, channel: String; params: String): boolean;
  //TIrcCommandHandler = function (const netname, channel: string; params: string; nickname:string = ''): Boolean;
  TIrcCommand = record
    cmd: String;
    hnd: TIrcCommandHandler;
    minparams: integer;
    maxparams: integer;
    hlpgrp: String;
  end;

  TIRCCommandThread = class(TThread)
    c: TIRCCommandHandler;
    th: TMyIrcThread;
    netname, channel, cmd, params: String;
    constructor Create(c: TIRCCommandHandler; netname, channel, params: String; cmd: String = '');
    //constructor Create(c: TIRCCommandHandler; netname, channel, params: string; nickname:string = '');
    procedure Execute; override;
  end;

procedure IrcCommandInit;
procedure IrcCommandUninit;

function IrcNope(const netname, channel: String; params: String): boolean;
function IrcHelpHeader(const netname, channel: String; params: String): boolean;
function IrcHelpSeperator(const netname, channel: String; params: String): boolean;

function IrcHelpv2(const Netname, Channel: String; params: String): boolean;

function FindIrcCommand(cmd: String): integer; // overload;
//function FindIrcCommand(cmd: string): boolean; overload;
function IrcDie(const netname, channel: String; params: String): boolean;
function IrcHelp(const netname, channel: String; params: String): boolean;
function IrcUptime(const netname, channel: String; params: String): boolean;
function IrcRaw(const netname, channel: String; params: String): boolean;
function IrcManageUser(const netname, channel: String; params: String): boolean;
function IrcInvite(const netname, channel: String; params: String): boolean;
function IrcBnctest(const netname, channel: String; params: String): boolean;
function IrcKill(const netname, channel: String; params: String): boolean;
function IrcSites(const netname, channel: String; params: String): boolean;
function IrcSite(const netname, channel: String; params: String): boolean;
function IrcBnc(const netname, channel: String; params: String): boolean;
function IrcSetdown(const netname, channel: String; params: String): boolean;
//function IrcNope(const netname, channel: string;params: string; nickname:string = ''): Boolean;

function IrcQueue(const netname, channel: String; params: String): boolean;

function IrcMaxUpDn(const netname, channel: String; params: String): boolean;
function IrcMaxUpPerRip(const netname, channel: String; params: String): boolean;
function IrcMaxIdle(const netname, channel: String; params: String): boolean;
function IrcTimeout(const netname, channel: String; params: String): boolean;
function IrcDelsite(const netname, channel: String; params: String): boolean;
function IrcSlots(const netname, channel: String; params: String): boolean;
function IrcSlotsShow(const netname, channel: String; params: String): boolean;

function IrcAddSite(const netname, channel: String; params: String): boolean;

function IrcAddSiteInfos(const netname, channel: String; params: String): boolean;

function IrcAddBnc(const netname, channel: String; params: String): boolean;
function IrcDelBnc(const netname, channel: String; params: String): boolean;

function IrcSiteUser(const netname, channel: String; params: String): boolean;
function IrcSitePass(const netname, channel: String; params: String): boolean;

function IrcNetAddServer(const netname, channel: String; params: String): boolean;
function IrcNetDelServer(const netname, channel: String; params: String): boolean;

function IrcNetAddPerform(const netname, channel: String; params: String): boolean;
function IrcNetDelPerform(const netname, channel: String; params: String): boolean;
function IrcNetListPerform(const netname, channel: String; params: String): boolean;
function IrcNetDoPerform(const netname, channel: String; params: String): boolean;

function IrcSetdir(const netname, channel: String; params: String): boolean;
function IrcSslmethod(const netname, channel: String; params: String): boolean;
function IrcSslfxp(const netname, channel: String; params: String): boolean;
function IrcLegacyCwd(const netname, channel: String; params: String): boolean;

function IrcRankLock(const netname, channel: String; params: String): boolean;
function IrcRanks(const netname, channel: String; params: String): boolean;
function IrcRank(const netname, channel: String; params: String): boolean;
function IrcRankRecalc(const netname, channel: String; params: String): boolean;

function IrcNoannouncesite(const netname, channel: String; params: String): boolean;

function IrcSpeeds(const netname, channel: String; params: String): boolean;
function IrcSetSpeed(const netname, channel: String; params: String): boolean;
function IrcLockSpeed(const netname, channel: String; params: String): boolean;
function IrcSetRoute(const netname, channel: String; params: String; lock: boolean = False): boolean;
function IrcInroutes(const netname, channel: String; params: String): boolean;
function IrcOutroutes(const netname, channel: String; params: String): boolean;

function IrcDirlist(const netname, channel: String; params: String): boolean;
function IrcLatest(const netname, channel: String; params: String): boolean;

function IrcDelrelease(const netname, channel: String; params: String): boolean;
function IrcDelAllrelease(const netname, channel: String; params: String): boolean;

function IrcSpread(const netname, channel: String; params: String): boolean;
function IrcCStop(const netname, channel: String; params: String): boolean;

function IrcTransfer(const netname, channel: String; params: String): boolean;

function IrcStatus(const netname, channel: String; params: String): boolean;
function IrcChannels(const netname, channel: String; params: String): boolean;
function IrcChanAdd(const netname, channel: String; params: String): boolean;
function IrcSetBlowkey(const netname, channel: String; params: String): boolean;
function IrcSetChankey(const netname, channel: String; params: String): boolean;
function IrcSetChanName(const netname, channel: String; params: String): boolean;
//function IrcSetChanInvite(const netname, channel: string;params: string): Boolean;
function IrcShowNet(const netname, channel: String; params: String): boolean;
function IrcAddnet(const netname, channel: String; params: String): boolean;
function IrcModnet(const netname, channel: String; params: String): boolean;
function IrcModesNet(const netname, channel: String; params: String): boolean;
function IrcDelnet(const netname, channel: String; params: String): boolean;
function IrcDelchan(const netname, channel: String; params: String): boolean;
function IrcJump(const netname, channel: String; params: String): boolean;
function IrcSay(const netname, channel: String; params: String): boolean;

function IrcSitechan(const netname, channel: String; params: String): boolean;
function IrcPrereload(const netname, channel: String; params: String): boolean;
function IrcPrelist(const netname, channel: String; params: String): boolean;
function IrcPreadd(const netname, channel: String; params: String): boolean;
function IrcPredel(const netname, channel: String; params: String): boolean;
function IrcPreCatchtest(const netname, channel: String; params: String): boolean;
function IrcPreCatchDebug(const netname, channel: String; params: String): boolean;

function IrcRuleAdd(const netname, channel: String; params: String): boolean;
function IrcRuleIns(const netname, channel: String; params: String): boolean;
function IrcRuleMod(const netname, channel: String; params: String): boolean;
function IrcRuleDel(const netname, channel: String; params: String): boolean;
function IrcAllRuleDel(const netname, channel: String; params: String): boolean;
function IrcRuleHelp(const netname, channel: String; params: String): boolean;
function IrcRuleList(const netname, channel: String; params: String): boolean;
function IrcRules(const netname, channel: String; params: String): boolean;
function IrcRulesLoad(const netname, channel: String; params: String): boolean;
function IrcRulesReload(const netname, channel: String; params: String): boolean;

function IrcAffils(const netname, channel: String; params: String): boolean;
function IrcSetAffils(const netname, channel: String; params: String): boolean;
function IrcUsers(const netname, channel: String; params: String): boolean;
function IrcCountry(const netname, channel: String; params: String): boolean;
function IrcInfo(const netname, channel: String; params: String): boolean;
function IrcName(const netname, channel: String; params: String): boolean;
function IrcSize(const netname, channel: String; params: String): boolean;
function IrcLink(const netname, channel: String; params: String): boolean;
function IrcNotes(const netname, channel: String; params: String): boolean;
function IrcLeechers(const netname, channel: String; params: String): boolean;
function IrcTraders(const netname, channel: String; params: String): boolean;
function IrcUserslots(const netname, channel: String; params: String): boolean;
function IrcFreeslots(const netname, channel: String; params: String): boolean;
function IrcFindAffil(const netname, channel: String; params: String): boolean;
function IrcFindCountry(const netname, channel: String; params: String): boolean;
function IrcFindSection(const netname, channel: String; params: String): boolean;
function IrcFindUser(const netname, channel: String; params: String): boolean;
function IrcAuto(const netname, channel: String; params: String): boolean;
//function IrcCrawler(const netname, channel: String; params: String): boolean;
//function IrcConfirmerAnnounce(const netname, channel: String; params: String): boolean;
//function IrcCrawl(const netname, channel: String; params: String): boolean;
function IrcAutoLogin(const netname, channel: String; params: String): boolean;
function IrcAutoBncTest(const netname, channel: String; params: String): boolean;
function IrcAutoRules(const netname, channel: String; params: String): boolean;
function IrcAutoNuke(const netname, channel: String; params: String): boolean;
function IrcAutoDirlist(const netname, channel: String; params: String): boolean;
//function IrcAutoCrawler(const netname, channel: String; params: String): boolean;
function IrcAutoIndex(const netname, channel: String; params: String): boolean;
function IrcKbShow(const netname, channel: String; params: String): boolean;
function IrcKbList(const netname, channel: String; params: String): boolean;
function IrcKbExtra(const netname, channel: String; params: String): boolean;
function IrcKbAdd(const netname, channel: String; params: String): boolean;

function IrcSkipReload(const netname, channel: String; params: String): boolean;

function IrcNoHelp(const netname, channel: String; params: String): boolean;
function IrcIdent(const netname, channel: String; params: String): boolean;
function IrcNoSocks5(const netname, channel: String; params: String): boolean;

function IrcLookup(const netname, channel: String; params: String): boolean;

function IrcKnowngroups(const netname, channel: String; params: String): boolean;

function IrcShowWindow(const netname, channel: String; params: String): boolean;
function IrcShowWindows(const netname, channel: String; params: String): boolean;
function IrcDelWindow(const netname, channel: String; params: String): boolean;
function IrcRepaint(const netname, channel: String; params: String): boolean;
function IrcIrcNames(const netname, channel: String; params: String): boolean;

function DirlistB(const netname, channel: String; sitename, dir: String; SpeedTest:
  boolean = False): TDirList;
procedure RawB(const netname, channel: String; sitename, dir, command: String;
  AnnounceSitename: boolean = False);
function RawC(const Netname, Channel: String; sitename, dir, command: String;
  AnnounceSitename: boolean = False): String;

function IrcNuke(const netname, channel: String; params: String): boolean;
function IrcUnnuke(const netname, channel: String; params: String): boolean;

function IrcOper(const netname, channel: String; params: String): boolean;

function IrcNews(const netname, channel: String; params: String): boolean;
function IrcNewsAdd(const netname, channel: String; params: String): boolean;
function IrcNewsDel(const netname, channel: String; params: String): boolean;
function IrcNewsCategories(const netname, channel: String; params: String): boolean;

function IrcSpeedStats(const netname, channel: String; params: String): boolean;
function IrcSpeedRecalc(const netname, channel: String; params: String): boolean;

function IrcSpeedTestLocal(const netname, channel: String; params: String): boolean;
function IrcSpeedTestCleanup(const netname, channel: String; params: String): boolean;
function IrcSpeedTestIn(const netname, channel: String; params: String): boolean;
function IrcSpeedTestOut(const netname, channel: String; params: String): boolean;

function IrcIndexStat(const netname, channel: String; params: String): boolean;
function IrcIndexQuery(const netname, channel: String; params: String): boolean;
function IrcIndexDropSection(const netname, channel: String; params: String): boolean;

//function IrcSetSpeedtestToPredir(const netname, channel: string;params: string): Boolean;

function IrcStatSites(const netname, channel: String; params: String): boolean;
function IrcStatSitesByGroup(const netname, channel: String; params: String): boolean;
function IrcStatSitesByUser(const netname, channel: String; params: String): boolean;
function IrcStatRaces(const netname, channel: String; params: String): boolean;

function IrcStatGroups(const netname, channel: String; params: String): boolean;
function IrcStatGroupsBySite(const netname, channel: String; params: String): boolean;

function IrcStatUsers(const netname, channel: String; params: String): boolean;
function IrcStatUsersByGroup(const netname, channel: String; params: String): boolean;
function IrcStatUsersBySite(const netname, channel: String; params: String): boolean;
function IrcStatUsersByGroupBySite(const netname, channel: String; params: String):
  boolean;

function IrcDelayLeech(const netname, channel: String; params: String): boolean;
function IrcDelayUpload(const netname, channel: String; params: String): boolean;

function IrcTweak(const netname, channel: String; params: String): boolean;

function IrcCatchMod(const netname, channel: String; params: String): boolean;

function IrcShowAllRules(const netname, channel: String; params: String): boolean;
function IrcKillAll(const netname, channel: String; params: String): boolean;
function IrcNetNoSocks5(const netname, channel: String; params: String): boolean;
function IrcSetMYIrcNick(const netname, channel: String; params: String): boolean;
function IrcInviteMyIRCNICK(const netname, channel: String; params: String): boolean;
//function IrcNetBotNick(const netname, channel: string;params: string): Boolean;

//Site_stuff
function IrcNoLoginMSG(const netname, channel: String; params: String): boolean;
function IrcUseForNFOdownload(const Netname, Channel: String; params: String): boolean;
function IrcSkipBeingUploadedFiles(const Netname, Channel: String; params: String): boolean;
function IrcSiteUserFetch(const Netname, Channel: String; params: String): boolean;

//function IrcCustomDelrelease(const netname, channel: string;params: string): Boolean;

function IrcCreateBackup(const netname, channel: String; params: String): boolean;

function IrcLanguageBaseReload(const netname, channel: String; params: String):
  boolean;
function IrcTestLanguageBase(const netname, channel: String; params: String): boolean;

function IrcSpamConfig(const netname, channel: String; params: String): boolean;

function IrcSLFTPConfig(const netname, channel: String; params: String): boolean;

function IrcRuleCopy(const netname, channel: String; params: String): boolean;

(* PreURLs *)
function IrcPreURLAdd(const netname, channel: String; params: String): boolean;
function IrcPreURLDel(const netname, channel: String; params: String): boolean;
function IrcPreURLMod(const netname, channel: String; params: String): boolean;
function IrcPreURLList(const netname, channel: String; params: String): boolean;

function IrcSetupOffset(const netname, channel: String; params: String): boolean;
function IrcSetupPretimeMode(const netname, channel: String; params: String): boolean;
function IrcSetupPretimeMode2(const netname, channel: String; params: String): boolean;
function IrcSetupADDPreMode(const netname, channel: String; params: String): boolean;

function IrcFindPretime(const netname, channel: String; params: String): boolean;

//function IrcReloadoffset(const netname, channel: string;params: string): Boolean;
function Irctestoffset(const netname, channel: String; params: String): boolean;

//function IrcSetMYSQLData(const netname, channel: string;params: string): Boolean;
//function IrcViewMYSQLValue(const netname, channel: string;params: string): Boolean;
//function IrcTweakMYSQL(const netname, channel: string;params: string): Boolean;
//function IrcMYSQLStatus(const netname, channel: string;params: string): Boolean;
(* SOCKS5 *)
function IrcAddSocks5(const netname, channel: String; params: String): boolean;
function IrcDelSocks5(const netname, channel: String; params: String): boolean;
function IrcDisplaySocks5(const netname, channel: String; params: String): boolean;
function IrcTweakSocks5(const netname, channel: String; params: String): boolean;
function IrcSetSocks5(const netname, channel: String; params: String): boolean;
function IrcRehashSocks5(const netname, channel: String; params: String): boolean;

function IrcDisplayMappings(const netname, channel: String; params: String): boolean;

function IrcReloadGlobalSkipGrouplist(const netname, channel: String; params: String): boolean;

function IrcShowCredits(const netname, channel: String; params: String): boolean;
procedure ShowCredits(const netname, channel, siteName: String); overload;
procedure ShowCredits(const netname, channel: String; s : Tsite); overload;

function IrcShowAppStatus(const netname, channel: String; params: String): boolean;

function Ircaddknowngroup(const netname, channel: String; params: String): boolean;

function IrcChanSetSitename(const netname, channel: String; params: String): boolean;

function IrcSetSitePermdown(const netname, channel: String; params: String): boolean;

function IrcAnnounceIMDBInfo(const netname, channel: String; params: String): boolean;

function IrcShowSiteNukes(const netname, channel: String; params: String): boolean;

function IrcSetAutoInvite(const netname, channel: String; params: String): boolean;

//function IrcMain_Restart(const netname, channel: string;params: string): Boolean;

function IrcDelPart(const netname, channel: String; params: String): boolean;
function IrcFakeReload(const netname, channel: String; params: String): boolean;

function IrcSetPretime(const netname, channel: String; params: String): boolean;

function IrcRebuildSlot(const netname, channel: String; params: String): boolean;
function IrcRecalcFreeslots(const netname, channel: String; params: String): boolean;

function IrcLastLog(const Netname, Channel: String; params: String): boolean;
function IrcSetDebugverbosity(const Netname, Channel: String; params: String): boolean;

{        Sections                   }
function IrcSections(const netname, channel: String; params: String): boolean;
{        Test functions             }
function IrcTestColors(const Netname, Channel: String; params: String): boolean;

{ TVInfo aka TTVRelease aka TVMaze  }
function IrcAnnounceTVInfo(const netname, channel: String; params: String): boolean;
function IrcAddTVMazeToDb(const netname, channel: String; params: String): boolean;
function IrcUpdateTVMazeInfo(const Netname, Channel: String; params: String): boolean;
function IrcDelTheTVDbInfo(const Netname, Channel: String; params: String): boolean;
function IrcSetTheTVDbID(const Netname, Channel: String; params: String): boolean;
function IrcSetTVRageID(const netname, channel: String; params: String): boolean;

const
  helpCommands: array[0..22] of String = ('general', 'site', 'auto', 'route',
    'rank', 'speed', 'work', 'rip', 'stats', 'slots', 'misc', 'news', 'irc',
    'rules', 'indexer', 'info', 'reload', 'socks5', 'pretime', 'imdb', 'tv', 'test',
    'section');

  irccommands: array[1..250] of TIrcCommand = (
    (cmd: 'GENERAL'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$general'),
    (cmd: 'help'; hnd: IrcHelp; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'die'; hnd: IrcDie; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'uptime'; hnd: IrcUptime; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'status'; hnd: IrcShowAppStatus; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'nhelp'; hnd: IrcHelpv2; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'queue'; hnd: IrcQueue; minparams: 0; maxparams: 2; hlpgrp: 'general'),
    (cmd: 'lastlog'; hnd: IrcLastLog; minparams: 0; maxparams: 1; hlpgrp: 'general'),
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
    (cmd: 'maxupperrip'; hnd: IrcMaxUpPerRip; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    //(cmd: 'setspeedtesttopredir'; hnd: IrcSetSpeedtesttoPredir; minparams: 0; maxparams: 1; hlpgrp:'site'),
    (cmd: 'maxidle'; hnd: IrcMaxIdle; minparams: 2; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'timeout'; hnd: IrcTimeout; minparams: 3; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'sslfxp'; hnd: IrcSslfxp; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'sslmethod'; hnd: IrcSslmethod; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'legacycwd'; hnd: IrcLegacycwd; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'nologinmsg'; hnd: IrcNoLoginMSG; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'skipinc'; hnd: IrcSkipBeingUploadedFiles; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'fetchuser'; hnd: IrcSiteUserFetch; minparams: 1; maxparams: 2; hlpgrp: 'site'),
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
    (cmd: 'routelock'; hnd: IrcLockspeed; minparams: 3; maxparams: -1; hlpgrp: 'route'),
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
    (cmd: 'dirlist'; hnd: IrcDirlist; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'autodirlist'; hnd: IrcAutoDirlist; minparams: 1; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'latest'; hnd: IrcLatest; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'lame'; hnd: IrcLame; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'spread'; hnd: IrcSpread; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'transfer'; hnd: IrcTransfer; minparams: 5; maxparams: 5; hlpgrp: 'work'),
    (cmd: 'stop'; hnd: IrcCStop; minparams: 1; maxparams: 1; hlpgrp: 'work'),
    (cmd: 'lookup'; hnd: IrcLookup; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'nuke'; hnd: IrcNuke; minparams: 4; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'unnuke'; hnd: IrcUnNuke; minparams: 3; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'nukes'; hnd: IrcShowSiteNukes; minparams: 1; maxparams: 2; hlpgrp: 'work'),
    (cmd: 'autonuke'; hnd: IrcAutoNuke; minparams: 1; maxparams: 2; hlpgrp: 'work'),
    (cmd: 'checkforrip'; hnd: IrcCheckForExistsRip; minparams: 1; maxparams: 1; hlpgrp: 'work'),

    (cmd: 'RIPS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$rip'),
    (cmd: 'setprecmd'; hnd: IrcPrecmd; minparams: 2; maxparams: - 1; hlpgrp: 'rip'),
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
    (cmd: 'news'; hnd: IrcNews; minparams: 0; maxparams: 2; hlpgrp: 'news'),
    (cmd: 'newsadd'; hnd: IrcNewsAdd; minparams: 2; maxparams: - 1; hlpgrp: 'news'),
    (cmd: 'newsdel'; hnd: IrcNewsDel; minparams: 1; maxparams: 2; hlpgrp: 'news'),
    (cmd: 'newscategories'; hnd: IrcNewsCategories; minparams: 0; maxparams: 0; hlpgrp: 'news'),

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
    (cmd: 'setaffils'; hnd: IrcSetAffils; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
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
    (cmd: 'setsocks5'; hnd: IrcSetSocks5; minparams: 2; maxparams: 3; hlpgrp: 'socks5'),

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

procedure IrcLineBreak(const Netname, Channel: String; const commatext: String;
  QuoteChar: Char = '"'; fronttext: String = ''; breakafter: integer = 16);

implementation

uses sltcp, SysUtils, DateUtils, Math, versioninfo, knowngroups, encinifile, speedstatsunit,
  debugunit, queueunit, tasksunit, mystrings, notify, taskraw, tasklogin,
  indexer, taskdirlist, taskdel, tasklame, taskcwd, taskrace, pazo, configunit, console,
  slconsole, uintlist, nuke, kb, helper, ircblowfish, precatcher, rulesunit, mainthread,
  taskspeedtest, taskfilesize, statsunit, skiplists, slssl, ranksunit, taskautocrawler,
  RegExpr, mslproxys, http, strUtils, inifiles, rcmdline,
  mysqlutilunit, backupunit, sllanguagebase, irccolorunit, mrdohutils, fake, taskpretime,
  dbaddpre, dbaddurl, dbaddnfo, dbaddimdb, dbtvinfo, globalskipunit, xmlwrapper,
  tasktvinfolookup, uLkJSON, TypInfo, globals, news {$IFDEF FPC}, process {$ENDIF}, CompVers, IdGlobal;

{$I common.inc}

const
  section = 'irccommands';

procedure IrcLineBreak(const Netname, Channel: String; const commatext: String; QuoteChar: Char = '"'; fronttext: String = ''; breakafter: integer = 16);
var
  xs: TStringList;
  i, ii: integer;
  s, ss: String;
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

function FindIrcCommand(cmd: String): integer;
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

function IrcSections(const Netname, Channel: String; params: String): boolean;
var
  ss, sitename, secs: String;
  s: TSite;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  secs := UpperCase(mystrings.RightStr(params, length(sitename) + 1));

  if ((sitename = '') and (secs = '')) then
  begin
    IrcLineBreak(Netname, Channel, kb_sections.commatext, AnsiChar('"'), '<b>Global Sections</b>: ');
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
    Result := true;
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

function IrcSetdir(const Netname, Channel: String; params: String): boolean;
var
  sitename, section: String;
  s: TSite;
  dir: String;
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
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b></c>, use REQUEST as section name.');
    exit;
  end;

  if section = 'SPEEDTESTS' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b></c>, use SPEEDTEST as section name.');
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

  // if empty, it can be removed
  if dir = '' then
  begin
    s.SetSections(section, True);
    s.sectiondir[section] := '';
    s.sectionpretime[section] := -10;
    s.SetRankLock(section, 0);
    RulesRemove(sitename, section);
    RemoveRanks(sitename, section);
    RemoveStats(sitename, section);
    RemoveSpeedStats(sitename, section);
    irc_addtext(Netname, Channel, 'Section <b>%s</b> removed from site <b>%s</b>', [section, s.Name]);
  end
  else
  begin
    s.sectiondir[section] := dir;
    s.SetSections(section, False);
    irc_addtext(Netname, Channel, 'Section <b>%s</b> dir on site <b>%s</b> set to <b>%s</b>', [section, s.Name, dir]);
  end;

  Result := True;
end;

procedure Outroutes(const sitename, Netname, Channel: String);
var
  x: TStringList;
  ii, i: integer;
  ss: String;
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


procedure OutroutesB(const Netname, Channel: String; const sitename: String);
var
  x: TStringList;
  ii, i: integer;
  ss: String;
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

procedure Inroutes(const sitename, Netname, Channel: String);
var
  x: TStringList;
  i: integer;
  ss: String;
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

procedure InroutesB(const Netname, Channel: String; const sitename: String);
var
  x: TStringList;
  ii, i: integer;
  ss: String;
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

function IrcSpeeds(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcSetSpeed(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;

  if (IrcSetRoute(Netname, Channel, params, False)) then
    Result := True;
end;


function IrcLockSpeed(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;

  if (IrcSetRoute(Netname, Channel, params, True)) then
    Result := True;
end;

function IrcSetRoute(const Netname, Channel: String; params: String; lock: boolean = False): boolean;
var
  source, dest, admin_site: String;
  rcmd: TCommandLineReader;
  c1, c2, sw1, sw2: String;
  i,j: integer;
  DoIt: Boolean;
  backtext: String;
  apply, back: Boolean;
  source_sites, dest_sites: TStringList;
  site: TSite;
  speed: integer;

begin
  Result := False;

  // Parse the params
  rcmd := TCommandLineReader.create();

  try
    try
      rcmd.allowDOSStyle := True;
      rcmd.automaticalShowError := False;
      rcmd.declareString('c1','','');
      rcmd.declareString('c2','','');
      rcmd.declareString('sw1','','');
      rcmd.declareString('sw2','','');
      rcmd.declareFlag('apply','Apply changes');
      rcmd.addAbbreviation('a', 'apply');
      rcmd.declareFlag('back','Also add back route');

      rcmd.addAbbreviation('b', 'back');
      rcmd.parse(params);

    except
      on e: Exception do
      begin
        irc_addtext(Netname, Channel, '<c4><b>%s</b></c>', [e.Message]);
        Debug(dpError, section, '[EXCEPTION] IrcSetSpeed(rcmd.parse): %s', [e.Message]);
        exit;
      end;
    end;

    source := AnsiUpperCase(rcmd.readNamelessString()[0]);
    dest := AnsiUpperCase(rcmd.readNamelessString()[1]);
    speed := StrToIntDef(rcmd.readNamelessString()[2], -1);
    c1 := stringreplace(rcmd.readString('c1'), '.', '', [rfReplaceAll]);
    c2 := stringreplace(rcmd.readString('c2'), '.', '', [rfReplaceAll]);
    sw1 := rcmd.readString('sw1');
    sw2 := rcmd.readString('sw2');
    apply := rcmd.readFlag('apply');
    back := rcmd.readFlag('back');

    // debug shit to be removed
    //irc_addtext(Netname, Channel, '[debug] source: %s | dest: %s | speed :%d | c1: %s | c2: %s | sw1: %s | sw2: %s | backroute: %s | apply: %s',
    //  [source, dest, speed, c1, c2, sw1, sw2, BoolToStr(back), BoolToStr(apply)]);

  finally
    rcmd.Free;
  end;

  admin_site := getAdminSiteName;

  // basic sanity check for first args
  if (source = admin_site) or (dest = admin_site) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>You can not use admin site with this function</b>.</c>');
    exit;
  end;
  if (speed > 9) or (speed < 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Third argument must be a speed between 0 and 9</b>.</c>');
    exit;
  end;

  // additional checks for optional filters
  if (c1 <> '') and (AnsiIndexText(c1, CountryCodes) = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Sorry bro, %s is not a valid country code.</b>. Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements</c>', [c1]);
    exit;
  end;
  if (c2 <> '') and (AnsiIndexText(c2, CountryCodes) = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Sorry bro, %s is not a valid country code.</b>. Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements</c>', [c2]);
    exit;
  end;
  if (sw1 <> '') and (StringToSiteSoftWare(sw1) = sswUnknown) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Hey dude, %s is not a valid ftp server software.</b>. Must be one of GLFTPD, IOFTPD, DRFTPD.</c>', [sw1]);
    exit;
  end;
  if (sw2 <> '') and (StringToSiteSoftWare(sw2) = sswUnknown) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Hey dude, %s is not a valid ftp server software.</b>. Must be one of GLFTPD, IOFTPD, DRFTPD.</c>', [sw2]);
    exit;
  end;

  // by default do not apply changes if we are using the overpowered wildcards stuff
  if ((source = '*') or (dest = '*')) and (not Apply)then
    doit := False
  else
    doit := True;

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

        // Admin site
        if site.Name = admin_site then
          Continue;

        // filters handling
        if (c1 <> '') and (AnsiLowerCase(site.Country) <> ('.' + AnsiLowerCase(c1))) then
          Continue;
        if (sw1 <> '') and (StringToSiteSoftWare(sw1) <> site.sw) then begin
          Continue;
        end;
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

        // filters handling
        if (c2 <> '') and (AnsiLowerCase(site.Country) <> ('.' + AnsiLowerCase(c2))) then
          Continue;
        if (sw2 <> '') and (StringToSiteSoftWare(sw2) <> site.sw) then
          Continue;

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

        // announce what we're doing
        backtext := '';
        if back then
          backtext := ' (and backroute)';
        if speed > 0 then
        begin
          if lock then
            irc_addtext(Netname, Channel, 'Routelock from <b>%s</b> to <b>%s</b> set to %d%s', [source_sites[i], dest_sites[j], speed, backtext])
          else
            irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> set to %d%s', [source_sites[i], dest_sites[j], speed, backtext]);
        end
        else
        begin
          if lock then
            irc_addtext(Netname, Channel, 'Routelock from <b>%s</b> to <b>%s</b> removed%s', [source_sites[i], dest_sites[j], backtext])
          else
            irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> removed%s', [source_sites[i], dest_sites[j], backtext]);
        end;

        // When using wildcards apply changes only if -apply has been specified (to avoid unwanted changes)
        if DoIt then
        begin
          if speed > 0 then
          begin
            // normal route
            sitesdat.WriteInteger('speed-from-' + source_sites[i], dest_sites[j], speed);
            sitesdat.WriteInteger('speed-to-' + dest_sites[j], source_sites[i], speed);
            if back then
            begin
              sitesdat.WriteInteger('speed-from-' + dest_sites[j], source_sites[i], speed);
              sitesdat.WriteInteger('speed-to-' + source_sites[i], dest_sites[j], speed);
            end;

            // locked route
            if lock then
            begin
              sitesdat.WriteInteger('speedlock-from-' + source_sites[i], dest_sites[j], speed);
              sitesdat.WriteInteger('speedlock-to-' + dest_sites[j], source_sites[i], speed);
              if back then
              begin
                sitesdat.WriteInteger('speedlock-from-' + dest_sites[j], source_sites[i], speed);
                sitesdat.WriteInteger('speedlock-to-' + source_sites[i], dest_sites[j], speed);
              end;
            end;
          end
          else
          begin
            if not lock then
            begin
              // normal route
              sitesdat.DeleteKey('speed-from-' + source_sites[i], dest_sites[j]);
              sitesdat.DeleteKey('speed-to-' + dest_sites[j], source_sites[i]);
              if back then
              begin
                sitesdat.DeleteKey('speed-from-' + dest_sites[j], source_sites[i]);
                sitesdat.DeleteKey('speed-to-' + source_sites[i], dest_sites[j]);
              end;
            end;

            // locked route
            if lock then
            begin
              sitesdat.DeleteKey('speedlock-from-' + source_sites[i], dest_sites[j]);
              sitesdat.DeleteKey('speedlock-to-' + dest_sites[j], source_sites[i]);
              if back then
              begin
                sitesdat.DeleteKey('speedlock-from-' + dest_sites[j], source_sites[i]);
                sitesdat.DeleteKey('speedlock-to-' + source_sites[i], dest_sites[j]);
              end;
            end;
          end;
        end;
      end;
    end;

    if not DoIt then
      irc_addtext(Netname, Channel, 'Route were not really added. Check if you are satisfied and add -apply to the command.');

  finally
    FreeAndNil(source_sites);
    FreeAndNil(dest_sites);
  end;

  Result := True;
end;

function IrcInroutes(const Netname, Channel: String; params: String): boolean;
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

function IrcOutroutes(const Netname, Channel: String; params: String): boolean;
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

function DirlistB(const Netname, Channel: String; sitename, dir: String;
  SpeedTest: boolean = False): TDirList;
var
  r: TDirlistTask;
  tn: TTaskNotify;
  s: String;
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

function IrcDirlist(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, sectiondir, dir: String;
  d: TDirList;
  de: TDirListEntry;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  dir := SubString(params, ' ', 3);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  sectiondir := s.sectiondir[section];

  if (sectiondir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.', [sitename, section]);
    exit;
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  sectiondir := todaycsere(sectiondir);

  d := DirlistB(Netname, Channel, sitename, MyIncludeTrailingSlash(sectiondir) + dir);
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

function IrcLatest(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, sectiondir: String;
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

  sectiondir := s.sectiondir[section];
  if (sectiondir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  if (amount <= 0) then
  begin
    irc_addtext(Netname, Channel, 'Invalid amount');
    exit;
  end;

  sectiondir := todaycsere(sectiondir);

  d := DirlistB(Netname, Channel, sitename, sectiondir);
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

function IrcDelrelease(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  rlsname, sitename, section, predir, dir: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

function IrcDelallrelease(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  predir, section, sitename, dir: String;
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

procedure Routeable(honnan: String; y: TStringList);
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
  Result := CompareValue(StrToIntDef(List.ValueFromIndex[Index2], 0), StrToIntDef(List.ValueFromIndex[Index1], 0));
end;

function IrcSpread(const Netname, Channel: String; params: String): boolean;
var
  sp, s: TSite;
  ps: TPazoSite;
  ssite, predir, sitename, section, dir: String;
  lastAnn: TDateTime;
  ann: integer;
  pazo_id: integer;
  p: TPazo;
  y: TStringList;
  sdone, ssss, ss, si, sj, sss: String;
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
          if ps.Name = getAdminSiteName then
            Continue;

          if ps.dirlist = nil then
            irc_addtext(Netname, Channel,
              '<c7>DEBUG<b></c></b>: %s have no dirlist.', [ps.Name]);

          if ((ps <> nil) and (ps.dirlist <> nil)) then
          begin
            sj := IntToStr(ps.dirlist.RacedByMe);
            sdone := IntToStr(ps.dirlist.Done);

            dd := ps.dirlist.SizeRacedByMe;

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

function IrcTransfer(const Netname, Channel: String; params: String): boolean;
var
  srcsitename, dstsitename, srcdir, dstdir, rlsname, ftpsrcdir, ftpdstdir: String;
  srcsite, dstsite: TSite;
  p: TPazo;
  ps_src, ps_dst: TPazoSite;
  rc: TCRelease;
  rls: TRelease;
  //  pazo_id: integer;
  pd: TPazoDirlistTask;
  lastAnn: TDateTime;

  ann: integer;
  i, j, k: String;
begin
  Result := False;

  srcsitename := UpperCase(SubString(params, ' ', 1));
  dstsitename := UpperCase(SubString(params, ' ', 2));
  srcdir := SubString(params, ' ', 3);
  dstdir := SubString(params, ' ', 4);
  rlsname := SubString(params, ' ', 5);

  if ((srcsitename = '') or (dstsitename = '') or (srcdir = '') or (dstdir = '') or (rlsname = '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  // Check if source site is valid
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

  // Check if destination site is valid
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

  // Decide whether the supplied source dir is a direct path or a section
  if ((1 = AnsiPos('/', srcdir)) or (length(srcdir) = LastDelimiter('/', srcdir))) then
  begin
    ftpsrcdir := srcdir;
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a path.</c>', [srcdir]);
  end
  else
  begin
    srcdir := UpperCase(srcdir);
    ftpsrcdir := srcsite.sectiondir[srcdir];
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a slftp section.</c>', [srcdir]);
  end;

  // Decide whether the supplied destination dir is a direct path or a section
  if ((1 = AnsiPos('/', dstdir)) or (length(dstdir) = LastDelimiter('/', dstdir))) then
  begin
    ftpdstdir := dstdir;
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a path.</c>', [dstdir]);
  end
  else
  begin
    dstdir := UpperCase(dstdir);
    ftpdstdir := dstsite.sectiondir[dstdir];
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a slftp section.</c>', [dstdir]);
  end;

  // Check if source or destination dir is a SECTION but dir is not set
  if (ftpsrcdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.',
      [srcsitename, srcdir]);
    exit;
  end;
  if (ftpdstdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.',
      [dstsitename, dstdir]);
    exit;
  end;

  // The fun begins
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

  pd := TPazoDirlistTask.Create(Netname, Channel, ps_src.Name, p, '', False, False);
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

function IrcCStop(const Netname, Channel: String; params: String): boolean;
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

function IrcSslmethod(const Netname, Channel: String; params: String): boolean;
var
  method, sitename: String;
  s: TSite;
  i: integer;
  x: TStringList;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  method := SubString(params, ' ', 2);
  i := StrToIntDef(method, -1);

  if ((method <> '') and ((i < 0) or (i > Integer(High(TSSLMethods))))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</c></b>: %s is not valid SSL method.', [method]);
    Result := True;
    Exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;
      if s.PermDown then
        Continue;
      if method <> '' then
        s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));

      irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename, sslMethodToSTring(s)]);
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

        if method <> '' then
          s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));

        irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename, sslMethodToSTring(s)]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSslfxp(const Netname, Channel: String; params: String): boolean;
var
  s: String;
  sname: String;
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
      if (site.Name = getAdminSiteName) then
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

function IrcLegacycwd(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcRank(const Netname, Channel: String; params: String): boolean;
var
  sitename, section: String;
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

function IrcRanks(const Netname, Channel: String; params: String): boolean;
var
  section: String;
  i, j: integer;
  s: TSite;
  x: TStringList;
  ss: String;
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

function IrcRankLock(const Netname, Channel: String; params: String): boolean;
var
  sitename, section: String;
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

function IrcNoannouncesite(const Netname, Channel: String; params: String):
  boolean;
var
  sitename: String;
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

function IrcAddSite(const Netname, Channel: String; params: String): boolean;
var
  sitename, username, password: String;
  s: TSite;
  bnc: String;
  bnchost: String;
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

function IrcAddBnc(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  s: TSite;
  aktbnc, bnc: String;
  bnchost: String;
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

function IrcSiteUser(const Netname, Channel: String; params: String): boolean;
var
  username, sname: String;
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
  if s.Name = getAdminSiteName then
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

function IrcSitePass(const Netname, Channel: String; params: String): boolean;
var
  password, sname: String;
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
  if s.Name = getAdminSiteName then
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

function IrcNetAddServer(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  aktbnc, bnc: String;
  bnchost: String;
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

function IrcNetDelServer(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  bnc: String;
  aktbnchost, bnchost: String;
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

function IrcNetAddPerform(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  aktperform, Perform: String;
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

function IrcNetDelPerform(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  aktperform: integer;
  Perform: String;
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

function IrcNetListPerform(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  aktperform: String;
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

function IrcNetDoPerform(const Netname, Channel: String; params: String):
  boolean;
var
  nn: String;
  nnth: TMyIrcThread;
  aktperform: String;
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

function IrcDelBnc(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  s: TSite;
  bnc: String;
  aktbnchost, bnchost: String;
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

function IrcMaxUpDn(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

function IrcMaxUpPerRip(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  s: TSite;
  upperrip: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  upperrip := StrToIntDef(SubString(params, ' ', 2), -1);

  if (upperrip < -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      if (upperrip = -1) then
        irc_addtext(Netname, Channel, 'Site <b>%s</b> max. up per rip value: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).MaxUpPerRip])
      else
        TSite(sites.Items[i]).MaxUpPerRip := upperrip;
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
        if s.PermDown then
        Continue;

        if (upperrip = -1) then
          irc_addtext(Netname, Channel, 'Site <b>%s</b> max. up per rip value: %d', [s.Name, s.MaxUpPerRip])
        else
          s.MaxUpPerRip := upperrip;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcMaxIdle(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

function IrcTimeout(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

function IrcDelsite(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
  finally
    sitesdat.UpdateFile;
  end;

  Result := True;
end;

function IrcSlotsShow(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcSlots(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;
      if s.PermDown then
        Continue;

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
    ss := TStringList.Create;
    try
      ss.commatext := sitename;

      for i := 0 to ss.Count - 1 do
      begin
        s := FindSiteByName(Netname, ss.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Strings[i]]);
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
    finally
      ss.Free;
    end;
  end;

  Result := True;
end;

function IrcQueue(const Netname, Channel: String; params: String): boolean;
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

function RawC(const Netname, Channel: String; sitename, dir, command: String;
  AnnounceSitename: boolean = False): String;
var
  r: TRawTask;
  tn: TTaskNotify;
  i: integer;
  ss: String;

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

procedure RawB(const Netname, Channel: String; sitename, dir, command: String;
  AnnounceSitename: boolean = False);
var
  r: TRawTask;
  tn: TTaskNotify;
  i: integer;
  ss: String;

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

function IrcInvite(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcRaw(const Netname, Channel: String; params: String): boolean;
var
  command, sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcManageUser(const Netname, Channel: String; params: String): boolean;
var
  command, username: String;
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

function Bnctest(const Netname, Channel: String; s: TSite; tn: TTaskNotify; kill: boolean = False): boolean;
var
  l: TLoginTask;
begin
  if s.Name <> getAdminSiteName then
  begin
    l := TLoginTask.Create(Netname, Channel, s.Name, kill, False);
    if tn <> nil then
      tn.tasks.Add(l);

    l.startat := GiveSiteLastStart;
    AddTask(l);
  end;

  Result := True;
end;

function IrcKill(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcBnctest(const Netname, Channel: String; params: String): boolean;
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
  try
    x.Delimiter := ' ';
    x.DelimitedText := UpperCase(params);

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
        if (s.Name = getAdminSiteName) then
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
        if (s.Name = getAdminSiteName) then
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

  finally
    x.Free;
  end;

  if added then
    QueueFire;

  if added then
    tn.event.WaitFor($FFFFFFFF);

  if (db > 1) then
    IrcSites(Netname, Channel, 'IrcBnctest');

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
  if s.AutoRulesStatus <> 0 then
    s.AutoRules;
  // if s.RCString('autologin','-1') <> '-1' then
  if s.RCInteger('autobnctest', 0) <> 0 then
    s.AutoBnctest;

  RemoveTN(tn);

  Result := True;
end;

function IrcShownet(const Netname, Channel: String; params: String): boolean;
var
  nn, host: String;
  x: TStringList;
  i: integer;
  trigger: String;
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

function IrcAddnet(const Netname, Channel: String; params: String): boolean;
var
  nn, host, password, user, ident, nick: String;
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

function IrcModesNet(const netname, channel: String; params: String): boolean;
var
  mode, n_modes, nn: String;
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

function IrcModnet(const Netname, Channel: String; params: String): boolean;
var
  nn, password: String;
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

function IrcDelnet(const Netname, Channel: String; params: String): boolean;
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

function IrcJump(const Netname, Channel: String; params: String): boolean;
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

function IrcStatus(const Netname, Channel: String; params: String): boolean;
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

function IrcChannels(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  b: TIrcBlowkey;
  nn: String;
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

function IrcSay(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel, tosay: String;
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

function Check_For_Vailed_Chanrole(Name: String): boolean;
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

function IrcSetChanName(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel, Names: String;
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
    irc_addtext(Netname, Channel, '<c4><b>ERROR</c>:</b> Cant find network %s', [nn]);
    exit;
  end;

  b := FindIrcBlowfish(nn, blowchannel, False);
  if b = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find Channel %s', [blowchannel]);
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
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c>:</b> %s is not a valid chanrole.', [y.Strings[i]]);
        Result := False;
        exit;
      end;

    b := FindIrcBlowfish(nn, blowchannel, False);
    if b <> nil then
    begin
      if Names = '' then
      begin
        irc_addtext_b(Netname, Channel, format('Channel name(s): %s', [trim(b.Names)]));
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
      irc_addtext_b(Netname, Channel, format('Channel %s@%s not found.', [blowchannel, nn]));

  finally
    y.Free;
  end;

  Result := True;
end;

function IrcSetChankey(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel, key: String;
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
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcDelchan(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel: String;
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
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcSetBlowkey(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel, key: String;
  b: TIrcBlowkey;
  ircth: TMyIrcThread;
  cbc: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  key := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);
  cbc := False;

  // for CBC, key must start with 'cbc:' for setup compatibility with other software
  if {$IFDEF UNICODE}StartsText{$ELSE}AnsiStartsText{$ENDIF}('cbc:',key) then
  begin
   Delete(key, 1, 4);
   cbc := True;
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
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcChanAdd(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel: String;
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
    irc_addtext_b(Netname, Channel, format('Channel %s@%s is already added', [blowchannel, Netname]));

  Result := True;
end;

function IrcSitechan(const Netname, Channel: String; params: String): boolean;
var
  sitename, nn: String;
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

function IrcRuleAdd(const Netname, Channel: String; params: String): boolean;
var
  r: TRule;
  sitename, rule, section, error: String;
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

function IrcRuleIns(const Netname, Channel: String; params: String): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, error: String;
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

function IrcRuleMod(const Netname, Channel: String; params: String): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, error: String;
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

function IrcRuleDel(const Netname, Channel: String; params: String): boolean;
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

function IrcRuleCopy(const Netname, Channel: String; params: String): boolean;
var
  rr, r: TRule;
  rule, error, src_s, dst_s, src_section: String;
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

function IrcRuleHelp(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s, ss: String;
begin
  Result := False;

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
        irc_addtext(Netname, Channel, '<b>Accepted ops:</b> ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      break;
    end;
  end;

  Result := True;
end;

function IrcRules(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  r: TRule;
  s: TSite;
  sitename, section: String;
begin
  Result := False;
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

function IrcRuleList(const Netname, Channel: String; params: String): boolean;
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

function IrcPrereload(const Netname, Channel: String; params: String): boolean;
var
  vs: String;
begin
  vs := PrecatcherReload;
  if vs <> '' then
    irc_addtext(Netname, Channel, vs);
  Result := True;
end;

function IrcPreadd(const Netname, Channel: String; params: String): boolean;
var
  sitename, nn, channelname, botnicks, event, words, section: String;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  nn := UpperCase(SubString(params, ' ', 2));
  channelname := SubString(params, ' ', 3);
  botnicks := SubString(params, ' ', 4);
  event := UpperCase(SubString(params, ' ', 5));
  words := SubString(params, ' ', 6);
  section := SubString(params, ' ', 7);

  if ((event <> 'PRE') and (event <> 'COMPLETE') and (event <> 'NEWDIR') and (event <> 'NUKE') and (event <> 'REQUEST')) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, unknown event: ' + event);
    exit;
  end;

  if nil = FindSiteByName(Netname, sitename) then
  begin
    irc_addtext(Netname, Channel, Format('Site %s not found', [sitename]));
    exit;
  end;

  if nil = FindIrcBlowfish(nn, channelname, False) then
  begin
    irc_addtext(Netname, Channel, Format('Channel %s not found or no blowfish set.', [channelname]));
    exit;
  end;

  catcherFile.Add(format('%s;%s;%s;%s;%s;%s;%s', [nn, channelname, botnicks, sitename, event, words, section]));
  PrecatcherRebuild;

  Result := True;
end;

function IrcPredel(const Netname, Channel: String; params: String): boolean;
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

function IrcPrecatchtest(const Netname, Channel: String; params: String):
  boolean;
var
  net, chan, nick, rest: String;
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

function IrcPreCatchDebug(const Netname, Channel: String; params: String): boolean;
begin
  if params <> '' then
  begin
    if params = '0' then
      precatcher_ircdebug := False;

    if params = '1' then
      precatcher_ircdebug := True;

    irc_addtext(Netname, Channel, 'CatchDebug is: ' + BoolToStr(precatcher_ircdebug));
  end
  else
    irc_addtext(Netname, Channel, 'CatchDebug is: ' + BoolToStr(precatcher_ircdebug));

  Result := True;
end;

function IrcPrelist(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s1, s2: String;
  mehetki: boolean;
  nn, aktchannel, sitename, nick, event, words, section: String;
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

procedure _readHelpTXTFile(const Netname, Channel: String; filename: String);
var
  s, fn: String;
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
        //s := trim(s);
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

function IrcHelpv2(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  ss, s: String;
begin
  result := False;
  s := '';

  if (params = '') then
  begin
    _readHelpTXTFile(Netname, Channel, 'nhelp');
  end;

  //Show all commands Start
  if ((params = '--all') or (params = '-all') or (params = '--a') or (params = '-a')) then
  begin
    irc_addtext(Netname, Channel, '<b><u>Available commands are</b>:</u>');
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
    irc_addtext(Netname, Channel, 'Type <b>%shelp</b> command to get detailed info.', [irccmdprefix]);
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

function IrcHelp(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s: String;
  f: TextFile;
  fn: String;
begin
  if params <> '' then
  begin
    if (1 = Pos(irccmdprefix, params)) then
      params := Copy(params, length(irccmdprefix) + 1, 1000);

    i := FindIrcCommand(params);
    if i <> 0 then
    begin
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
          end;
        end;
        CloseFile(f);
      end
      else
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

        irc_addtext(Netname, Channel, ':: <u><c7><b>%s</c></u> :</b>', [irccommands[i].cmd]);
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

    irc_addtext(Netname, Channel, '<b>Type %shelp command to get detailed info</b>.', [irccmdprefix]);
  end;

  Result := True;
end;

function IrcSites(const Netname, Channel: String; params: String): boolean;
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

    IrcLineBreak(Netname, Channel, sup.commatext, AnsiChar('"'), '<' + globals.SiteColorOnline + '>UP</c>(' + IntToStr(sup.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, sdn.commatext, AnsiChar('"'), '<' + globals.SiteColorOffline + '>DN</c>(' + IntToStr(sdn.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, suk.commatext, AnsiChar('"'), '<' + globals.SiteColorUnknown + '>??</c>(' + IntToStr(suk.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, spd.commatext, AnsiChar('"'), '<' + globals.SiteColorPermdown + '>PD</c>(' + IntToStr(spd.Count) + '/' + IntToStr(scount) + '): ');
  finally
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function IrcAddSiteInfos(const netname, channel: String; params: String):
  boolean;
var
  s: TSite;
  Text, sitename: String;
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

function IrcInfo(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s: TSite;
  sitename: String;
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

// TODO: Rework this and improve helpfile

  x := TStringList.Create;
  try
    irc_addtext(Netname, Channel, '<b>Site</b> %s:', [s.Name]);
    irc_addtext(Netname, Channel, ' name/speed/location/size: %s / %s / %s / %s', [s.RCString('name', '??'), s.RCString('link', '??'), s.Country, s.RCString('size', '??')]);
    irc_addtext(Netname, Channel, ' sections: %s', [s.sections]);

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
    irc_addtext(Netname, Channel, ' leechers (%d/%d):B %s', [x.Count, s.RCInteger('maxleechers', -1), x.DelimitedText]);
    x.DelimitedText := s.traders;
    irc_addtext(Netname, Channel, ' traders (%d/%d):B %s', [x.Count, s.RCInteger('maxtraders', -1), x.DelimitedText]);

    if s.RCString('notes', '') <> '' then
      irc_addtext(Netname, Channel, ' notes: ' + s.RCString('notes', ''));

  finally
    x.Free;
  end;

  Result := True;
end;

function IrcSite(const Netname, Channel: String; params: String): boolean;
var
  i, i_sec, j_sec: integer;
  s: TSite;
  host, sitename: String;
  x: TStringList;
  s_section, s_sections: String;
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

function IrcBnc(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s: TSite;
  host, sitename: String;
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

    irc_addtext(Netname, Channel, ' bnc: %s:%d', [host, s.RCInteger('bnc_port-' + IntToStr(i), 0)]);
    Inc(i);
  end;

  Result := True;
end;

function IrcDie(const Netname, Channel: String; params: String): boolean;
begin
  try
    slshutdown := IrcSetdown(Netname, Channel, '!ALL!');
  finally
    Result := slshutdown;
  end;
end;

function IrcAffils(const Netname, Channel: String; params: String): boolean;
var
  affils_new, affillist, sitename: String;
  s: TSite;
  TStringList_affils_new, TStringList_affils_old: TStringList;
  i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  affils_new := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if affils_new <> '' then
  begin
    TStringList_affils_new := TStringList.Create;
    TStringList_affils_old := TStringList.Create;
    try
      TStringList_affils_new.Delimiter := ' ';
      TStringList_affils_new.CaseSensitive := False;
      TStringList_affils_new.Sorted := True;
      TStringList_affils_new.Duplicates := dupIgnore;
      TStringList_affils_new.DelimitedText := affils_new;

      TStringList_affils_old.Delimiter := ' ';
      TStringList_affils_old.CaseSensitive := False;
      TStringList_affils_old.Sorted := True;
      TStringList_affils_old.Duplicates := dupIgnore;
      TStringList_affils_old.DelimitedText := s.SiteAffils;

      for i := 0 to TStringList_affils_new.Count - 1 do
      begin
        if (TStringList_affils_old.IndexOf(TStringList_affils_new[i]) <> -1) then
        Begin
          TStringList_affils_old.Delete(TStringList_affils_old.IndexOf(TStringList_affils_new[i]));
        End
        else
        begin
          TStringList_affils_old.Add(TStringList_affils_new[i]);
        end
      end;

      s.SiteAffils := TStringList_affils_old.DelimitedText;
    finally
      TStringList_affils_new.Free;
      TStringList_affils_old.Free;
    end;
  end;

  affillist := s.SiteAffils;

  if affillist <> '' then
    IrcLineBreak(Netname, Channel, affillist, ' ', Format('<b>%s</b>@%s : ', ['', sitename]), 12)
  else
    irc_addText(Netname, Channel, 'No affils available.');

  Result := True;
end;

function IrcSetAffils(const Netname, Channel: String; params: String): boolean;
var
  affils, ss, sitename: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  affils := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Site <b>%s</b> not found.', [sitename]);
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

  Result := True;
end;

function IrcIdent(const Netname, Channel: String; params: String): boolean;
var
  ss, sitename: String;
  s: TSite;
  ident: String;
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

function IrcKnowngroups(const Netname, Channel: String; params: String):
  boolean;
begin
  KnownGroupsStart();
  Result := True;
end;

function IrcNoSocks5(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  s: TSite;
  q: integer;
  s2: String;
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

function IrcLookup(const Netname, Channel: String; params: String): boolean;
var
  sitename, section, dir: String;
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
      irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].', [section, section]);
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

function IrcLeechers(const Netname, Channel: String; params: String): boolean;
var
  leecherlist, sitename, users: String;
  i: Integer;
  s: TSite;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  users := mystrings.RightStr(params, length(sitename) + 1);

  x := TStringList.Create;
  try
    x.commatext := sitename;

    for i := 0 to x.Count - 1 do
    begin
      leecherlist := '';

      s := FindSiteByName(Netname, x.Strings[i]);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site %s not found.', [x.Strings[i]]);
        Continue;
      end;

      leecherlist := s.SetLeechers(users, True);
      if leecherlist <> '' then
      begin
        irc_addText(Netname, Channel, 'Leecher list for <b>%s</b>: %s', [s.Name, leecherlist]);
      end;

    end;
  finally
    x.Free;
  end;

  Result := True;
end;

function IrcCountry(const Netname, Channel: String; params: String): boolean;
var
  sitename, country: String;
  i: Integer;
  s: TSite;
begin
  Result := False;
  sitename := AnsiUpperCase(SubString(params, ' ', 1));
  country := AnsiUpperCase(stringreplace(mystrings.RightStr(params, length(sitename) + 1), '.', '', [rfReplaceAll]));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  i := AnsiIndexText(country, CountryCodes);
  if not (i > -1) then
  begin
    irc_addtext(Netname, Channel, 'Country .%s is not a valid country! Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements', [country]);
    exit;
  end;

  s.Country := '.' + country;
  irc_addtext(Netname, Channel, 'Country for %s set to %s (%s)', [sitename, s.Country, CountryNames[i]]);

  Result := True;
end;

function IrcLink(const Netname, Channel: String; params: String): boolean;
var
  sitename, link: String;
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

function IrcNotes(const Netname, Channel: String; params: String): boolean;
var
  sitename, notes: String;
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

function IrcSize(const Netname, Channel: String; params: String): boolean;
var
  sitename, size: String;
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

function IrcName(const Netname, Channel: String; params: String): boolean;
var
  sitename, Name: String;
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

function IrcTraders(const Netname, Channel: String; params: String): boolean;
var
  ss, sitename, users: String;
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

function IrcUserslots(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcFreeslots(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  db: integer;
  ss: String;
begin
  Result := False;

  db := 0;
  ss := '';
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if (TSite(sites.Items[i]).Name = getAdminSiteName) then
      Continue;

    ss := ss + format('<b>%s</b> (%d/%d) ', [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
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

function IrcFindAffil(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  db, i: integer;
  ss: String;
  affil: String;
  found: Boolean;
begin
  Result := False;
  affil := Uppercase(SubString(params, ' ', 1));

  ss := 'Sites with Affilgroup <b>' + affil + '</b> are: ';
  db := 0;
  found := False;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsAffil(affil) then
    begin
      found := True;

      if s.PermDown then
      begin
        ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorPermdown, s.Name]);
      end
      else
      begin
        case s.working of
          sstUp: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorOnline, s.Name]);
          sstDown: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorOffline, s.Name]);
          sstUnknown: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorUnknown, s.Name]);
        end;
      end;

      Inc(db);
      if db >= 10 then
      begin
        irc_addtext(Netname, Channel, ss);
        db := 0;
        ss := '';
      end;
    end;
  end;

  if found then
    irc_addtext(Netname, Channel, ss)
  else
    irc_addtext(Netname, Channel, Format('No Site with %s as Affilgroup found!', [affil]));

  Result := True;
end;

function IrcFindCountry(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  site_found: boolean;
  country, ss: String;
begin
  country := AnsiUpperCase(stringreplace(SubString(params, ' ', 1), '.', '', [rfReplaceAll]));
  site_found := False;
  ss := format('Site(s) with Country .%s:', [country]);

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if '.' + country = s.Country then
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
    irc_addtext(Netname, Channel, 'No sites in country .%s found!', [country]);
  end;

  Result := True;
end;

function IrcFindSection(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  section: String;
begin
  section := UpperCase(SubString(params, ' ', 1));

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if (TSite(sites.Items[i]).Name = getAdminSiteName) then
      Continue;

    if s.IsSection(section) then
      irc_addtext(Netname, Channel, '<b>%s</b>: Free trader and leech slots: %d %d', [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
  end;

  Result := True;
end;

function IrcAuto(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;

  if params <> '' then
  begin
    if params = '0' then
    begin
      sitesdat.WriteBool('precatcher', 'auto', False);
      irc_addtext(Netname, Channel, Format('Auto is disabled [%s] now!', [IntToStr(integer(precatcher.precatcherauto))]));
    end;

    if params = '1' then
    begin
      sitesdat.WriteBool('precatcher', 'auto', True);
      irc_addtext(Netname, Channel, Format('Auto is enabled [%s] now!', [IntToStr(integer(precatcher.precatcherauto))]));
    end;
  end
  else
  begin
    if precatcher.precatcherauto then
      irc_addtext(Netname, Channel, Format('Precatcher auto is: Enabled [%s]', [IntToStr(integer(precatcher.precatcherauto))]))
    else
      irc_addtext(Netname, Channel, Format('Precatcher auto is: Disabled [%s]', [IntToStr(integer(precatcher.precatcherauto))]));
  end;

  Result := True;
end;
(*
TODO: Maybe remove this? Or for what is this??

function IrcAutoCrawler(const Netname, Channel: String; params: String):
  boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: String;
  ss: String;
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

function IrcCrawler(const Netname, Channel: String; params: String): boolean;
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

function IrcConfirmerAnnounce(const Netname, Channel: String; params: String):
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

function IrcCrawl(const Netname, Channel: String; params: String): boolean;
var
  y, m, d: integer;
  dd: TDateTime;
  sitename, section: String;
  i: integer;
  s: TSite;
  asc, sc: String;
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
        asc := Fetch(sc, ' ', True, False);
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

function IrcAutoLogin(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      if status > -1 then
      begin
        TSite(sites.Items[i]).WCInteger('autologin', status);
      end;

      irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [TSite(sites.Items[i]).Name, integer(TSite(sites.Items[i]).RCBool('autologin', False))]);
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

        irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [sitename, integer(s.RCBool('autologin', False))]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

// TODO: add a property in TSite for autobnctest
function IrcAutoBnctest(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  enableAutobnctest: boolean;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      enableAutobnctest := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).RCInteger('autobnctest', 0) <= 0 then
            enableAutobnctest := True;

          TSite(sites.Items[i]).WCInteger('autobnctest', status);
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autobnctest');
          TSite(sites.Items[i]).RemoveAutoBnctest;
        end;
      end;

      irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).RCInteger('autobnctest', 0)]);

      if enableAutobnctest then
        TSite(sites.Items[i]).AutoBnctest;
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

        enableAutobnctest := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.RCInteger('autobnctest', 0) <= 0 then
              enableAutobnctest := True;

            s.WCInteger('autobnctest', status);
          end
          else
          begin
            s.DeleteKey('autobnctest');
            s.RemoveAutoBnctest;
          end;
        end;

        irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d', [sitename, s.RCInteger('autobnctest', 0)]);

        if enableAutobnctest then
          s.AutoBnctest;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcAutoRules(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  StartTask: boolean;
  i: integer;
  x: TStringList;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      StartTask := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).AutoRulesStatus <= 0 then
            StartTask := True;

          TSite(sites.Items[i]).AutoRulesStatus := status;
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autorules');
          TSite(sites.Items[i]).RemoveAutoRules;
        end;
      end;
      irc_addtext(Netname, Channel, 'Autorules of %s is: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).AutoRulesStatus]);

      if StartTask then
        TSite(sites.Items[i]).AutoRules;
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
        if s.PermDown then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.', [sitename]);
          continue;
        end;

        StartTask := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.AutoRulesStatus <= 0 then
              StartTask := True;

            s.AutoRulesStatus := status;
          end
          else
          begin
            s.DeleteKey('autorules');
            s.RemoveAutoRules;
          end;
        end;
        irc_addtext(Netname, Channel, 'Autorules of %s is: %d', [sitename, s.AutoRulesStatus]);

        if StartTask then
          s.AutoRules;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcAutoDirlist(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  fNeedTaskCreate: boolean;
  sections: String;
  ss: String;
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

  fNeedTaskCreate := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.RCInteger('autodirlist', 0) <= 0 then
        fNeedTaskCreate := True;
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
    [sitename, s.RCInteger('autodirlist', 0), s.RCString('autodirlistsections', '')]);

  if fNeedTaskCreate then
    s.AutoDirlist;

  Result := True;
end;

function IrcAutoIndex(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: String;
  ss: String;
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

function IrcAutoNuke(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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

function IrcKbShow(const Netname, Channel: String; params: String): boolean;
var
  section, rls: String;
  p: TPazo;
  i: integer;
  s, ss: String;
begin
  Result := False;
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
    irc_addtext(Netname, Channel, Format('Can not find any knowledge base entry for %s %s', [section, rls]));

  Result := True;
end;

function IrcKbList(const Netname, Channel: String; params: String): boolean;
var
  p: TPazo;
  i, db: integer;
  section: String;
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

function IrcKbExtra(const Netname, Channel: String; params: String): boolean;
var
  section, rls, extra: String;
begin
  section := UpperCase(SubString(params, ' ', 1));
  rls := SubString(params, ' ', 2);
  extra := mystrings.RightStr(params, length(section) + length(rls) + 2);
  kb_Add(Netname, Channel, '', section, extra, 'NEWDIR', rls, '', True);

  Result := True;
end;

function IrcKbAdd(const Netname, Channel: String; params: String): boolean;
var
  sitename, event, section, rls_section, rls: String;
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

function IrcSkipReload(const Netname, Channel: String; params: String): boolean;
begin
  try
    Result := SkiplistRehash;
  finally
    irc_addtext(Netname, Channel, 'Skiplist reloaded... (%d entries)', [SkiplistCount]);
  end;
end;

function IrcNoHelp(const Netname, Channel: String; params: String): boolean;
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

function IrcFindUser(const Netname, Channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  user: String;
  leech_up: String;
  leech_dn: String;
  ratio_up: String;
  ratio_dn: String;
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

function IrcUsers(const Netname, Channel: String; params: String): boolean;
var
  ss, sitename: String;
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

function IrcShowWindow(const Netname, Channel: String; params: String): boolean;
begin
  Result := console_showwindow(params);
end;

function IrcShowWindows(const Netname, Channel: String; params: String):
  boolean;
var
  Windows, s: String;
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

function IrcDelWindow(const Netname, Channel: String; params: String): boolean;
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

function IrcIrcNames(const Netname, Channel: String; params: String): boolean;
var
  th: TMyIrcThread;
  nn, ch: String;
  i: integer;
  s: String;
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

function IrcRepaint(const Netname, Channel: String; params: String): boolean;
begin
  console_repaint;
  Result := True;
end;

function IrcNuke(const Netname, Channel: String; params: String): boolean;
var
  i, t, h, multiplier: integer;
  datestamp: String;
  reason, sitename, rip, section, yyyy, yy, mm, dd: String;
  site: TSite;
  n: TNukeQueueItem;
begin
  Result := False;
  i := 0; //< position of date value in string
  h := 0; //< position of first reason character
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

  for i := 0 to sites.Count - 1 do
  begin
    site := nil;
    site := TSite(sites[i]);

    if site = nil then
      Continue;
    if site.Name = getAdminSiteName then
      Continue;

    if site.IsAffil(GotGroupname(rip)) then
    begin
      irc_addtext(Netname, Channel,
        '<b>%s</b> is affil on %s we dont nuke affil!',
        [GotGroupname(rip), site.Name]);
      Continue;
    end;

    if ((sitename = '') or (site.Name = sitename)) then
    begin
      n := TNukeQueueItem.Create;
      n.site := site.Name;
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

  Result := True;
end;

function IrcUnnuke(const Netname, Channel: String; params: String): boolean;
var
  i, t, h: integer;
  datestamp: String;
  reason, sitename, rip, section, yyyy, yy, mm, dd: String;
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

function IrcOper(const Netname, Channel: String; params: String): boolean;
var
  nn, userid, pass: String;
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
      irc_addtext(Netname, Channel, 'IRC oper settings for %s are: %s %s', [nn, userid, pass])
    else
      irc_addtext(Netname, Channel, 'IRC oper settings for %s are turned off.', [nn]);
  end
  else if userid = '-' then
  begin
    // delete mode
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_userid');
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_password');
    irc_addtext(Netname, Channel, 'IRC oper settings for %s are now deleted.', [nn]);
  end
  else
  begin
    // set mode
    sitesdat.WriteString('ircnet-' + nn, 'oper_userid', userid);
    sitesdat.WriteString('ircnet-' + nn, 'oper_password', pass);
  end;

  Result := True;
end;

function IrcNews(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  category: String;
begin
  i := StrToIntDef(SubString(params, ' ', 1), 10);
  category := UpperCase(SubString(params, ' ', 2));
  Result := SlftpNewsShow(Netname, Channel, i, category);
end;

function IrcNewsAdd(const Netname, Channel: String; params: String): boolean;
var
  category: String;
begin
  category := UpperCase(SubString(params, ' ', 1));

  Result := SlftpNewsAdd(Netname, Channel, category, mystrings.RightStr(params, Length(category) + 1));
end;

function IrcNewsDel(const Netname, Channel: String; params: String): boolean;
var
  DeleteNumber: integer;
  input, announce: String;
  AnnounceIt: boolean;
begin
  AnnounceIt := False;
  input := SubString(params, ' ', 1);
  announce := SubString(params, ' ', 2);

  // check if first char of first input is alphabetical
  if input[1] in ['A'..'Z', 'a'..'z'] then
  begin
    if (announce = '-s') or (announce = '-show') then
      AnnounceIt := True;

    Result := SlftpNewsDelete(Netname, Channel, input, AnnounceIt);
  end
  else
  begin
    if input = '*' then
      DeleteNumber := -1
    else
      DeleteNumber := StrToIntDef(input, 0);

    Result := SlftpNewsDelete(Netname, Channel, DeleteNumber);
  end;
end;

function IrcNewsCategories(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;
  irc_addtext(Netname, Channel, 'Valid categories are: %s', [ValidCategoriesAsString]);
  Result := True;
end;

function IrcSpeedStats(const Netname, Channel: String; params: String): boolean;
var
  sitename, section, rip: String;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rip := SubString(params, ' ', 3);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  SpeedStatsShowStats(Netname, Channel, sitename, section, rip);

  Result := True;
end;

function IrcSpeedRecalc(const Netname, Channel: String; params: String):
  boolean;
begin
  // SpeedStatsRecalc(netname, channel);
  SpeedStatsRecalc('CONSOLE', 'ADMIN');
  Result := True;
end;

function IrcRankRecalc(const Netname, Channel: String; params: String): boolean;
begin
  RanksRecalc(Netname, Channel);
  Result := True;
end;

function IrcSpeedTestLocal(const Netname, Channel: String; params: String):
  boolean;
var
  sitename: String;
  s: TSite;
  dir: String;
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

function IrcSpeedTestCleanup(const Netname, Channel: String; params: String):
  boolean;
var
  ss: String;
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
      ss := Fetch(params, ' ', True, False);
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

procedure PickupSpeedtestFile(d: TDirList; var fsfilename: String;
  var fsfilesize: Int64);
var
  de: TDirListEntry;
  i: integer;
begin
  fsfilename := '';
  fsfilesize := 0;
  d.dirlist_lock.Enter;
  try
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
  finally
    d.dirlist_lock.Leave;
  end;
end;

function IrcSpeedTestIn(const Netname, Channel: String; params: String): boolean;
var
  oparams, ss: String;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  ds: TDirlistTask;
  fssitename: String;
  d1, d2: double;
  added: integer;
  d: TDirList;
  fsfilename: String;
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
    ss := Fetch(params, ' ', True, False);
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
    ss := Fetch(params, ' ', True, False);
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
      ss := Fetch(params, ' ', True, False);
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

function IrcSpeedTestOut(const Netname, Channel: String; params: String): boolean;
var
  oparams, ss: String;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  fs: TFileSizeTask;
  fssitename, fsfilename: String;
  fsfilesize: Int64;
  fsfilesizemb: double;
  todel: String;
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
    ss := Fetch(params, ' ', True, False);
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
    ss := Fetch(params, ' ', True, False);
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
    ss := Fetch(params, ' ', True, False);
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

function IrcIndexQuery(const Netname, Channel: String; params: String): boolean;
var
  s, ss: String;
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

function IrcIndexDropSection(const Netname, Channel: String; params: String):
  boolean;
begin
  params := UpperCase(params);

  indexerRemoveSiteSection(SubString(params, ' ', 1),
    SubString(params, ' ', 2));

  Result := True;
end;

function IrcIndexStat(const Netname, Channel: String; params: String): boolean;
var
  s, ss: String;
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

function IrcStatRaces(const Netname, Channel: String; params: String): boolean;
var
  sitename, period: String;
  detailed: Boolean;
begin
  Result := False;
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

function IrcStatSites(const Netname, Channel: String; params: String): boolean;
var
  q, ss: String;
  sectionname, sectionfilter: String;
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

function IrcStatSitesByGroup(const Netname, Channel: String; params: String):
  boolean;
var
  sss, q, ss: String;
  groupname, groupfilter, sectionname, sectionfilter: String;
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

function IrcStatSitesByUser(const Netname, Channel: String; params: String): boolean;
var
  q, ss: String;
  username, userfilter, sectionname, sectionfilter: String;
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

function IrcStatGroups(const Netname, Channel: String; params: String): boolean;
var
  q, ss: String;
  sectionname, sectionfilter: String;
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

function IrcStatGroupsBySite(const Netname, Channel: String; params: String):
  boolean;
var
  q, ss: String;
  sitename, sitefilter, sectionname, sectionfilter: String;
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

function IrcStatUsers(const Netname, Channel: String; params: String): boolean;
var
  q, ss: String;
  sectionname, sectionfilter: String;
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

function IrcStatUsersBySite(const Netname, Channel: String; params: String):
  boolean;
var
  q, ss: String;
  sitename, sitefilter, sectionname, sectionfilter: String;
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

function IrcStatUsersByGroup(const Netname, Channel: String; params: String):
  boolean;
var
  q, ss: String;
  groupname, groupfilter, sectionname, sectionfilter: String;
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

function IrcStatUsersByGroupBySite(const Netname, Channel: String;
  params: String): boolean;
var
  q, ss: String;
  groupname, groupfilter, sitename, sitefilter, sectionname, sectionfilter:
    String;
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

procedure DisplayDelay(Netname, Channel, s1, s2, s3: String);
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

procedure DisplayAllDelay(Netname, Channel, s1, s2: String);
var
  x: TStringList;
  minv, maxv: integer;
  i: integer;
  s, s3: String;
begin
  x := TStringList.Create;
  try
    sitesdat.ReadSection('site-' + s1, x);
    for i := 0 to x.Count - 1 do
    begin
      if ((1 = Pos('delay' + s2, x[i])) and (0 <> Pos('-min', x[i]))) then
      begin
        s := x[i];
        Fetch(s, '-', True, False);
        s3 := Fetch(s, '-', True, False);
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

procedure DeleteDelay(Netname, Channel, s1, s2, s3: String);
begin
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-min');
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-max');
  irc_addtext(Netname, Channel, s3 + ' ' + s2 +
    ' delay is deleted on site ' + s1 + '.');
end;

procedure SpecifyDelay(Netname, Channel, s1, s2, s3: String; minv, maxv:
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

function IrcDelayLeech(const Netname, Channel: String; params: String): boolean;
const
  tipus = 'leech';
var
  sitename, section, s: String;
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
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcDelayUpload(const Netname, Channel: String; params: String):
  boolean;
const
  tipus = 'upload';
var
  sitename, section, s: String;
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
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcTweak(const Netname, Channel: String; params: String): boolean;
var
  ss1, ss2, s1, s2, s3: String;
  x: TRegExpr;
begin
  Result := False;
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
// TODO: it's not used or? Remove or fix it? Emptry url? don't know...^e
function Irctestoffset(const Netname, Channel: String; params: String): boolean;
var
  voctime, vctime, vnow, cnow: int64;
  response, url, ss, s: String;
  x: TRegExpr;
  fHttpGetErrMsg: String;
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
      if not HttpGetUrl(url + params, response, fHttpGetErrMsg) then
      begin
        irc_addtext(Netname, Channel, Format('<c4>[FAILED]</c> Offset TEST --> %s', [fHttpGetErrMsg]));
        exit;
      end;

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

function IrcKillAll(const Netname, Channel: String; params: String): boolean;
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

function IrcSetMYIrcNick(const Netname, Channel: String; params: String): boolean;
var
  ircnick, sname: String;
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

function IrcInviteMyIRCNICK(const Netname, Channel: String; params: String): boolean;
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

function IrcNetNoSocks5(const Netname, Channel: String; params: String): boolean;
var
  nname, Value: String;
  status: boolean;
begin
  nname := SubString(params, ' ', 1);
  Value := SubString(params, ' ', 2);
  status := boolean(StrToInt(Value));
  sitesdat.WriteBool('ircnet-' + nname, 'nosocks5', status);
  Result := True;
end;

function IrcTweakSocks5(const Netname, Channel: String; params: String): boolean;
var
  fname, ftrigger, fvalue: String;
  s5: TmSLSocks5;
begin
  Result := False;

  fname := SubString(params, ' ', 1);
  ftrigger := AnsiLowerCase(SubString(params, ' ', 2));
  fvalue := SubString(params, ' ', 3);

  s5 := FindProxyByName(fname);
  if s5 = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [fname]);
    exit;
  end;

  try
    if ftrigger = 'host' then
    begin
      s5.Host := fvalue;
    end
    else if ftrigger = 'port' then
    begin
      s5.Port := StrToInt(fvalue);
    end
    else if ftrigger = 'user' then
    begin
      s5.Username := fvalue;
    end
    else if ftrigger = 'password' then
    begin
      s5.Password := fvalue;
    end
    else if ftrigger = 'status' then
    begin
      s5.Enabled := StrToBool(fvalue);
    end
    else
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Unknown trigger: %s!', [ftrigger]);
      exit;
    end;
  except
    on e: Exception do
    begin
      Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcTweakSocks5 saving value %s', [e.Message]);
      exit;
    end;
  end;

  Result := True;
end;

function IrcAddSocks5(const Netname, Channel: String; params: String): boolean;
var
  fhostport, fhost, fuser, fpass, fname: String;
  fport, fstatus: integer;
begin
  Result := False;
  fname := UpperCase(SubString(params, ' ', 1));

  if FindProxyByName(fname) <> nil then
  begin
    irc_addtext(Netname, Channel, 'Proxy with name %s already exists!', [fname]);
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

  if not AddNewProxy(fname, fhost, fuser, fpass, fport, boolean(fstatus)) then
  begin
    Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: Adding a new Proxy failed.');
  end;

  Result := True;
end;

function IrcDelSocks5(const Netname, Channel: String; params: String): boolean;
var
  trigger, Value: String;
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

function IrcRehashSocks5(const Netname, Channel: String; params: String):
  boolean;
begin
  Result := RehashProxys;
end;

function IrcDisplaySocks5(const Netname, Channel: String; params: String): boolean;
var
  i, fProxyCount: integer;
begin
  fProxyCount := GetTotalProxyCount;
  irc_addtext(Netname, Channel, 'Listing all %d Proxys:', [fProxyCount]);
  for i := 0 to fProxyCount - 1 do
  begin
    irc_addtext(Netname, Channel, GetFormattedProxyInfo(i));
  end;
  Result := True;
end;

function IrcSetSocks5(const Netname, Channel: String; params: String): boolean;
var
  vname, vvalue, vtrigger: String;
  virc: TMyIrcThread;
  vsite: TSite;
  vsocks: TmSLSocks5;
begin
  Result := False;
  vsocks := nil;

  vtrigger := UpperCase(SubString(params, ' ', 1));
  if vtrigger = 'HTTP' then
  begin
    vvalue := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    vname := UpperCase(SubString(params, ' ', 2));
    vvalue := UpperCase(SubString(params, ' ', 3));

    if (vname = '') or (vvalue = '') then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Wrong input parameters.');
      exit;
    end;
  end;

  if vtrigger = 'SITE' then
  begin
    vsite := FindSiteByName('', vname);
    if vsite = nil then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Site with name %s!', [vname]);
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
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
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
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find IRCNetwork with name %s!', [vname]);
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
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;
      virc.ProxyName := vvalue;
    end;
  end
  else if vtrigger = 'HTTP' then
  begin
    if vvalue = '-1' then
    begin
      config.WriteInteger('http', 'enabled', 0);
      config.WriteString('http', 'proxyname', '');
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;

      config.WriteInteger('http', 'enabled', 1);
      config.WriteString('http', 'proxyname', vvalue);
    end;

    config.UpdateFile;
  end;

  Result := True;
end;

function IrcTestLanguageBase(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;
  irc_addtext(Netname, Channel, 'Read language for: ' + params + ' (only new languagebase supported right now...)');
  // if use_new_language_base then begin
  // irc_addtext(netname,channel,'New languagebase...');
  irc_addtext(Netname, Channel, 'language ->' + FindLanguageOnDirectory(params));
  Result := True;
end;

function IrcLanguageBaseReload(const Netname, Channel: String; params: String): boolean;
begin
  irc_addtext(Netname, Channel, SLLanguagesReload);
  Result := True;
end;

function IrcShowAllRules(const Netname, Channel: String; params: String): boolean;
var
  sitename, sections: String;
  xs: TStringList;
  ii, i, count: Integer;
  r: TRule;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1));
  count := 0;

  if sections <> '' then
  begin
    xs := TStringList.Create;
    try
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
            Inc(count);
          end;
        end;
      end;
    finally
      xs.Free;
    end;
  end
  else
  begin
    if sitename = '*' then
    begin
      irc_addtext(Netname, Channel, 'You can not use the special sitename * as a wildcard for all sites to show all rules.', [sitename, sections]);
      Inc(count);
    end
    else
    begin
      for ii := 0 to rules.Count - 1 do
      begin
        r := TRule(rules[ii]);
        if r.sitename = sitename then
        begin
          irc_addtext(Netname, Channel, '%d %s', [ii, r.AsText(True)]);
          Inc(count);
        end;
      end;
    end;
  end;

  if count = 0 then
    irc_addtext(Netname, Channel, 'No matching rule for your input of %s %s found!', [sitename, sections]);

  Result := True;
end;

// TODO: rewrite it, it's not working as in helpfile declared
function IrcAllRuleDel(const Netname, Channel: String; params: String): boolean;
var
  sitess, sectionss: TStringList;
  // s: TSite;
  sitename, section: String;
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
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcSetMYSQLData(const Netname, Channel: String; params: String): boolean;
var
  fhostport, fhost, fport, fuser, fpassw, fdbname, ftable: String;
begin
  Result := False;
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
  end;
  Result := True;
end;

function IrcViewMYSQLValue(const Netname, Channel: String; params: String): boolean;
begin
  Result := True;
end;

function IrcTweakMYSQL(const Netname, Channel: String; params: String): boolean;
begin
  Result := True;
end;

function IrcMYSQLStatus(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;
end;

function IrcCreateBackup(const Netname, Channel: String; params: String): boolean;
var
  error: String;
begin
  Result := ircBackup(error);
  if error <> '' then
    irc_addtext(Netname, Channel, '<b><c4>%s</b></c>', [error]);
end;

function IrcNoLoginMSG(const Netname, Channel: String; params: String): boolean;
var
  svalue, sname: String;
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
      if (ss.Name = getAdminSiteName) then
        Continue;
      if ss.PermDown then
        Continue;

      if svalue = '' then
      begin
        if (ss.sw = sswGlftpd) then
        begin
          irc_addtext(Netname, Channel, '%s NoLogin MSG: %d', [ss.Name, Ord(ss.NoLoginMSG)]);
        end
        else
        begin
          irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name, SiteSoftWareToSTring(ss)]);
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
          irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name, SiteSoftWareToSTring(ss)]);
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
        irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name, SiteSoftWareToSTring(ss)]);
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
        irc_addtext(Netname, Channel, '%s NoLogin MSG: Not supported on %s', [ss.Name, SiteSoftWareToSTring(ss)]);
      end;
    end
    else
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
  end;

  Result := True;
end;

function IrcUseForNFOdownload(const Netname, Channel: String; params: String): boolean;
var
  sname: String;
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
      if (ss.Name = getAdminSiteName) then
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

function IrcSkipBeingUploadedFiles(const Netname, Channel: String; params: String): boolean;
var
  svalue, sname: String;
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
      if (ss.Name = getAdminSiteName) then
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

function IrcSiteUserFetch(const Netname, Channel: String; params: String): boolean;
var
  i: integer;
  s: TSite;
  r: TRawTask;
  tn: TTaskNotify;
  x: TRegExpr;
  sitename, response, username: String;
  logins, maxdn, maxup: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if (s.Name = getAdminSiteName) then
  begin
    exit;
  end;

  // username can be omitted or you specify another name (e.g. if your a siteop)
  if (username = '') then
    username := s.username;

  tn := AddNotify;
  try
    r := TRawTask.Create(Netname, Channel, s.Name, '', 'SITE USER ' + username);
    tn.tasks.Add(r);
    AddTask(r);
    QueueFire;
    tn.event.WaitFor($FFFFFFFF);
  except
  on E: Exception do
    begin
      RemoveTN(tn);
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
      Exit;
    end;
  end;

  for i := 0 to tn.responses.Count - 1 do
  begin
    response := TSiteResponse(tn.responses[i]).response;

    if ((0 <> AnsiPos('You do not have access', response)) or (0 <> AnsiPos('Access denied', response))) then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: You do not have access to this command.');
      break;
    end;

    if (0 <> AnsiPos('does not exist', response)) then
    begin
      irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: User %s does not exist.', [username]));
      break;
    end;

    x := TRegExpr.Create;
    try
      x.ModifierI := True;

      x.Expression := 'Max Logins\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Logins: %s', [x.Match[1]]);
        logins := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

      x.Expression := 'Max Sim Uploads\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Sim Uploads: %s', [x.Match[1]]);
        maxup := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

      x.Expression := 'Max Sim Downloads\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Sim Downloads: %s', [x.Match[1]]);
        maxdn := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

    finally
      x.free;
    end;
  end;

  RemoveTN(tn);

  if (logins <> '') and (maxup <> '') and (maxdn <> '') then
  begin
    irc_addtext(Netname, Channel, 'Slots:');
    irc_addtext(Netname, Channel, '!slots %s %s', [s.Name, logins]);
    irc_addtext(Netname, Channel, 'Max. number of slots for uploading/downloading:');
    irc_addtext(Netname, Channel, '!maxupdn %s %s %s', [s.Name, maxup, maxdn]);
  end;

  Result := True;
end;

(* PreURLs *)
function IrcPreURLAdd(const Netname, Channel: String; params: String): boolean;
var
  url, offset: String;
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

function IrcPreURLDel(const Netname, Channel: String; params: String): boolean;
var
  s: String;
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

function IrcPreURLMod(const Netname, Channel: String; params: String): boolean;
var
  s, url, offset: String;
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

function IrcPreURLList(const Netname, Channel: String; params: String): boolean;
var
  url, offset: String;
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

function IrcFakeReload(const Netname, Channel: String; params: String): boolean;
begin
  Result := FakesRehash;
end;

function IrcSpamConfig(const Netname, Channel: String; params: String): boolean;
var
  vsecs, vsval: TStringList;
  i: integer;
  csec, ckey, cvalue: String;
begin
  Result := False;
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
          IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>' + vsecs.Strings[i] + ':</b> ', 9);
        end;
        Result := True;
        exit;
      end;

      csec := SubString(params, ' ', 1);
      ckey := SubString(params, ' ', 2);
      if ckey = '' then
      begin
        vsval.Clear;
        spamcfg.ReadSection(csec, vsval);
        IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>valid keys:</b> ', 9);
        Result := True;
        exit;
      end;

      cvalue := SubString(params, ' ', 3);
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


function IrcSetupOffset(const Netname, Channel: String; params: String): boolean;
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

function IrcSetupPretimeMode(const Netname, Channel: String; params: String): boolean;
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

function IrcSetupPretimeMode2(const netname, channel: String; params: String): boolean;
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

function IrcSetupADDPreMode(const netname, channel: String; params: String): boolean;
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


function IrcFindPretime(const Netname, Channel: String; params: String):
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

function IrcDisplayMappings(const Netname, Channel: String; params: String):
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

function IrcDelPart(const Netname, Channel: String; params: String): boolean;
var
  nn, blowchannel: String;
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
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcReloadGlobalSkipGrouplist(const Netname, Channel: String; params: String): boolean;
begin
  Result := Rehashglobalskiplist;
  if Result then
    irc_addtext(Netname, Channel, '%d groups in list.', [globalgroupskip.Count]);
end;

function IrcSetPretime(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
  section: String;
  s_pretime: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

function IrcRulesLoad(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;
end;

function IrcSetSitePermdown(const Netname, Channel: String; params: String):
  boolean;
var
  s: TSite;
  sname: String;
  svalue: String;
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
      s.WCInteger('disabled_autorules', s.AutoRulesStatus);
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
      s.AutoRulesStatus := s.RCInteger('disabled_autorules', 0);
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

function IrcSetdown(const Netname, Channel: String; params: String): boolean;
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
        if (TSite(sites.Items[i]).Name = getAdminSiteName)
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

        if (s.Name = getAdminSiteName) then
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

function IrcRulesReload(const Netname, Channel: String; params: String): boolean;
begin
  RulesReload;
  Result := True;
end;

function parseSTATLine(sitename, line: String; includeLastCredits: boolean = false): String;
var
  x: TRegExpr;
  ss, creds, ratio: String;
  minus: boolean;
  c: double;
begin
  x := TRegExpr.Create;
  try
    x.ModifierI := True;

    x.Expression := config.ReadString('sites', 'ratio_regex', '(Ratio|R|Shield|Health\s?):.+?(\d+\:\d+|Unlimited|Leech)');
    if x.Exec(line) then
    begin
      if (AnsiContainsText(x.Match[3], 'Unlimited') or (x.Match[3] = '1:0.0')) then
        ratio := 'Unlimited'
      else
        ratio := x.Match[2];
    end;

    x.Expression := config.ReadString('sites', 'credits_regex', '(Credits|Creds|C|Damage|Ha\-ooh\!)\:?\s?([\-\d\.\,]+)\s?(MB|GB|TB|EP|ZP)\]?');
    if x.Exec(line) then
    begin
      minus := False;
      ss := x.Match[2];
      if AnsiContainsText(ss, '-') then
      begin
        minus := True;
        ss := StringReplace(ss, '-', '', [rfReplaceAll, rfIgnoreCase]);
      end;
      {$IFDEF FPC}
        ss := StringReplace(ss, '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        ss := StringReplace(ss, '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      c := strtofloat(ss);
      ss := x.Match[3];
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

function IrcShowCredits(const Netname, Channel: String; params: String): boolean;
var
  i: integer;

  sitename: String;
  sitesList : TStringList;
begin
  Result := False;
  sitename := AnsiUpperCase(SubString(params, ' ', 1));

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ShowCredits(Netname, channel, TSite(sites.Items[i]));
    end;
  end
  else
  begin
    sitesList := TStringList.Create;
    try
      sitesList.Delimiter := ' ';
      sitesList.DelimitedText := UpperCase(params);

      for i := 0 to sitesList.Count - 1 do
      begin
        ShowCredits(Netname, channel, sitesList[i]);
      end;

    finally
      sitesList.Free;
    end;
  end;

  Result := True;
end;

procedure ShowCredits(const Netname, Channel, siteName: String);
var s : Tsite;
begin
  s := FindSiteByName(Netname, siteName);
  if s <> nil then
    ShowCredits(Netname, channel, s)
  else
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [siteName]);
end;

procedure ShowCredits(const Netname, Channel: String; s : Tsite);
var
  r: TRawTask;
  tn: TTaskNotify;
begin
  if ((s = nil) or (s.Name = getAdminSiteName)) then
    exit;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Site %s is set permdown! </c></b>', [s.Name]);
    exit;
  end;
  if not s.IsUp then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Site %s is temporarily offline! </c></b>', [s.Name]);
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
        exit;
      end;
    end;
    irc_addtext(Netname, Channel, parseSTATLine(s.Name,
      TSiteResponse(tn.responses[0]).response));
  finally
    RemoveTN(tn);
  end;
end;

function IrcUptime(const Netname, Channel: String; params: String): boolean;
var
  cpuversion: String;
  fProcessID, fCmdLine, fUsageInfo, fUnit: String;
  fMemUsage: double;
  rr: TRegexpr;
begin
  {$IFDEF FPC}
    {$IFDEF CPU32}
      cpuversion := '32-Bit';
    {$ELSE}
      cpuversion := '64-Bit';
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      {$IFDEF WIN32}
        cpuversion := '32-Bit';
      {$ELSE}
        cpuversion := '64-Bit';
      {$ENDIF}
    {$ELSE}
      {$IFDEF LINUX32}
        cpuversion := '32-Bit';
      {$ELSE}
        cpuversion := '64-Bit';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPUARM}
    cpuversion := cpuversion + ' on ARM';
  {$ENDIF}

  cpuversion := cpuversion + ' [' + Compiler_Str + ']';

  fProcessID := IntToStr(CurrentProcessId);

  {$IFDEF MSWINDOWS}
    // TODO: read memory usage on windows
    fMemUsage := 13.37;
    fUnit := 'GB';
  {$ELSE}
    {$IFDEF UNIX}
      fCmdLine := '/proc/' + fProcessID + '/status';

      if RunCommand('cat', [fCmdLine], fUsageInfo) then
      begin
        rr := TRegexpr.Create;
        try
          rr.ModifierI := True;
          rr.Expression := 'VmRSS\:\s*(\d+)\s*\w+';

          if rr.Exec(fUsageInfo) then
          begin
            fMemUsage := StrToFloat(rr.Match[1]);
            RecalcSizeValueAndUnit(fMemUsage, fUnit, 1);
          end;
        finally
          rr.Free;
        end;
      end;
    {$ENDIF}
  {$ENDIF}

  irc_addtext(Netname, Channel, '<b>%s</b> (%s) (PID: %s / MEM: %s %s) with OpenSSL %s is up for <c7><b>%s</b></c> [%s]', [Get_VersionString, cpuversion, fProcessID, FloatToStrF(fMemUsage, ffNumber, 15, 2), fUnit, OpenSSLShortVersion, DateTimeAsString(started), DatetimetoStr(started)]);

  Result := True;
end;

function IrcShowAppStatus(const Netname, Channel: String; params: String): boolean;
var
  rx: TRegexpr;
  spd, sup, sdn, suk: TStringList;
begin
  IrcUptime(Netname, Channel, '');

  irc_addtext(Netname, Channel, SlftpNewsStatus);

  irc_addtext(Netname, Channel, '<b>Knowledge Base</b>: %d Rip''s in mind', [kb_list.Count]);
  irc_addtext(Netname, Channel, TheTVDbStatus);

  if TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)) = plmSQLITE then
    irc_addtext(Netname, Channel, dbaddpre_Status);

  irc_addtext(Netname, Channel, 'Other Stats: %s <b>-</b> %s <b>-</b> %s', [dbaddurl_Status, dbaddimdb_Status, dbaddnfo_Status]);

  rx := TRegexpr.Create;
  sup := TStringList.Create;
  spd := TStringList.Create;
  sdn := TStringList.Create;
  suk := TStringList.Create;
  try
    rx.ModifierI := True;
    SitesWorkingStatusToStringlist(Netname, Channel, sup, sdn, suk, spd);

    irc_addtext(Netname, Channel,
      '<b>Sites count</b>: %d | <b>Online</b> %d - <b>Offline</b> %d - <b>Unknown</b> %d - <b>Permanent offline</b> %d ', [sites.Count - 1, sup.Count, sdn.Count, suk.Count, spd.Count]);

    rx.Expression := 'QUEUE\:\s(\d+)\s\(Race\:(\d+)\sDir\:(\d+)\sAuto\:(\d+)\sOther\:(\d+)\)';
    if rx.Exec(ReadAppQueueCaption) then
      irc_addtext(Netname, Channel,
        '<b>Complete queue count</b>: %s | <b>Racetasks</b> %s - <b>Dirlisttasks</b> %s - <b>Autotasks</b> %s - <b>Other</b> %s', [rx.Match[1], rx.Match[2], rx.Match[3], rx.Match[4], rx.Match[5]]);

  finally
    rx.free;
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function Ircaddknowngroup(const Netname, Channel: String; params: String): boolean;
var
  section, glist: String;
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

function IrcSLFTPConfig(const Netname, Channel: String; params: String): boolean;
var
  x, y: TStringList;
  csec, ckey, cvalue, s: String;
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

function IrcChanSetSitename(const Netname, Channel: String; params: String): boolean;
var
  sname, nname, chans: String;
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

function IrcAnnounceIMDBInfo(const Netname, Channel: String; params: String):
  boolean;
var
  //  rlzname: string;
  i: integer;
  imdbdata: TDbImdbData;
begin
  //  Result := False;

  dbaddimdb_cs.Enter;
  try
    i := last_imdbdata.IndexOf(params);
  finally
    dbaddimdb_cs.Leave;
  end;

  if i = -1 then
  begin
    irc_addtext(Netname, Channel,
      format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    Result := True;
    exit;
  end
  else
  begin
    dbaddimdb_cs.Enter;
    try
      imdbdata := TDbImdbData(last_imdbdata.Objects[i]);
    finally
      dbaddimdb_cs.Leave;
    end;

    imdbdata.PostResults(Netname, Channel, params);
  end;
  Result := True;
end;

{ The TV dB Function              }

function IrcAnnounceTVInfo(const Netname, Channel: String; params: String):
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

function IrcDelTheTVDbInfo(const Netname, Channel: String; params: String):
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

function IrcUpdateTVMazeInfo(const Netname, Channel: String; params: String): boolean;
var
  respo, tvmaze_id, tv_showname: String;
  otvr, newtvi: TTVInfoDB;
  fHttpGetErrMsg: String;
begin
  Result := false;

  tvmaze_id := '';
  tv_showname := '';

  if strtointdef(params, -1) > -1 then
    tvmaze_id := params
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
    Irc_AddText(Netname, Channel, '<b><c4>Error</c></b>: Show named <b>%s</b> not found in our local database.', [tv_showname]);
    exit;
  end;

  if not HttpGetUrl('https://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode', respo, fHttpGetErrMsg) then
  begin
    if tv_showname <> '' then
      Irc_AddText(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze Update for %s (ID: %s) --> %s', [tv_showname, tvmaze_id, fHttpGetErrMsg]))
    else
      Irc_AddText(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze Update for %s --> %s', [tvmaze_id, fHttpGetErrMsg]));
    exit;
  end;

  if ((respo = '') or (respo = '[]')) then
  begin
    Irc_AddText(Netname, Channel, '<c4>IrcUpdateTVMazeInfo</c>: No Result from TVMaze API when updating %s', [tvmaze_id]);
    Exit;
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
      Irc_AddText(Netname, Channel, '<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]);
      Exit;
    end;
  end;
end;

function IrcSetTVRageID(const Netname, Channel: String; params: String): boolean;
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

function IrcSetTheTVDBID(const netname, channel: String; params: String): boolean;
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

function IrcAddTVMazeToDb(const netname, channel: String; params: String): boolean;
var
  resp, ssname, sid: String;
  tvr: TTVInfoDB;
  x: TRegExpr;
  i, sresMAXi: integer;
  res: TStringlist;
  showname: String;
  fHttpGetErrMsg: String;
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
    if not HttpGetUrl('https://api.tvmaze.com/shows/' + sid + '?embed[]=nextepisode&embed[]=previousepisode', resp, fHttpGetErrMsg) then
    begin
      // TODO: maybe not showing correct stuff when no info was found
      irc_addtext(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze API search for %s --> %s', [ssname, fHttpGetErrMsg]));
      Result := True;
      exit;
    end;

    if ((resp = '') or (resp = '[]')) then
    begin
      irc_addtext(Netname, Channel, Format('<c4>IrcUpdateTVMazeInfo</c>: No info found for %s', [ssname]));
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

function IrcShowSiteNukes(const netname, channel: String; params: String): boolean;
var
  sitename, ss: String;
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

function IrcCatchMod(const netname, channel: String; params: String): boolean;
var
  index, sitename, nn, channelname, botnicks, event, words, section: String;
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

function IrcLastLog(const Netname, Channel: String; params: String): boolean;
var
  lines: integer;
  lastlog: String;
begin
  Result := False;
  lines := StrToIntDef(params, -1);

  if lines >= 100 then
  begin
    irc_Addtext(Netname, Channel, 'Are you okay ? <b>%d</b> is too much lines. Do you want to break the internet ?!', [lines]);
    exit;
  end;

  if lines = -1 then
    lines := 5;

  lastlog := LogTail(lines);
  if lastlog <> '' then
  begin
    irc_Addtext(Netname, Channel, 'Displaying last <b>%d</b> log lines:', [lines]);
    irc_Addtext(Netname, Channel, lastlog);
  end
  else
  begin
    irc_Addtext(Netname, Channel, 'No log entries to display.');
  end;

  Result := True;
end;

function IrcSetDebugverbosity(const Netname, Channel: String; params: String):
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

function IrcRebuildSlot(const Netname, Channel: String; params: String):
  boolean;
var
  sitename: String;
  s_slot: String;
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

function IrcRecalcFreeslots(const Netname, Channel: String; params: String): boolean;
var
  sitename: String;
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
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
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

function IrcSetAutoInvite(const netname, channel: String; params: String): boolean;
var
  sitename, value: String;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  value := SubString(params, ' ', 2);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    Exit;
  end;

  if site.IRCNick = '' then
  begin
    irc_addtext(Netname, Channel, 'You have to define an IRC Nick for <b>%s</b>.', [sitename]);
    Exit;
  end;

  if value = '' then
  begin
    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite)]);
  end
  else if ((value = '1') or (value = '0')) then
  begin
    if value = '1' then
      site.UseAutoInvite := True;
    if value = '0' then
      site.UseAutoInvite := False;

    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite)]);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Syntax error. Wrong value parameter!');
    Exit;
  end;

  Result := True;
end;


function IrcTestColors(const Netname, Channel: String; params: String): boolean;
var
  i, colorscount: integer;
  colors: String;
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
  Netname, Channel, params: String; cmd: String = '');
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

function IrcHelpHeader(const netname, channel: String; params: String): boolean;
begin
  Result := False;
end;

function IrcHelpSeperator(const netname, channel: String; params: String): boolean;
begin
  Result := False;
end;

function IrcNope(const Netname, Channel: String; params: String): boolean;
begin
  Result := False;
end;

end.

