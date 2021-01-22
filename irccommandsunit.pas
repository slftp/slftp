unit irccommandsunit;

interface

uses
  Classes, dirlist, irc, irccommands.rank, irccommands.tv, irccommands.speed, irccommands.work,
  irccommands.windows, irccommands.slots, irccommands.news, irccommands.kb, irccommands.indexer,
  irccommands.reload, irccommands.section, irccommands.imdb, irccommands.pretime, irccommands.socks,
  irccommands.rules, irccommands.info, irccommands.precatcher, irccommands.irc, irccommands.misc,
  irccommands.stats, irccommands.prebot, irccommands.route, irccommands.site, irccommands.test,
  irccommands.general{, irccommands.preurl, irccommands.mysql};

type
  { Function prototype for all IRC commands }
  TIrcCommandHandler = function(const netname, channel, params: String): boolean;

  { Record which will be used in @link(TIRCCommandThread) to execute the @link(TIrcCommandHandler) function }
  TIrcCommand = record
    cmd: String; //< triggername for command
    hnd: TIrcCommandHandler; //< function which is used for the command
    minparams: integer; //< minimal count of input parameters
    maxparams: integer; //< maximal count of input parameters
    hlpgrp: String; //< name of the group the command belongs to
  end;

  { Thread which will execute the IRC command }
  TIRCCommandThread = class(TThread)
  private
    c: TIRCCommandHandler;
    th: TMyIrcThread;
    Netname, Channel, CMD, Params: String;
  protected
    procedure Execute; override;
  public
    constructor Create(c: TIRCCommandHandler; const netname, channel, params: String; cmd: String = '');
  end;

{ Searchs in @link(ircCommandsArray) for given command @link(aCmd)
  @param(aCmd commandname)
  @returns(Index for commandname in @link(ircCommandsArray)) }
function FindIrcCommand(const aCmd: String): integer;

{ help section handler }
function IrcHelpHeader(const netname, channel, params: String): boolean;

const
  { Names of IRC command groups for @link(hlpgrp) }
  helpCommands: array[0..22] of String = ('general', 'site', 'auto', 'route',
    'rank', 'speed', 'work', 'prebot', 'stats', 'slots', 'misc', 'news', 'irc',
    'rules', 'indexer', 'info', 'reload', 'socks5', 'pretime', 'imdb', 'tv', 'test',
    'section' {, 'preurl', 'mysql'});

  { Declarations of all IRC commands as @link(TIrcCommand) records }
  ircCommandsArray: array[1..221] of TIrcCommand = (
    (cmd: 'GENERAL'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$general'),
    (cmd: 'help'; hnd: IrcHelp; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'die'; hnd: IrcDie; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'uptime'; hnd: IrcUptime; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'status'; hnd: IrcShowAppStatus; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'queue'; hnd: IrcQueue; minparams: 0; maxparams: 2; hlpgrp: 'general'),
    (cmd: 'lastlog'; hnd: IrcLastLog; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'logverbosity'; hnd: IrcSetDebugverbosity; minparams: 0; maxparams: 1; hlpgrp: 'general'),
    (cmd: 'backup'; hnd: IrcCreateBackup; minparams: 0; maxparams: 0; hlpgrp: 'general'),
    (cmd: 'auto'; hnd: IrcAuto; minparams: 0; maxparams: 1; hlpgrp: 'general'),

    (cmd: 'SITES'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$site'),
    (cmd: 'sites'; hnd: IrcSites; minparams: 0; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'site'; hnd: IrcSite; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'addsite'; hnd: IrcAddSite; minparams: 4; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'delsite'; hnd: IrcDelsite; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'addbnc'; hnd: IrcAddBnc; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'delbnc'; hnd: IrcDelBnc; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'siteuser'; hnd: IrcSiteUser; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'sitepass'; hnd: IrcSitePass; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'setdown'; hnd: IrcSetdown; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'setpermdown'; hnd: IrcSetSitePermdown; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'setdir'; hnd: IrcSetDir; minparams: 2; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'slots'; hnd: IrcSlots; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'maxupdn'; hnd: IrcMaxUpDn; minparams: 3; maxparams: 4; hlpgrp: 'site'),
    (cmd: 'maxupperrip'; hnd: IrcMaxUpPerRip; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'maxidle'; hnd: IrcMaxIdle; minparams: 2; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'timeout'; hnd: IrcTimeout; minparams: 3; maxparams: 3; hlpgrp: 'site'),
    (cmd: 'sslfxp'; hnd: IrcSslfxp; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'sslmethod'; hnd: IrcSslmethod; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'legacycwd'; hnd: IrcLegacyCwd; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'skipinc'; hnd: IrcSkipBeingUploadedFiles; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'fetchuser'; hnd: IrcSiteUserFetch; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'usefornfodownload'; hnd: IrcUseForNFOdownload; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'autologin'; hnd: IrcAutoLogin; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'autobnctest'; hnd: IrcAutoBncTest; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'credits'; hnd: IrcShowCredits; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'siteinfo'; hnd: IrcAddSiteInfos; minparams: 1; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'slotsshow'; hnd: IrcSlotsShow; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'bnc'; hnd: IrcBnc; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'bnctest'; hnd: IrcBnctest; minparams: 0; maxparams: - 1; hlpgrp: 'site'),
    (cmd: 'ghost'; hnd: IrcKill; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'rebuildslot'; hnd: IrcRebuildSlot; minparams: 2; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'recalcfreeslots'; hnd: IrcRecalcFreeslots; minparams: 1; maxparams: 1; hlpgrp: 'site'),
    (cmd: 'setdownoutofspace'; hnd: IrcSetDownOnOutOfSpace; minparams: 1; maxparams: 2; hlpgrp: 'site'),
    (cmd: 'reversefxp'; hnd: IrcSetReverseFxp; minparams: 1; maxparams: 3; hlpgrp: 'site'),

    (cmd: 'ROUTES'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$route'),
    (cmd: 'routes'; hnd: IrcSpeeds; minparams: 1; maxparams: 1; hlpgrp: 'route'),
    (cmd: 'routeset'; hnd: IrcSetSpeed; minparams: 3; maxparams: -1 ; hlpgrp: 'route'),
    (cmd: 'routelock'; hnd: IrcLockSpeed; minparams: 3; maxparams: -1; hlpgrp: 'route'),
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
    (cmd: 'spread'; hnd: IrcSpread; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'transfer'; hnd: IrcTransfer; minparams: 5; maxparams: 5; hlpgrp: 'work'),
    (cmd: 'stop'; hnd: IrcPazoStop; minparams: 1; maxparams: 1; hlpgrp: 'work'),
    (cmd: 'lookup'; hnd: IrcLookup; minparams: 2; maxparams: 3; hlpgrp: 'work'),
    (cmd: 'nuke'; hnd: IrcNuke; minparams: 4; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'unnuke'; hnd: IrcUnNuke; minparams: 3; maxparams: - 1; hlpgrp: 'work'),
    (cmd: 'nukes'; hnd: IrcShowSiteNukes; minparams: 1; maxparams: 2; hlpgrp: 'work'),
    (cmd: 'autonuke'; hnd: IrcAutoNuke; minparams: 1; maxparams: 2; hlpgrp: 'work'),
    (cmd: 'checkforrip'; hnd: IrcCheckForExistsRip; minparams: 1; maxparams: 2; hlpgrp: 'work'),

    (cmd: 'PREBOT'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$prebot'),
    (cmd: 'setprecmd'; hnd: IrcPrecmd; minparams: 2; maxparams: - 1; hlpgrp: 'prebot'),
    (cmd: 'setpredir'; hnd: IrcPredir; minparams: 2; maxparams: 3; hlpgrp: 'prebot'),
    (cmd: 'check'; hnd: IrcCheck; minparams: 2; maxparams: 3; hlpgrp: 'prebot'),
    (cmd: 'pre'; hnd: IrcPre; minparams: 1; maxparams: 2; hlpgrp: 'prebot'),
    (cmd: 'pretest'; hnd: IrcPretest; minparams: 2; maxparams: 3; hlpgrp: 'prebot'),
    (cmd: 'batch'; hnd: IrcBatchAdd; minparams: 2; maxparams: 4; hlpgrp: 'prebot'),
    (cmd: 'batchdel'; hnd: IrcBatchDel; minparams: 2; maxparams: 2; hlpgrp: 'prebot'),
    (cmd: 'delrelease'; hnd: IrcDelrelease; minparams: 2; maxparams: 3; hlpgrp: 'prebot'),
    (cmd: 'delallrelease'; hnd: IrcDelallrelease; minparams: 2; maxparams: 3; hlpgrp: 'prebot'),
    (cmd: 'prelist'; hnd: IrcListPreContent; minparams: 1; maxparams: 2; hlpgrp: 'prebot'),
    (cmd: 'prechecktime'; hnd: IrcSetReexamineTime; minparams: 0; maxparams: 1; hlpgrp: 'prebot'),
    (cmd: 'skippre'; hnd: IrcSetSkipPre; minparams: 1; maxparams: 2; hlpgrp: 'prebot'),

    (cmd: 'RACE STATS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$stats'),
    (cmd: 'statrace'; hnd: IrcStatRaces; minparams: 1; maxparams: 3; hlpgrp: 'stats'),

    (cmd: 'LEECH SLOTS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$slots'),
    (cmd: 'delayleech'; hnd: IrcDelayLeech; minparams: 1; maxparams: 4; hlpgrp: 'slots'),
    (cmd: 'delayupload'; hnd: IrcDelayUpload; minparams: 1; maxparams: 4; hlpgrp: 'slots'),

    (cmd: 'MISC'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$misc'),
    (cmd: 'raw'; hnd: IrcRaw; minparams: 1; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'invite'; hnd: IrcInvite; minparams: 1; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'sitechan'; hnd: IrcSiteChan; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'autoinvite'; hnd: IrcSetAutoInvite; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'tweak'; hnd: IrcTweak; minparams: 2; maxparams: - 1; hlpgrp: 'misc'),
    (cmd: 'ident'; hnd: IrcIdent; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'nosocks5'; hnd: IrcNoSocks5; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'noannouncesite'; hnd: IrcNoAnnounceSite; minparams: 1; maxparams: 2; hlpgrp: 'misc'),
    (cmd: 'nohelp'; hnd: IrcNoHelp; minparams: 0; maxparams: 0; hlpgrp: 'misc'),
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
    (cmd: 'ircnet'; hnd: IrcShownet; minparams: 1; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetadd'; hnd: IrcAddnet; minparams: 3; maxparams: 7; hlpgrp: 'irc'),
    (cmd: 'ircnetmod'; hnd: IrcModnet; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircnetmodes'; hnd: IrcModesNet; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdel'; hnd: IrcDelnet; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircnetaddserver'; hnd: IrcNetAddServer; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetdelserver'; hnd: IrcNetDelServer; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetaddperform'; hnd: IrcNetAddPerform; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdelperform'; hnd: IrcNetDelPerform; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnetlistperform'; hnd: IrcNetListPerform; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircnetdoperform'; hnd: IrcNetDoPerform; minparams: 1; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircchannels'; hnd: IrcChannels; minparams: 0; maxparams: 1; hlpgrp: 'irc'),
    (cmd: 'ircchanadd'; hnd: IrcChanAdd; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircchandel'; hnd: IrcDelchan; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircchanblow'; hnd: IrcSetBlowkey; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircchankey'; hnd: IrcSetChankey; minparams: 2; maxparams: 3; hlpgrp: 'irc'),
    (cmd: 'ircchanrole'; hnd: IrcSetChanRole; minparams: 2; maxparams: - 1; hlpgrp: 'irc'),
    (cmd: 'ircchanpart'; hnd: IrcDelPart; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
    (cmd: 'ircnick'; hnd: IrcSetMYIrcNick; minparams: 2; maxparams: 2; hlpgrp: 'irc'),
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
    (cmd: 'rulecp'; hnd: IrcRuleCopy; minparams: 3; maxparams: 4; hlpgrp: 'rules'),
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

    (cmd: 'AFFILS/USERS/INFOS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$info'),
    (cmd: 'info'; hnd: IrcInfo; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'name'; hnd: IrcName; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'link'; hnd: IrcLink; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'affils'; hnd: IrcAffils; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'setaffils'; hnd: IrcSetAffils; minparams: 1; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'size'; hnd: IrcSize; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'country'; hnd: IrcCountry; minparams: 2; maxparams: 2; hlpgrp: 'info'),
    (cmd: 'notes'; hnd: IrcNotes; minparams: 2; maxparams: - 1; hlpgrp: 'info'),
    (cmd: 'findaffil'; hnd: IrcFindAffil; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'findcountry'; hnd: IrcFindCountry; minparams: 1; maxparams: 1; hlpgrp: 'info'),
    (cmd: 'findsection'; hnd: IrcFindSection; minparams: 1; maxparams: 1; hlpgrp: 'info'),

    (cmd: 'RELOAD'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$reload'),
    (cmd: 'catchreload'; hnd: IrcPrereload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'skipreload'; hnd: IrcSkipReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'languagereload'; hnd: IrcLanguageBaseReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'socks5reload'; hnd: IrcRehashSocks5; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'fakereload'; hnd: IrcFakeReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'rulesreload'; hnd: IrcRulesReload; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'globalskipreload'; hnd: IrcReloadGlobalSkipGrouplist; minparams: 0; maxparams: 0; hlpgrp: 'reload'),
    (cmd: 'knowngroupreload'; hnd: IrcKnowngroups; minparams: 0; maxparams: 0; hlpgrp: 'reload'),

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

    (cmd: 'IMDB'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$imdb'),
    (cmd: 'imdbinfo'; hnd: IrcAnnounceIMDBInfo; minparams: 1; maxparams: 1; hlpgrp: 'imdb'),
    (cmd: 'delimdbinfo'; hnd: IrcDeleteIMDBInfo; minparams: 1; maxparams: 1; hlpgrp: 'imdb'),

    (cmd: 'TVINFO'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$tv'),
    (cmd: 'tvinfo'; hnd: IrcAnnounceTVInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'addtvinfo'; hnd: IrcAddTVMazeToDb; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'settvdbid'; hnd: IrcSetTheTVDbID; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'settvrageid'; hnd: IrcSetTVRageID; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'updatetvinfo'; hnd: IrcUpdateTVMazeInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),
    (cmd: 'deltvinfo'; hnd: IrcDelTheTVDbInfo; minparams: 1; maxparams: - 1; hlpgrp: 'tv'),

    (cmd: 'SECTIONS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$section'),
    (cmd: 'sections'; hnd: IrcSections; minparams: 0; maxparams: - 1; hlpgrp: 'section'),

    (*
      // Disabled - probably need some refactoring
      (cmd: 'PREURLS'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$preurl'),
      (cmd: 'preurls'; hnd: IrcPreURLList; minparams: 0; maxparams: 0; hlpgrp: 'preurl'),
      (cmd: 'preurladd'; hnd: IrcPreURLAdd; minparams: 2; maxparams: 2; hlpgrp: 'preurl'),
      (cmd: 'preurldel'; hnd: IrcPreURLDel; minparams: 1; maxparams: 1; hlpgrp: 'preurl'),
      (cmd: 'preurlmod'; hnd: IrcPreURLMod; minparams: 3; maxparams: 3; hlpgrp: 'preurl'),
      (cmd: 'testoffset'; hnd: IrcTestOffset; minparams: 1; maxparams: 3; hlpgrp: 'preurl'),

      // Disabled - probably need some refactoring
      (cmd: 'MYSQL'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$mysql'),
      (cmd: 'setmysql'; hnd: IrcSetMYSQLData; minparams: 5; maxparams: 5; hlpgrp: 'mysql'),
      (cmd: 'mysqlvalue'; hnd: IrcViewMYSQLValue; minparams: 0; maxparams: 0; hlpgrp: 'mysql'),
      (cmd: 'tweakmysql'; hnd: IrcTweakMYSQL; minparams: 2; maxparams: 2; hlpgrp: 'mysql'),
      (cmd: 'mysql'; hnd: IrcMYSQLStatus; minparams: 1; maxparams: 1; hlpgrp: 'mysql'),
    *)

    (cmd: 'TESTING'; hnd: IrcHelpHeader; minparams: 0; maxparams: 0; hlpgrp: '$test'),
    (cmd: 'irccolors'; hnd: IrcTestColors; minparams: 0; maxparams: 0; hlpgrp: 'test')
  );

{ shared functions for IRC commands }
procedure IrcLineBreak(const Netname, Channel, commatext: String; QuoteChar: Char = '"'; fronttext: String = ''; breakafter: integer = 16);
function DirlistB(const netname, channel: String; sitename, dir: String; SpeedTest: boolean = False): TDirList;
procedure RawB(const netname, channel: String; sitename, dir, command: String; AnnounceSitename: boolean = False);

implementation

uses
  SysUtils, Contnrs, debugunit, mystrings, notify, taskdirlist, queueunit, taskraw, sltcp;

const
  section = 'irccommandsunit';

{ TIRCCommandThread }

constructor TIRCCommandThread.Create(c: TIRCCommandHandler; const netname, channel, params: String; cmd: String = '');
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('IRC Command', self.ThreadID);
  {$ENDIF}
  FreeOnTerminate := True;

  self.c := c;
  self.Netname := netname;
  self.th := th;
  self.Channel := channel;
  self.Params := params;
  self.CMD := cmd;
end;

procedure TIRCCommandThread.Execute;
begin
  try
    if c(Netname, Channel, Params) then
    begin
      if (CMD <> 'kbadd') then
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
      Debug(dpError, section, Format('[EXCEPTION] TIRCCommandThread.Execute: %s (%s %s %s %s)', [E.Message, Netname, Channel, CMD, Params]));
    end;
  end;
end;

function FindIrcCommand(const aCmd: String): integer;
var
  i: integer;
begin
  Result := 0;
  if ((aCmd <> '') and (aCmd[1] = '-')) then
    exit;

  for i := Low(ircCommandsArray) to High(ircCommandsArray) do
    if ircCommandsArray[i].cmd = lowercase(aCmd) then
    begin
      Result := i;
      exit;
    end;
end;

{ help section handler }

function IrcHelpHeader(const netname, channel, params: String): boolean;
begin
  Result := False;
end;

procedure IrcLineBreak(const Netname, Channel, commatext: String; QuoteChar: Char = '"'; fronttext: String = ''; breakafter: integer = 16);
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
      end;
      s := s + xs.Strings[i] + ', ';
      Inc(ii);
    end;
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
      end;
    end;
  finally
    xs.Free;
  end;
end;

function DirlistB(const Netname, Channel: String; sitename, dir: String; SpeedTest: boolean = False): TDirList;
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
      Result.FullPath := dir;
  end;
end;

procedure RawB(const Netname, Channel: String; sitename, dir, command: String; AnnounceSitename: boolean = False);
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

end.
