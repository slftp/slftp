unit sitesunit;

interface

uses Classes, encinifile, Contnrs, sltcp, slssl, SyncObjs, Regexpr,
  taskautodirlist, taskautocrawler, taskautonuke, taskautoindex,
  tasklogin, tasksunit, taskrules;

type
  TSlotStatus = (ssNone, ssDown, ssOffline, ssOnline, ssMarkedDown);
  TSSLMethods = (sslNone, sslImplicitSSLv23, sslAuthSslSSLv23,
    sslAuthTLSSSLv23, sslAuthSslTLSv1, sslAuthTlsTLSv1,
    sslImplicitTLSv1, sslAuthTlsTLSv1_2, sslImplicitTLSv1_2);

  {
  @value(sswUnknown unknown ftpd software)
  @value(sswGlftpd glFTPd software)
  @value(sswDrftpd DrFTPD software)
  @value(sswIoftpd ioFTPD software)
  }
  TSiteSw = (sswUnknown, sswGlftpd, sswDrftpd, sswIoftpd);

  TProtection = (prNone, prProtP, prProtC);

  {
  @value(sstUnknown unknown (not yet connected) status)
  @value(sstUp reachable and usable (UP) status)
  @value(sstDown down status)
  @value(sstMarkedDown marked as down because of temporary problems (MAYBE, NOT SURE ABOUT IT!))
  @value(sstOutOfCreds no credits left)
  @value(sstOutOfSpace no space left)
  }
  TSiteStatus = (sstUnknown, sstUp, sstDown, sstMarkedDown, sstOutOfCreds, sstOutOfSpace);

  {
  @value(srNone Site to Site (s2s) SSL not needed)
  @value(srNeeded Site to Site (s2s) SSL needed)
  @value(srUnsupported Site to Site (s2s) SSL not supported)
  }
  TSSLReq = (srNone, srNeeded, srUnsupported);

  TSite = class; // forward

  TSiteSlot = class(TslTCPThread)
  private
    mdtmre: TRegExpr;
    aktdir: String;
    prot: TProtection;
    kilepve: boolean;
    no: integer;
    fstatus: TSlotStatus;
    event: TEvent;
    function LoginBnc(const i: integer; kill: boolean = False): boolean;
    procedure AddLoginTask;
    procedure SetOnline(Value: TSlotStatus);
    procedure ProcessFeat;
    procedure SetDownloadingFrom(const Value: boolean);
    procedure SetUploadingTo(const Value: boolean);
    procedure SetTodotask(Value: TTask);
  public
    //    pre: Boolean;
    localport: integer;
    peerport: integer;
    peerip: String;
    fuploadingto: boolean;
    fdownloadingfrom: boolean;
    lastio: TDateTime;
    lastactivity: TDateTime;
    lastResponse: String;
    lastResponseCode: integer;

    ftodotask: TTask;
    site: TSite;
    procedure DestroySocket(down: boolean);
    procedure Quit;
    function Name: String;
    procedure Fire;
    function Login(kill: boolean = False): boolean;
    procedure Execute; override;
    constructor Create(site: TSite; no: integer);
    destructor Destroy; override;
    function RCBool(Name: String; def: boolean): boolean;
    function RCInteger(Name: String; def: integer): integer;
    function RCDateTime(Name: String; def: TDateTime): TDateTime;
    function RCString(Name, def: String): String;

    procedure Stop; override;

    function MdtmSeconds(filename: String): integer;
    function Read(read_cmd: String = ''): boolean; overload;
    function Read(const read_cmd: String; raiseontimeout: boolean; raiseonclose: boolean; timeout: integer = 0): boolean; overload;
    function Send(const s: String): boolean; overload;
    function Send(const s: String; const Args: array of const): boolean; overload;
    function ReLogin(limit_maxrelogins: integer = 0; kill: boolean = False; s_message: String = ''): boolean;
    function bnc: String;
    function Cwd(dir: String; force: boolean = False): boolean;
    function Dirlist(const dir: String; forcecwd: boolean = False; fulldirlist: boolean = False): boolean;
    function Leechfile(dest: TStream; const filename: String; restFrom: Integer = 0; maxRead: Integer = 0): Integer;
    //    function DirlistD(dir: string; forcecwd: Boolean=False; use_custom_cmd:Boolean = False; fulldirlist: Boolean= False): Boolean;
    function RemoveFile(const dir, filename: String): boolean;
    function RemoveDir(dir: String): boolean;
    function SendProtP: boolean;
    function SendProtC: boolean;
    function Mkdir(const dirtocreate: String): boolean;
    function TranslateFilename(const filename: String): String;
    function Pwd(var dir: String): boolean;
    property uploadingto: boolean read fUploadingTo write SetUploadingTo;
    property downloadingfrom: boolean read fDownloadingFrom write SetDownloadingFrom;
    property todotask: TTask read fTodotask write SetTodotask;
  published
    property Status: TSlotStatus read fstatus write SetOnline;
  end;

  TSite = class
  private
    fworking: TSiteStatus;
    foutofannounce: TDateTime;
    fkreditz: TDateTime;
    fNumDn: integer;
    fNumUp: integer;
    function GetSkipPreStatus: boolean;
    procedure SetSkipPreStatus(Value: boolean);

    function GetPermDownStatus: boolean;
    procedure SetPermDownStatus(Value: boolean);

    function Software: TSiteSW;

    procedure SetWorking(Value: TSiteStatus);

    function GetMaxDn: integer;
    procedure SetMaxDn(Value: integer);
    function GetMaxPreDn: integer;
    procedure SetMaxPreDn(Value: integer);
    function GetMaxUp: integer;
    procedure SetMaxUp(Value: integer);
    function GetMaxIdle: integer;
    procedure SetMaxIdle(Value: integer);
    function GetIdleInterval: integer;
    procedure SetIdleInterval(Value: integer);
    function GetIo_timeout: integer;
    procedure SetIo_timeout(const Value: integer);
    function GetConnect_timeout: integer;
    procedure SetConnect_timeout(const Value: integer);
    function Getsslmethod: TSSLMethods;
    procedure Setsslmethod(const Value: TSSLMethods);
    function Getsslfxp: TSSLReq;
    procedure Setsslfxp(const Value: TSSLReq);
    function GetPredir: String;
    procedure SetPredir(const Value: String);
    function Getlegacydirlist: boolean;
    procedure Setlegacydirlist(const Value: boolean);
    function GetSectionDir(Name: String): String;
    procedure SetSectionDir(Name: String; const Value: String);
    function GetSectionPrecmd(Name: String): String;
    procedure SetSectionPrecmd(Name: String; const Value: String);
    function GetAffils: String;
    procedure SetAffils(Value: String);
    function GetSectionPreTime(Name: String): integer;
    procedure SetSectionPreTime(Name: String; const Value: integer);
    function GetSections: String;
    procedure SettSections(Value: String);
    function GetLeechers: String;
    procedure SettLeechers(Value: String);
    function GetTraders: String;
    procedure SettTraders(Value: String);
    function GetUsers: String;
    function GetNoannounce: boolean;
    procedure SetNoAnnounce(const Value: boolean);
    function FetchAutoIndex: TAutoIndexTask;
    function FetchAutoBnctest: TLoginTask;
    function FetchAutoRules: TRulesTask;
    function FetchAutoDirlist: TAutoDirlistTask;
    function FetchAutoCrawler: TAutoCrawlerTask;
    function FetchAutoNuke: TAutoNukeTask;
    procedure SetNumDn(const Value: integer);
    procedure SetNumUp(const Value: integer);
    procedure SetFreeSlots(const Value: integer);

    function GetProxyName: String;
    procedure SetProxyName(Value: String);

    function GetSiteUsername: String;
    procedure SetSiteUsername(Value: String);

    function GetSitePassword: String;
    procedure SetSitePassword(Value: String);

    function GetSiteCountry: String;
    procedure SetSiteCountry(Value: String);

    function GetSiteMaxUpPerRip: integer;
    procedure SetSiteMaxUpPerRip(const Value: integer);

    function GetNoLoginMSG: boolean;
    procedure SetNoLoginMSG(Value: boolean);

    function GetUseForNFOdownload: integer;
    procedure SetUseForNFOdownload(Value: integer);

    function GetSkipBeingUploadedFiles: boolean;
    procedure SetSkipBeingUploadedFiles(Value: boolean);

    function GetIRCNick: String;
    procedure SetIRCNick(Value: String);

    function GetSiteInfos: String;
    procedure SetSiteInfos(Value: String);

    function GetLastKnownCredits: int64;
    procedure SetLastKnownCredits(Value: int64);

    function GetUseAutoInvite: Boolean;
    procedure SetUseAutoInvite(Value: Boolean);

    function GetIsUp: Boolean;

    function GetAutoRulesStatus: integer;
    procedure SetAutoRulesStatus(const Value: integer);

    function GetSetDownOnOutOfSpace: boolean;
    procedure SetSetDownOnOutOfSpace(const Value: boolean);
    function GetSetDownOnOutOfCredits: boolean;
    procedure SetSetDownOnOutOfCredits(const Value: boolean);
    { Sets the necessary values to set the site down due to no space or credits left }
    procedure SetDownSiteDueToCreditsOrSpace;
  public
    emptyQueue: boolean;
    markeddown: boolean;
    siteinvited: boolean;

    ffreeslots: integer;
    Name: String; //< sitename
    slots: TObjectList;

    // siteinvited: Boolean;

    constructor Create(const Name: String);
    destructor Destroy; override;

    procedure Stop;
    procedure DeleteKey(const Name: String);

    function RCString(const Name, def: String): String;
    procedure WCString(const Name: String; const val: String);
    function RCInteger(const Name: String; const def: integer): integer;
    procedure WCInteger(const Name: String; const val: integer);
    function RCBool(const Name: String; const def: boolean): boolean;
    procedure WCBool(const Name: String; const val: boolean);
    function RCDateTime(const Name: String; def: TDateTime): TDateTime;
    procedure WCDateTime(const Name: String; val: TDateTime);

    procedure SetOutofSpace;
    procedure SetKredits;

    procedure RemoveAutoIndex;
    procedure RemoveAutoBnctest;
    procedure RemoveAutoRules;
    procedure RemoveAutoNuke;
    procedure RemoveAutoDirlist;
    procedure RemoveAutoCrawler;

    procedure AutoBnctest;
    procedure AutoRules;
    procedure AutoDirlist;
    procedure AutoCrawler;
    procedure AutoNuke;
    procedure AutoIndex;
    procedure Auto;

    procedure RecalcFreeslots;
    procedure FullLogin;

    function GetSw: TSiteSw; //< function for sw property to read Site Software from inifile
    procedure SetSw(Value: TSiteSw); //< procedure for sw property to write Site Software to inifile

    function GetRank(const section: String): integer;
    procedure SetRank(const section: String; Value: integer);
    function GetRankLock(const section: String): integer;
    procedure SetRankLock(const section: String; Value: integer);

    function FreeLeechSlots: integer;
    function FreeTraderSlots: integer;
    function SetSections(const sections: String; remove: boolean = False): String;
    function SetLeechers(const users: String; remove: boolean): String;
    function SetTraders(const users: String; remove: boolean): String;
    function IsSection(const section: String): boolean;
    function IsAffil(affil: String): boolean;
    function AddAffil(const affil: String): boolean;
    // TODO function DelAffil(affil: string): Boolean;
//    function SetAffils(affils: String): String;
    function SetAffilsALL(affils: String): String;
    function IsUser(user: String): boolean;
    function IsLeecher(user: String): boolean;
    function IsTrader(user: String): boolean;

    function IsPretimeOk(const section: String; rlz_pretime: TDateTime): boolean;
    function GetPretime(const section: String): String;

    function isRouteableTo(const sitename: String): boolean;
    function isRouteableFrom(const sitename: String): boolean;

    property sections: String read GetSections write SettSections;
    property leechers: String read GetLeechers write SettLeechers;
    property traders: String read GetTraders write SettTraders;
    property users: String read GetUsers;
    property sectiondir[Name: String]: String read GetSectionDir write SetSectionDir;
    property sectionprecmd[Name: String]: String read GetSectionPreCmd write SetSectionPrecmd;
    property siteaffils: String read GetAffils write SetAffils;
    property sectionpretime[Name: String]: integer read GetSectionPreTime write SetSectionPreTime;
    property num_dn: integer read fNumDn write SetNumDn;
    property num_up: integer read fNumUp write SetNumUp;
    property freeslots: integer read fFreeslots write SetFreeSlots;
    property IRCNick: String read getircnick write setircnick;
    property ProxyName: String read GetProxyName write SetProxyName;
    property UserName: String read GetSiteUsername write SetSiteUsername;
    property PassWord: String read GetSitePassword write SetSitePassword;
    property Country: String read GetSiteCountry write SetSiteCountry;
    property MaxUpPerRip: integer read GetSiteMaxUpPerRip write SetSiteMaxUpPerRip;
  published
    property sw: TSiteSw read GetSw write SetSw;
    property noannounce: boolean read GetNoannounce write SetNoAnnounce;
    property working: TSiteStatus read fWorking write SetWorking;
    property max_dn: integer read GetMaxDn write SetMaxDn;
    property max_pre_dn: integer read GetMaxPreDn write SetMaxPreDn;
    property max_up: integer read GetMaxUp write SetMaxUp;
    property maxidle: integer read Getmaxidle write Setmaxidle;
    property idleinterval: integer read Getidleinterval write Setidleinterval;

    property io_timeout: integer read Getio_timeout write Setio_timeout;
    property connect_timeout: integer read Getconnect_timeout write Setconnect_timeout;
    property sslmethod: TSSLMethods read Getsslmethod write Setsslmethod;
    property sslfxp: TSSLReq read Getsslfxp write Setsslfxp;
    property legacydirlist: boolean read Getlegacydirlist write Setlegacydirlist;
    property predir: String read GetPredir write SetPredir;

    property NoLoginMSG: boolean read GetNoLoginMSG write SetNoLoginMSG;
    property UseForNFOdownload: integer read GetUseForNFOdownload write SetUseForNFOdownload;
    property SkipBeingUploadedFiles: boolean read GetSkipBeingUploadedFiles write SetSkipBeingUploadedFiles;
    property PermDown: boolean read GetPermDownStatus write SetPermDownStatus;
    property SkipPre: boolean read GetSkipPreStatus write SetSkipPreStatus;

    property SiteInfos: String read GetSiteInfos write SetSiteInfos;
    property LastCredits: int64 read GetLastKnownCredits write SetLastKnownCredits;
    property UseAutoInvite: Boolean read GetUseAutoInvite write SetUseAutoInvite;

    property IsUp: Boolean read GetIsUp;
    property AutoRulesStatus: integer read GetAutoRulesStatus write SetAutoRulesStatus;

    property SetDownOnOutOfSpace: Boolean read GetSetDownOnOutOfSpace write SetSetDownOnOutOfSpace; //< per site set_down_on_out_of_space setting, uses global if not set
    property SetDownOnOutOfCredits: Boolean read GetSetDownOnOutOfCredits write SetSetDownOnOutOfCredits; //< per site set_down_on_out_of_credits setting, uses global if not set
  end;

function ReadSites(): boolean;
procedure SlotsFire;
procedure SiteAutoStart;

function FindSiteByName(netname, sitename: String): TSite;
function FindSlotByName(slotname: String): TSiteSlot;
procedure SitesInit;
procedure SitesStart;
procedure SitesUninit;
function GiveSiteLastStart: TDateTime;

{ Returns the admin_sitename from slftp.ini as uppercase }
function getAdminSiteName: String;

function SiteSoftWareToString(sitename: String): String; overload;
function SiteSoftWareToString(site: TSite): String; overload;
function StringToSiteSoftWare(s: String): TSiteSw;

function sslMethodToString(sitename: String): String; overload;
function sslMethodToString(site: TSite): String; overload;

{ Checks each sites working property and add it to a formated Stringlist for irc output
  Skips sites with @true noannounce value. Adds ffreeslots & total slot count for sitesup.
  @param(sitesup Stringlist for working (sstUp) sites)
  @param(sitesdn Stringlist for down (sstDown) sites)
  @param(sitesuk Stringlist for unknown (not yet connected) (sstUnknown) sites)
  @param(sitespd Stringlist for permdown (PermDown) sites)
}
procedure SitesWorkingStatusToStringlist(const Netname, Channel: String; var sitesup, sitesdn, sitesuk, sitespd: TStringList);

var
  sitesdat: TEncIniFile = nil;
  sites: TObjectList = nil;
  sitesautosend: TDateTime;

implementation

uses
  SysUtils, irc, DateUtils, configunit, queueunit, debugunit, socks5, console, knowngroups,
  mystrings, versioninfo, mainthread, IniFiles, Math, mrdohutils, taskrace, pazo, globals;

const
  section = 'sites';

var
  bnccsere: TCriticalSection = nil;
  sitelaststart: TDateTime;
  // Config vars
  maxrelogins: integer = 3;
  delay_between_connects: integer = 200;
  admin_siteslots: integer = 10;
  autologin: boolean = False;
  killafter: integer = 0;

function getAdminSiteName: String;
begin
  Result := UpperCase(config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

function SiteSoftWareToString(sitename: String): String;
begin
  Result := SiteSoftWareToString(FindSiteByName('', sitename));
end;

function SiteSoftWareToString(site: TSite): String;
begin
  Result := 'Unknown';

  // sswUnknown, sswGlftpd, sswDrftpd, sswIoftpd
  case TSite(site).Software of
    sswUnknown: Result := 'Unknown';
    sswGlftpd: Result := 'GlFTPD';
    sswDrftpd: Result := 'DrFTPD';
    sswIoftpd: Result := 'ioFTPD';
  end;
end;

function StringToSiteSoftWare(s: String): TSiteSw;
begin
  Result := sswUnknown;
  s := AnsiLowerCase(s);

  if s = 'glftpd' then
    Result := sswGlftpd;
  if s = 'drftpd' then
    Result := sswDrftpd;
  if s = 'ioftpd' then
    Result := sswIoftpd;
end;

function sslMethodToString(sitename: String): String;
begin
  Result := sslMethodToString(FindSiteByName('', sitename));
end;

function sslMethodToString(site: TSite): String;
begin
  Result := 'Unknown';
  case TSite(site).sslmethod of
    sslNone: Result := ' no encryption used';
    sslImplicitSSLv23: Result := ' implicit ssl handshake using SSLv23 after TCP connection was established';
    sslAuthSslSSLv23: Result := ' AUTH SSL then ssl handshake using SSLv23';
    sslAuthTLSSSLv23: Result := ' AUTH TLS then ssl handshake using SSLv23';
    sslAuthSslTLSv1: Result := ' AUTH SSL then ssl handshake using TLSv1';
    sslAuthTlsTLSv1: Result := ' AUTH TLS then ssl handshake using TLSv1';
    sslImplicitTLSv1: Result := ' implicit ssl handshake using TLSv1 after TCP connection was established';
    sslAuthTlsTLSv1_2: Result := ' AUTH TLS then ssl handshake using TLSv12';
    sslImplicitTLSv1_2: Result := ' implicit ssl handshake using TLSv12 after TCP connection was established';
  end;
end;

procedure SitesWorkingStatusToStringlist(const Netname, Channel: String; var sitesup, sitesdn, sitesuk, sitespd: TStringList);
var
  s: TSite;
  i: integer;
begin
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    if s.Name = getAdminSiteName then
      Continue;
    if ((Netname <> 'CONSOLE') and (Netname <> '') and (s.noannounce)) then
      Continue;
    if s.PermDown then
    begin
      sitespd.Add(s.Name);
      Continue;
    end;

    case s.working of
      sstUp: sitesup.Add('<b>' + s.Name + '</b>' + ' (<b>' + IntToStr(s.ffreeslots) + '</b>/' + IntToStr(s.slots.Count) + ')');
      sstDown: sitesdn.Add('<b>' + s.Name + '</b>');
      sstUnknown: sitesuk.Add('<b>' + s.Name + '</b>');
    end;
  end;
end;

// NOTE: ez a fuggveny hivasahoz lokkolni KELL eloszor a mindensegit
function FindSiteByName(netname, sitename: String): TSite;
var
  i: integer;
  s: TSite;
begin
  Result := nil;
  try
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if s.Name = sitename then
      begin
        if ((netname <> '') and (netname <> 'CONSOLE') and (s.noannounce)) then
        begin
          exit;
        end;
        Result := s;
        break;
      end;
    end;
  except
    Result := nil;
  end;
end;

function FindSlotByName(slotname: String): TSiteSlot;
var
  i, j: integer;
begin
  Result := nil;
  try
    for i := 0 to sites.Count - 1 do
    begin
      for j := 0 to TSite(sites[i]).slots.Count - 1 do
      begin
        if TSiteSlot(TSite(sites[i]).slots[j]).Name = slotname then
        begin
          Result := TSiteSlot(TSite(sites[i]).slots[j]);
          exit;
        end;
      end;
    end;
  except
    Result := nil;
  end;
end;

function ReadSites(): boolean;
var
  sitesdatfile: String;
begin
  Result := False;
  sitesdatfile := ExtractFilePath(ParamStr(0)) + 'sites.dat';
  if not FileExists(sitesdatfile) then
  begin
    Debug(dpError, section, 'sites.dat not exists, creating it');
    sitesdat := TEncIniFile.Create(sitesdatfile, passphrase, True);
    sitesdat.WriteString(section, 'default', 'exists');
    sitesdat.UpdateFile;
    Result := True;
  end
  else
  begin
    try
      sitesdat := TEncIniFile.Create(sitesdatfile, passphrase);
      if sitesdat.ReadString(section, 'default', '') = 'exists' then
      begin
        sitesdat.autoupdate := True;
        Result := True;
      end;
    except
      on e: Exception do
        debug(dpError, section, 'Error opening sites.dat: %s', [e.Message])
    end;
  end;
end;

procedure SitesInit;
begin
  sitelaststart := Now();
  bnccsere := TCriticalSection.Create;
  sites := TObjectList.Create;
end;

function CompareSiteNamesForAlphabeticalOrder(site1, site2: TSite): Integer;
begin
  Result := CompareText(site1.Name, site2.Name);
end;

procedure SitesStart;
var
  x: TStringList;
  i: integer;
begin
  debug(dpSpam, section, 'SitesStart begin');

  delay_between_connects := config.readInteger(section, 'delay_between_connects', 200);
  admin_siteslots := config.ReadInteger(section, 'admin_siteslots', 10);
  maxrelogins := config.ReadInteger(section, 'maxrelogins', 3);
  autologin := config.ReadBool(section, 'autologin', False);
  killafter := config.ReadInteger(section, 'killafter', 0);

  // Add admin site
  sites.Add(TSite.Create(getAdminSiteName));

  x := TStringList.Create;
  try
    sitesdat.ReadSections(x);
    for i := 0 to x.Count - 1 do
      if 1 = Pos('site-', x[i]) then
        sites.Add(TSite.Create(Copy(x[i], 6, 1000)));
  finally
    x.Free;
  end;

  // sort sites alphabetical
  sites.Sort(@CompareSiteNamesForAlphabeticalOrder);

  debug(dpSpam, section, 'SitesStart end');
end;

procedure SitesUninit;
begin
  Debug(dpSpam, section, 'Uninit1');

  if sites <> nil then
  begin
    sites.Free;
    sites := nil;
  end;

  if sitesdat <> nil then
  begin
    sitesdat.Free;
    sitesdat := nil;
  end;

  bnccsere.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

{ TSiteSlot }

function GiveSiteLastStart: TDateTime;
begin
  bnccsere.Enter;
  try
    if siteLastStart < Now then
      siteLastStart := Now;
    siteLastStart := IncMilliSecond(sitelaststart, delay_between_connects);
    Result := siteLastStart;
  finally
    bnccsere.Leave;
  end;
end;

procedure TSiteSlot.AddLoginTask;
var
  t: TLoginTask;
begin

  t := TLoginTask.Create('', '', site.Name, False, False);
  t.wantedslot := Name;
  t.startat := GiveSiteLastStart;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.AddLoginTask AddTask: %s',
        [e.Message]));
    end;
  end;
end;

constructor TSiteSlot.Create(site: TSite; no: integer);
begin
  self.site := site;
  self.no := no;
  debug(dpSpam, section, 'Slot %s is creating', [Name]);

  todotask := nil;
  event := TEvent.Create(nil, False, False, Name);
  kilepve := False;

  aktdir := '';
  prot := prNone;
  status := ssNone;
  lastResponse := '';
  lastResponseCode := 0;
  lastio := Now();
  lastactivity := Now();

  mdtmre := TRegExpr.Create;
  mdtmre.Expression := '(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)';

  if (self.site.Name <> getAdminSiteName) then
  begin
    if not site.PermDown then
    begin
      // ha autologin be van kapcsolva akkor -- If auto login is enabled then
      if (((autologin) or (RCBool('autologin', False))) and not site.PermDown) then
        AddLoginTask;
    end
    else
      status := ssMarkedDown;
  end;

  debug(dpSpam, section, 'Slot %s has created', [Name]);
  inherited Create(False);
end;

function TSiteSlot.Name: String;
begin
  Result := Format('%s/%d', [site.Name, no]);
end;

procedure TSiteSlot.DestroySocket(down: boolean);
begin
  try
    Disconnect;
    socks5.Enabled := False;
    Console_Slot_Close(Name);
    prot := prNone;
    aktdir := '';
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in DestroySocket: %s', [e.Message]));
    end;
  end;
  if down then
    status := ssDown
  else
    status := ssOffline;
end;

procedure TSiteSlot.Execute;
var
  tname: String;
begin
  Debug(dpSpam, section, 'Slot %s has started', [Name]);
  tname := 'nil';
  console_add_sitewindow(Name);
  while ((not slshutdown) and (not shouldquit)) do // and (not False)
  begin
    try
      if status = ssOnline then
        Console_Slot_Add(Name, 'Idle...');

      if ((todotask <> nil) and (not queue_debug_mode)) then
      begin
        try
          try
            tname := todotask.Name;
          except
            on E: Exception do
              Debug(dpError, section,
                Format('[EXCEPTION] TSiteSlot.Execute(todotask.name) %s: %s',
                [tname, e.Message]));
          end;

          Debug(dpSpam, section, Format('--> %s', [Name]));

          try
            if todotask.Execute(self) then
              lastactivity := Now();

          except
            on E: Exception do
              Debug(dpError, section,
                Format('[EXCEPTION] TSiteSlot.Execute(if todotask.Execute(self) then) %s: %s', [tname, e.Message]));
          end;
          Debug(dpSpam, section, Format('<-- %s', [Name]));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Execute %s: %s',
              [tname, e.Message]));
          end;
        end;

        uploadingto := False;
        downloadingfrom := False;

        if (todotask <> nil) then
        begin
          try
            try
              if (todotask.slot1 <> nil) then
              begin
                todotask.slot1 := nil;
              end;
            finally
              todotask := nil;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section,
                Format('[EXCEPTION] TSiteSlot.Execute : Exception remove todotask : %s',
                [e.Message]));
            end;
          end;
        end;

        if ((not shouldquit) and (not slshutdown)) then
        begin
          QueueFire;
        end;
      end
      else
      begin
        //event.WaitFor($FFFFFFFF);
        case event.WaitFor(15 * 60 * 1000) of
          wrSignaled: { Event fired. Normal exit. }
            begin

            end;
        else { Timeout reach }
          begin
            if spamcfg.readbool(section, 'siteslot_recycle', False) then
              irc_Adderror('TSiteSlot.Execute: <c2>Force Leave</c>:' +
                Name + ' SiteSlot Recycle 15min');
            Debug(dpSpam, section, 'TSiteSlot.Execute: Force Leave:' +
              Name + ' SiteSlot Recycle 15min');
          end;
        end;
      end;

    except
      on E: Exception do
      begin
        Debug(dpError, section, '[Exception] Slot exception : %s', [e.Message]);
        try
          todotask := nil;
        except
          on e: Exception do
          begin
            Debug(dpError, section,
              Format('[EXCEPTION] TSiteSlot.Execute : Exception remove todotask : %s',
              [e.Message]));
            break;
          end;
        end;
      end;
    end;
  end;
  console_delwindow(Name);
  kilepve := True;
end;

destructor TSiteSlot.Destroy;
begin
  Debug(dpSpam, section, 'Slot %s destroy begin', [Name]);
  Stop;
  DestroySocket(True);

  FreeAndNil(event);
  mdtmre.Free;

  inherited;
  Debug(dpSpam, section, 'Slot %s destroy end', [Name]);
end;

function TSiteSlot.SendProtC: boolean;
begin
  Result := False;
  if prot <> prProtC then
  begin
    if not Send('PROT C') then
      exit;
    if not Read('PROT C') then
      exit;

    prot := prProtC;
  end;
  Result := True;
end;

function TSiteSlot.SendProtP: boolean;
begin
  Result := False;
  if prot <> prProtP then
  begin
    if not Send('PROT P') then
      exit;
    if not Read('PROT P') then
      exit;

    prot := prProtP;
  end;
  Result := True;
end;

procedure TSiteSlot.ProcessFeat;
begin
  {
  * GLFTPD *
    211- Extensions supported:
     AUTH TLS
     AUTH SSL
     PBSZ
     PROT
     CPSV
     SSCN
     MDTM
     SIZE
     REST STREAM
     SYST

  * the three below added with glFTPd 2.08 *
     EPRT
     EPSV
     CEPR

    211 End
  }

  {
  * DRFTPD *
    211-Extensions supported:
     PRET
     AUTH SSL
     PBSZ
     CPSV
     SSCN
     CLNT
     NOOP
     MLST type*,x.crc32*,size*,modify*,unix.owner*,unix.group*,x.slaves*,x.xfertime*
    211 End
  }

  {
  * IOFTPD *
    FEAT
    500 'FEAT': Command not understood
    * found on https://bugs.kde.org/show_bug.cgi?id=114100

  * IOFTPD  7.7.3 *
    211-Extensions supported:
     AUTH SSL
     AUTH TLS
     CLNT
     CPSV
     LIST -1aAdflLRsTU
     MDTM
     MDTM YYYYMMDDHHMMSS filename
     PBSZ
     PROT
     REST STREAM
     SIZE
     SSCN
     STAT -1aAdflLRsTU
     TVFS
     XCRC filename;start;end
    211 END
  }
  if (0 < Pos('PRET', lastResponse)) then
  begin
    if site.sw <> sswDrftpd then
      sitesdat.WriteInteger('site-' + site.Name, 'sw', integer(sswDrftpd));
  end
  else if (0 < Pos('Command not understood', lastResponse)) or (0 < Pos('TVFS', lastResponse)) or (0 < Pos('XCRC', lastResponse)) then
  begin
    if site.sw <> sswIoftpd then
      sitesdat.WriteInteger('site-' + site.Name, 'sw', integer(sswIoftpd));
  end
  else if (0 < Pos('CPSV', lastResponse)) then
  begin
    if site.sw <> sswGlftpd then
      sitesdat.WriteInteger('site-' + site.Name, 'sw', integer(sswGlftpd));
  end;
end;

function TSiteSlot.Cwd(dir: String; force: boolean = False): boolean;
begin
  Result := False;
  dir := MyIncludeTrailingSlash(dir);

  if ((dir <> aktdir) or (force)) then
  begin
    if ((site.legacydirlist) or (force)) then
    begin
      if not Send('CWD %s', [dir]) then
        exit;
      if not Read('CWD') then
        exit;

      if (lastResponseCode = 250) then
      begin
        if (0 <> Pos('250- Matched ', lastresponse)) then
        begin
          Debug(dpError, section, 'TRIMMED RLSNAME DETECTED! ' + Name + ' ' + dir);

          if dir[1] <> '/' then
            aktdir := aktdir + dir
          else
            aktdir := dir;

          Result := True;
          exit;
        end;
        (*
                if (0 <> Pos('Looks like this is a pre', lastresponse)) then
                  pre:= True;
        *)
        if dir[1] <> '/' then
          aktdir := aktdir + dir
        else
          aktdir := dir;
      end
      else
      begin
        //irc_addtext(todotask, '%s: %s', [name, trim(lastResponse)]);
        Result := False;
        exit;
      end;
    end
    else
    begin
      if dir[1] <> '/' then
        aktdir := aktdir + dir
      else
        aktdir := dir;
    end;
  end;
  Result := True;
end;

function TSiteSlot.LoginBnc(const i: integer; kill: boolean = False): boolean;
var
  sslm: TSSLMethods;
  un, upw, tmp: String;
  bncList, splitted: TStringList;
  j: Integer;
  currentBnc, tmpBnc, tmpHost: String;
  tmpPort: Integer;
begin
  Result := False;

  if (self.site.Name = getAdminSiteName) then
  begin
    Result := True;
    exit;
  end;

  if ((site.proxyname = '!!NOIN!!') or (site.proxyname = '0') or (site.proxyname = '')) then
    SetupSocks5(self, (not RCBool('nosocks5', False)) and (config.ReadBool(section, 'socks5', False)))
  else
    mSLSetupSocks5(site.proxyname, self, True);

  //First step to connect
  Host := RCString('bnc_host-' + IntToStr(i), '');
  Port := RCInteger('bnc_port-' + IntToStr(i), 0);
  Connect(site.connect_timeout * 1000);

  peerport := slSocket.PeerPort;
  peerip := slSocket.PeerIP;
  localport := slSocket.localPort;

  sslm := TSSLMethods(site.sslmethod);
  if sslm in [sslImplicitSSLv23, sslImplicitTLSv1, sslImplicitTLSv1_2] then
  begin
    if sslm = sslImplicitTLSv1_2 then
      SetSSLContext(slTLSv1_2)
    else
      SetSSLContext(slTLSv1);
    if not TurnToSSL(site.io_timeout * 1000) then
      exit;
  end;

  // banner
  if not Read('BANNER') then
    exit;

  if (lastResponseCode <> 220) then
  begin
    error := Trim(lastResponse);
    exit;
  end;

  if (sslm in [sslAuthSslSSLv23, sslAuthSslTLSv1, sslAuthTlsSSLv23, sslAuthTlsTLSv1, sslAuthTlsTLSv1_2]) then
  begin
    if sslm in [sslAuthSslSSLv23, sslAuthTlsSSLv23] then
      SetSSLContext(slSslv23);

    if sslm in [sslAuthTlsTLSv1] then
      SetSSLContext(slTLSv1);

    if sslm in [sslAuthTlsTLSv1_2] then
      SetSSLContext(slTLSv1_2);

    if sslm in [sslAuthSslSSLv23, sslAuthSslTLSv1] then
      tmp := 'AUTH SSL'
    else
      tmp := 'AUTH TLS';

    // trying AUTH SSL|TLS
    if not Send(tmp) then
      exit;
    if not Read('AUTH') then
      exit;

    if lastResponseCode <> 234 then
      exit;

    if not TurnToSSL(site.io_timeout * 1000) then
      exit;
  end;
  //else
  //  Debug(dpMessage, section, '%s: TRYING PLAINTEXT LOGIN', [name]);

  un := self.site.UserName;
  upw := self.site.PassWord;

  // to bypass welcome message you have to use '-' as first char on your password
  // WORKS ONLY @ GLFTPD
  if site.sw = sswGlftpd then
    if self.site.NoLoginMSG then
      upw := '-' + upw;

  // to kill ghost logins you need to use '!' as first char on your username
  if (kill) then
  begin
    un := '!' + un;
  end;

  if not Send('USER %s', [un]) then
    exit;
  if not Read('USER') then
    exit;

  if lastResponseCode <> 331 then
  begin
    error := Trim(lastResponse);
    exit;
  end;

  if not Send('PASS %s', [upw]) then
    exit;
  if not Read('PASS') then
    exit;

  if lastResponseCode <> 230 then
  begin
    error := Trim(lastResponse);
    exit;
  end;

  if not Send('TYPE I') then
    exit;
  if not Read('TYPE I') then
    exit;

  if (TSiteSw(RCInteger('sw', 0)) = sswUnknown) then
  begin
    if not Send('FEAT') then
      exit;
    if not Read('FEAT') then
      exit;

    ProcessFeat();
  end;

  if not Send('SITE XDUPE 3') then
    exit;
  if not Read('XDUPE') then
    exit;

  if (site.sslfxp = srNeeded) then
  begin
    if (not SendProtP()) then
      exit;
  end;

  if (TSiteSw(RCInteger('sw', 0)) = sswDrftpd) then
  begin
    if (not Send('CLNT %s', [GetFullVersionString])) then
      exit;
    if not Read('CLNT') then
      exit;
  end;

  if (site.predir <> '') then
  begin
    if not Cwd(site.predir) then
      if status = ssDown then
        exit;
  end;

  // successful login
  Result := True;

  // change order of bnc if the current successfull bnc is not the first
  if i <> 0 then
  begin
    bncList := TStringList.Create;
    bncList.CaseSensitive := False;
    bncList.Duplicates := dupIgnore;
    splitted := TStringList.Create;
    bnccsere.Enter;
    try
      currentBnc := Host + ':' + IntToStr(Port);
      bncList.Add(currentBnc);

      j := 0;
      while (True) do
      begin
        tmpHost := RCString('bnc_host-' + IntToStr(j), '');

        // reached end of bnc list for this site
        if tmpHost = '' then
          break;

        tmpPort := RCInteger('bnc_port-' + IntToStr(j), 0);
        tmpBnc := tmpHost + ':' + IntToStr(tmpPort);

        // skip active bnc
        if tmpBnc <> currentBnc then
          bncList.Add(tmpBnc);

        inc(j)
      end;

      // Something went wrong populating the new bnc list. Exiting
      if bncList.Count < 1 then
      begin
        Debug(dpError, section, '[bncsort] Error re-ordering bnc list. New bnc list count is %d.', [bncList.Count]);
        exit;
      end;

      // Clear current bnclist
      j := 0;
      while (True) do
      begin
        if RCString('bnc_host-' + IntToStr(j), '') = '' then
          break;

        sitesdat.DeleteKey('site-' + site.Name, 'bnc_host-' + IntToStr(j));
        sitesdat.DeleteKey('site-' + site.Name, 'bnc_port-' + IntToStr(j));
        Debug(dpSpam, section, '[bncsort] Removed BNC from %s: %s', [site.Name, RCString('bnc_host-' + IntToStr(j), '') + ':' + IntToStr(RCInteger('bnc_port-' + IntToStr(j), 0))]);
        inc(j)
      end;

      // Re-add sorted bnc list
      for j := 0 to bncList.Count - 1 do
      begin
        splitString(bncList[j], ':', splitted);
        tmpHost := splitted[0];
        tmpPort := StrToInt(splitted[1]);
        Debug(dpSpam, section, '[bncsort] Added BNC to %s: %s', [site.Name, tmpHost + ':' + IntToStr(tmpPort)]);

        sitesdat.WriteString('site-' + site.Name, 'bnc_host-' + IntToStr(j), tmpHost);
        sitesdat.WriteInteger('site-' + site.Name, 'bnc_port-' + IntToStr(j), tmpPort);
      end;
    finally
      bnccsere.Leave;
      FreeAndNil(bncList);
      FreeAndNil(splitted);
    end;
  end;

  if spamcfg.readbool(section, 'login_logout', False) then
    irc_SendRACESTATS(Format('LOGIN <b>%s</b> (%s)', [site.Name, Name]));

  status := ssOnline;
end;

function TSiteSlot.Login(kill: boolean = False): boolean;
var
  host: String;
  i: integer;
begin
  Result := False;

  i := 0;
  while ((not slshutdown) and (not shouldquit)) do
  begin
    if i > 20 then
      Break;

    try
      host := RCString('bnc_host-' + IntToStr(i), '');
      if host = '' then
        break;
      if Result then
        Break;

      Result := LoginBnc(i, kill);
      if Result then
      begin
        Break;
      end;

      if (((lastResponseCode = 530) and (0 <> Pos('your account is restricted to', lastResponse))) or
        ((lastResponseCode = 530) and (0 <> Pos('your maximum number of connections', lastResponse)))) then
      begin
        if site.sw = sswGlftpd then
        begin
          DestroySocket(False);
          Result := LoginBnc(i, True);
        end;
      end
      else
      begin
        irc_Adderror(todotask, '<c4>[ERROR Login]</c> %s@%s:: %s', [Name, bnc, error]);
        if ((lastResponseCode = 421) and (0 <> Pos('Hammer Protection', lastResponse))) then
        begin
          break;
        end;
      end;

      Inc(i);
    except
      break;
    end;
  end;

  if ((not slshutdown) and (not shouldquit)) then
    if not Result then
    begin
      if (((lastResponseCode = 530) and (0 <> Pos('your account is restricted to', lastResponse))) or
        ((lastResponseCode = 530) and (0 <> Pos('your maximum number of connections', lastResponse)))) then
      begin
        DestroySocket(False);
      end
      else
      begin
        DestroySocket(False);
        irc_addtext(todotask, '<c4>SLOT <b>%s</b> IS DOWN</c>', [Name]);
      end;
    end;
end;

function TSiteSlot.ReLogin(limit_maxrelogins: integer = 0; kill: boolean = False; s_message: String = ''): boolean;
var
  l_maxrelogins: integer;
  relogins: integer;
  i: integer;
  ss: TSiteSlot;
begin
  Result := False;
  Debug(dpSpam, section, 'Relogin ' + Name + ' ' + IntToStr(limit_maxrelogins));

  if limit_maxrelogins = 0 then
    l_maxrelogins := maxrelogins
  else
    l_maxrelogins := limit_maxrelogins;

  if Status = ssOnline then
  begin
    Result := True;
    exit;
  end;

  relogins := 0;
  while ((relogins < l_maxrelogins) and (not slshutdown) and (not shouldquit)) do
  begin
    try
      if relogins > 10 then
        Break;
    except
      Break;
    end;
    Result := Login(kill);
    if Result then
      Break;

    if ((lastResponseCode = 421) and (0 <> Pos('Hammer Protection', lastResponse))) then
    begin
      break;
    end;
    if (killafter <> 0) then
    begin
      if (relogins > killafter) then
        kill := True;
    end;
    sleep(delay_between_connects);
    Inc(relogins);
  end;

  if ((not slshutdown) and (not shouldquit)) then
  begin
    if not Result then
    begin
      if ((lastResponseCode = 421) and (0 <> Pos('Hammer Protection', lastResponse))) then
      begin
        site.working := sstDown;
        exit;
      end;

      if ((lastResponseCode = 234) and (0 <> Pos('234 AUTH TLS successful', lastResponse))) then
      begin
        irc_addtext(todotask, '<c4>SITE <b>%s</b></c> WiLL DOWN, maybe enforce TLS?', [site.Name]);
        site.working := sstDown;
        exit;
      end;

      irc_addtext(todotask, '<c4>SITE <b>%s</b></c> WiLL DOWN %s - lastResponse: %d %s', [site.Name, s_message, lastResponseCode, lastResponse]);
      for i := 0 to site.slots.Count - 1 do
      begin
        ss := TSiteSlot(site.slots[i]);
        if ss.Status = ssOnline then
        begin
          // we have at least one slot up and running, so no need to setdown the site
          exit;
        end;
      end;
      site.working := sstDown;
    end;
  end;
end;

procedure TSiteSlot.Fire;
begin
  event.SetEvent;
end;

function TSiteSlot.Read(read_cmd: String = ''): boolean;
begin
  try
    Result := Read(read_cmd, True, True, 0);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read: %s', [e.Message]));
      lastResponse := '';
      lastResponseCode := 0;
      Result := False;
      exit;
    end;
  end;
end;

function TSiteSlot.Read(const read_cmd: String; raiseontimeout: boolean; raiseonclose: boolean; timeout: integer = 0): boolean;
label
  ujra;
var
  aktread: String;
  numreads: integer;
  read_start: TDateTime;
begin
  numreads := 0;
  lastResponse := '';
  lastResponseCode := 0;
  Result := False;
  if ((timeout = 0) and (read_cmd = 'read_cmd')) then
    timeout := site.connect_timeout * 1000;
  if timeout = 0 then
    timeout := site.io_timeout * 1000;

  ujra:
  Inc(numreads);
  if numreads > 500 then
  begin
    Debug(dpError, section, Format('[ERROR] TSiteSlot.Read numreads', []));
    lastResponse := '';
    lastResponseCode := 0;
    error := 'TSiteSlot.Read numreads';
    Result := False;
    exit;
  end;

  try
    read_start := Now;
    if not Read(aktread, timeout) then
    begin
      if (error = 'exception') then
        exit;

      if ((error = 'timeout') and (not raiseontimeout)) then
        exit;

      DestroySocket(False);
      if raiseOnClose then
      begin
        irc_Adderror(todotask, '<c4>[ERROR Read]</c> %s: %s %s %d/%d (%s)', [Name, read_cmd, error, MilliSecondsBetween(Now, read_start), timeout, bnc]);
        Result := False;
      end;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read: %s', [e.Message]));
      lastResponse := '';
      lastResponseCode := 0;
      error := 'TSiteSlot.Read';
      Result := False;
      exit;
    end;
  end;

  try
    lastResponse := lastResponse + aktread;
    //Debug(dpSpam, 'protocol', name+' <<'+#13#10+aktread);
    lastResponseCode := ParseResponseCode(lastResponse);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read ParseResponseCode: %s',
        [e.Message]));
      lastResponse := '';
      lastResponseCode := 0;
      error := 'TSiteSlot.Read ParseResponseCode';
      Result := False;
      exit;
    end;
  end;

  if (lastResponseCode <> 230) then
  begin
    console_addline(Name, aktread);
  end;

  if ((lastResponseCode >= 1000) or (lastResponseCode < 100)) then // auto read more
    goto ujra;

  lastio := Now();

  Result := True;
end;

function TSiteSlot.Send(const s: String): boolean;
begin
  Result := False;
  try
    Console_Slot_Add(Name, s);
    console_addline(Name, s);

    if not WriteLn(s, site.io_timeout * 1000) then
    begin
      irc_Adderror(todotask, '<c4>[ERROR Send]</c> %s: %s (%s)', [Name, error, s]);
      DestroySocket(False);
      exit;
    end;

    lastio := Now();
    Result := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Send: %s : %s', [e.Message, s]));
      Result := False;
      exit;
    end;
  end;
end;

function TSiteSlot.Send(const s: String; const Args: array of const): boolean;
begin
  try
    Result := Send(Format(s, Args));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Send: %s : %s', [e.Message, s]));
      Result := False;
      exit;
    end;
  end;
end;

function TSiteSlot.RCInteger(Name: String; def: integer): integer;
begin
  Result := site.RCInteger(Name, def);
end;

function TSiteSlot.RCDateTime(Name: String; def: TDateTime): TDateTime;
begin
  Result := site.RCDateTime(Name, def);
end;

function TSiteSlot.RCString(Name, def: String): String;
begin
  Result := site.RCString(Name, def);
end;

procedure TSiteSlot.SetOnline(Value: TSlotStatus);
begin
  fStatus := Value;

  if (fStatus = ssOnline) then
    site.working := sstUp;
end;

function TSiteSlot.bnc: String;
begin
  Result := Host + ':' + IntToStr(Port);
end;

procedure TSiteSlot.Quit;
begin
  if status <> ssOnline then
    exit;

  if (not Send('QUIT')) then
    exit;
  Read('QUIT', False, False);
  DestroySocket(False);
end;

function TSiteSlot.RCBool(Name: String; def: boolean): boolean;
begin
  Result := site.RCBool(Name, def);
end;

function TSiteSlot.RemoveFile(const dir, filename: String): boolean;
var
  cmd: String;
begin
  Result := False;
  if site.legacydirlist then
  begin
    if not Cwd(dir) then
      exit;
    cmd := 'DELE ' + filename;
  end
  else
    cmd := 'DELE ' + MyIncludeTrailingSlash(dir) + filename;

  if not Send(cmd) then
    exit;
  if not Read('DELE') then
    exit;

  Result := True;
end;

function TSiteSlot.RemoveDir(dir: String): boolean;
var
  cmd: String;
  feljebb: String;
begin
  Result := False;
  if dir = '' then
    exit;

  if dir[Length(dir)] = '/' then
    dir := Copy(dir, 1, Length(dir) - 1);
  if site.legacydirlist then
  begin
    feljebb := Copy(dir, 1, Rpos('/', dir));
    if not Cwd(feljebb) then
      exit;
    cmd := 'RMD ' + Copy(dir, Rpos('/', dir) + 1, 1000);
  end
  else
    cmd := 'RMD ' + dir;

  if not Send(cmd) then
    exit;
  if not Read('RMD') then
    exit;

  Result := True;
end;

function TSiteSlot.Mkdir(const dirtocreate: String): boolean;
var
  dir: String;
begin
  Result := False;
  try
    if (site.legacydirlist) then
    begin
      dir := dirtocreate;
    end
    else
      dir := aktdir + dirtocreate;

    if not Send('MKD %s', [dir]) then
      exit;
    if not Read('MKD') then
      exit;

    Result := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Mkdir: %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSiteSlot.Pwd(var dir: String): boolean;
begin
  Result := False;
  try
    if not Send('PWD') then
    begin
      Debug(dpError, section, '[PWD] Could not send command PWD to :%s', [site.Name]);
      exit;
    end;
    if not Read('PWD') then
    begin
      Debug(dpError, section, '[PWD] Could not read PWD answer from :%s', [site.Name]);
      exit;
    end;
    if lastResponseCode <> 257 then
    begin
      Debug(dpError, section, '[PWD] Last response code not expected :%d', [lastResponseCode]);
      exit;
    end;

    dir := Copy(lastResponse, 6, 100);
    dir := Copy(dir, 1, Pos('"', dir) - 1);

    aktdir := MyIncludeTrailingSlash(dir);
    Result := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Pwd: %s', [e.Message]);
      Result := False;
    end;
  end;
end;


function TSiteSlot.Dirlist(const dir: String; forcecwd: boolean = False; fulldirlist: boolean = False): boolean;
var
  cmd, list_everything: String;
begin
  Result := False;
  list_everything := '';

  {
  Difference between STAT -l and STAT -la on GLFTPD and DRFTPD, see below:
  * GLFTPD
  [L] 213- status of -l ZABKAT.xplorer2.Ult.v3.3.0.2.x64.Multilingual.Incl.Patch.and.Keymaker-ZWT:
  [L] total 5535
  [L] drwxrwxrwx   2 uname     NoGroup         0 Feb 20 11:01 [ABC] - ( 3M 1F - COMPLETE ) - [ABC]
  [L] -rw-r--r--   1 uname     NoGroup       125 Feb 19 13:02 file_id.diz
  [L] -rw-r--r--   1 uname     NoGroup   2822461 Feb 20 11:01 zh6khopy.zip
  [L] -rw-r--r--   1 uname     NoGroup      6359 Feb 19 13:02 zwt.nfo
  [L] 213 End of Status

  [L] 213- status of -la ZABKAT.xplorer2.Ult.v3.3.0.2.x64.Multilingual.Incl.Patch.and.Keymaker-ZWT:
  [L] total 5553
  [L] drwxrwxrwx   3 uname     NoGroup      2763 Feb 20 11:01 .
  [L] drwxrwxrwx  38 glftpd   glftpd          0 Feb 20 22:01 ..
  [L] -rw-rw-rw-   1 uname     NoGroup       923 Feb 20 11:01 .message
  [L] drwxrwxrwx   2 uname     NoGroup         0 Feb 20 11:01 [ABC] - ( 3M 1F - COMPLETE ) - [ABC]
  [L] -rw-r--r--   1 uname     NoGroup       125 Feb 19 13:02 file_id.diz
  [L] -rw-r--r--   1 uname     NoGroup   2822461 Feb 20 11:01 zh6khopy.zip
  [L] -rw-r--r--   1 uname     NoGroup      6359 Feb 19 13:02 zwt.nfo
  [L] 213 End of Status

  * DRFTPD
  * same result for both commands on my side (tested with 1 site)
  }

  try
    if fulldirlist then
      list_everything := 'a';

    if dir <> '' then
      if not Cwd(dir, forcecwd) then
      begin
        Debug(dpMessage, section, 'TSiteSlot.Dirlist ERROR: can not CWD to %s on %s', [dir, site.Name]);
        exit;
      end;

    if config.ReadBool('indexer', 'use_custom_dirlist_command', False) then
    begin
      if ((dir = '') or (site.legacydirlist) or (forcecwd)) then
        cmd := config.ReadString('indexer', 'custom_dirlist_command', 'list -al')
      else if dir[1] = '/' then
        cmd := config.ReadString('indexer', 'custom_dirlist_command', 'list -al') + ' ' + MyIncludeTrailingSlash(dir)
      else
        cmd := config.ReadString('indexer', 'custom_dirlist_command', 'list -al') + ' ' + aktdir + MyIncludeTrailingSlash(dir);
    end
    else
    begin
      if ((dir = '') or (site.legacydirlist) or (forcecwd)) then
        cmd := 'STAT -l' + list_everything
      else if dir[1] = '/' then
        cmd := 'STAT -l' + list_everything + ' ' + MyIncludeTrailingSlash(dir)
      else
        cmd := 'STAT -l' + list_everything + ' ' + aktdir + MyIncludeTrailingSlash(dir);
    end;

    if not Send(cmd) then
    begin
      Debug(dpMessage, section, 'TSiteSlot.Dirlist ERROR: can not send command %s to %s', [cmd, site.Name]);
      exit;
    end;

    if not Read('Dirlist') then
    begin
      Debug(dpMessage, section, 'TSiteSlot.Dirlist ERROR: can not read answer of %s from %s', [cmd, site.Name]);
      exit;
    end;

    Result := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Dirlist: %s', [e.Message]);
    end;
  end;
end;

function TSiteSlot.Leechfile(dest: TStream; const filename: String; restFrom: Integer = 0; maxRead: Integer = 0): Integer;
var
  idTCP: TslTCPSocket;
  host: String;
  port: Integer;
begin
  Result := 0;

  // stop using sites where you don't add some download slots
  (* TODO: Write a function which can be used before from every caller to this function + depend check if PRE or not *)
  if ( (site.max_pre_dn = 0) or (site.max_dn = 0) ) then
    exit;

  try
    idTCP := TslTCPSocket.Create;

    try
      if not SendProtP then
        exit;

      if site.sw = sswDrftpd then
      begin
        if not Send('PRET RETR %s', [TranslateFilename(filename)]) then
          exit;
        if not Read('PRET RETR %s') then
          exit;
      end;

      if not Send('PASV') then
        exit;
      if not Read('PASV') then
        exit;

      if (lastResponseCode <> 227) then
      begin
        irc_addtext(todotask, Trim(lastResponse));
        Result := -1;
        exit;
      end;
      ParsePASVString(lastResponse, host, port);
      if port = 0 then
      begin
        irc_Adderror(todotask, '<c4>[LEECHFILE ERROR]</c>: Could not parse PASV string from site %s while getting %s', [site.name, filename]);
        Result := -1;
        exit;
      end;

      idTCP.Host := host;
      idTCP.Port := port;

      if not Send('REST %d', [restFrom]) then
        exit;
      if not Read('REST') then
        exit;

      if not Send('RETR %s', [TranslateFilename(filename)]) then
        exit;

      if not idTCP.Connect(site.connect_timeout * 1000) then
      begin
        irc_Adderror(todotask, '<c4>[LEECHFILE ERROR]</c>: Can not connect to site %s while getting %s: %s', [site.name, filename, idTCP.error]);
        DestroySocket(False);
        Result := -1;
        exit;
      end;

      if not idTCP.TurnToSSL(site.io_timeout * 1000) then
      begin
        irc_Adderror(todotask, '<c4>[LEECHFILE ERROR]</c>: SSL negotiation with site %s while getting %s: %s', [site.name, filename, idTCP.error]);
        site.UseForNFOdownload := 2; // TODO: rename me
        DestroySocket(False);
        Result := -1;
        exit;
      end;

      if not Read('RETR') then
      begin
        irc_Adderror(todotask, '<c4>[LEECHFILE ERROR]</c>: No response from site %s while getting %s: %s', [site.name, filename]);
        Result := -1;
        exit;
      end;

      if not idTCP.Read(dest, site.io_timeout * 1000, maxRead, True) then
      begin
        irc_Adderror(todotask, '<c4>[LEECHFILE ERROR]</c>: Could not get file content on site %s while getting %s: %s', [site.name, filename, idTCP.error]);
        DestroySocket(False);
        Result := -1;
        exit;
      end;

      idTCP.Disconnect;

      if not Read() then
        exit;

      Result := 1;
    finally
      idTCP.Free;
    end;

  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.LeechFile : %s', [e.Message]));
      exit;
    end;
  end;
end;

function TSiteSlot.TranslateFilename(const filename: String): String;
begin
  Result := filename;
  if ((filename[1] <> '/') and (not site.legacydirlist)) then
    Result := aktdir + filename;
end;

procedure TSiteSlot.SetDownloadingFrom(const Value: boolean);
begin
  if Value <> fDownloadingFrom then
  begin
    bnccsere.Enter;
    fDownloadingFrom := Value;
    if fDownloadingFrom then
    begin
      site.num_dn := site.num_dn + 1;
      Debug(dpSpam, section, 'Site %s: Download slots in use: %d!', [site.Name,site.num_dn ]);
    end
    else
    begin
      site.num_dn := site.num_dn - 1;
      Debug(dpSpam, section, 'Site %s: Download slots in use: %d!', [site.Name,site.num_dn ]);
    end;
    bnccsere.Leave;
  end;
end;

procedure TSiteSlot.SetUploadingTo(const Value: boolean);
begin
  if Value <> fUploadingTo then
  begin
    bnccsere.Enter;
    fUploadingTo := Value;
    if fUploadingTo then
      begin
        site.num_up := site.num_up + 1;
        Debug(dpSpam, section, 'Site %s: Upload slots in use: %d!', [site.Name,site.num_up ]);
      end
    else
      begin
        site.num_up := site.num_up - 1;
        Debug(dpSpam, section, 'Site %s: Upload slots in use: %d!', [site.Name,site.num_up ]);
      end;
    bnccsere.Leave;
  end;
end;

procedure TSiteSlot.SetTodotask(Value: TTask);
begin
  if fTodotask <> Value then
  begin
    bnccsere.Enter;
    fTodotask := Value;
    if fTodoTask <> nil then
      begin
        site.freeslots := site.freeslots - 1;
        Debug(dpSpam, section, 'Site %s: Free slots: %d!', [site.Name,site.freeslots ]);
        end
    else
      begin
        site.freeslots := site.freeslots + 1;
        Debug(dpSpam, section, 'Site %s: Free slots: %d!', [site.Name,site.freeslots ]);
      end;
    bnccsere.Leave;
  end;
end;

{ TSite }

constructor TSite.Create(const Name: String);
var
  i, j: integer;
  ss, affils: String;
begin
  if (Name = getAdminSiteName) then
  begin
    self.Name := Name;
    working := sstUp;

    slots := TObjectList.Create();
    for i := 1 to admin_siteslots do
      slots.Add(TSiteSlot.Create(self, i - 1));

    RecalcFreeslots;

    exit;
  end;
  //  siteinvited:= False;
  self.Name := Name;

  debug(dpSpam, section, 'Site %s is creating', [Name]);

  foutofannounce := 0;
  // nullazni a felfedezendo beallitasokat
  sitesdat.WriteInteger('site-' + Name, 'sw', integer(sswUnknown));
  working := sstUnknown;

  // rakjuk rendbe a direket
  if ((RCString('predir', '') <> '') and (sectiondir['PRE'] = '')) then
  begin
    sectiondir['PRE'] := RCString('predir', '');
    sitesdat.DeleteKey('site-' + self.Name, 'predir');
  end;

  // es a precmd-ket is
  if ((RCString('precmd', '') <> '') and (sectionprecmd['PRE'] = '')) then
  begin
    sectionprecmd['PRE'] := RCString('precmd', '');
    sitesdat.DeleteKey('site-' + self.Name, 'precmd');
  end;

  slots := TObjectList.Create();
  for i := 1 to RCInteger('slots', 2) do
    slots.Add(TSiteSlot.Create(self, i - 1));

  RecalcFreeslots;

  for i := 1 to 1000 do // convert section affils to new global affil format
  begin
    ss := SubString(self.sections, ' ', i);
    if ss = '' then
      Break;
    affils := RCString('affils-' + ss, '');
    DeleteKey('affils-' + ss);
    if affils = '' then
      Continue;
    for j := 1 to 1000 do
    begin
      ss := SubString(affils, ' ', j);
      if ss = '' then
        Break;
      self.AddAffil(ss);
    end;
  end;

  debug(dpSpam, section, 'Site %s has created', [Name]);
end;

function TSite.isRouteableTo(const sitename: String): boolean;
var
  y: TStringList;
begin
  y := TStringList.Create;
  y.Sorted := True;
  try
    sitesdat.ReadSection('speed-to-' + sitename, y);
    if y.IndexOf(self.Name) = -1 then
      Result := False
    else
      Result := True;
  finally
    y.Free;
  end;
end;

function TSite.isRouteableFrom(const sitename: String): boolean;
var
  y: TStringList;
begin
  y := TStringList.Create;
  y.Sorted := True;
  try
    sitesdat.ReadSection('speed-from-' + self.Name, y);
    if y.IndexOf(sitename) = -1 then
      Result := False
    else
      Result := True;
  finally
    y.Free;
  end;
end;

procedure TSiteSlot.Stop;
begin
  if event <> nil then
  begin
    Debug(dpSpam, section, 'Slot %s stop begin', [Name]);
    shouldquit := True;
    event.SetEvent;
    inherited;
    Debug(dpSpam, section, 'Slot %s stop end', [Name]);
  end;
end;

procedure TSite.Stop;
var
  i: integer;
begin
  Debug(dpSpam, section, 'Site %s stop begin', [Name]);
  for i := 0 to slots.Count - 1 do
    TSiteSlot(slots[i]).Stop;
  Debug(dpSpam, section, 'Site %s stop end', [Name]);
end;

procedure TSite.DeleteKey(const Name: String);
begin
  sitesdat.DeleteKey('site-' + self.Name, Name);
end;

function TSite.RCString(const Name: String; const def: String): String;
begin
  Result := sitesdat.ReadString('site-' + self.Name, Name, def);
end;

procedure TSite.WCString(const Name: String; const val: String);
begin
  sitesdat.WriteString('site-' + self.Name, Name, val);
end;

function TSite.RCInteger(const Name: String; const def: integer): integer;
begin
  Result := sitesdat.ReadInteger('site-' + self.Name, Name, def);
end;

procedure TSite.WCInteger(const Name: String; const val: integer);
begin
  sitesdat.WriteInteger('site-' + self.Name, Name, val);
end;

function TSite.RCBool(const Name: String; const def: boolean): boolean;
begin
  Result := sitesdat.ReadBool('site-' + self.Name, Name, def);
end;

procedure TSite.WCBool(const Name: String; const val: boolean);
begin
  sitesdat.WriteBool('site-' + self.Name, Name, val);
end;

function TSite.RCDateTime(const Name: String; def: TDateTime): TDateTime;
begin
  Result := MyStrToDate(sitesdat.ReadString('site-' + self.Name, Name, ''));
end;

procedure TSite.WCDateTime(const Name: String; val: TDateTime);
begin
  sitesdat.WriteString('site-' + self.Name, Name, MyDateToStr(val));
end;

destructor TSite.Destroy;
begin
  Debug(dpSpam, section, 'Site %s destroy begin', [Name]);
  QueueEmpty(Name);
  // crash on !die
  slots.Free;
  Debug(dpSpam, section, 'Site %s destroy end', [Name]);
  inherited;
end;

procedure SlotsFire;
var
  i, j: integer;
begin
  for i := 0 to sites.Count - 1 do
    for j := 0 to TSite(sites[i]).slots.Count - 1 do
      TSiteSlot(TSite(sites[i]).slots[j]).Fire;
end;

procedure SiteStat;
var
  i: integer;
  allsites, upsites, downsites, unknown: integer;
begin
  allsites := 0;
  upsites := 0;
  downsites := 0;
  unknown := 0;
  for i := 0 to sites.Count - 1 do
  begin
    if TSite(sites[i]).Name = getAdminSiteName then
      continue;

    case TSite(sites[i]).working of
      sstUnknown: Inc(unknown);
      sstUp: Inc(upsites);
      sstDown: Inc(downsites);
    end;
    Inc(allsites);
  end;

  Console_SiteStat(allsites, upsites, downsites, unknown);
end;

function TSite.GetSkipPreStatus: boolean;
begin
  Result := RCBool('skip_pre', False);
end;

procedure TSite.SetSkipPreStatus(Value: boolean);
begin
  WCBool('skip_pre', Value);
end;

procedure TSite.SetWorking(Value: TSiteStatus);
begin
  if Value <> fWorking then
  begin
    fWorking := Value;

    if Name = getAdminSiteName then
    begin
      markeddown := False;
      Exit;
    end;

    if Value = sstUp then
    begin
      irc_addadmin(Format('<%s>SITE <b>%s</b> IS UP</c>', [globals.SiteColorOnline, Name]));
      markeddown := False;

      if RCInteger('autonuke', 0) <> 0 then
        AutoNuke;
      if RCInteger('autoindex', 0) <> 0 then
        AutoIndex;
      //if s.RCString('autologin','-1') <> '-1' then
      if RCInteger('autobnctest', 0) <> 0 then
        AutoBnctest;
      if AutoRulesStatus <> 0 then
        AutoRules;
      if RCInteger('autodirlist', 0) <> 0 then
        AutoDirlist;
    end
    else if Value = sstDown then
    begin
      irc_addadmin(Format('<%s>SITE <b>%s</b> IS DOWN</c>', [globals.SiteColorOffline, Name]));
      //removeing all tasks for the site...
      //    RemoveAutoIndex;
      //    RemoveAutoBnctest;
      //    RemoveAutoRules;
      //    RemoveAutoNuke;
      //    RemoveAutoDirlist;
      //    RemoveAutoCrawler;
    end;

    SiteStat;
  end;

  if (Value = sstDown) then
  begin
    QueueEmpty(Name);
  end;
end;

function TSite.Getconnect_timeout: integer;
begin
  //TODO: Maybe use [timeout] from slftp.ini as default value
  Result := RCInteger('connect_timeout', 15);
end;

function TSite.GetIdleInterval: integer;
begin
  Result := RCInteger('idleinterval', config.ReadInteger(section, 'idleinterval', 25));
end;

function TSite.Getio_timeout: integer;
begin
  Result := RCInteger('io_timeout', 15);
end;

function TSite.GetMaxIdle: integer;
begin
  Result := RCInteger('max_idle', config.ReadInteger(section, 'maxidle', 60));
end;

function TSite.GetMaxDn: integer;
begin
  Result := RCInteger('max_dn', 2);
end;

procedure TSite.SetMaxDn(Value: integer);
begin
  WCInteger('max_dn', Value);
end;

function TSite.GetMaxPreDn: integer;
begin
  // if max_pre_dn is not set, we use max_dn value to avoid bugs when users
  // haven't setup their maxupdn again after using new version with this feature
  Result := RCInteger('max_pre_dn', max_dn);
end;

procedure TSite.SetMaxPreDn(Value: integer);
begin
  WCInteger('max_pre_dn', Value);
end;

function TSite.GetMaxUp: integer;
begin
  Result := RCInteger('max_up', 2);
end;

procedure TSite.SetMaxUp(Value: integer);
begin
  WCInteger('max_up', Value);
end;

procedure TSite.Setconnect_timeout(const Value: integer);
begin
  WCInteger('connect_timeout', Value);
end;

procedure TSite.SetIdleInterval(Value: integer);
begin
  WCInteger('idleinterval', Value);
end;

procedure TSite.Setio_timeout(const Value: integer);
begin
  WCInteger('io_timeout', Value);
end;

procedure TSite.SetMaxIdle(Value: integer);
begin
  WCInteger('max_idle', Value);
end;

function TSite.Getsslmethod: TSSLMethods;
begin
  Result := TSSLMethods(RCInteger('sslmethod', integer(sslAuthTlsTLSv1_2)));
end;

procedure TSite.Setsslmethod(const Value: TSSLMethods);
begin
  WCInteger('sslmethod', integer(Value));
end;

function TSite.Getsslfxp: TSSLReq;
begin
  Result := TSSLReq(RCInteger('sslfxp', 0));
end;

procedure TSite.Setsslfxp(const Value: TSSLReq);
begin
  WCInteger('sslfxp', integer(Value));
end;

function TSite.GetPredir: String;
begin
  Result := sectiondir['PRE'];
end;

procedure TSite.SetPredir(const Value: String);
begin
  sectiondir['PRE'] := Value;
end;

function TSite.Getlegacydirlist: boolean;
begin
  Result := RCBool('legacycwd', False);
end;

procedure TSite.Setlegacydirlist(const Value: boolean);
begin
  WCBool('legacycwd', Value);
end;

procedure TSite.SetDownSiteDueToCreditsOrSpace;
begin
  markeddown := True;
  working := sstDown;
  RemoveAutoIndex;
  RemoveAutoBnctest; // maybe remove, so autobnctest will set it up again...or find a better solution than cycling
  RemoveAutoRules;
end;

procedure TSite.SetOutofSpace;
begin
  if ((foutofannounce = 0) or (HoursBetween(Now, foutofannounce) >= 1)) then
  begin
    foutofannounce := Now();
    irc_addadmin(Format('<c4>Site <b>%s</b> is out of disk space.</c>', [Name]));
    QueueEmpty(Name);

    if SetDownOnOutOfSpace then
    begin
      SetDownSiteDueToCreditsOrSpace;
    end;
  end;
end;

procedure TSite.SetKredits;
begin
  if ((fkreditz = 0) or (HoursBetween(Now, fkreditz) >= 1)) then
  begin
    fkreditz := Now();
    irc_addadmin(Format('Site %s is out of credits.', [Name]));
    QueueEmpty(Name);

    if SetDownOnOutOfCredits then
    begin
      SetDownSiteDueToCreditsOrSpace;
    end;
  end;
end;

function TSite.GetSectionDir(Name: String): String;
begin
  Result := RCString('dir-' + Name, '');
end;

procedure TSite.SetSectionDir(Name: String; const Value: String);
begin
  if Value <> '' then
    WCString('dir-' + Name, Value)
  else
  begin
    DeleteKey('dir-' + Name);
  end;
end;

function TSite.GetSections: String;
begin
  Result := RCString('sections', '');
end;

procedure TSite.SettSections(Value: String);
begin
  WCString('sections', Value);
end;

function TSite.GetAffils: String;
begin
  Result := RCString('affils', '');
end;

procedure TSite.SetAffils(Value: String);
begin
  WCString('affils', Value);
end;

function TSite.GetSectionPreTime(Name: String): integer;
begin
  Result := RCInteger('pretime-' + Name, -1);
end;

procedure TSite.SetSectionPreTime(Name: String; const Value: integer);
begin
  if Value <> -10 then
  begin
    WCInteger('pretime-' + Name, Value);
  end
  else
  begin
    DeleteKey('pretime-' + Name);
  end;
end;

function TSite.IsPretimeOk(const section: String; rlz_pretime: TDateTime): boolean;
var
  sec_pretime: integer;
begin
  // set default pretime to 10 min
  sec_pretime := config.ReadInteger('taskpretime', 'default_pretime', 600);

  // get pretime for default section
  if (sectionpretime['*'] <> -1) then
  begin
    sec_pretime := sectionpretime['*'];
  end;

  if (sectionpretime[section] <> -1) then
  begin
    sec_pretime := sectionpretime[section];
  end;

  if (SecondsBetween(Now(), rlz_pretime) < sec_pretime) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TSite.GetPretime(const section: String): String;
var
  sec_pretime: integer;
begin
  // set default pretime to 10 min
  sec_pretime := 10 * 60;

  // get pretime for default section
  if (sectionpretime['*'] <> -1) then
  begin
    sec_pretime := sectionpretime['*'];
  end;

  if (sectionpretime[section] <> -1) then
  begin
    sec_pretime := sectionpretime[section];
  end;

  if sec_pretime >= 604800 then
    Result := Format('%2.2d Weeks %1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 604800, (sec_pretime div 86400) mod 7, (sec_pretime div 3600) mod 24, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else if sec_pretime >= 86400 then
    Result := Format('%1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 86400, (sec_pretime div 3600) mod 24, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else if sec_pretime >= 3600 then
    Result := Format('%2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 3600, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else if sec_pretime >= 60 then
    Result := Format('%2.2d Min %2.2d Sec', [(sec_pretime div 60) mod 60, sec_pretime mod 60])
  else
    Result := Format('%2.2d Sec', [sec_pretime mod 60]);
end;

function TSite.IsAffil(affil: String): boolean;
var
  x: TStringList;
  i: integer;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := siteaffils;


    i := x.IndexOf(RemoveINT(affil));
    if i <> -1 then
    begin
      // for testing purpose, some ppl have problems with slftp as it tries to race affilgroups to site
      Debug(dpError, section, '[isAffils] Site: %s - affils: %s - affillist: %s - IndexOf: %d', [name, affil, x.DelimitedText, x.IndexOf(RemoveINT(affil))]);
    end;
    Result := i <> -1;


    //Result := x.IndexOf(RemoveINT(affil)) <> -1;
  finally
    x.Free;
  end;
end;

function TSite.IsSection(const section: String): boolean;
var
  x: TStringList;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := sections;
    Result := x.IndexOf(section) <> -1;
  finally
    x.Free;
  end;
end;

function TSite.IsUser(user: String): boolean;
var
  x: TStringList;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := leechers;
    Result := x.IndexOf(user) <> -1;
    if not Result then
    begin
      x.DelimitedText := traders;
      Result := x.IndexOf(user) <> -1;
    end;
  finally
    x.Free;
  end;
end;

function TSite.SetSections(const sections: String; remove: boolean): String;
var
  x: TStringList;
  ss: String;
  i: integer;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := self.sections;
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        Break;

      if x.IndexOf(ss) <> -1 then
      begin
        if remove then
          x.Delete(x.IndexOf(ss));
      end
      else
        x.Add(ss);
    end;
    x.Sort;
    self.sections := x.DelimitedText;
    Result := x.DelimitedText;
  finally
    x.Free;
  end;
end;

function TSite.SetLeechers(const users: String; remove: boolean): String;
var
  x: TStringList;
  ss: String;
  voltmar: boolean;
  i, maxleechers: integer;
begin
  voltmar := True;
  maxleechers := RCInteger('maxleechers', -1);
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := self.leechers;
    //  irc_addtexT('debug: '+IntToStr(maxleechers)+' '+x.DelimitedText);
    for i := 1 to 1000 do
    begin
      ss := SubString(users, ' ', i);
      if ss = '' then
        Break;

      if x.IndexOf(ss) <> -1 then
      begin
        if remove then
          x.Delete(x.IndexOf(ss));
      end
      else
      begin
        if ((maxleechers = -1) or (x.Count + 1 <= maxleechers)) then
          x.Add(ss)
        else
        begin
          if not voltmar then
          begin
            // irc_Addtext('Limit reached');
            voltmar := True;
          end;
        end;
      end;
    end;
    x.Sort;
    self.leechers := x.DelimitedText;
    Result := x.DelimitedText;
  finally
    x.Free;
  end;
end;

function TSite.SetTraders(const users: String; remove: boolean): String;
var
  x: TStringList;
  ss: String;
  i, maxtraders: integer;
  voltmar: boolean;
begin
  maxtraders := RCInteger('maxtraders', -1);
  voltmar := False;
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := self.traders;
    for i := 1 to 1000 do
    begin
      ss := SubString(users, ' ', i);
      if ss = '' then
        Break;

      if x.IndexOf(ss) <> -1 then
      begin
        if remove then
          x.Delete(x.IndexOf(ss));
      end
      else
      begin
        if ((maxtraders = -1) or (x.Count + 1 <= maxtraders)) then
          x.Add(ss)
        else
        begin
          if not voltmar then
          begin
            // irc_Addtext('Limit reached');
            voltmar := True;
          end;
        end;
      end;
    end;
    x.Sort;
    self.traders := x.DelimitedText;
    Result := x.DelimitedText;
  finally
    x.Free;
  end;
end;

function TSite.SetAffilsALL(affils: String): String;
var
  x: TStringList;
  List: TStrings;
  affil: String;
  i: integer;
begin
  x := TStringList.Create;
  List := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.Sorted := True;
    x.Duplicates := dupIgnore;
    {$IFDEF UNICODE}
      ExtractStrings([' ', ',', '|'], [], PChar(affils), List);
    {$ELSE}
      ExtractStrings([' ', ',', '|'], [], PAnsiChar(affils), List);
    {$ENDIF}

    for i := 0 to List.Count - 1 do
    begin
      affil := List[i];
      if affil = '' then
        continue;
      if x.IndexOf(affil) = -1 then
        x.Add(affil);
    end;
    x.Sort;
    siteaffils := x.DelimitedText;
    Result := x.DelimitedText;
  finally
    x.Free;
    List.Free;
  end;
end;

function TSite.AddAffil(const affil: String): boolean;
var
  x: TStringList;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.Sorted := True;
    x.Duplicates := dupIgnore;
    x.CaseSensitive := False;
    x.DelimitedText := siteaffils;
    if x.IndexOf(affil) = -1 then
    begin
      x.Add(affil);
      x.Sort;
      siteaffils := x.DelimitedText;
      Result := True;
    end
    else
      Result := False;
  finally
    x.Free;
  end;
end;

function TSite.GetLeechers: String;
begin
  Result := RCString('leechers', '');
end;

function TSite.GetTraders: String;
begin
  Result := RCString('traders', '');
end;

procedure TSite.SettLeechers(Value: String);
begin
  WCString('leechers', Value);
end;

procedure TSite.SettTraders(Value: String);
begin
  WCString('traders', Value);
end;

function TSite.GetUsers: String;
begin
  Result := Format('<b>%s</b> %s', [leechers, traders]);
end;

function TSite.FreeLeechSlots: integer;
var
  x: TStringList;
begin
  Result := RCInteger('maxleechers', -1);
  if Result = -1 then
    exit;

  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := leechers;
    if x.Count <= Result then
      Dec(Result, x.Count)
    else
      Result := 0;
  finally
    x.Free;
  end;
end;

function TSite.FreeTraderSlots: integer;
var
  x: TStringList;
begin
  Result := RCInteger('maxtraders', -1);
  if Result = -1 then
    exit;

  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := traders;
    if x.Count <= Result then
      Dec(Result, x.Count)
    else
      Result := 0;
  finally
    x.Free;
  end;
end;

procedure TSite.AutoBnctest;
var
  t: TLoginTask;
begin
  if PermDown then
    Exit;
  t := FetchAutoBnctest;
  if t <> nil then
    exit;

  // nincs, addolni kell.
  t := TLoginTask.Create('', '', Name, False, True);
  t.dontremove := True;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSite.AutoBnctest AddTask: %s',
        [e.Message]));
    end;
  end;
end;

procedure TSite.AutoRules;
var
  t: TRulesTask;
begin
  if PermDown then
    Exit;
  t := FetchAutoRules;
  if t <> nil then
    exit;
  // nincs, addolni kell.
  t := TRulesTask.Create('', '', Name);
  t.dontremove := True;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSite.AutoRules AddTask: %s', [e.Message]));
    end;
  end;
end;

procedure TSite.AutoDirlist;
var
  t: TAutoDirlistTask;
begin
  if PermDown then
    Exit;
  t := FetchAutoDirlist;
  if t <> nil then
    exit;

  t := TAutoDirlistTask.Create('', '', Name);
  t.startat := RcDateTime('nextautodirlist', 0);
  t.dontremove := True;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSite.AutoDirlist AddTask: %s',
        [e.Message]));
    end;
  end;
end;

procedure TSite.AutoCrawler;
//var t: TAutoCrawlerTask;
begin

  //there is no code for this stuff so wie can remiove it or?

  Exit;

  (*
    t:= FetchAutoCrawler;
    if t <> nil then exit;
    if PermDown then Exit;
    // nincs, addolni kell.
    t:= TAutoCrawlerTask.Create('', '', name);
    t.startat:= RcDateTime('nextautocrawler', 0);
    t.dontremove:= True;
    try
      AddTask(t);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TSite.AutoCrawler AddTask: %s', [e.Message]));
      end;
    end;
  *)
end;

procedure TSite.AutoNuke;
var
  t: TAutoNukeTask;
begin
  if PermDown then
    Exit;
  t := FetchAutoNuke;
  if t <> nil then
    exit;
  // nincs, addolni kell.
  t := TAutoNukeTask.Create('', '', Name);
  t.startat := RcDateTime('nextautonuke', 0);
  t.dontremove := True;
  AddTask(t);
end;

procedure TSite.AutoIndex;
var
  t: TAutoIndexTask;
begin
  if PermDown then
    Exit;
  if nil <> FetchAutoIndex then
    exit;
  // nincs, addolni kell.
  t := TAutoIndexTask.Create('', '', Name);
  t.startat := RcDateTime('nextautoindex', 0);
  t.dontremove := True;
  AddTask(t);
end;

function TSite.FetchAutoIndex: TAutoIndexTask;
var
  i: integer;
  t: TAutoIndexTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TAutoIndexTask) then
      begin
        t := TAutoIndexTask(tasks[i]);
        if (t.site1 = Name) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSite.FetchAutoDirlist: TAutoDirlistTask;
var
  i: integer;
  t: TAutoDirlistTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TAutoDirlistTask) then
      begin
        t := TAutoDirlistTask(tasks[i]);
        if (t.site1 = Name) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSite.FetchAutoCrawler: TAutoCrawlerTask;
var
  i: integer;
  t: TAutoCrawlerTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TAutoCrawlerTask) then
      begin
        t := TAutoCrawlerTask(tasks[i]);
        if (t.site1 = Name) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSite.FetchAutoNuke: TAutoNukeTask;
var
  i: integer;
  t: TAutoNukeTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TAutoNukeTask) then
      begin
        t := TAutoNukeTask(tasks[i]);
        if (t.site1 = Name) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSite.FetchAutoBnctest: TLoginTask;
var
  i: integer;
  t: TLoginTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TLoginTask) then
      begin
        t := TLoginTask(tasks[i]);
        if (t.site1 = Name) and (t.readd) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSite.FetchAutoRules: TRulesTask;
var
  i: integer;
  t: TRulesTask;
begin
  Result := nil;
  for i := 0 to tasks.Count - 1 do
  begin
    try
      if (tasks[i] is TRulesTask) then
      begin
        t := TRulesTask(tasks[i]);
        if (t.site1 = Name) then
        begin
          Result := t;
          exit;
        end;
      end;
    except
      Result := nil;
    end;
  end;
end;

procedure TSite.RemoveAutoIndex;
var
  t: TAutoIndexTask;
begin
  //crashes with !bnctest <sitename>
  t := FetchAutoIndex;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.RemoveAutoBnctest;
var
  t: TLoginTask;
begin
  //crashes
  t := FetchAutoBnctest;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.RemoveAutoRules;
var
  t: TRulesTask;
begin
  t := FetchAutoRules;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.RemoveAutoNuke;
var
  t: TAutoNukeTask;
begin
  t := FetchAutoNuke;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.RemoveAutoDirlist;
var
  t: TAutoDirlistTask;
begin
  t := FetchAutoDirlist;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.RemoveAutoCrawler;
var
  t: TAutoCrawlerTask;
begin
  t := FetchAutoCrawler;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready := True;
end;

procedure TSite.Auto;
begin
  if PermDown then
    Exit;

  if RCInteger('autobnctest', 0) > 0 then
    AutoBnctest;

  if AutoRulesStatus > 0 then
    AutoRules;

  if RCInteger('autodirlist', 0) > 0 then
    AutoDirlist;

  if RCInteger('autonuke', 0) > 0 then
    AutoNuke;

  if RCInteger('autoindex', 0) > 0 then
    AutoIndex;

  if RCInteger('autocrawler', 0) > 0 then
    AutoCrawler;
end;

procedure SiteAutoStart;
var
  i: integer;
begin
  for i := 0 to sites.Count - 1 do
    TSite(sites[i]).Auto;
end;

function TSite.Software: TSiteSW;
begin
  if self.sw <> sswUnknown then
    Result := self.sw
  else
    Result := TSiteSw(sitesdat.ReadInteger('site-' + Name, 'sw', integer(sswUnknown))); // TODO: maybe use self.GetSw for it?
end;

function TSite.IsLeecher(user: String): boolean;
var
  x: TStringList;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := leechers;
    Result := x.IndexOf(user) <> -1;
  finally
    x.Free;
  end;
end;

function TSite.IsTrader(user: String): boolean;
var
  x: TStringList;
begin
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.CaseSensitive := False;
    x.DelimitedText := traders;
    Result := x.IndexOf(user) <> -1;
  finally
    x.Free;
  end;
end;

function TSite.GetNoannounce: boolean;
begin
  Result := RCBool('noannounce', False);
end;

procedure TSite.SetNoAnnounce(const Value: boolean);
begin
  WCBool('noannounce', Value);
end;

function TSite.GetSectionPrecmd(Name: String): String;
begin
  Result := RCString('precmd-' + Name, '');
end;

procedure TSite.SetSectionPrecmd(Name: String; const Value: String);
begin
  if Value <> '' then
    WCString('precmd-' + Name, Value)
  else
  begin
    DeleteKey('precmd-' + Name);
  end;
end;

function TSite.GetSw: TSiteSw;
begin
  Result := TSiteSw(RCInteger('sw', 0));
end;

procedure TSite.SetSw(Value: TSiteSw);
begin
  WCInteger('sw', integer(Value));
end;

function TSite.GetRank(const section: String): integer;
begin
  Result := RCInteger('ranklock-' + section, 0);
  if Result = 0 then
  begin
    Result := RCInteger('ranklock', 0);
    if Result = 0 then
    begin
      Result := RCInteger('rank-' + section, 1);
    end;
  end;
end;

procedure TSite.SetRank(const section: String; Value: integer);
begin
  if Value <> 0 then
    WCInteger('rank-' + section, Value)
  else
    DeleteKey('rank-' + section);
end;

function TSite.GetRankLock(const section: String): integer;
begin
  Result := RCInteger('ranklock-' + section, 0);
  if Result = 0 then
  begin
    Result := RCInteger('ranklock', 0);
  end;
end;

procedure TSite.SetRankLock(const section: String; Value: integer);
begin
  if ((section = '') or (section = '*')) then
  begin
    if Value <> 0 then
      WCInteger('ranklock', Value)
    else
      DeleteKey('ranklock');
  end
  else
  begin
    if Value <> 0 then
      WCInteger('ranklock-' + section, Value)
    else
      DeleteKey('ranklock-' + section);
  end;
end;

function TSiteSlot.MdtmSeconds(filename: String): integer;
begin
  Result := 0;

  filename := TranslateFilename(filename);

  if not Send('MDTM %s', [filename]) then
    exit;
  if not Read('MDTM') then
    exit;

  if mdtmre.exec(lastresponse) then
    Result := StrToIntDef(mdtmre.Match[6], 0);
end;

procedure TSite.SetNumDn(const Value: integer);
begin
  if Value >= 0 then
    fNumDn := Value;
end;

procedure TSite.SetNumUp(const Value: integer);
begin
  if Value >= 0 then
    fNumUp := Value;
end;

procedure TSite.SetFreeSlots(const Value: integer);
begin
  if Value >= 0 then
    fFreeslots := Value;
end;

procedure TSite.RecalcFreeslots;
var
  i: integer;
  ss: TSiteSlot;
  fs: integer;
begin
  fs := 0;
  for i := 0 to slots.Count - 1 do
  begin
    (*
        try
          ss:= TSiteSlot(slots[i]);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('Nil Slot: %s %d recreationg', [name, i]));
            irc_Adderror(Format('<c4>[ERROR]</c> Nil Slot: %s %d recreationg', [name, i]));
            slots[i] := nil;
            slots[i] := TSiteSlot.Create(self, i);
            ss:= TSiteSlot(slots[i]);
            irc_Adderror(Format('<c4>[INFO]</c> Nil Slot: %s %d recreated', [name, i]));
          end;
        end;
        if ((slots[i] = nil) or (ss = nil)) then
        begin
          Debug(dpError, section, Format('Nil Slot: %s %d recreationg', [name, i]));
          irc_Adderror(Format('<c4>[ERROR]</c> Nil Slot: %s %d recreationg', [name, i]));
          slots[i] := nil;
          slots[i] := TSiteSlot.Create(self, i);
          ss:= TSiteSlot(slots[i]);
          irc_Adderror(Format('<c4>[INFO]</c> Nil Slot: %s %d recreated', [name, i]));
        end;
    *)
    ss := TSiteSlot(slots[i]);
    if ss.todotask = nil then
      Inc(fs);
  end;

  ffreeslots := fs;
end;

procedure TSite.FullLogin;
var
  i: integer;
  ss: TSiteSlot;
  fs: integer;
begin
  fs := 0;
  for i := 0 to slots.Count - 1 do
  begin
    ss := TSiteSlot(slots[i]);
    if ((ss.Status <> ssOnline) and (ss.todotask = nil)) then
    begin
      ss.ReLogin(1, False, 'FullLogin');
    end;
  end;

  ffreeslots := fs;
end;

function TSite.GetSiteInfos: String;
begin
  Result := RCString('siteinfos', '');
end;

procedure TSite.SetSiteInfos(Value: String);
begin
  WCString('siteinfos', Value);
end;

function TSite.GetLastKnownCredits: int64;
begin
  Result := -1;
end;

procedure TSite.SetLastKnownCredits(Value: int64);
begin
  //
end;

function TSite.GetUseAutoInvite: boolean;
begin
  Result := RCBool('useautoinvite', True);
end;

procedure TSite.SetUseAutoInvite(Value: Boolean);
begin
  WCBool('useautoinvite', Value);
end;

function TSite.GetIsUp: boolean;
begin
  Result := working = sstUp;
end;

function TSite.GetAutoRulesStatus: integer;
begin
  Result := RCInteger('autorules', 0);
end;

procedure TSite.SetAutoRulesStatus(const Value: integer);
begin
  WCInteger('autorules', Value);
end;

function TSite.GetSetDownOnOutOfSpace: boolean;
begin
  Result := RCBool('set_down_on_out_of_space', config.ReadBool('sites', 'set_down_on_out_of_space', False));
end;

procedure TSite.SetSetDownOnOutOfSpace(const Value: boolean);
begin
  WCBool('set_down_on_out_of_space', Value);
end;

function TSite.GetSetDownOnOutOfCredits: boolean;
begin
  Result := RCBool('set_down_on_out_of_credits', config.ReadBool('sites', 'set_down_on_out_of_credits', False));
end;

procedure TSite.SetSetDownOnOutOfCredits(const Value: boolean);
begin
  WCBool('set_down_on_out_of_credits', Value);
end;

procedure TSite.SetIRCNick(Value: String);
begin
  WCString('ircnick', Value);
end;

function TSite.GetIRCNick: String;
begin
  Result := RCString('ircnick', '');
end;

procedure TSite.SetProxyName(Value: String);
begin
  WCString('proxyname', Value);
end;

function TSite.GetProxyName;
begin
  Result := RCString('proxyname', '!!NOIN!!');
end;

procedure TSite.SetSiteUsername(Value: String);
begin
  WCString('username', Value);
end;

function TSite.GetSiteUsername;
begin
  Result := RCString('username', 'anonymous_slFtp');
end;

procedure TSite.SetSitePassword(Value: String);
begin
  WCString('password', Value);
end;

function TSite.GetSitePassword;
begin
  Result := RCString('password', 'CR4P_P4$$W0RD');
end;

function TSite.GetSiteCountry;
begin
  Result := RCString('country', '??');
end;

procedure TSite.SetSiteCountry(Value: String);
begin
  WCString('country', Value);
end;

function TSite.GetSiteMaxUpPerRip: integer;
begin
  Result := RCInteger('maxupperrip', 0);
end;

procedure TSite.SetSiteMaxUpPerRip(const Value: integer);
begin
  WCInteger('maxupperrip', Value);
end;

function TSite.GetNoLoginMSG: boolean;
begin
  Result := RCBool('nologinmsg', False);
end;

procedure TSite.SetNoLoginMSG(Value: boolean);
begin
  WCBool('nologinmsg', Value);
end;

function TSite.GetUseForNFOdownload: integer;
begin
  // 0 means disabled
  // 1 means enabled
  // 2 means automatically disabled by slftp due to problems (some SSL or out of credits)
  Result := RCInteger('usefornfodownload', 1);
end;

procedure TSite.SetUseForNFOdownload(Value: integer);
begin
  WCInteger('usefornfodownload', Value);
end;

function TSite.GetSkipBeingUploadedFiles: boolean;
begin
  Result := RCBool('skip_being_uploaded_files', config.ReadBool('dirlist', 'skip_being_uploaded_files', False));
end;

procedure TSite.SetSkipBeingUploadedFiles(Value: boolean);
begin
  WCBool('skip_being_uploaded_files', Value);
end;

function TSite.GetPermDownStatus: boolean;
begin
  Result := RCBool('permdown', False);
end;

procedure TSite.SetPermDownStatus(Value: boolean);
begin
  WCBool('permdown', Value);
end;

end.

