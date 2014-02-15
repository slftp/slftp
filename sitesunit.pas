unit sitesunit;

interface

uses Classes, encinifile, Contnrs, sltcp, SyncObjs, Regexpr,
     taskautodirlist, taskautocrawler, taskautonuke, taskautoindex, tasklogin, tasksunit, taskrules;

type
  TSlotStatus = (ssNone, ssDown, ssOffline, ssOnline, ssMarkedDown);
  TSSLMethods = (sslNone, sslImplicitSSLv23, sslAuthSslSSLv23, sslAuthTLSSSLv23, sslAuthSslTLSv1, sslAuthTlsTLSv1, sslImplicitTLSv1);
  TSiteSw = (sswUnknown, sswGlftpd, sswDrftpd, sswIoftpd);
  TProtection = (prNone, prProtP, prProtC);
  TSiteStatus = (sstUnknown, sstUp, sstDown, sstMarkedDown, sstOutOfCreds, sstOutOfSpace);
  TSSLReq = (srNone, srNeeded, srUnsupported);

  TSite = class; // forward
  TSiteSlot = class(TslTCPThread)
  private
    mdtmre: TRegExpr;
    aktdir: string;
    prot: TProtection;
    kilepve: Boolean;
    no: Integer;
    fstatus: TSlotStatus;
    event: TEvent;
    function LoginBnc(i: Integer; kill: Boolean = False): Boolean;
    procedure AddLoginTask;
    procedure SetOnline(value: TSlotStatus);
    procedure ProcessFeat;
    procedure SetDownloadingFrom(const Value: Boolean);
    procedure SetUploadingTo(const Value: Boolean);
    procedure SetTodotask(Value: TTask);
  public
//    pre: Boolean;
    localport: Integer;
    peerport: Integer;
    peerip: string;
    fuploadingto: Boolean;
    fdownloadingfrom: Boolean;
    lastio: TDateTime;
    lastactivity: TDateTime;
    lastResponse: string;
    lastResponseCode: Integer;

    ftodotask: TTask;
    site: TSite;
    procedure DestroySocket(down: Boolean);
    procedure Quit;
    function Name: string;
    procedure Fire;
    function Login(kill: Boolean = False): boolean;
    procedure Execute; override;
    constructor Create(site: TSite; no: Integer);
    destructor Destroy; override;
    function RCBool(name: string; def: Boolean): Boolean;
    function RCInteger(name: string; def: Integer): Integer;
    function RCDateTime(name: string; def: TDateTime): TDateTime;
    function RCString(name, def: string): string;

    procedure Stop; override;

    function MdtmSeconds(filename: string): Integer;
    function Read(read_cmd : String = ''): Boolean; overload;
    function Read(read_cmd : String; raiseontimeout: Boolean; raiseonclose: Boolean; timeout: Integer = 0): Boolean; overload;
    function Send(s: string): Boolean; overload;
    function Send(s: string; const Args: array of const): Boolean; overload;
    function ReLogin(hanyszor: Integer = 0; kill: Boolean = False; s_message: String = ''): boolean;
    function bnc: string;
    function Cwd(dir: string; force: Boolean = False): Boolean;
    function Dirlist(dir: string; forcecwd: Boolean=False; fulldirlist: Boolean= False): Boolean;
//    function DirlistD(dir: string; forcecwd: Boolean=False; use_custom_cmd:Boolean = False; fulldirlist: Boolean= False): Boolean;
    function RemoveFile(dir, filename: string): Boolean;
    function RemoveDir(dir: string): Boolean;
    function SendProtP: Boolean;
    function SendProtC: Boolean;
    function Mkdir(dirtocreate: string): Boolean;
    function TranslateFilename(filename: string): string;
    function Pwd(var dir: string): Boolean;
    property uploadingto: Boolean read fUploadingTo write SetUploadingTo;
    property downloadingfrom: Boolean read fDownloadingFrom write SetDownloadingFrom;
    property todotask: TTask read fTodotask write SetTodotask;
  published
    property Status: TSlotStatus read fstatus write SetOnline;
  end;
  TSite = class
  private
    fworking: TSiteStatus;
    foutofannounce: TDateTime;
    fkreditz: TDateTime;
    fNumDn: Integer;
    fNumUp: Integer;
//    fskippre:boolean;
    function GetSkipPreStatus:boolean;
    procedure SetSkipPreStatus(value:boolean);


    function GetPermDownStatus:boolean;
    procedure SetPermDownStatus(value:boolean);

    function Software: TSiteSW;

    procedure SetWorking(value: TSiteStatus);

    function GetMaxDn: Integer;
    procedure SetMaxDn(value: Integer);
    function GetMaxUp: Integer;
    procedure SetMaxUp(value: Integer);
    function GetMaxIdle: Integer;
    procedure SetMaxIdle(value: Integer);
    function GetIdleInterval: Integer;
    procedure SetIdleInterval(value: Integer);
    function GetIo_timeout: Integer;
    procedure SetIo_timeout(const value: Integer);
    function GetConnect_timeout: Integer;
    procedure SetConnect_timeout(const value: Integer);
    function Getsslmethod: TSSLMethods;
    procedure Setsslmethod(const Value: TSSLMethods);
    function Getsslfxp: TSSLReq;
    procedure Setsslfxp(const Value: TSSLReq);
    procedure WCBool(name: string; val: Boolean);
    function GetPredir: string;
    procedure SetPredir(const Value: string);
    function Getlegacydirlist: Boolean;
    procedure Setlegacydirlist(const Value: Boolean);
    function GetSectionDir(name: string): string;
    procedure SetSectionDir(name: string; const Value: string);
    function GetSectionPrecmd(name: string): string;
    procedure SetSectionPrecmd(name: string; const Value: string);
    function GetSectionAffil(name: string): string;
    procedure SetSectionAffil(name: string; const Value: string);
    function GetSectionPreTime(Name: string): integer;
    procedure SetSectionPreTime(Name: string; const Value: integer);
    function GetSections: string;
    procedure SettSections(value: string);
    function GetLeechers: string;
    procedure SettLeechers(value: string);
    function GetTraders: string;
    procedure SettTraders(value: string);
    function GetUsers: string;
    function GetNoannounce: Boolean;
    procedure SetNoAnnounce(const Value: Boolean);
    function FetchAutoIndex: TAutoIndexTask;
    function FetchAutoBnctest: TLoginTask;
    function FetchAutoRules: TRulesTask;
    function FetchAutoDirlist: TAutoDirlistTask;
    function FetchAutoCrawler: TAutoCrawlerTask;
    function FetchAutoNuke: TAutoNukeTask;
    procedure SetNumDn(const Value: Integer);
    procedure SetNumUp(const Value: Integer);
    procedure SetFreeSlots(const Value: Integer);

    function GetProxyName:string;
    procedure SetProxyName(value:string);

    function GetNoLoginMSG:boolean;
    procedure SetNoLoginMSG(value:boolean);

    function GetIRCNick:string;
    procedure SetIRCNick(value:string);




  public
    emptyQueue: Boolean;
    markeddown: Boolean;
    siteinvited: Boolean;

    ffreeslots: Integer;
    name: string;
    slots: TObjectList;

//    siteinvited: Boolean;

    function GetSw: TSiteSw;
    procedure SetSw(Value: TSiteSw);
    procedure Stop;
    constructor Create(name: string);
    destructor Destroy; override;
    procedure DeleteKey(name: string);
    function RCBool(name: string; def: Boolean): Boolean;
    function RCInteger(name: string; def: Integer): Integer;
    function RCDateTime(name: string; def: TDateTime): TDateTime;
    function RCString(name, def: string): string;
    procedure WCDateTime(name: string; val: TDateTime);
    procedure WCInteger(name: string; val: Integer);
    procedure WCString(name: string; val: string);
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

    function GetRank(section: string): Integer;
    procedure SetRank(section: string; Value: Integer);
    function GetRankLock(section: string): Integer;
    procedure SetRankLock(section: string; Value: Integer);

    function FreeLeechSlots: Integer;
    function FreeTraderSlots: Integer;
    function SetSections(sections: string; remove: Boolean = False): string;
    function SetLeechers(users: string; remove: Boolean): string;
    function SetTraders(users: string; remove: Boolean): string;
    function IsSection(section: string): Boolean;
    function IsAffil(section, affil: string): Boolean;
    function SetAffils(section, affils: string; remove: Boolean = False): string;
    function IsUser(user: string): Boolean;
    function IsLeecher(user: string): Boolean;
    function IsTrader(user: string): Boolean;

    function IsPretimeOk(section: string; rlz_pretime: TDateTime): boolean;
    function GetPretime(section: string): string;


    function isRouteableTo(sitename:string):boolean;
    function isRouteableFrom(sitename:string):boolean;

    property sections: string read GetSections write SettSections;
    property leechers: string read GetLeechers write SettLeechers;
    property traders: string read GetTraders write SettTraders;
    property users: string read GetUsers;
    property sectiondir[name: string]: string read GetSectionDir write SetSectionDir;
    property sectionprecmd[name: string]: string read GetSectionPreCmd write SetSectionPrecmd;
    property sectionaffil[name: string]: string read GetSectionAffil write SetSectionAffil;

    property sectionpretime[Name: string]: integer read GetSectionPreTime write SetSectionPreTime;

    property num_dn: Integer read fNumDn write SetNumDn;
    property num_up: Integer read fNumUp write SetNumUp;
    property freeslots: Integer read fFreeslots write SetFreeSlots;
    property IRCNick:string read getircnick write setircnick;
    property ProxyName:string read GetProxyName write SetProxyName;
  published
    property sw: TSiteSw read GetSw write SetSw;
    property noannounce: Boolean read GetNoannounce write SetNoAnnounce;
    property working: TSiteStatus read fWorking write SetWorking;
    property max_dn: Integer read GetMaxDn write SetMaxDn;
    property max_up: Integer read GetMaxUp write SetMaxUp;
    property maxidle: Integer read Getmaxidle write Setmaxidle;
    property idleinterval: Integer read Getidleinterval write Setidleinterval;

    property io_timeout: Integer read Getio_timeout write Setio_timeout;
    property connect_timeout: Integer read Getconnect_timeout write Setconnect_timeout;
    property sslmethod: TSSLMethods read Getsslmethod write Setsslmethod;
    property sslfxp: TSSLReq read Getsslfxp write Setsslfxp;
    property legacydirlist: Boolean read Getlegacydirlist write Setlegacydirlist;
    property predir: string read GetPredir write SetPredir;

    property NoLoginMSG:Boolean read GetNoLoginMSG write SetNoLoginMSG;

    property PermDown:boolean read GetPermDownStatus write SetPermDownStatus;
    property SkipPre:boolean read GetSkipPreStatus write SetSkipPreStatus;

  end;

function ReadSites(): Boolean;
procedure SitesStart;
procedure SlotsFire;
procedure SiteAutoStart;

function FindSiteByName(netname, sitename: string): TSite;
function FindSlotByName(slotname: string): TSiteSlot;
procedure SitesInit;
procedure SitesUninit;
function GiveSiteLastStart: TDateTime;


//function

function SiteSoftWareToSTring(sitename:string):String; overload;
function SiteSoftWareToSTring(site:TSite):String; overload;


var sitesdat: TEncIniFile = nil;
    sites: TObjectList = nil;
    sitesautosend:TDateTime;

implementation

uses SysUtils, irc,DateUtils, configunit, queueunit, debugunit,
  socks5, console,
  mystrings, versioninfo, mainthread, IniFiles, Math, mrdohutils, taskrace, pazo;
const section='sites';

var bnccsere: TCriticalSection = nil;
    sitelaststart: TDateTime;
    // Config vars
    maxrelogins: Integer = 3;
    delay_between_connects: Integer = 200;
    admin_sitename: String = 'SLFTP';
    admin_siteslots: Integer = 10;
    autologin: Boolean = false;
    killafter: Integer = 0;

function SiteSoftWareToSTring(sitename:string):String;
begin
  result:= SiteSoftWareToSTring(FindSiteByName('',sitename));
end;

function SiteSoftWareToSTring(site:TSite):String;
begin
  Result:='Unknown';
  // sswUnknown, sswGlftpd, sswDrftpd, sswIoftpd
  case TSite(site).Software of
    sswUnknown:Result:='Unknown';
    sswGlftpd:Result:='GlFTPD';
    sswDrftpd:Result:='DrFTPD';
    sswIoftpd:Result:='ioFTPD';
  end;
end;

// NOTE: ez a fuggveny hivasahoz lokkolni KELL eloszor a mindensegit
function FindSiteByName(netname, sitename: string): TSite;
var i: Integer;
    s: TSite;
begin
  Result:= nil;
  try
    for i:= 0 to sites.Count-1 do
    begin
      s:= TSite(sites[i]);
      if s.name = sitename then
      begin
        if ((netname <> '') and (netname <> 'CONSOLE') and (s.noannounce)) then
        begin
          exit;
        end;
        Result:= s;
        break;
      end;
    end;
  except
    Result:= nil;
  end;
end;

function FindSlotByName(slotname: string): TSiteSlot;
var i, j: Integer;
begin
  Result:= nil;
  try
    for i:= 0 to sites.Count-1 do
    begin
      for j:= 0 to TSite(sites[i]).slots.Count-1 do
      begin
        if TSiteSlot(TSite(sites[i]).slots[j]).name = slotname then
        begin
          Result:= TSiteSlot(TSite(sites[i]).slots[j]);
          exit;
        end;
      end;
    end;
  except
    Result:= nil;
  end;
end;


function ReadSites(): Boolean;
var sitesdatfile: string;
begin
  Result:= False;
  sitesdatfile:= ExtractFilePath(ParamStr(0))+'sites.dat';
  if not FileExists(sitesdatfile) then
  begin
    Debug(dpError, section, 'sites.dat not exists, creating it');
    sitesdat:= TEncIniFile.Create(sitesdatfile, passphrase, True);
    sitesdat.WriteString(section, 'default', 'exists');
    sitesdat.UpdateFile;
    Result:= True;
  end else
  begin
    try
      sitesdat:= TEncIniFile.Create(sitesdatfile, passphrase);
      if sitesdat.ReadString(section, 'default', '') = 'exists' then
      begin
        sitesdat.autoupdate:= True;
        Result:= True;
      end;
    except on e: Exception do
      debug(dpError, section, 'Error opening sites.dat: %s', [e.Message])
    end;
  end;
end;



procedure SitesInit;
begin
  sitelaststart:= Now();
  bnccsere:= TCriticalSection.Create;
  sites:= TObjectList.Create;
end;
procedure SitesUninit;
begin
  Debug(dpSpam, section, 'Uninit1');


  if sites <> nil then
  begin
    sites.Free;
    sites:= nil;
  end;


  if sitesdat <> nil then
  begin
    sitesdat.Free;
    sitesdat:= nil;
  end;


  bnccsere.Free;
  Debug(dpSpam, section, 'Uninit2');  
end;

{ TSiteSlot }

function GiveSiteLastStart: TDateTime;
begin
  bnccsere.Enter;
  if siteLastStart < Now then
    siteLastStart:= Now;
  siteLastStart:= IncMilliSecond(sitelaststart, delay_between_connects);
  Result:= siteLastStart;
  bnccsere.Leave;
end;

procedure TSiteSlot.AddLoginTask;
var t: TLoginTask;
begin

  t:= TLoginTask.Create('', '', site.name, False, False);
  t.wantedslot:= name;
  t.startat:= GiveSiteLastStart;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.AddLoginTask AddTask: %s', [e.Message]));
    end;
  end;
end;

constructor TSiteSlot.Create(site: TSite; no: Integer);
begin
  self.site:= site;
  self.no:= no;
  debug(dpSpam, section, 'Slot %s is creating', [name]);

  todotask:= nil;
  event:= TEvent.Create(nil, False, False, Name);
  kilepve:= False;

  aktdir:= '';
  prot:= prNone;
  status:= ssNone;
  lastResponse:= '';
  lastResponseCode:= 0;
  lastio:= Now();
  lastactivity:= Now();

  mdtmre:= TRegExpr.Create;
  mdtmre.Expression:= '(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)';

  if (self.site.name <> admin_sitename) then
  begin
    if not site.PermDown then
    begin
      // ha autologin be van kapcsolva akkor
      if ((autologin) or (RCBool('autologin', False))) then
        AddLoginTask;
      //self.socks5

    end;
  end;

  debug(dpSpam, section, 'Slot %s has created', [name]);
  inherited Create(False);
end;


function TSiteSlot.Name: string;
begin
  Result:= site.name+'/'+IntToStr(no);
end;
procedure TSiteSlot.DestroySocket(down: Boolean);
begin
  try
    Disconnect;
    socks5.enabled:= False;
    Console_Slot_Close(name);
    prot:= prNone;
    aktdir:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in DestroySocket: %s', [e.Message]));
    end;
  end;
  if down then
    status:= ssDown
  else
    status:= ssOffline;
end;
procedure TSiteSlot.Execute;
var tname: String;
begin
  Debug(dpSpam, section, 'Slot %s has started', [name]);

  console_add_sitewindow(name);
  while ((not kilepes) and  (not shouldquit)) do// and (not False)
  begin
    try
      if status = ssOnline then
        Console_Slot_Add(name, 'Idle...');

      if ((todotask <> nil) and (not queue_debug_mode)) then
      begin
        try
          tname:= todotask.Name;
          Debug(dpSpam, section, '--> '+Format('%s', [name]));
          if todotask.Execute(self) then
            lastactivity:= Now();

          Debug(dpSpam, section, '<-- '+Format('%s', [name]));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Execute %s: %s', [tname, e.Message]));
          end;
        end;

        uploadingto:= False;
        downloadingfrom:= False;

        if (todotask <> nil) then
        begin
          try
            try
              if (todotask.slot1 <> nil) then
              begin
                todotask.slot1:= nil;
              end;
            finally
              todotask:= nil;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Execute : Exception remove todotask : %s', [e.Message]));
            end;
          end;
        end;

        if ((not shouldquit) and (not kilepes)) then
        begin
          QueueFire;
        end;
      end else begin
        //event.WaitFor($FFFFFFFF);
        case event.WaitFor(15 * 60 * 1000) of
          wrSignaled : { Event fired. Normal exit. }
          begin

          end;
          else { Timeout reach }
          begin
            if spamcfg.readbool(section,'siteslot_recycle',True) then
              irc_Adderror('TSiteSlot.Execute: <c2>Force Leave</c>:'+Name+' SiteSlot Recycle 15min');
            Debug(dpSpam, section,'TSiteSlot.Execute: Force Leave:'+Name+' SiteSlot Recycle 15min');
          end;
        end;
      end;

    except
      on E: Exception do
      begin
        Debug(dpError, section, '[Exception] Slot exception : %s', [e.Message]);
        try
          todotask:= nil;
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Execute : Exception remove todotask : %s', [e.Message]));
            break;
          end;
        end;
      end;
    end;
  end;
  console_delwindow(name);
  kilepve:= True;
end;

destructor TSiteSlot.Destroy;
begin
  Debug(dpSpam, section, 'Slot %s destroy begin', [name]);
  Stop;
  DestroySocket(True);
  event.Free;
  event:= nil;
  mdtmre.Free;
  inherited;
  Debug(dpSpam, section, 'Slot %s destroy end', [name]);
end;


function TSiteSlot.SendProtC: Boolean;
begin
  Result:= False;
  if prot <> prProtC then
  begin
    if not Send('PROT C') then exit;
    if not Read('PROT C') then exit;

    prot:= prProtC;
  end;
  Result:= True;
end;

function TSiteSlot.SendProtP: Boolean;
begin
  Result:= False;
  if prot <> prProtP then
  begin
    if not Send('PROT P') then exit;
    if not Read('PROT P') then exit;

    prot:= prProtP;
  end;
  Result:= True;
end;

procedure TSiteSlot.ProcessFeat;
begin
(*
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
*)
  if (0 < Pos('PRET', lastResponse)) then
  begin
    if site.sw <> sswDrftpd then
      sitesdat.WriteInteger('site-'+site.name, 'sw', Integer(sswDrftpd));
  end else
  if (0 < Pos('CPSV', lastResponse)) then
  begin
    if site.sw <> sswGlftpd then
      sitesdat.WriteInteger('site-'+site.name, 'sw', Integer(sswGlftpd));
  end;
end;

function TSiteSlot.Cwd(dir: string; force: Boolean = False): Boolean;
begin
  Result:= False;
  dir:= MyIncludeTrailingSlash(dir);
  if ((dir <> aktdir) or (force)) then
  begin
    if ((site.legacydirlist) or (force)) then
    begin
      if not Send('CWD %s', [dir]) then exit;
      if not Read('CWD') then exit;
      if (lastResponseCode = 250) then
      begin
        if (0 <> Pos('250- Matched ', lastresponse)) then
        begin
          Debug(dpError, section, 'TRIMMED RLSNAME DETECTED! '+Name+' '+dir);
          if dir[1] <> '/' then
            aktdir:= aktdir + dir
          else
            aktdir:= dir;
          Result:= True;
          exit;
        end;
(*
        if (0 <> Pos('Looks like this is a pre', lastresponse)) then
          pre:= True;
*)
        if dir[1] <> '/' then
          aktdir:= aktdir + dir
        else
          aktdir:= dir;
      end
      else
      begin
        //irc_addtext(todotask, '%s: %s', [name, trim(lastResponse)]);
        Result:= False;
        exit;
      end;
    end else
    begin
      if dir[1] <> '/' then
        aktdir:= aktdir + dir
      else
        aktdir:= dir;
    end;
  end;
  Result:= True;
end;

function TSiteSlot.LoginBnc(i: Integer; kill: Boolean = False): Boolean;
var sslm: TSSLMethods;
    un: string;
    upw:string;
    tmp: string;
begin
  Result:= False;

  if (self.site.name = admin_sitename) then
  begin
    Result := True;
    exit;
  end;

if ((site.proxyname = '!!NOIN!!') or (site.proxyname = '0') or (site.proxyname = '')) then
SetupSocks5(self, (not RCBool('nosocks5', False)) and (config.ReadBool(section, 'socks5', False)))
else
mSLSetupSocks5(site.proxyname,self, True);




  // elso lepes a connect
    Host:= RCString('bnc_host-'+IntToStr(i), '');
    Port:= RCInteger('bnc_port-'+IntToStr(i), 0);
    Connect(site.connect_timeout*1000);

    peerport:= slSocket.PeerPort;
    peerip:= slSocket.PeerIP;
    localport:= slSocket.localPort;

    sslm:= TSSLMethods(site.sslmethod);

    if sslm in [sslImplicitSSLv23, sslImplicitTLSv1 ] then
    begin
      SetSSLContext(slTLSv1);
      if not TurnToSSL(site.io_timeout * 1000) then exit;
    end;


  // banner
  if not Read('BANNER') then exit;

  if(lastResponseCode <> 220) then
  begin
    error:= Trim(lastResponse);
    exit;
  end;

  if (sslm in [sslAuthSslSSLv23, sslAuthSslTLSv1, sslAuthTlsSSLv23, sslAuthTlsTLSv1]) then
  begin
    if sslm in [sslAuthSslSSLv23, sslAuthTlsSSLv23] then
      SetSSLContext(slSslv23)
    else
      SetSSLContext(slTLSv1);

    if sslm in [sslAuthSslSSLv23, sslAuthSslTLSv1] then
      tmp:= 'AUTH SSL'
    else
      tmp:= 'AUTH TLS';

      // AUTH TLS-t probalunk
      if not Send(tmp) then exit;
      if not Read('AUTH') then exit;

      if lastResponseCode <> 234 then exit;
      if not TurnToSSL(site.io_timeout * 1000) then exit;

  end;
  (* else
    Debug(dpMessage, section, '%s: TRYING PLAINTEXT LOGIN', [name]);
  *)

  un:= RCString('username', 'anonymous');
  upw:=RCString('password', 'foo@foobar.hu');
  if site.sw = sswGlftpd then
  if self.site.NoLoginMSG then upw:='-'+upw;


  if(kill) then
  begin
    //irc_addtext(todotask, '<c7>LoginBnc</c> <b>%s</b> with KILL', [Name]);
    un:= '!'+un;
  end;

  if not Send('USER %s', [un]) then exit;
  if not Read('USER') then exit;

  if lastResponseCode <> 331 then
  begin
    error:= Trim(lastResponse);
    exit;
  end;

  if not Send('PASS %s', [upw]) then exit;
  if not Read('PASS') then exit;

  if lastResponseCode <> 230 then
  begin
    error:= Trim(lastResponse);
    exit;
  end;


  if not Send('TYPE I') then exit;
  if not Read('TYPE I') then exit;


  if(TSiteSw(RCInteger('sw', 0)) = sswUnknown) then
  begin
    if not Send('FEAT') then exit;
    if not Read('FEAT') then exit;

	  ProcessFeat();
  end;

  if not Send('SITE XDUPE 3') then exit;
  if not Read('XDUPE') then exit;

  if (site.sslfxp = srNeeded) then
  begin
    if (not SendProtP()) then exit;
  end;



   if (TSiteSw(RCInteger('sw', 0)) = sswDrftpd) then
   begin
      if ( not Send('CLNT %s', [Get_VersionString(ParamStr(0))])) then exit;
      if not Read('CLNT') then exit;
   end;

   if(site.predir <> '') then
   begin
     if not Cwd(site.predir) then
       if status = ssDown then exit;
   end;

    // siker
    Result:= True;
    // Announce(section, False, 'SLOT %s IS UP: %s', [name, bnc]);

    // modositjuk is a top1 bnc-t erre:
    if i <> 0 then
    begin
      bnccsere.Enter;
      sitesdat.WriteString('site-'+site.name, 'bnc_host-'+IntToStr(i), RCString('bnc_host-0', ''));
      sitesdat.WriteInteger('site-'+site.name, 'bnc_port-'+IntToStr(i), RCInteger('bnc_port-0', 0));

      sitesdat.WriteString('site-'+site.name, 'bnc_host-0', Host);
      sitesdat.WriteInteger('site-'+site.name, 'bnc_port-0', Port);
      bnccsere.Leave;
    end;

    irc_SendRACESTATS(Format('LOGIN <b>%s</b> (%s)', [site.name, name]));
    status:= ssOnline;
    
end;


function TSiteSlot.Login(kill: Boolean = False): boolean;
var host: string;
    i: Integer;
begin
  Result:= False;

  i:= 0;
  while ((not kilepes) and (not shouldquit)) do
  begin
    if i>20 then Break;

    try
      host:= RCString('bnc_host-'+IntToStr(i), '');
      if host = '' then break;
      if Result then Break;

      Result:= LoginBnc(i, kill);
      if Result then
      begin
        Break;
      end;

      if (
         ((lastResponseCode = 530) and (0 <> Pos('your account is restricted to', lastResponse))) or
         ((lastResponseCode = 530) and (0 <> Pos('your maximum number of connections', lastResponse)))
         ) then
      begin
        if site.sw = sswGlftpd then
        begin
          //DestroySocket(False);
          //Result:= LoginBnc(i, true);
        end;
      end else begin
        irc_Adderror(todotask, '<c4>[ERROR Login]</c> %s@%s:: %s', [name, bnc, error]);
        if ((lastResponseCode = 421) and (0 <> Pos('Hammer Protection', lastResponse))) then
        begin
          break;
        end;
        
      end;
      inc(i);
    except
      break;
    end;
  end;

  if ((not kilepes) and (not shouldquit)) then
    if not Result then
    begin
      if (
         ((lastResponseCode = 530) and (0 <> Pos('your account is restricted to', lastResponse))) or
         ((lastResponseCode = 530) and (0 <> Pos('your maximum number of connections', lastResponse)))
         ) then
      begin
        DestroySocket(False);
      end else begin
        DestroySocket(False);
        irc_addtext(todotask, '<c4>SLOT <b>%s</b> IS DOWN</c>', [Name]);
      end;
    end;
end;

function TSiteSlot.ReLogin(hanyszor: Integer = 0; kill: Boolean = False; s_message: String = ''): boolean;
var l_maxrelogins: Integer;
    relogins: Integer;
    i: Integer;
    ss: TSiteSlot;
begin
  Result:= False;
  Debug(dpSpam, section, 'Relogin '+name+' '+IntToStr(hanyszor));
  if hanyszor = 0 then
    l_maxrelogins:= maxrelogins
  else
    l_maxrelogins:= hanyszor;

  if Status = ssOnline then
  begin
    Result:= True;
    exit;
  end;

  relogins:= 0;
  while ((relogins < l_maxrelogins) and (not kilepes) and (not shouldquit)) do
  begin
    try if relogins > 10 then Break; except Break; end;
    Result:= Login(kill);
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
    inc(relogins);
  end;

  if ((not kilepes) and (not shouldquit)) then
  begin
    if not Result then
    begin
      if ((lastResponseCode = 421) and (0 <> Pos('Hammer Protection', lastResponse))) then
      begin
        site.working:= sstDown;
        exit;
      end;
      irc_addtext(todotask, '<c4>SITE <b>%s</b></c> WiLL DOWN %s %d %s', [site.name, s_message, lastResponseCode, lastResponse]);
      for i := 0 to site.slots.Count - 1 do
      begin
        ss:= TSiteSlot(site.slots[i]);
        if ss.Status = ssOnline then
        begin
          // we have at least one slot up and running so no need to setdown all the site
          exit;
        end;
      end;
      site.working:= sstDown;
    end;
  end;
end;

procedure TSiteSlot.Fire;
begin
  event.SetEvent;
end;

function TSiteSlot.Read(read_cmd : String = ''): Boolean;
begin
  try
    Result := Read(read_cmd, True, True, 0);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read: %s', [e.Message]));
      lastResponse:= '';
      lastResponseCode:= 0;
      Result:= False;
      exit;
    end;
  end;
end;

function TSiteSlot.Read(read_cmd : String; raiseontimeout: Boolean; raiseonclose: Boolean; timeout: Integer = 0): Boolean;
label ujra;
var aktread: string;
    numreads: Integer;
    read_start: TDateTime;
begin
  numreads := 0;
  lastResponse:= '';
  lastResponseCode:= 0;
  Result:= False;
  if ((timeout = 0) and (read_cmd = 'read_cmd')) then timeout:= site.connect_timeout * 1000;
  if timeout = 0 then timeout:= site.io_timeout * 1000;

ujra:
  inc(numreads);
  if numreads > 500 then
  begin
    Debug(dpError, section, Format('[ERROR] TSiteSlot.Read numreads', []));
    lastResponse:= '';
    lastResponseCode:= 0;
    error:= 'TSiteSlot.Read numreads';
    Result:= False;
    exit;
  end;

  
  try
    read_start:= Now;
    if not Read(aktread, timeout) then
    begin
      if (error = 'exception') then exit;
      
      if ((error = 'timeout') and (not raiseontimeout)) then exit;

      DestroySocket(False);
      if raiseOnClose then
      begin
        irc_Adderror(todotask, '<c4>[ERROR Read]</c> %s: %s %s %d/%d (%s)', [name, read_cmd, error, MilliSecondsBetween(Now, read_start), timeout, bnc]);
        Result:= False;
      end;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read: %s', [e.Message]));
      lastResponse:= '';
      lastResponseCode:= 0;
      error:= 'TSiteSlot.Read';
      Result:= False;
      exit;
    end;
  end;

  try
    lastResponse:= lastResponse + aktread;
    //Debug(dpSpam, 'protocol', name+' <<'+#13#10+aktread);
    lastResponseCode:= ParseResponseCode(lastResponse);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Read ParseResponseCode: %s', [e.Message]));
      lastResponse:= '';
      lastResponseCode:= 0;
      error:= 'TSiteSlot.Read ParseResponseCode';
      Result:= False;
      exit;
    end;
  end;

  if (lastResponseCode <> 230) then
  begin
    console_addline(name, aktread);
  end;

  if ((lastResponseCode >= 1000) or (lastResponseCode < 100)) then // auto read more
    goto ujra;

  lastio:= Now();

  Result:= True;
end;

function TSiteSlot.Send(s: string): Boolean;
begin
  Result:= False;
  try
    Console_Slot_Add(name, s);
    console_addline(name, s);

    if not WriteLn(s, site.io_timeout * 1000) then
    begin
      irc_Adderror(todotask, '<c4>[ERROR Send]</c> %s: %s (%s)', [name, error, s]);
      DestroySocket(False);
      exit;
    end;
    //Debug(dpSpam, 'protocol', name+' >>'+#13#10+s);
    lastio:= Now();
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Send: %s : %s', [e.Message, s]));
      Result:= False;
      exit;
    end;
  end;
end;

function TSiteSlot.Send(s: string; const Args: array of const): Boolean;
begin
  try
    Result:= Send(Format(s, Args));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSiteSlot.Send: %s : %s', [e.Message, s]));
      Result:= False;
      exit;
    end;
  end;
end;

function TSiteSlot.RCInteger(name: string; def: Integer): Integer;
begin
  Result:= site.RCInteger(name, def);
end;
function TSiteSlot.RCDateTime(name: string; def: TDateTime): TDateTime;
begin
  Result:= site.RCDateTime(name, def);
end;

function TSiteSlot.RCString(name, def: string): string;
begin
  Result:= site.RCString(name, def);
end;

procedure TSiteSlot.SetOnline(value: TSlotStatus);
begin
  fStatus:= value;

  if (fStatus = ssOnline) then
    site.working:= sstUp;
end;

function TSiteSlot.bnc: string;
begin
  Result:= Host+':'+IntToStr( Port);
end;

procedure TSiteSlot.Quit;
begin
  if status <> ssOnline then exit;

  if (not Send('QUIT')) then exit;
  Read('QUIT', False, False);
  DestroySocket(False);
end;

function TSiteSlot.RCBool(name: string; def: Boolean): Boolean;
begin
  Result:= site.RCBool(name, def);
end;

function TSiteSlot.RemoveFile(dir, filename: string): Boolean;
var cmd: string;
begin
  Result:= False;
  if site.legacydirlist then
  begin
    if not Cwd(dir) then exit;
    cmd:= 'DELE '+filename;
  end else
    cmd:= 'DELE '+MyIncludeTrailingSlash(dir)+filename;

  if not Send(cmd) then exit;
  if not Read('DELE') then exit;

  Result:= True;
end;
function TSiteSlot.RemoveDir(dir: string): Boolean;
var cmd: string;
    feljebb: string;
begin
  Result:= False;
  if dir = '' then exit;
  
  if dir[Length(dir)] = '/' then dir:= Copy(dir, 1, Length(dir)-1);
  if site.legacydirlist then
  begin
    feljebb:= Copy(dir, 1, Rpos('/', dir));
    if not Cwd(feljebb) then exit;
    cmd:= 'RMD '+Copy(dir, Rpos('/', dir)+1,1000)
  end else
    cmd:= 'RMD '+dir;

  if not Send(cmd) then exit;
  if not Read('RMD') then exit;

  Result:= True;
end;


function TSiteSlot.Mkdir(dirtocreate: string): Boolean;
var dir: string;
begin
  Result:= False;
  try
    if (site.legacydirlist)  then
    begin
      dir:= dirtocreate;
    end else
      dir:= aktdir+dirtocreate;
    if not Send('MKD %s', [dir]) then exit;
    if not Read('MKD') then exit;
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Mkdir: %s', [e.Message]);
      Result:= False;
    end;
  end;
end;

function TSiteSlot.Pwd(var dir: string): Boolean;
begin
  Result:= False;
  try
    if not Send('PWD') then exit;
    if not Read('PWD') then exit;
//[L] PWD
//[L] 257 "/MOVIES/DivX-XviD-TVRiP/Xxx-Porno" is current directory.

    if lastResponseCode <> 257 then exit;
    dir:= Copy(lastResponse, 6, 100);
    dir:= Copy(dir, 1, Pos('"', dir)-1);

    aktdir:= MyIncludeTrailingSlash( dir );
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Pwd: %s', [e.Message]);
      Result:= False;
    end;
  end;
end;

function TSiteSlot.Dirlist(dir: string; forcecwd: Boolean=False; fulldirlist: Boolean= False): Boolean;
var cmd, kapcsolo: string;
begin
  Result:= False;
  try
    kapcsolo:= '';
    if fulldirlist then
      kapcsolo:= 'a';

    if dir <> '' then
      if not Cwd(dir, forcecwd) then exit;

      if config.ReadBool('indexer','use_custom_dirlist_command',False) then begin
    if ((dir = '') or (site.legacydirlist) or (forcecwd)) then
      cmd:= config.ReadString('indexer','custom_dirlist_command','list -al')
    else
    if dir[1] = '/' then
      cmd:= config.ReadString('indexer','custom_dirlist_command','list -al')+' '+MyIncludeTrailingSlash(dir)
    else
      cmd:= config.ReadString('indexer','custom_dirlist_command','list -al')+' '+aktdir+MyIncludeTrailingSlash(dir);

      end else begin
    if ((dir = '') or (site.legacydirlist) or (forcecwd)) then
      cmd:= 'STAT -l'+kapcsolo
    else
    if dir[1] = '/' then
      cmd:= 'STAT -l'+kapcsolo+' '+MyIncludeTrailingSlash(dir)
    else
      cmd:= 'STAT -l'+kapcsolo+' '+aktdir+MyIncludeTrailingSlash(dir);
      end;

    if not Send(cmd) then exit;
    if not Read('Dirlist') then exit;

    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TSiteSlot.Dirlist: %s', [e.Message]);
      Result:= False;
    end;
  end;
end;

function TSiteSlot.TranslateFilename(filename: string): string;
begin
  Result:= filename;
  if ((filename[1] <> '/') and (not site.legacydirlist)) then
    Result:= aktdir + filename;
end;

procedure TSiteSlot.SetDownloadingFrom(const Value: Boolean);
begin
  if Value <> fDownloadingFrom then
  begin
    fDownloadingFrom := Value;
    if fDownloadingFrom then
      site.num_dn:= site.num_dn + 1
    else
      site.num_dn:= site.num_dn - 1;
  end;
end;

procedure TSiteSlot.SetUploadingTo(const Value: Boolean);
begin
  if Value <> fUploadingTo then
  begin
    fUploadingTo := Value;
    if fUploadingTo then
      site.num_up:= site.num_up + 1
    else
      site.num_up:= site.num_up - 1;
  end;
end;

procedure TSiteSlot.SetTodotask(Value: TTask);
begin
  if fTodotask <> Value then
  begin
    fTodotask := Value;
    if fTodoTask <> nil then
      site.freeslots:= site.freeslots - 1
    else
      site.freeslots:= site.freeslots + 1;
  end;
end;

{ TSite }

constructor TSite.Create(name: string);
var i: Integer;
begin
  if (name = admin_sitename) then
  begin
    self.name:= name;
    working:= sstUp;

    slots:= TObjectList.Create();
    for i:= 1 to admin_siteslots do
      slots.Add(TSiteSlot.Create(self, i-1));

    RecalcFreeslots;

    exit;
  end;
//  siteinvited:= False;
  self.name:= name;
  
  debug(dpSpam, section, 'Site %s is creating', [name]);

  foutofannounce:= 0;
// nullazni a felfedezendo beallitasokat
  sitesdat.WriteInteger('site-'+name, 'sw', Integer(sswUnknown));
  working:= sstUnknown;



  // rakjuk rendbe a direket
  if ((RCString('predir', '') <> '') and (sectiondir['PRE'] = '')) then
  begin
    sectiondir['PRE']:= RCString('predir', '');
    sitesdat.DeleteKey('site-'+self.name, 'predir');
  end;

  // es a precmd-ket is
  if ((RCString('precmd', '') <> '') and (sectionprecmd['PRE'] = '')) then
  begin
    sectionprecmd['PRE']:= RCString('precmd', '');
    sitesdat.DeleteKey('site-'+self.name, 'precmd');
  end;


  slots:= TObjectList.Create();
  for i:= 1 to RCInteger('slots', 2) do
    slots.Add(TSiteSlot.Create(self, i-1));

  RecalcFreeslots;

  debug(dpSpam, section, 'Site %s has created', [name]);
end;



function TSite.isRouteableTo(sitename:string):boolean;
var i:integer; y:TStringlist;
begin
y:=TStringlist.Create;
y.Sorted:=True;
try
sitesdat.ReadSection('speed-to-' + self.name, y);
if y.IndexOf(sitename) = -1 then result:=False else result:=True;
finally
y.free;
end;
end;

function TSite.isRouteableFrom(sitename:string):boolean;
var i:integer; y:TStringlist;
begin
y:=TStringlist.Create;
y.Sorted:=True;
try
sitesdat.ReadSection('speed-from-' + self.name, y);
if y.IndexOf(sitename) = -1 then result:=False else result:=True;
finally
y.free;
end;
end;


procedure TSiteSlot.Stop;
begin
  if event <> nil then
  begin
    Debug(dpSpam, section, 'Slot %s stop begin', [name]);
    shouldquit:= True;
    event.SetEvent;
    inherited;
    Debug(dpSpam, section, 'Slot %s stop end', [name]);
  end;
end;

procedure TSite.Stop;
var i: Integer;
begin
  Debug(dpSpam, section, 'Site %s stop begin', [name]);
  for i:= 0 to slots.Count -1 do
    TSiteSlot(slots[i]).Stop;
  Debug(dpSpam, section, 'Site %s stop end', [name]);
end;

destructor TSite.Destroy;
begin
  Debug(dpSpam, section, 'Site %s destroy begin', [name]);
  QueueEmpty(name);
  slots.Free;
  Debug(dpSpam, section, 'Site %s destroy end', [name]);
  inherited;
end;

procedure SitesStart;
var x: TStringList;
    i: Integer;
begin
  debug(dpSpam, section, 'SitesStart begin');

  delay_between_connects:= config.readInteger(section, 'delay_between_connects', 200);
  admin_sitename:= config.ReadString(section, 'admin_sitename', 'SLFTP');
  admin_siteslots:= config.ReadInteger(section, 'admin_siteslots', 10);
  maxrelogins:= config.ReadInteger(section, 'maxrelogins', 3);
  autologin:= config.ReadBool(section, 'autologin', False);
  killafter:= config.ReadInteger(section, 'killafter', 0);

  // Add admin site
  sites.Add(TSite.Create(admin_sitename));

  x:= TStringList.Create;
  sitesdat.ReadSections(x);
  for i:= 0 to x.Count -1 do
    if 1 = Pos('site-', x[i]) then
      sites.Add(TSite.Create(Copy(x[i], 6, 1000)));
  x.Free;
  debug(dpSpam, section, 'SitesStart end');
end;


procedure SlotsFire;
var i, j: Integer;
begin
  for i:= 0 to sites.Count-1 do
    for j:= 0 to TSite(sites[i]).slots.Count-1 do
      TSiteSlot(TSite(sites[i]).slots[j]).Fire;
end;

function TSite.RCString(name: string; def: string): string;
begin
  Result:=  sitesdat.ReadString('site-'+self.name, name, def);
end;

function TSite.RCInteger(name: string; def: Integer): Integer;
begin
  Result:=  sitesdat.ReadInteger('site-'+self.name, name, def);
end;
procedure TSite.WCDateTime(name: string; val: TDateTime);
begin
  sitesdat.WriteString('site-'+self.name, name, MyDateToStr(val) );
end;

function TSite.RCDateTime(name: string; def: TDateTime): TDateTime;
begin
  Result:= MyStrToDate( sitesdat.ReadString('site-'+self.name, name, '') );
end;

function TSite.RCBool(name: string; def: Boolean): Boolean;
begin
  Result:=  sitesdat.ReadBool('site-'+self.name, name, def);
end;

procedure SiteStat;
var i: Integer;
    allsites, upsites, downsites, unknown: Integer;
begin
  allsites:= 0;
  upsites:= 0;
  downsites:= 0;
  unknown:= 0;
  for i:= 0 to sites.Count -1 do
  begin
if TSite(sites[i]).name = 'SLFTP' then continue;

    case TSite(sites[i]).working of
      sstUnknown: inc(unknown);
      sstUp: inc(upsites);
      sstDown: inc(downsites);
    end;
    inc(allsites);
  end;

  Console_SiteStat(allsites, upsites, downsites, unknown);
end;

function TSite.GetSkipPreStatus:boolean;
begin
result:=RCBool('skip_pre',False);
end;

procedure TSite.SetSkipPreStatus(value: Boolean);
begin
WCBool('skip_pre',Value);
end;


procedure TSite.SetWorking(value: TSiteStatus);
begin
  if value <> fWorking then
  begin
    fWorking:= value;

  if name = 'SLFTP' then begin
    markeddown:= False;
    Exit;
  end;

    if value = sstUp then
    begin

      irc_addadmin(Format('<c3>SITE <b>%s</b> IS UP</c>', [name]));
      markeddown:= False;

      if RCInteger('autonuke',0) <> 0 then AutoNuke;
      if RCInteger('autoindex',0) <> 0 then AutoIndex;
      //if s.RCString('autologin','-1') <> '-1' then
      if RCInteger('autobnctest',0) <> 0 then AutoBnctest;
      if RCInteger('autorules',0) <> 0 then AutoRules;
      if RCInteger('autodirlist',0) <> 0 then AutoDirlist;
    end else
    if value = sstDown then begin
      irc_addadmin(Format('<c4>SITE <b>%s</b> IS DOWN</c>', [name]));
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

  if (value = sstDown)  then
  begin
    QueueEmpty(name);
  end;
end;


function TSite.Getconnect_timeout: Integer;
begin
  Result:= RCInteger('connect_timeout', 15);
end;


function TSite.GetIdleInterval: Integer;
begin
  Result:= RCInteger('idleinterval', 20);
end;

function TSite.Getio_timeout: Integer;
begin
  Result:= RCInteger('io_timeout', 15);
end;

function TSite.GetMaxDn: Integer;
begin
  Result:= RCInteger('max_dn', 2);
end;

function TSite.GetMaxIdle: Integer;
begin
  Result:= RCInteger('max_idle', 120);
end;

function TSite.GetMaxUp: Integer;
begin
  Result:= RCInteger('max_up', 2);
end;

procedure TSite.Setconnect_timeout(const Value: Integer);
begin
  WCInteger('connect_timeout', Value);
end;

procedure TSite.SetIdleInterval(value: Integer);
begin
  WCInteger('idleinterval', Value);
end;

procedure TSite.Setio_timeout(const Value: Integer);
begin
  WCInteger('io_timeout', Value);
end;

procedure TSite.SetMaxDn(value: Integer);
begin
  WCInteger('max_dn', Value);
end;

procedure TSite.SetMaxIdle(value: Integer);
begin
  WCInteger('max_idle', Value);
end;

procedure TSite.SetMaxUp(value: Integer);
begin
  WCInteger('max_up', Value);
end;

function TSite.Getsslmethod: TSSLMethods;
begin
  Result:= TSSLMethods(RCInteger('sslmethod', Integer(sslAuthTlsSSLv23)));
end;

procedure TSite.Setsslmethod(const Value: TSSLMethods);
begin
  WCInteger('sslmethod', Integer(Value));
end;

procedure TSite.WCBool(name: string; val: Boolean);
begin
  sitesdat.WriteBool('site-'+self.name, name, val);
end;
procedure TSite.WCInteger(name: string; val: Integer);
begin
  sitesdat.WriteInteger('site-'+self.name, name, val);
end;
procedure TSite.WCString(name: string; val: string);
begin
  sitesdat.WriteString('site-'+self.name, name, val);
end;

function TSite.Getsslfxp: TSSLReq;
begin
  Result:= TSSLReq(RCInteger('sslfxp', 0));
end;

procedure TSite.Setsslfxp(const Value: TSSLReq);
begin
  WCInteger('sslfxp', Integer(Value));
end;


function TSite.GetPredir: string;
begin
  Result:= sectiondir['PRE'];
end;


procedure TSite.SetPredir(const Value: string);
begin
  sectiondir['PRE']:= Value;
end;

function TSite.Getlegacydirlist: Boolean;
begin
{ OLD CODE!
  if Software = sswGlftpd then
    Result:= RCBool('legacycwd', config.ReadBool(section, 'legacycwd', False))
    Result:= RCBool('legacycwd', config.ReadBool(section, 'legacycwd', False))
  else
    Result:= True
}
Result:= RCBool('legacycwd', False);
end;

procedure TSite.Setlegacydirlist(const Value: Boolean);
begin
  WCBool('legacycwd', Value);

end;

procedure TSite.SetOutofSpace;
begin
  if ((foutofannounce = 0) or (HoursBetween(Now, foutofannounce) >= 1)) then
  begin
    foutofannounce:= Now();
    irc_addadmin(Format('<c4>Site <b>%s</b> is out of disk space.</c>', [name]));
    QueueEmpty(name);
    if config.ReadBool('sites','set_down_on_out_of_space',False) then begin
      markeddown:= True;
      working:= sstDown;
      RemoveAutoIndex;
      RemoveAutoBnctest;
      RemoveAutoRules;
    end;
  end;
end;

procedure TSite.SetKredits;
begin
  if ((fkreditz = 0) or (HoursBetween(Now, fkreditz) >= 1)) then
  begin
    fkreditz:= Now();
    irc_addadmin(Format('Site %s is out of credits.', [name]));
    QueueEmpty(name);    
    if config.ReadBool('sites','set_down_on_out_of_credits',False) then begin
      markeddown:= True;
      working:= sstDown;
      RemoveAutoIndex;
      RemoveAutoBnctest;
      RemoveAutoRules;
     end;
  end;

end;

function TSite.GetSectionDir(name: string): string;
begin
  Result:= RCString('dir-'+name, '')
end;

procedure TSite.SetSectionDir(name: string; const Value: string);
begin
  if Value <> '' then
    WCString('dir-'+name, Value)
  else
  begin
    DeleteKey('dir-'+name);
  end;
end;

function TSite.GetSections: string;
begin
  Result:= RCString('sections', '');
end;
procedure TSite.SettSections(value: string);
begin
  WCString('sections', value);
end;

procedure TSite.DeleteKey(name: string);
begin
  sitesdat.DeleteKey('site-'+self.name, name);
end;

function TSite.GetSectionAffil(name: string): string;
begin
  Result:= RCString('affils-'+name, '')
end;

procedure TSite.SetSectionAffil(name: string; const Value: string);
begin
  if Value <> '' then
    WCString('affils-'+name, Value)
  else
  begin
    DeleteKey('affils-'+name);
  end;
end;

function TSite.GetSectionPreTime(Name: string): integer;
begin
  Result := RCInteger('pretime-' + Name, -1);
end;

procedure TSite.SetSectionPreTime(Name: string; const Value: integer);
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

function TSite.IsPretimeOk(section: string; rlz_pretime: TDateTime): boolean;
var
  sec_pretime: integer;
begin
  // set default pretime to 10 min
  sec_pretime := config.ReadInteger('taskpretime','default_pretime',600);

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
  end else
  begin
    Result := False;
  end;
end;

function TSite.GetPretime(section: string): string;
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
    Result := Format('%2.2d Weeks %1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 604800, (sec_pretime div 86400) mod
      7, (sec_pretime div 3600) mod 24, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else
  if sec_pretime >= 86400 then
    Result := Format('%1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 86400, (sec_pretime div 3600) mod
      24, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else
  if sec_pretime >= 3600 then
    Result := Format('%2.2d Hour %2.2d Min %2.2d Sec', [sec_pretime div 3600, (sec_pretime div 60) mod 60, sec_pretime mod 60])
  else
  if sec_pretime >= 60 then
    Result := Format('%2.2d Min %2.2d Sec', [(sec_pretime div 60) mod 60, sec_pretime mod 60])
  else
    Result := Format('%2.2d Sec', [sec_pretime mod 60]);
end;

function TSite.IsAffil(section, affil: string): Boolean;
var x: TStringList;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= sectionaffil[section];
  Result:= x.IndexOf(affil) <> -1;
  x.Free;
end;
function TSite.IsSection(section: string): Boolean;
var x: TStringList;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= sections;
  Result:= x.IndexOf(section) <> -1;
  x.Free;
end;
function TSite.IsUser(user: string): Boolean;
var x: TStringList;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= leechers;
  Result:= x.IndexOf(user) <> -1;
  if not Result then
  begin
    x.DelimitedText:= traders;
    Result:= x.IndexOf(user) <> -1;
  end;
  x.Free;
end;


function TSite.SetSections(sections: string; remove: Boolean): string;
var x: TStringList;
    ss: string;
    i: Integer;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= self.sections;
  for i:= 1 to 1000 do
  begin
    ss:= SubString(sections, ' ', i);
    if ss = '' then Break;

    if x.IndexOf(ss) <> -1 then
    begin
      if remove then
        x.Delete(x.IndexOf(ss))
    end
    else
      x.Add(ss);
  end;
  x.Sort;
  self.sections:= x.DelimitedText;
  Result:= x.DelimitedText;
  x.Free;
end;

function TSite.SetLeechers(users: string; remove: Boolean): string;
var x: TStringList;
    ss: string;
    voltmar: Boolean;
    i, maxleechers: Integer;
begin
  voltmar:= True;
  maxleechers:= RCInteger('maxleechers', -1);
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= self.leechers;
//  irc_addtexT('debug: '+IntToStr(maxleechers)+' '+x.DelimitedText);
  for i:= 1 to 1000 do
  begin
    ss:= SubString(users, ' ', i);
    if ss = '' then Break;

    if x.IndexOf(ss) <> -1 then
    begin
      if remove then
        x.Delete(x.IndexOf(ss))
    end
    else
    begin
      if ((maxleechers = -1) or (x.Count+1 <= maxleechers)) then
        x.Add(ss)
      else
      begin
        if not voltmar then
        begin
          // irc_Addtext('Limit reached');
          voltmar:= True;
        end;
      end;
    end;
  end;
  x.Sort;
  self.leechers:= x.DelimitedText;
  Result:= x.DelimitedText;
  x.Free;
end;

function TSite.SetTraders(users: string; remove: Boolean): string;
var x: TStringList;
    ss: string;
    i, maxtraders: Integer;
    voltmar: Boolean;
begin
  maxtraders:= RCInteger('maxtraders', -1);
  voltmar:= False;
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= self.traders;
  for i:= 1 to 1000 do
  begin
    ss:= SubString(users, ' ', i);
    if ss = '' then Break;

    if x.IndexOf(ss) <> -1 then
    begin
      if remove then
        x.Delete(x.IndexOf(ss))
    end
    else
    begin
      if ((maxtraders = -1) or (x.Count+1 <= maxtraders)) then
        x.Add(ss)
      else
      begin
        if not voltmar then
        begin
          // irc_Addtext('Limit reached');
          voltmar:= True;
        end;
      end;
    end;
  end;
  x.Sort;
  self.traders:= x.DelimitedText;
  Result:= x.DelimitedText;
  x.Free;
end;

function TSite.SetAffils(section, affils: string; remove: Boolean = False): string;
var x: TStringList;
    ss: string;
    i: Integer;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= sectionaffil[section];
  for i:= 1 to 1000 do
  begin
    ss:= SubString(affils, ' ', i);
    if ss = '' then Break;

    if x.IndexOf(ss) <> -1 then
    begin
      if remove then
        x.Delete(x.IndexOf(ss))
    end
    else
      x.Add(ss);
  end;
  x.Sort;
  sectionaffil[section]:= x.DelimitedText;
  Result:= x.DelimitedText;
  x.Free;
end;

function TSite.GetLeechers: string;
begin
  Result:= RCString('leechers', '');
end;

function TSite.GetTraders: string;
begin
  Result:= RCString('traders', '');
end;

procedure TSite.SettLeechers(value: string);
begin
  WCString('leechers', value);
end;

procedure TSite.SettTraders(value: string);
begin
  WCString('traders', value);
end;

function TSite.GetUsers: string;
begin
  Result:= Format('<b>%s</b> %s',[leechers,traders]);
end;

function TSite.FreeLeechSlots: Integer;
var
    x: TStringList;
begin
  Result:= RCInteger('maxleechers', -1);
  if Result = -1 then exit;

  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.DelimitedText:= leechers;
  if x.Count <= Result then dec(Result, x.Count) else  Result:= 0;
  x.Free;
end;
function TSite.FreeTraderSlots: Integer;
var
    x: TStringList;
begin
  Result:= RCInteger('maxtraders', -1);
  if Result = -1 then exit;

  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.DelimitedText:= traders;
  if x.Count <= Result then dec(Result, x.Count) else  Result:= 0;
  x.Free;
end;

procedure TSite.AutoBnctest;
var t: TLoginTask;
begin
  t:= FetchAutoBnctest;
  if t <> nil then exit;

  // nincs, addolni kell.
  t:= TLoginTask.Create('', '', name, False, True);
  t.dontremove:= True;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSite.AutoBnctest AddTask: %s', [e.Message]));
    end;
  end;
end;

procedure TSite.AutoRules;
var t: TRulesTask;
begin
  t:= FetchAutoRules;
  if t <> nil then exit;

  // nincs, addolni kell.
  t:= TRulesTask.Create('', '', name);
  t.dontremove:= True;
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
var t: TAutoDirlistTask;
begin
  t:= FetchAutoDirlist;
  if t <> nil then exit;

  // nincs, addolni kell.
  t:= TAutoDirlistTask.Create('', '', name);
  t.startat:= RcDateTime('nextautodirlist', 0);
  t.dontremove:= True;
  try
    AddTask(t);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSite.AutoDirlist AddTask: %s', [e.Message]));
    end;
  end;
end;

procedure TSite.AutoCrawler;
var t: TAutoCrawlerTask;
begin
  t:= FetchAutoCrawler;
  if t <> nil then exit;

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
end;


procedure TSite.AutoNuke;
var t: TAutoNukeTask;
begin
  t:= FetchAutoNuke;
  if t <> nil then exit;

  // nincs, addolni kell.
  t:= TAutoNukeTask.Create('', '', name);
  t.startat:= RcDateTime('nextautonuke', 0);
  t.dontremove:= True;
  AddTask(t);
end;

procedure TSite.AutoIndex;
var t: TAutoIndexTask;
begin
  if nil <> FetchAutoIndex then exit;

  // nincs, addolni kell.
  t:= TAutoIndexTask.Create('', '', name);
  t.startat:= RcDateTime('nextautoindex', 0);
  t.dontremove:= True;
  AddTask(t);
end;

function TSite.FetchAutoIndex: TAutoIndexTask;
var i: Integer;
    t: TAutoIndexTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TAutoIndexTask) then
      begin
        t:= TAutoIndexTask(tasks[i]);
        if (t.site1 = name)  then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;

function TSite.FetchAutoDirlist: TAutoDirlistTask;
var i: Integer;
    t: TAutoDirlistTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TAutoDirlistTask) then
      begin
        t:= TAutoDirlistTask(tasks[i]);
        if (t.site1 = name)  then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;

function TSite.FetchAutoCrawler: TAutoCrawlerTask;
var i: Integer;
    t: TAutoCrawlerTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TAutoCrawlerTask) then
      begin
        t:= TAutoCrawlerTask(tasks[i]);
        if (t.site1 = name)  then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;


function TSite.FetchAutoNuke: TAutoNukeTask;
var i: Integer;
    t: TAutoNukeTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TAutoNukeTask) then
      begin
        t:= TAutoNukeTask(tasks[i]);
        if (t.site1 = name)  then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;

function TSite.FetchAutoBnctest: TLoginTask;
var i: Integer;
    t: TLoginTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TLoginTask) then
      begin
        t:= TLoginTask(tasks[i]);
        if (t.site1 = name) and (t.readd)  then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;

function TSite.FetchAutoRules: TRulesTask;
var i: Integer;
    t: TRulesTask;
begin
  Result:= nil;
  for i:= 0 to tasks.Count -1 do
  begin
    try
      if (tasks[i] is TRulesTask) then
      begin
        t:= TRulesTask(tasks[i]);
        if (t.site1 = name) then
        begin
          Result:= t;
          exit;
        end;
      end;
    except
      Result:= nil;
    end;
  end;
end;

procedure TSite.RemoveAutoIndex;
var
    t: TAutoIndexTask;
begin
  t:= FetchAutoIndex;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;

procedure TSite.RemoveAutoBnctest;
var
    t: TLoginTask;
begin
  t:= FetchAutoBnctest;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;

procedure TSite.RemoveAutoRules;
var t: TRulesTask;
begin
  t:= FetchAutoRules;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;

procedure TSite.RemoveAutoNuke;
var
    t: TAutoNukeTask;
begin
  t:= FetchAutoNuke;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;

procedure TSite.RemoveAutoDirlist;
var
    t: TAutoDirlistTask;
begin
  t:= FetchAutoDirlist;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;

procedure TSite.RemoveAutoCrawler;
var
    t: TAutoCrawlerTask;
begin
  t:= FetchAutoCrawler;
  if ((t <> nil) and (t.slot1 = nil)) then
    t.ready:= True;
end;


procedure TSite.Auto;
begin
  if RCInteger('autobnctest', 0) > 0 then
    AutoBnctest;

  if RCInteger('autorules', 0) > 0 then
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
var i: Integer;
begin
  for i:= 0 to sites.Count -1 do
    TSite(sites[i]).Auto;
end;

function TSite.Software: TSiteSW;
begin
  if self.sw <> sswUnknown then
    Result:= self.sw
  else
    Result:=  TSiteSw(sitesdat.ReadInteger('site-'+name, 'sw', Integer(sswUnknown)));
end;

function TSite.IsLeecher(user: string): Boolean;
var x: TStringList;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= leechers;
  Result:= x.IndexOf(user) <> -1;
  x.Free;
end;

function TSite.IsTrader(user: string): Boolean;
var x: TStringList;
begin
  x:= TStringList.Create;
  x.Delimiter:= ' ';
  x.CaseSensitive:= False;
  x.DelimitedText:= traders;
  Result:= x.IndexOf(user) <> -1;
  x.Free;
end;

function TSite.GetNoannounce: Boolean;
begin
  Result:= RCBool('noannounce', False);
end;

procedure TSite.SetNoAnnounce(const Value: Boolean);
begin
  WCBool('noannounce', Value);
end;

function TSite.GetSectionPrecmd(name: string): string;
begin
  Result:= RCString('precmd-'+name, '')
end;

procedure TSite.SetSectionPrecmd(name: string; const Value: string);
begin
  if Value <> '' then
    WCString('precmd-'+name, Value)
  else
  begin
    DeleteKey('precmd-'+name);
  end;
end;

function TSite.GetSw: TSiteSw;
begin
  Result:= TSiteSw(RCInteger('sw', 0));
end;

procedure TSite.SetSw(Value: TSiteSw);
begin
  WCInteger('sw', Integer(Value));
end;

function TSite.GetRank(section: string): Integer;
begin
  Result:= RCInteger('ranklock-'+section, 0);
  if Result = 0 then
  begin
    Result:= RCInteger('ranklock', 0);
    if Result = 0 then
    begin
      Result:= RCInteger('rank-'+section, 1);
    end;
  end;
end;

procedure TSite.SetRank(section: string; Value: Integer);
begin
  if Value <> 0 then
    WCInteger('rank-'+section, Value)
  else
    DeleteKey('rank-'+section);
end;

function TSite.GetRankLock(section: string): Integer;
begin
  Result:= RCInteger('ranklock-'+section, 0);
  if Result = 0 then
  begin
    Result:= RCInteger('ranklock', 0);
  end;
end;

procedure TSite.SetRankLock(section: string; Value: Integer);
begin
  if ((section ='') or (section = '*')) then
  begin
    if Value <> 0 then
      WCInteger('ranklock', Value)
    else
      DeleteKey('ranklock');
  end else
  begin
    if Value <> 0 then
      WCInteger('ranklock-'+section, Value)
    else
      DeleteKey('ranklock-'+section);
  end;
end;

function TSiteSlot.MdtmSeconds(filename: string): Integer;
begin
  Result:= 0;

  filename:= TranslateFilename(filename);

  if not Send('MDTM %s', [filename]) then exit;
  if not Read('MDTM') then exit;

  if mdtmre.exec(lastresponse) then
    Result:= StrToIntDef(mdtmre.Match[6], 0);
end;

procedure TSite.SetNumDn(const Value: Integer);
begin
  if Value >= 0 then
    fNumDn := Value;
end;

procedure TSite.SetNumUp(const Value: Integer);
begin
  if Value >= 0 then
    fNumUp := Value;
end;

procedure TSite.SetFreeSlots(const Value: Integer);
begin
  if Value >= 0 then
    fFreeslots := Value;
end;

procedure TSite.RecalcFreeslots;
var i: Integer;
    ss: TSiteSlot;
    fs: Integer;
begin
  fs:= 0;
  for i:= 0 to slots.Count -1 do
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
    ss:= TSiteSlot(slots[i]);
    if ss.todotask = nil then
      inc(fs);
  end;

  ffreeslots:= fs;
end;

procedure TSite.FullLogin;
var i: Integer;
    ss: TSiteSlot;
    fs: Integer;
begin
  fs:= 0;
  for i:= 0 to slots.Count -1 do
  begin
    ss:= TSiteSlot(slots[i]);
    if ((ss.Status <> ssOnline) and (ss.todotask = nil))then
    begin
      ss.ReLogin(1, False, 'FullLogin');
    end;
  end;

  ffreeslots:= fs;
end;

procedure TSite.SetIRCNick(value: string);
begin
WCString('IRCNick',Value);
end;

function TSite.GetIRCNick:string;
begin
result:= RCString('IRCNick','');
end;

procedure TSite.SetProxyName(value: string);
begin
WCString('ProxyName',Value);
end;

function TSite.GetProxyName;
begin
result:= RCString('ProxyName','!!NOIN!!');
end;

function TSite.GetNoLoginMSG:boolean;
begin
Result:=RCBool('NoLoginMSG',False);
end;

procedure TSite.SetNoLoginMSG(value:boolean);
begin
WCBool('NoLoginMSG',value);
end;


function TSite.GetPermDownStatus:boolean;
begin
  result:=RCBool('permdown',False);
end;
procedure TSite.SetPermDownStatus(value:boolean);
begin
WCBool('permdown',value);
end;

end.
