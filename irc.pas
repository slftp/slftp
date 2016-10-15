unit irc;

interface

uses Classes, SyncObjs, Contnrs, SysUtils, tasksunit, sltcp;

type
  TIRCChannroles = record
    Name: AnsiString;
    Description: AnsiString;
  end;

  TMyIrcThread = class(TslTCPThread)
  private

    irc_lock: TCriticalSection;

    irc_last_read: TDateTime;
    registered: Boolean;
    irc_last_written: tdatetime;
    lastservername: AnsiString;

    function GetIrcSSL: Boolean;
    procedure SetIrcSSL(value: Boolean);

    function GetIrcFlood: Integer;
    procedure SetIrcFlood(value: Integer);
    function GetIrcNick: AnsiString;
    procedure SetIrcNick(value: AnsiString);
    function GetIrcANick: AnsiString;
    procedure SetIrcANick(value: AnsiString);
    function GetIrcUsername: AnsiString;
    procedure SetIrcUsername(value: AnsiString);
    function GetIrcIdent: AnsiString;
    procedure SetIrcIdent(value: AnsiString);
    function GetIrcPassword: AnsiString;
    procedure SetIrcPassword(value: AnsiString);

    procedure Setmanglehost(value: boolean);
    function Getmanglehost: boolean;

    procedure Setinvisible(value: boolean);
    function Getinvisible: boolean;

    function IrcRegister: Boolean;
    function IrcProcessLine(s: AnsiString): Boolean;
    function IrcProcess: Boolean;
    function IrcPing(cumo: AnsiString): Boolean;
    procedure IrcPrivMsg(s: AnsiString);
    function ShouldJoinGame: Boolean;
    procedure ClearSiteInvited;
    function ChannelsList: AnsiString;

    procedure chanjoin(chan, nick: AnsiString);
    procedure BncCsere;

    function RCBool(name: AnsiString; def: Boolean): Boolean;
    function RCString(name, def: AnsiString): AnsiString;
    function RCInt(name: AnsiString; def: integer): integer;

    procedure WCInt(name: AnsiString; val: integer);
    procedure WCString(name: AnsiString; val: AnsiString);
    procedure WCBool(name: AnsiString; val: boolean);

    //BotNick Stuff
    function GetBotIRCNick: AnsiString;
    procedure SetBotIRCNick(value: AnsiString);
    //Proxy Stuff
    function GetProxyName: AnsiString;
    procedure SetProxyName(value: AnsiString);
    //NickServ Stuff
    //    function GetNickServNick:string;
    //    procedure SetNickServNick(value:string);
    //    function GetNickServPassw:string;
    //    procedure SetNickServPassw(value:string);
    //    function GetNSCommandLine:string;
    //    function IdentifyNickname:boolean;

  public
    shouldrestart: Boolean;
    shouldjoin: Boolean;
    netname: AnsiString;
    status: AnsiString;
    channels: TStringList;

    procedure IrcSendPrivMessage(channel, plainmsgformat: AnsiString; const args: array of const); overload;
    procedure IrcSendPrivMessage(channel, plainmsg: AnsiString); overload;
    function IrcSendPrivMessage(oneliner: AnsiString): Boolean; overload;
    procedure IrcSetupSocket;
    procedure chanpart(chan, nick: AnsiString);
    function IrcConnect: Boolean;
    procedure IrcQuit;
    function ChanNicks(chan: AnsiString): AnsiString;
    constructor Create(netname: AnsiString);
    procedure Execute; override;
    destructor Destroy; override;

    function IrcWrite(s: AnsiString; hide: boolean = False): Boolean;

    property flood: Integer read GetIrcFlood write SetIrcFlood;
    property ssl: Boolean read GetIrcSSL write SetIrcSSL;

    property irc_nick: AnsiString read GetIrcNick write SetIrcNick;
    property irc_anick: AnsiString read GetIrcANick write SetIrcANick;
    property irc_username: AnsiString read GetIrcUsername write SetIrcUsername;
    property irc_ident: AnsiString read GetIrcIdent write SetIrcIdent;
    property ircpassword: AnsiString read GetIrcPassword write SetIrcPassword;

    property BotNick: AnsiString read GetBotIRCNick write SetBotIRCNick;
    property ProxyName: AnsiString read GetProxyName write SetProxyName;

    property MangleHost: boolean read Getmanglehost write Setmanglehost;
    property Invisible: boolean read Getinvisible write Setinvisible;

    //    property NickServNick:string read GetNickServNick write SetNickServNick;
    //    property NickServPassword:string read GetNickServPassw write SetNickServpassw;
  end;

  //<!--- IRC Colors -----------------------------------------------------------!>

  //function Bold(s: string): string; overload;
  //function Bold(s: Integer): string; overload;

procedure IrcStart;
procedure irc_Addtext_b(const netname, channel: AnsiString; msg: AnsiString); overload;
procedure irc_Addtext(const netname, channel: AnsiString; msg: AnsiString); overload;
procedure irc_Addtext(const netname, channel: AnsiString; msgFormat: AnsiString; Args: array of const); overload;
procedure irc_Addtext(task: TTask; msg: AnsiString); overload;
procedure irc_Addtext(task: TTask; msgFormat: AnsiString; Args: array of const); overload;
function irc_Addtext_by_key(key, msg: AnsiString): Integer;
procedure IrcProcessCommand(const netname, channel: AnsiString; msg: AnsiString);
procedure irc_Addadmin(msg: AnsiString); overload;
procedure irc_AddAdmin(msgFormat: AnsiString; Args: array of const); overload;

procedure irc_AddConsole(msg: AnsiString); overload;

procedure irc_Addstats(msgirc: AnsiString); overload;
procedure irc_AddstatsB(msgirc: AnsiString); overload;

procedure irc_Adderror(task: TTask; msg: AnsiString); overload;
procedure irc_Adderror(msgirc: AnsiString); overload;
procedure irc_Adderror(task: TTask; msgFormat: AnsiString; Args: array of const); overload;

procedure irc_AddINFO(msgirc: AnsiString); overload;
procedure irc_AddINFO(msgFormat: AnsiString; Args: array of const); overload;

function CommandAuthorized(const netname, channel, cmd: AnsiString): Boolean;

procedure irc_SendAddPre(msgirc: AnsiString);
procedure irc_SendSPEEDSTATS(msgirc: AnsiString);
procedure irc_SendINDEXER(msgirc: AnsiString);
procedure irc_SendRANKSTATS(msgirc: AnsiString);
procedure irc_SendROUTEINFOS(msgirc: AnsiString);
procedure irc_SendRACESTATS(msgirc: AnsiString);
procedure irc_SendIRCEvent(msgirc: AnsiString);

function FindIrcnetwork(netname: AnsiString): TMyIrcThread;

procedure IrcInit;
procedure IrcUninit;
procedure ircStop;

function IrcRestart: boolean;

function mynickname: AnsiString;
function irccmdprefix: AnsiString;

var
  myIrcThreads: TObjectList = nil;
  irc_queue: TStringList;
  irc_queue_nets: TStringList;

const
  irc_chanroleindex = 20;
  (*
  ircchanroles: array [0..irc_chanroleindex] of TIRCChannroles = (
  (Name:'ADMIN',Description:'Give an IRC Chanel Admin privilege'),
  (Name:'KB',Description:'Allows u to send kb commands'),
  (Name:'STATS',Description:'Announces new KB hits and status message'),
  (Name:'ERROR',Description:'Send Error messages.'),
  (Name:'INFO',Description:'Announces --'),
  (Name:'INDEXER',Description:'Announces Autoindexer process'),
  (Name:'GROUP',Description:'Give an IRC Chanel Group privilege, pre, spread, check and so on.'),
  (Name:'NUKE',Description:'Give an IRC Chanel Nuke privilege, nuke and unnuke'),
  (Name:'IRCEVENT',Description:'Give an IRC Chanel Nuke privilege, nuke and unnuke'),
  (Name:'SPEEDSTATS',Description:'Announces --'),
  (Name:'RACETATS',Description:'Announces --'),
  (Name:'RANKSTATS',Description:'Announces --'),
  (Name:'PRECATCHSTATS',Description:'Announces --'),
  (Name:'ROUTEINFOS',Description:'Announces --'),
  (Name:'SKIPLOG',Description:'Announces --'),
  (Name:'ADDPRE',Description:'Give an IRC Chanel ADDPRE privilege, allows you to fill the internal dupedb.'),
  (Name:'ADDNFO',Description:'Give an IRC Chanel ADDNFO privilege, -- gone?'),
  (Name:'ADDURL',Description:'Give an IRC Chanel ADDURL privilege'),
  (Name:'ADDIMDB',Description:''),
  (Name:'ADDPREECHO',Description:''),
  (Name:'ADDGN',Description:'')
  );
  *)

  irc_chanroles: array[0..irc_chanroleindex] of AnsiString = (
    'ADMIN', 'STATS', 'ERROR', 'INFO', 'INDEXER', 'GROUP', 'NUKE', 'IRCEVENT', 'ADDPRE',
    'ADDTVMAZE', 'ADDURL', 'ADDIMDB', 'ADDPREECHO', 'SPEEDSTATS', 'RACESTATS',
    'RANKSTATS', 'PRECATCHSTATS', 'SKIPLOG', 'ROUTEINFOS',
    'KB', 'ADDGN'
    );

implementation

uses debugunit, configunit, ircblowfish, irccolorunit, precatcher, console,
  socks5, versioninfo, helper, mystrings, DateUtils, irccommandsunit,
  sitesunit, taskraw, queueunit, mainthread, dbaddpre, dbtvinfo, dbaddurl, dbaddimdb,
  dbaddgenre
{$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

const
  section = 'irc';

function mynickname: AnsiString;
begin
  result := config.ReadString(section, 'nickname', 'sl');
end;

function irccmdprefix: AnsiString;
begin
  result := config.ReadString(section, 'cmdprefix', '!');
end;

function FindIrcnetwork(netname: AnsiString): TMyIrcThread;
var
  i: Integer;
begin
  Result := nil;
  try
    for i := myIrcThreads.Count - 1 downto 0 do
      if AnsiSameText((myIrcThreads[i] as TMyIrcThread).netname, netname) then
      begin
        Result := myIrcThreads[i] as TMyIrcThread;
        exit;
      end;
  except
    on e: exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] FindIrcnetwork: %s', [e.Message]));
      Result := nil;
    end;
  end;
end;

procedure irc_Addtext_b(const netname, channel: AnsiString; msg: AnsiString); overload;
var
  direct_echo: TMyIrcThread;
begin
  if slshutdown then
    exit;

  if ((netname = 'CONSOLE') or (netname = '')) then
  begin
    try
      console_addline(channel, msg);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] irc_Addtext_b CONSOLE: %s', [e.Message]));
      end;
    end;
    exit;
  end;

  try

    if (config.ReadBool(section, 'direct_echo', False)) then
    begin
      direct_echo := FindIrcnetwork(netname);
      if (direct_echo <> nil) then
      begin
        direct_echo.IrcSendPrivMessage(channel + ' ' + msg);
      end;
    end
    else
    begin
      irc_queue.Add(channel + ' ' + msg);
      irc_queue_nets.Add(netname);
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] irc_Addtext_b: %s', [e.Message]));
    end;
  end;
end;

procedure irc_Addtext(const netname, channel: AnsiString; msg: AnsiString); overload;
begin
  try
    irc_addtext_b(netname, channel, ReplaceThemeMSG(msg));
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_Addtext.irc_addtext_b: %s', [e.Message]);
    end;
  end;
end;

procedure irc_Addtext(const netname, channel: AnsiString; msgFormat: AnsiString; Args: array of const); overload;
begin
  try
    irc_Addtext(netname, channel, Format(msgFormat, Args));
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_Addtext.netname_channel: %s', [e.Message]);
    end;
  end;
end;

procedure irc_Addtext(task: TTask; msg: AnsiString); overload;
begin
  if ((task <> nil) and (task.netname <> '') and (task.channel <> '')) then
    irc_Addtext(task.netname, task.channel, msg)
  else
    irc_Addadmin(msg);
end;

procedure irc_Addtext(task: TTask; msgFormat: AnsiString; Args: array of const); overload;
var
  s: AnsiString;
begin
  try
    s := Format(msgFormat, Args);
    irc_addtext(task, s);
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_Addtext.Task: %s', [e.Message]);
    end;
  end;
end;

procedure irc_AddstatsB(msgirc: AnsiString); overload;
var
  b: TIrcBlowKey;
  i: Integer;
begin
  try
    for i := chankeys.Count - 1 downto 0 do
    begin
      b := TIrcBlowKey(chankeys[i]);
      if b.HasKey('STATS') then
        IrcLineBreak(b.netname, b.channel, msgirc);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_AddstatsB: %s', [e.Message]);
    end;
  end;
end;

function irc_Addtext_by_key(key, msg: AnsiString): Integer;
var
  b: TIrcBlowKey;
  i, j: Integer;
  s, ss: AnsiString;
begin
  Result := 0;
  try
    for i := chankeys.Count - 1 downto 0 do
    begin
      b := TIrcBlowKey(chankeys[i]);
      if b.HasKey(key) then
      begin
        inc(Result);

        s := msg;
        for j := 1 to 1000 do
        begin
          ss := SubString(s, #13#10, j);
          if ss = '' then
            break;
          irc_addtext(b.netname, b.channel, '%s', [ss]);
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_Addtext_by_key: %s', [e.Message]);
    end;
  end;
end;

procedure irc_AddAdmin(msgFormat: AnsiString; Args: array of const); overload;
begin
  try
    irc_AddAdmin(Format(msgFormat, args));
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_AddAdmin: %s', [e.Message]);
    end;
  end;
end;

procedure irc_Addadmin(msg: AnsiString); overload;
begin
  irc_addtext('CONSOLE', 'Admin', msg);
  irc_Addtext_by_key('ADMIN', msg);
end;

procedure irc_AddConsole(msg: AnsiString); overload;
begin
  irc_addtext('CONSOLE', 'Admin', msg);
end;

procedure irc_Addstats(msgirc: AnsiString); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('STATS', msgirc)
end;

procedure irc_Adderror(task: TTask; msgFormat: AnsiString; Args: array of const); overload;
begin
  try
    irc_Adderror(task, Format(msgFormat, Args));
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_Adderror: %s', [e.Message]);
    end;
  end;
end;

procedure irc_Adderror(task: TTask; msg: AnsiString); overload;
begin
  irc_Addtext_by_key('ERROR', msg);
end;

procedure irc_Adderror(msgirc: AnsiString); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ERROR', msgirc);
end;

procedure irc_AddINFO(msgirc: AnsiString); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('INFO', msgirc);
end;

procedure irc_AddINFO(msgFormat: AnsiString; Args: array of const); overload;
begin
  try
    irc_AddINFO(Format(msgformat, args));
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] irc_AddINFO: %s', [e.Message]);
    end;
  end;
end;

procedure irc_SendIRCEvent(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('IRCEVENT', msgirc);
end;

procedure irc_SendAddPre(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ADDPREECHO', msgirc);
end;

procedure irc_SendSPEEDSTATS(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('SPEEDSTATS', msgirc);
end;

procedure irc_SendINDEXER(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('INDEXER', msgirc);
end;

procedure irc_SendRANKSTATS(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('RANKSTATS', msgirc)
end;

procedure irc_SendROUTEINFOS(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ROUTEINFOS', msgirc)
end;

procedure irc_SendRACESTATS(msgirc: AnsiString);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('RACESTATS', msgirc)
end;

procedure IrcStart;
var
  x: TStringList;
  i: Integer;
  channel, nn: AnsiString;
  b: TIrcBlowKey;
begin

  // register other sitechan keys
  x := TStringList.Create;
  sitesdat.ReadSections(x);
  for i := 0 to x.Count - 1 do
  begin
    if (1 = Pos('channel-', x[i])) then
    begin
      nn := SubString(x[i], '-', 2);
      channel := Copy(x[i], Length('channel-') + Length(nn) + 2, 1000);
      b := irc_RegisterChannel(nn, channel, sitesdat.ReadString(x[i], 'blowkey', ''), sitesdat.ReadString(x[i], 'chankey', ''), sitesdat.ReadBool(x[i], 'inviteonly', False));
      b.names := ' ' + sitesdat.ReadString(x[i], 'names', '') + ' ';
    end;
  end;

  // create other network threads
  sitesdat.ReadSections(x);
  if (config.ReadBool(section, 'enabled', True)) then
  begin
    for i := 0 to x.Count - 1 do
      if (1 = Pos('ircnet-', x[i])) then
      begin
        myIrcThreads.Add(TMyIrcThread.Create(Copy(x[i], 8, 1000)));
        sleep(500);
      end;
  end;
  x.Free;

end;

{ TMyIrcThread }

constructor TMyIrcThread.Create(netname: AnsiString);
begin
  irc_lock := TCriticalSection.Create;

  channels := TStringList.Create;

  irc_last_written := Now;

  self.netname := netname;
  status := 'creating...';
  shouldquit := False;
  shouldrestart := False;

  //  flood:= config.ReadInteger(section, 'flood', 333);
  Debug(dpMessage, section, 'Irc thread for %s has started', [netname]);
  console_add_ircwindow(netname);
  if sitesdat.ReadString('ircnet-' + netname, 'host', '') <> '' then
  begin
    // converting old entries to new
    sitesdat.WriteString('ircnet-' + netname, 'bnc_host-0', sitesdat.ReadString('ircnet-' + netname, 'host', ''));
    sitesdat.WriteInteger('ircnet-' + netname, 'bnc_port-0', sitesdat.ReadInteger('ircnet-' + netname, 'port', 0));
    sitesdat.DeleteKey('ircnet-' + netname, 'host');
    sitesdat.DeleteKey('ircnet-' + netname, 'port');
  end;
  inherited Create(False);
end;

destructor TMyIrcThread.Destroy;
begin
  irc_lock.Free;

  status := 'destroying...';
  console_delwindow(netname);
  channels.Free;
  inherited;
end;

function TMyIrcThread.GetBotIRCNick: AnsiString;
begin
  //result:=irc_nick ;
  result := sitesdat.ReadString('ircnet-' + netname, 'nick', irc_nick);
end;

procedure TMyIrcThread.SetBotIRCNick(value: AnsiString);
begin
  sitesdat.WriteString('ircnet-' + netname, 'nick', value);
  irc_nick := value;
end;

procedure TMyIrcThread.SetProxyName(value: AnsiString);
begin
  sitesdat.WriteString('ircnet-' + netname, 'ProxyName', Value);
end;

function TMyIrcThread.GetProxyName: AnsiString;
begin
  result := sitesdat.ReadString('ircnet-' + netname, 'ProxyName', '!!NOIN!!');
end;

procedure TMyIrcThread.IrcSetupSocket;
begin
  irc_last_read := Now();
  registered := False;
  Disconnect;

  Host := sitesdat.ReadString('ircnet-' + netname, 'bnc_host-0', '');
  Port := sitesdat.ReadInteger('ircnet-' + netname, 'bnc_port-0', 0);
  //    ssl:= sitesdat.ReadBool('ircnet-'+netname, 'ssl', False);

  if ((ProxyName = '!!NOIN!!') or (ProxyName = '0') or (ProxyName = '')) then
    SetupSocks5(self, config.ReadBool(section, 'socks5', False))
  else
    mSLSetupSocks5(proxyname, self, True);

end;

function TMyIrcThread.IrcWrite(s: AnsiString; hide: boolean = False): Boolean;
begin
  Result := False;
  irc_lock.Enter;
  try
    irc_last_read := Now();
    try
      Result := WriteLn(Copy(s, 1, MaxInt));
    except on E: Exception do
        Debug(dpError, section, '[EXCEPTION] TMyIrcThread.IrcWrite : %s', [e.Message]);
    end;
  finally
    irc_lock.Leave;
  end;

  try
    console_addline(netname, s);
  except on E: Exception do
      Debug(dpError, section, '[EXCEPTION] TMyIrcThread.IrcWrite(console_addline) : %s', [e.Message]);
  end;

end;

(*

function TMyIrcThread.IrcWrite(s: string; hide:boolean = False): Boolean;
begin
  Result:= False;
  try
    irc_last_read:= Now();
    irc_lock.Enter;
    try
      Result:= WriteLn(Copy(s,1, MaxInt));
    finally
      irc_lock.Leave;
    end;
    //if not hide then Debug(dpSpam, section, netname+'>> '+s);
    try
    console_addline(netname, s);
    except on E: Exception do
    Debug(dpError, section, '[EXCEPTION] TMyIrcThread.IrcWrite(console_addline) : %s', [e.Message]);
    end;

  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TMyIrcThread.IrcWrite : %s', [e.Message]);
    end;
  end;
end;

*)

function TMyIrcThread.IrcConnect: Boolean;
var
  LOurAddr: AnsiString;
begin
  Result := False;
  status := 'connecting...';

  if not Connect(config_connect_timeout * 1000) then
    exit;

  if ssl then
  begin
    status := 'ssl handshake...';
    if not TurnToSSL(config_io_timeout * 1000) then
      exit;
  end;

  status := 'connected...';

  if (Length(slSocket.localip) > 0) then
    LOurAddr := slSocket.localip
  else
    LOurAddr := sltcp_localaddresses[0];

  (*
  NICK rsc
  USER rsc 127.0.0.1 irc.link-net.hu :Realname
  *)

  if ircpassword <> '' then
    if not IrcWrite('PASS ' + ircpassword, Hide_plain_text) then
      exit;

  if not IrcWrite('NICK ' + irc_nick, Hide_plain_text) then
    exit;
  if not IrcWrite(
    Format('USER %s %s %s :%s', [irc_username, LOurAddr, Host, irc_ident])) then
    exit;

  Result := True;
end;

procedure TMyIrcThread.IrcQuit;
begin
  IrcWrite('QUIT :I live in a dark world, where no light shines through');
  //  Sleep(100);
  //  myIrcClient.Disconnect; // majd a free ugyis rendbetesz
end;

function TMyIrcThread.IrcPing(cumo: AnsiString): Boolean;
begin
  Result := IrcWrite('PONG ' + cumo);
  lastservername := cumo;
end;

function CommandAuthorized(const netname, channel, cmd: AnsiString): Boolean;
var
  b: TIrcBlowkey;
begin
  try
    b := FindIrcBlowfish(netname, channel, False);
    // fo kulccsal barmit barhol meg lehet tenni.
    if ((netname = 'CONSOLE') or (b.HasKey('ADMIN'))) then
    begin
      Result := True;
      exit;
    end;

    Result := False;

    // privat uzenet, nem fo kulccsal. itt csak transfer/stop/uptime engedelyezett
    if (channel[1] <> '#') then
    begin
      if ((cmd = 'transfer') or (cmd = 'stop') or (cmd = 'uptime')) then
      begin
        Result := True;
        exit;
      end;

    end
    else
    begin
      // akarmilyen csatorna, itt mar csak a specialis csatornarol fogadunk el
      if (b.HasKey('GROUP') and ((cmd = 'sites') or (cmd = 'bnc') or (cmd = 'news') or (cmd = 'spread') or
        (cmd = 'stop') or (cmd = 'pre') or (cmd = 'prelist') or (cmd = 'help'))) then
      begin
        Result := True;
        exit;
      end
      else if (b.HasKey('NUKE') and ((cmd = 'nuke') or (cmd = 'unnuke') or (cmd = 'nukedir') or (cmd = 'autonuke'))) then
      begin
        Result := True;
        exit;
      end
      else if (b.HasKey('STATS') and (1 = Pos('stat', cmd))) then
      begin
        Result := True;
        exit;
      end
      else if (b.HasKey('KB') and ((cmd = 'kbadd') or (cmd = 'kbshow') or (cmd = 'kblist') or (cmd = 'kbextra'))) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    Result := False;
  end;
end;

procedure IrcProcessCommand(const netname, channel: AnsiString; msg: AnsiString);
var
  cmd: AnsiString;
  i, c: integer;
  params: AnsiString;
begin
  cmd := SubString(msg, ' ', 1);

  // ACL.
  if not CommandAuthorized(netname, channel, cmd) then
    exit;

  i := FindIrcCommand(cmd);
  if i = 0 then
  begin
    irc_Addtext(netname, channel, Format('"<b>%s</b>" is unknown! try %shelp', [cmd, irccmdprefix]));
    exit;
  end;

  if netname = 'CONSOLE' then
    irc_addtext('CONSOLE', 'Admin', irccmdprefix + msg);

  params := Trim(RightStrv2(msg, length(cmd) + 1));
  c := Count(' ', params);
  if params <> '' then
    inc(c);

  if ((irccommands[i].minparams <> -1) and (irccommands[i].minparams > c)) then
  begin
    irc_Addtext(netname, channel, 'Too few parameters.');
    exit;
  end;
  if ((irccommands[i].maxparams <> -1) and (irccommands[i].maxparams < c)) then
  begin
    irc_Addtext(netname, channel, 'Too few parameters.');
    exit;
  end;

  TIRCCommandThread.Create(irccommands[i].hnd, netname, channel, params, irccommands[i].cmd);
end;

procedure TMyIrcThread.IrcPrivMsg(s: AnsiString);
var
  channel, msg, nick: AnsiString;
  is_crypted_msg: Boolean;
  l: Integer;
  b: TIrcBlowkey;
  adminnet: TMyIrcThread;
begin
  channel := SubString(s, ' ', 3);
  if channel = '' then
    exit;

  nick := Copy(s, 2, Pos('!', s) - 2);
  if nick = irc_nick then
  begin
    exit;
  end;

  if channel = irc_nick then // nickname az a sajat nickem amivel ircen vagyok
  begin
    try
      //privat uzenet, ki kell hamozni a nikket
      channel := nick;
      msg := RightStrv2(s, Pos(' ', s));
      msg := RightStrv2(msg, Pos(':', msg));
      //irc_Addadmin('->PRIVMSG from: <b>'+nick+'</b>@'+netname+' : '+msg);
      if ((nick <> config.ReadString(section, 'nickname', 'slftp')) and config.ReadBool(section, 'admin_forward_msgs', True)) then
      begin
        adminnet := FindIrcnetwork(config.ReadString(section, 'admin_net', 'SLFTP'));
        adminnet.IrcWrite('PRIVMSG ' + config.ReadString(section, 'admin_nick', 'slftp') + ' :' + ReplaceThemeMSG('->PRIVMSG from: <b>' + nick + '</b>@' + netname + ' : ' + msg));
      end;
      exit;
    except on E: Exception do
        Debug(dpError, section, Format('[EXCEPTION] in adminnet.IrcWrite: %s', [e.Message]));
    end;
  end;
  msg := RightStrv2(s, Pos(' ', s));
  msg := RightStrv2(msg, Pos(':', msg));
  l := length(msg);
  if l = 0 then
    exit;

  if ((msg[1] = #1) and (msg[l] = #1)) then
  begin
    // CTCP
    msg := Trim(msg);

    if Substring(msg, ' ', 1) = 'PING' then
      IrcWrite('PRIVMSG ' + channel + ' :' + #1 + 'PING ' + IntToStr(DateTimeToUnix(now)) + #1);

    exit;
  end;

  is_crypted_msg := False;
  if ((1 = Pos('+OK ', msg)) and (l > 5)) then
  begin
    is_crypted_msg := True;
    try
      msg := TrimRight(irc_decrypt(netname, channel, Copy(msg, 5, 1000)));
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in irc_decrypt: %s', [e.Message]));
        msg := '';
      end;
    end;
  end
  else if ((1 = Pos('mcps ', msg)) and (l > 6)) then
  begin
    is_crypted_msg := True;
    try
      msg := TrimRight(irc_decrypt(netname, channel, Copy(msg, 6, 1000)));
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in irc_decrypt: %s', [e.Message]));
        msg := '';
      end;
    end;
    //if not Hide_plain_text then Debug(dpSpam, section, 'PLAIN: '+msg);
  end;

  if ((1 = Pos('+OK ', msg)) or (1 = Pos('mcps ', msg))) then
  begin
    exit;
  end;

  try
    b := FindIrcBlowfish(netname, channel, False);
    if (b = nil) then
    begin
      exit;
    end;
  except
    exit;
  end;

  console_addline(netname + ' ' + channel, Format('[%s] <%s> %s', [FormatDateTime('hh:nn:ss', Now), nick, msg]));

  if (b.HasKey('ADDPRE')) then
  begin
    try
      if dbaddpre_Process(netname, channel, nick, msg) then
      begin
        Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
        Exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in dbaddpre_Process: : %s', [e.Message]));
        exit;
      end;
    end;
  end;
  (*
    if (b.HasKey('ADDNFO')) then
    begin
      try
        if dbaddnfo_Process(netname, channel, nick, msg) then
        begin
          Debug(dpSpam, section, '<-- '+channel+' '+nick+' '+msg);
          Exit;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] in dbaddnfo_Process: : %s', [e.Message]));
          exit;
        end;
      end;
    end;
             *)

  if (b.HasKey('ADDTVMAZE')) then
  begin
    try
      if dbTVInfo_Process(netname, channel, nick, msg) then
      begin
        Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
        Exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in dbaddnfo_Process: : %s', [e.Message]));
        exit;
      end;
    end;
  end;

  if (b.HasKey('ADDURL')) then
  begin
    try
      if dbaddurl_Process(netname, channel, nick, msg) then
      begin
        Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
        Exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in dbaddurl_Process: : %s', [e.Message]));
        exit;
      end;
    end;
  end;

  if (b.HasKey('ADDIMDB')) then
  begin
    try
      if dbaddimdb_Process(netname, channel, nick, msg) then
      begin
        Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
        Exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in dbaddimdb_Process: : %s', [e.Message]));
        exit;
      end;
    end;
  end;

  if (b.HasKey('ADDGN')) then
  begin
    try
      if dbaddgenre_Process(netname, channel, nick, msg) then
      begin
        Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
        Exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] in dbaddgenre_Process: : %s', [e.Message]));
        exit;
      end;
    end;
  end;

  if (is_crypted_msg) then
  begin
    if (1 = Pos(irccmdprefix, msg)) then
    begin
      // commandhandlerrel kezdodik
      try
        msg := Copy(msg, length(irccmdprefix) + 1, 1000);
        //
        IrcProcessCommand(netname, channel, msg);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] in IrcProcessCommand: : %s', [e.Message]));
        end;
      end;
      Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
      exit;
    end;
  end;

  Debug(dpSpam, section, '--> ' + channel + ' ' + nick + ' ' + msg);
  try
    PrecatcherProcess(netname, channel, nick, msg);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] in precatcher: : %s', [e.Message]));
    end;
  end;
  Debug(dpSpam, section, '<-- ' + channel + ' ' + nick + ' ' + msg);
end;

function TMyIrcThread.ChannelsList: AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to channels.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ', ';
    Result := Result + channels.Names[i];
  end;
end;

procedure TMyIrcThread.chanpart(chan, nick: AnsiString);
var
  x: TStringList;
  i: Integer;
begin
  if nick = irc_nick then
  begin
    i := channels.IndexOfName(chan);
    if i = -1 then
    begin
      Exit;
    end;

    irc_Addadmin('Try to part ' + channels.Names[i]);

    try
      channels.BeginUpdate;
      channels.Delete(i);
    finally
      channels.EndUpdate;
    end;
    console_delwindow(netname + ' ' + chan);
    WriteLn('PART ' + chan);
    status := 'online (' + channelsList + ')';
    exit;
  end;

  x := TStringList.Create;
  x.DelimitedText := channels.Values[chan];
  i := x.IndexOf(nick);
  if i <> -1 then
    x.Delete(i);
  channels.Values[chan] := x.DelimitedText;

  status := 'online (' + channelsList + ')';

  x.Free;
end;

procedure TMyIrcThread.chanjoin(chan, nick: AnsiString);
var
  x: TStringList;
  i: Integer;
begin
  x := TStringList.Create;
  try
    x.DelimitedText := channels.Values[chan];
    i := x.IndexOf(nick);
    if i = -1 then
      x.Add(nick);
    channels.Values[chan] := x.DelimitedText;

    status := 'online (' + channelsList + ')';
  finally
    x.Free;
  end;
end;

function TMyIrcThread.IrcProcessLine(s: AnsiString): Boolean;
var
  msg, nick, snick, s1, s2, chan: AnsiString;
  b: TIrcBlowkey;
  i: Integer;
  crypted: boolean;
begin
  crypted := false;
  console_addline(netname, s);
  if s = '' then
  begin
    Result := True;
    exit;
  end;
  //  Result:= False;

  irc_last_read := Now();
  //Debug(dpSpam, section, netname+'<< '+s);
  if 1 = Pos('PING :', s) then
    IrcPing(Copy(s, 6, 1000))
  else {// MODES=} if ((registered = False) and ((0 <> Pos(' 266 ', s)) or (0 <> Pos(' 376 ', s)) or (0 <> Pos(' 422 ', s)))) then
    registered := True
  else
  begin
    s1 := SubString(s, ' ', 1);

    if (s1 = 'ERROR') then
    begin
      //  02-20 20:28:16.887 (12C8) [irc         ] << ERROR :Closing Link: 213.186.38.105 (*** Banned )
      irc_addadmin(Format('<%s> %s', [netname, RightStrV2(s, 7)]));
    end;

    s2 := SubString(s, ' ', 2);

    if (0 = Pos(':' + irc_nick + '!', s)) then
    begin
      if (s2 = 'PRIVMSG') then
      begin
        try
          IrcPrivMsg(s);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] IrcPrivMsg: %s', [e.Message]));
            Result := True;
            exit;
          end;
        end;
      end
      else if (s2 = 'INVITE') then
      begin
        chan := Copy(SubString(s, ' ', 4), 2, 1000);
        irc_Addadmin('INVITE on ' + netname + ' to ' + chan + ' by ' + Copy(SubString(SubString(s, ' ', 1), '!', 1), 2, 100));
        b := FindIrcBlowfish(netname, chan, False);
        if nil <> b then
        begin
          // oke, ha hivtak hat belepunk
          if not WriteLn(Trim('JOIN ' + b.channel + ' ' + b.chankey)) then
          begin
            Result := True;
            exit;
          end;
        end;
      end;
    end;

    snick := Copy(s, 2, Pos('!', s) - 2);
    //:rsc!rsctm@coctail.sda.bme.hu KICK #femforgacs rsctm :no reason
    if (s2 = 'KICK') then
    begin
      chan := SubString(s, ' ', 3);
      nick := SubString(s, ' ', 4);

      if (nick <> irc_nick) then
      begin
        if config.ReadBool(section, 'echo_kick_events', False) then
        begin
          irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>KICK</b> %s/%s %s by %s', [netname, chan, nick, snick]));
          console_addline(netname + ' ' + chan, Format('--> KICK %s by %s <--', [nick, snick]));
        end;
      end;
      chanpart(chan, nick);
    end
    else if (s2 = 'JOIN') then
    begin
      chan := Copy(SubString(s, ' ', 3), 2, 1000);
      snick := Copy(s, 2, Pos('!', s) - 2);
      console_add_ircwindow(netname + ' ' + chan);
      if (snick <> irc_nick) then
      begin
        if config.ReadBool(section, 'echo_join_part_events', False) then
        begin
          irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>JOIN</b> %s/%s %s', [netname, chan, snick]));
          console_addline(netname + ' ' + chan, Format('--> JOIN %s <--', [snick]));
        end;
      end;

      chanjoin(chan, snick);
    end
    else if (s2 = 'PART') then
    begin
      chan := SubString(s, ' ', 3);
      snick := Copy(s, 2, Pos('!', s) - 2);
      if (snick <> irc_nick) then
      begin
        console_addline(netname + ' ' + chan, Format('--> PART %s <--', [snick]));
        if config.ReadBool(section, 'echo_join_part_events', False) then
          irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>PART</b> %s/%s %s', [netname, chan, snick]));
      end;
      chanpart(chan, snick);
    end
    else if (s2 = 'TOPIC') then
    begin
      s1 := Copy(s, Pos(':', s) + 1, MaxInt);
      chan := SubString(s, ' ', 3);
      s1 := Copy(s1, Pos(' ', s1), MaxInt);
      msg := Copy(s1, Pos(':', s1) + 1, MaxInt);
      //irc_addinfo(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s',[netname, chan, Copy(s1, Pos(':', s1)+1, MaxInt)]));
      if (1 = Pos('+OK ', msg)) then
      begin
        try
          crypted := True;
          if config.ReadBool(section, 'echo_topic_change_events', False) then
            irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, irc_decrypt(netname, chan, Copy(msg, 5, MaxInt))]));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] in irc_decrypt: %s', [e.Message]));
          end;
        end;
      end
      else if (1 = Pos('mcps ', msg)) then
      begin
        try
          crypted := True;
          if config.ReadBool(section, 'echo_topic_change_events', False) then
            irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, irc_decrypt(netname, chan, Copy(msg, 6, MaxInt))]));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] in irc_decrypt: %s', [e.Message]));
          end;
        end;
      end;
      if not crypted then
      begin
        if config.ReadBool(section, 'echo_topic_change_events', False) then
          irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, msg]));
      end;
    end
    else if (s2 = 'NICK') then
    begin
      snick := Copy(s, 2, Pos('!', s) - 2);
      if (snick <> irc_nick) then
      begin
        if config.ReadBool(section, 'echo_nick_change_events', False) then
          irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>NICK</b> %s %s -> %s', [netname, snick, Copy(s, RPos(':', s) + 1, MaxInt)]));
      end;
    end
    else if ((s2 = 'QUIT') and (snick <> irc_nick)) then
    begin
      s1 := Copy(s, RPos(':', s) + 1, 1000);
      for i := 0 to channels.Count - 1 do
      begin
        chan := channels.Names[i];
        console_addline(netname + ' ' + chan, Format('--> QUIT %s (%s) <--', [snick, s1]));
        chanpart(chan, snick);
      end;
    end;

  end;

  Result := True;
end;

procedure TMyIrcThread.IrcSendPrivMessage(channel, plainmsgformat: AnsiString; const args: array of const);
begin
  IrcSendPrivMessage(channel, FormaT(plainmsgformat, args));
end;

procedure TMyIrcThread.IrcSendPrivMessage(channel, plainmsg: AnsiString);
begin
  irc_last_read := Now();
  //if not Hide_plain_text then Debug(dpSpam, section, 'PLAIN: '+plainmsg);
  IrcWrite('PRIVMSG ' + channel + ' :' + irc_encrypt(netname, channel, plainmsg, True));
  console_addline(netname + ' ' + channel, Format('[%s] <%s> %s', [FormatDateTime('hh:nn:ss', Now), irc_nick, plainmsg]));
  irc_last_written := Now;
end;

function TMyIrcThread.IrcSendPrivMessage(oneliner: AnsiString): Boolean;
var
  channel, msg: AnsiString;
begin
  Result := False;
  channel := SubString(oneliner, ' ', 1);
  msg := Copy(oneliner, length(channel) + 2, 1000);
  if channel = '' then
  begin
    if msg <> '' then
      ircwrite(msg);
    Result := True;
    exit;
  end;
  if ((channel[1] = '#') and (channels.IndexOfName(channel) = -1)) then
    exit;
  IrcSendPrivMessage(channel, msg);
  Result := True;
end;

function TMyIrcThread.ShouldJoinGame: Boolean;
var
  i: Integer;
  b: TIrcBlowkey;
  s: TSite;
  r: TRawTask;
  added: Boolean;
begin
  Result := False;

  debug(dpSpam, section, netname + ': ShouldJoinGame');

  shouldjoin := False;

  // most akkor belepunk mindenhova illetve addoljuk az invite taskokat
  for i := 0 to chankeys.Count - 1 do
  begin
    b := chankeys[i] as TIrcBlowkey;
    if ((b.netname = netname) and (-1 = channels.IndexOf(b.channel)) and (b.channel[1] = '#')) then
    begin
      // be kell lepni erre a csatornara
      if (not b.inviteonly) then // meghivot kene kuldeni
      begin
        if b.chankey <> '' then
        begin
          debug(dpSpam, section, '%s: Trying to join %s with key %s', [netname, b.channel, b.chankey]);
          if not WriteLn(Trim('JOIN ' + b.channel + ' ' + b.chankey)) then
          begin
            exit;
          end;
        end
        else
        begin
          debug(dpSpam, section, '%s: Trying to join %s without key', [netname, b.channel]);
          if not WriteLn(Trim('JOIN ' + b.channel)) then
          begin
            exit;
          end;
        end;
      end;
    end;
  end;

  (* no idea what this is here for, parting channels w/o blowkeys does not really make sense and the code doesn't work either
  // itt pedig azt nezzuk kell e valahonnan partolni
  for i:= 0 to channels.Count -1 do
    if nil = FindIrcBlowfish(netname, channels[i], False) then begin
      debug(dpSpam, section, '%s: Parting %s because blowkey is nil', [netname, b.channel]);
      if not WriteLn('PART '+channels[i]) then
      begin
        exit;
      end;
  end;
  *)

  added := False;
  for i := 0 to sites.Count - 1 do
  begin
    s := sites[i] as TSite;
    if ((s.RCString('ircnet', '') = netname) and (not s.siteinvited) and (not s.PermDown)) then
    begin
      debug(dpSpam, section, '%s: Trying to issue SITE INVITE to join chans as %s', [netname, irc_nick]);
      s.siteinvited := True;
      r := TRawTask.Create('', '', s.name, '', 'SITE INVITE ' + irc_nick);
      AddTask(r);
      added := True;
    end;
  end;
  if added then
    QueueFire;

  Result := True;
end;

function TMyIrcThread.IrcProcess: Boolean;
var
  s, osszes: AnsiString;
  //    s2: string;
  i: Integer;
begin
  Result := False;
  while ((not shouldrestart) and (not shouldquit)) do
  begin
    if shouldjoin then
      ShouldJoinGame();

    try
      while ReadLn(s, osszes, 100) do
      begin
        if (s = '') then
          Break;
        if not IrcProcessLine(s) then
          break;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TMyIrcThread.ReadLn: %s', [e.Message]));
        Exit;
      end;
    end;

    if ((error <> '') and (error <> 'timeout')) then
      exit;

    if ((not config.ReadBool(section, 'direct_echo', False)) and (MilliSecondsBetween(Now, irc_last_written) > flood)) then
    begin
      i := 0;
      try
        while (i < irc_queue.Count) do
        begin
          if (irc_queue_nets[i] = netname) then
          begin
            IrcSendPrivMessage(irc_queue[i]);
            irc_queue.Delete(i);
            irc_queue_nets.Delete(i);
            Break;
          end;
          inc(i);
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TMyIrcThread.IrcProcess: %s', [e.Message]));
          irc_queue.Clear;
          irc_queue_nets.Clear;
          Exit;
        end;
      end;
    end;

    if ((SecondsBetween(Now, irc_last_read) > 60) and (lastservername <> '')) then
      ircwrite('PING ' + lastservername);

    if SecondsBetween(Now, irc_last_read) > config.ReadInteger(section, 'timeout', 120) then
    begin
      error := 'IRC Server didnt PING, it might be down';
      exit;
    end;
  end;

  Result := True;
end;

function TMyIrcThread.IrcRegister: Boolean;
var
  s, osszes: AnsiString;
  elotte: TDateTime;
  userid, pass: AnsiString;
  i: Integer;
  perform: AnsiString;
begin
  Result := False;
  registered := False;
  status := 'registering...';

  elotte := Now();
  while (SecondsBetween(Now, elotte) < config.ReadInteger(section, 'register_timeout', 10)) do
  begin
    while (ReadLn(s, osszes, 1000)) do
    begin
      if not IrcProcessLine(s) then
        exit;
    end;

    if ((error <> '') and (error <> 'timeout')) then
      exit;

    if registered then
      Break;
  end;

  if not registered then
  begin
    error := netname + ': IRC Not registered within io timeout';
    exit;
  end;

  status := 'online...';

  if (config.ReadBool(section, 'manglehost', True) and (MangleHost)) then
  begin
    if not IrcWrite('MODE ' + irc_nick + ' +h') then
    begin
      MangleHost := False;
      exit;
    end;
  end;

  if (Invisible) then
  begin
    if not IrcWrite('MODE ' + irc_nick + ' +i') then
    begin
      Invisible := False;
      exit;
    end;
  end;

  userid := sitesdat.ReadString('ircnet-' + netname, 'oper_userid', '');
  pass := sitesdat.ReadString('ircnet-' + netname, 'oper_password', '');
  if ((userid <> '') and (pass <> '')) then
    if not IrcWrite('OPER ' + userid + ' ' + pass) then
      exit;

  i := 0;
  while (true) do
  begin
    perform := sitesdat.ReadString('ircnet-' + netname, 'perform-' + IntToStr(i), '');
    if (perform = '') then
      break;
    if not IrcWrite(perform) then
    begin
      continue;
    end;
    inc(i);
  end;

  Result := True;
end;

procedure TMyIrcThread.ClearSiteInvited;
var
  s: TSite;
  i: Integer;
begin
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    if s.RCString('ircnet', '') = netname then
      s.siteinvited := False;
  end;
end;

procedure TMyIrcThread.BncCsere;
var
  i: Integer;
  elsobncport, aktbncport: Integer;
  elsobnchost, aktbnchost: AnsiString;
begin
  i := 0;
  elsobncport := 0;
  elsobnchost := '';

  while (true) do
  begin
    aktbnchost := sitesdat.ReadString('ircnet-' + netname, 'bnc_host-' + IntToStr(i), '');
    aktbncport := sitesdat.ReadInteger('ircnet-' + netname, 'bnc_port-' + IntToStr(i), 0);
    if (aktbnchost = '') then
      break;

    if (i = 0) then
    begin
      elsobnchost := aktbnchost;
      elsobncport := aktbncport;
    end
    else
    begin
      sitesdat.WriteString('ircnet-' + netname, 'bnc_host-' + IntToStr(i - 1), aktbnchost);
      sitesdat.WriteInteger('ircnet-' + netname, 'bnc_port-' + IntToStr(i - 1), aktbncport);
    end;

    sitesdat.DeleteKey('ircnet-' + netname, 'bnc_host-' + IntToStr(i));
    sitesdat.DeleteKey('ircnet-' + netname, 'bnc_port-' + IntToStr(i));

    inc(i);
  end;

  if ((i <> 0) and (elsobnchost <> '') and (elsobncport <> 0)) then
  begin
    sitesdat.WriteString('ircnet-' + netname, 'bnc_host-' + IntToStr(i - 1), elsobnchost);
    sitesdat.WriteInteger('ircnet-' + netname, 'bnc_port-' + IntToStr(i - 1), elsobncport);
  end;

end;

procedure TMyIrcThread.Execute;
label
  hiba;
var
  i, m: Integer;
begin

  while (not shouldquit) do
  begin
    try
      shouldrestart := False;
      shouldjoin := True;
      error := '';
      channels.Clear;

      ClearSiteInvited;

      IrcSetupSocket;

      if not IrcConnect then
        goto hiba;

      if not IrcRegister then
        goto hiba;

      if not IrcProcess then
        goto hiba;

      IrcQuit;

      if shouldrestart then
        BncCsere;

      status := 'offline';
      Continue;
      hiba:
      if (not shouldquit) then
      begin
        Debug(dpError, section, netname + ': ' + error + ' Restarting ...');
      end;
      status := 'offline';

      m := config.ReadInteger(section, 'sleep_on_error', 60);
      for i := 1 to m do
        if (not shouldquit) then
        begin
          status := 'sleeping additional ' + IntToStr(m - i) + ' seconds before retrying';
          Sleep(1000);
        end;
      if not shouldquit then
        BncCsere;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TMyIrcThread.Execute: %s', [e.Message]));
        Continue;
      end;
    end;
  end;
end;

function IrcRestart: boolean;
begin
  try
    ircStop;
    IrcStart;
    result := True;
  except
    result := False;
  end;
end;

procedure IrcInit;
begin
  myIrcThreads := TObjectList.Create(True);
  irc_queue := TStringList.Create;
  irc_queue_nets := TStringList.Create;
end;

procedure IrcUnInit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if myircThreads <> nil then
  begin
    myIrcThreads.Free;
    myIrcThreads := nil;
  end;
  irc_queue.Free;
  irc_queue_nets.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

procedure ircStop;
begin
  if myIrcThreads <> nil then
    myIrcThreads.Clear;
end;

function TMyIrcThread.GetIrcNick: AnsiString;
begin
  result := RCString('nick', mynickname);
end;

function TMyIrcThread.GetIrcANick: AnsiString;
begin
  result := RCString('anick', mynickname);
end;

function TMyIrcThread.GetIrcUsername: AnsiString;
begin
  result := RCString('username', mynickname);
end;

function TMyIrcThread.GetIrcIdent: AnsiString;
begin
  result := RCString('ident', mynickname);
end;

function TMyIrcThread.GetIrcPassword: AnsiString;
begin
  result := RCString('password', '');
end;

procedure TMyIrcThread.SetIrcNick(value: AnsiString);
begin
  WCString('nick', value);
end;

procedure TMyIrcThread.SetIrcANick(value: AnsiString);
begin
  WCString('anick', value);
end;

procedure TMyIrcThread.SetIrcUsername(value: AnsiString);
begin
  WCString('username', value);
end;

procedure TMyIrcThread.SetIrcIdent(value: AnsiString);
begin
  WCString('ident', value);
end;

procedure TMyIrcThread.SetIrcPassword(value: AnsiString);
begin
  WCString('password', value);
end;

procedure TMyIrcThread.SetIrcSSL(value: Boolean);
begin
  //
end;

procedure TMyIrcThread.SetIrcFlood(value: Integer);
begin
  //
end;

function TMyIrcThread.GetIrcSSL: boolean;
begin
  result := RCBool('ssl', false);
end;

function TMyIrcThread.GetIrcFlood: integer;
begin
  result := RCInt('flood', 333); //config.ReadInteger('irc', 'flood', 333);
end;

function TMyIrcThread.ChanNicks(chan: AnsiString): AnsiString;
begin
  Result := channels.Values[chan];
end;

procedure TMyIrcThread.Setmanglehost(value: boolean);
begin
  WCBool('manglehost', value);
end;

function TMyIrcThread.Getmanglehost: boolean;
begin
  result := RCBool('manglehost', False); //sitesdat.ReadBool('ircnet-' + netname, 'manglehost', True);
end;

procedure TMyIrcThread.Setinvisible(value: boolean);
begin
  WCBool('invisible', value);
end;

function TMyIrcThread.Getinvisible: boolean;
begin
  result := RCBool('invisible', False); //sitesdat.ReadBool('ircnet-' + netname, 'invisible', True);
end;

(*
function TMyIrcThread.GetNickServNick:string;
begin
result:=RCString('NS_Nick','NickServ');
end;
procedure TMyIrcThread.SetNickServNick(value: string);
begin
WCString('NS_Nick',value);
end;

function TMyIrcThread.GetNickServPassw:string;
begin
result:=RCString('NS_Passw','');
end;
procedure TMyIrcThread.SetNickServPassw(value: string);
begin
WCString('NS_Passw',value);
end;

function TMyIrcThread.GetNSCommandLine:string;
begin
result:=format('%s identify %s',[GetNickServNick,GetNickServPassw]);
end;

function TMyIrcThread.IdentifyNickname:boolean;
begin
//
end;
*)

//function TMyIrcThread
//procedure TMyIrcThread

//WCString('',value);

function TMyIrcThread.RCString(name: AnsiString; def: AnsiString): AnsiString;
begin
  Result := sitesdat.ReadString('ircnet-' + netname, name, def);
end;

function TMyIrcThread.RCBool(name: AnsiString; def: Boolean): Boolean;
begin
  Result := sitesdat.ReadBool('ircnet-' + netname, name, def);
end;

function TMyIrcThread.RCInt(name: AnsiString; def: Integer): integer;
begin
  Result := sitesdat.ReadInteger('ircnet-' + netname, name, def);
end;

procedure TMyIrcThread.WCString(name: AnsiString; val: AnsiString);
begin
  sitesdat.WriteString('ircnet-' + netname, name, val);
end;

procedure TMyIrcThread.WCBool(name: AnsiString; val: boolean);
begin
  sitesdat.WriteBool('ircnet-' + netname, name, val);
end;

procedure TMyIrcThread.WCInt(name: AnsiString; val: integer);
begin
  sitesdat.WriteInteger('ircnet-' + netname, name, val);
end;

end.

