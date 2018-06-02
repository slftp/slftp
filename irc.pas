unit irc;

interface

uses
  Classes, SyncObjs, Contnrs, SysUtils, tasksunit, sltcp;

type
  TIRCChannroles = record
    Name: String;
    Description: String;
  end;

  TMyIrcThread = class(TslTCPThread)
  private

    irc_lock: TCriticalSection;

    irc_last_read: TDateTime;
    registered: Boolean;
    irc_last_written: tdatetime;
    lastservername: String;

    function GetIrcSSL: Boolean;
    procedure SetIrcSSL(value: Boolean);

    function GetIrcFlood: Integer;
    procedure SetIrcFlood(value: Integer);
    function GetIrcNick: String;
    procedure SetIrcNick(value: String);
    function GetIrcANick: String;
    procedure SetIrcANick(value: String);
    function GetIrcUsername: String;
    procedure SetIrcUsername(value: String);
    function GetIrcIdent: String;
    procedure SetIrcIdent(value: String);
    function GetIrcPassword: String;
    procedure SetIrcPassword(value: String);

    procedure Setmanglehost(value: boolean);
    function Getmanglehost: boolean;

    procedure Setinvisible(value: boolean);
    function Getinvisible: boolean;

    function IrcRegister: Boolean;
    function IrcProcessLine(s: String): Boolean;
    function IrcProcess: Boolean;
    function IrcPing(cumo: String): Boolean;
    procedure IrcPrivMsg(const s: String);
    function ShouldJoinGame: Boolean;
    procedure ClearSiteInvited;
    function ChannelsList: String;

    procedure chanjoin(chan, nick: String);
    procedure BncCsere;

    function RCBool(name: String; def: Boolean): Boolean;
    function RCString(name, def: String): String;
    function RCInt(name: String; def: integer): integer;

    //procedure WCInt(name: String; val: integer);
    procedure WCString(name: String; val: String);
    procedure WCBool(name: String; val: boolean);

    //BotNick Stuff
    function GetBotIRCNick: String;
    procedure SetBotIRCNick(value: String);
    //Proxy Stuff
    function GetProxyName: String;
    procedure SetProxyName(value: String);
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
    netname: String;
    status: String;
    channels: TStringList;

    procedure IrcSendPrivMessage(channel, plainmsgformat: String; const args: array of const); overload;
    procedure IrcSendPrivMessage(channel, plainmsg: String); overload;
    function IrcSendPrivMessage(oneliner: String): Boolean; overload;
    procedure IrcSetupSocket;
    procedure chanpart(chan, nick: String);
    function IrcConnect: Boolean;
    procedure IrcQuit;
    function ChanNicks(chan: String): String;
    constructor Create(netname: String);
    procedure Execute; override;
    destructor Destroy; override;

    function IrcWrite(s: String; hide: boolean = False): Boolean;

    property flood: Integer read GetIrcFlood write SetIrcFlood;
    property ssl: Boolean read GetIrcSSL write SetIrcSSL;

    property irc_nick: String read GetIrcNick write SetIrcNick; //< your nickname on this network
    property irc_anick: String read GetIrcANick write SetIrcANick;
    property irc_username: String read GetIrcUsername write SetIrcUsername;
    property irc_ident: String read GetIrcIdent write SetIrcIdent;
    property ircpassword: String read GetIrcPassword write SetIrcPassword;

    property BotNick: String read GetBotIRCNick write SetBotIRCNick;
    property ProxyName: String read GetProxyName write SetProxyName;

    property MangleHost: boolean read Getmanglehost write Setmanglehost;
    property Invisible: boolean read Getinvisible write Setinvisible;

    //    property NickServNick:string read GetNickServNick write SetNickServNick;
    //    property NickServPassword:string read GetNickServPassw write SetNickServpassw;
  end;

procedure irc_Addtext_b(const netname, channel: String; msg: String); overload;
procedure irc_Addtext(const netname, channel: String; msg: String); overload;
procedure irc_Addtext(const netname, channel: String; msgFormat: String; Args: array of const); overload;
procedure irc_Addtext(task: TTask; msg: String); overload;
procedure irc_Addtext(task: TTask; msgFormat: String; Args: array of const); overload;
function irc_Addtext_by_key(key, msg: String): Integer;
procedure IrcProcessCommand(const netname, channel: String; msg: String);
procedure irc_Addadmin(msg: String); overload;
procedure irc_AddAdmin(msgFormat: String; Args: array of const); overload;

procedure irc_AddConsole(msg: String); overload;

procedure irc_Addstats(msgirc: String); overload;
procedure irc_AddstatsB(msgirc: String); overload;

procedure irc_Adderror(task: TTask; msg: String); overload;
procedure irc_Adderror(msgirc: String); overload;
procedure irc_Adderror(task: TTask; msgFormat: String; Args: array of const); overload;

procedure irc_AddINFO(msgirc: String); overload;
procedure irc_AddINFO(msgFormat: String; Args: array of const); overload;

function CommandAuthorized(const netname, channel, cmd: String): Boolean;

procedure irc_SendAddPre(msgirc: String);
procedure irc_SendSPEEDSTATS(msgirc: String);
procedure irc_SendINDEXER(msgirc: String);
procedure irc_SendRANKSTATS(msgirc: String);
procedure irc_SendROUTEINFOS(msgirc: String);
procedure irc_SendRACESTATS(msgirc: String);
procedure irc_SendIRCEvent(msgirc: String);

procedure irc_SendUPDATE(msgirc: String);

function FindIrcnetwork(netname: String): TMyIrcThread;

procedure IrcInit;
procedure IrcStart;
procedure IrcUninit;
procedure ircStop;

function IrcRestart: boolean;

function mynickname: String;
function irccmdprefix: String;

var
  myIrcThreads: TObjectList = nil;
  irc_message_lock: TCriticalSection;
  irc_queue: TStringList;
  irc_queue_nets: TStringList;

const
  irc_chanroleindex = 21;

  irc_chanroles: array[0..irc_chanroleindex] of String = (
    'ADMIN', 'STATS', 'ERROR', 'INFO', 'INDEXER', 'GROUP', 'NUKE', 'IRCEVENT', 'KB',
    'UPDATE',
    'SPEEDSTATS', 'RACESTATS', 'RANKSTATS', 'PRECATCHSTATS', 'SKIPLOG', 'ROUTEINFOS',
    'ADDPRE','ADDTVMAZE', 'ADDURL', 'ADDIMDB', 'ADDPREECHO', 'ADDGN');

implementation

uses
  debugunit, configunit, ircblowfish, irccolorunit, precatcher, console,
  socks5, versioninfo, mystrings, DateUtils, irccommandsunit,
  sitesunit, taskraw, queueunit, mainthread, dbaddpre, dbtvinfo, dbaddurl,
  dbaddimdb, dbaddgenre, news, StrUtils
  {$IFDEF MSWINDOWS}
    , Windows
  {$ENDIF}
  ;

const
  section = 'irc';

function mynickname: String;
begin
  result := config.ReadString(section, 'nickname', 'sl');
end;

function irccmdprefix: String;
begin
  result := config.ReadString(section, 'cmdprefix', '!');
end;

function FindIrcnetwork(netname: String): TMyIrcThread;
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

procedure irc_Addtext_b(const netname, channel: String; msg: String); overload;
var
  direct_echo: TMyIrcThread;
  msgs: TStringList;
  i: Integer;

begin
  if slshutdown then
    exit;

  // actually we want to send this msg to the console
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

  // okay it's not for the console
  irc_message_lock.Enter;
  msgs := TStringList.Create;
  try
    msgs.Text := wraptext(msg, 256);
    try
      if (config.ReadBool(section, 'direct_echo', False)) then
      begin
        direct_echo := FindIrcnetwork(netname);
        if (direct_echo <> nil) then
        begin
          for I := 0 to msgs.Count - 1 do
            direct_echo.IrcSendPrivMessage(channel + ' ' + msgs.Strings[i]);
        end;
      end
      else
      begin
          for I := 0 to msgs.Count - 1 do begin
            irc_queue.Add(channel + ' ' + msgs.Strings[i]);
            irc_queue_nets.Add(netname);
          end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] irc_Addtext_b: %s', [e.Message]));
      end;
    end;
  finally
    irc_message_lock.Leave;
    msgs.Free;
  end;
end;

procedure irc_Addtext(const netname, channel: String; msg: String); overload;
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

procedure irc_Addtext(const netname, channel: String; msgFormat: String; Args: array of const); overload;
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

procedure irc_Addtext(task: TTask; msg: String); overload;
begin
  if ((task <> nil) and (task.netname <> '') and (task.channel <> '')) then
    irc_Addtext(task.netname, task.channel, msg)
  else
    irc_Addadmin(msg);
end;

procedure irc_Addtext(task: TTask; msgFormat: String; Args: array of const); overload;
var
  s: String;
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

procedure irc_AddstatsB(msgirc: String); overload;
var
  b: TIrcBlowKey;
  i: Integer;
begin
  if chankeys = nil then
    exit;

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

function irc_Addtext_by_key(key, msg: String): Integer;
var
  b: TIrcBlowKey;
  i, j: Integer;
  s, ss: String;
begin
  Result := 0;

  if chankeys = nil then
    exit;

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

procedure irc_AddAdmin(msgFormat: String; Args: array of const); overload;
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

procedure irc_Addadmin(msg: String); overload;
begin
  irc_addtext('CONSOLE', 'Admin', msg);
  irc_Addtext_by_key('ADMIN', msg);
end;

procedure irc_AddConsole(msg: String); overload;
begin
  irc_addtext('CONSOLE', 'Admin', msg);
end;

procedure irc_Addstats(msgirc: String); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('STATS', msgirc)
end;

procedure irc_Adderror(task: TTask; msgFormat: String; Args: array of const); overload;
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

procedure irc_Adderror(task: TTask; msg: String); overload;
begin
  irc_Addtext_by_key('ERROR', msg);
end;

procedure irc_Adderror(msgirc: String); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ERROR', msgirc);
end;

procedure irc_AddINFO(msgirc: String); overload;
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('INFO', msgirc);
end;

procedure irc_AddINFO(msgFormat: String; Args: array of const); overload;
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

procedure irc_SendIRCEvent(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('IRCEVENT', msgirc);
end;

procedure irc_SendAddPre(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ADDPREECHO', msgirc);
end;

procedure irc_SendSPEEDSTATS(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('SPEEDSTATS', msgirc);
end;

procedure irc_SendINDEXER(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('INDEXER', msgirc);
end;

procedure irc_SendRANKSTATS(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('RANKSTATS', msgirc)
end;

procedure irc_SendROUTEINFOS(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('ROUTEINFOS', msgirc)
end;

procedure irc_SendRACESTATS(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('RACESTATS', msgirc)
end;

procedure irc_SendUPDATE(msgirc: String);
begin
  if (msgirc = '') then
    exit;
  irc_Addtext_by_key('UPDATE', msgirc)
end;


procedure IrcStart;
var
  x: TStringList;
  i: Integer;
  channel, nn: String;
  b: TIrcBlowKey;
begin
  // register other sitechan keys
  x := TStringList.Create;
  try
    sitesdat.ReadSections(x);
    for i := 0 to x.Count - 1 do
    begin
      if (1 = Pos('channel-', x[i])) then
      begin
        nn := SubString(x[i], '-', 2);
        channel := Copy(x[i], Length('channel-') + Length(nn) + 2, 1000);
        b := irc_RegisterChannel(nn, channel, sitesdat.ReadString(x[i], 'blowkey', ''), sitesdat.ReadString(x[i], 'chankey', ''), sitesdat.ReadBool(x[i], 'inviteonly', False), sitesdat.ReadBool(x[i], 'cbc', False));
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
  finally
    x.Free;
  end;
end;

{ TMyIrcThread }

constructor TMyIrcThread.Create(netname: String);
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

function TMyIrcThread.GetBotIRCNick: String;
begin
  //result:=irc_nick ;
  result := sitesdat.ReadString('ircnet-' + netname, 'nick', irc_nick);
end;

procedure TMyIrcThread.SetBotIRCNick(value: String);
begin
  sitesdat.WriteString('ircnet-' + netname, 'nick', value);
  irc_nick := value;
end;

procedure TMyIrcThread.SetProxyName(value: String);
begin
  sitesdat.WriteString('ircnet-' + netname, 'proxyname', Value);
end;

function TMyIrcThread.GetProxyName: String;
begin
  result := sitesdat.ReadString('ircnet-' + netname, 'proxyname', '!!NOIN!!');
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

function TMyIrcThread.IrcWrite(s: String; hide: boolean = False): Boolean;
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

function TMyIrcThread.IrcConnect: Boolean;
var
  LOurAddr: String;
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

function TMyIrcThread.IrcPing(cumo: String): Boolean;
begin
  Result := IrcWrite('PONG ' + cumo);
  lastservername := cumo;
end;

function CommandAuthorized(const netname, channel, cmd: String): Boolean;
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

procedure IrcProcessCommand(const netname, channel: String; msg: String);
var
  cmd: String;
  i, c: integer;
  params: String;
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

  params := Trim(mystrings.RightStr(msg, length(cmd) + 1));
  c := Count(' ', params);
  if params <> '' then
    inc(c);

  if ((irccommands[i].minparams <> -1) and (irccommands[i].minparams > c)) then
  begin
    irc_Addtext(netname, channel, 'Not enough parameters specified. Try !help %s', [cmd]);
    exit;
  end;
  if ((irccommands[i].maxparams <> -1) and (irccommands[i].maxparams < c)) then
  begin
    irc_Addtext(netname, channel, 'Too many parameters specified. Try !help %s', [cmd]);
    exit;
  end;

  TIRCCommandThread.Create(irccommands[i].hnd, netname, channel, params, irccommands[i].cmd);
end;

procedure TMyIrcThread.IrcPrivMsg(const s: String);
var
  channel, msg, nick, ctcp_event: String;
  is_crypted_msg: Boolean;
  i, FishModeArrayIndex, l: Integer;
  b: TIrcBlowkey;
  {$I common.inc}
begin
  FishModeArrayIndex := -1;
  {
  * full input string 's' looks like:
  :***!znc@znc.in PRIVMSG #channel :Buffer Playback... <-- znc
  :Benti!~IDENT@91.x.x.256 PRIVMSG #sl-test :+OK OKm.j0tnE8Y0hLJOr.r90Sk.q829n/YneL6/IwZZI/.QW05.CrF9M.2L9Uw.mO3pZ1aJDry1TwKts.YDZKL01q1aH.BVbQe/9X/0/1JLntf/1Q5f90d8dea/ <-- a message on any chan
  :SL!SiLLY-BiTCH@C404J4.284M2F.B26DIC.CPDBA5 PRIVMSG #adminchan :+OK 26S0X1DFeaf0W7PLv0RpNcU/0YfTK.v1HXb/zA9IQ0KA/OR/ <-- slftp bot
  :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG #adminchan :+OK 2DshD/5hHM1/ <-- command in admin chan
  :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG #adminchan :omg this shit!! <-- unencrypted msg
  :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :stupid idiot <-- private message to slftp bot [query] (unencrypted)
  :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :VERSION <- CTCP 'version'
  :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :PING 1503674480 <- CTCP 'ping'
  }

  // find channel name or username from your query partner (private message)
  channel := SubString(s, ' ', 3);
  if channel = '' then
  begin
    exit;
  end;

  // find username who wrote the message
  nick := Copy(s, 2, Pos('!', s) - 2);
  if nick = irc_nick then
  begin
    exit;
  end;


  // get the part of 's' starting with PRIVMSG
  {
    e.g.: PRIVMSG #adminchan :+OK 2DshD/5hHM1/
    e.g.: PRIVMSG SL :stupid idiot
  }
  msg := mystrings.RightStr(s, Pos(' ', s));

  // get the msg text which was sent
  {
    e.g.: +OK 2DshD/5hHM1/
    e.g.: stupid idiot
  }
  msg := mystrings.RightStr(msg, Pos(':', msg));

  l := Length(msg);
  if l = 0 then
    exit;

  // check if PRIVMSG #CHANNEL argument is my irc nick
  if channel = irc_nick then
  begin
    {
    :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :stupid idiot
    :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :VERSION
    :eN!TESTBOX@977026.6HADFD.5C4LD7.272AXF PRIVMSG SL :PING 1503674480
    }

    if ((msg[1] = #1) and (msg[l] = #1)) then
    begin
      // CTCP (Client-To-Client-Protocol) which is a special type of communication between IRC Clients
      msg := Trim(msg);
      ctcp_event := Substring(msg, ' ', 1);

      // somehow it starts spamming with no end -- good idea to implement ddos
      if ( (Length(ctcp_event) <> Length(msg)) and (ctcp_event <> 'PING') ) then
        exit;

      if ctcp_event = 'CLIENTINFO' then
      begin
        IrcWrite(Format('PRIVMSG %s :%sCLIENTINFO PING VERSION TIME FINGER%s', [nick, #1, #1]));
      end
      else if ctcp_event = 'PING' then
      begin
        // PING requires a NOTICE and reply needs to contain exactly the same parameters as the original query
        IrcWrite(Format('NOTICE %s :%s%s%s', [nick, #1, msg, #1]));
      end
      else if ctcp_event = 'VERSION' then
      begin
        IrcWrite(Format('PRIVMSG %s :%sVERSION IRC v%s%s', [nick, #1, Get_VersionOnlyString, #1]));
      end
      else if ctcp_event = 'TIME' then
      begin
        IrcWrite(Format('PRIVMSG %s :%sTIME: %s %s%s', [nick, #1, DateToStr(date), TimeToStr(Time), #1]));
      end
      else if ctcp_event = 'FINGER' then
      begin
        IrcWrite(Format('PRIVMSG %s :%sFINGER :%s (anonymous@ftp.net) Idle %d seconds%s', [nick, #1, config.ReadString(section, 'nickname', 'slftp'), DateTimeToUnix(now), #1]));
      end;

      exit;
    end;

    // we received a private message
    if (config.ReadBool(section, 'admin_forward_msgs', True)) then
    begin
      irc_Addadmin(Format('[PRIVMSG] <b>%s</b>@%s : %s', [nick, netname, msg]));
      if ((nick <> config.ReadString(section, 'nickname', 'slftp'))) then
      begin
        // seems not to work...
        if AnsiMatchText(msg, NewsSystemSpamMessages) then
          news.SlftpNewsAdd('IRC', Format('[PRIVMSG] <b>%s</b>@%s : %s', [nick, netname, msg, True]))
        else
          news.SlftpNewsAdd('IRC', Format('[PRIVMSG] <b>%s</b>@%s : %s', [nick, netname, msg]));
      end;
    end;

    exit;
  end;


  is_crypted_msg := False;

  for i := Low(BlowfishIdentificationWords) to High(BlowfishIdentificationWords) do
  begin
    if {$IFDEF UNICODE}StartsText{$ELSE}AnsiStartsText{$ENDIF}(BlowfishIdentificationWords[i], msg) then
    begin
      FishModeArrayIndex := i;
      break;
    end;
  end;

  // handle decryption based on blowfish identification word
  case FishModeArrayIndex of
    0: // CBC '+OK *'
    begin
      if (l > 6) then
      begin
        is_crypted_msg := True;
        try
          msg := TrimRight(irc_cbc_decrypt(netname, channel, Copy(msg, 6, 1000)));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] in irc_cbc_decrypt: %s', [e.Message]));
            exit;
          end;
        end;
      end;
    end;
    1: // ECB '+OK '
    begin
      if (l > 5) then
      begin
        is_crypted_msg := True;
        try
          msg := TrimRight(irc_ecb_decrypt(netname, channel, Copy(msg, 5, 1000)));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] in irc_ecb_decrypt: %s', [e.Message]));
            exit;
          end;
        end;
      end;
    end;
    2: // ECB alternative start-word 'mcps ' (Mircryption)
    begin
      if (l > 6) then
      begin
        is_crypted_msg := True;
        try
          msg := TrimRight(irc_ecb_decrypt(netname, channel, Copy(msg, 6, 1000)));
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] in irc_ecb_decrypt: %s', [e.Message]));
            exit;
          end;
        end;
      end;
    end;
  end;

  // decrypting wasn't successful or failed somehow
  if (is_crypted_msg) and ({$IFDEF UNICODE}StartsText{$ELSE}AnsiStartsText{$ENDIF}(BlowfishIdentificationWords[FishModeArrayIndex], msg)) then
  begin
    Debug(dpMessage, section, Format('[FiSH] Decryption failed for %s: %s', [channel, msg]));
    exit;
  end;


  b := FindIrcBlowfish(netname, channel, False);
  if (b = nil) then
  begin
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
    // commandhandler for slftp commands
    if (1 = Pos(irccmdprefix, msg)) then
    begin
      try
        msg := Copy(msg, length(irccmdprefix) + 1, 1000);
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

  // no case from above matched, let's check if we have a catchadd for it
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

function TMyIrcThread.ChannelsList: String;
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

procedure TMyIrcThread.chanpart(chan, nick: String);
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

    channels.BeginUpdate;
    try
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
  try
    x.DelimitedText := channels.Values[chan];
    i := x.IndexOf(nick);
    if i <> -1 then
      x.Delete(i);
    channels.Values[chan] := x.DelimitedText;

    status := 'online (' + channelsList + ')';
  finally
    x.Free;
  end;
end;

procedure TMyIrcThread.chanjoin(chan, nick: String);
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

function TMyIrcThread.IrcProcessLine(s: String): Boolean;
var
  msg, nick, snick, s1, s2, chan: String;
  b: TIrcBlowkey;
  i, FishModeArrayIndex: Integer;
  crypted: boolean;
  {$I common.inc}
begin
  FishModeArrayIndex := -1;
  crypted := false;
  console_addline(netname, s);
  if s = '' then
  begin
    Result := True;
    exit;
  end;

  irc_last_read := Now();

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
      irc_addadmin(Format('<%s> %s', [netname, mystrings.RightStr(s, 7)]));
    end;

    s2 := SubString(s, ' ', 2);

    // TODO: Add a case for requesting fishkey - if we want to support this!

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


      for i := Low(BlowfishIdentificationWords) to High(BlowfishIdentificationWords) do
      begin
        if {$IFDEF UNICODE}StartsText{$ELSE}AnsiStartsText{$ENDIF}(BlowfishIdentificationWords[i], msg) then
        begin
          FishModeArrayIndex := i;
          break;
        end;
      end;

      // handle decryption of IRC Topics if encrypted
      case FishModeArrayIndex of
        0: // CBC '+OK *'
        begin
          crypted := True;
          try
            if config.ReadBool(section, 'echo_topic_change_events', False) then
              //irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, irc_cbc_decrypt(netname, chan, Copy(msg, 6, MaxInt))]));
              irc_Addadmin(Format('[CBC encrypted Topic] %s : %s', [chan, msg]));
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] in irc_cbc_decrypt for Topic: %s', [e.Message]));
            end;
          end;
        end;
        1: // ECB '+OK '
        begin
          crypted := True;
          try
            if config.ReadBool(section, 'echo_topic_change_events', False) then
              irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, irc_ecb_decrypt(netname, chan, Copy(msg, 5, MaxInt))]));
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] in irc_ecb_decrypt for Topic: %s', [e.Message]));
            end;
          end;
        end;
        2: // ECB alternative start-word 'mcps ' (Mircryption)
        begin
          crypted := True;
          try
            if config.ReadBool(section, 'echo_topic_change_events', False) then
              irc_SendIRCEvent(Format('<c5>[IRC]</c> <b>TOPIC</b> %s/%s %s', [netname, chan, irc_ecb_decrypt(netname, chan, Copy(msg, 6, MaxInt))]));
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] in irc_ecb_decrypt for Topic: %s', [e.Message]));
            end;
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

procedure TMyIrcThread.IrcSendPrivMessage(channel, plainmsgformat: String; const args: array of const);
begin
  IrcSendPrivMessage(channel, FormaT(plainmsgformat, args));
end;

procedure TMyIrcThread.IrcSendPrivMessage(channel, plainmsg: String);
begin
  irc_message_lock.Enter;
  try
    irc_last_read := Now();
    IrcWrite('PRIVMSG ' + channel + ' :' + irc_encrypt(netname, channel, plainmsg, True));
    console_addline(netname + ' ' + channel, Format('[%s] <%s> %s', [FormatDateTime('hh:nn:ss', Now), irc_nick, plainmsg]));
    irc_last_written := Now;
  finally
    irc_message_lock.Leave;
  end;

end;

function TMyIrcThread.IrcSendPrivMessage(oneliner: String): Boolean;
var
  channel, msg: String;
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

  if chankeys = nil then
    exit;

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
    if ((s.RCString('ircnet', '') = netname) and (not s.siteinvited) and (not s.PermDown) and (s.UseAutoInvite)) then
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
  s, osszes: String;
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
  s, osszes: String;
  elotte: TDateTime;
  userid, pass: String;
  i: Integer;
  perform: String;
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
  elsobnchost, aktbnchost: String;
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
        Begin
          Debug(dpError, section, netname + ': ' + status + 'time elapsed... now Reconnecting');
          BncCsere;
        End;
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
  irc_message_lock:= TCriticalSection.Create;
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
  irc_message_lock.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

procedure ircStop;
begin
  if myIrcThreads <> nil then
    myIrcThreads.Clear;
end;

function TMyIrcThread.GetIrcNick: String;
begin
  result := RCString('nick', mynickname);
end;

function TMyIrcThread.GetIrcANick: String;
begin
  result := RCString('anick', mynickname);
end;

function TMyIrcThread.GetIrcUsername: String;
begin
  result := RCString('username', mynickname);
end;

function TMyIrcThread.GetIrcIdent: String;
begin
  result := RCString('ident', mynickname);
end;

function TMyIrcThread.GetIrcPassword: String;
begin
  result := RCString('password', '');
end;

procedure TMyIrcThread.SetIrcNick(value: String);
begin
  WCString('nick', value);
end;

procedure TMyIrcThread.SetIrcANick(value: String);
begin
  WCString('anick', value);
end;

procedure TMyIrcThread.SetIrcUsername(value: String);
begin
  WCString('username', value);
end;

procedure TMyIrcThread.SetIrcIdent(value: String);
begin
  WCString('ident', value);
end;

procedure TMyIrcThread.SetIrcPassword(value: String);
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

function TMyIrcThread.ChanNicks(chan: String): String;
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

function TMyIrcThread.RCString(name: String; def: String): String;
begin
  Result := sitesdat.ReadString('ircnet-' + netname, name, def);
end;

function TMyIrcThread.RCBool(name: String; def: Boolean): Boolean;
begin
  Result := sitesdat.ReadBool('ircnet-' + netname, name, def);
end;

function TMyIrcThread.RCInt(name: String; def: Integer): integer;
begin
  Result := sitesdat.ReadInteger('ircnet-' + netname, name, def);
end;

procedure TMyIrcThread.WCString(name: String; val: String);
begin
  sitesdat.WriteString('ircnet-' + netname, name, val);
end;

procedure TMyIrcThread.WCBool(name: String; val: boolean);
begin
  sitesdat.WriteBool('ircnet-' + netname, name, val);
end;

(*
procedure TMyIrcThread.WCInt(name: String; val: integer);
begin
  sitesdat.WriteInteger('ircnet-' + netname, name, val);
end;
*)

end.

