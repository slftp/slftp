unit ircchansettings;

interface

uses
  Generics.Collections;

type
  { @abstract(General settings for an IRC Channel) }
  TIrcChannelSettings = class
  private
    FNetname: String; //< netname of IRC network
    FChannel: String; //< IRC channelname
    FInviteOnly: Boolean; //< @true if channel is invite only (you have to invite yourself first), @false otherwise
    FChankey: String; //< chankey for @link(Channel) which is needed to join it
    FChanRoles: TList<string>; //< chanroles for @link(Channel), saved under 'names' entry in sites.dat
    function GetChanRoles: String; //< returns the strings in @link(FChanRoles), delimited by a space
    procedure SetChanRoles(const aChanRoles: String); //< adds the @link(aChanRoles) String (multiple roles should be delimited by a space) to @link(FChanRoles) if not added yet, every call to it clears the list as first step
  public
    { Creates a new TIrcChannelSettings entry which holds infos about Chankey, Chanroles and if channel is invite only
      @param(aNetname irc network name)
      @param(aChannel irc channel name)
      @param(aChanRoles irc chanroles for this channel)
      @param(aChankey channel key to join channel)
      @param(aInviteOnly @true if channel can only joined with previous invite, @false otherwise) }
    constructor Create(const aNetname, aChannel, aChanRoles: String; aChankey: String = ''; aInviteOnly: Boolean = True);
    { Sets/Updates blowkey
      @param(aBlowkey blowkey) }
    procedure UpdateKey(const aBlowkey: String); virtual; abstract;
    { Checks if @link(Channel) has a chanrole for @link(aCheckChanRole)
      @param(aCheckChanRole Chanrole which should be checked if its supported by chan@network)
      @returns(@true if chanrole is set, @false otherwise) }
    function HasThisChanRole(const aCheckChanRole: String): Boolean;
    { Encrypts decrypted input text
      @param(dText encrypted message)
      @returns(encrypted text) }
    function EncryptMessage(const dText: String): String; virtual; abstract;
    { Decrypts encrypted input text
      @param(eText encrypted message)
      @returns(decrypted text) }
    function DecryptMessage(const eText: String): String; virtual; abstract;

    property Netname: String read FNetname;
    property Channel: String read FChannel;
    property InviteOnly: Boolean read FInviteOnly;
    property ChanKey: String read FChankey write FChankey;
    property ChanRoles: String read GetChanRoles write SetChanRoles;
  end;

{ Just a helper function to initialize @link(TIrcChanSettingsList) }
procedure IrcChannelSettingsInit;
{ Just a helper function to free @link(TIrcChanSettingsList) }
procedure IrcChannelSettingsUninit;
{ Find object with IRC Channel Stuff for Netname-Channel combination, does not suppress logging of 'No IrcChannelInfos found'
  @param(aNetname irc network name)
  @param(aChannel irc channel name)
  @returns(Corresponding object for Channel@Netname if existing, else @nil) }
function FindIrcChannelSettings(const aNetname, aChannel: String): TIrcChannelSettings; overload;
{ Find object with IRC Channel Stuff for Netname-Channel combination
  @param(aNetname irc network name)
  @param(aChannel irc channel name)
  @param(aSuppressDebugEntry Set it to @true if you want to suppress 'No IrcChannelInfos found' logging, otherwise use @false)
  @returns(Corresponding object for Channel@Netname if existing, else @nil) }
function FindIrcChannelSettings(const aNetname, aChannel: String; aSuppressDebugEntry: Boolean): TIrcChannelSettings; overload;
{ Creates a new object for IRC Channel Stuff for Netname-Channel combination if not existing
  @param(aNetname irc network name)
  @param(aChannel irc channel name)
  @param(aChanRoles irc channel roles)
  @param(aBlowkey irc fishkey)
  @param(aChankey irc channel key)
  @param(aInviteOnly @true if channel can only be joined with previous invite, @false otherwise)
  @param(aIsCBCEncrypted @true if chan uses CBC encryption, @false otherwise) }
procedure RegisterChannelSettings(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False; aIsCBCEncrypted: Boolean = False);

var
  { Case sensitive hashed objectlist with 'netnamechannel' as key and corresponding object }
  IrcChanSettingsList: TObjectDictionary<string, TIrcChannelSettings>;

implementation

uses
  SysUtils, StrUtils, console, debugunit, ircblowfish.ECB, ircblowfish.CBC, ircblowfish.plaintext;

const
  section = 'ircchansettings';

{ TIrcChannelSettings }

constructor TIrcChannelSettings.Create(const aNetname, aChannel, aChanRoles: String; aChankey: String = ''; aInviteOnly: Boolean = True);
begin
  FNetname := aNetname;
  FChannel := aChannel;

  FChanRoles := TList<string>.Create;
  SetChanRoles(aChanRoles);

  FChankey := aChankey;
  FInviteOnly := aInviteOnly;
end;

function TIrcChannelSettings.GetChanRoles: String;
var
  fStrHelper: String;
begin
  Result := '';
  for fStrHelper in FChanRoles do
  begin
    Result := Result + fStrHelper + ' ';
  end;
  Trim(Result);
end;

procedure TIrcChannelSettings.SetChanRoles(const aChanRoles: String);
var
  fStrHelper: String;
begin
  FChanRoles.Clear;

  for fStrHelper in aChanRoles.Split([' ']) do
  begin
    FChanRoles.Add(fStrHelper);
  end;

  FChanRoles.Sort;
end;

function TIrcChannelSettings.HasThisChanRole(const aCheckChanRole: String): Boolean;
begin
  Result := FChanRoles.Contains(aCheckChanRole);
end;

procedure IrcChannelSettingsInit;
begin
  IrcChanSettingsList := TObjectDictionary<string, TIrcChannelSettings>.Create([doOwnsValues]);
end;

procedure IrcChannelSettingsUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  IrcChanSettingsList.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

function FindIrcChannelSettings(const aNetname, aChannel: String): TIrcChannelSettings;
begin
  Result := FindIrcChannelSettings(aNetname, aChannel, False);
end;

function FindIrcChannelSettings(const aNetname, aChannel: String; aSuppressDebugEntry: Boolean): TIrcChannelSettings;
var
  fChanSettingsObj: TIrcChannelSettings;
  fItem: TPair<string, TIrcChannelSettings>;
  fNetChanHelper: String;
begin
  Result := nil;

  if IrcChanSettingsList.TryGetValue(aNetname + aChannel, fChanSettingsObj) then
  begin
    // fast case sensitive approach via hash code
    Result := fChanSettingsObj;
    exit;
  end
  else
  begin
    // slower fallback case insensitive approach
    fNetChanHelper := LowerCase(aNetname + aChannel);
    for fItem in IrcChanSettingsList do
    begin
      if (fNetChanHelper = LowerCase(fItem.Key)) then
      begin
        Result := fItem.Value;
        exit;
      end;
    end;
  end;

  if not aSuppressDebugEntry then
  begin
    Debug(dpSpam, section, Format('No IrcChannelInfos found for chan %s on net %s - check if its correctly cased/spelled', [aChannel, aNetname]));
  end;
end;

procedure RegisterChannelSettings(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False; aIsCBCEncrypted: Boolean = False);
var
  fChanSettingsObj: TIrcChannelSettings;
begin
  fChanSettingsObj := FindIrcChannelSettings(aNetname, aChannel, True);
  if fChanSettingsObj = nil then
  begin
    console_add_ircwindow(aNetname + ' ' + aChannel);

    if aBlowkey = '' then
    begin
      fChanSettingsObj := TIrcBlowkeyPlaintext.Create(aNetname, aChannel, aChanRoles, aBlowkey, aChankey, aInviteOnly);
    end
    else
    begin
      if aIsCBCEncrypted then
        fChanSettingsObj := TIrcBlowkeyCBC.Create(aNetname, aChannel, aChanRoles, aBlowkey, aChankey, aInviteOnly)
      else
        fChanSettingsObj := TIrcBlowkeyECB.Create(aNetname, aChannel, aChanRoles, aBlowkey, aChankey, aInviteOnly);
    end;

    try
      IrcChanSettingsList.Add(aNetname + aChannel, fChanSettingsObj);
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] Cannot add %s on net %s - duplicate!', [aChannel, aNetname]));
      end;
    end;
  end;
end;

end.
