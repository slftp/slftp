unit ircblowfish.plaintext;

interface

uses
  ircchansettings, SysUtils;

type
  TIrcBlowkeyPlaintext = class(TIrcChannelSettings)
  private
    FBlowkey: String; //< blowkey for channel
    FBlowkeyLength: integer; //< blowkey length
  public
    { Creates a new IrcBlowkey entry }
    constructor Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
    { Sets @link(FBlowkey) and @link(FBlowkeyLength)
      @param(aBlowkey blowkey) }
    procedure UpdateKey(const aBlowkey: String); override;
    { DOES NOT ENCRYPT THE MESSAGE, JUST RETURNS THE INPUT MESSAGE
      @param(dText decrypted message)
      @returns(dText decrypted message) }
    function EncryptMessage(const dText: String): String; override;
    { DOES NOT DECRYPT THE MESSAGE, JUST RETURNS THE INPUT MESSAGE
      @param(eText encrypted message)
      @returns(eText encrypted message) }
    function DecryptMessage(const eText: String): String; override;

    property Blowkey: String read FBlowkey;
  end;

implementation

const
  section = 'ircblowfish.plaintext';

{ TIrcBlowkeyPlaintext }

constructor TIrcBlowkeyPlaintext.Create(const aNetname, aChannel, aChanRoles, aBlowkey: String; aChankey: String = ''; aInviteOnly: Boolean = False);
begin
  inherited Create(aNetname, aChannel, aChanRoles, aChankey, aInviteOnly);

  UpdateKey(aBlowkey);
end;

procedure TIrcBlowkeyPlaintext.UpdateKey(const aBlowkey: String);
begin
  FBlowkey := '';
  FBlowkeyLength := 0;
end;

function TIrcBlowkeyPlaintext.EncryptMessage(const dText: String): String;
begin
  Result := dText;
end;

function TIrcBlowkeyPlaintext.DecryptMessage(const eText: String): String;
begin
  Result := eText;
end;

end.
