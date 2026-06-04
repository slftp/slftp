unit identserver;

interface

uses
  IdGlobal, IdContext, IdIdentServer;

(*
  A request:  56646,13307
  B answer:  56646,13307 : USERID : UNIX : slftpuser
*)

type
  { @abstract(Ident server for replying to ident requests) }
  TIdentServer = class(TIdIdentServer)
  private
    FDefaultIdentResponse: String; //< default ident response from inifile

    { Tries to find the site's ident by iterating all siteslots to find the appropriate connection by IP and Port
      @param(aPeerIP IP address from server who requests ident)
      @param(aPeerPort Port from server who requests ident)
      @returns(Sites ident string if configured, otherwise default ident from config) }
    function FindSiteIdent(const aPeerIP: String; const aPeerPort: Integer): String;
    { Event which is triggered when an ident was requested, sends the ident answer
      @param(AContext Connection information)
      @param(AServerPort Server port)
      @param(AClientPort Client port) }
    procedure SendReply(AContext: TIdContext; AServerPort, AClientPort: TIdPort);
  public
    { Creates an ident server on port 113 which replies to ident requests }
    constructor Create;

    property DefaultIdentResponse: String read FDefaultIdentResponse;
  end;

{ Just a helper function to initialize @link(IdentServer) if enabled in config
  @returns(Exception string on failure, otherwise empty string) }
function IdentServerInit: String;
{ Just a helper function to free @link(IdentServer) }
procedure IdentServerStop;

implementation

uses
  SysUtils, configunit, sitesunit, debugunit;

const
  section = 'ident';

var
  glMyIdentServer: TIdentServer = nil;

{ TIdentServer }

constructor TIdentServer.Create;
begin
  inherited;

  FDefaultIdentResponse := config.ReadString(section, 'response', 'slftpuser');
  OnIdentQuery := SendReply;
  Active := True;
end;

function TIdentServer.FindSiteIdent(const aPeerIP: String; const aPeerPort: Integer): String;
var
  i, j: Integer;
  s: TSite;
  ss: TSiteSlot;
begin
  Result := FDefaultIdentResponse;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    for j := 0 to s.slots.Count - 1 do
    begin
      ss := TSiteSlot(s.slots[j]);
      if ((ss.peerport = aPeerPort) and (ss.peerip = aPeerIP)) then
      begin
        Result := ss.GetIdentReply;
        exit;
      end;
    end;
  end;
end;

procedure TIdentServer.SendReply(AContext: TIdContext; AServerPort, AClientPort: TIdPort);
var
  fServerIP: String;
  fIdentReply: String;
begin
  fServerIP := AContext.Connection.Socket.Binding.PeerIP;
  Debug(dpSpam, section, Format('IDENT request from %s:%d for port %d', [fServerIP, AServerPort, AClientPort]));
  fIdentReply := FindSiteIdent(fServerIP, AServerPort);
  Debug(dpSpam, section, Format('IDENT reply is %s', [fIdentReply]));
  ReplyIdent(AContext, AServerPort, AClientPort, 'UNIX', fIdentReply, {Charset}'');
end;

function IdentServerInit: String;
begin
  Result := '';
  try
    if config.ReadBool(section, 'enabled', False) then
    begin
      glMyIdentServer := TIdentServer.Create;
      Debug(dpMessage, section, 'Ident server successfully started');
    end;
  except
    on e: Exception do
    begin
      Result := e.Message;
      glMyIdentServer := nil;
    end;
  end;
end;

procedure IdentServerStop;
begin
  if Assigned(glMyIdentServer) then
  begin
    FreeAndNil(glMyIdentServer);
  end;
end;

end.

