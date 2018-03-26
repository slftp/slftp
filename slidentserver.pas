unit slidentserver;

interface

uses sltcp;

type

  TslIdentLookup = class(TslSCHThread)
    procedure RealExecute(); override;
  end;
  TslIdentServer = class(TslTCPServer)
  public
    function Lookup(ip: String; localport, remoteport: Integer): String; virtual; abstract;
    constructor Create;
  end;


implementation

uses SysUtils;

{ TslIdentServer }


(*
56646,13307

56646,13307 : USERID : UNIX : rsctm
*)

procedure TslIdentLookup.RealExecute;
var s, ossz, username: String;
    localport, remoteport: Integer;
    i: Integer;
begin
  if not client.ReadLn(s, ossz, 5000) then exit;

  i:= Pos(',', s);
  if i = 0 then exit;
  localport:= StrToIntDef(Copy(s, 1, i-1), -1);
  remoteport:= StrToIntDef(Copy(s, i+1, 8), -1);

  if ((localport < 0) or (remoteport < 0)) then exit;

  username:= TslIdentServer(ServerThread.Server).Lookup(client.slSocket.peerip, localport, remoteport);

  client.WriteLn(Format('%d,%d : USERID : UNIX : %s', [localport, remoteport, username]))

end;

constructor TslIdentServer.Create;
begin
  inherited Create(TslIdentLookup);
  BindAdd(113);
end;


end.
