unit ident;

interface

uses SysUtils, slidentserver;

type
  TMyIdentServer = class(TslIdentServer)
  private
    identresponse: String;
    function FindIdent(PeerIP: String; PeerPort: Integer): String;
  public
    constructor Create;
    function Lookup(ip: String; localport, remoteport: Integer): String; override;
  end;

procedure IdentStart;
procedure IdentStop;

var myIdentserver: TMyIdentServer = nil;

implementation

uses configunit, sitesunit, queueunit, debugunit, IniFiles, mainthread,
  sltcp, console;
const section='ident';

procedure IdentStart;
begin
{$IFDEF MSWINDOWS}
  try
    if config.ReadBool(section, 'enabled', True) then
      myIdentserver:= TMyIdentServer.Create;
  except
    myIdentserver:= nil;
  end;
{$ENDIF}
end;
procedure IdentStop;
begin
  if myIdentserver <> nil then
  begin
    myIdentserver.Free;
    myIdentserver:= nil;
  end;
end;

{ TMyIdentServer }

constructor TMyIdentServer.Create;
begin
  inherited Create;

  identresponse:= config.ReadString(section, 'response', 'rsctm');
  backlog:= config.ReadInteger(section, 'listenqueue', 30);

  Start(False);
end;


function TMyIdentServer.FindIdent(PeerIP: String; PeerPort: Integer): String;
var i, j: Integer;
    s: TSite;
    ss: TSiteSlot;
begin
  Result:= identresponse;
  for i:= 0 to sites.Count -1 do
  begin
    s:= TSite(sites[i]);
    for j:= 0 to s.slots.Count-1 do
    begin
      ss:= TSiteSlot(s.slots[j]);
      if ((ss.peerport = peerport) and (ss.peerip = peerip)) then
      begin
        Result:= ss.RCString('ident', identresponse);
        exit;
      end;
    end;
  end;
end;

function TMyIdentServer.Lookup(ip: String; localport, remoteport: Integer): String;
begin
  Result:= FindIdent(ip, remoteport);
  //console_addline('', 'IDENT request from '+IP+':'+IntToStr(RemotePort)+' => '+Result);
end;

end.

