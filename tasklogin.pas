unit tasklogin;

interface

uses tasksunit;

type
  TLoginTask = class(TTask)
  public
    noannounce: Boolean;
    readd: Boolean; //< @true if called from autobnctest, @false otherwise
    kill: Boolean;
    constructor Create(const netname, channel, site: String; kill: Boolean; readd: Boolean);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TLoginTask }

constructor TLoginTask.Create(const netname, channel, site: String; kill: Boolean; readd: Boolean);
begin
  inherited Create(netname, channel, site);
  self.kill := kill;
  self.readd := readd;
end;

function TLoginTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TLoginTask.Name: String;
begin
  Result := 'LOGIN';
end;

end.
