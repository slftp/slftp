unit taskcwd;

interface

uses
  tasksunit;

type
  TCWDTask = class(TTask)
  private
    dir: String;
  public
    constructor Create(const netname, channel, site, dir: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SysUtils;

{ TCWDTask }

constructor TCWDTask.Create(const netname, channel, site, dir: String);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
end;

function TCWDTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TCWDTask.Name: String;
begin
  Result := 'CWD';
end;

end.