unit taskdirlist;

interface

uses
  tasksunit;

type
  TDirlistTask = class(TTask)
  private
    forcecwd: Boolean;
    dir: String;
  public
    constructor Create(const netname, channel, site, dir: String; forcecwd: Boolean = False);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TDirlistTask }

constructor TDirlistTask.Create(const netname, channel, site, dir: String; forcecwd: Boolean = False);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
  self.forcecwd := forcecwd;
end;

function TDirlistTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TDirlistTask.Name: String;
begin
  Result := 'DIRLIST';
end;

end.