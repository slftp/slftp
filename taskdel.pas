unit taskdel;

interface

uses
  SyncObjs, SysUtils, tasksunit;

type
  TDelReleaseTask = class(TTask)
  private
    dir: String;
    devent: TEvent;
  public
    constructor Create(const netname, channel, site, dir: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TDelReleaseTask }

constructor TDelReleaseTask.Create(const netname, channel, site, dir: String);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
  devent := TEvent.Create(nil, true, false, 'DEL_' + site + '-' + dir);
end;

destructor TDelReleaseTask.Destroy;
begin
  devent.Free;
  inherited;
end;

function TDelReleaseTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TDelReleaseTask.Name: String;
begin
  Result := 'DELRELEASE';
end;

end.
