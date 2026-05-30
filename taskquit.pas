unit taskquit;

interface

uses
  tasksunit;

type
  TQuitTask = class(TTask)
    constructor Create(const netname, channel, site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TQuitTask }

constructor TQuitTask.Create(const netname, channel, site: String);
begin
  inherited Create(netname, channel, site);
end;

function TQuitTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TQuitTask.Name: String;
begin
  Result := 'QUIT';
end;

end.