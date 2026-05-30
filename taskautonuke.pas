unit taskautonuke;

interface

uses
  tasksunit;

type
  TAutoNukeTask = class(TTask)
  private
  public
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TAutoNukeTask }

function TAutoNukeTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TAutoNukeTask.Name: String;
begin
  Result := 'AUTONUKE';
end;

end.
