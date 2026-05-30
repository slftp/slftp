unit taskautoindex;

interface

uses
  tasksunit;

type
  TAutoIndexTask = class(TTask)
  private
    function DoIndexing(slot: Pointer; const sectionname, path: String; const aktszint: Integer): Integer;
  public
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TAutoIndexTask }

function TAutoIndexTask.DoIndexing(slot: Pointer; const sectionname, path: String; const aktszint: Integer): Integer;
begin
  Result := 0;
end;

function TAutoIndexTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TAutoIndexTask.Name: String;
begin
  Result := 'AUTOINDEX';
end;

end.