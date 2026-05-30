unit taskautodirlist;

interface

uses
  tasksunit;

type
  TAutoDirlistTask = class(TTask)
  private
    FSpecificRlsName: String;
  public
    constructor Create(const netname, channel, site, aSpecificRlsName: String);
    procedure ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

procedure AutoDirlistInit;
procedure AutoDirlistUninit;
procedure SetRequestFilled(const aKbKey: string);
function IsRequestFilled(const aKbKey: string): boolean;

implementation

procedure AutoDirlistInit;
begin
end;

procedure AutoDirlistUninit;
begin
end;

procedure SetRequestFilled(const aKbKey: string);
begin
end;

function IsRequestFilled(const aKbKey: string): boolean;
begin
  Result := False;
end;

{ TAutoDirlistTask }

constructor TAutoDirlistTask.Create(const netname, channel, site, aSpecificRlsName: String);
begin
  inherited Create(netname, channel, site);
  self.FSpecificRlsName := aSpecificRlsName;
end;

procedure TAutoDirlistTask.ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
begin
end;

function TAutoDirlistTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TAutoDirlistTask.Name: String;
begin
  Result := 'AUTODIRLIST';
end;

end.
