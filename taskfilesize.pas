unit taskfilesize;

interface

uses
  tasksunit;

type
  TFileSizeTask = class(TTask)
  private
    filename: String;
  public
    constructor Create(const netname, channel, site, filename: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

{ TFileSizeTask }

constructor TFileSizeTask.Create(const netname, channel, site, filename: String);
begin
  inherited Create(netname, channel, site);
  self.filename := filename;
end;

function TFileSizeTask.Execute(slot: Pointer): Boolean;
begin
  response := '-1'; // file not found
  ready := True;
  Result := True;
end;

function TFileSizeTask.Name: String;
begin
  Result := 'FILESIZE';
end;

end.