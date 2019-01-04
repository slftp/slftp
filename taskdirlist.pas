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

uses
  sitesunit, SysUtils, mystrings, DebugUnit;

const
  section = 'taskdirlist';

{ TDirlistTask }

constructor TDirlistTask.Create(const netname, channel, site, dir: String; forcecwd: Boolean = False);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
  self.forcecwd := forcecwd;
end;

function TDirlistTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  numerrors: Integer;
begin
  Result := False;
  s := slot;
  Debug(dpMessage, section, Name);
  numerrors := 0;

ujra:
  inc(numerrors);
  if numerrors > 3 then
  begin
    Debug(dpError, section, Format('ERROR: numerrors > 3 for %s @ %s', [dir, s.Name]));
    readyerror := True;
    exit;
  end;

  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;


  if (not s.Dirlist(dir, forcecwd)) then
  begin
    if s.status <> ssOnline then
      goto ujra;

    // could not list directory
    Debug(dpError, section, Format('ERROR: can not dirlist dir %s on %s with forcecwd value %s', [dir, site1, BoolToStr(forcecwd, True)]));
    readyerror := True;
    exit;
  end;
  response := s.lastResponse;

  Result := True;
  ready := True;
end;

function TDirlistTask.Name: String;
begin
  try
    Result := Format('<b>DIRLIST:</b> %s @ %s', [dir, site1]);
  except
    Result := 'DIRLIST';
  end;
end;

end.