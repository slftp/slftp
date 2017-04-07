unit taskdirlist;
interface

uses tasksunit;

type TDirlistTask = class(TTask)
       forcecwd: Boolean;
       dir: AnsiString;
       constructor Create(const netname, channel: AnsiString;site: AnsiString; dir: AnsiString; forcecwd: Boolean = False);
       function Execute(slot: Pointer): Boolean; override;
       function Name: AnsiString; override;
     end;

implementation

uses sitesunit, SysUtils, mystrings, DebugUnit;

const section = 'dirlist';

{ TDirlistTask }

constructor TDirlistTask.Create(const netname, channel: AnsiString;site: AnsiString; dir: AnsiString; forcecwd: Boolean = False);
begin
  self.dir:= dir;
  self.forcecwd:= forcecwd;
  inherited Create(netname, channel, site);
end;

function TDirlistTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    numerrors: Integer;
begin
  Result:= False;
  s:= slot;
  Debug(dpMessage, section, Name);
  numerrors:=0;
  
ujra:
  inc(numerrors);
  if numerrors > 3 then
  begin
    Debug(dpError, section, Format('ERROR: numerrors > 3 for %s @ %s', [dir, s.Name]));
    readyerror:= True;
    exit;
  end;

  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror:= True;
      exit;
    end;


  if (not s.Dirlist(dir, forcecwd)) then
  begin
    if s.status <> ssOnline then
      goto ujra;

    // but if it is not broke do not exist in the dir ...
    Debug(dpError, section, Format('ERROR: can not dirlist %s - %s', [dir, BoolToStr(forcecwd)]));
    readyerror:= True;
    exit;
  end;
  response:= s.lastResponse;

  Result:= True;
  ready:= True;
end;

function TDirlistTask.Name: AnsiString;
begin
  try
    Result:= Format('<b>DIRLIST:</b> %s @ %s',[dir,site1]);
  except
    Result:= 'DIRLIST';
  end;
end;

end.

