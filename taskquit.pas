unit taskquit;

interface

uses tasksunit;

type
  TQuitTask = class(TTask)
    constructor Create(const netname, channel, site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, DebugUnit, irc, mrdohutils;

const
  section = 'quit';

constructor TQuitTask.Create(const netname, channel, site: String);
begin
  inherited Create(netname, channel, site);
end;

function TQuitTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
begin
  Result := False;

  s := slot;
  s.Quit;

  ready := True;

  if spamcfg.readbool('sites', 'login_logout', False) then
    irc_SendRACESTATS(Name + Format(' (%s)', [s.Name]));

  Debug(dpSpam, section, Name);
end;

function TQuitTask.Name: String;
begin
  try
    Result := Format('QUIT <b>%s</b>',[site1]);
  except
    Result := 'QUIT';
  end;
end;

end.

