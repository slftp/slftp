unit taskquit;

interface

uses tasksunit;

type TQuitTask = class(TTask)
       constructor Create(const netname, channel, site: string);
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;

implementation

uses sitesunit, SysUtils, DebugUnit, irc;

{ TLoginTask }

const section = 'quit';

constructor TQuitTask.Create(const netname, channel, site: string);
begin
  inherited Create(netname, channel, site);
end;

function TQuitTask.Execute(slot: Pointer): Boolean;
var s: TSiteSlot;
begin
  Result:= False;

  s:= slot;

  s.Quit;
  ready:= True;
  Debug(dpSpam, section, Name);
  irc_SendRACESTATS(Name + Format(' (%s)', [s.Name]));
end;

function TQuitTask.Name: string;
begin
  try
    Result:= Format('QUIT <b>%s</b>',[site1]);
  except
    Result:= 'QUIT';
  end;
end;

end.

