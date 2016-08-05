unit tasklogin;

interface

uses tasksunit;

type TLoginTask = class(TTask)
     private
       kill: Boolean;
     public
       noannounce: Boolean;
       readd: Boolean; // autobnctest-hez...
       constructor Create(const netname, channel: AnsiString; site: AnsiString; kill: Boolean; readd: Boolean);
       function Execute(slot: Pointer): Boolean; override;
       function Name: AnsiString; override;
     end;

implementation

uses sitesunit, queueunit, dateutils, SysUtils, irc, debugunit;

const section = 'login';

{ TLoginTask }

constructor TLoginTask.Create(const netname, channel: AnsiString; site: AnsiString; kill: Boolean; readd: Boolean);
begin
  self.kill:= kill;
  self.readd:= readd;
  inherited Create(netname, channel, site);
end;

function TLoginTask.Execute(slot: Pointer): Boolean;
label vege;
var s: TSiteSlot;
    i: Integer;
    l: TLoginTask;

begin
  Result:= False;
  s:= slot;
  debugunit.Debug(dpSpam, section, '-->'+Name);

  if readd then
  begin
    // megnezzuk, kell e meg a taszk
    if s.RCInteger('autobnctest', 0) = 0 then
    begin
      ready:= True;
      Result:= True;
      exit;
    end;
  end;

  if ((s.site.working = sstUp) and (readd)) then
  begin
    // nem teszteljuk ujra, csak orulunk neki
    goto vege;
  end;


  try
    if ((not readd) or (not s.site.markeddown)) then
    begin
      s.Quit;
      Result:= s.ReLogin(1, kill, section);
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TLoginTask.s.ReLogin: %s', [e.Message]));
    end;
  end;

  if s.Status = ssOnline then
    announce:= Format('<b>%s</b>: %s',[s.site.name,s.bnc]);

vege:
  if readd then
  begin
    // megnezzuk, kell e meg a taszk
    i:= s.RCInteger('autobnctest', 0);
    if i > 0 then
    begin
      try
        l:= TLoginTask.Create(netname, channel, site1, kill, readd);
        l.startat:= IncSecond(Now, i);
        l.dontremove:= True;
        AddTask(l);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TLoginTask.Execute.AddTask: %s', [e.Message]));
        end;
      end;
    end;
  end;
  debugunit.Debug(dpSpam, section, '<--'+Name);
  ready:= True;
end;

function TLoginTask.Name: AnsiString;
begin
  Result:= '';
  try
    if readd then Result:= 'AUTO';
    Result:= Result + 'LOGIN '+site1+' '+ScheduleText;
  except
    Result:= 'LOGIN';
  end;
end;

end.
