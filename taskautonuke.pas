unit taskautonuke;

interface

uses tasksunit;

type TAutoNukeTask = class(TTask)
     private
     public
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;

implementation

uses configunit, mainthread, sitesunit, precatcher, kb, queueunit, mystrings, dateutils, dirlist, SysUtils, irc, debugunit, nuke;

const rsections = 'autonuke';

{ TAutoSectionTask }



function TAutoNukeTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    i: Integer;
    ss: string;
    l: TAutoNukeTask;
    n: TNukeQueueItem;
    b: Boolean;

  procedure UjraAddolas;
  begin
    // megnezzuk, kell e meg a taszk    -- we have to look, do we need the task
    i:= s.RCInteger('autonuke', 0);
    if i > 0 then
    begin
      try
        l:= TAutoNukeTask.Create(netname, channel, site1);
        l.startat:= IncSecond(Now, i);
        l.dontremove:= True;
        AddTask(l);
        s.site.WCDateTime('nextautonuke', l.startat);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TAutoNukeTask.Execute AddTask: %s', [e.Message]));
        end;
      end;
    end;
  end;

begin
  Result:= False;
  s:= slot;
  debugunit.Debug(dpMessage, rsections, Name);

    // megnezzuk, kell e meg a taszk  -- we have to look, do we need the task
    if s.RCInteger('autonuke', 0) = 0 then
    begin
      ready:= True;
      Result:= True;
      exit;
    end;

ujra:
  if s.site.working = sstDown then
    begin
      ujraaddolas();
      readyerror:= True;
      exit;
    end;

  if (s.status <> ssOnline) then
    if (not s.ReLogin) then
    begin
      ujraaddolas();
      readyerror:= True;
      exit;
    end;


    i:= 0;
    while ((i < nukequeue.Count) and (not slshutdown) and (not s.shouldquit)) do
    begin
      n:= TNukeQueueItem(nukequeue[i]);
      if n.site = site1 then
      begin

        ss:= s.site.sectiondir[n.section];
        if ss <> '' then
        begin

          ss:= Csere(ss, '<yyyy>', n.yyyy);
          ss:= Csere(ss, '<yy>', n.yy);
          ss:= Csere(ss, '<mm>', n.mm);
          ss:= Csere(ss, '<dd>', n.dd);
          if s.Cwd(ss, True) then
          begin
            if n.multiplier >= 0 then
              b:= s.Send('SITE NUKE %s %d %s', [n.rip, n.multiplier, n.reason])
            else
              b:= s.Send('SITE UNNUKE %s %s', [n.rip, n.reason]);

            if not b then goto ujra;

            if not s.Read('NUKE') then
              goto ujra;
          end else
          begin
            if s.Status <> ssOnline then
              goto ujra;
          end;

        end;
        nukequeue.Remove(n);
        n.Free;
        NukeSave;
        i:= -1;
      end;
      inc(i);
    end;


    ujraaddolas();

  Result:= True;
  ready:= True;
end;

function TAutoNukeTask.Name: string;
begin
  Result:= 'AUTONUKE '+site1+ScheduleText;
end;

end.
