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

uses
  Classes, configunit, mainthread, sitesunit, precatcher, kb, queueunit, StrUtils,
  dateutils, dirlist, SysUtils, irc, debugunit, nuke;

const
  rsections = 'autonuke';

{ TAutoNukeTask }

function TAutoNukeTask.Execute(slot: Pointer): Boolean;
label
  TryAgain;
var
  s: TSiteSlot;
  i: Integer;
  ss: String;
  l: TAutoNukeTask;
  n: TNukeQueueItem;
  b: Boolean;

  procedure RetryNextTime;
  begin
    // If value of autonuke is bigger zero, feature is still enabled and slftp need
    // to add a new task which will be executed in i seconds
    i := s.RCInteger('autonuke', 0); // TODO: add property to TSite
    if i > 0 then
    begin
      try
        l := TAutoNukeTask.Create(netname, channel, site1);
        l.startat := IncSecond(Now, i);
        l.dontremove := True;
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
  Result := False;
  s := slot;
  debugunit.Debug(dpMessage, rsections, Name);

  // autonuke=0 means that the feature isn't in use anymore -> exit
  if s.RCInteger('autonuke', 0) = 0 then
  begin
    ready := True;
    Result := True;
    exit;
  end;

TryAgain:
  if s.site.working = sstDown then
  begin
    RetryNextTime();
    readyerror := True;
    exit;
  end;

  if (s.status <> ssOnline) then
  begin
    if (not s.ReLogin) then
    begin
      RetryNextTime();
      readyerror := True;
      exit;
    end;
  end;

  i := 0;
  while ((i < nukequeue.Count) and (not slshutdown) and (not s.shouldquit)) do
  begin
    n := TNukeQueueItem(nukequeue[i]);
    if n.site = site1 then
    begin
      ss := s.site.sectiondir[n.section];
      if ss <> '' then
      begin
        ss := ReplaceText(ss, '<yyyy>', n.yyyy);
        ss := ReplaceText(ss, '<yy>', n.yy);
        ss := ReplaceText(ss, '<mm>', n.mm);
        ss := ReplaceText(ss, '<dd>', n.dd);
        if s.Cwd(ss, True) then
        begin
          if n.multiplier >= 0 then
            b := s.Send('SITE NUKE %s %d %s', [n.rip, n.multiplier, n.reason])
          else
            b := s.Send('SITE UNNUKE %s %s', [n.rip, n.reason]);

          if not b then 
            goto TryAgain;

          if not s.Read('NUKE') then
            goto TryAgain;
        end
        else
        begin
          if s.Status <> ssOnline then
            goto TryAgain;
        end;
      end;

      nukequeue.Remove(n);
      NukeSave;

      i := -1;
    end;
    inc(i);
  end;

  RetryNextTime();

  Result := True;
  ready := True;
end;

function TAutoNukeTask.Name: String;
begin
  Result := 'AUTONUKE ' + site1 + ScheduleText;
end;

end.