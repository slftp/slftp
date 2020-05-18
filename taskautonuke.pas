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
  Classes, StrUtils, DateUtils, Contnrs, Types, configunit, mainthread, sitesunit, precatcher,
  kb, queueunit, dirlist, SysUtils, irc, debugunit, nuke, mystrings;

const
  rsections = 'autonuke';

{ TAutoNukeTask }

function TAutoNukeTask.Execute(slot: Pointer): Boolean;
label
  TryAgain;
var
  s: TSiteSlot;
  i, fInterval: Integer;
  sectiondir: String;
  l: TAutoNukeTask;
  n: TNukeQueueItem;
  b: Boolean;
  fNuketime: TDateTime;

  procedure RetryNextTime;
  begin
    // fInterval > 0: feature is enabled and new task will be executed in fInterval seconds
    if fInterval > 0 then
    begin
      try
        l := TAutoNukeTask.Create(netname, channel, site1);
        l.startat := IncSecond(Now, fInterval);
        l.dontremove := True;
        AddTask(l);
        s.site.NextAutoNukeDateTime := l.startat;
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
  Debug(dpSpam, rsections, '-->' + Name);

  fInterval := s.site.AutoNukeInterval;
  // fInterval = 0: feature disabled
  if fInterval = 0 then
  begin
    ready := True;
    Result := True;
    exit;
  end;

TryAgain:
  if not (s.site.WorkingStatus in [sstUnknown, sstUp, sstMarkedAsDownByUser]) then
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
  while ((i < NukeQueue.Count) and (not slshutdown) and (not s.shouldquit)) do
  begin
    n := TNukeQueueItem(NukeQueue[i]);
    if n.site = site1 then
    begin
      sectiondir := s.site.sectiondir[n.section];
      if sectiondir <> '' then
      begin
        fNuketime := UnixToDateTime(0);
        // identifier for week <ww> cannot be defined in TNukeQueueItem and is not used in nuke dates
        // but it could result in issues if sectiondir is confed with it, atm we won't care about it until someone reports
        if not TryEncodeDateTime(StrToInt(n.yyyy), StrToInt(n.mm), StrToInt(n.dd), 0, 0, 0, 0, fNuketime) then
        begin
          Debug(dpError, rsections, Format('TAutoNukeTask.Execute TryEncodeDateTime: Could not recode date: %s (%d) %s (%d) %s (%d)',
            [n.yyyy, StrToInt(n.yyyy), n.mm, StrToInt(n.mm), n.dd, StrToInt(n.dd)]));
          readyerror := true;
          Result := True;
        end;
        sectiondir := DatumIdentifierReplace(sectiondir, fNuketime);

        if s.Cwd(sectiondir, True) then
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

      NukeQueue.Remove(n);
      NukeSave;

      i := -1;
    end;
    inc(i);
  end;

  RetryNextTime();

  ready := True;
  Debug(dpSpam, rsections, '<--' + Name);
  Result := True;
end;

function TAutoNukeTask.Name: String;
begin
  Result := Format('AUTONUKE %s %s', [site1, ScheduleText]);
end;

end.
