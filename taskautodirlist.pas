unit taskautodirlist;

interface

uses tasksunit;

type
  TAutoDirlistTask = class(TTask)
  private
    procedure ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
  public
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SyncObjs, Contnrs, configunit, sitesunit, taskraw, indexer, Math, pazo, taskrace, Classes,
  precatcher, kb, queueunit, StrUtils, dateutils, dirlist, SysUtils, irc, debugunit, RegExpr,
  mystrings, IdGlobal;

const
  rsections = 'autodirlist';

type
  TReqFillerThread = class(TThread)
  private
    p: TPazo;
    secdir: String;
    rlsname: String;
  public
    constructor Create(p: Tpazo; const secdir, rlsname: String);
    procedure Execute; override;
  end;

{ TAutoDirlistTask }

procedure TAutoDirlistTask.ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
var
  x: TStringList;
  i, db: Integer;
  sitename: String;
  p: TPazo;
  ps: TPazoSite;
  rc: TCRelease;
  rls: TRelease;
  s: TSiteSlot;
  ss: String;
  datum: String;
  maindir: String;
  releasenametofind: String;
  notdown: Boolean;
  prestatus: Boolean;
  site: TSite;
  pdt: TPazoDirlistTask;
begin
  //2009-05-11_
  releasenametofind := releasename;
  datum := Copy(releasenametofind, 1, 10);
  datum := ReplaceText(datum, '-', '');
  datum := ReplaceText(datum, '_', '');
  if StrToIntDef(datum, -1) <> -1 then
  begin
    releasenametofind := Copy(releasenametofind, 12, 1000);
  end;

  i := kb_list.IndexOf('REQUEST-' + site1 + '-' + releasenametofind);
  if i <> -1 then
  begin
    irc_Addadmin(Format('already sending request %s to %s', [releasenametofind, site1]));
    exit;
  end;

  s := slot;
  x := TStringList.Create;
  try
    x.Text := indexerQuery(releasenametofind);
    db := 0;
    i := 0;
    maindir := secdir + reqdir;

    while (i < x.Count) do
    begin
      ss := x.Names[i];
      sitename := Fetch(ss, '-', True, False);
      if sitename = site1 then
      begin
        // lolka fel van toltve helyben.  -- Lolka is charged on site.
        ss := x.Values[x.Names[i]];
        ss := ReplaceText(ss, '/', '_');

        if not s.Cwd(maindir, True) then
          Break;
        if not s.Send('MKD Already_on_site_in_' + ss) then
          Break;
        if not s.Read('MKD Already_on_site_in_' + ss) then
          break;

        db := 0;
        Break;
      end;

      site := FindSiteByName(netname, sitename);
      notdown := ((site <> nil) and (site.working <> sstDown));


      if ((notdown) and (
        (site.isRouteableTo(site1)) or not
        (config.ReadBool(rsections, 'only_use_routable_sites_on_reqfill', False))
        )) then
      begin
        inc(db);
        inc(i);
      end
      else
        x.Delete(i);
    end;

    if db > 0 then
    begin
      // jo, addolunk racetaskot dirlistekkel meg minden   ~~ jo, addolunk racetaskot dirlistekkel all
      ss := x.Names[0];
      Fetch(ss, '-', True, False);

      rc := FindSectionHandler(ss);
      rls := rc.Create(releasename, ss);
      p := PazoAdd(rls);
      kb_list.AddObject('REQUEST-' + site1 + '-' + releasenametofind, p);

      p.AddSite(site1, maindir);
      for i := 0 to x.Count - 1 do
      begin
        ss := x.Names[i];
        sitename := Fetch(ss, '-', True, False);
        ps := p.AddSite(sitename, x.Values[x.Names[i]]);
        ps.AddDestination(site1, sitesdat.ReadInteger('speed-from-' + sitename, site1, 0));
      end;

      for i := 0 to p.sites.Count - 1 do
      begin
        try
          // Treat source sites for filling as presites, destination site is a normal race destination
          if (i = 0) then
            prestatus := False
          else
            prestatus := True;
          pdt := TPazoDirlistTask.Create(netname, channel, TPazoSite(p.sites[i]).name, p, '', prestatus);
          AddTask(pdt);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, Format('[EXCEPTION] TAutoDirlistTask.ProcessRequest AddTask: %s', [e.Message]));
          end;
        end;
      end;

      TReqFillerThread.Create(p, secdir, releasename);
    end;
  finally
    x.Free;
  end;
end;

function TAutoDirlistTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  i, j: Integer;
  l: TAutoDirlistTask;
  asection, ss, section, sectiondir: String;
  dl: TDirList;
  de: TDirListEntry;
  reqrgx: TRegExpr;

  procedure UjraAddolas;
  begin
    // megnezzuk, kell e meg a taszk -- We look at whether it should be the task
    i := s.RCInteger('autodirlist', 0);
    if i > 0 then
    begin
      try
        l := TAutoDirlistTask.Create(netname, channel, site1);
        l.startat := IncSecond(Now, i);
        l.dontremove := True;
        AddTask(l);
        s.site.WCDateTime('nextautodirlist', l.startat);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoDirlistTask.Execute AddTask: %s', [e.Message]));
        end;
      end;
    end;
  end;

begin
  Result := False;
  s := slot;
  debugunit.Debug(dpMessage, rsections, Name);

  // megnezzuk, kell e meg a taszk -- We look at whether it should be the task
  if s.RCInteger('autodirlist', 0) = 0 then
  begin
    ready := True;
    Result := True;
    exit;
  end;

  if s.site.working = sstDown then
  begin
    ujraaddolas();
    readyerror := True;
    exit;
  end;

  if (s.status <> ssOnline) then
  begin
    if (not s.ReLogin(1)) then
    begin
      ujraaddolas();
      readyerror := True;
      exit;
    end;
  end;

  // implement the task itself
  ss := s.RCString('autodirlistsections', '');
  for i := 1 to 1000 do
  begin
    section := SubString(ss, ' ', i);
    if section = '' then
      break;
    sectiondir := s.site.sectiondir[section];
    if sectiondir <> '' then
    begin
      sectiondir := DatumIdentifierReplace(sectiondir);

      if not s.Dirlist(sectiondir, True) then // daydir might have change
      begin
        readyerror := True;
        irc_Addadmin(Format('%s daydir might have change', [s.site.Name]));
        exit;
      end;

      // dirlist successful, you must work with the elements
      dl := TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
      dl.dirlist_lock.Enter;
      try
        for j := 0 to dl.entries.Count - 1 do
        begin
          de := TDirlistEntry(dl.entries[j]);
          if ((de.Directory) and (0 = pos('nuked', de.filenamelc))) then
          begin
            if section = 'REQUEST' then
            begin
              reqrgx := TRegExpr.Create;
              try
                reqrgx.ModifierI := True;
                reqrgx.Expression := '^R[3E]Q(UEST)?-(by.[^\-]+\-)?(.*)$';
                if reqrgx.Exec(de.filename) then
                begin
                  ProcessRequest(slot, MyIncludeTrailingSlash(sectiondir), de.filename, reqrgx.match[3]);
                end;
              finally
                reqrgx.Free;
              end;
            end
            else
            begin
              if (SecondsBetween(Now(), de.timestamp) < config.readInteger(rsections, 'dropolder', 86400)) then
              begin
                try
                  asection := PrecatcherSectionMapping(de.filename, section);
                  kb_add(netname, channel, site1, asection, '', 'NEWDIR', de.filename, '', False, False, de.timestamp);
                except
                  on e: Exception do
                  begin
                    Debug(dpError, section, Format('Exception in TPazoImdbTask kb_add: %s', [e.Message]));
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        dl.dirlist_lock.Leave;
        dl.Free;
      end;
    end;
  end;

  ujraaddolas();

  Result := True;
  ready := True;
end;

function TAutoDirlistTask.Name: String;
var
  cstr: String;
begin
  if ScheduleText <> '' then
    cstr := format('(%s)', [ScheduleText])
  else
    cstr := '';
  Result := format('::AUTODIRLIST:: %s %s', [site1, cstr]);
end;

{ TReqFillerThread }

constructor TReqFillerThread.Create(p: Tpazo; const secdir, rlsname: String);
begin
  self.p := p;
  self.secdir := secdir;
  self.rlsname := rlsname;
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TReqFillerThread.Execute;
var
  rt: TRawTask;
  reqfill_delay: Integer;
begin
  irc_Addstats(Format('<c8>[REQUEST]</c> New request, %s on %s filling from %s, type %sstop %d', [p.rls.rlsname, TPazoSite(p.sites[0]).Name, TPazoSite(p.sites[1]).Name, irccmdprefix, p.pazo_id]));

  while (true) do
  begin
    if p.readyerror then
    begin
      irc_Addadmin('readyWithError %s', [p.errorreason]);
      Break;
    end;

    (*
      prefer completion folders over filecount comparison as that is more accurate
      if the target site does not have completion folders due to missing dirscript
      in the requests folder we fall back to comparing the filecount of all
      (sub-)dirs and if those are equal we set cache_completed on the dirlist to
      true to indicate the dirlist task can finish because the release is complete
    *)
    if ((TPazoSite(p.sites[0]).dirlist.complete_tag = '') and (TPazoSite(p.sites[1]).dirlist.done > 0) and (TPazoSite(p.sites[0]).dirlist.done = TPazoSite(p.sites[1]).dirlist.done)) then
      TPazoSite(p.sites[0]).dirlist.cache_completed := True;
    if ((p.ready) and (TPazoSite(p.sites[0]).dirlist.Complete)) then
    begin
      reqfill_delay := config.ReadInteger(rsections, 'reqfill_delay', 60);
      irc_Addadmin(Format('<c8>[REQUEST]</c> Request for %s on %s is ready! Reqfill command will be executed in %ds', [p.rls.rlsname, TPazoSite(p.sites[0]).Name, reqfill_delay]));
      rt := TRawTask.Create('', '', TPazoSite(p.sites[0]).Name, secdir, 'SITE REQFILLED ' + rlsname);
      rt.startat := IncSecond(now, reqfill_delay);
      try
        AddTask(rt);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TReqFillerThread.Execute AddTask: %s', [e.Message]));
        end;
      end;
      exit;
    end;

    sleep(1000);
  end;
end;

end.
