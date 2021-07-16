unit taskautodirlist;

interface

uses tasksunit;

type
  TAutoDirlistTask = class(TTask)
  private
    FSpecificRlsName: String; //< if set only search for this rls in the request dir
    procedure ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
  public
    constructor Create(const netname, channel, site, aSpecificRlsName: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SyncObjs, Contnrs, configunit, sitesunit, taskraw, indexer, Math, pazo, taskrace, Classes,
  precatcher, kb, queueunit, StrUtils, dateutils, dirlist, SysUtils, irc, debugunit, RegExpr,
  kb.releaseinfo, mystrings, IdGlobal, tasksearchrelease, notify;

const
  rsections = 'autodirlist';

type
  TReqFillerThread = class(TThread)
  private
    p: TPazo; //< associated pazo element with the sites and releases
    secdir: String; //< directory of the request section on site
    rlsname: String; //< name of the release being filled in this request
  public
    constructor Create(p: Tpazo; const secdir, rlsname: String);
    procedure Execute; override;
  end;

{ TAutoDirlistTask }

constructor TAutoDirlistTask.Create(const netname, channel, site, aSpecificRlsName: String);
begin
  inherited Create(netname, channel, site);
  FSpecificRlsName := aSpecificRlsName;
end;

procedure TAutoDirlistTask.ProcessRequest(slot: Pointer; const secdir, reqdir, releasename: String);
var
  x: TStringList; //< result list for the indexer query
  fSiteSearchResponse: TStringList; //< holds paths from site search
  i, db: Integer;
  sitename: String;
  p: TPazo;
  ps: TPazoSite;
  rc: TCRelease;
  rls: TRelease;
  s: TSiteSlot;
  ss: String;
  datum: String; //< helper var to remove prefixed dates in yyyy-mm-dd_ shape (or similar)
  maindir: String; //< full path of reqdir on site
  releasenametofind: String; //< actual releasename, with possible date prefix removed
  prestatus: Boolean; //< @true if source site for the request, @false if destination site
  site: TSite;
  pdt: TPazoDirlistTask;
  fSiteSearchTask: TSearchReleaseTask; //< task for 'site search' to find a release
  fTaskNotify: TTaskNotify; //< wait for tasks
  fSiteResponse: TSiteResponse; //< site search response

  function IsSourceSiteValid(aSite: TSite): boolean;
  var
    notdown: Boolean; //< @true if site is not down, @false otherwise
  begin
    notdown := ((aSite <> nil) and (aSite.Name <> site1) and (aSite.WorkingStatus in [sstUnknown, sstUp]) and not aSite.PermDown);

    Result := ((notdown) and (aSite.Name <> getadminsitename) and (
      (aSite.isRouteableTo(site1)) or not
      (config.ReadBool(rsections, 'only_use_routable_sites_on_reqfill', False))
      ))
  end;

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
    exit;
  end;

  s := slot;
  x := TStringList.Create;
  try
    x.Text := indexerQuery(releasenametofind);
    db := 0;
    i := 0;
    maindir := secdir + reqdir;

    if x.Count = 0 then //not found in index, try site search
    begin
      //do a fake check when using site search to make sure we don't do shit like transfering whole sections
      try
        rls := TRelease.Create(releasenametofind, 'FAKECHECK', True, 0);
        if rls.fake
          //we don't care about these fake reasons. If the site search finds it, it's OK.
          and not LowerCase(rls.fakereason).Contains('many')
          and not LowerCase(rls.fakereason).Contains('banned')
          and not LowerCase(rls.fakereason).Contains('in a word')
          and not LowerCase(rls.fakereason).Contains('in word') then
        begin
          Debug(dpSpam, rsections, Format('[REQFILLER] Rls detected as fake, don''t fill: %s', [releasenametofind]));
          rls.Free;
          exit;
        end;
      except
      on e: Exception do
        begin
          if rls <> nil then
            FreeAndNil(rls);

          Debug(dpError, rsections, Format('[EXCEPTION] TAutoDirlistTask.ProcessRequest FakeCheck : %s', [e.Message]));
          exit;
        end;
      end;

      fTaskNotify := AddNotify;
      for site in sites do
      begin
        if site.UseSiteSearchOnReqFill and (IsSourceSiteValid(site)) then
        begin
          fSiteSearchTask := TSearchReleaseTask.Create('', '', site.Name, releasenametofind);
          fTaskNotify.tasks.Add(fSiteSearchTask);
          AddTask(fSiteSearchTask);
        end;
      end;

      fTaskNotify.event.WaitFor($FFFFFFFF);

      fSiteSearchResponse := TStringList.Create;
      try
        for fSiteResponse in fTaskNotify.responses do
        begin
          fSiteSearchResponse.Text := ParsePathFromSiteSearchResult(fSiteResponse.response, releasenametofind);

          //use just the first search result for now
          if fSiteSearchResponse.Count > 0 then
          begin
            ss := fSiteSearchResponse[0];
            //remove the release name from the end of the path, because we use the parent directory path as section
            SetLength(ss, LENGTH(ss) - (LENGTH(releasenametofind) + 1));
            x.Add(fSiteResponse.sitename + '-' + ss + '=' + ss);
          end;
        end;
      finally
        fSiteSearchResponse.Free;
        RemoveTN(fTaskNotify);
      end;
    end;

    while (i < x.Count) do
    begin
      ss := x.Names[i];
      sitename := Fetch(ss, '-', True, False);
      if sitename = site1 then
      begin
        // Requested release is already indexed on this site

        if config.ReadBool(rsections, 'create_already_on_site_in_directory', True) then
        begin
          ss := x.Values[x.Names[i]];
          ss := ReplaceText(ss, '/', '_');

          if s.Cwd(maindir, True) then
            if s.Send('MKD Already_on_site_in_' + ss) then
              s.Read('MKD Already_on_site_in_' + ss);
        end;

        if not config.ReadBool(rsections, 'fill_already_on_site', False) then
          Exit;
      end;

      site := FindSiteByName(netname, sitename);
      if IsSourceSiteValid(site) then
      begin
        inc(db);
        inc(i);
      end
      else
        x.Delete(i);
    end;

    if db > 0 then
    begin
      // Found at least one site that has the release, issue dirlists for each one and create pazo to send it to destination
      ss := x.Names[0];
      Fetch(ss, '-', True, False);

      rc := FindSectionHandler(ss);
      rls := rc.Create(releasenametofind, ss);
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

      for ps in p.PazoSitesList do
      begin
        try
          (*
            Treat source sites for filling as presites, destination site is a normal race destination
            this leads to only dirlisting the source sites once, as they're unlikely to change in content
            but we will dirlist the destination site unless the release was sent completely or any of the
            other conditions for ending a dirlist kick in
          *)
          if (i = 0) then
            prestatus := False
          else
            prestatus := True;
          pdt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', prestatus);
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

  procedure RescheduleTask;
  begin
    // Check autodirlist interval
    i := s.RCInteger('autodirlist', 0);
    if (i > 0) and (FSpecificRlsName = '') then //no reschedule if we're searching for a specific request because of a precatcher event
    begin
      try
        l := TAutoDirlistTask.Create(netname, channel, site1, FSpecificRlsName);
        l.startat := IncSecond(Now, i);
        l.dontremove := True;
        AddTask(l);
        s.site.WCDateTime('nextautodirlist', l.startat);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoDirlistTask.Execute RescheduleTask: %s', [e.Message]));
        end;
      end;
    end;
  end;

begin
  Result := False;
  s := slot;
  debugunit.Debug(dpMessage, rsections, Name);

  // Check autodirlist interval, whether autodirlist is still enabled
  if (FSpecificRlsName = '') and (s.RCInteger('autodirlist', 0) = 0) then
  begin
    ready := True;
    Result := True;
    exit;
  end;

  if not (s.site.WorkingStatus in [sstUnknown, sstUp, sstMarkedAsDownByUser]) then
  begin
    RescheduleTask();
    readyerror := True;
    exit;
  end;

  if (s.status <> ssOnline) then
  begin
    if (not s.ReLogin(1)) then
    begin
      RescheduleTask();
      readyerror := True;
      exit;
    end;
  end;

  //if we're searching for a request, only get the request section dir
  if FSpecificRlsName <> '' then
  begin
    ss := 'REQUEST';
    if s.site.sectiondir[ss] = '' then
    begin
      irc_Addstats(Format('<c5>[SECTION NOT SET]</c> : %s %s @ %s (%s)', ['REQUEST', FSpecificRlsName, s.site.Name, KBEventTypeToString(kbeRequest)]));
      readyerror := True;
      exit;
    end;
  end

  //normal autodirlist behaviour
  else
  begin
    ss := s.RCString('autodirlistsections', '');
  end;

  // implement the task itself
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
          if ((de.Directory) and (0 = pos('nuked', de.FilenameLowerCased))) then
          begin
            if section = 'REQUEST' then
            begin
              reqrgx := TRegExpr.Create;
              try
                reqrgx.ModifierI := True;
                reqrgx.Expression := '^R[3E]Q(UEST)?-(by.[^\-]+\-)?(.*)$';
                if reqrgx.Exec(de.filename) and ((FSpecificRlsName = '') or String(reqrgx.match[3]).Contains(FSpecificRlsName)) then
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
                  kb_add(netname, channel, site1, asection, '', kbeNEWDIR, de.filename, '', False, False, de.timestamp);
                except
                  on e: Exception do
                  begin
                    Debug(dpError, section, Format('Exception in TAutoDirlistTask kb_add: %s', [e.Message]));
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

  RescheduleTask();

  Result := True;
  ready := True;
end;

function TAutoDirlistTask.Name: String;
var
  cstr, fRlsName: String;
begin
  if ScheduleText <> '' then
    cstr := format('(%s)', [ScheduleText])
  else
    cstr := '';

  if FSpecificRlsName <> '' then
    fRlsName := format('(%s)', [FSpecificRlsName])
  else
    fRlsName := '';

  Result := format('::AUTODIRLIST:: %s %s %s', [site1, cstr, fRlsName]);
end;

{ TReqFillerThread }

constructor TReqFillerThread.Create(p: Tpazo; const secdir, rlsname: String);
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('ReqFiller', self.ThreadID);
  {$ENDIF}
  FreeOnTerminate := True;

  self.p := p;
  self.secdir := secdir;
  self.rlsname := rlsname;
end;

procedure TReqFillerThread.Execute;
var
  rt: TRawTask;
  reqfill_delay: Integer;
  fSourceSitesInfo: String;
  i: integer;
begin
  fSourceSitesInfo := '';
  for i := 1 to p.PazoSitesList.Count - 1 do
  begin
    if fSourceSitesInfo <> '' then
    begin
      fSourceSitesInfo := fSourceSitesInfo + ', ';
    end;
    fSourceSitesInfo := fSourceSitesInfo + TPazoSite(p.PazoSitesList[i]).Name
  end;

  irc_Addstats(Format('<c8>[REQUEST]</c> New request, %s on %s filling from %s, type %sstop %d', [p.rls.rlsname, TPazoSite(p.PazoSitesList[0]).Name, fSourceSitesInfo, irccmdprefix, p.pazo_id]));

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
      (sub-)dirs and if those are equal we set CachedCompleteResult on the dirlist to
      true to indicate the dirlist task can finish because the release is complete
    *)
    if config.ReadBool(rsections, 'compare_files_for_reqfilled_fallback', True)
      and ((TPazoSite(p.PazoSitesList[0]).dirlist.CompleteDirTag = '')
      and (TPazoSite(p.PazoSitesList[1]).dirlist.done > 0)
      and (TPazoSite(p.PazoSitesList[0]).dirlist.done = TPazoSite(p.PazoSitesList[1]).dirlist.done)) then
    begin

      //if there is only 1 file and that file is a NFO, the 'Complete' function will return true if the release type allows it (dirfix, nfofix, ...)
      //badly spread releases or incomplete archives containing only the NFO would lead to a false reqfilled cmd with only the NFO filled
      if not ((TPazoSite(p.PazoSitesList[0]).dirlist.done = 1) and TPazoSite(p.PazoSitesList[0]).dirlist.HasNFO) then
      begin
        TPazoSite(p.PazoSitesList[0]).dirlist.CachedCompleteResult := True;
      end;
    end;

    if ((p.ready) and (TPazoSite(p.PazoSitesList[0]).dirlist.Complete)) then
    begin
      reqfill_delay := config.ReadInteger(rsections, 'reqfill_delay', 60);
      irc_Addadmin(Format('<c8>[REQUEST]</c> Request for %s on %s is ready! Reqfill command will be executed in %ds', [p.rls.rlsname, TPazoSite(p.PazoSitesList[0]).Name, reqfill_delay]));
      rt := TRawTask.Create('', '', TPazoSite(p.PazoSitesList[0]).Name, secdir, 'SITE REQFILLED ' + rlsname);
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
