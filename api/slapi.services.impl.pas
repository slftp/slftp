unit slapi.services.impl;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  DateUtils,
  Variants,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.soa.core,
  mormot.soa.server,
  slapi.types,
	  slapi.services,
	  slapi.issues,
	  sitesunit,
	  queueunit,
	  tasksunit,
  tasklogin,
  statsunit,
  ranksunit,
  rulesunit,
  speedstatsunit,
  irc,
  ircchansettings,
  kb,
  pazo,
  precatcher,
  mainthread,
	  debugunit,
	  configunit,
	  routeconfig,
	  delphimd5,
	  RegExpr,
	  slcriticalsection2;

type
  { System Service Implementation }
  TApiSystemServiceImpl = class(TInjectableObjectRest, IApiSystemService)
  public
    function GetStatus(out Response: TApiSystemStatus): boolean;
    function GetUptime: Int64;
    function GetVersion: RawUTF8;
    function Shutdown: boolean;
    function CreateBackup: boolean;
    function GetRecentReleases(const Limit: integer; out Response: TApiReleasesList): boolean;
    function GetReleaseDetails(const PazoId: integer; out Response: TApiReleaseInfo): boolean;
    function GetAutoStatus: boolean;
    function SetAutoStatus(Enabled: boolean): boolean;
  end;

  { Sites Service Implementation }
  TApiSitesServiceImpl = class(TInjectableObjectRest, IApiSitesService)
  public
    function GetSites(const Filter: RawUTF8; out Sites: TApiSitesList): boolean;
    function GetSite(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;
    function AddSite(const Name, Host: RawUTF8; Port: integer;
                     const Username, Password: RawUTF8;
                     SslEnabled: boolean): boolean;
    function DeleteSite(const SiteName: RawUTF8): boolean;
    function SetSiteStatus(const SiteName: RawUTF8; const Status: RawUTF8): boolean;
    function SetSiteSlots(const SiteName: RawUTF8; Slots: integer): boolean;
    function SetSiteMaxUpDn(const SiteName: RawUTF8; MaxUp, MaxDn: integer): boolean;
    function SetSiteMaxPreDn(const SiteName: RawUTF8; MaxPreDn: integer): boolean;
    function SetSitePermDown(const SiteName: RawUTF8; PermDown: boolean): boolean;
    function SetSiteAutoLogin(const SiteName: RawUTF8; Enabled: boolean): boolean;
    function SetSiteAutoRules(const SiteName: RawUTF8; IntervalSeconds: integer): boolean;
    function SetSiteAffils(const SiteName, Affils: RawUTF8): boolean;
    function SetSiteIrcNick(const SiteName, IrcNick: RawUTF8): boolean;
    function RunSiteAutoRules(const SiteName: RawUTF8): boolean;
    function GetSiteRoutes(const SiteName: RawUTF8; out Routes: TApiSiteRoutes): boolean;
    function SetSiteRoute(const SourceSite, DestSite: RawUTF8; Speed: integer;
                          Locked, AffilOnly, NoAffil: boolean): boolean;
    function TestSite(const SiteName: RawUTF8): boolean;
    function ResolveHostname(const Hostname: RawUTF8): RawUTF8;
    function GhostSite(const SiteName: RawUTF8): boolean;
    function RecalcFreeSlots(const SiteName: RawUTF8): boolean;
    function RebuildSlots(const SiteName: RawUTF8): boolean;
    function ExecuteIrcCommand(const Command: RawUTF8): boolean;
    function GetSiteInfo(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;
    function SetSiteCredentials(const SiteName: RawUTF8;
                                const Username, Password: RawUTF8;
                                const BncsJson: RawUTF8;
                                MaxIdle, IdleInterval: integer;
                                LegacyCwd: boolean;
                                SslFxp: integer): boolean;
    function SetSiteConfig(const SiteName: RawUTF8; const Config: RawJSON): boolean;
    function GetAvailableSections: RawJSON;
    function GetSiteSections(const SiteName: RawUTF8): RawJSON;
    function SetSiteSection(const SiteName, Section, Dir: RawUTF8): boolean;
    function GetSiteRtpl(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;
    function GetSiteRulesSnapshot(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;
    function ValidateRtpl(const Content: RawUTF8; out Validation: TApiRulesValidation): boolean;
    function SaveSiteRtpl(const SiteName: RawUTF8; const Content: RawUTF8; const ExpectedMd5: RawUTF8;
      Reload: boolean; out SaveResult: TApiRulesSaveResult): boolean;
    function ReloadRules: boolean;
    function GetRuleConditions: RawJSON;
  end;

  { Queue Service Implementation }
  TApiQueueServiceImpl = class(TInjectableObjectRest, IApiQueueService)
  public
    function GetQueueStats(out Stats: TApiQueueStats): boolean;
    function GetQueue(const SiteName: RawUTF8): RawJSON;
    function GetTask(TaskUid: Int64; out Info: TApiTaskInfo): boolean;
    function CreateDirlistTask(const SiteName, Section, Dir: RawUTF8): Int64;
    function CreateSpreadTask(const SourceSite, Section, Release: RawUTF8): Int64;
    function CreateTransferTask(const SourceSite, DestSite, Section,
                                Dir, FileName: RawUTF8): Int64;
    function StopTask(TaskUid: Int64): boolean;
    function EmptyQueue(const SiteName: RawUTF8): boolean;
  end;

  { Stats Service Implementation }
  TApiStatsServiceImpl = class(TInjectableObjectRest, IApiStatsService)
  public
    function GetRaceStats(const SiteName, Period: RawUTF8;
                          Detailed: boolean): RawJSON;
    function GetRanks(const SiteName: RawUTF8): RawJSON;
    function SetRank(const SiteName, Section: RawUTF8; Score: integer): boolean;
    function RecalculateRanks: boolean;
  end;

  { IRC Service Implementation }
  TApiIrcServiceImpl = class(TInjectableObjectRest, IApiIrcService)
  public
    function GetNetworks: RawJSON;
    function GetNetworkStatus(const NetName: RawUTF8;
                              out Info: TApiIrcNetwork): boolean;
    function GetChannels(const NetName: RawUTF8): RawJSON;
    function SendMessage(const NetName, Channel, Message: RawUTF8): boolean;
    function JumpServer(const NetName: RawUTF8): boolean;
    function SetChannelBlowkey(const NetName, Channel, Blowkey: RawUTF8): boolean;
    function SetChannelKey(const NetName, Channel, ChanKey: RawUTF8): boolean;
    function SetChannelRoles(const NetName, Channel, Roles: RawUTF8): boolean;
    function AddChannel(const NetName, Channel, ChanKey, Blowkey, Roles: RawUTF8): boolean;
    function DeleteChannel(const NetName, Channel: RawUTF8): boolean;
    function AddNetwork(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;
    function DeleteNetwork(const NetName: RawUTF8): boolean;
  end;

  { Rules Service Implementation }
  TApiRulesServiceImpl = class(TInjectableObjectRest, IApiRulesService)
  public
    function GetRules(const SiteName, Section: RawUTF8): RawJSON;
    function GetRule(RuleId: integer): RawJSON;
    function AddRule(const RuleData: RawJSON): integer;
    function ModifyRule(RuleId: integer; const RuleData: RawJSON): boolean;
    function DeleteRule(RuleId: integer): boolean;
    function TestRule(const RuleData, ReleaseName: RawUTF8): boolean;
    function ReloadRules: boolean;
  end;

  { Speed Service Implementation }
  TApiSpeedServiceImpl = class(TInjectableObjectRest, IApiSpeedService)
  public
    function GetRoutes(const SiteName: RawUTF8): RawJSON;
    function TestSpeedLocal(const SiteName: RawUTF8): boolean;
    function TestSpeedOut(const SourceSite: RawUTF8;
                          const DestSites: RawUTF8): boolean;
    function TestSpeedIn(const DestSite: RawUTF8;
                         const SourceSites: RawUTF8): boolean;
    function GetSpeedResults(const SiteName: RawUTF8): RawJSON;
    function RecalculateRoutes: boolean;
  end;

  { KB Service Implementation }
  TApiKnowledgeBaseServiceImpl = class(TInjectableObjectRest, IApiKnowledgeBaseService)
  public
    function GetKBEntries(const Section: RawUTF8; Limit: integer): RawJSON;
    function SearchKB(const Query: RawUTF8): RawJSON;
    function AddKBEntry(const Section, Release: RawUTF8): boolean;
  end;

	  { Precatcher Service Implementation }
	  TApiPrecatcherServiceImpl = class(TInjectableObjectRest, IApiPrecatcherService)
	  public
	    function GetPrecatcherRules: RawJSON;
	    function AddPrecatcherRule(const RuleData: RawJSON): integer;
	    function DeletePrecatcherRule(RuleId: integer): boolean;
	    function TestPrecatcher(const Announce: RawUTF8): RawJSON;
	    function ReloadPrecatcher: boolean;
	    function GetMappings: RawJSON;
	  end;

	  { Issues Service Implementation }
	  TApiIssuesServiceImpl = class(TInjectableObjectRest, IApiIssuesService)
	  public
	    function GetSummary(const WindowSeconds: integer; out Response: TApiIssuesSummary): boolean;
	    function GetIssues(const Limit: integer; const SinceUnix: Int64; const TypesCsv: RawUTF8; out Response: TApiIssuesList): boolean;
	    function ClearIssues: boolean;
	  end;

	  { Log Service Implementation }
	  TApiLogServiceImpl = class(TInjectableObjectRest, IApiLogService)
	  public
	    function GetLogs(const Lines: integer): RawJSON;
	    function ClearLogs: boolean;
	  end;

implementation

uses
  Contnrs,
  kb.releaseinfo,
  mystrings,
  IdStack,
  irccommands.irc;

{$I ../slftp.inc}

const
  section = 'slapi.services';

{ TApiLogServiceImpl }

function TApiLogServiceImpl.GetLogs(const Lines: integer): RawJSON;
var
  logContent: string;
  linesToRead: integer;
  jsonArr: TDocVariantData;
  sl: TStringList;
  i: integer;
begin
  Result := '[]';
  linesToRead := Lines;
  if linesToRead <= 0 then linesToRead := 100;
  if linesToRead > 50000 then linesToRead := 50000;

  try
    logContent := debugunit.LogTail(linesToRead);

    sl := TStringList.Create;
    try
      sl.Text := logContent;
      jsonArr.InitFast(dvArray);
      for i := 0 to sl.Count - 1 do
      begin
        if Trim(sl[i]) <> '' then
          jsonArr.AddItem(UTF8Encode(sl[i]));
      end;
      Result := jsonArr.ToJSON;
    finally
      sl.Free;
    end;
  except
    on E: Exception do
      Debug(dpError, section, Format('[EXCEPTION] GetLogs: %s', [E.Message]));
  end;
end;

function TApiLogServiceImpl.ClearLogs: boolean;
begin
  Result := False; // Not implemented safely yet
end;

{ TApiIssuesServiceImpl }

function TApiIssuesServiceImpl.GetSummary(const WindowSeconds: integer; out Response: TApiIssuesSummary): boolean;
var
  total, skip, dontMatch, missingSection, nuke: integer;
  window: integer;
begin
  Result := False;
  Response := TApiIssuesSummary.Create;
  try
    window := WindowSeconds;
    if window <= 0 then
      window := 24 * 3600;

    IssuesStore.GetCounts(window, total, skip, dontMatch, missingSection, nuke);
    Response.WindowSeconds := window;
    Response.Total := total;
    Response.Skip := skip;
    Response.DontMatch := dontMatch;
    Response.MissingSection := missingSection;
    Response.Nuke := nuke;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSummary: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIssuesServiceImpl.GetIssues(const Limit: integer; const SinceUnix: Int64; const TypesCsv: RawUTF8;
  out Response: TApiIssuesList): boolean;
var
  events: TIssueEvents;
  issuesArray: TDocVariantData;
  issueJson: variant;
  i: integer;
begin
  Result := False;
  Response := TApiIssuesList.Create;
  try
    events := IssuesStore.GetSnapshot(Limit, SinceUnix, UTF8ToString(TypesCsv));
    issuesArray.Init(JSON_FAST, dvArray);

    for i := 0 to High(events) do
    begin
      TDocVariant.New(issueJson);
      issueJson.Id := events[i].Id;
      issueJson.TsUnix := events[i].TsUnix;
      issueJson.IssueType := UTF8Encode(events[i].IssueType);
      issueJson.Section := UTF8Encode(events[i].Section);
      issueJson.ReleaseName := UTF8Encode(events[i].ReleaseName);
      issueJson.SiteName := UTF8Encode(events[i].SiteName);
      issueJson.Reason := UTF8Encode(events[i].Reason);
      issueJson.KbEvent := UTF8Encode(events[i].KbEvent);
      issuesArray.AddItem(issueJson);
    end;

    Response.Total := Length(events);
    Response.Issues := TDocVariantData(issuesArray).ToJSON;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetIssues: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIssuesServiceImpl.ClearIssues: boolean;
begin
  IssuesStore.Clear;
  Result := True;
end;

{ TApiSystemServiceImpl }

function TApiSystemServiceImpl.GetStatus(out Response: TApiSystemStatus): boolean;
var
  i: integer;
  s: TSite;
  upCount, downCount: integer;
  qTotal, qRace, qDir, qAuto, qOther: integer;
  activeSum: integer;
begin
  Result := False;
  try
    Response := TApiSystemStatus.Create;
    if SL_REV <> '' then
      Response.Version := UTF8Encode(Format('%s (git# %s)', [SL_VERSION, SL_REV]))
    else
      Response.Version := UTF8Encode(SL_VERSION);

    Response.Uptime := SecondsBetween(Now, mainthread_started);

    upCount := 0;
    downCount := 0;
    activeSum := 0;

    if sitesunit.sites <> nil then
    begin
      Response.SitesCount := sitesunit.sites.Count;

      for i := 0 to sitesunit.sites.Count - 1 do
      begin
        s := TSite(sitesunit.sites[i]);
        if s = nil then
          Continue;
        if s.WorkingStatus = sstUp then
          Inc(upCount)
        else if s.PermDown then
          Inc(downCount)
        else if (s.WorkingStatus = sstDown) or (s.WorkingStatus = sstMarkedAsDownByUser) then
          Inc(downCount);

        // Sum current active transfers (download+upload)
        activeSum := activeSum + s.num_dn + s.num_up;
      end;
    end
    else
      Response.SitesCount := 0;

    Response.SitesUp := upCount;
    Response.SitesDown := downCount;

    // Gather queue stats snapshot
    QueueStatAll;
    GetQueueTotals(qTotal, qRace, qDir, qAuto, qOther);
    Response.QueueSize := qTotal;
    // Treat all queued tasks as active for dashboard purposes; transfers also counted via activeSum
    Response.ActiveTasks := qTotal;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetStatus: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSystemServiceImpl.GetUptime: Int64;
begin
  Result := SecondsBetween(Now, mainthread_started);
end;

function TApiSystemServiceImpl.GetVersion: RawUTF8;
begin
  Result := UTF8Encode(config.ReadString('irc', 'version', 'unknown'));
end;

function TApiSystemServiceImpl.Shutdown: boolean;
begin
  Result := False;
  try
    Debug(dpMessage, section, 'Shutdown requested via API');
    slshutdown := True;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] Shutdown: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSystemServiceImpl.CreateBackup: boolean;
begin
  Result := False;
  try
    Debug(dpMessage, section, 'Backup requested via API');
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateBackup: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSystemServiceImpl.GetRecentReleases(const Limit: integer; out Response: TApiReleasesList): boolean;
var
  i, count, maxCount: integer;
  p: TPazo;
  ps: TPazoSite;
  releaseJson: variant;
  releasesArray: TDocVariantData;
  sitesArray: TDocVariantData;
  kbList: TStringList;
  kbLock: TSlCriticalSection2;
begin
  Result := False;
  Response := TApiReleasesList.Create;

  try
    maxCount := Limit;
    if maxCount <= 0 then
      maxCount := 20; // Default limit
    if maxCount > 100 then
      maxCount := 100; // Max limit

    count := 0;
    releasesArray.Init(JSON_FAST, dvArray);

    kbList := GetKBList;
    kbLock := GetKBLock;

    kbLock.Enter('GetRecentReleases');
    try
      // Iterate through kb_list backwards (newest first)
      for i := kbList.Count - 1 downto 0 do
      begin
        if count >= maxCount then
          Break;

        p := TPazo(kbList.Objects[i]);
        if p = nil then
          Continue;

        // Build release info
        TDocVariant.New(releaseJson);
        releaseJson.ReleaseName := UTF8Encode(p.rls.rlsname);
        releaseJson.Section := UTF8Encode(p.rls.section);
        releaseJson.Added := DateTimeToUnix(p.added);
        releaseJson.PazoId := p.pazo_id;

        // Calculate status based on sites
        if p.stopped then
        begin
          releaseJson.Ready := False;
          releaseJson.Stopped := True;
        end
        else if p.ready then
        begin
          releaseJson.Ready := True;
          releaseJson.Stopped := False;
        end
        else
        begin
          // Check if all sites are complete
          releaseJson.Ready := True; // Assume complete until proven otherwise
          for ps in p.PazoSitesList do
          begin
            if not (ps.status in [rssNotAllowed, rssComplete, rssRealPre]) then
            begin
              releaseJson.Ready := False; // At least one site is not complete
              Break;
            end;
          end;
          releaseJson.Stopped := False;
        end;

        releaseJson.QueueNumber := p.queuenumber.Value;

        // Collect site names
        sitesArray.Init(JSON_FAST, dvArray);
        for ps in p.PazoSitesList do
        begin
          if ps.Name <> '' then
          begin
            sitesArray.AddItem(UTF8Encode(ps.Name));
          end;
        end;
        releaseJson.Sites := variant(sitesArray);

        releasesArray.AddItem(releaseJson);
        Inc(count);
      end;

      Response.Total := count;
      // Convert the releasesArray to JSON and store it
      Response.Releases := TDocVariantData(releasesArray).ToJSON;
      Result := True;

    finally
      kbLock.Leave;
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetRecentReleases: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSystemServiceImpl.GetReleaseDetails(const PazoId: integer; out Response: TApiReleaseInfo): boolean;
var
  p: TPazo;
  ps: TPazoSite;
  siteDetailsArray: TDocVariantData;
  siteDetail: variant;
  totalFiles: integer;
begin
  Result := False;
  Response := TApiReleaseInfo.Create;

  try
    p := FindPazoById(PazoId);
    if p = nil then
    begin
      Debug(dpError, section, Format('Pazo with ID %d not found', [PazoId]));
      Exit;
    end;

    // Basic info
    Response.ReleaseName := UTF8Encode(p.rls.rlsname);
    Response.Section := UTF8Encode(p.rls.section);
    Response.Added := p.added;
    Response.PazoId := p.pazo_id;
    Response.Ready := p.ready;
    Response.Stopped := p.stopped;
    Response.QueueNumber := p.queuenumber.Value;
    Response.ErrorReason := UTF8Encode(p.errorreason);
    Response.TotalFiles := p.GetCountOfCachedFiles;

    // Collect site details
    siteDetailsArray.Init(JSON_FAST, dvArray);

    for ps in p.PazoSitesList do
    begin
      TDocVariant.New(siteDetail);
      siteDetail.SiteName := UTF8Encode(ps.Name);
      siteDetail.Complete := ps.dirlist.Complete;
      siteDetail.FileCount := ps.dirlist.entries.Count;
      siteDetail.TotalFiles := p.GetCountOfCachedFiles;
      siteDetail.FilesRacedByMe := ps.dirlist.FilesRacedByMe(True);

      // Calculate percent
      totalFiles := p.GetCountOfCachedFiles;
      if totalFiles > 0 then
        siteDetail.Percent := (ps.dirlist.entries.Count / totalFiles) * 100.0
      else
        siteDetail.Percent := 0.0;

      // Status text
      case ps.status of
        rssNotAllowed: siteDetail.Status := 'Not Allowed';
        rssNotAllowedButItsThere: siteDetail.Status := 'Not Allowed (Present)';
        rssAllowed: siteDetail.Status := 'Allowed';
        rssShouldPre: siteDetail.Status := 'Should Pre';
        rssRealPre: siteDetail.Status := 'Pre';
        rssComplete: siteDetail.Status := 'Complete';
        rssNuked: siteDetail.Status := 'Nuked';
      else
        siteDetail.Status := 'Unknown';
      end;

      siteDetailsArray.AddItem(siteDetail);
    end;

    Response.SiteDetails := TDocVariantData(siteDetailsArray).ToJSON;
    Result := True;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetReleaseDetails: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSystemServiceImpl.GetAutoStatus: boolean;
begin
  Result := precatcher.precatcherauto;
end;

function TApiSystemServiceImpl.SetAutoStatus(Enabled: boolean): boolean;
begin
  sitesdat.WriteBool('precatcher', 'auto', Enabled);
  Result := True;
end;

{ TApiSitesServiceImpl }

function TApiSitesServiceImpl.GetSites(const Filter: RawUTF8; out Sites: TApiSitesList): boolean;
type
  TSiteSnapshot = record
    Name: string;
    Status: TSiteStatus;
    Slots: integer; // max_dn
    FreeSlots: integer;
    MaxUp: integer;
    MaxDn: integer;
    MaxPreDn: integer;
    NumDn: integer;
    NumUp: integer;
    PermDown: boolean;
    AutoLogin: boolean;
    AutoRulesInterval: integer;
    IrcNick: string;
  end;
var
  i: integer;
  s: TSite;
  sitesArray: TDocVariantData;
  siteDoc: variant;
  upCount, downCount: integer;
  filterStr: string;
  snapshots: array of TSiteSnapshot;
  snapshot: TSiteSnapshot;
  snapshotCount: integer;
begin
  Result := False;
  Sites := TApiSitesList.Create;
  sitesArray.InitFast(dvArray);

  filterStr := UTF8ToString(Filter);
  upCount := 0;
  downCount := 0;
  snapshotCount := 0;

  try
    if sitesunit.sites = nil then
    begin
      Sites.Total := 0;
      Sites.Up := 0;
      Sites.Down := 0;
      Sites.Sites := '[]';
      Result := True;
      Exit;
    end;

    if sitesunit.sites.Count = 0 then
    begin
      Sites.Total := 0;
      Sites.Up := 0;
      Sites.Down := 0;
      Sites.Sites := '[]';
      Result := True;
      Exit;
    end;

    // Quickly snapshot all site data in one pass
    SetLength(snapshots, sitesunit.sites.Count);
    try
      for i := 0 to sitesunit.sites.Count - 1 do
      begin
        s := TSite(sitesunit.sites[i]);
        if s = nil then
          Continue;

        // Copy data immediately while we have the reference.
        // Never skip a site just because a single field fails to read.
        snapshots[snapshotCount].Name := '';
        try
          snapshots[snapshotCount].Name := s.Name;
        except
          // ignore
        end;
        if snapshots[snapshotCount].Name = '' then
          Continue;

        snapshots[snapshotCount].Status := sstUnknown;
        try
          snapshots[snapshotCount].Status := s.WorkingStatus;
        except
          // ignore
        end;
        try
          if s.PermDown then
            snapshots[snapshotCount].Status := sstDown; // treat permdown as down in snapshots
        except
          // ignore
        end;

        snapshots[snapshotCount].Slots := 0;
        try
          if s.slots <> nil then
            snapshots[snapshotCount].Slots := s.slots.Count;
        except
          // ignore
        end;

        snapshots[snapshotCount].FreeSlots := 0;
        try
          snapshots[snapshotCount].FreeSlots := s.freeslots;
        except
          // ignore
        end;

        snapshots[snapshotCount].MaxUp := 0;
        try
          snapshots[snapshotCount].MaxUp := s.RCInteger('max_up', s.max_up);
        except
          // ignore
        end;

        snapshots[snapshotCount].MaxPreDn := 0;
        try
          snapshots[snapshotCount].MaxPreDn := s.RCInteger('max_pre_dn', s.max_pre_dn);
        except
          // ignore
        end;

        snapshots[snapshotCount].MaxDn := snapshots[snapshotCount].Slots;
        try
          snapshots[snapshotCount].MaxDn := s.RCInteger('max_dn', snapshots[snapshotCount].Slots);
        except
          // ignore
        end;

        snapshots[snapshotCount].NumDn := 0;
        try
          snapshots[snapshotCount].NumDn := s.num_dn;
        except
          // ignore
        end;

        snapshots[snapshotCount].NumUp := 0;
        try
          snapshots[snapshotCount].NumUp := s.num_up;
        except
          // ignore
        end;

        snapshots[snapshotCount].PermDown := False;
        try
          snapshots[snapshotCount].PermDown := s.PermDown;
        except
          // ignore
        end;

        snapshots[snapshotCount].AutoLogin := False;
        try
          snapshots[snapshotCount].AutoLogin := s.RCBool('autologin', False);
        except
          // ignore
        end;

        snapshots[snapshotCount].AutoRulesInterval := 0;
        try
          snapshots[snapshotCount].AutoRulesInterval := s.AutoRulesStatus;
        except
          // ignore
        end;

        snapshots[snapshotCount].IrcNick := '';
        try
          snapshots[snapshotCount].IrcNick := s.ircnick;
        except
          // ignore
        end;

        Inc(snapshotCount);
      end;
    except
      on E: Exception do
        Debug(dpError, section, Format('[EXCEPTION] GetSites snapshotting: %s', [E.Message]));
    end;

    // Now process the snapshots (no more TSite access)
    for i := 0 to snapshotCount - 1 do
    begin
      snapshot := snapshots[i];

      if (filterStr <> '') and (filterStr <> '*') then
      begin
        if UpperCase(snapshot.Name) <> UpperCase(filterStr) then
          Continue;
      end;

      try
        TDocVariant.New(siteDoc);
      except
        Continue;
      end;

      try
        TDocVariantData(siteDoc).AddValue('name', snapshot.Name);
      except
        Continue;
      end;

      try
        case snapshot.Status of
          sstUp: TDocVariantData(siteDoc).AddValue('status', 'UP');
          sstDown: TDocVariantData(siteDoc).AddValue('status', 'DOWN');
          sstMarkedAsDownByUser: TDocVariantData(siteDoc).AddValue('status', 'DOWN_BY_USER');
          sstUnknown: TDocVariantData(siteDoc).AddValue('status', 'UNKNOWN');
          else TDocVariantData(siteDoc).AddValue('status', 'UNKNOWN');
        end;
      except
        Continue;
      end;

      try
        TDocVariantData(siteDoc).AddValue('slots', snapshot.Slots);
      except
        Continue;
      end;

      try
        TDocVariantData(siteDoc).AddValue('freeslots', snapshot.FreeSlots);
      except
        Continue;
      end;

      // Extended slot info (optional consumers)
      try
        TDocVariantData(siteDoc).AddValue('max_dn', snapshot.MaxDn);
        TDocVariantData(siteDoc).AddValue('max_up', snapshot.MaxUp);
        TDocVariantData(siteDoc).AddValue('max_pre_dn', snapshot.MaxPreDn);
        TDocVariantData(siteDoc).AddValue('num_dn', snapshot.NumDn);
        TDocVariantData(siteDoc).AddValue('num_up', snapshot.NumUp);
        TDocVariantData(siteDoc).AddValue('permdown', snapshot.PermDown);
        TDocVariantData(siteDoc).AddValue('autologin', snapshot.AutoLogin);
        TDocVariantData(siteDoc).AddValue('autorules_interval', snapshot.AutoRulesInterval);
        TDocVariantData(siteDoc).AddValue('ircnick', UTF8Encode(snapshot.IrcNick));
      except
        // ignore add failures
      end;

      try
        sitesArray.AddItem(siteDoc);
      except
        Continue;
      end;

      if snapshot.Status = sstUp then
        Inc(upCount)
      else if (snapshot.Status = sstDown) or (snapshot.Status = sstMarkedAsDownByUser) then
        Inc(downCount);
    end;

    Sites.Total := sitesArray.Count;
    Sites.Up := upCount;
    Sites.Down := downCount;
    Sites.Sites := sitesArray.ToJSON;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSites: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetSite(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;
var
  s: TSite;
begin
  Result := False;
  Info := TApiSiteInfo.Create;

  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
    begin
      Debug(dpError, section, Format('Site not found: %s', [UTF8ToString(SiteName)]));
      Exit;
    end;

    Info.Name := UTF8Encode(s.Name);
    Info.Username := UTF8Encode(s.RCString('username', ''));

    case s.WorkingStatus of
      sstUp: Info.Status := 'UP';
      sstDown: Info.Status := 'DOWN';
      sstMarkedAsDownByUser: Info.Status := 'DOWN_BY_USER';
      sstUnknown: Info.Status := 'UNKNOWN';
      else Info.Status := 'UNKNOWN';
    end;

    Info.Slots := s.slots.Count;
    Info.FreeSlots := s.freeslots;
    Info.SslEnabled := (s.RCInteger('sslfxp', 0) > 0);

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.AddSite(const Name, Host: RawUTF8; Port: integer;
                                      const Username, Password: RawUTF8;
                                      SslEnabled: boolean): boolean;
var
  s: TSite;
  siteNameStr: string;
begin
  Result := False;
  try
    siteNameStr := UTF8ToString(Name);
    Debug(dpMessage, section, Format('AddSite API: %s@%s:%d', [siteNameStr, UTF8ToString(Host), Port]));

    if FindSiteByName('', siteNameStr) <> nil then
    begin
      Debug(dpError, section, Format('AddSite: Site %s already exists', [siteNameStr]));
      Exit;
    end;

    s := TSite.Create(siteNameStr);
    s.WCString('username', UTF8ToString(Username));
    s.WCString('password', UTF8ToString(Password));
    s.WCString('bnc_host-0', UTF8ToString(Host));
    s.WCInteger('bnc_port-0', Port);

    if SslEnabled then
      s.sslmethod := sslAuthTLS
    else
      s.sslmethod := sslNone;

    sitesunit.AddSite(s);

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] AddSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.DeleteSite(const SiteName: RawUTF8): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    Debug(dpMessage, section, Format('DeleteSite API: %s', [UTF8ToString(SiteName)]));

    s.Stop;
    sitesunit.DeleteSite(s);

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] DeleteSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteStatus(const SiteName: RawUTF8; const Status: RawUTF8): boolean;
var
  s: TSite;
  statusStr: string;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    statusStr := UpperCase(UTF8ToString(Status));

    if statusStr = 'UP' then
      s.WorkingStatus := sstUp
    else if statusStr = 'DOWN' then
      s.WorkingStatus := sstMarkedAsDownByUser;

    Debug(dpMessage, section, Format('SetSiteStatus API: %s -> %s', [UTF8ToString(SiteName), statusStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteStatus: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteSlots(const SiteName: RawUTF8; Slots: integer): boolean;
var
  s: TSite;
  current, i: integer;
  newSlot: TSiteSlot;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if Slots < 0 then
      Exit;

    current := s.slots.Count;

    if Slots > current then
    begin
      for i := 1 to Slots - current do
      begin
        newSlot := TSiteSlot.Create(s, s.slots.Count);
        s.slots.Add(newSlot);
      end;
    end
    else if Slots < current then
    begin
      for i := current - 1 downto Slots do
      begin
        TSiteSlot(s.slots[i]).Stop;
        TSiteSlot(s.slots[i]).Free;
        s.slots.Delete(i);
      end;
    end;

    s.WCInteger('slots', Slots);
    s.RecalcFreeslots;

    Debug(dpMessage, section, Format('SetSiteSlots API: %s -> %d', [UTF8ToString(SiteName), Slots]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteSlots: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteMaxUpDn(const SiteName: RawUTF8; MaxUp, MaxDn: integer): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if MaxUp < 0 then
      MaxUp := 0;
    if MaxDn < 0 then
      MaxDn := 0;

    // persist to config first to keep cache in sync
    s.WCInteger('max_up', MaxUp);
    s.WCInteger('max_dn', MaxDn);
    s.WCInteger('max_pre_dn', MaxDn);

    s.max_up := MaxUp;
    s.max_dn := MaxDn;
    // keep pre queue limit aligned with max downloads by default
    s.max_pre_dn := MaxDn;

    Debug(dpMessage, section, Format('SetSiteMaxUpDn API: %s -> up:%d dn:%d', [UTF8ToString(SiteName), MaxUp, MaxDn]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteMaxUpDn: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteMaxPreDn(const SiteName: RawUTF8; MaxPreDn: integer): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if MaxPreDn < 0 then
      MaxPreDn := 0;

    s.max_pre_dn := MaxPreDn;
    Debug(dpMessage, section, Format('SetSiteMaxPreDn API: %s -> %d', [UTF8ToString(SiteName), MaxPreDn]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteMaxPreDn: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSitePermDown(const SiteName: RawUTF8; PermDown: boolean): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    s.PermDown := PermDown;
    Debug(dpMessage, section, Format('SetSitePermDown API: %s -> %s', [UTF8ToString(SiteName), BoolToStr(PermDown, True)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSitePermDown: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteAutoLogin(const SiteName: RawUTF8; Enabled: boolean): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    s.WCInteger('autologin', Ord(Enabled));
    Debug(dpMessage, section, Format('SetSiteAutoLogin API: %s -> %s', [UTF8ToString(SiteName), BoolToStr(Enabled, True)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteAutoLogin: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteAutoRules(const SiteName: RawUTF8; IntervalSeconds: integer): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if IntervalSeconds < 0 then
      IntervalSeconds := 0;

    s.AutoRulesStatus := IntervalSeconds;
    Debug(dpMessage, section, Format('SetSiteAutoRules API: %s -> %d sec', [UTF8ToString(SiteName), IntervalSeconds]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteAutoRules: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteAffils(const SiteName, Affils: RawUTF8): boolean;
var
  s: TSite;
  affilsStr: string;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    affilsStr := UTF8ToString(Affils);
    if Pos(',', affilsStr) <> 0 then
      Exit;

    s.siteaffils := Trim(affilsStr);
    Debug(dpMessage, section, Format('SetSiteAffils API: %s -> %s', [UTF8ToString(SiteName), s.siteaffils]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteAffils: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteIrcNick(const SiteName, IrcNick: RawUTF8): boolean;
var
  s: TSite;
  ircnickStr: string;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    ircnickStr := UTF8ToString(IrcNick);
    s.ircnick := Trim(ircnickStr);
    Debug(dpMessage, section, Format('SetSiteIrcNick API: %s -> %s', [UTF8ToString(SiteName), s.ircnick]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteIrcNick: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.RunSiteAutoRules(const SiteName: RawUTF8): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    s.AutoRules;
    Debug(dpMessage, section, Format('RunSiteAutoRules API: %s -> started', [UTF8ToString(SiteName)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RunSiteAutoRules: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteRoutes(const SiteName: RawUTF8; out Routes: TApiSiteRoutes): boolean;
var
  s: TSite;
  routeList: TList<TSpeedFromRouteInfo>;
  i: integer;
  routeVar: TDocVariantData;
  routesArr: TDocVariantData;
begin
  Result := False;
  Routes := nil;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    routesArr.InitFast(dvArray);
    routeList := s.Speed_From;
    try
      for i := 0 to routeList.Count - 1 do
      begin
        routeVar.InitFast;
        TDocVariantData(routeVar).AddValue('dest', routeList[i].Sitename);
        TDocVariantData(routeVar).AddValue('speed', routeList[i].Speed);
        TDocVariantData(routeVar).AddValue('affil_only', routeList[i].AffilOnly);
        TDocVariantData(routeVar).AddValue('no_affil', routeList[i].NoAffil);
        TDocVariantData(routeVar).AddValue('locked', routeList[i].Locked);
        routesArr.AddItem(routeVar);
      end;
    finally
      routeList.Free;
    end;

    Routes := TApiSiteRoutes.Create;
    Routes.Routes := routesArr.ToJSON;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteRoutes: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteRoute(const SourceSite, DestSite: RawUTF8;
  Speed: integer; Locked, AffilOnly, NoAffil: boolean): boolean;
var
  srcSite, dstSite: TSite;
  fSpeedInfo: TSpeedFromRouteInfo;
  srcName, dstName, adminSite: String;
begin
  Result := False;
  try
    srcName := UTF8ToString(SourceSite);
    dstName := UTF8ToString(DestSite);
    adminSite := getAdminSiteName;

    if (srcName = adminSite) or (dstName = adminSite) then
      Exit;

    if (Speed > 9) or (Speed < 0) then
      Exit;

    srcSite := FindSiteByName('', srcName);
    if srcSite = nil then
      Exit;

    dstSite := FindSiteByName('', dstName);
    if dstSite = nil then
      Exit;

    if srcName = dstName then
      Exit;

    if AffilOnly and NoAffil then
      Exit;

    if Speed > 0 then
    begin
      fSpeedInfo.Speed := Speed;
      fSpeedInfo.Locked := Locked;
      fSpeedInfo.AffilOnly := AffilOnly;
      fSpeedInfo.NoAffil := NoAffil;
      sitesdat.WriteString('speed-from-' + srcName, dstName, fSpeedInfo.ToConfigString);
    end
    else
    begin
      sitesdat.DeleteKey('speed-from-' + srcName, dstName);
    end;

    srcSite.UpdateSpeedFromCache;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteRoute: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.TestSite(const SiteName: RawUTF8): boolean;
var
  s: TSite;
  t: TLoginTask;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if s.PermDown then
    begin
      Debug(dpMessage, section, Format('TestSite API: %s skipped (PermDown)', [UTF8ToString(SiteName)]));
      Exit;
    end;

    // Reset status if marked down by user (same as !bnctest)
    if (s.WorkingStatus = sstMarkedAsDownByUser) then
      s.WorkingStatus := sstUnknown;

    t := TLoginTask.Create('API', '', s.Name, False, False);
    t.noannounce := True; // keep IRC quiet
    t.startat := GiveSiteLastStart; // Fair scheduling
    AddTask(t);
    s.QueueFire; // Trigger queue processing

    Debug(dpMessage, section, Format('TestSite API: %s -> login task queued', [UTF8ToString(SiteName)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.ResolveHostname(const Hostname: RawUTF8): RawUTF8;
begin
  Result := '';
  try
    TIdStack.IncUsage;
    try
      Result := UTF8Encode(GStack.ResolveHost(UTF8ToString(Hostname)));
    finally
      TIdStack.DecUsage;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ResolveHostname (%s): %s', [UTF8ToString(Hostname), E.Message]));
    end;
  end;
end;

function TApiSitesServiceImpl.GhostSite(const SiteName: RawUTF8): boolean;
begin
  Result := False;
  try
    Debug(dpMessage, section, Format('GhostSite API: %s', [UTF8ToString(SiteName)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GhostSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.RecalcFreeSlots(const SiteName: RawUTF8): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    s.RecalcFreeslots;
    Debug(dpMessage, section, Format('RecalcFreeSlots API: %s', [UTF8ToString(SiteName)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RecalcFreeSlots: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.RebuildSlots(const SiteName: RawUTF8): boolean;
var
  s: TSite;
  i: integer;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    for i := 0 to s.slots.Count - 1 do
      s.RebuildSlot(i);
    s.RecalcFreeslots;

    Debug(dpMessage, section, Format('RebuildSlots API: %s (slots=%d)', [UTF8ToString(SiteName), s.slots.Count]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RebuildSlots: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.ExecuteIrcCommand(const Command: RawUTF8): boolean;
begin
  Result := False;
  try
    IrcProcessCommand('CONSOLE', 'Admin', UTF8ToString(Command));
    Debug(dpMessage, section, Format('ExecuteIrcCommand API: %s', [UTF8ToString(Command)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ExecuteIrcCommand: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteInfo(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;
var
  s: TSite;
  bncsArray: TDocVariantData;
  bncDoc: variant;
  i: integer;
  bncHost: string;
  bncPort: integer;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    Info := TApiSiteInfo.Create;
    Info.Name := SiteName;
    Info.Username := UTF8Encode(s.RCString('username', ''));

    case s.WorkingStatus of
      sstUp: Info.Status := 'UP';
      sstDown: Info.Status := 'DOWN';
      sstTempDown: Info.Status := 'DOWN';
      sstMarkedAsDownByUser: Info.Status := 'DOWN_BY_USER';
      sstUnknown: Info.Status := 'UNKNOWN';
      else Info.Status := 'UNKNOWN';
    end;

    Info.Slots := s.slots.Count;
    Info.FreeSlots := s.freeslots;
    Info.SslEnabled := (s.RCInteger('sslfxp', 0) > 0);
    Info.SslFxp := s.RCInteger('sslfxp', 0);

    bncsArray.InitFast(dvArray);
    i := 0;
    while i < 20 do
    begin
      bncHost := s.RCString('bnc_host-' + IntToStr(i), '');
      if bncHost = '' then
        Break;
      bncPort := s.RCInteger('bnc_port-' + IntToStr(i), 21);

      TDocVariant.New(bncDoc);
      TDocVariantData(bncDoc).AddValue('host', UTF8Encode(bncHost));
      TDocVariantData(bncDoc).AddValue('port', bncPort);
      bncsArray.AddItem(bncDoc);

      Inc(i);
    end;

    Info.Bncs := TDocVariantData(bncsArray).ToJSON;
    Info.Affils := UTF8Encode(s.siteaffils);
    Info.MaxIdle := s.RCInteger('max_idle', 0);
    Info.IdleInterval := s.RCInteger('idleinterval', 30);
    Info.LegacyCwd := s.RCBool('legacycwd', False);
    Info.AutoBncTestInterval := s.AutoBncTestInterval;
    Info.AutoDirlistInterval := s.AutoDirlistInterval;
    Info.AutoIndexInterval := s.AutoIndexInterval;
    Info.AutoNukeInterval := s.AutoNukeInterval;
    Info.Country := UTF8Encode(s.Country);
    Info.SkipBeingUploadedFiles := Integer(s.SkipBeingUploadedFiles);
    Info.KillConnectionOnStalledTransferSeconds := s.KillConnectionOnStalledTransferSeconds;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteInfo: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteConfig(const SiteName: RawUTF8; const Config: RawJSON): boolean;
var
  s: TSite;
  data: TDocVariantData;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if not data.InitJson(Config) then Exit;

    if data.GetValueIndex('autobnctest') >= 0 then s.AutoBncTestInterval := data.GetValueOrNull('autobnctest');
    if data.GetValueIndex('autodirlist') >= 0 then s.AutoDirlistInterval := data.GetValueOrNull('autodirlist');
    if data.GetValueIndex('autoindex') >= 0 then s.AutoIndexInterval := data.GetValueOrNull('autoindex');
    if data.GetValueIndex('autonuke') >= 0 then s.AutoNukeInterval := data.GetValueOrNull('autonuke');
    if data.GetValueIndex('country') >= 0 then s.Country := string(data.GetValueOrNull('country'));
    if data.GetValueIndex('skip_being_uploaded_files') >= 0 then s.SkipBeingUploadedFiles := TSkipBeingUploaded(Integer(data.GetValueOrNull('skip_being_uploaded_files')));
    if data.GetValueIndex('kill_connection_on_stalled_transfer') >= 0 then s.KillConnectionOnStalledTransferSeconds := data.GetValueOrNull('kill_connection_on_stalled_transfer');

    Debug(dpMessage, section, Format('SetSiteConfig API: %s updated', [UTF8ToString(SiteName)]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteConfig: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteCredentials(const SiteName: RawUTF8;
                                                  const Username, Password: RawUTF8;
                                                  const BncsJson: RawUTF8;
                                                  MaxIdle, IdleInterval: integer;
                                                  LegacyCwd: boolean;
                                                  SslFxp: integer): boolean;
var
  s: TSite;
  bncsArray: variant;
  i: integer;
  bncHost: string;
  bncPort: integer;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    s.WCString('username', UTF8ToString(Username));
    if Password <> '' then
      s.WCString('password', UTF8ToString(Password));

    i := 0;
    while i < 20 do
    begin
      if s.RCString('bnc_host-' + IntToStr(i), '') = '' then
        Break;
      s.DeleteKey('bnc_host-' + IntToStr(i));
      s.DeleteKey('bnc_port-' + IntToStr(i));
      Inc(i);
    end;

    if BncsJson <> '' then
    begin
      bncsArray := _JsonFast(BncsJson);
      for i := 0 to TDocVariantData(bncsArray).Count - 1 do
      begin
        bncHost := VariantToUTF8(TDocVariantData(bncsArray).Values[i].host);
        bncPort := TDocVariantData(bncsArray).Values[i].port;
        if bncHost <> '' then
        begin
          s.WCString('bnc_host-' + IntToStr(i), bncHost);
          s.WCInteger('bnc_port-' + IntToStr(i), bncPort);
        end;
      end;
    end;

    s.WCInteger('max_idle', MaxIdle);
    s.WCInteger('idleinterval', IdleInterval);
    s.WCBool('legacycwd', LegacyCwd);
    s.WCInteger('sslfxp', SslFxp);

    Debug(dpMessage, section, Format('SetSiteCredentials API: %s (BNCs=%d, SSLFXP=%d)', [UTF8ToString(SiteName), TDocVariantData(bncsArray).Count, SslFxp]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteCredentials: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetAvailableSections: RawJSON;
var
  sectionsArray: TDocVariantData;
  i: integer;
begin
  Result := '';
  try
    sectionsArray.InitFast(dvArray);

    if kb_sections <> nil then
    begin
      for i := 0 to kb_sections.Count - 1 do
      begin
        sectionsArray.AddItem(UTF8Encode(kb_sections[i]));
      end;
    end;

    Result := sectionsArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetAvailableSections: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteSections(const SiteName: RawUTF8): RawJSON;
var
  s: TSite;
  sectionsArray: TDocVariantData;
  sectionDoc: variant;
  i: integer;
  sectionName: string;
  sectionDir: string;
begin
  Result := '';
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
    begin
      Debug(dpError, section, Format('Site not found: %s', [UTF8ToString(SiteName)]));
      Result := '[]';
      Exit;
    end;

    sectionsArray.InitFast(dvArray);

    if kb_sections <> nil then
    begin
      for i := 0 to kb_sections.Count - 1 do
      begin
        sectionName := kb_sections[i];
        sectionDir := s.sectiondir[sectionName];

        TDocVariant.New(sectionDoc);
        TDocVariantData(sectionDoc).AddValue('section', UTF8Encode(sectionName));
        TDocVariantData(sectionDoc).AddValue('dir', UTF8Encode(sectionDir));
        sectionsArray.AddItem(sectionDoc);
      end;
    end;

    Result := sectionsArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteSections: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiSitesServiceImpl.SetSiteSection(const SiteName, Section, Dir: RawUTF8): boolean;
var
  s: TSite;
  sectionStr, dirStr: string;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
    begin
      Debug(dpError, section, Format('Site not found: %s', [UTF8ToString(SiteName)]));
      Exit;
    end;

    sectionStr := UTF8ToString(Section);
    dirStr := UTF8ToString(Dir);

    if dirStr = '' then
    begin
      s.SetSections(sectionStr, True);
      s.sectiondir[sectionStr] := '';
      s.sectionpretime[sectionStr] := -10;
      s.SetRankLock(sectionStr, 0);
      RulesRemove(UTF8ToString(SiteName), sectionStr);
      RemoveRanks(UTF8ToString(SiteName), sectionStr);
      RemoveSpeedStats(UTF8ToString(SiteName), sectionStr);
      Debug(dpMessage, section, Format('SetSiteSection API: Section %s removed from site %s', [sectionStr, UTF8ToString(SiteName)]));
    end
    else
    begin
      s.sectiondir[sectionStr] := dirStr;
      s.SetSections(sectionStr, False);
      Debug(dpMessage, section, Format('SetSiteSection API: Section %s dir on site %s set to %s', [sectionStr, UTF8ToString(SiteName), dirStr]));
    end;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteSection: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function _Md5OfUtf8String(const aText: RawUTF8): RawUTF8;
begin
  Result := MD5DigestToStr(MD5String(aText));
end;

function _Md5OfFile(const aFileName: string): RawUTF8;
begin
  Result := MD5DigestToStr(MD5File(aFileName));
end;

procedure _WriteUtf8TextFileAtomic(const aFileName: string; const aContent: RawUTF8);
var
  tmpFile: string;
  fs: TFileStream;
begin
  tmpFile := aFileName + '.sltmp';
  fs := TFileStream.Create(tmpFile, fmCreate);
  try
    if aContent <> '' then
      fs.WriteBuffer(Pointer(aContent)^, Length(aContent));
  finally
    fs.Free;
  end;

  if FileExists(aFileName) then
  begin
    if not DeleteFile(aFileName) then
      raise Exception.CreateFmt('Cannot delete existing file: %s', [aFileName]);
  end;

  if not RenameFile(tmpFile, aFileName) then
    raise Exception.CreateFmt('Cannot move file from %s to %s', [tmpFile, aFileName]);
end;

function _ResolveRtplFileName(const aSiteName: string): string;
var
  resolved: string;
begin
  resolved := UpperCase(aSiteName);
  if (resolved = '*') then
    resolved := getAdminSiteName;
  Result := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim + resolved + '.rtpl';
end;

function _ResolveSiteRulesSnapshotFileName(const aSiteName: string): string;
var
  resolved: string;
  splitSiteData: boolean;
begin
  resolved := UpperCase(aSiteName);
  splitSiteData := config.ReadBool('sites', 'split_site_data', False);
  if splitSiteData then
    Result := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim + resolved + '.siterules'
  else
    Result := ExtractFilePath(ParamStr(0)) + 'rules' + PathDelim + resolved + '.rules';
end;

function TApiSitesServiceImpl.GetSiteRtpl(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;
var
  fileName: string;
  s: RawUTF8;
  sl: TStringList;
begin
  Result := False;
  FileInfo := nil;
  try
    fileName := _ResolveRtplFileName(UTF8ToString(SiteName));
    ForceDirectories(ExtractFilePath(fileName));

    FileInfo := TApiTextFile.Create;
    FileInfo.SiteName := SiteName;
    FileInfo.Path := UTF8Encode(fileName);
    FileInfo.Exists := FileExists(fileName);
    if FileInfo.Exists then
      FileInfo.Md5 := _Md5OfFile(fileName)
    else
      FileInfo.Md5 := '';

    if FileInfo.Exists then
    begin
      sl := TStringList.Create;
      try
        sl.LoadFromFile(fileName);
        s := UTF8Encode(sl.Text);
        FileInfo.Content := s;
      finally
        sl.Free;
      end;
    end
    else
      FileInfo.Content := '';

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteRtpl: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteRulesSnapshot(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;
var
  fileName: string;
  s: RawUTF8;
  sl: TStringList;
begin
  Result := False;
  FileInfo := nil;
  try
    fileName := _ResolveSiteRulesSnapshotFileName(UTF8ToString(SiteName));
    ForceDirectories(ExtractFilePath(fileName));

    FileInfo := TApiTextFile.Create;
    FileInfo.SiteName := SiteName;
    FileInfo.Path := UTF8Encode(fileName);
    FileInfo.Exists := FileExists(fileName);
    if FileInfo.Exists then
      FileInfo.Md5 := _Md5OfFile(fileName)
    else
      FileInfo.Md5 := '';

    if FileInfo.Exists then
    begin
      sl := TStringList.Create;
      try
        sl.LoadFromFile(fileName);
        s := UTF8Encode(sl.Text);
        FileInfo.Content := s;
      finally
        sl.Free;
      end;
    end
    else
      FileInfo.Content := '';

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteRulesSnapshot: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.ValidateRtpl(const Content: RawUTF8; out Validation: TApiRulesValidation): boolean;
var
  lines: TStringList;
  errors: TDocVariantData;
  errObj: variant;
  i: integer;
  line: string;
  rule: TRule;
  tokens: TStringList;
  token: string;
  re: TRegExpr;
  reExpr: string;
  reCaseInsensitive: boolean;

  procedure _AddError(const aLine: integer; const aMessage: string);
  begin
    TDocVariant.New(errObj);
    TDocVariantData(errObj).AddValue('line', aLine);
    TDocVariantData(errObj).AddValue('message', UTF8Encode(aMessage));
    errors.AddItem(errObj);
  end;

  procedure _ValidateRegexToken(const aLine: integer; const aToken: string);
  var
    fLen: integer;
  begin
    fLen := Length(aToken);
    if fLen < 2 then
      Exit;
    if aToken[1] <> '/' then
      Exit;

    reCaseInsensitive := False;
    if aToken[fLen] = '/' then
    begin
      reExpr := Copy(aToken, 2, fLen - 2);
    end
    else if (fLen >= 3) and (aToken[fLen] = 'i') and (aToken[fLen - 1] = '/') then
    begin
      reCaseInsensitive := True;
      reExpr := Copy(aToken, 2, fLen - 3);
    end
    else
      Exit; // not a regex token in slmasks terms

    re := TRegExpr.Create;
    try
      re.ModifierI := reCaseInsensitive;
      re.Expression := reExpr;
      re.Compile; // enforce compilation to catch syntax errors early
    except
      on E: Exception do
        _AddError(aLine, Format('Invalid regex %s: %s', [aToken, E.Message]));
    end;
    re.Free;
  end;
begin
  Result := False;
  try
    Validation := TApiRulesValidation.Create;
    errors.InitFast(dvArray);

    lines := TStringList.Create;
    try
      lines.Text := UTF8ToString(Content);
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        if (line = '') or (line[1] = '#') then
          Continue;

        rule := nil;
        try
          rule := TRule.Create(line);
        except
          on E: Exception do
          begin
            _AddError(i + 1, E.Message);
            Continue;
          end;
        end;

        try
          if rule.error <> '' then
          begin
            _AddError(i + 1, rule.error);
            Continue;
          end;

          // Additional validation: ensure regex tokens compile (TRegExpr compiles lazily at Exec time)
          tokens := TStringList.Create;
          try
            ExtractStrings([' '], [], PChar(line), tokens);
            for token in tokens do
              _ValidateRegexToken(i + 1, token);
          finally
            tokens.Free;
          end;
        finally
          rule.Free;
        end;
      end;
    finally
      lines.Free;
    end;

    Validation.Ok := TDocVariantData(errors).Count = 0;
    Validation.Errors := errors.ToJSON;
    Exit(True);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ValidateRtpl: %s', [E.Message]));
      Exit(False);
    end;
  end;
end;

function TApiSitesServiceImpl.SaveSiteRtpl(const SiteName: RawUTF8; const Content: RawUTF8; const ExpectedMd5: RawUTF8;
  Reload: boolean; out SaveResult: TApiRulesSaveResult): boolean;
var
  fileName: string;
  currentMd5: RawUTF8;
  validation: TApiRulesValidation;
  msg: RawUTF8;
begin
  Result := False;
  try
    SaveResult := TApiRulesSaveResult.Create;
    SaveResult.Ok := False;

    fileName := _ResolveRtplFileName(UTF8ToString(SiteName));
    ForceDirectories(ExtractFilePath(fileName));

    currentMd5 := '';
    if FileExists(fileName) then
      currentMd5 := _Md5OfFile(fileName);

    if (ExpectedMd5 <> '') and (currentMd5 <> '') and (UpperCase(UTF8ToString(ExpectedMd5)) <> UpperCase(UTF8ToString(currentMd5))) then
    begin
      msg := 'Conflict: file changed on disk since last load';
      SaveResult.Message := msg;
      SaveResult.Path := UTF8Encode(fileName);
      SaveResult.Md5 := currentMd5;
      SaveResult.Errors := '[]';
      Exit(True);
    end;

    if not ValidateRtpl(Content, validation) then
      Exit(False);
    try
      if not validation.Ok then
      begin
        SaveResult.Message := 'Validation failed';
        SaveResult.Path := UTF8Encode(fileName);
        SaveResult.Md5 := currentMd5;
        SaveResult.Errors := validation.Errors;
        Exit(True);
      end;
    finally
      validation.Free;
    end;

    _WriteUtf8TextFileAtomic(fileName, Content);
    SaveResult.Path := UTF8Encode(fileName);
    SaveResult.Md5 := _Md5OfFile(fileName);

    if Reload then
      RulesReload;

    SaveResult.Ok := True;
    SaveResult.Message := 'Saved';
    SaveResult.Errors := '[]';
    Exit(True);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SaveSiteRtpl: %s', [E.Message]));
      Exit(False);
    end;
  end;
end;

function TApiSitesServiceImpl.ReloadRules: boolean;
begin
  Result := False;
  try
    RulesReload;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ReloadRules: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetRuleConditions: RawJSON;
var
  arr: TDocVariantData;
  item: variant;
  i: integer;
  condClass: TConditionClass;
begin
  Result := '[]';
  try
    arr.InitFast(dvArray);
    if rulesunit.conditions <> nil then
    begin
      for i := 0 to rulesunit.conditions.Count - 1 do
      begin
        condClass := TConditionClass(rulesunit.conditions[i]);
        TDocVariant.New(item);
        TDocVariantData(item).AddValue('name', UTF8Encode(condClass.Name));
        if condClass <> TBooleanCondition then
          TDocVariantData(item).AddValue('ops', UTF8Encode(condClass.AcceptedOperatorsAsText))
        else
          TDocVariantData(item).AddValue('ops', '');
        TDocVariantData(item).AddValue('description', UTF8Encode(condClass.Description));
        arr.AddItem(item);
      end;
    end;
    Result := arr.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetRuleConditions: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

{ TApiQueueServiceImpl }

function TApiQueueServiceImpl.GetQueueStats(out Stats: TApiQueueStats): boolean;
var
  tTotal, tRace, tDir, tAuto, tOther: integer;
begin
  Result := False;
  try
    Stats := TApiQueueStats.Create;
    QueueStatAll;

    GetQueueTotals(tTotal, tRace, tDir, tAuto, tOther);

    Stats.TotalTasks := tTotal;
    Stats.RaceTasks := tRace;
    Stats.DirlistTasks := tDir;
    Stats.AutoTasks := tAuto;
    Stats.OtherTasks := tOther;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetQueueStats: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiQueueServiceImpl.GetQueue(const SiteName: RawUTF8): RawJSON;
var
  queueDoc: TDocVariantData;
begin
  queueDoc.InitFast(dvArray);

  try
    Debug(dpSpam, section, Format('GetQueue API: %s', [UTF8ToString(SiteName)]));
    Result := queueDoc.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetQueue: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiQueueServiceImpl.GetTask(TaskUid: Int64; out Info: TApiTaskInfo): boolean;
begin
  Result := False;
  try
    Info := TApiTaskInfo.Create;
    Info.Uid := TaskUid;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetTask: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiQueueServiceImpl.CreateDirlistTask(const SiteName, Section, Dir: RawUTF8): Int64;
begin
  Result := 0;
  try
    Debug(dpMessage, section, Format('CreateDirlistTask API: %s %s %s',
          [UTF8ToString(SiteName), UTF8ToString(Section), UTF8ToString(Dir)]));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateDirlistTask: %s', [E.Message]));
    end;
  end;
end;

function TApiQueueServiceImpl.CreateSpreadTask(const SourceSite, Section, Release: RawUTF8): Int64;
begin
  Result := 0;
  try
    Debug(dpMessage, section, Format('CreateSpreadTask API: %s %s %s',
          [UTF8ToString(SourceSite), UTF8ToString(Section), UTF8ToString(Release)]));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateSpreadTask: %s', [E.Message]));
    end;
  end;
end;

function TApiQueueServiceImpl.CreateTransferTask(const SourceSite, DestSite, Section,
                                                Dir, FileName: RawUTF8): Int64;
begin
  Result := 0;
  try
    Debug(dpMessage, section, Format('CreateTransferTask API: %s -> %s',
          [UTF8ToString(SourceSite), UTF8ToString(DestSite)]));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateTransferTask: %s', [E.Message]));
    end;
  end;
end;

function TApiQueueServiceImpl.StopTask(TaskUid: Int64): boolean;
begin
  Result := False;
  try
    Debug(dpMessage, section, Format('StopTask API: %d', [TaskUid]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] StopTask: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiQueueServiceImpl.EmptyQueue(const SiteName: RawUTF8): boolean;
begin
  Result := False;
  try
    Debug(dpMessage, section, Format('EmptyQueue API: %s', [UTF8ToString(SiteName)]));
    QueueEmpty(UTF8ToString(SiteName));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] EmptyQueue: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

{ Stub implementations for other services }

function TApiStatsServiceImpl.GetRaceStats(const SiteName, Period: RawUTF8; Detailed: boolean): RawJSON;
var
  fSiteName: String;
  fPeriod: String;
  temp: TTextWriterStackBuffer;
begin
  fSiteName := UpperCase(Trim(UTF8ToString(SiteName)));
  if fSiteName = '' then
    fSiteName := '*';

  fPeriod := UpperCase(Trim(UTF8ToString(Period)));
  if (fPeriod <> 'YEAR') and (fPeriod <> 'MONTH') then
    fPeriod := 'DAY';

  if (fSiteName <> '*') and (FindSiteByName('', fSiteName) = nil) then
  begin
    with TJsonWriter.CreateOwnedStream(temp) do
    try
      AddShort('{"enabled":');
      Add(IsStatsDatabaseActive);
      AddShort(',"error":"Site not found","site":');
      AddJsonString(UTF8Encode(fSiteName));
      AddShort(',"period":');
      AddJsonString(UTF8Encode(fPeriod));
      AddDirect('}');
      SetText(Result);
    finally
      Free;
    end;
    Exit;
  end;

  Result := StatsGetRaceStatsJson(fSiteName, fPeriod, Detailed);
end;

function TApiStatsServiceImpl.GetRanks(const SiteName: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiStatsServiceImpl.SetRank(const SiteName, Section: RawUTF8; Score: integer): boolean;
begin
  Result := True;
end;

function TApiStatsServiceImpl.RecalculateRanks: boolean;
begin
  Result := True;
end;

function TApiIrcServiceImpl.GetNetworks: RawJSON;
var
  i: integer;
  th: TMyIrcThread;
  networksArray: TDocVariantData;
  netDoc: variant;
begin
  Result := '';
  try
    networksArray.InitFast(dvArray);

    if myIrcThreads <> nil then
    begin
      for i := 0 to myIrcThreads.Count - 1 do
      begin
        try
          th := TMyIrcThread(myIrcThreads[i]);
          if th = nil then
            Continue;

          TDocVariant.New(netDoc);
          TDocVariantData(netDoc).AddValue('name', UTF8Encode(th.Netname));
          TDocVariantData(netDoc).AddValue('host', UTF8Encode(th.host));
          TDocVariantData(netDoc).AddValue('port', th.port);
          TDocVariantData(netDoc).AddValue('status', UTF8Encode(th.status));
          TDocVariantData(netDoc).AddValue('nickname', UTF8Encode(th.irc_nick));
          TDocVariantData(netDoc).AddValue('connected', Pos('online', LowerCase(th.status)) > 0);

          if th.channels <> nil then
            TDocVariantData(netDoc).AddValue('channels_count', th.channels.Count)
          else
            TDocVariantData(netDoc).AddValue('channels_count', 0);

          networksArray.AddItem(netDoc);
        except
          on E: Exception do
          begin
            Debug(dpError, 'slapi', Format('[EXCEPTION] GetNetworks loop: %s', [E.Message]));
            Continue;
          end;
        end;
      end;
    end;

    Result := networksArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetNetworks: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiIrcServiceImpl.GetNetworkStatus(const NetName: RawUTF8; out Info: TApiIrcNetwork): boolean;
begin
  Info := TApiIrcNetwork.Create;
  Result := True;
end;

function TApiIrcServiceImpl.GetChannels(const NetName: RawUTF8): RawJSON;
var
  channelsArray: TDocVariantData;
  chanDoc: variant;
  chanSettings: TIrcChannelSettings;
  key, settingsKey: string;
  netNameStr: string;
  i, j: integer;
  th: TMyIrcThread;
  channelName: string;
begin
  Result := '';
  try
    channelsArray.InitFast(dvArray);
    netNameStr := UTF8ToString(NetName);

    // Find the IRC thread for this network
    th := nil;
    if myIrcThreads <> nil then
    begin
      for i := 0 to myIrcThreads.Count - 1 do
      begin
        try
          th := TMyIrcThread(myIrcThreads[i]);
          if (th <> nil) and SameText(th.netname, netNameStr) then
            Break
          else
            th := nil;
        except
          th := nil;
        end;
      end;
    end;

    // If thread found, iterate through actual connected channels
    if (th <> nil) and (th.channels <> nil) then
    begin
      for j := 0 to th.channels.Count - 1 do
      begin
        try
          // Extract channel name (key) from TStringList Name=Value pair
          channelName := th.channels.Names[j];
          if channelName = '' then
            Continue;

          TDocVariant.New(chanDoc);
          TDocVariantData(chanDoc).AddValue('channel', UTF8Encode(channelName));

          // Try to find settings for this channel
          chanSettings := nil;
          if IrcChanSettingsList <> nil then
          begin
            for settingsKey in IrcChanSettingsList.Keys do
            begin
              try
                chanSettings := IrcChanSettingsList[settingsKey];
                if (chanSettings <> nil) and
                   SameText(chanSettings.Netname, netNameStr) and
                   SameText(chanSettings.Channel, channelName) then
                  Break
                else
                  chanSettings := nil;
              except
                chanSettings := nil;
              end;
            end;
          end;

          // Add settings if found, otherwise use empty values
          if chanSettings <> nil then
          begin
            TDocVariantData(chanDoc).AddValue('chankey', UTF8Encode(chanSettings.ChanKey));
            TDocVariantData(chanDoc).AddValue('chanroles', UTF8Encode(chanSettings.ChanRoles));

            // Get blowkey - need to check class type for access
            if chanSettings.ClassName = 'TIrcBlowkeyECB' then
              TDocVariantData(chanDoc).AddValue('blowkey', '[ECB encrypted]')
            else if chanSettings.ClassName = 'TIrcBlowkeyCBC' then
              TDocVariantData(chanDoc).AddValue('blowkey', '[CBC encrypted]')
            else
              TDocVariantData(chanDoc).AddValue('blowkey', '');
          end
          else
          begin
            TDocVariantData(chanDoc).AddValue('chankey', '');
            TDocVariantData(chanDoc).AddValue('chanroles', '');
            TDocVariantData(chanDoc).AddValue('blowkey', '');
          end;

          channelsArray.AddItem(chanDoc);
        except
          on E: Exception do
          begin
            Debug(dpError, 'slapi', Format('[EXCEPTION] GetChannels loop: %s', [E.Message]));
            Continue;
          end;
        end;
      end;
    end;

    Result := channelsArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetChannels: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiIrcServiceImpl.SendMessage(const NetName, Channel, Message: RawUTF8): boolean;
var
  netNameStr, channelStr, messageStr: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);
    messageStr := UTF8ToString(Message);

    if FindIrcChannelSettings(netNameStr, channelStr) = nil then
    begin
      Debug(dpError, 'slapi', Format('SendMessage: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    irc_addtext(netNameStr, channelStr, messageStr);
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SendMessage: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.JumpServer(const NetName: RawUTF8): boolean;
var
  ircth: TMyIrcThread;
  netNameStr: string;
begin
  Result := False;
  try
    netNameStr := UpperCase(UTF8ToString(NetName));
    ircth := FindIrcnetwork(netNameStr);

    if ircth <> nil then
    begin
      ircth.shouldrestart := True;
      myIrcThreads.Remove(ircth);
      myIrcThreads.Add(TMyIrcThread.Create(netNameStr));
      Debug(dpMessage, 'slapi', Format('JumpServer: Restarting IRC network %s', [netNameStr]));
      Result := True;
    end
    else
    begin
      Debug(dpError, 'slapi', Format('JumpServer: Network %s not found', [netNameStr]));
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] JumpServer: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.SetChannelBlowkey(const NetName, Channel, Blowkey: RawUTF8): boolean;
var
  chanSettings: TIrcChannelSettings;
  netNameStr, channelStr, blowkeyStr: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);
    blowkeyStr := UTF8ToString(Blowkey);

    chanSettings := FindIrcChannelSettings(netNameStr, channelStr);
    if chanSettings = nil then
    begin
      Debug(dpError, 'slapi', Format('SetChannelBlowkey: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    chanSettings.UpdateKey(blowkeyStr);
    Debug(dpMessage, 'slapi', Format('SetChannelBlowkey: Updated blowkey for %s@%s', [channelStr, netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SetChannelBlowkey: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.SetChannelKey(const NetName, Channel, ChanKey: RawUTF8): boolean;
var
  chanSettings: TIrcChannelSettings;
  netNameStr, channelStr, chankeyStr: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);
    chankeyStr := UTF8ToString(ChanKey);

    chanSettings := FindIrcChannelSettings(netNameStr, channelStr);
    if chanSettings = nil then
    begin
      Debug(dpError, 'slapi', Format('SetChannelKey: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    chanSettings.ChanKey := chankeyStr;
    Debug(dpMessage, 'slapi', Format('SetChannelKey: Updated chankey for %s@%s', [channelStr, netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SetChannelKey: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.SetChannelRoles(const NetName, Channel, Roles: RawUTF8): boolean;
var
  chanSettings: TIrcChannelSettings;
  netNameStr, channelStr, rolesStr: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);
    rolesStr := UTF8ToString(Roles);

    chanSettings := FindIrcChannelSettings(netNameStr, channelStr);
    if chanSettings = nil then
    begin
      Debug(dpError, 'slapi', Format('SetChannelRoles: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    chanSettings.ChanRoles := rolesStr;
    Debug(dpMessage, 'slapi', Format('SetChannelRoles: Updated roles for %s@%s', [channelStr, netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SetChannelRoles: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.AddChannel(const NetName, Channel, ChanKey, Blowkey, Roles: RawUTF8): boolean;
var
  netNameStr, channelStr, chankeyStr, blowkeyStr, rolesStr: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);
    chankeyStr := UTF8ToString(ChanKey);
    blowkeyStr := UTF8ToString(Blowkey);
    rolesStr := UTF8ToString(Roles);

    // Check if channel already exists
    if FindIrcChannelSettings(netNameStr, channelStr) <> nil then
    begin
      Debug(dpError, 'slapi', Format('AddChannel: Channel %s@%s already exists', [channelStr, netNameStr]));
      Exit;
    end;

    // Use RegisterChannelSettings to add the channel
    RegisterChannelSettings(netNameStr, channelStr, rolesStr, blowkeyStr, chankeyStr, False, True);

    Debug(dpMessage, 'slapi', Format('AddChannel: Added channel %s@%s', [channelStr, netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] AddChannel: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.DeleteChannel(const NetName, Channel: RawUTF8): boolean;
var
  netNameStr, channelStr, dictKey: string;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);

    // Check if channel exists
    if FindIrcChannelSettings(netNameStr, channelStr) = nil then
    begin
      Debug(dpError, 'slapi', Format('DeleteChannel: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    // Create key for dictionary (same format as in IrcChanSettingsList)
    dictKey := netNameStr + channelStr;

    // Remove from global list
    IrcChanSettingsList.Remove(dictKey);

    Debug(dpMessage, 'slapi', Format('DeleteChannel: Removed channel %s@%s', [channelStr, netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] DeleteChannel: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.AddNetwork(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;
var
  params: string;
  sslStr: string;
begin
  Result := False;
  try
    if Ssl then
      sslStr := '1'
    else
      sslStr := '0';
    params := UTF8ToString(NetName) + ' ' + UTF8ToString(Host) + ':' + IntToStr(Port) + ' ' + sslStr + ' ' + UTF8ToString(Password) + ' ' + UTF8ToString(Nick) + ' ' + UTF8ToString(Ident) + ' ' + UTF8ToString(User);
    Result := IrcAddnet('', '', params);
    if Result then
      Debug(dpMessage, 'slapi', Format('AddNetwork: Added IRC network %s', [UTF8ToString(NetName)]))
    else
      Debug(dpError, 'slapi', Format('AddNetwork: Failed to add IRC network %s', [UTF8ToString(NetName)]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] AddNetwork: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiIrcServiceImpl.DeleteNetwork(const NetName: RawUTF8): boolean;
var
  params: string;
begin
  Result := False;
  try
    params := UTF8ToString(NetName);
    Result := IrcDelnet('', '', params);
    if Result then
      Debug(dpMessage, 'slapi', Format('DeleteNetwork: Deleted IRC network %s', [UTF8ToString(NetName)]))
    else
      Debug(dpError, 'slapi', Format('DeleteNetwork: Failed to delete IRC network %s', [UTF8ToString(NetName)]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] DeleteNetwork: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiRulesServiceImpl.GetRules(const SiteName, Section: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiRulesServiceImpl.GetRule(RuleId: integer): RawJSON;
begin
  Result := '{}';
end;

function TApiRulesServiceImpl.AddRule(const RuleData: RawJSON): integer;
begin
  Result := 0;
end;

function TApiRulesServiceImpl.ModifyRule(RuleId: integer; const RuleData: RawJSON): boolean;
begin
  Result := True;
end;

function TApiRulesServiceImpl.DeleteRule(RuleId: integer): boolean;
begin
  Result := True;
end;

function TApiRulesServiceImpl.TestRule(const RuleData, ReleaseName: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiRulesServiceImpl.ReloadRules: boolean;
begin
  Result := True;
end;

function TApiSpeedServiceImpl.GetRoutes(const SiteName: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiSpeedServiceImpl.TestSpeedLocal(const SiteName: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiSpeedServiceImpl.TestSpeedOut(const SourceSite, DestSites: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiSpeedServiceImpl.TestSpeedIn(const DestSite, SourceSites: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiSpeedServiceImpl.GetSpeedResults(const SiteName: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiSpeedServiceImpl.RecalculateRoutes: boolean;
begin
  Result := True;
end;

function TApiKnowledgeBaseServiceImpl.GetKBEntries(const Section: RawUTF8; Limit: integer): RawJSON;
begin
  Result := '[]';
end;

function TApiKnowledgeBaseServiceImpl.SearchKB(const Query: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiKnowledgeBaseServiceImpl.AddKBEntry(const Section, Release: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiPrecatcherServiceImpl.GetPrecatcherRules: RawJSON;
var
  i: integer;
  rulesArray: TDocVariantData;
  ruleDoc: variant;
  netname, channel, botnicks, sitename, event, words, section: string;
begin
  Result := '[]';
  try
    rulesArray.InitFast(dvArray);

    if catcherFile <> nil then
    begin
      for i := 0 to catcherFile.Count - 1 do
      begin
        netname := SubString(catcherFile[i], ';', 1);
        channel := SubString(catcherFile[i], ';', 2);
        botnicks := SubString(catcherFile[i], ';', 3);
        sitename := SubString(catcherFile[i], ';', 4);
        event := SubString(catcherFile[i], ';', 5);
        words := SubString(catcherFile[i], ';', 6);
        section := SubString(catcherFile[i], ';', 7);

        TDocVariant.New(ruleDoc);
        TDocVariantData(ruleDoc).AddValue('id', i);
        TDocVariantData(ruleDoc).AddValue('netname', UTF8Encode(netname));
        TDocVariantData(ruleDoc).AddValue('channel', UTF8Encode(channel));
        TDocVariantData(ruleDoc).AddValue('botnicks', UTF8Encode(botnicks));
        TDocVariantData(ruleDoc).AddValue('sitename', UTF8Encode(sitename));
        TDocVariantData(ruleDoc).AddValue('event', UTF8Encode(event));
        TDocVariantData(ruleDoc).AddValue('words', UTF8Encode(words));
        TDocVariantData(ruleDoc).AddValue('section', UTF8Encode(section));

        rulesArray.AddItem(ruleDoc);
      end;
    end;

    Result := rulesArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetPrecatcherRules: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiPrecatcherServiceImpl.AddPrecatcherRule(const RuleData: RawJSON): integer;
var
  ruleDoc: variant;
  netname, channel, botnicks, sitename, event, words, section: string;
  kb_event: TKBEventType;
begin
  Result := -1;
  try
    ruleDoc := _JsonFast(RuleData);

    netname := UpperCase(UTF8ToString(VariantToUTF8(ruleDoc.netname)));
    channel := UTF8ToString(VariantToUTF8(ruleDoc.channel));
    botnicks := UTF8ToString(VariantToUTF8(ruleDoc.botnicks));
    sitename := UpperCase(UTF8ToString(VariantToUTF8(ruleDoc.sitename)));
    event := UpperCase(UTF8ToString(VariantToUTF8(ruleDoc.event)));
    words := UTF8ToString(VariantToUTF8(ruleDoc.words));
    section := UTF8ToString(VariantToUTF8(ruleDoc.section));

    // Validate event type
    kb_event := EventStringToTKBEventType(event);
    if not (kb_event in [kbePRE, kbeADDPRE, kbeCOMPLETE, kbeNEWDIR, kbeNUKE, kbeREQUEST]) then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Invalid event type: %s', [event]));
      Exit;
    end;

    // Validate site exists
    if FindSiteByName('', sitename) = nil then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Site %s not found', [sitename]));
      Exit;
    end;

    // Validate channel exists
    if FindIrcChannelSettings(netname, channel) = nil then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Channel %s@%s not found', [channel, netname]));
      Exit;
    end;

    // Add rule to catcherFile
    catcherFile.Add(Format('%s;%s;%s;%s;%s;%s;%s',
      [netname, channel, botnicks, sitename, event, words, section]));

    // Rebuild precatcher
    PrecatcherRebuild;

    Result := catcherFile.Count - 1; // Return ID of newly added rule

    Debug(dpMessage, 'slapi', Format('AddPrecatcherRule: Added rule for %s@%s -> %s', [channel, netname, sitename]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] AddPrecatcherRule: %s', [E.Message]));
      Result := -1;
    end;
  end;
end;

function TApiPrecatcherServiceImpl.DeletePrecatcherRule(RuleId: integer): boolean;
begin
  Result := False;
  try
    if (RuleId < 0) or (RuleId >= catcherFile.Count) then
    begin
      Debug(dpError, 'slapi', Format('DeletePrecatcherRule: Invalid rule ID: %d', [RuleId]));
      Exit;
    end;

    catcherFile.Delete(RuleId);
    PrecatcherRebuild;

    Debug(dpMessage, 'slapi', Format('DeletePrecatcherRule: Deleted rule #%d', [RuleId]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] DeletePrecatcherRule: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiPrecatcherServiceImpl.TestPrecatcher(const Announce: RawUTF8): RawJSON;
var
  announceDoc, resultDoc: variant;
  netname, channel, nick, text: string;
  debugLines: TStringList;
begin
  Result := '{}';
  debugLines := nil;
  try
    if Announce = '' then
    begin
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode('Missing announce payload'));
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    announceDoc := _JsonFast(Announce);

    if VarIsEmpty(announceDoc) or VarIsNull(announceDoc) then
    begin
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode('Invalid announce JSON'));
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    // Accept both direct payload and wrapper { "Announce": { ... } }
    try
      if not VarIsEmpty(announceDoc.Announce) and not VarIsNull(announceDoc.Announce) then
        announceDoc := announceDoc.Announce;
    except
      // ignore if no Announce field
    end;

    netname := UpperCase(UTF8ToString(VariantToUTF8(announceDoc.netname)));
    channel := UTF8ToString(VariantToUTF8(announceDoc.channel));
    nick := UTF8ToString(VariantToUTF8(announceDoc.nick));
    text := UTF8ToString(VariantToUTF8(announceDoc.text));

    if Pos('@', channel) > 0 then
      channel := Copy(channel, 1, Pos('@', channel) - 1);

    if (netname = '') or (channel = '') or (nick = '') or (text = '') then
    begin
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode('Missing required fields (netname, channel, nick, text)'));
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    if FindIrcChannelSettings(netname, channel) = nil then
    begin
      Debug(dpError, 'slapi', Format('TestPrecatcher: Channel %s@%s not found', [channel, netname]));
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(Format('Channel %s@%s not found', [channel, netname])));
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    // Enable debug capture temporarily
    Precatcher_BeginDebugCapture(debugLines);

    // Process the announce
    PrecatcherProcessB(netname, channel, nick, text);

    // Disable debug capture
    Precatcher_EndDebugCapture(debugLines);

    // Return success
    TDocVariant.New(resultDoc);
    TDocVariantData(resultDoc).AddValue('success', True);
    TDocVariantData(resultDoc).AddValue('message', UTF8Encode('Precatcher test completed successfully'));
    if (debugLines <> nil) and (debugLines.Count > 0) then
      TDocVariantData(resultDoc).AddValue('output', UTF8Encode(debugLines.Text))
    else
      TDocVariantData(resultDoc).AddValue('output', UTF8Encode(''));
    Result := VariantSaveJSON(resultDoc);

    Debug(dpMessage, 'slapi', Format('TestPrecatcher: Tested announce from %s on %s@%s', [nick, channel, netname]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] TestPrecatcher: %s', [E.Message]));
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(E.Message));
      Result := VariantSaveJSON(resultDoc);
    end;
  end;
  if debugLines <> nil then
    debugLines.Free;
end;

function TApiPrecatcherServiceImpl.ReloadPrecatcher: boolean;
var
  error_msg: string;
begin
  Result := False;
  try
    error_msg := PrecatcherReload;
    if error_msg = '' then
    begin
      Debug(dpMessage, 'slapi', 'ReloadPrecatcher: Precatcher reloaded successfully');
      Result := True;
    end
    else
    begin
      Debug(dpError, 'slapi', Format('ReloadPrecatcher: %s', [error_msg]));
      Result := False;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] ReloadPrecatcher: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiPrecatcherServiceImpl.GetMappings: RawJSON;
var
  i: integer;
  mappingsArray: TDocVariantData;
  mappingDoc: variant;
  mapping: TMap;
begin
  Result := '[]';
  try
    mappingsArray.InitFast(dvArray);

    if mappingslist <> nil then
    begin
      for i := 0 to mappingslist.Count - 1 do
      begin
        mapping := TMap(mappingslist.Items[i]);
        if mapping <> nil then
        begin
          TDocVariant.New(mappingDoc);
          TDocVariantData(mappingDoc).AddValue('id', i);
          TDocVariantData(mappingDoc).AddValue('origsection', UTF8Encode(mapping.origsection));
          TDocVariantData(mappingDoc).AddValue('newsection', UTF8Encode(mapping.newsection));
          TDocVariantData(mappingDoc).AddValue('mask', UTF8Encode(mapping.mask.mask));

          mappingsArray.AddItem(mappingDoc);
        end;
      end;
    end;

    Result := mappingsArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetMappings: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

end.
