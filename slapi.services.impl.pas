unit slapi.services.impl;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  DateUtils,
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
  sitesunit,
  queueunit,
  tasksunit,
  tasklogin,
  statsunit,
  ranksunit,
  rulesunit,
  irc,
  kb,
  pazo,
  precatcher,
  mainthread,
  debugunit,
  configunit,
  routeconfig,
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
    function RunSiteAutoRules(const SiteName: RawUTF8): boolean;
    function GetSiteRoutes(const SiteName: RawUTF8; out Routes: TApiSiteRoutes): boolean;
    function TestSite(const SiteName: RawUTF8): boolean;
    function GhostSite(const SiteName: RawUTF8): boolean;
    function RecalcFreeSlots(const SiteName: RawUTF8): boolean;
    function RebuildSlots(const SiteName: RawUTF8): boolean;
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

implementation

uses
  Contnrs;

{$I slftp.inc}

const
  section = 'slapi.services';

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
        try
          s := TSite(sitesunit.sites[i]);
          if s = nil then
            Continue;

          // Copy data immediately while we have the reference
          snapshots[snapshotCount].Name := s.Name;
          snapshots[snapshotCount].Status := s.WorkingStatus;
          if s.PermDown then
            snapshots[snapshotCount].Status := sstDown; // treat permdown as down in snapshots
          snapshots[snapshotCount].Slots := s.slots.Count;
          snapshots[snapshotCount].FreeSlots := s.freeslots;
          snapshots[snapshotCount].MaxUp := s.RCInteger('max_up', s.max_up);
          snapshots[snapshotCount].MaxPreDn := s.RCInteger('max_pre_dn', s.max_pre_dn);
          snapshots[snapshotCount].MaxDn := s.RCInteger('max_dn', s.slots.Count);
          snapshots[snapshotCount].NumDn := s.num_dn;
          snapshots[snapshotCount].NumUp := s.num_up;
          snapshots[snapshotCount].PermDown := s.PermDown;
          snapshots[snapshotCount].AutoLogin := s.RCBool('autologin', False);
          snapshots[snapshotCount].AutoRulesInterval := s.AutoRulesStatus;
          Inc(snapshotCount);
        except
          // Skip this site if we can't read it
          Continue;
        end;
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
    Info.Host := UTF8Encode(s.RCString('host', ''));
    Info.Port := s.RCInteger('port', 21);
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
begin
  Result := False;
  try
    Debug(dpMessage, section, Format('AddSite API: %s@%s:%d', [UTF8ToString(Name), UTF8ToString(Host), Port]));
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

    t := TLoginTask.Create('API', '', s.Name, False, False);
    t.noannounce := True; // keep IRC quiet
    AddTask(t);

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
begin
  Result := '{}';
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
begin
  Result := '[]';
end;

function TApiIrcServiceImpl.GetNetworkStatus(const NetName: RawUTF8; out Info: TApiIrcNetwork): boolean;
begin
  Info := TApiIrcNetwork.Create;
  Result := True;
end;

function TApiIrcServiceImpl.GetChannels(const NetName: RawUTF8): RawJSON;
begin
  Result := '[]';
end;

function TApiIrcServiceImpl.SendMessage(const NetName, Channel, Message: RawUTF8): boolean;
begin
  Result := True;
end;

function TApiIrcServiceImpl.JumpServer(const NetName: RawUTF8): boolean;
begin
  Result := True;
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
begin
  Result := '[]';
end;

function TApiPrecatcherServiceImpl.AddPrecatcherRule(const RuleData: RawJSON): integer;
begin
  Result := 0;
end;

function TApiPrecatcherServiceImpl.DeletePrecatcherRule(RuleId: integer): boolean;
begin
  Result := True;
end;

function TApiPrecatcherServiceImpl.TestPrecatcher(const Announce: RawUTF8): RawJSON;
begin
  Result := '{}';
end;

function TApiPrecatcherServiceImpl.ReloadPrecatcher: boolean;
begin
  Result := True;
end;

function TApiPrecatcherServiceImpl.GetMappings: RawJSON;
begin
  Result := '[]';
end;

end.
