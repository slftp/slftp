unit slapi.services.impl;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  DateUtils,
  Variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.soa.core,
  mormot.soa.server,
  slapi.types,
	  slapi.services,
    slapi.speedtest,
	  slapi.issues,
	  sitesunit,
	  queueunit,
	  tasksunit,
  taskrace,
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
  slcriticalsection2,
  globals;

function ApiGetSlotsRuntimeJson(const SiteName: RawUTF8): RawJSON;
function ApiGetSlotHistorySSE(const aSiteName: string; aSlotNumber: integer; aSeq: QWord; aTimeoutMs: integer): RawUTF8;

{ Updates system status peak values (load avg, CPU, queue size).
  Called periodically from Main_Iter so peaks are tracked independently of API calls. }
procedure UpdateSystemStatusPeaks;

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
    function GetSlotsRuntime(const SiteName: RawUTF8): RawJSON;
    function GetSiteCredits(const SiteName: RawUTF8; ForceRefresh: boolean; out Credits: TApiSiteCredits): boolean;
    function GetSiteUser(const SiteName, UserName: RawUTF8; out Info: TApiSiteUserInfo): boolean;
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
    function SetSiteSslMethod(const SiteName: RawUTF8; SslMethod: integer): boolean;
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
    function CreateReleaseTransferTask(const SourceSite, DestSite, SourceDir,
                                DestDir, RlsName: RawUTF8): Int64;
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

  { Live Races Service Implementation }
  TApiRacesServiceImpl = class(TInjectableObjectRest, IApiRacesService)
  public
    function GetRaces(const Page: integer; const PageSize: integer; const SinceUnix: Int64): RawJSON;
    function GetReleaseTransfers(const Release: RawUTF8; const Page: integer; const PageSize: integer; const SinceUnix: Int64): RawJSON;
  end;

  { IRC Service Implementation }
  TApiIrcServiceImpl = class(TInjectableObjectRest, IApiIrcService)
  public
    function GetNetworks: RawJSON;
    function GetNetworkStatus(const NetName: RawUTF8;
                              out Info: TApiIrcNetwork): boolean;
    function GetNetworkConfig(const NetName: RawUTF8;
                              out Info: TApiIrcNetworkConfig): boolean;
    function GetChannels(const NetName: RawUTF8): RawJSON;
    function SendMessage(const NetName, Channel, Message: RawUTF8): boolean;
    function JumpServer(const NetName: RawUTF8): boolean;
    function SetChannelBlowkey(const NetName, Channel, Blowkey: RawUTF8): boolean;
    function SetChannelKey(const NetName, Channel, ChanKey: RawUTF8): boolean;
    function SetChannelRoles(const NetName, Channel, Roles: RawUTF8): boolean;
    function AddChannel(const NetName, Channel, ChanKey, Blowkey, Roles: RawUTF8): boolean;
    function DeleteChannel(const NetName, Channel: RawUTF8): boolean;
    function AddNetwork(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;
    function SetNetworkConfig(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;
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
    function TestSpeedLocal(const SiteName: RawUTF8): RawUTF8;
    function TestSpeedOut(const SourceSite: RawUTF8;
                          const DestSites: RawUTF8): RawUTF8;
    function TestSpeedIn(const DestSite: RawUTF8;
                         const SourceSites: RawUTF8): RawUTF8;
    function TestSpeedCleanup(const Sites: RawUTF8): RawUTF8;
    function TestSpeedMatrix(const IncludeSites: RawUTF8 = '';
                             const ExcludeSites: RawUTF8 = ''): RawUTF8;
    function GetSpeedTestSites: RawJSON;
    function GetTestLog(const TestId: RawUTF8): RawJSON;
    function GetTestStatus(const TestId: RawUTF8): RawJSON;
    function AbortSpeedTest(const TestId: RawUTF8): boolean;
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
	    function UpdatePrecatcherRule(RuleId: integer; const RuleData: RawJSON): boolean;
	    function DeletePrecatcherRule(RuleId: integer): boolean;
	    function TestPrecatcher(const Announce: RawUTF8): RawJSON;
	    function ReloadPrecatcher: boolean;
	    function GetMappings: RawJSON;
	    function GetHits(const Limit: integer; const SinceUnix: Int64;
	      const ReleaseName: RawUTF8; const SiteName: RawUTF8): RawJSON;
	    function GetPrecatcherConfig: RawJSON;
	    function ValidatePrecatcherConfig(const Content: RawUTF8): RawJSON;
	    function SavePrecatcherConfig(const Content: RawUTF8; const ExpectedMd5: RawUTF8; Reload: boolean): RawJSON;
	    function GetPrecatcherHelpers: RawJSON;
	  end;

	  { Simulator Service Implementation }
	  TApiSimulatorServiceImpl = class(TInjectableObjectRest, IApiSimulatorService)
	  public
	    function Simulate(const Section, ReleaseName: RawUTF8; const SimulatePre: boolean): RawJSON;
	    function DetectSection(const ReleaseName: RawUTF8): RawJSON;
	  end;

	  { Issues Service Implementation }
	  TApiIssuesServiceImpl = class(TInjectableObjectRest, IApiIssuesService)
	  public
	    function GetSummary(const WindowSeconds: integer; out Response: TApiIssuesSummary): boolean;
	    function GetIssues(const Limit: integer; const SinceUnix: Int64; const TypesCsv: RawUTF8; out Response: TApiIssuesList): boolean;
	    function DeleteIssue(const IssueId: Int64): boolean;
	    function ClearIssues: boolean;
	  end;

	  { Log Service Implementation }
	  TApiLogServiceImpl = class(TInjectableObjectRest, IApiLogService)
	  public
	    function GetLogs(const Lines: integer): RawJSON;
	    function ClearLogs: boolean;
	  end;

  { Browser Service Implementation }
  TApiBrowserServiceImpl = class(TInjectableObjectRest, IApiBrowserService)
  public
    function GetPath(const SiteName: RawUTF8; const Path: RawUTF8; ForceRefresh: boolean): RawJSON;
  end;

  { IMDB Service Implementation }
  TApiImdbServiceImpl = class(TInjectableObjectRest, IApiImdbService)
  public
    function GetAllImdbRecords(out Response: TApiImdbRecordList): boolean;
    function GetImdbRecordById(const ImdbId: RawUTF8; out Response: TApiImdbRecord): boolean;
    function CreateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                              const Genres, Countries, Languages, ImdbType: RawUTF8;
                              out NewId: RawUTF8): boolean;
    function UpdateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                              const Genres, Countries, Languages, ImdbType: RawUTF8): boolean;
    function DeleteImdbRecord(const ImdbId: RawUTF8): boolean;
  end;

  { TV Service Implementation }
  TApiTVServiceImpl = class(TInjectableObjectRest, IApiTVService)
  public
    function GetAllTVRecords(out Response: TApiTVRecordList): boolean;
    function GetTVRecordById(const TVMazeId: RawUTF8; out Response: TApiTVRecord): boolean;
    function CreateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                            Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer;
                            out NewId: RawUTF8): boolean;
    function UpdateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                            Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer): boolean;
    function DeleteTVRecord(const TVMazeId: RawUTF8): boolean;
  end;

  { Config Service Implementation }
  TApiConfigServiceImpl = class(TInjectableObjectRest, IApiConfigService)
  public
    function GetConfigList: RawJSON;
    function GetConfigContent(const Filename: RawUTF8): RawJSON;
    function SaveConfigContent(const Filename, Content: RawUTF8): boolean;
    function ReloadConfig(const Filename: RawUTF8): boolean;
  end;

  TApiHelpServiceImpl = class(TInjectableObjectRest, IApiHelpService)
  public
    function GetHelpDocs: RawJSON;
    function GetHelpDocContent(const Name: RawUTF8): RawJSON;
    function SearchHelpDocs(const Query: RawUTF8): RawJSON;
  end;

implementation

uses
  Contnrs,
  kb.releaseinfo,
  simulator,
  mystrings,
  notify,
  sltcp,
  taskraw,
  SyncObjs,
  IdStack,
  irccommands.irc,
  dirlist.helpers,
  dbaddimdb,
  dbtvinfo,
  dbhandler,
  sllanguagebase,
  skiplists,
  globalskipunit,
  knowngroups,
  loadmonitorunit,
  mrdohutils,
  mormot.orm.core,
  mormot.orm.base,
  mormot.db.raw.sqlite3,
  mormot.rest.sqlite3;

{$I ../slftp.inc}

const
  section = 'slapi.services';
  CGetSiteCreditsTimeoutMs = 30000;
  CGetSiteCreditsCacheSeconds = 3600;
  CGetSiteUserTimeoutMs = 30000;
  CBrowserCacheSeconds = 60; // Cache duration for browser listings

var
  GlApiTaskToPazoId: TDictionary<Int64, Integer>;
  GlApiTaskToPazoIdLock: TSLCriticalSection2;

  TVDatabase: TSQLRestClientDB;
  TVDBModel: TSQLModel;

type
  TSiteCreditsCacheEntry = record
    FetchedAt: TDateTime;
    Ok: boolean;
    Message: RawUTF8;
    Credits: RawUTF8;
    Ratio: RawUTF8;
    StatLine: RawUTF8;
  end;

  TBrowserCacheStatus = (bcsPending, bcsReady, bcsError);

  TBrowserCacheEntry = class
  public
    Status: TBrowserCacheStatus;
    Timestamp: TDateTime;
    Data: RawUTF8; // JSON string of file list
    Error: string;
  end;

  TBrowserDirlistTask = class(TTask)
  private
    FDir: string;
    FCacheKey: string;
  public
    constructor Create(const aSite, aDir, aCacheKey: string);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

var
  glSiteCreditsCacheLock: TSlCriticalSection2;
  glSiteCreditsCache: TDictionary<string, TSiteCreditsCacheEntry>;
  
  glBrowserCacheLock: TSlCriticalSection2;
  glBrowserCache: TObjectDictionary<string, TBrowserCacheEntry>;
  glPrecatcherDebugCaptureLock: TSlCriticalSection2;
  glSystemStatusQueueSizeMax: integer = 0;
  glSystemStatusCpuLoadMax: integer = 0;
  glSystemStatusLoadAvgPeak1: Double = 0;
  glSystemStatusLoadAvgPeak5: Double = 0;
  glSystemStatusLoadAvgPeak15: Double = 0;

{ TBrowserDirlistTask }

constructor TBrowserDirlistTask.Create(const aSite, aDir, aCacheKey: string);
begin
  inherited Create('', '', aSite);
  FDir := aDir;
  FCacheKey := aCacheKey;
end;

function TBrowserDirlistTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  entry: TBrowserCacheEntry;
  parsedList: TObjectList<TParsedDirListEntry>;
  parsedEntry: TParsedDirlistEntry;
  jsonArr: TDocVariantData;
  fileObj: variant;
  i: integer;
begin
  Result := False;
  s := TSiteSlot(slot);
  
  // Try to login if needed
  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      glBrowserCacheLock.Enter('BrowserTask_LoginFail');
      try
        if glBrowserCache.TryGetValue(FCacheKey, entry) then
        begin
          entry.Status := bcsError;
          entry.Error := 'Login failed';
          entry.Timestamp := Now;
        end;
      finally
        glBrowserCacheLock.Leave;
      end;
      readyerror := True;
      Exit;
    end;

  // Execute Dirlist
  // We force CWD to ensure we are in the right directory and get a clean listing
  if not s.Dirlist(FDir, True) then
  begin
    glBrowserCacheLock.Enter('BrowserTask_DirlistFail');
    try
      if glBrowserCache.TryGetValue(FCacheKey, entry) then
      begin
        entry.Status := bcsError;
        entry.Error := Format('Failed to list directory: %s', [s.lastResponse]);
        entry.Timestamp := Now;
      end;
    finally
      glBrowserCacheLock.Leave;
    end;
    readyerror := True;
    Exit;
  end;

  // Parse result
  try
    parsedList := ParseStatResponse(s.lastResponse);
    try
      jsonArr.InitFast(dvArray);
      
      for i := 0 to parsedList.Count - 1 do
      begin
        parsedEntry := parsedList[i];
        
        // Skip current/parent dir dots if they appear (usually filtered but just in case)
        if (parsedEntry.Filename = '.') or (parsedEntry.Filename = '..') then
          Continue;

        TDocVariant.New(fileObj);
        TDocVariantData(fileObj).AddValue('name', UTF8Encode(parsedEntry.Filename));
        TDocVariantData(fileObj).AddValue('size', parsedEntry.Filesize);
        TDocVariantData(fileObj).AddValue('date', UTF8Encode(parsedEntry.Date));
        TDocVariantData(fileObj).AddValue('user', UTF8Encode(parsedEntry.Username));
        TDocVariantData(fileObj).AddValue('group', UTF8Encode(parsedEntry.Groupname));
        TDocVariantData(fileObj).AddValue('perm', UTF8Encode(parsedEntry.DirMask));
        
        // IsDirectory check: usually starts with 'd'
        if (Length(parsedEntry.DirMask) > 0) and (parsedEntry.DirMask[1] = 'd') then
          TDocVariantData(fileObj).AddValue('is_dir', True)
        else
          TDocVariantData(fileObj).AddValue('is_dir', False);

        TDocVariantData(fileObj).AddValue('is_symlink', parsedEntry.IsSymlink);
        if parsedEntry.IsSymlink then
          TDocVariantData(fileObj).AddValue('symlink_target', UTF8Encode(parsedEntry.SymlinkTarget));
          
        jsonArr.AddItem(fileObj);
      end;
      
      // Update Cache
      glBrowserCacheLock.Enter('BrowserTask_Success');
      try
        if glBrowserCache.TryGetValue(FCacheKey, entry) then
        begin
          entry.Status := bcsReady;
          entry.Data := jsonArr.ToJSON;
          entry.Error := '';
          entry.Timestamp := Now;
        end;
      finally
        glBrowserCacheLock.Leave;
      end;
      
      Result := True;
      ready := True;
      
    finally
      parsedList.Free;
    end;
  except
    on E: Exception do
    begin
      glBrowserCacheLock.Enter('BrowserTask_Exception');
      try
        if glBrowserCache.TryGetValue(FCacheKey, entry) then
        begin
          entry.Status := bcsError;
          entry.Error := Format('Exception parsing dirlist: %s', [E.Message]);
          entry.Timestamp := Now;
        end;
      finally
        glBrowserCacheLock.Leave;
      end;
      readyerror := True;
    end;
  end;
end;

function TBrowserDirlistTask.Name: String;
begin
  Result := Format('BROWSER: %s @ %s', [FDir, site1]);
end;

{ TApiBrowserServiceImpl }

function TApiBrowserServiceImpl.GetPath(const SiteName: RawUTF8; const Path: RawUTF8; ForceRefresh: boolean): RawJSON;
var
  s: TSite;
  fPath: string;
  fSiteName: string;
  cacheKey: string;
  entry: TBrowserCacheEntry;
  needsFetch: boolean;
  resultDoc: variant;
  task: TBrowserDirlistTask;
begin
  Result := '{}';
  try
    fSiteName := UTF8ToString(SiteName);
    fPath := UTF8ToString(Path);
    
    // Normalize path
    if fPath = '' then fPath := '/';
    // ensure leading slash
    if fPath[1] <> '/' then fPath := '/' + fPath;
    // remove trailing slash if not root
    if (Length(fPath) > 1) and (fPath[Length(fPath)] = '/') then
      Delete(fPath, Length(fPath), 1);
      
    cacheKey := UpperCase(fSiteName) + '|' + fPath;
    needsFetch := False;
    
    // Check Cache
    glBrowserCacheLock.Enter('GetPath_CheckCache');
    try
      if glBrowserCache.TryGetValue(cacheKey, entry) then
      begin
        if ForceRefresh then
          needsFetch := True
        else if (entry.Status = bcsReady) and (SecondsBetween(Now, entry.Timestamp) > CBrowserCacheSeconds) then
          needsFetch := True
        else if (entry.Status = bcsError) and (SecondsBetween(Now, entry.Timestamp) > 10) then // Retry errors after 10s
          needsFetch := True;
      end
      else
      begin
        // New entry
        entry := TBrowserCacheEntry.Create;
        entry.Status := bcsPending;
        entry.Timestamp := Now;
        glBrowserCache.Add(cacheKey, entry);
        needsFetch := True;
      end;
      
      // If we are pending but not needsFetch (meaning it's already pending from a recent request), just return pending
      if (entry.Status = bcsPending) and (not needsFetch) then
      begin
        // If it's been pending for too long (> 30s), treat as timeout/needs refetch
        if SecondsBetween(Now, entry.Timestamp) > 30 then
          needsFetch := True;
      end;
      
      // Update entry if we are fetching
      if needsFetch then
      begin
        entry.Status := bcsPending;
        entry.Timestamp := Now;
        entry.Error := '';
      end;
      
    finally
      glBrowserCacheLock.Leave;
    end;

    // Trigger Task if needed
    if needsFetch then
    begin
      s := FindSiteByName('', fSiteName);
      if s = nil then
      begin
        TDocVariant.New(resultDoc);
        TDocVariantData(resultDoc).AddValue('status', 'error');
        TDocVariantData(resultDoc).AddValue('message', UTF8Encode('Site not found'));
        Result := VariantSaveJSON(resultDoc);
        
        // Update cache to error
        glBrowserCacheLock.Enter('GetPath_SiteError');
        try
          if glBrowserCache.TryGetValue(cacheKey, entry) then
          begin
            entry.Status := bcsError;
            entry.Error := 'Site not found';
          end;
        finally
          glBrowserCacheLock.Leave;
        end;
        Exit;
      end;
      
      task := TBrowserDirlistTask.Create(fSiteName, fPath, cacheKey);
      // Give it high priority? TBrowserDirlistTask inherits from TTask. 
      // AddTask puts it in the queue.
      // We rely on queue order.
      s.AddTask(task, True); // True = Fire queue immediately
    end;

    // Build Response based on current state (even if we just queued it)
    glBrowserCacheLock.Enter('GetPath_BuildResponse');
    try
      if glBrowserCache.TryGetValue(cacheKey, entry) then
      begin
        TDocVariant.New(resultDoc);
        
        case entry.Status of
          bcsPending:
          begin
            TDocVariantData(resultDoc).AddValue('status', 'pending');
          end;
          bcsReady:
          begin
            TDocVariantData(resultDoc).AddValue('status', 'ready');
            TDocVariantData(resultDoc).AddValue('files', _JsonFast(entry.Data));
            TDocVariantData(resultDoc).AddValue('path', UTF8Encode(fPath));
            TDocVariantData(resultDoc).AddValue('timestamp', DateTimeToUnix(entry.Timestamp));
          end;
          bcsError:
          begin
            TDocVariantData(resultDoc).AddValue('status', 'error');
            TDocVariantData(resultDoc).AddValue('message', UTF8Encode(entry.Error));
          end;
        end;
        
        Result := VariantSaveJSON(resultDoc);
      end;
    finally
      glBrowserCacheLock.Leave;
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetPath: %s', [E.Message]));
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('status', 'error');
      TDocVariantData(resultDoc).AddValue('message', UTF8Encode(E.Message));
      Result := VariantSaveJSON(resultDoc);
    end;
  end;
end;

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

function TApiIssuesServiceImpl.DeleteIssue(const IssueId: Int64): boolean;
begin
  Result := False;
  try
    IssuesStore.DeleteIssue(IssueId);
    Result := True;
  except
    on E: Exception do
      Debug(dpError, section, Format('[EXCEPTION] DeleteIssue: %s', [E.Message]));
  end;
end;

function TApiIssuesServiceImpl.ClearIssues: boolean;
begin
  IssuesStore.Clear;
  Result := True;
end;

{ System status peak tracking }

function TryGetLoadAverage(out aAvg1, aAvg5, aAvg15: Double): boolean;
var
  fLoadText: string;
  fParts: TStringList;
  fFormatSettings: TFormatSettings;
begin
  Result := False;
  aAvg1 := 0;
  aAvg5 := 0;
  aAvg15 := 0;

  fLoadText := Trim(String(RetrieveLoadAvg));
  if fLoadText = '' then
    Exit;

  // Windows returns system/user/kernel CPU text, not POSIX load average triplet.
  if (Pos('U:', UpperCase(fLoadText)) > 0) or (Pos('K:', UpperCase(fLoadText)) > 0) then
    Exit;

  fParts := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(fLoadText), fParts);
    if fParts.Count < 3 then
      Exit;

    fFormatSettings := DefaultFormatSettings;
    fFormatSettings.DecimalSeparator := '.';
    if not TryStrToFloat(fParts[0], aAvg1, fFormatSettings) then
      Exit;
    if not TryStrToFloat(fParts[1], aAvg5, fFormatSettings) then
      Exit;
    if not TryStrToFloat(fParts[2], aAvg15, fFormatSettings) then
      Exit;

    Result := True;
  finally
    fParts.Free;
  end;
end;

procedure UpdateSystemStatusPeaks;
var
  avg1, avg5, avg15: Double;
  qTotal, qRace, qDir, qAuto, qOther: integer;
  cpuLoad: integer;
begin
  // Load average peaks
  if TryGetLoadAverage(avg1, avg5, avg15) then
  begin
    if avg1 > glSystemStatusLoadAvgPeak1 then
      glSystemStatusLoadAvgPeak1 := avg1;
    if avg5 > glSystemStatusLoadAvgPeak5 then
      glSystemStatusLoadAvgPeak5 := avg5;
    if avg15 > glSystemStatusLoadAvgPeak15 then
      glSystemStatusLoadAvgPeak15 := avg15;
  end;

  // Queue size peak
  GetQueueTotals(qTotal, qRace, qDir, qAuto, qOther);
  if qTotal > glSystemStatusQueueSizeMax then
    glSystemStatusQueueSizeMax := qTotal;

  // CPU load peak
  if IsLoadMonitorAvailable then
  begin
    cpuLoad := GlLoadMonitor.CurrentCPUUsageTotal;
    if cpuLoad > glSystemStatusCpuLoadMax then
      glSystemStatusCpuLoadMax := cpuLoad;
  end;
end;

{ TApiSystemServiceImpl }

function TApiSystemServiceImpl.GetStatus(out Response: TApiSystemStatus): boolean;
var
  i: integer;
  s: TSite;
  upCount, downCount, siteCount: integer;
  qTotal, qRace, qDir, qAuto, qOther: integer;
  activeSum: integer;
  cpuLoadAvailable: boolean;
  currentLoadAvg1, currentLoadAvg5, currentLoadAvg15: Double;
  loadAvgAvailable: boolean;
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
    siteCount := 0;
    activeSum := 0;

    if sitesunit.sites <> nil then
    begin
      for i := 0 to sitesunit.sites.Count - 1 do
      begin
        s := TSite(sitesunit.sites[i]);
        if s = nil then
          Continue;

        // Skip admin site (SLFTP) in count
        if s.Name = sitesunit.getAdminSiteName then
          Continue;

        Inc(siteCount);

        if s.WorkingStatus = sstUp then
          Inc(upCount)
        else if s.PermDown then
          Inc(downCount)
        else if (s.WorkingStatus = sstDown) or (s.WorkingStatus = sstMarkedAsDownByUser) then
          Inc(downCount);

        // Sum current active transfers (download+upload)
        activeSum := activeSum + s.num_dn + s.num_up;
      end;
    end;

    Response.SitesCount := siteCount;

    Response.SitesUp := upCount;
    Response.SitesDown := downCount;

    // Gather queue stats snapshot
    QueueStatAll;
    GetQueueTotals(qTotal, qRace, qDir, qAuto, qOther);
    Response.QueueSize := qTotal;
    Response.QueueSizeMax := glSystemStatusQueueSizeMax;
    // Treat all queued tasks as active for dashboard purposes; transfers also counted via activeSum
    Response.ActiveTasks := qTotal;

    loadAvgAvailable := TryGetLoadAverage(currentLoadAvg1, currentLoadAvg5, currentLoadAvg15);
    Response.LoadAvgAvailable := loadAvgAvailable;
    if loadAvgAvailable then
    begin
      Response.LoadAvgCurrent1 := currentLoadAvg1;
      Response.LoadAvgCurrent5 := currentLoadAvg5;
      Response.LoadAvgCurrent15 := currentLoadAvg15;
      Response.LoadAvgPeak1 := glSystemStatusLoadAvgPeak1;
      Response.LoadAvgPeak5 := glSystemStatusLoadAvgPeak5;
      Response.LoadAvgPeak15 := glSystemStatusLoadAvgPeak15;
    end
    else
    begin
      Response.LoadAvgCurrent1 := 0;
      Response.LoadAvgCurrent5 := 0;
      Response.LoadAvgCurrent15 := 0;
      Response.LoadAvgPeak1 := 0;
      Response.LoadAvgPeak5 := 0;
      Response.LoadAvgPeak15 := 0;
    end;

    cpuLoadAvailable := IsLoadMonitorAvailable;
    Response.CpuLoadAvailable := cpuLoadAvailable;
    if cpuLoadAvailable then
    begin
      Response.CpuLoadCurrent := GlLoadMonitor.CurrentCPUUsageTotal;
      Response.CpuLoadMax := glSystemStatusCpuLoadMax;
      Response.PerformanceLevel := GlLoadMonitor.CurrentPerformanceLevel;
    end
    else
    begin
      Response.CpuLoadCurrent := 0;
      Response.CpuLoadMax := 0;
      Response.PerformanceLevel := 0;
    end;

    // Use global rate calculated by QueueThread
    Response.DirlistPerSecond := GlDirlistRate;
    Response.DirlistPerSecondMax := GlDirlistRateMax;

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
  expectedSitesArray: TDocVariantData;
  kbList: TStringList;
  kbLock: TSlCriticalSection2;
  totalSites, allowedSites, presentSites, expectedSites, notAllowedSites: Integer;
  isNotAllowed: Boolean;
  isPresent: Boolean;
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

    if (kbList = nil) or (kbLock = nil) then
    begin
      Debug(dpError, section, 'GetRecentReleases: KB not initialized');
      Response.Releases := '[]';
      Result := True;
      Exit;
    end;

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

        // Skip if essential properties are not available
        try
          if (p.rls = nil) or (p.PazoSitesList = nil) then
            Continue;
        except
          Continue;
        end;

        // Build release info
        try
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
            try
              for ps in p.PazoSitesList do
              begin
                if ps = nil then
                  Continue;
                try
                  if not (ps.status in [rssNotAllowed, rssComplete, rssRealPre]) then
                  begin
                    releaseJson.Ready := False; // At least one site is not complete
                    Break;
                  end;
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('GetRecentReleases: Error checking site status for Pazo %d: %s',
                      [p.pazo_id, E.Message]));
                  end;
                end;
              end;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('GetRecentReleases: Error iterating sites for Ready check, Pazo %d: %s',
                  [p.pazo_id, E.Message]));
                releaseJson.Ready := False;
              end;
            end;
            releaseJson.Stopped := False;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('GetRecentReleases: Error building basic info for Pazo %d: %s',
              [p.pazo_id, E.Message]));
            Continue;
          end;
        end;

        try
          releaseJson.QueueNumber := p.queuenumber.Value;

          // Collect site names and compute basic status counts
          totalSites := 0;
          allowedSites := 0;
          presentSites := 0;
          expectedSites := 0;
          notAllowedSites := 0;

          sitesArray.Init(JSON_FAST, dvArray);
          expectedSitesArray.Init(JSON_FAST, dvArray);

          try
            for ps in p.PazoSitesList do
            begin
              if ps = nil then
                Continue;

              try
                if ps.Name <> '' then
                begin
                  sitesArray.AddItem(UTF8Encode(ps.Name));
                end;

                Inc(totalSites);

                isNotAllowed := ps.status in [rssNotAllowed, rssNotAllowedButItsThere];
                if isNotAllowed then
                  Inc(notAllowedSites)
                else
                  Inc(allowedSites);

                isPresent := False;
                try
                  if (ps.dirlist <> nil) then
                    isPresent := (ps.dirlist.entries.Count > 0) or ps.dirlist.Complete;
                except
                  isPresent := False;
                end;
                if not isPresent then
                  isPresent := ps.status in [rssRealPre, rssComplete, rssNotAllowedButItsThere];

                if isPresent then
                  Inc(presentSites);

                if isPresent and (not isNotAllowed) then
                begin
                  Inc(expectedSites);
                  if ps.Name <> '' then
                    expectedSitesArray.AddItem(UTF8Encode(ps.Name));
                end;
              except
                on E: Exception do
                begin
                  Debug(dpError, section, Format('GetRecentReleases: Error processing site info for Pazo %d: %s',
                    [p.pazo_id, E.Message]));
                end;
              end;
            end;
          except
            on E: Exception do
            begin
              Debug(dpError, section, Format('GetRecentReleases: Error iterating sites for site info, Pazo %d: %s',
                [p.pazo_id, E.Message]));
            end;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('GetRecentReleases: Error collecting site info for Pazo %d: %s',
              [p.pazo_id, E.Message]));
            Continue;
          end;
        end;
        releaseJson.Sites := variant(sitesArray);
        releaseJson.TotalSites := totalSites;
        releaseJson.AllowedSites := allowedSites;
        releaseJson.PresentSites := presentSites;
        releaseJson.ExpectedSites := expectedSites;
        releaseJson.ExpectedSitesList := variant(expectedSitesArray);
        releaseJson.NotAllowedSites := notAllowedSites;

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
      Debug(dpError, section, Format('GetReleaseDetails: Pazo with ID %d not found', [PazoId]));
      Exit;
    end;

    // Check essential properties
    try
      if p.rls = nil then
      begin
        Debug(dpError, section, Format('GetReleaseDetails: Pazo %d has nil rls property', [PazoId]));
        Exit;
      end;
      if p.PazoSitesList = nil then
      begin
        Debug(dpError, section, Format('GetReleaseDetails: Pazo %d has nil PazoSitesList property', [PazoId]));
        Exit;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('GetReleaseDetails: Error checking Pazo %d properties: %s', [PazoId, E.Message]));
        Exit;
      end;
    end;

    // Basic info
    try
      Response.ReleaseName := UTF8Encode(p.rls.rlsname);
      Response.Section := UTF8Encode(p.rls.section);
      Response.Added := p.added;
      Response.PazoId := p.pazo_id;
      Response.Ready := p.ready;
      Response.Stopped := p.stopped;
      Response.QueueNumber := p.queuenumber.Value;
      Response.ErrorReason := UTF8Encode(p.errorreason);
      Response.TotalFiles := p.GetCountOfCachedFiles;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('GetReleaseDetails: Error reading basic info for Pazo %d: %s', [PazoId, E.Message]));
        Exit;
      end;
    end;

    // Collect site details
    siteDetailsArray.Init(JSON_FAST, dvArray);

    try
      for ps in p.PazoSitesList do
      begin
        if ps = nil then
        begin
          Debug(dpError, section, Format('GetReleaseDetails: Pazo %d has nil PazoSite in list', [PazoId]));
          Continue;
        end;

        try
          TDocVariant.New(siteDetail);
          siteDetail.SiteName := UTF8Encode(ps.Name);

          // Safe dirlist access
          try
            if ps.dirlist <> nil then
            begin
              siteDetail.Complete := ps.dirlist.Complete;
              siteDetail.FileCount := ps.dirlist.entries.Count;
              siteDetail.FilesRacedByMe := ps.dirlist.FilesRacedByMe(True);
              siteDetail.StartedTime := DateTimeToUnixMSTime(ps.dirlist.StartedTime);
              siteDetail.CompletedTime := DateTimeToUnixMSTime(ps.dirlist.CompletedTime);

              // Calculate percent
              totalFiles := p.GetCountOfCachedFiles;
              if totalFiles > 0 then
                siteDetail.Percent := (ps.dirlist.entries.Count / totalFiles) * 100.0
              else
                siteDetail.Percent := 0.0;
            end
            else
            begin
              siteDetail.Complete := False;
              siteDetail.FileCount := 0;
              siteDetail.FilesRacedByMe := 0;
              siteDetail.Percent := 0.0;
              siteDetail.StartedTime := 0;
              siteDetail.CompletedTime := 0;
            end;
          except
            on E: Exception do
            begin
              Debug(dpError, section, Format('GetReleaseDetails: Error accessing dirlist for site %s in Pazo %d: %s',
                [ps.Name, PazoId, E.Message]));
              siteDetail.Complete := False;
              siteDetail.FileCount := 0;
              siteDetail.FilesRacedByMe := 0;
              siteDetail.Percent := 0.0;
              siteDetail.StartedTime := 0;
              siteDetail.CompletedTime := 0;
            end;
          end;

          siteDetail.TotalFiles := p.GetCountOfCachedFiles;

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
        except
          on E: Exception do
          begin
            if ps <> nil then
              Debug(dpError, section, Format('GetReleaseDetails: Error processing site %s in Pazo %d: %s',
                [ps.Name, PazoId, E.Message]))
            else
              Debug(dpError, section, Format('GetReleaseDetails: Error processing nil site in Pazo %d: %s',
                [PazoId, E.Message]));
            // Skip this site but continue with others
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('GetReleaseDetails: Error iterating PazoSitesList for Pazo %d: %s', [PazoId, E.Message]));
        // Continue with empty site details
      end;
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
    DirlistPriority: integer;
    NewdirDirlistReadd: integer;
    PerformanceAdjustedDirlist: boolean;
    DestinationQueueLimit: integer;
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

        snapshots[snapshotCount].DirlistPriority := 2; // spNormal
        try
          snapshots[snapshotCount].DirlistPriority := s.DirlistPriority;
        except
          // ignore
        end;

        snapshots[snapshotCount].NewdirDirlistReadd := 0;
        try
          snapshots[snapshotCount].NewdirDirlistReadd := s.NewdirDirlistReadd;
        except
          // ignore
        end;

        snapshots[snapshotCount].PerformanceAdjustedDirlist := False;
        try
          snapshots[snapshotCount].PerformanceAdjustedDirlist := s.PerformanceAdjustedDirlist;
        except
          // ignore
        end;

        snapshots[snapshotCount].DestinationQueueLimit := 0;
        try
          snapshots[snapshotCount].DestinationQueueLimit := s.RCInteger('destination_queue_limit', 0);
          if snapshots[snapshotCount].DestinationQueueLimit < 0 then
            snapshots[snapshotCount].DestinationQueueLimit := 0;
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
        TDocVariantData(siteDoc).AddValue('dirlist_priority', snapshot.DirlistPriority);
        TDocVariantData(siteDoc).AddValue('newdir_dirlist_readd', snapshot.NewdirDirlistReadd);
        TDocVariantData(siteDoc).AddValue('performance_adjusted_dirlist', snapshot.PerformanceAdjustedDirlist);
        TDocVariantData(siteDoc).AddValue('destination_queue_limit', snapshot.DestinationQueueLimit);
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
    Info.SslMethod := integer(s.sslmethod);

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSite: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function ApiGetSlotsRuntimeJson(const SiteName: RawUTF8): RawJSON;
var
  sitesArray: TDocVariantData;
  siteDoc, slotsDoc, slotDoc: variant;
  filterSite: string;
  i, j: integer;
  s: TSite;
  ss: TSiteSlot;
  task: TTask;
  taskName: string;
  taskUid: Int64;
  nowTs: TDateTime;
  lockAcquired: boolean;
  totalSlots: integer;
  freeSlots: integer;
  activeSlots: integer;
  siteStatus: RawUTF8;

  function AgeSeconds(const aWhen, aNow: TDateTime): integer;
  var
    delta: Int64;
  begin
    Result := -1;
    if aWhen <= 0 then
      Exit;
    try
      delta := SecondsBetween(aNow, aWhen);
      if delta < 0 then
        delta := 0;
      if delta > High(Integer) then
        delta := High(Integer);
      Result := integer(delta);
    except
      Result := -1;
    end;
  end;

  function DirectionText(const aUploading, aDownloading: boolean): RawUTF8;
  begin
    if aUploading and aDownloading then
      Result := 'up+down'
    else if aUploading then
      Result := 'up'
    else if aDownloading then
      Result := 'down'
    else
      Result := 'idle';
  end;

begin
  Result := '[]';
  sitesArray.InitFast(dvArray);
  filterSite := Trim(UTF8ToString(SiteName));
  nowTs := Now;

  try
    if sitesunit.sites = nil then
    begin
      Result := sitesArray.ToJSON;
      Exit;
    end;

    for i := 0 to sitesunit.sites.Count - 1 do
    begin
      s := TSite(sitesunit.sites[i]);
      if s = nil then
        Continue;
      if s.Name = sitesunit.getAdminSiteName then
        Continue;
      if (filterSite <> '') and (filterSite <> '*') and (CompareText(s.Name, filterSite) <> 0) then
        Continue;

      case s.WorkingStatus of
        sstUp: siteStatus := 'UP';
        sstDown, sstTempDown: siteStatus := 'DOWN';
        sstMarkedAsDownByUser: siteStatus := 'DOWN_BY_USER';
      else
        siteStatus := 'UNKNOWN';
      end;

      totalSlots := 0;
      freeSlots := -1;
      activeSlots := 0;
      TDocVariant.New(slotsDoc);
      TDocVariantData(slotsDoc).InitFast(dvArray);
      lockAcquired := s.AcquireSlotsAssignmentLock(150, 'GetSlotsRuntime');
      try
        if lockAcquired then
        begin
          if s.slots <> nil then
            totalSlots := s.slots.Count;
          freeSlots := s.freeslots;

          for j := 0 to totalSlots - 1 do
          begin
            ss := TSiteSlot(s.slots[j]);
            if ss = nil then
              Continue;

            TDocVariant.New(slotDoc);
            TDocVariantData(slotDoc).AddValue('slot', ss.SlotNumber);
            TDocVariantData(slotDoc).AddValue('name', UTF8Encode(ss.Name));
            TDocVariantData(slotDoc).AddValue('status', UTF8Encode(SlotStatusToString(ss.Status)));

            task := ss.todotask;
            taskName := 'Idle';
            taskUid := 0;
            if task <> nil then
            begin
              taskName := 'Task';
              try
                taskName := task.Name;
              except
                // keep fallback task name
              end;
              if taskName = '' then
                taskName := 'Task';
              try
                taskUid := task.uid;
              except
                taskUid := 0;
              end;
              Inc(activeSlots);
            end;

            TDocVariantData(slotDoc).AddValue('task', UTF8Encode(taskName));
            if taskUid > 0 then
              TDocVariantData(slotDoc).AddValue('task_uid', taskUid);
            TDocVariantData(slotDoc).AddValue('action', UTF8Encode(ss.CurrentAction));
            TDocVariantData(slotDoc).AddValue('uploading', ss.uploadingto);
            TDocVariantData(slotDoc).AddValue('downloading', ss.downloadingfrom);
            TDocVariantData(slotDoc).AddValue('direction', DirectionText(ss.uploadingto, ss.downloadingfrom));
            TDocVariantData(slotDoc).AddValue('last_io_sec', AgeSeconds(ss.LastIO, nowTs));
            TDocVariantData(slotDoc).AddValue('last_task_sec', AgeSeconds(ss.LastTaskExecution, nowTs));
            TDocVariantData(slotDoc).AddValue('last_non_idle_task_sec', AgeSeconds(ss.LastNonIdleTaskExecution, nowTs));
            TDocVariantData(slotDoc).AddValue('response_code', ss.lastResponseCode);
            TDocVariantData(slotsDoc).AddItem(slotDoc);
          end;
        end;
      finally
        if lockAcquired then
          s.ReleaseSlotsAssignmentLock;
      end;

      TDocVariant.New(siteDoc);
      TDocVariantData(siteDoc).AddValue('site', UTF8Encode(s.Name));
      TDocVariantData(siteDoc).AddValue('site_status', siteStatus);
      TDocVariantData(siteDoc).AddValue('locked', not lockAcquired);
      TDocVariantData(siteDoc).AddValue('slots_total', totalSlots);
      TDocVariantData(siteDoc).AddValue('slots_free', freeSlots);
      TDocVariantData(siteDoc).AddValue('active_slots', activeSlots);
      TDocVariantData(siteDoc).AddValue('slots', slotsDoc);
      sitesArray.AddItem(siteDoc);
    end;

    Result := sitesArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSlotsRuntime: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiSitesServiceImpl.GetSlotsRuntime(const SiteName: RawUTF8): RawJSON;
begin
  Result := ApiGetSlotsRuntimeJson(SiteName);
end;

function ApiGetSlotHistorySSE(const aSiteName: string; aSlotNumber: integer; aSeq: QWord; aTimeoutMs: integer): RawUTF8;
var
  doc: TDocVariantData;
  linesArr: TDocVariantData;
  s: TSite;
  ss: TSiteSlot;
  lines: TArray<String>;
  currentSeq: QWord;
  i: integer;
  lockAcquired: boolean;
  startTick: QWord;
  changed: boolean;
  clampedTimeout: integer;
  dataJson: RawUTF8;
begin
  Result := 'retry: 250'#10'event: ping'#10'data: {}'#10#10;

  clampedTimeout := aTimeoutMs;
  if clampedTimeout < 500 then
    clampedTimeout := 500;
  if clampedTimeout > 15000 then
    clampedTimeout := 15000;

  try
    s := sitesunit.FindSiteByName('', aSiteName);
    if s = nil then
      Exit;

    // Resolve the slot once (outside the poll loop)
    lockAcquired := s.AcquireSlotsAssignmentLock(150, 'GetSlotHistory');
    try
      if not lockAcquired then
        Exit;
      if (s.slots = nil) or (aSlotNumber < 0) or (aSlotNumber >= s.slots.Count) then
        Exit;
      ss := TSiteSlot(s.slots[aSlotNumber]);
    finally
      if lockAcquired then
        s.ReleaseSlotsAssignmentLock;
    end;

    if ss = nil then
      Exit;

    // Long-poll: wait until seq changes or timeout
    startTick := GetTickCount64;
    changed := False;
    repeat
      currentSeq := ss.HistorySeq;
      if (aSeq = 0) or (currentSeq <> aSeq) then
      begin
        changed := True;
        Break;
      end;
      Sleep(100);
    until (GetTickCount64 - startTick) >= QWord(clampedTimeout);

    if not changed then
    begin
      Result := 'retry: 250'#10 +
        'event: ping'#10 +
        'data: {"seq":' + UTF8Encode(IntToStr(currentSeq)) + '}'#10#10;
      Exit;
    end;

    // Fetch history snapshot
    lines := ss.GetHistory;
    currentSeq := ss.HistorySeq;

    doc.InitFast(dvObject);
    linesArr.InitFast(dvArray);

    if (aSeq = 0) then
    begin
      // Initial load: send full buffer
      for i := 0 to High(lines) do
        linesArr.AddItem(UTF8Encode(lines[i]));
    end
    else
    begin
      // Delta: only send new lines since last seq
      // newCount = how many lines were added since aSeq
      // but cap to available lines in buffer
      i := High(lines) - integer(currentSeq - aSeq) + 1;
      if i < 0 then
        i := 0;
      while i <= High(lines) do
      begin
        linesArr.AddItem(UTF8Encode(lines[i]));
        Inc(i);
      end;
    end;

    doc.AddValue('seq', Int64(currentSeq));
    doc.AddValue('full', aSeq = 0);
    doc.AddValue('lines', Variant(linesArr));
    dataJson := doc.ToJSON;

    Result := 'retry: 250'#10 +
      'event: history'#10 +
      'data: ' + dataJson + #10#10;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ApiGetSlotHistorySSE: %s', [E.Message]));
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteCredits(const SiteName: RawUTF8; ForceRefresh: boolean; out Credits: TApiSiteCredits): boolean;
var
  s: TSite;
  tn: TTaskNotify;
  r: TRawTask;
  waitRes: TWaitResult;
  fCredits, fRatio: String;
  statLine: String;
  cacheKey: string;
  cacheEntry: TSiteCreditsCacheEntry;
begin
  Result := False;
  Credits := TApiSiteCredits.Create;
  Credits.SiteName := SiteName;
  Credits.Ok := False;
  Credits.Message := '';
  Credits.Credits := '';
  Credits.Ratio := '';
  Credits.StatLine := '';

  try
    cacheKey := UpperCase(UTF8ToString(SiteName));
    if (not ForceRefresh) and (glSiteCreditsCache <> nil) then
    begin
      glSiteCreditsCacheLock.Enter('GetSiteCredits(cache)');
      try
        if glSiteCreditsCache.TryGetValue(cacheKey, cacheEntry) then
        begin
          if (cacheEntry.FetchedAt > 0) and (SecondsBetween(Now, cacheEntry.FetchedAt) < CGetSiteCreditsCacheSeconds) then
          begin
            Credits.Ok := cacheEntry.Ok;
            Credits.Message := cacheEntry.Message;
            Credits.Credits := cacheEntry.Credits;
            Credits.Ratio := cacheEntry.Ratio;
            Credits.StatLine := cacheEntry.StatLine;
            Result := True;
            Exit;
          end;
        end;
      finally
        glSiteCreditsCacheLock.Leave;
      end;
    end;

    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
    begin
      Credits.Message := 'Site not found';
      Result := True;
      Exit;
    end;

    if s.PermDown then
    begin
      Credits.Message := 'Site is permdown';
      Result := True;
      Exit;
    end;

    if not s.IsUp then
    begin
      Credits.Message := 'Site is offline';
      Result := True;
      Exit;
    end;

    tn := AddNotify;
    try
      r := TRawTask.Create('API', '', s.Name, '', 'SITE STAT');
      tn.AddTask(r);
      AddTask(r, True);

      waitRes := tn.event.WaitFor(CGetSiteCreditsTimeoutMs);
      if waitRes <> wrSignaled then
      begin
        Credits.Message := 'Timed out waiting for SITE STAT';
        Result := True;
        Exit;
      end;

      if (tn.responses = nil) or (tn.responses.Count = 0) then
      begin
        Credits.Message := 'No SITE STAT response received';
        Result := True;
        Exit;
      end;

      statLine := TSiteResponse(tn.responses[0]).response;
      Credits.StatLine := UTF8Encode(statLine);

      fCredits := '';
      fRatio := '';
      ParseSTATLine(statLine, fCredits, fRatio);

      Credits.Credits := UTF8Encode(fCredits);
      Credits.Ratio := UTF8Encode(fRatio);
      Credits.Ok := (Trim(fCredits) <> '') or (Trim(fRatio) <> '');
      if not Credits.Ok then
        Credits.Message := 'Failed to parse credits/ratio from SITE STAT';

      if glSiteCreditsCache <> nil then
      begin
        cacheEntry.FetchedAt := Now;
        cacheEntry.Ok := Credits.Ok;
        cacheEntry.Message := Credits.Message;
        cacheEntry.Credits := Credits.Credits;
        cacheEntry.Ratio := Credits.Ratio;
        cacheEntry.StatLine := Credits.StatLine;
        glSiteCreditsCacheLock.Enter('GetSiteCredits(store)');
        try
          glSiteCreditsCache.AddOrSetValue(cacheKey, cacheEntry);
        finally
          glSiteCreditsCacheLock.Leave;
        end;
      end;

      Result := True;
    finally
      RemoveTN(tn);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteCredits: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiSitesServiceImpl.GetSiteUser(const SiteName, UserName: RawUTF8; out Info: TApiSiteUserInfo): boolean;
var
  fSite: TSite;
  fNotify: TTaskNotify;
  fTask: TRawTask;
  fWaitRes: TWaitResult;
  fOutput: string;
  fResponse: string;
  fUserName: string;
  fSiteNameStr: string;
  fIndex: integer;
begin
  Result := False;
  Info := TApiSiteUserInfo.Create;
  Info.SiteName := SiteName;
  Info.UserName := UserName;
  Info.Ok := False;
  Info.Message := '';
  Info.Output := '';

  try
    fSiteNameStr := UTF8ToString(SiteName);
    fSite := FindSiteByName('', fSiteNameStr);
    if fSite = nil then
    begin
      Info.Message := 'Site not found';
      Result := True;
      Exit;
    end;

    if fSite.PermDown then
    begin
      Info.Message := 'Site is permdown';
      Result := True;
      Exit;
    end;

    if not fSite.IsUp then
    begin
      Info.Message := 'Site is offline';
      Result := True;
      Exit;
    end;

    fUserName := UTF8ToString(UserName);
    if fUserName = '' then
      fUserName := fSite.username;

    if fUserName = '' then
    begin
      Info.Message := 'No username configured';
      Result := True;
      Exit;
    end;

    Info.UserName := UTF8Encode(fUserName);

    fNotify := AddNotify;
    try
      fTask := TRawTask.Create('API', '', fSite.Name, '', 'SITE USER ' + fUserName);
      fNotify.AddTask(fTask);
      AddTask(fTask, True);

      fWaitRes := fNotify.event.WaitFor(CGetSiteUserTimeoutMs);
      if fWaitRes <> wrSignaled then
      begin
        Info.Message := 'Timed out waiting for SITE USER';
        Result := True;
        Exit;
      end;

      if (fNotify.responses = nil) or (fNotify.responses.Count = 0) then
      begin
        Info.Message := 'No SITE USER response received';
        Result := True;
        Exit;
      end;

      fOutput := '';
      for fIndex := 0 to fNotify.responses.Count - 1 do
      begin
        fResponse := TSiteResponse(fNotify.responses[fIndex]).response;
        if fResponse = '' then
          Continue;
        if fOutput <> '' then
          fOutput := fOutput + slEOL;
        fOutput := fOutput + fResponse;
      end;

      if fOutput = '' then
      begin
        Info.Message := 'Empty SITE USER response';
        Result := True;
        Exit;
      end;

      if (Pos('You do not have access', fOutput) <> 0) or (Pos('Access denied', fOutput) <> 0) then
      begin
        Info.Message := 'Access denied for SITE USER';
        Info.Output := UTF8Encode(fOutput);
        Result := True;
        Exit;
      end;

      if Pos('does not exist', fOutput) <> 0 then
      begin
        Info.Message := 'User does not exist';
        Info.Output := UTF8Encode(fOutput);
        Result := True;
        Exit;
      end;

      Info.Output := UTF8Encode(fOutput);
      Info.Ok := True;
      Result := True;
    finally
      RemoveTN(fNotify);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSiteUser: %s', [E.Message]));
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
  sname: string;
  i: integer;
begin
  Result := False;
  sname := UTF8ToString(SiteName);

  try
    s := FindSiteByName('', sname);
    if s = nil then
      Exit;

    Debug(dpMessage, section, Format('DeleteSite API: %s', [sname]));

    s.Stop;

    // Cleanup sites.dat - same logic as IRC IrcDelsite command
    try
      // Erase speed-from and speed-to sections
      sitesdat.EraseSection('speed-from-' + sname);
      sitesdat.EraseSection('speed-to-' + sname);

      // Remove this site from other sites' speed routes
      for i := 0 to sites.Count - 1 do
      begin
        sitesdat.DeleteKey('speed-from-' + TSite(sites.Items[i]).Name, sname);
        sitesdat.DeleteKey('speed-to-' + TSite(sites.Items[i]).Name, sname);
      end;
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - remove routes failed: %s', [E.Message]));
    end;

    try
      RulesRemove(sname, '');
      RulesSave;
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - rules remove failed: %s', [E.Message]));
    end;

    try
      RemoveRanks(sname);
      RanksSave;
      RanksReload;
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - ranks remove failed: %s', [E.Message]));
    end;

    try
      Precatcher_DelSiteChans(sname);
      PrecatcherRebuild;
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - catches remove failed: %s', [E.Message]));
    end;

    if not RemoveStats(sname) then
      Debug(dpError, section, Format('DeleteSite - stats remove failed for %s', [sname]));

    try
      sitesdat.EraseSection('site-' + sname);
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - erase site section failed: %s', [E.Message]));
    end;

    try
      sitesunit.DeleteSite(s);
    except
      on E: Exception do
        Debug(dpError, section, Format('DeleteSite - remove TSite object failed: %s', [E.Message]));
    end;

    sitesdat.UpdateFile;

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
    Info.SslMethod := integer(s.sslmethod);
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
    Info.DirlistPriority := s.DirlistPriority;
    Info.NewdirDirlistReadd := s.NewdirDirlistReadd;
    Info.GlobalDirlistInterval := GetNewdirDirlistReaddValue();
    Info.PerformanceAdjustedDirlist := s.PerformanceAdjustedDirlist;
    Info.SkipBeingUploadedFiles := Integer(s.SkipBeingUploadedFiles);
    Info.KillConnectionOnStalledTransferSeconds := s.KillConnectionOnStalledTransferSeconds;
    Info.DestinationQueueLimit := s.RCInteger('destination_queue_limit', 0);
    if Info.DestinationQueueLimit < 0 then
      Info.DestinationQueueLimit := 0;
    Info.UseForNFOdownload := Integer(s.UseForNFOdownload);
    Info.SiteFullName := UTF8Encode(s.SiteFullName);
    Info.SiteLinkSpeed := UTF8Encode(s.SiteLinkSpeed);
    Info.SiteSize := UTF8Encode(s.SiteSize);
    Info.SiteNotes := UTF8Encode(s.SiteNotes);
    Info.Ident := UTF8Encode(s.Ident);
    Info.SiteInfos := UTF8Encode(s.SiteInfos);
    Info.MaxUpPerRip := s.MaxUpPerRip;

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
  fDestinationQueueLimit: integer;
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
    if data.GetValueIndex('dirlist_priority') >= 0 then s.DirlistPriority := data.GetValueOrNull('dirlist_priority');
    if data.GetValueIndex('newdir_dirlist_readd') >= 0 then s.NewdirDirlistReadd := data.GetValueOrNull('newdir_dirlist_readd');
    if data.GetValueIndex('performance_adjusted_dirlist') >= 0 then s.PerformanceAdjustedDirlist := boolean(data.GetValueOrNull('performance_adjusted_dirlist'));
    if data.GetValueIndex('skip_being_uploaded_files') >= 0 then s.SkipBeingUploadedFiles := TSkipBeingUploaded(Integer(data.GetValueOrNull('skip_being_uploaded_files')));
    if data.GetValueIndex('kill_connection_on_stalled_transfer') >= 0 then s.KillConnectionOnStalledTransferSeconds := data.GetValueOrNull('kill_connection_on_stalled_transfer');
    if data.GetValueIndex('destination_queue_limit') >= 0 then
    begin
      fDestinationQueueLimit := data.GetValueOrNull('destination_queue_limit');
      if fDestinationQueueLimit < 0 then
        fDestinationQueueLimit := 0;
      s.WCInteger('destination_queue_limit', fDestinationQueueLimit);
    end;
    if data.GetValueIndex('maxupperrip') >= 0 then s.MaxUpPerRip := data.GetValueOrNull('maxupperrip');
    if data.GetValueIndex('site_full_name') >= 0 then s.SiteFullName := string(data.GetValueOrNull('site_full_name'));
    if data.GetValueIndex('site_link_speed') >= 0 then s.SiteLinkSpeed := string(data.GetValueOrNull('site_link_speed'));
    if data.GetValueIndex('site_size') >= 0 then s.SiteSize := string(data.GetValueOrNull('site_size'));
    if data.GetValueIndex('site_notes') >= 0 then s.SiteNotes := string(data.GetValueOrNull('site_notes'));
    if data.GetValueIndex('ident_response') >= 0 then s.Ident := string(data.GetValueOrNull('ident_response'));
    if data.GetValueIndex('site_infos') >= 0 then s.SiteInfos := string(data.GetValueOrNull('site_infos'));
    if data.GetValueIndex('usefornfodownload') >= 0 then
      s.UseForNFOdownload := TUseForNfoDownload(Integer(data.GetValueOrNull('usefornfodownload')));

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

function TApiSitesServiceImpl.SetSiteSslMethod(const SiteName: RawUTF8; SslMethod: integer): boolean;
var
  s: TSite;
begin
  Result := False;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
      Exit;

    if (SslMethod < 0) or (SslMethod > Integer(High(TSSLMethods))) then
      Exit;

    s.sslmethod := TSSLMethods(SslMethod);
    Debug(dpMessage, section, Format('SetSiteSslMethod API: %s -> %d', [UTF8ToString(SiteName), SslMethod]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] SetSiteSslMethod: %s', [E.Message]));
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
  siteSections: TStringList;
begin
  Result := '';
  siteSections := nil;
  try
    s := FindSiteByName('', UTF8ToString(SiteName));
    if s = nil then
    begin
      Debug(dpError, section, Format('Site not found: %s', [UTF8ToString(SiteName)]));
      Result := '[]';
      Exit;
    end;

    sectionsArray.InitFast(dvArray);

    // Parse site's actual sections property instead of using global kb_sections
    siteSections := TStringList.Create;
    try
      siteSections.Delimiter := ' ';
      siteSections.StrictDelimiter := True;
      siteSections.DelimitedText := s.sections;

      for i := 0 to siteSections.Count - 1 do
      begin
        sectionName := Trim(siteSections[i]);
        if sectionName = '' then
          Continue;

        sectionDir := s.sectiondir[sectionName];

        TDocVariant.New(sectionDoc);
        TDocVariantData(sectionDoc).AddValue('section', UTF8Encode(sectionName));
        TDocVariantData(sectionDoc).AddValue('dir', UTF8Encode(sectionDir));
        sectionsArray.AddItem(sectionDoc);
      end;
    finally
      siteSections.Free;
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
var
  fPazoId: Integer;
  fPazo: TPazo;
begin
  Result := False;
  Info := nil;
  try
    Info := TApiTaskInfo.Create;
    Info.Uid := TaskUid;
    Info.TaskType := 'Transfer';

    fPazoId := -1;
    if (GlApiTaskToPazoId <> nil) then
    begin
      GlApiTaskToPazoIdLock.Enter('ApiTaskMap.GetTask');
      try
        if not GlApiTaskToPazoId.TryGetValue(TaskUid, fPazoId) then
          fPazoId := -1;
      finally
        GlApiTaskToPazoIdLock.Leave;
      end;
    end;

    if fPazoId = -1 then
    begin
      Info.Status := 'unknown';
      Exit(True);
    end;

    fPazo := FindPazoById(fPazoId);
    if fPazo = nil then
    begin
      Info.Status := 'completed';
      Info.Completed := Now;
      Exit(True);
    end;

    Info.Created := fPazo.added;
    if fPazo.readyerror or fPazo.stopped then
    begin
      Info.Status := 'failed';
      Info.Completed := Now;
      Info.Error := UTF8Encode(fPazo.errorreason);
    end
    else if fPazo.ready then
    begin
      Info.Status := 'completed';
      Info.Completed := Now;
    end
    else
    begin
      if fPazo.queuenumber.Value > 0 then
        Info.Status := 'in_progress'
      else
        Info.Status := 'pending';
    end;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetTask: %s', [E.Message]));
      if Info <> nil then
        FreeAndNil(Info);
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
var
  fSourceSite: String;
  fDestSite: String;
  fDestDir: String;
  fFullSourcePath: String;
  fSourceDir: String;
  fBaseName: String;
  fLastSlash: Integer;
  fPazo: TPazo;
  fSrcPs, fDstPs: TPazoSite;
  fTask: TPazoRaceTask;
begin
  Result := 0;
  try
    fSourceSite := UpperCase(Trim(UTF8ToString(SourceSite)));
    fDestSite := UpperCase(Trim(UTF8ToString(DestSite)));
    fDestDir := Trim(UTF8ToString(Dir));
    fFullSourcePath := Trim(UTF8ToString(FileName));

    Debug(dpMessage, section, Format('CreateTransferTask API: %s -> %s (%s) %s -> %s',
          [fSourceSite, fDestSite, UTF8ToString(Section), fFullSourcePath, fDestDir]));

    if (fSourceSite = '') or (fDestSite = '') then
      raise Exception.Create('SourceSite/DestSite is required');
    if FindSiteByName('', fSourceSite) = nil then
      raise Exception.CreateFmt('Unknown SourceSite: %s', [fSourceSite]);
    if FindSiteByName('', fDestSite) = nil then
      raise Exception.CreateFmt('Unknown DestSite: %s', [fDestSite]);
    if fFullSourcePath = '' then
      raise Exception.Create('FileName is required');

    // Normalize FTP paths (always '/')
    if fDestDir = '' then
      fDestDir := '/';
    if fDestDir[1] <> '/' then
      fDestDir := '/' + fDestDir;

    if fFullSourcePath[1] <> '/' then
      fFullSourcePath := '/' + fFullSourcePath;

    // Split FTP path into dir + basename (don't use ExtractFileName/Dir; those are OS dependent)
    fLastSlash := LastDelimiter('/', fFullSourcePath);
    if fLastSlash <= 0 then
    begin
      fSourceDir := '/';
      fBaseName := fFullSourcePath;
    end
    else if fLastSlash = 1 then
    begin
      fSourceDir := '/';
      fBaseName := Copy(fFullSourcePath, 2, MaxInt);
    end
    else
    begin
      fSourceDir := Copy(fFullSourcePath, 1, fLastSlash - 1);
      fBaseName := Copy(fFullSourcePath, fLastSlash + 1, MaxInt);
    end;

    if fBaseName = '' then
      raise Exception.CreateFmt('Invalid FileName (no basename): %s', [fFullSourcePath]);

    fPazo := PazoAdd(nil);
    AddPazoToKB(Format('TRANSFER-API-%d', [fPazo.pazo_id]), fPazo);

    fSrcPs := fPazo.AddSite(fSourceSite, fSourceDir, False);
    fDstPs := fPazo.AddSite(fDestSite, fDestDir, False);
    fSrcPs.AddDestination(fDstPs, 1);

    // Align with IRC transfer behavior: mark destination allowed + source dirlist "present"
    fDstPs.status := rssAllowed;
    if (fSrcPs.dirlist <> nil) then
      fSrcPs.dirlist.dirlistadded := True;

    fTask := TPazoRaceTask.Create('CONSOLE', 'Browser', fSourceSite, fDestSite, fPazo, nil, '', fBaseName, 0, 1);
    AddTask(fTask, True);

    if GlApiTaskToPazoId <> nil then
    begin
      GlApiTaskToPazoIdLock.Enter('ApiTaskMap.Add');
      try
        GlApiTaskToPazoId.AddOrSetValue(fTask.uid, fPazo.pazo_id);
      finally
        GlApiTaskToPazoIdLock.Leave;
      end;
    end;

    Result := fTask.uid;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateTransferTask: %s %s', [E.ClassName, E.Message]));
      Result := 0;
    end;
  end;
end;

function TApiQueueServiceImpl.CreateReleaseTransferTask(const SourceSite, DestSite, SourceDir,
                                DestDir, RlsName: RawUTF8): Int64;
var
  fSourceSite, fDestSite, fRlsName: String;
  fSrcDirParam, fDstDirParam: String;
  fFtpSrcDir, fFtpDstDir: String;
  sSrc, sDst: TSite;
  rc: TCRelease;
  rls: TRelease;
  p: TPazo;
  ps_src, ps_dst: TPazoSite;
  pd: TPazoDirlistTask;
begin
  Result := 0;
  try
    fSourceSite := UpperCase(Trim(UTF8ToString(SourceSite)));
    fDestSite := UpperCase(Trim(UTF8ToString(DestSite)));
    fSrcDirParam := Trim(UTF8ToString(SourceDir));
    fDstDirParam := Trim(UTF8ToString(DestDir));
    fRlsName := Trim(UTF8ToString(RlsName));

    Debug(dpMessage, section, Format('CreateReleaseTransferTask API: %s -> %s | %s -> %s | Rls: %s',
          [fSourceSite, fDestSite, fSrcDirParam, fDstDirParam, fRlsName]));

    if (fSourceSite = '') or (fDestSite = '') or (fSrcDirParam = '') or (fDstDirParam = '') or (fRlsName = '') then
      raise Exception.Create('Missing parameters (SourceSite, DestSite, SourceDir, DestDir, RlsName are required)');

    // Validate Source Site
    sSrc := FindSiteByName('', fSourceSite);
    if sSrc = nil then raise Exception.CreateFmt('Source site %s not found', [fSourceSite]);
    if sSrc.PermDown then raise Exception.CreateFmt('Source site %s is perm down', [fSourceSite]);
    if not (sSrc.WorkingStatus in [sstUnknown, sstUp]) then raise Exception.CreateFmt('Source site %s is down', [fSourceSite]);

    // Validate Dest Site
    sDst := FindSiteByName('', fDestSite);
    if sDst = nil then raise Exception.CreateFmt('Dest site %s not found', [fDestSite]);
    if sDst.PermDown then raise Exception.CreateFmt('Dest site %s is perm down', [fDestSite]);
    if not (sDst.WorkingStatus in [sstUnknown, sstUp]) then raise Exception.CreateFmt('Dest site %s is down', [fDestSite]);

    // Resolve Source Dir (Path vs Section)
    if ((1 = Pos('/', fSrcDirParam)) or (length(fSrcDirParam) = LastDelimiter('/', fSrcDirParam))) then
      fFtpSrcDir := fSrcDirParam
    else
    begin
      fFtpSrcDir := sSrc.sectiondir[UpperCase(fSrcDirParam)];
      if fFtpSrcDir = '' then raise Exception.CreateFmt('Source site %s has no dir for section %s', [fSourceSite, fSrcDirParam]);
    end;

    // Resolve Dest Dir (Path vs Section)
    if ((1 = Pos('/', fDstDirParam)) or (length(fDstDirParam) = LastDelimiter('/', fDstDirParam))) then
      fFtpDstDir := fDstDirParam
    else
    begin
      fFtpDstDir := sDst.sectiondir[UpperCase(fDstDirParam)];
      if fFtpDstDir = '' then raise Exception.CreateFmt('Dest site %s has no dir for section %s', [fDestSite, fDstDirParam]);
    end;

    // Create Pazo and Task
    // We treat fSrcDirParam as "section" for FindSectionHandler if it's a section, 
    // or fallback to 'PRE' or similar if path. 
    // IrcTransfer uses srcdir as "section" param to FindSectionHandler.
    rc := FindSectionHandler(fSrcDirParam); 
    rls := rc.Create(fRlsName, fSrcDirParam);
    p := PazoAdd(rls);
    
    // Add to KB for tracking
    AddPazoToKB(Format('TRANSFER-API-%d', [p.pazo_id]), p);

    p.AddSite(sSrc.Name, fFtpSrcDir, False);
    p.AddSite(sDst.Name, fFtpDstDir, False);

    ps_src := p.FindSite(sSrc.Name);
    ps_src.AddDestination(sDst.Name, 200);

    ps_dst := p.FindSite(sDst.Name);
    ps_dst.status := rssAllowed;

    // Ensure source has "dirlist added" so it processes results
    // ps_src re-retrieval (it might have changed since addsite?)
    if p.PazoSitesList.Count > 0 then
    begin
       ps_src := TPazoSite(p.PazoSitesList[0]); 
       if ps_src.dirlist <> nil then
         ps_src.dirlist.dirlistadded := True;
    end;

    // Create the Dirlist Task which starts the chain reaction
    pd := TPazoDirlistTask.Create('CONSOLE', 'Browser', ps_src.Name, p, '', False, False);
    AddTask(pd, True);

    // Map the task UID (of the Dirlist task) to the Pazo ID so GetTask can track status
    if GlApiTaskToPazoId <> nil then
    begin
      GlApiTaskToPazoIdLock.Enter('ApiTaskMap.Add');
      try
        GlApiTaskToPazoId.AddOrSetValue(pd.uid, p.pazo_id);
      finally
        GlApiTaskToPazoIdLock.Leave;
      end;
    end;

    Result := pd.uid;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateReleaseTransferTask: %s %s', [E.ClassName, E.Message]));
      Result := 0;
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
      SetText(RAWUTF8(Result));
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

function TApiRacesServiceImpl.GetRaces(const Page: integer; const PageSize: integer; const SinceUnix: Int64): RawJSON;
begin
  Result := StatsGetRecentRacesJson(Page, PageSize, SinceUnix);
end;

function TApiRacesServiceImpl.GetReleaseTransfers(const Release: RawUTF8; const Page: integer; const PageSize: integer;
  const SinceUnix: Int64): RawJSON;
begin
  Result := StatsGetReleaseRacesJson(UTF8ToString(Release), Page, PageSize, SinceUnix);
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

function TApiIrcServiceImpl.GetNetworkConfig(const NetName: RawUTF8; out Info: TApiIrcNetworkConfig): boolean;
var
  netNameStr: string;
  host: string;
  port: integer;
  ident: string;
  suffix: string;
begin
  Result := False;
  try
    netNameStr := UpperCase(UTF8ToString(NetName));
    host := sitesdat.ReadString('ircnet-' + netNameStr, 'bnc_host-0', '');
    if host = '' then
      host := sitesdat.ReadString('ircnet-' + netNameStr, 'host', '');
    if host = '' then
      Exit;

    port := sitesdat.ReadInteger('ircnet-' + netNameStr, 'bnc_port-0',
      sitesdat.ReadInteger('ircnet-' + netNameStr, 'port', 0));

    Info := TApiIrcNetworkConfig.Create;
    Info.Name := UTF8Encode(netNameStr);
    Info.Host := UTF8Encode(host);
    Info.Port := port;
    Info.Ssl := sitesdat.ReadBool('ircnet-' + netNameStr, 'ssl', False);
    Info.Password := UTF8Encode(sitesdat.ReadString('ircnet-' + netNameStr, 'password', ''));
    Info.Nick := UTF8Encode(sitesdat.ReadString('ircnet-' + netNameStr, 'nick', ''));
    ident := sitesdat.ReadString('ircnet-' + netNameStr, 'ident', '');
    suffix := '@soulless.ftp';
    if (Length(ident) > Length(suffix)) and (Copy(ident, Length(ident) - Length(suffix) + 1, Length(suffix)) = suffix) then
      ident := Copy(ident, 1, Length(ident) - Length(suffix));
    Info.Ident := UTF8Encode(ident);
    Info.User := UTF8Encode(sitesdat.ReadString('ircnet-' + netNameStr, 'username', ''));

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetNetworkConfig: %s', [E.Message]));
      Result := False;
    end;
  end;
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

          // Add is_added flag (true if channel has settings = was added via ircchanadd)
          TDocVariantData(chanDoc).AddValue('is_added', chanSettings <> nil);

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
  cbc, inviteonly: boolean;
  chankey, chanroles: string;
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

    // Check if blowkey starts with 'cbc:' prefix (for CBC mode)
    cbc := False;
    if Copy(LowerCase(blowkeyStr), 1, 4) = 'cbc:' then
    begin
      Delete(blowkeyStr, 1, 4);
      cbc := True;
    end;

    // Store current settings
    chankey := chanSettings.ChanKey;
    chanroles := chanSettings.ChanRoles;
    inviteonly := False; // Not stored in current settings, use default

    // Write to config
    sitesdat.WriteString('channel-' + netNameStr + '-' + channelStr, 'blowkey', blowkeyStr);
    sitesdat.WriteBool('channel-' + netNameStr + '-' + channelStr, 'cbc', cbc);

    // Remove old entry from list to create proper blowfish class (ECB or CBC)
    IrcChanSettingsList.Remove(netNameStr + channelStr);

    // Re-register with new settings
    RegisterChannelSettings(netNameStr, channelStr, chanroles, blowkeyStr, chankey, inviteonly, cbc);

    Debug(dpMessage, 'slapi', Format('SetChannelBlowkey: Updated blowkey for %s@%s (CBC: %s)', [channelStr, netNameStr, BoolToStr(cbc, True)]));
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
  cbc: boolean;
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

    // Check if blowkey starts with 'cbc:' prefix (for CBC mode)
    cbc := False;
    if Copy(LowerCase(blowkeyStr), 1, 4) = 'cbc:' then
    begin
      Delete(blowkeyStr, 1, 4);
      cbc := True;
    end;

    // Write to config if blowkey is set
    if blowkeyStr <> '' then
    begin
      sitesdat.WriteString('channel-' + netNameStr + '-' + channelStr, 'blowkey', blowkeyStr);
      sitesdat.WriteBool('channel-' + netNameStr + '-' + channelStr, 'cbc', cbc);
    end;

    // Use RegisterChannelSettings to add the channel
    RegisterChannelSettings(netNameStr, channelStr, rolesStr, blowkeyStr, chankeyStr, False, cbc);

    Debug(dpMessage, 'slapi', Format('AddChannel: Added channel %s@%s (CBC: %s)', [channelStr, netNameStr, BoolToStr(cbc, True)]));
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
  ircth: TMyIrcThread;
begin
  Result := False;
  try
    netNameStr := UTF8ToString(NetName);
    channelStr := UTF8ToString(Channel);

    ircth := FindIrcnetwork(netNameStr);
    if ircth = nil then
    begin
      Debug(dpError, 'slapi', Format('DeleteChannel: Network %s not found', [netNameStr]));
      Exit;
    end;

    // Check if channel exists
    if FindIrcChannelSettings(netNameStr, channelStr) = nil then
    begin
      Debug(dpError, 'slapi', Format('DeleteChannel: Channel %s@%s not found', [channelStr, netNameStr]));
      Exit;
    end;

    // Create key for dictionary (same format as in IrcChanSettingsList)
    dictKey := netNameStr + channelStr;

    // Part channel and remove from config to match !ircchandel behavior
    ircth.chanpart(channelStr);

    // Remove from global list
    IrcChanSettingsList.Remove(dictKey);

    // Remove from sites.dat
    sitesdat.EraseSection('channel-' + netNameStr + '-' + channelStr);

    ircth.shouldjoin := True;

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

function TApiIrcServiceImpl.SetNetworkConfig(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;
var
  netNameStr: string;
  hostStr: string;
  nickStr: string;
  identStr: string;
  userStr: string;
  suffix: string;
  ircThread: TMyIrcThread;
begin
  Result := False;
  try
    netNameStr := UpperCase(UTF8ToString(NetName));
    hostStr := UTF8ToString(Host);
    if hostStr = '' then
      Exit;

    nickStr := UTF8ToString(Nick);
    if nickStr = '' then
      nickStr := config.ReadString('irc', 'nickname', 'slftp');

    userStr := UTF8ToString(User);
    if userStr = '' then
      userStr := config.ReadString('irc', 'username', 'slftp');

    identStr := UTF8ToString(Ident);
    if identStr = '' then
      identStr := config.ReadString('irc', 'realname', 'slftp');
    suffix := '@soulless.ftp';
    if (Length(identStr) <= Length(suffix)) or (Copy(identStr, Length(identStr) - Length(suffix) + 1, Length(suffix)) <> suffix) then
      identStr := identStr + suffix;

    sitesdat.WriteString('ircnet-' + netNameStr, 'bnc_host-0', hostStr);
    sitesdat.WriteInteger('ircnet-' + netNameStr, 'bnc_port-0', Port);
    sitesdat.DeleteKey('ircnet-' + netNameStr, 'host');
    sitesdat.DeleteKey('ircnet-' + netNameStr, 'port');
    sitesdat.WriteBool('ircnet-' + netNameStr, 'ssl', Ssl);
    sitesdat.WriteString('ircnet-' + netNameStr, 'password', UTF8ToString(Password));
    sitesdat.WriteString('ircnet-' + netNameStr, 'nick', nickStr);
    sitesdat.WriteString('ircnet-' + netNameStr, 'anick', '_' + nickStr);
    sitesdat.WriteString('ircnet-' + netNameStr, 'ident', identStr);
    sitesdat.WriteString('ircnet-' + netNameStr, 'username', userStr);

    ircThread := FindIrcnetwork(netNameStr);
    if ircThread <> nil then
      ircThread.shouldrestart := True
    else
      myIrcThreads.Add(TMyIrcThread.Create(netNameStr));

    Debug(dpMessage, 'slapi', Format('SetNetworkConfig: Updated IRC network %s', [netNameStr]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SetNetworkConfig: %s', [E.Message]));
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

function TApiSpeedServiceImpl.TestSpeedLocal(const SiteName: RawUTF8): RawUTF8;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.StartLocal(UTF8ToString(SiteName)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSpeedLocal: %s', [E.Message]));
      raise; // Let mORMot handle the 500, but we have logged it.
    end;
  end;
end;

function TApiSpeedServiceImpl.TestSpeedOut(const SourceSite, DestSites: RawUTF8): RawUTF8;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.StartOut(UTF8ToString(SourceSite), UTF8ToString(DestSites)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSpeedOut: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.TestSpeedIn(const DestSite, SourceSites: RawUTF8): RawUTF8;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.StartIn(UTF8ToString(DestSite), UTF8ToString(SourceSites)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSpeedIn: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.TestSpeedCleanup(const Sites: RawUTF8): RawUTF8;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.StartCleanup(UTF8ToString(Sites)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSpeedCleanup: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.TestSpeedMatrix(const IncludeSites: RawUTF8;
  const ExcludeSites: RawUTF8): RawUTF8;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.StartMatrix(
      UTF8ToString(IncludeSites),
      UTF8ToString(ExcludeSites)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TestSpeedMatrix: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.GetSpeedTestSites: RawJSON;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.GetSpeedTestSites);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetSpeedTestSites: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.GetTestLog(const TestId: RawUTF8): RawJSON;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.GetLog(UTF8ToString(TestId)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetTestLog: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.GetTestStatus(const TestId: RawUTF8): RawJSON;
begin
  try
    Result := UTF8Encode(TSpeedTestManager.Instance.GetStatus(UTF8ToString(TestId)));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetTestStatus: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TApiSpeedServiceImpl.AbortSpeedTest(const TestId: RawUTF8): boolean;
begin
  Result := False;
  try
    Result := TSpeedTestManager.Instance.AbortTest(UTF8ToString(TestId));
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] AbortSpeedTest: %s', [E.Message]));
      Result := False;
    end;
  end;
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
  missingFields: string;

  function GetRuleField(const FieldName: RawUTF8; out FieldValue: string): boolean;
  var
    rawValue: variant;
  begin
    Result := False;
    try
      if FieldName = 'netname' then
        rawValue := ruleDoc.netname
      else if FieldName = 'channel' then
        rawValue := ruleDoc.channel
      else if FieldName = 'botnicks' then
        rawValue := ruleDoc.botnicks
      else if FieldName = 'sitename' then
        rawValue := ruleDoc.sitename
      else if FieldName = 'event' then
        rawValue := ruleDoc.event
      else if FieldName = 'words' then
        rawValue := ruleDoc.words
      else if FieldName = 'section' then
        rawValue := ruleDoc.section
      else
      begin
        Debug(dpError, 'slapi', Format('AddPrecatcherRule: Unknown field %s', [UTF8ToString(FieldName)]));
        Exit;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'slapi', Format('AddPrecatcherRule: Missing or invalid field %s (%s)', [UTF8ToString(FieldName), E.Message]));
        Exit;
      end;
    end;

    if VarIsNull(rawValue) or VarIsEmpty(rawValue) then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Field %s is null/empty (type=%d)', [UTF8ToString(FieldName), VarType(rawValue)]));
      Exit;
    end;

    FieldValue := UTF8ToString(VariantToUTF8(rawValue));
    Result := True;
  end;
begin
  Result := -1;
  try
    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Incoming payload len=%d', [Length(RuleData)]));
    if RuleData = '' then
    begin
      Debug(dpError, 'slapi', 'AddPrecatcherRule: Empty RuleData payload (expected JSON in RuleData param)');
      Exit;
    end;

    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Raw payload=%s', [RuleData]));
    ruleDoc := _JsonFast(RuleData);

    if VarIsEmpty(ruleDoc) or VarIsNull(ruleDoc) then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Invalid JSON payload: %s', [RuleData]));
      Exit;
    end;

    // Accept wrapper payload: { "RuleData": { ... } }
    try
      if not VarIsEmpty(ruleDoc.RuleData) and not VarIsNull(ruleDoc.RuleData) then
      begin
        Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Using RuleData wrapper payload');
        ruleDoc := ruleDoc.RuleData;
      end
      else
        Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Using direct payload');
    except
      // ignore if no RuleData field
      Debug(dpSpam, 'slapi', 'AddPrecatcherRule: No RuleData wrapper present');
    end;

    missingFields := '';
    if not GetRuleField('netname', netname) then
      missingFields := missingFields + ' netname';
    if not GetRuleField('channel', channel) then
      missingFields := missingFields + ' channel';
    if not GetRuleField('botnicks', botnicks) then
      missingFields := missingFields + ' botnicks';
    if not GetRuleField('sitename', sitename) then
      missingFields := missingFields + ' sitename';
    if not GetRuleField('event', event) then
      missingFields := missingFields + ' event';
    if not GetRuleField('words', words) then
      missingFields := missingFields + ' words';
    if not GetRuleField('section', section) then
      missingFields := missingFields + ' section';

    if missingFields <> '' then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Missing required fields:%s (payload=%s)', [missingFields, RuleData]));
      Exit;
    end;

    netname := UpperCase(netname);
    sitename := UpperCase(sitename);
    event := UpperCase(event);

    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Parsed netname=%s channel=%s botnicks=%s sitename=%s event=%s words=%s section=%s',
      [netname, channel, botnicks, sitename, event, words, section]));

    // Validate event type
    kb_event := EventStringToTKBEventType(event);
    if not (kb_event in [kbePRE, kbeADDPRE, kbeCOMPLETE, kbeNEWDIR, kbeNUKE, kbeREQUEST]) then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Invalid event type: %s (payload=%s)', [event, RuleData]));
      Exit;
    end;
    Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Event type OK');

    // Validate site exists
    if FindSiteByName('', sitename) = nil then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Site %s not found (payload=%s)', [sitename, RuleData]));
      Exit;
    end;
    Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Site OK');

    // Validate channel exists
    if FindIrcChannelSettings(netname, channel) = nil then
    begin
      Debug(dpError, 'slapi', Format('AddPrecatcherRule: Channel %s@%s not found (payload=%s)', [channel, netname, RuleData]));
      Exit;
    end;
    Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Channel OK');

    // Add rule to catcherFile
    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Adding rule to catcherFile (count before=%d)', [catcherFile.Count]));
    catcherFile.Add(Format('%s;%s;%s;%s;%s;%s;%s',
      [netname, channel, botnicks, sitename, event, words, section]));
    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Added rule line=%s', [catcherFile[catcherFile.Count - 1]]));
    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: catcherFile count after add=%d', [catcherFile.Count]));

    // Rebuild precatcher
    Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Rebuilding precatcher');
    PrecatcherRebuild;
    Debug(dpSpam, 'slapi', 'AddPrecatcherRule: Precatcher rebuild complete');

    Result := catcherFile.Count - 1; // Return ID of newly added rule
    Debug(dpSpam, 'slapi', Format('AddPrecatcherRule: Returning rule id=%d', [Result]));

    Debug(dpMessage, 'slapi', Format('AddPrecatcherRule: Added rule for %s@%s -> %s', [channel, netname, sitename]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] AddPrecatcherRule: %s (payload=%s)', [E.Message, RuleData]));
      Result := -1;
    end;
  end;
end;

function TApiPrecatcherServiceImpl.UpdatePrecatcherRule(RuleId: integer; const RuleData: RawJSON): boolean;
var
  ruleDoc: variant;
  netname, channel, botnicks, sitename, event, words, section: string;
  kb_event: TKBEventType;
  missingFields: string;

  function GetRuleField(const FieldName: RawUTF8; out FieldValue: string): boolean;
  var
    rawValue: variant;
  begin
    Result := False;
    try
      if FieldName = 'netname' then
        rawValue := ruleDoc.netname
      else if FieldName = 'channel' then
        rawValue := ruleDoc.channel
      else if FieldName = 'botnicks' then
        rawValue := ruleDoc.botnicks
      else if FieldName = 'sitename' then
        rawValue := ruleDoc.sitename
      else if FieldName = 'event' then
        rawValue := ruleDoc.event
      else if FieldName = 'words' then
        rawValue := ruleDoc.words
      else if FieldName = 'section' then
        rawValue := ruleDoc.section
      else
      begin
        Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Unknown field %s', [UTF8ToString(FieldName)]));
        Exit;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Missing or invalid field %s (%s)', [UTF8ToString(FieldName), E.Message]));
        Exit;
      end;
    end;

    if VarIsNull(rawValue) or VarIsEmpty(rawValue) then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Field %s is null/empty (type=%d)', [UTF8ToString(FieldName), VarType(rawValue)]));
      Exit;
    end;

    FieldValue := UTF8ToString(VariantToUTF8(rawValue));
    Result := True;
  end;
begin
  Result := False;
  try
    if (RuleId < 0) or (RuleId >= catcherFile.Count) then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Invalid rule ID: %d', [RuleId]));
      Exit;
    end;

    Debug(dpSpam, 'slapi', Format('UpdatePrecatcherRule: Incoming payload len=%d', [Length(RuleData)]));
    if RuleData = '' then
    begin
      Debug(dpError, 'slapi', 'UpdatePrecatcherRule: Empty RuleData payload (expected JSON in RuleData param)');
      Exit;
    end;

    Debug(dpSpam, 'slapi', Format('UpdatePrecatcherRule: Raw payload=%s', [RuleData]));
    ruleDoc := _JsonFast(RuleData);

    if VarIsEmpty(ruleDoc) or VarIsNull(ruleDoc) then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Invalid JSON payload: %s', [RuleData]));
      Exit;
    end;

    // Accept wrapper payload: { "RuleData": { ... } }
    try
      if not VarIsEmpty(ruleDoc.RuleData) and not VarIsNull(ruleDoc.RuleData) then
      begin
        Debug(dpSpam, 'slapi', 'UpdatePrecatcherRule: Using RuleData wrapper payload');
        ruleDoc := ruleDoc.RuleData;
      end
      else
        Debug(dpSpam, 'slapi', 'UpdatePrecatcherRule: Using direct payload');
    except
      // ignore if no RuleData field
      Debug(dpSpam, 'slapi', 'UpdatePrecatcherRule: No RuleData wrapper present');
    end;

    missingFields := '';
    if not GetRuleField('netname', netname) then
      missingFields := missingFields + ' netname';
    if not GetRuleField('channel', channel) then
      missingFields := missingFields + ' channel';
    if not GetRuleField('botnicks', botnicks) then
      missingFields := missingFields + ' botnicks';
    if not GetRuleField('sitename', sitename) then
      missingFields := missingFields + ' sitename';
    if not GetRuleField('event', event) then
      missingFields := missingFields + ' event';
    if not GetRuleField('words', words) then
      missingFields := missingFields + ' words';
    if not GetRuleField('section', section) then
      missingFields := missingFields + ' section';

    if missingFields <> '' then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Missing required fields:%s (payload=%s)', [missingFields, RuleData]));
      Exit;
    end;

    netname := UpperCase(netname);
    sitename := UpperCase(sitename);
    event := UpperCase(event);

    Debug(dpSpam, 'slapi', Format('UpdatePrecatcherRule: Parsed netname=%s channel=%s botnicks=%s sitename=%s event=%s words=%s section=%s',
      [netname, channel, botnicks, sitename, event, words, section]));

    // Validate event type
    kb_event := EventStringToTKBEventType(event);
    if not (kb_event in [kbePRE, kbeADDPRE, kbeCOMPLETE, kbeNEWDIR, kbeNUKE, kbeREQUEST]) then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Invalid event type: %s (payload=%s)', [event, RuleData]));
      Exit;
    end;

    // Validate site exists
    if FindSiteByName('', sitename) = nil then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Site %s not found (payload=%s)', [sitename, RuleData]));
      Exit;
    end;

    // Validate channel exists
    if FindIrcChannelSettings(netname, channel) = nil then
    begin
      Debug(dpError, 'slapi', Format('UpdatePrecatcherRule: Channel %s@%s not found (payload=%s)', [channel, netname, RuleData]));
      Exit;
    end;

    // Update rule in catcherFile
    catcherFile[RuleId] := Format('%s;%s;%s;%s;%s;%s;%s',
      [netname, channel, botnicks, sitename, event, words, section]);

    // Rebuild precatcher
    PrecatcherRebuild;

    Debug(dpMessage, 'slapi', Format('UpdatePrecatcherRule: Updated rule #%d for %s@%s -> %s', [RuleId, channel, netname, sitename]));
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] UpdatePrecatcherRule: %s (payload=%s)', [E.Message, RuleData]));
      Result := False;
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
  debugCaptureActive: Boolean;
begin
  Result := '{}';
  debugLines := nil;
  debugCaptureActive := False;
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

    glPrecatcherDebugCaptureLock.Enter('TestPrecatcher');
    try
      // Enable debug capture temporarily
      Precatcher_BeginDebugCapture(debugLines);
      debugCaptureActive := True;
      try
        // Process the announce
        PrecatcherProcessB(netname, channel, nick, text);
      finally
        Precatcher_EndDebugCapture(debugLines);
        debugCaptureActive := False;
      end;
    finally
      if debugCaptureActive then
      begin
        try
          Precatcher_EndDebugCapture(debugLines);
        except
          // ignore cleanup errors in exception path
        end;
      end;
      glPrecatcherDebugCaptureLock.Leave;
    end;

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

function TApiPrecatcherServiceImpl.GetHits(const Limit: integer; const SinceUnix: Int64;
  const ReleaseName: RawUTF8; const SiteName: RawUTF8): RawJSON;
var
  hits: TPrecatcherHits;
  hitsArray: TDocVariantData;
  hitDoc: variant;
  i: Integer;
  releaseFilter: String;
  siteFilter: String;
begin
  Result := '[]';
  try
    releaseFilter := UTF8ToString(ReleaseName);
    siteFilter := UTF8ToString(SiteName);
    Precatcher_GetHits(Limit, SinceUnix, releaseFilter, siteFilter, hits);

    hitsArray.InitFast(dvArray);
    for i := 0 to Length(hits) - 1 do
    begin
      TDocVariant.New(hitDoc);
      TDocVariantData(hitDoc).AddValue('id', hits[i].Id);
      TDocVariantData(hitDoc).AddValue('atUnix', DateTimeToUnix(hits[i].At));
      TDocVariantData(hitDoc).AddValue('netname', UTF8Encode(hits[i].Netname));
      TDocVariantData(hitDoc).AddValue('channel', UTF8Encode(hits[i].Channel));
      TDocVariantData(hitDoc).AddValue('nick', UTF8Encode(hits[i].Nick));
      TDocVariantData(hitDoc).AddValue('sitename', UTF8Encode(hits[i].Sitename));
      TDocVariantData(hitDoc).AddValue('event', UTF8Encode(KBEventTypeToString(hits[i].EventType)));
      TDocVariantData(hitDoc).AddValue('section', UTF8Encode(hits[i].Section));
      TDocVariantData(hitDoc).AddValue('releaseName', UTF8Encode(hits[i].ReleaseName));
      TDocVariantData(hitDoc).AddValue('ruleId', hits[i].RuleId);
      TDocVariantData(hitDoc).AddValue('ruleLine', UTF8Encode(hits[i].RuleLine));
      TDocVariantData(hitDoc).AddValue('text', UTF8Encode(hits[i].Text));
      hitsArray.AddItem(hitDoc);
    end;

    Result := hitsArray.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetHits: %s', [E.Message]));
      Result := '[]';
    end;
  end;
end;

function TApiPrecatcherServiceImpl.GetPrecatcherConfig: RawJSON;
var
  fileName: string;
  fileDoc: variant;
  sl: TStringList;
  md5Hash: RawUTF8;
begin
  Result := '{}';
  try
    fileName := ExtractFilePath(ParamStr(0)) + 'slftp.precatcher';

    TDocVariant.New(fileDoc);
    TDocVariantData(fileDoc).AddValue('Path', UTF8Encode(fileName));
    TDocVariantData(fileDoc).AddValue('Exists', FileExists(fileName));

    if FileExists(fileName) then
    begin
      md5Hash := _Md5OfFile(fileName);
      TDocVariantData(fileDoc).AddValue('Md5', md5Hash);

      sl := TStringList.Create;
      try
        sl.LoadFromFile(fileName);
        TDocVariantData(fileDoc).AddValue('Content', UTF8Encode(sl.Text));
      finally
        sl.Free;
      end;
    end
    else
    begin
      TDocVariantData(fileDoc).AddValue('Md5', '');
      TDocVariantData(fileDoc).AddValue('Content', '');
    end;

    Result := TDocVariantData(fileDoc).ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetPrecatcherConfig: %s', [E.Message]));
      Result := '{}';
    end;
  end;
end;

function TApiPrecatcherServiceImpl.ValidatePrecatcherConfig(const Content: RawUTF8): RawJSON;
var
  lines: TStringList;
  errors: TDocVariantData;
  errObj: variant;
  resultDoc: variant;
  i: integer;
  line: string;
  currentSection: string;
  replacefromCount, replacetoCount: integer;
  re: TRegExpr;
  regexField, regexPattern: string;
  regexEnd: integer;
  semicolonCount, j: integer;

  procedure AddError(const aLine: integer; const aMessage: string);
  begin
    TDocVariant.New(errObj);
    TDocVariantData(errObj).AddValue('line', aLine);
    TDocVariantData(errObj).AddValue('message', UTF8Encode(aMessage));
    errors.AddItem(errObj);
  end;

  function ValidateRegex(const regexStr: string): boolean;
  begin
    Result := False;
    re := TRegExpr.Create;
    try
      try
        re.Expression := regexStr;
        re.Compile;
        Result := True;
      except
        on E: Exception do
          Result := False;
      end;
    finally
      re.Free;
    end;
  end;

begin
  Result := '{}';
  currentSection := '';
  replacefromCount := 0;
  replacetoCount := 0;

  try
    TDocVariant.New(resultDoc);
    errors.InitFast(dvArray);

    lines := TStringList.Create;
    try
      lines.Text := UTF8ToString(Content);
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);

        // Skip empty lines and comments
        if (line = '') or (Length(line) > 0) and ((line[1] = '/') or (line[1] = ';') or (line[1] = '#')) then
          Continue;

        // Section headers
        if line = '[racetool]' then
          currentSection := 'racetool'
        else if line = '[ignorelist]' then
          currentSection := 'ignorelist'
        else if line = '[replace]' then
          currentSection := 'replace'
        else if line = '[sections]' then
          currentSection := 'sections'
        else if line = '[mappings]' then
          currentSection := 'mappings'
        else if line = '[channels]' then
          currentSection := 'channels'
        else if line = '[pretime]' then
          currentSection := 'pretime'
        else
        begin
          // Validate based on current section
          if currentSection = 'sections' then
          begin
            if Pos('=', line) = 0 then
              AddError(i + 1, 'Invalid sections format. Expected: SECTION=alias1,alias2');
          end
          else if currentSection = 'mappings' then
          begin
            // Count semicolons manually
            semicolonCount := 0;
            for j := 1 to Length(line) do
              if line[j] = ';' then
                Inc(semicolonCount);

            if semicolonCount <> 2 then
              AddError(i + 1, 'Invalid mappings format. Expected: orig;new;/regex/')
            else
            begin
              regexField := SubString(line, ';', 3);
              if (Length(regexField) >= 2) and (regexField[1] = '/') then
              begin
                regexEnd := Pos('/', Copy(regexField, 2, MaxInt));
                if regexEnd > 0 then
                begin
                  regexPattern := Copy(regexField, 2, regexEnd - 1);
                  if not ValidateRegex(regexPattern) then
                    AddError(i + 1, Format('Invalid regex pattern: %s', [regexPattern]));
                end;
              end;
            end;
          end
          else if currentSection = 'replace' then
          begin
            if Pos('replacefrom=', line) = 1 then
              Inc(replacefromCount)
            else if Pos('replaceto=', line) = 1 then
              Inc(replacetoCount);
          end
          else if currentSection = 'ignorelist' then
          begin
            if (Pos('ignorewords=', line) <> 1) and (Pos('tagline=', line) <> 1) then
              AddError(i + 1, 'Invalid ignorelist format. Expected: ignorewords= or tagline=');
          end;
        end;
      end;

      // Validate replacefrom/replaceto pairing
      if replacefromCount <> replacetoCount then
        AddError(0, Format('Mismatched replacefrom/replaceto count: %d from, %d to', [replacefromCount, replacetoCount]));

    finally
      lines.Free;
    end;

    TDocVariantData(resultDoc).AddValue('Ok', TDocVariantData(errors).Count = 0);
    TDocVariantData(resultDoc).AddValue('Errors', variant(errors));
    Result := TDocVariantData(resultDoc).ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] ValidatePrecatcherConfig: %s', [E.Message]));
      Result := '{"Ok":false,"Errors":[]}';
    end;
  end;
end;

function TApiPrecatcherServiceImpl.SavePrecatcherConfig(const Content: RawUTF8; const ExpectedMd5: RawUTF8; Reload: boolean): RawJSON;
var
  fileName: string;
  currentMd5: RawUTF8;
  validationResult: RawJSON;
  validationDoc: variant;
  resultDoc: variant;
  sl: TStringList;
  newMd5: RawUTF8;
begin
  Result := '{}';
  try
    TDocVariant.New(resultDoc);
    fileName := ExtractFilePath(ParamStr(0)) + 'slftp.precatcher';

    // MD5 conflict check
    currentMd5 := '';
    if FileExists(fileName) then
      currentMd5 := _Md5OfFile(fileName);

    if (ExpectedMd5 <> '') and (currentMd5 <> '') and
       (UpperCase(UTF8ToString(ExpectedMd5)) <> UpperCase(UTF8ToString(currentMd5))) then
    begin
      TDocVariantData(resultDoc).AddValue('Ok', False);
      TDocVariantData(resultDoc).AddValue('Message', 'Conflict: file changed on disk since last load');
      TDocVariantData(resultDoc).AddValue('Path', UTF8Encode(fileName));
      TDocVariantData(resultDoc).AddValue('Md5', currentMd5);
      TDocVariantData(resultDoc).AddValue('Errors', '[]');
      Result := TDocVariantData(resultDoc).ToJSON;
      Exit;
    end;

    // Validate before save
    validationResult := ValidatePrecatcherConfig(Content);
    validationDoc := _JsonFast(validationResult);

    if not validationDoc.Ok then
    begin
      TDocVariantData(resultDoc).AddValue('Ok', False);
      TDocVariantData(resultDoc).AddValue('Message', 'Validation failed');
      TDocVariantData(resultDoc).AddValue('Path', UTF8Encode(fileName));
      TDocVariantData(resultDoc).AddValue('Md5', currentMd5);
      TDocVariantData(resultDoc).AddValue('Errors', validationDoc.Errors);
      Result := TDocVariantData(resultDoc).ToJSON;
      Exit;
    end;

    // Save to disk
    sl := TStringList.Create;
    try
      sl.Text := UTF8ToString(Content);
      sl.SaveToFile(fileName);
    finally
      sl.Free;
    end;

    // Calculate new MD5
    newMd5 := _Md5OfFile(fileName);

    // Reload if requested
    if Reload then
      PrecatcherReload;

    TDocVariantData(resultDoc).AddValue('Ok', True);
    TDocVariantData(resultDoc).AddValue('Message', 'Saved successfully');
    TDocVariantData(resultDoc).AddValue('Path', UTF8Encode(fileName));
    TDocVariantData(resultDoc).AddValue('Md5', newMd5);
    TDocVariantData(resultDoc).AddValue('Errors', '[]');
    Result := TDocVariantData(resultDoc).ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] SavePrecatcherConfig: %s', [E.Message]));
      Result := '{"Ok":false,"Message":"Exception occurred","Errors":[]}';
    end;
  end;
end;

function TApiPrecatcherServiceImpl.GetPrecatcherHelpers: RawJSON;
var
  helpers: TDocVariantData;
  sections: TDocVariantData;
  mappingTemplates: TDocVariantData;
  i: integer;
begin
  Result := '{}';
  try
    helpers.InitFast(dvObject);

    // Section names from kb_sections
    sections.InitFast(dvArray);
    if kb_sections <> nil then
    begin
      for i := 0 to kb_sections.Count - 1 do
        sections.AddItem(UTF8Encode(kb_sections[i]));
    end;
    helpers.AddValue('sections', variant(sections));

    // Mapping templates
    mappingTemplates.InitFast(dvArray);
    mappingTemplates.AddItem(';SECTION;/regex/');
    mappingTemplates.AddItem(';SECTION;/regex/i');
    mappingTemplates.AddItem('ORIG;NEW;/regex/');
    mappingTemplates.AddItem('ORIG;NEW;/regex/i');
    helpers.AddValue('mappingTemplates', variant(mappingTemplates));

    Result := helpers.ToJSON;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] GetPrecatcherHelpers: %s', [E.Message]));
      Result := '{}';
    end;
  end;
end;

{ TApiSimulatorServiceImpl }

function TApiSimulatorServiceImpl.Simulate(const Section, ReleaseName: RawUTF8; const SimulatePre: boolean): RawJSON;
var
  resultDoc: variant;
  simDoc: variant;
  sitesArr: TDocVariantData;
  routesArr: TDocVariantData;
  siteDoc: variant;
  routeDoc: variant;
  res: TSimulationResult;
  i: integer;
  sec: string;
  rls: string;
begin
  Result := '{}';
  try
    sec := Trim(UTF8ToString(Section));
    rls := Trim(UTF8ToString(ReleaseName));

    if (sec = '') or (rls = '') then
    begin
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode('Missing required fields (Section, ReleaseName)'));
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    res := SimulateRelease(sec, rls, SimulatePre);
    try
      TDocVariant.New(simDoc);
      TDocVariantData(simDoc).AddValue('Releasename', UTF8Encode(res.Releasename));
      TDocVariantData(simDoc).AddValue('Section', UTF8Encode(res.Section));
      TDocVariantData(simDoc).AddValue('TotalSites', res.TotalSites);
      TDocVariantData(simDoc).AddValue('AllowedSites', res.AllowedSites);
      TDocVariantData(simDoc).AddValue('ErrorMessage', UTF8Encode(res.ErrorMessage));

      sitesArr.InitFast(dvArray);
      for i := 0 to res.SiteResults.Count - 1 do
      begin
        TDocVariant.New(siteDoc);
        TDocVariantData(siteDoc).AddValue('Sitename', UTF8Encode(res.SiteResults[i].Sitename));
        TDocVariantData(siteDoc).AddValue('Section', UTF8Encode(res.SiteResults[i].Section));
        TDocVariantData(siteDoc).AddValue('Allowed', res.SiteResults[i].Allowed);
        TDocVariantData(siteDoc).AddValue('Reason', UTF8Encode(res.SiteResults[i].Reason));
        TDocVariantData(siteDoc).AddValue('RuleAction', UTF8Encode(res.SiteResults[i].RuleAction));
        TDocVariantData(siteDoc).AddValue('IsAffil', res.SiteResults[i].IsAffil);
        TDocVariantData(siteDoc).AddValue('HasSection', res.SiteResults[i].HasSection);
        TDocVariantData(siteDoc).AddValue('SiteDown', res.SiteResults[i].SiteDown);
        TDocVariantData(siteDoc).AddValue('PretimeOk', res.SiteResults[i].PretimeOk);
        sitesArr.AddItem(siteDoc);
      end;
      TDocVariantData(simDoc).AddValue('Sites', _Json(sitesArr.ToJSON));

      routesArr.InitFast(dvArray);
      for i := 0 to res.RouteResults.Count - 1 do
      begin
        TDocVariant.New(routeDoc);
        TDocVariantData(routeDoc).AddValue('SourceSite', UTF8Encode(res.RouteResults[i].SourceSite));
        TDocVariantData(routeDoc).AddValue('DestinationSite', UTF8Encode(res.RouteResults[i].DestinationSite));
        TDocVariantData(routeDoc).AddValue('Rank', res.RouteResults[i].Rank);
        TDocVariantData(routeDoc).AddValue('RouteWeight', res.RouteResults[i].RouteWeight);
        routesArr.AddItem(routeDoc);
      end;
      TDocVariantData(simDoc).AddValue('Routes', _Json(routesArr.ToJSON));

      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', res.ErrorMessage = '');
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(res.ErrorMessage));
      TDocVariantData(resultDoc).AddValue('simulation', simDoc);
      Result := VariantSaveJSON(resultDoc);
    finally
      res.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] Simulate: %s', [E.Message]));
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(E.Message));
      Result := VariantSaveJSON(resultDoc);
    end;
  end;
end;

function TApiSimulatorServiceImpl.DetectSection(const ReleaseName: RawUTF8): RawJSON;
var
  resultDoc: variant;
  debugDoc: variant;
  rls: string;
  detectedSection: string;
  sectionDirect: string;
  sectionAfterReplace: string;
  sectionBeforeMapping: string;
  inputDirect: string;
  inputAfterReplace: string;
  usedReplace: Boolean;
  debugLines: TStringList;
  compactLines: TStringList;
  replaceLines: TStringList;
  mappingSections: TStringList;
  mappedLines: TStringList;
  line: string;
  sectionInChain: string;
  pathText: string;
  valueText: string;
  resolutionMode: string;
  replaceChanged: Boolean;
  mappingOnly: Boolean;
  i: Integer;
  p: Integer;
  debugCaptureActive: Boolean;
begin
  Result := '{}';
  debugLines := nil;
  compactLines := nil;
  replaceLines := nil;
  mappingSections := nil;
  mappedLines := nil;
  debugCaptureActive := False;
  try
    rls := Trim(UTF8ToString(ReleaseName));

    if rls = '' then
    begin
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode('ReleaseName is required'));
      TDocVariantData(resultDoc).AddValue('section', '');
      Result := VariantSaveJSON(resultDoc);
      Exit;
    end;

    glPrecatcherDebugCaptureLock.Enter('DetectSection');
    try
      Precatcher_BeginDebugCapture(debugLines);
      debugCaptureActive := True;
      try
        inputDirect := ' ' + rls + ' ';

        // Use precatcher logic to detect section
        sectionDirect := FindSection(inputDirect);
        detectedSection := sectionDirect;

        usedReplace := sectionDirect = '';
        inputAfterReplace := '';
        sectionAfterReplace := '';
        if usedReplace then
        begin
          inputAfterReplace := ProcessDoReplace(inputDirect, rls);
          sectionAfterReplace := FindSection(' ' + inputAfterReplace + ' ');
          detectedSection := sectionAfterReplace;
        end;

        sectionBeforeMapping := detectedSection;

        // Apply section mapping
        detectedSection := PrecatcherSectionMapping(rls, detectedSection);
      finally
        Precatcher_EndDebugCapture(debugLines);
        debugCaptureActive := False;
      end;
    finally
      if debugCaptureActive then
      begin
        try
          Precatcher_EndDebugCapture(debugLines);
        except
          // ignore cleanup errors in exception path
        end;
        debugCaptureActive := False;
      end;
      glPrecatcherDebugCaptureLock.Leave;
    end;

    compactLines := TStringList.Create;
    replaceLines := TStringList.Create;
    mappingSections := TStringList.Create;
    mappedLines := TStringList.Create;

    if debugLines <> nil then
    begin
      for i := 0 to debugLines.Count - 1 do
      begin
        line := Trim(debugLines[i]);
        if line = '' then
          Continue;

        if Pos('ProcessDoReplace ', line) = 1 then
        begin
          if replaceLines.IndexOf(line) = -1 then
            replaceLines.Add(line);
          Continue;
        end;

        if Pos('PrecatcherSectionMapping start testing ', line) = 1 then
        begin
          p := Pos(' in ', line);
          if p > 0 then
            sectionInChain := Trim(Copy(line, p + 4, MaxInt))
          else
            sectionInChain := '';

          if mappingSections.IndexOf(sectionInChain) = -1 then
            mappingSections.Add(sectionInChain);
          Continue;
        end;

        if (Pos('PrecatcherSectionMapping ', line) = 1) and (Pos(' mapped to ', line) > 0) then
        begin
          if mappedLines.IndexOf(line) = -1 then
            mappedLines.Add(line);
          Continue;
        end;
      end;
    end;

    compactLines.Add(Format('Release: %s', [rls]));
    if sectionDirect <> '' then
      valueText := sectionDirect
    else
      valueText := '(none)';
    compactLines.Add(Format('Direct section: %s', [valueText]));

    if sectionAfterReplace <> '' then
      valueText := sectionAfterReplace
    else
      valueText := '(none)';
    compactLines.Add(Format('Section after replace: %s', [valueText]));

    replaceChanged := usedReplace and (inputAfterReplace <> inputDirect);
    if usedReplace then
      compactLines.Add(Format('Replace changed input: %s', [BoolToStr(replaceChanged, True)]));

    if replaceChanged and (replaceLines.Count > 0) then
    begin
      compactLines.Add('');
      compactLines.Add('Applied replace rules:');
      for i := 0 to replaceLines.Count - 1 do
        compactLines.Add('- ' + replaceLines[i]);
    end;

    if mappingSections.Count > 0 then
    begin
      pathText := '';
      for i := 0 to mappingSections.Count - 1 do
      begin
        sectionInChain := mappingSections[i];
        if sectionInChain = '' then
          sectionInChain := '(root)';

        if pathText = '' then
          pathText := sectionInChain
        else
          pathText := pathText + ' -> ' + sectionInChain;
      end;
      compactLines.Add('');
      compactLines.Add('Mapping path: ' + pathText);
    end;

    if mappedLines.Count > 0 then
    begin
      compactLines.Add('');
      compactLines.Add('Matched mappings:');
      for i := 0 to mappedLines.Count - 1 do
        compactLines.Add('- ' + mappedLines[i]);
    end;

    mappingOnly := (sectionDirect = '') and (sectionAfterReplace = '') and (detectedSection <> '');
    if mappingOnly then
      resolutionMode := 'mapping-only'
    else if sectionAfterReplace <> '' then
      resolutionMode := 'replace+mapping'
    else if sectionDirect <> '' then
      resolutionMode := 'direct+mapping'
    else
      resolutionMode := 'none';

    compactLines.Add('');
    compactLines.Add('Resolution: ' + resolutionMode);

    compactLines.Add('');
    if sectionBeforeMapping <> '' then
      valueText := sectionBeforeMapping
    else
      valueText := '(none)';
    compactLines.Add(Format('Before mapping: %s', [valueText]));

    if detectedSection <> '' then
      valueText := detectedSection
    else
      valueText := '(none)';
    compactLines.Add(Format('Final section: %s', [valueText]));

    TDocVariant.New(resultDoc);
    TDocVariantData(resultDoc).AddValue('success', True);
    TDocVariantData(resultDoc).AddValue('error', '');
    TDocVariantData(resultDoc).AddValue('section', UTF8Encode(detectedSection));
    TDocVariant.New(debugDoc);
    TDocVariantData(debugDoc).AddValue('release', UTF8Encode(rls));
    TDocVariantData(debugDoc).AddValue('inputDirect', UTF8Encode(inputDirect));
    TDocVariantData(debugDoc).AddValue('sectionDirect', UTF8Encode(sectionDirect));
    TDocVariantData(debugDoc).AddValue('usedReplace', usedReplace);
    TDocVariantData(debugDoc).AddValue('replaceChanged', replaceChanged);
    TDocVariantData(debugDoc).AddValue('resolution', UTF8Encode(resolutionMode));
    TDocVariantData(debugDoc).AddValue('inputAfterReplace', UTF8Encode(inputAfterReplace));
    TDocVariantData(debugDoc).AddValue('sectionAfterReplace', UTF8Encode(sectionAfterReplace));
    TDocVariantData(debugDoc).AddValue('sectionBeforeMapping', UTF8Encode(sectionBeforeMapping));
    TDocVariantData(debugDoc).AddValue('sectionAfterMapping', UTF8Encode(detectedSection));
    TDocVariantData(debugDoc).AddValue('mappingChanged', sectionBeforeMapping <> detectedSection);
    if (debugLines <> nil) and (debugLines.Count > 0) then
      TDocVariantData(debugDoc).AddValue('trace', UTF8Encode(debugLines.Text))
    else
      TDocVariantData(debugDoc).AddValue('trace', UTF8Encode(''));
    if (compactLines <> nil) and (compactLines.Count > 0) then
      TDocVariantData(debugDoc).AddValue('compactTrace', UTF8Encode(compactLines.Text))
    else
      TDocVariantData(debugDoc).AddValue('compactTrace', UTF8Encode(''));
    TDocVariantData(resultDoc).AddValue('debug', debugDoc);
    Result := VariantSaveJSON(resultDoc);
  except
    on E: Exception do
    begin
      Debug(dpError, 'slapi', Format('[EXCEPTION] DetectSection: %s', [E.Message]));
      TDocVariant.New(resultDoc);
      TDocVariantData(resultDoc).AddValue('success', False);
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(E.Message));
      TDocVariantData(resultDoc).AddValue('section', '');
      Result := VariantSaveJSON(resultDoc);
    end;
  end;
  if mappedLines <> nil then
    mappedLines.Free;
  if mappingSections <> nil then
    mappingSections.Free;
  if replaceLines <> nil then
    replaceLines.Free;
  if compactLines <> nil then
    compactLines.Free;
  if debugLines <> nil then
    debugLines.Free;
end;

{ TApiImdbServiceImpl }

function TApiImdbServiceImpl.GetAllImdbRecords(out Response: TApiImdbRecordList): boolean;
var
  dbRecord: TIMDbDataRecord;
  recordsArray: TDocVariantData;
  recordItem: variant;
begin
  Result := False;
  Response := TApiImdbRecordList.Create;

  try
    recordsArray.InitFast(dvArray);

    if ImdbDatabase = nil then
    begin
      Debug(dpError, section, '[IMDB API] ImdbDatabase is nil');
      Response.Total := 0;
      Response.Records := '[]';
      Result := True;
      Exit;
    end;

    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, '1=1 ORDER BY UpdatedTime DESC', [], []);
    try
      while dbRecord.FillOne do
      begin
        TDocVariant.New(recordItem);
        TDocVariantData(recordItem).AddValue('ImdbId', dbRecord.IMDbID);
        TDocVariantData(recordItem).AddValue('Title', dbRecord.IMDbTitle);
        TDocVariantData(recordItem).AddValue('Year', dbRecord.IMDbYear);
        TDocVariantData(recordItem).AddValue('Rating', dbRecord.IMDbRating);
        TDocVariantData(recordItem).AddValue('Votes', dbRecord.IMDbVotes);
        TDocVariantData(recordItem).AddValue('Genres', dbRecord.IMDbGenres);
        TDocVariantData(recordItem).AddValue('Countries', dbRecord.IMDbCountries);
        TDocVariantData(recordItem).AddValue('Languages', dbRecord.IMDbLanguages);
        TDocVariantData(recordItem).AddValue('ImdbType', dbRecord.IMDbType);
        TDocVariantData(recordItem).AddValue('CreationTime', DateTimeToUnix(dbRecord.CreationTime, False));
        TDocVariantData(recordItem).AddValue('UpdatedTime', DateTimeToUnix(dbRecord.UpdatedTime, False));
        recordsArray.AddItem(recordItem);
      end;
    finally
      dbRecord.Free;
    end;

    Response.Total := recordsArray.Count;
    Response.Records := recordsArray.ToJSON;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetAllImdbRecords: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiImdbServiceImpl.GetImdbRecordById(const ImdbId: RawUTF8; out Response: TApiImdbRecord): boolean;
var
  dbRecord: TIMDbDataRecord;
begin
  Result := False;
  Response := TApiImdbRecord.Create;

  try
    if ImdbDatabase = nil then
    begin
      Debug(dpError, section, '[IMDB API] ImdbDatabase is nil');
      Exit;
    end;

    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [ImdbId]);
    try
      if dbRecord.FillOne then
      begin
        Response.ImdbId := dbRecord.IMDbID;
        Response.Title := dbRecord.IMDbTitle;
        Response.Year := dbRecord.IMDbYear;
        Response.Rating := dbRecord.IMDbRating;
        Response.Votes := dbRecord.IMDbVotes;
        Response.Genres := dbRecord.IMDbGenres;
        Response.Countries := dbRecord.IMDbCountries;
        Response.Languages := dbRecord.IMDbLanguages;
        Response.ImdbType := dbRecord.IMDbType;
        Response.CreationTime := dbRecord.CreationTime;
        Response.UpdatedTime := dbRecord.UpdatedTime;
        Result := True;
      end;
    finally
      dbRecord.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] GetImdbRecordById: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiImdbServiceImpl.CreateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                                              const Genres, Countries, Languages, ImdbType: RawUTF8;
                                              out NewId: RawUTF8): boolean;
var
  dbRecord: TIMDbDataRecord;
  currentTime: TDateTime;
  titleCleaned: string;
begin
  Result := False;
  NewId := '';

  try
    if ImdbDatabase = nil then
    begin
      Debug(dpError, section, '[IMDB API] ImdbDatabase is nil');
      Exit;
    end;

    // Check if record with this IMDB ID already exists
    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [ImdbId]);
    if dbRecord.FillOne then
    begin
      dbRecord.Free;
      Debug(dpError, section, Format('[IMDB API] Record with IMDB ID %s already exists', [UTF8ToString(ImdbId)]));
      Exit;
    end;
    dbRecord.Free;

    currentTime := Now;
    titleCleaned := getMovieNameWithoutSceneTags(UTF8ToString(Title));

    dbRecord := TIMDbDataRecord.Create;
    try
      dbRecord.IMDbID := ImdbId;
      dbRecord.IMDbTitle := Title;
      dbRecord.IMDbTitleCleaned := UTF8Encode(titleCleaned);
      dbRecord.IMDbYear := Year;
      dbRecord.IMDbRating := Rating;
      dbRecord.IMDbVotes := Votes;
      dbRecord.IMDbGenres := Genres;
      dbRecord.IMDbCountries := Countries;
      dbRecord.IMDbLanguages := Languages;
      dbRecord.IMDbType := ImdbType;
      dbRecord.CreationTime := currentTime;
      dbRecord.UpdatedTime := currentTime;

      ImdbDatabase.Add(dbRecord, True);
      NewId := ImdbId;
      Result := True;
      Debug(dpSpam, section, Format('[IMDB API] Created new record: %s', [UTF8ToString(ImdbId)]));
    finally
      dbRecord.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateImdbRecord: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiImdbServiceImpl.UpdateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                                              const Genres, Countries, Languages, ImdbType: RawUTF8): boolean;
var
  dbRecord: TIMDbDataRecord;
  titleCleaned: string;
begin
  Result := False;

  try
    if ImdbDatabase = nil then
    begin
      Debug(dpError, section, '[IMDB API] ImdbDatabase is nil');
      Exit;
    end;

    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [ImdbId]);
    try
      if dbRecord.FillOne then
      begin
        titleCleaned := getMovieNameWithoutSceneTags(UTF8ToString(Title));

        dbRecord.IMDbTitle := Title;
        dbRecord.IMDbTitleCleaned := UTF8Encode(titleCleaned);
        dbRecord.IMDbYear := Year;
        dbRecord.IMDbRating := Rating;
        dbRecord.IMDbVotes := Votes;
        dbRecord.IMDbGenres := Genres;
        dbRecord.IMDbCountries := Countries;
        dbRecord.IMDbLanguages := Languages;
        dbRecord.IMDbType := ImdbType;
        dbRecord.UpdatedTime := Now;

        ImdbDatabase.Update(dbRecord);
        Result := True;
        Debug(dpSpam, section, Format('[IMDB API] Updated record: %s', [UTF8ToString(ImdbId)]));
      end
      else
      begin
        Debug(dpError, section, Format('[IMDB API] Record with IMDB ID %s not found', [UTF8ToString(ImdbId)]));
      end;
    finally
      dbRecord.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] UpdateImdbRecord: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TApiImdbServiceImpl.DeleteImdbRecord(const ImdbId: RawUTF8): boolean;
var
  dbRecord: TIMDbDataRecord;
begin
  Result := False;

  try
    if ImdbDatabase = nil then
    begin
      Debug(dpError, section, '[IMDB API] ImdbDatabase is nil');
      Exit;
    end;

    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [ImdbId]);
    try
      if dbRecord.FillOne then
      begin
        Result := ImdbDatabase.Delete(TIMDbDataRecord, dbRecord.IDValue);
        Debug(dpSpam, section, Format('[IMDB API] Deleted record: %s', [UTF8ToString(ImdbId)]));
      end
      else
      begin
        Debug(dpError, section, Format('[IMDB API] Record with IMDB ID %s not found', [UTF8ToString(ImdbId)]));
      end;
    finally
      dbRecord.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] DeleteImdbRecord: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

{ TV Database Initialization }
procedure InitTVDatabase;
var
  dbPath: String;
begin
  if TVDatabase <> nil then
    Exit;

  dbPath := Trim(config.ReadString('tasktvinfo', 'database', 'tvinfos.db'));
  if dbPath = '' then
    dbPath := 'tvinfos.db';
  if ExtractFilePath(dbPath) = '' then
    dbPath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim + dbPath;
  TVDBModel := TSQLModel.Create([TInfos, TSeries]);
  TVDatabase := TSQLRestClientDB.Create(TVDBModel, nil, dbPath, TSQLRestServerDB, False, '');
  // Don't create missing tables - use existing database as-is
  TVDatabase.DB.LockingMode := lmNormal;
  TVDatabase.DB.Synchronous := smNormal;
end;

{ TV Service Implementation }

function TApiTVServiceImpl.GetAllTVRecords(out Response: TApiTVRecordList): boolean;
var
  recordsJson: RawJSON;
  totalCount: Integer;
begin
  Result := False;
  Response := TApiTVRecordList.Create;

  try
    if not TVInfoDbAlive then
      dbTVInfoStart;

    if not TVInfoDbAlive then
    begin
      Debug(dpError, section, '[TV API] TV info database is not available');
      Response.Total := 0;
      Response.Records := '[]';
      Result := True;
      Exit;
    end;

    getTVInfoRecords(recordsJson, totalCount);
    Response.Total := totalCount;
    Response.Records := recordsJson;
    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[GetAllTVRecords] Exception: ' + E.Message);
      Response.Free;
      Response := nil;
    end;
  end;
end;

function TApiTVServiceImpl.GetTVRecordById(const TVMazeId: RawUTF8; out Response: TApiTVRecord): boolean;
var
  tvInfo: TTVInfoDB;
begin
  Result := False;
  Response := TApiTVRecord.Create;
  try
    if not TVInfoDbAlive then
      dbTVInfoStart;

    tvInfo := getTVInfoByShowID(UTF8ToString(TVMazeId));
    if tvInfo = nil then
      Exit;
    try
      Response.TVMazeId := UTF8Encode(tvInfo.tvmaze_id);
      Response.Showname := UTF8Encode(tvInfo.tv_showname);
      Response.Country := UTF8Encode(tvInfo.tv_country);
      Response.Status := UTF8Encode(tvInfo.tv_status);
      Response.Classification := UTF8Encode(tvInfo.tv_classification);
      Response.Network := UTF8Encode(tvInfo.tv_network);
      Response.Genre := UTF8Encode(tvInfo.tv_genres.CommaText);
      Response.Language := UTF8Encode(tvInfo.tv_language);
      Response.PremieredYear := tvInfo.tv_premiered_year;
      Response.Rating := tvInfo.tv_rating;
    finally
      tvInfo.Free;
    end;

    Result := True;
  except
    on E: Exception do
    begin
      Debug(dpError, section, '[GetTVRecordById] Exception: ' + E.Message);
      Response.Free;
      Response := nil;
    end;
  end;
end;

function TApiTVServiceImpl.CreateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                                          Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer;
                                          out NewId: RawUTF8): boolean;
var
  tvmazeIdInt: Integer;
begin
  Result := False;
  try
    if not TryStrToInt(TVMazeId, tvmazeIdInt) then
      Exit;

    if not TVInfoDbAlive then
      dbTVInfoStart;
    upsertTVInfoRecord(UTF8ToString(TVMazeId), UTF8ToString(Country), UTF8ToString(Status),
      UTF8ToString(Classification), UTF8ToString(Network), UTF8ToString(Genre), UTF8ToString(Language),
      PremieredYear, Rating);
    upsertTVInfoSeries(UTF8ToString(TVMazeId), UTF8ToString(Showname));

    NewId := TVMazeId;
    Result := True;
  except
    on E: Exception do
      Debug(dpError, section, '[CreateTVRecord] Exception: ' + E.Message);
  end;
end;

function TApiTVServiceImpl.UpdateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                                          Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer): boolean;
var
  tvmazeIdInt: Integer;
begin
  Result := False;
  try
    if not TryStrToInt(TVMazeId, tvmazeIdInt) then
      Exit;

    if not TVInfoDbAlive then
      dbTVInfoStart;
    upsertTVInfoRecord(UTF8ToString(TVMazeId), UTF8ToString(Country), UTF8ToString(Status),
      UTF8ToString(Classification), UTF8ToString(Network), UTF8ToString(Genre), UTF8ToString(Language),
      PremieredYear, Rating);
    upsertTVInfoSeries(UTF8ToString(TVMazeId), UTF8ToString(Showname));

    Result := True;
  except
    on E: Exception do
      Debug(dpError, section, '[UpdateTVRecord] Exception: ' + E.Message);
  end;
end;

function TApiTVServiceImpl.DeleteTVRecord(const TVMazeId: RawUTF8): boolean;
var
  deleteResult: Integer;
begin
  Result := False;
  try
    if not TVInfoDbAlive then
      dbTVInfoStart;

    deleteResult := deleteTVInfoByID(UTF8ToString(TVMazeId));
    Result := deleteResult = 1;
  except
    on E: Exception do
      Debug(dpError, section, '[DeleteTVRecord] Exception: ' + E.Message);
  end;
end;

{ TApiConfigServiceImpl }

function TApiConfigServiceImpl.GetConfigList: RawJSON;
var
  files: TDocVariantData;
  baseDir: string;
begin
  files.Init(JSON_FAST, dvArray);
  baseDir := ExtractFilePath(ParamStr(0));

  if FileExists(baseDir + 'slftp.ini') then files.AddItem('slftp.ini');
  if FileExists(baseDir + 'slftp.spamconf') then files.AddItem('slftp.spamconf');
  if FileExists(baseDir + 'slftp.scheduler') then files.AddItem('slftp.scheduler');
  if FileExists(baseDir + 'slftp.skip') then files.AddItem('slftp.skip');
  if FileExists(baseDir + 'slftp.imdbcountries') then files.AddItem('slftp.imdbcountries');
  if FileExists(baseDir + 'slftp.imdbreplace') then files.AddItem('slftp.imdbreplace');
  if FileExists(baseDir + 'slftp.knowngroups') then files.AddItem('slftp.knowngroups');
  if FileExists(baseDir + 'slftp.precatcher') then files.AddItem('slftp.precatcher');
  if FileExists(baseDir + 'slftp.rules') then files.AddItem('slftp.rules');
  if FileExists(baseDir + 'slftp.languagebase') then files.AddItem('slftp.languagebase');
  if FileExists(baseDir + 'slftp.skipgroups') then files.AddItem('slftp.skipgroups');

  Result := files.ToJSON;
end;

function TApiConfigServiceImpl.GetConfigContent(const Filename: RawUTF8): RawJSON;
var
  fn, baseDir: string;
begin
  baseDir := ExtractFilePath(ParamStr(0));
  fn := UTF8ToString(Filename);
  
  // Security check: only allow known files
  if (fn <> 'slftp.ini') and (fn <> 'slftp.spamconf') and (fn <> 'slftp.scheduler') and 
     (fn <> 'slftp.skip') and (fn <> 'slftp.imdbcountries') and (fn <> 'slftp.imdbreplace') and 
     (fn <> 'slftp.knowngroups') and (fn <> 'slftp.precatcher') and (fn <> 'slftp.rules') and
     (fn <> 'slftp.languagebase') and (fn <> 'slftp.skipgroups') then
  begin
     Result := '""';
     Exit;
  end;

  if FileExists(baseDir + fn) then
    Result := VariantSaveJSON(StringFromFile(baseDir + fn))
  else
    Result := '""';
end;

function TApiConfigServiceImpl.SaveConfigContent(const Filename, Content: RawUTF8): boolean;
var
  fn, baseDir: string;
begin
  Result := False;
  baseDir := ExtractFilePath(ParamStr(0));
  fn := UTF8ToString(Filename);

  // Security check
   if (fn <> 'slftp.ini') and (fn <> 'slftp.spamconf') and (fn <> 'slftp.scheduler') and 
     (fn <> 'slftp.skip') and (fn <> 'slftp.imdbcountries') and (fn <> 'slftp.imdbreplace') and 
     (fn <> 'slftp.knowngroups') and (fn <> 'slftp.precatcher') and (fn <> 'slftp.rules') and
     (fn <> 'slftp.languagebase') and (fn <> 'slftp.skipgroups') then
     Exit;

  try
    FileFromString(Content, baseDir + fn);
    Result := True;
  except
    on E: Exception do
      Debug(dpError, section, '[SaveConfigContent] ' + E.Message);
  end;
end;

function TApiConfigServiceImpl.ReloadConfig(const Filename: RawUTF8): boolean;
var
  fn: string;
begin
  Result := False;
  fn := UTF8ToString(Filename);

  if fn = 'slftp.precatcher' then
  begin
    PrecatcherReload;
    Result := True;
  end
  else if (fn = 'slftp.skip') or (fn = 'slftp.skipgroups') then
  begin
    SkiplistRehash;
    Rehashglobalskiplist;
    Result := True;
  end
  else if fn = 'slftp.rules' then
  begin
    RulesReload;
    Result := True;
  end
  else if fn = 'slftp.languagebase' then
  begin
    // sllanguagebase.SLLanguagesReload;
    Result := True;
  end
  else if fn = 'slftp.knowngroups' then
  begin
    KnownGroupsStart();
    Result := True;
  end
  else if (fn = 'slftp.imdbcountries') or (fn = 'slftp.imdbreplace') then
  begin
    dbaddimdbReload;
    Result := True;
  end
  else if fn = 'slftp.spamconf' then
  begin
    UninitmRdOHConfigFiles;
    InitmRdOHConfigFiles;
    Result := True;
  end;
  // slftp.ini and slftp.scheduler currently require restart
end;

function GetHelpDocsRoot: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'docs';
end;

function IsHelpDocNameSafe(const Name: string): boolean;
begin
  Result := (Name <> '') and (ExtractFileExt(Name) = '') and
    (Pos('/', Name) = 0) and (Pos('\', Name) = 0) and (Pos('..', Name) = 0);
end;

function TApiHelpServiceImpl.GetHelpDocs: RawJSON;
var
  files: TDocVariantData;
  root: string;
  sr: TSearchRec;
begin
  files.Init(JSON_FAST, dvArray);
  root := GetHelpDocsRoot;

  if FindFirst(IncludeTrailingPathDelimiter(root) + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = 0 then
      begin
        if IsHelpDocNameSafe(sr.Name) then
          files.AddItem(StringToUtf8(sr.Name));
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;

  Result := files.ToJSON;
end;

function TApiHelpServiceImpl.GetHelpDocContent(const Name: RawUTF8): RawJSON;
var
  root, docName, fullPath: string;
begin
  Result := '""';
  root := GetHelpDocsRoot;
  docName := UTF8ToString(Name);

  if not IsHelpDocNameSafe(docName) then
    Exit;

  fullPath := IncludeTrailingPathDelimiter(root) + docName;
  if FileExists(fullPath) then
    Result := VariantSaveJSON(StringFromFile(fullPath));
end;

function TApiHelpServiceImpl.SearchHelpDocs(const Query: RawUTF8): RawJSON;
var
  files: TDocVariantData;
  root, queryLower, nameLower, contentLower, content: string;
  sr: TSearchRec;
begin
  files.Init(JSON_FAST, dvArray);
  root := GetHelpDocsRoot;
  queryLower := LowerCase(UTF8ToString(Query));

  if FindFirst(IncludeTrailingPathDelimiter(root) + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = 0 then
      begin
        if not IsHelpDocNameSafe(sr.Name) then
          Continue;

        nameLower := LowerCase(sr.Name);
        if (queryLower = '') or (Pos(queryLower, nameLower) > 0) then
        begin
          files.AddItem(StringToUtf8(sr.Name));
          Continue;
        end;

        content := StringFromFile(IncludeTrailingPathDelimiter(root) + sr.Name);
        contentLower := LowerCase(content);
        if Pos(queryLower, contentLower) > 0 then
          files.AddItem(StringToUtf8(sr.Name));
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;

  Result := files.ToJSON;
end;

initialization
  glSiteCreditsCacheLock := TSlCriticalSection2.Create('ApiSiteCreditsCache');
  glSiteCreditsCache := TDictionary<string, TSiteCreditsCacheEntry>.Create;
  glBrowserCacheLock := TSlCriticalSection2.Create('ApiBrowserCache');
  glBrowserCache := TObjectDictionary<string, TBrowserCacheEntry>.Create([doOwnsValues]);
  glPrecatcherDebugCaptureLock := TSlCriticalSection2.Create('ApiPrecatcherDebugCapture');
  GlApiTaskToPazoIdLock := TSLCriticalSection2.Create('ApiTaskMap');
  GlApiTaskToPazoId := TDictionary<Int64, Integer>.Create;

finalization
  glSiteCreditsCache.Free;
  glSiteCreditsCacheLock.Free;
  glBrowserCache.Free;
  glBrowserCacheLock.Free;
  glPrecatcherDebugCaptureLock.Free;
  GlApiTaskToPazoId.Free;
  GlApiTaskToPazoIdLock.Free;

  if TVDatabase <> nil then
  begin
    TVDatabase.Free;
    TVDBModel.Free;
  end;

end.
