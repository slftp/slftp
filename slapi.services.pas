unit slapi.services;

interface

uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.soa.core,
  mormot.soa.server,
  slapi.types;

type
  { REST API Service Interfaces - these will be automatically exposed as HTTP endpoints }

  { System & Status API }
  IApiSystemService = interface(IInvokable)
    ['{8F4A1D2E-3B5C-4F6A-9E7D-1C2B3A4D5E6F}']

    /// GET /api/system/status
    /// Returns current system status including uptime, sites, queue
    function GetStatus(out Response: TApiSystemStatus): boolean;

    /// GET /api/system/uptime
    /// Returns uptime in seconds
    function GetUptime: Int64;

    /// GET /api/system/version
    /// Returns slftp version
    function GetVersion: RawUTF8;

    /// POST /api/system/shutdown
    /// Initiates graceful shutdown
    function Shutdown: boolean;

    /// POST /api/system/backup
    /// Creates configuration backup
    function CreateBackup: boolean;

    /// GET /api/system/releases
    /// Returns list of recent releases with sites
    function GetRecentReleases(const Limit: integer; out Response: TApiReleasesList): boolean;

    /// GET /api/system/release/{id}
    /// Returns detailed info for specific release
    function GetReleaseDetails(const PazoId: integer; out Response: TApiReleaseInfo): boolean;
  end;

  { Sites Management API }
  IApiSitesService = interface(IInvokable)
    ['{9A5B2C3D-4E6F-5A7B-8C9D-2E3F4A5B6C7D}']

    /// GET /api/sites
    /// Returns list of all sites with status
    function GetSites(const Filter: RawUTF8; out Sites: TApiSitesList): boolean;

    /// GET /api/sites/{name}
    /// Returns detailed info for specific site
    function GetSite(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;

    /// POST /api/sites
    /// Adds new site
    function AddSite(const Name, Host: RawUTF8; Port: integer;
                     const Username, Password: RawUTF8;
                     SslEnabled: boolean): boolean;

    /// DELETE /api/sites/{name}
    /// Removes site
    function DeleteSite(const SiteName: RawUTF8): boolean;

    /// PATCH /api/sites/{name}/status
    /// Sets site status (up/down)
    function SetSiteStatus(const SiteName: RawUTF8; const Status: RawUTF8): boolean;

    /// PATCH /api/sites/{name}/slots
    /// Sets number of slots
    function SetSiteSlots(const SiteName: RawUTF8; Slots: integer): boolean;

    /// PATCH /api/sites/{name}/maxupdn
    /// Sets max simultaneous uploads/downloads
    function SetSiteMaxUpDn(const SiteName: RawUTF8; MaxUp, MaxDn: integer): boolean;

    /// PATCH /api/sites/{name}/maxpredn
    /// Sets max pre-download tasks (queue limiter)
    function SetSiteMaxPreDn(const SiteName: RawUTF8; MaxPreDn: integer): boolean;

    /// PATCH /api/sites/{name}/permdown
    /// Permanently disable/enable a site
    function SetSitePermDown(const SiteName: RawUTF8; PermDown: boolean): boolean;

    /// PATCH /api/sites/{name}/autologin
    /// Enable/disable autologin
    function SetSiteAutoLogin(const SiteName: RawUTF8; Enabled: boolean): boolean;

    /// PATCH /api/sites/{name}/autorules
    /// Enable/disable autorules interval (seconds, 0 = off)
    function SetSiteAutoRules(const SiteName: RawUTF8; IntervalSeconds: integer): boolean;

    /// POST /api/sites/{name}/autorules/run
    /// Run autorules once now
    function RunSiteAutoRules(const SiteName: RawUTF8): boolean;

    /// GET /api/sites/{name}/routes
    /// Returns speed routes for a site
    function GetSiteRoutes(const SiteName: RawUTF8; out Routes: TApiSiteRoutes): boolean;

    /// POST /api/sites/{name}/test
    /// Tests site connection
    function TestSite(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/{name}/ghost
    /// Kills ghost connections
    function GhostSite(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/{name}/recalc
    /// Recalculate freeslots
    function RecalcFreeSlots(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/{name}/rebuildslots
    /// Rebuilds all slots (reset slot objects)
    function RebuildSlots(const SiteName: RawUTF8): boolean;
  end;

  { Queue & Tasks API }
  IApiQueueService = interface(IInvokable)
    ['{1B6C3D4E-5F7A-6B8C-9D0E-3F4A5B6C7D8E}']

    /// GET /api/queue/stats
    /// Returns queue statistics
    function GetQueueStats(out Stats: TApiQueueStats): boolean;

    /// GET /api/queue
    /// Returns all tasks in queue
    function GetQueue(const SiteName: RawUTF8): RawJSON;

    /// GET /api/tasks/{uid}
    /// Returns task info by UID
    function GetTask(TaskUid: Int64; out Info: TApiTaskInfo): boolean;

    /// POST /api/tasks/dirlist
    /// Creates dirlist task
    function CreateDirlistTask(const SiteName, Section, Dir: RawUTF8): Int64;

    /// POST /api/tasks/spread
    /// Creates spread task
    function CreateSpreadTask(const SourceSite, Section, Release: RawUTF8): Int64;

    /// POST /api/tasks/transfer
    /// Creates FXP transfer task
    function CreateTransferTask(const SourceSite, DestSite, Section,
                                Dir, FileName: RawUTF8): Int64;

    /// DELETE /api/tasks/{uid}
    /// Stops and removes task
    function StopTask(TaskUid: Int64): boolean;

    /// DELETE /api/queue
    /// Empties queue for site
    function EmptyQueue(const SiteName: RawUTF8): boolean;
  end;

  { Stats & Ranks API }
  IApiStatsService = interface(IInvokable)
    ['{2C7D4E5F-6A8B-7C9D-0E1F-4A5B6C7D8E9F}']

    /// GET /api/stats/races
    /// Returns race statistics
    function GetRaceStats(const SiteName, Period: RawUTF8;
                          Detailed: boolean): RawJSON;

    /// GET /api/ranks
    /// Returns site rankings
    function GetRanks(const SiteName: RawUTF8): RawJSON;

    /// PATCH /api/ranks/{site}/{section}
    /// Sets manual rank
    function SetRank(const SiteName, Section: RawUTF8; Score: integer): boolean;

    /// POST /api/ranks/recalc
    /// Recalculates all ranks
    function RecalculateRanks: boolean;
  end;

  { IRC Management API }
  IApiIrcService = interface(IInvokable)
    ['{3D8E5F6A-7B9C-8D0E-1F2A-5B6C7D8E9F0A}']

    /// GET /api/irc/networks
    /// Returns IRC networks status
    function GetNetworks: RawJSON;

    /// GET /api/irc/networks/{name}/status
    /// Returns network connection status
    function GetNetworkStatus(const NetName: RawUTF8;
                              out Info: TApiIrcNetwork): boolean;

    /// GET /api/irc/channels
    /// Returns joined channels
    function GetChannels(const NetName: RawUTF8): RawJSON;

    /// POST /api/irc/say
    /// Sends message to channel
    function SendMessage(const NetName, Channel, Message: RawUTF8): boolean;

    /// POST /api/irc/jump
    /// Jumps to different server
    function JumpServer(const NetName: RawUTF8): boolean;
  end;

  { Rules Engine API }
  IApiRulesService = interface(IInvokable)
    ['{4E9F6A7B-8C0D-9E1F-2A3B-6C7D8E9F0A1B}']

    /// GET /api/rules
    /// Returns all rules
    function GetRules(const SiteName, Section: RawUTF8): RawJSON;

    /// GET /api/rules/{id}
    /// Returns rule details
    function GetRule(RuleId: integer): RawJSON;

    /// POST /api/rules
    /// Adds new rule
    function AddRule(const RuleData: RawJSON): integer;

    /// PUT /api/rules/{id}
    /// Modifies rule
    function ModifyRule(RuleId: integer; const RuleData: RawJSON): boolean;

    /// DELETE /api/rules/{id}
    /// Deletes rule
    function DeleteRule(RuleId: integer): boolean;

    /// POST /api/rules/test
    /// Tests rule against release
    function TestRule(const RuleData, ReleaseName: RawUTF8): boolean;

    /// POST /api/rules/reload
    /// Reloads rules from config
    function ReloadRules: boolean;
  end;

  { Speed & Routes API }
  IApiSpeedService = interface(IInvokable)
    ['{5F0A7B8C-9D1E-0F2A-3B4C-7D8E9F0A1B2C}']

    /// GET /api/routes/{site}
    /// Returns routes for site
    function GetRoutes(const SiteName: RawUTF8): RawJSON;

    /// POST /api/speed/test/local
    /// Starts local speed test
    function TestSpeedLocal(const SiteName: RawUTF8): boolean;

    /// POST /api/speed/test/out
    /// Starts outbound speed test
    function TestSpeedOut(const SourceSite: RawUTF8;
                          const DestSites: RawUTF8): boolean;

    /// POST /api/speed/test/in
    /// Starts inbound speed test
    function TestSpeedIn(const DestSite: RawUTF8;
                         const SourceSites: RawUTF8): boolean;

    /// GET /api/speed/results
    /// Returns speed test results
    function GetSpeedResults(const SiteName: RawUTF8): RawJSON;

    /// POST /api/routes/recalc
    /// Recalculates routes
    function RecalculateRoutes: boolean;
  end;

  { Knowledge Base API }
  IApiKnowledgeBaseService = interface(IInvokable)
    ['{6A1B8C9D-0E2F-1A3B-4C5D-8E9F0A1B2C3D}']

    /// GET /api/kb
    /// Returns KB entries
    function GetKBEntries(const Section: RawUTF8; Limit: integer): RawJSON;

    /// GET /api/kb/search
    /// Searches KB for release
    function SearchKB(const Query: RawUTF8): RawJSON;

    /// POST /api/kb
    /// Adds KB entry
    function AddKBEntry(const Section, Release: RawUTF8): boolean;
  end;

  { Precatcher API }
  IApiPrecatcherService = interface(IInvokable)
    ['{7B2C9D0E-1F3A-2B4C-5D6E-9F0A1B2C3D4E}']

    /// GET /api/precatcher
    /// Returns precatcher rules
    function GetPrecatcherRules: RawJSON;

    /// POST /api/precatcher
    /// Adds precatcher rule
    function AddPrecatcherRule(const RuleData: RawJSON): integer;

    /// DELETE /api/precatcher/{id}
    /// Deletes precatcher rule
    function DeletePrecatcherRule(RuleId: integer): boolean;

    /// POST /api/precatcher/test
    /// Tests precatcher against announce
    function TestPrecatcher(const Announce: RawUTF8): RawJSON;

    /// POST /api/precatcher/reload
    /// Reloads precatcher config
    function ReloadPrecatcher: boolean;

    /// GET /api/precatcher/mappings
    /// Returns section mappings
    function GetMappings: RawJSON;
  end;

implementation

{$I slftp.inc}

end.
