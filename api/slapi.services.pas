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

    /// GET /api/system/auto
    /// Returns global precatcher auto status
    function GetAutoStatus: boolean;

    /// POST /api/system/auto
    /// Sets global precatcher auto status
    function SetAutoStatus(Enabled: boolean): boolean;
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

    /// POST /api/sites/{name}/credits
    /// Executes SITE STAT and returns parsed credits/ratio (cached for ~1h unless ForceRefresh is true)
    function GetSiteCredits(const SiteName: RawUTF8; ForceRefresh: boolean; out Credits: TApiSiteCredits): boolean;

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

    /// PATCH /api/sites/{name}/affils
    /// Replace site affils list (whitespace delimited)
    function SetSiteAffils(const SiteName, Affils: RawUTF8): boolean;

    /// PATCH /api/sites/{name}/ircnick
    /// Sets IRC nickname for site
    function SetSiteIrcNick(const SiteName, IrcNick: RawUTF8): boolean;

    /// POST /api/sites/{name}/autorules/run
    /// Run autorules once now
    function RunSiteAutoRules(const SiteName: RawUTF8): boolean;

    /// GET /api/sites/{name}/routes
    /// Returns speed routes for a site
    function GetSiteRoutes(const SiteName: RawUTF8; out Routes: TApiSiteRoutes): boolean;

    function SetSiteRoute(const SourceSite, DestSite: RawUTF8; Speed: integer;
                          Locked, AffilOnly, NoAffil: boolean): boolean;

    /// POST /api/sites/{name}/test
    /// Tests site connection
    function TestSite(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/resolve
    /// Resolves a hostname to an IP address
    function ResolveHostname(const Hostname: RawUTF8): RawUTF8;

    /// POST /api/sites/{name}/ghost
    /// Kills ghost connections
    function GhostSite(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/{name}/recalc
    /// Recalculate freeslots
    function RecalcFreeSlots(const SiteName: RawUTF8): boolean;

    /// POST /api/sites/{name}/rebuildslots
    /// Rebuilds all slots (reset slot objects)
    function RebuildSlots(const SiteName: RawUTF8): boolean;

    /// POST /api/ExecuteIrcCommand
    /// Executes an IRC command via console
    function ExecuteIrcCommand(const Command: RawUTF8): boolean;

    function GetSiteInfo(const SiteName: RawUTF8; out Info: TApiSiteInfo): boolean;

    function SetSiteCredentials(const SiteName: RawUTF8;
                                const Username, Password: RawUTF8;
                                const BncsJson: RawUTF8;
                                MaxIdle, IdleInterval: integer;
                                LegacyCwd: boolean;
                                SslFxp: integer): boolean;

    /// POST /api/sites/{name}/config
    /// Sets various site configuration options (autobnctest, autodirlist, country, etc.)
    function SetSiteConfig(const SiteName: RawUTF8; const Config: RawJSON): boolean;

    function GetAvailableSections: RawJSON;
    function GetSiteSections(const SiteName: RawUTF8): RawJSON;
    function SetSiteSection(const SiteName, Section, Dir: RawUTF8): boolean;

    /// Loads incoming rules file for a site from rtpl/<site>.rtpl (or admin file for '*')
    function GetSiteRtpl(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;

    /// Loads the cached SITE RULES snapshot (rtpl/<site>.siterules or rules/<site>.rules)
    function GetSiteRulesSnapshot(const SiteName: RawUTF8; out FileInfo: TApiTextFile): boolean;

    /// Validates rtpl file content (line-by-line parser errors)
    function ValidateRtpl(const Content: RawUTF8; out Validation: TApiRulesValidation): boolean;

    /// Saves incoming rules file and optionally reloads rules in memory
    function SaveSiteRtpl(const SiteName: RawUTF8; const Content: RawUTF8; const ExpectedMd5: RawUTF8;
      Reload: boolean; out SaveResult: TApiRulesSaveResult): boolean;

    /// Reload rules from disk (rtpl/*.rtpl and slftp.rules)
    function ReloadRules: boolean;

    /// Returns list of rule condition handlers with operators and descriptions
    function GetRuleConditions: RawJSON;
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

  { Live Races API }
  IApiRacesService = interface(IInvokable)
    ['{A4A0E0F5-2D0D-4C6F-AE9C-8A62A5B736C8}']

    /// GET /api/races
    /// Returns recent raced file entries from stats DB (paged)
    function GetRaces(const Page: integer; const PageSize: integer; const SinceUnix: Int64): RawJSON;

    /// Returns raced file entries for a given release (paged)
    function GetReleaseTransfers(const Release: RawUTF8; const Page: integer; const PageSize: integer; const SinceUnix: Int64): RawJSON;
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

    /// POST /api/irc/channel/blowkey
    /// Sets blowfish key for channel
    function SetChannelBlowkey(const NetName, Channel, Blowkey: RawUTF8): boolean;

    /// POST /api/irc/channel/chankey
    /// Sets channel key
    function SetChannelKey(const NetName, Channel, ChanKey: RawUTF8): boolean;

    /// POST /api/irc/channel/roles
    /// Sets channel roles
    function SetChannelRoles(const NetName, Channel, Roles: RawUTF8): boolean;

    /// POST /api/irc/channel/add
    /// Adds new channel
    function AddChannel(const NetName, Channel, ChanKey, Blowkey, Roles: RawUTF8): boolean;

    /// DELETE /api/irc/channel
    /// Deletes channel
    function DeleteChannel(const NetName, Channel: RawUTF8): boolean;

    /// POST /api/irc/networks
    /// Adds a new IRC network
    function AddNetwork(const NetName, Host: RawUTF8; Port: integer; Ssl: boolean; const Password, Nick, Ident, User: RawUTF8): boolean;

    /// DELETE /api/irc/networks/{name}
    /// Deletes an IRC network
    function DeleteNetwork(const NetName: RawUTF8): boolean;
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
    /// Starts local speed test, returns Test ID
    function TestSpeedLocal(const SiteName: RawUTF8): RawUTF8;

    /// POST /api/speed/test/out
    /// Starts outbound speed test, returns Test ID
    function TestSpeedOut(const SourceSite: RawUTF8;
                          const DestSites: RawUTF8): RawUTF8;

    /// POST /api/speed/test/in
    /// Starts inbound speed test, returns Test ID
    function TestSpeedIn(const DestSite: RawUTF8;
                         const SourceSites: RawUTF8): RawUTF8;

    /// POST /api/speed/test/cleanup
    /// Starts cleanup, returns Test ID
    function TestSpeedCleanup(const Sites: RawUTF8): RawUTF8;

    /// POST /api/speed/test/matrix
    /// Starts matrix speed test (filtered sites), returns Test ID
    function TestSpeedMatrix(const IncludeSites: RawUTF8 = '';
                             const ExcludeSites: RawUTF8 = ''): RawUTF8;

    /// GET /api/speed/sites
    /// Returns array of sites with SPEEDTEST section
    function GetSpeedTestSites: RawJSON;

    /// GET /api/speed/test/{id}/log
    /// Returns live log for test
    function GetTestLog(const TestId: RawUTF8): RawJSON;

    /// GET /api/speed/test/{id}/status
    /// Returns status for test
    function GetTestStatus(const TestId: RawUTF8): RawJSON;

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

    /// PUT /api/precatcher/{id}
    /// Updates precatcher rule
    function UpdatePrecatcherRule(RuleId: integer; const RuleData: RawJSON): boolean;

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

    /// GET /api/precatcher/hits
    /// Returns recent precatcher matches (reverse chronological)
    function GetHits(const Limit: integer; const SinceUnix: Int64;
      const ReleaseName: RawUTF8; const SiteName: RawUTF8): RawJSON;

    /// GET /api/precatcher/config
    /// Returns slftp.precatcher file content with MD5
    function GetPrecatcherConfig: RawJSON;

    /// POST /api/precatcher/config/validate
    /// Validates precatcher config syntax
    function ValidatePrecatcherConfig(const Content: RawJSON): RawJSON;

    /// POST /api/precatcher/config/save
    /// Saves precatcher config with MD5 conflict detection and optional reload
    function SavePrecatcherConfig(const Content: RawJSON; const ExpectedMd5: RawUTF8; Reload: boolean): RawJSON;

    /// GET /api/precatcher/helpers
    /// Returns helper data (section names, mapping templates) for click-to-insert
    function GetPrecatcherHelpers: RawJSON;
  end;

  { Simulator API }
  IApiSimulatorService = interface(IInvokable)
    ['{2C3D4E5F-6A7B-8C9D-0E1F-2A3B4C5D6E7F}']

    /// POST /api/simulate
    /// Simulates routing and rule evaluation for a release
    function Simulate(const Section, ReleaseName: RawUTF8; const SimulatePre: boolean): RawJSON;

    /// POST /api/simulate/detect-section
    /// Detects section from release name using precatcher logic
    function DetectSection(const ReleaseName: RawUTF8): RawJSON;
  end;

  { Issues / Alerts API }
  IApiIssuesService = interface(IInvokable)
    ['{1B2C3D4E-5F6A-7B8C-9D0E-1F2A3B4C5D6E}']

    /// GET /api/issues/summary
    /// Returns counts within a time window
    function GetSummary(const WindowSeconds: integer; out Response: TApiIssuesSummary): boolean;

    /// GET /api/issues
    /// Returns recent issues/events (TypesCsv e.g. "SKIP,DONT_MATCH,MISSING_SECTION")
    function GetIssues(const Limit: integer; const SinceUnix: Int64; const TypesCsv: RawUTF8; out Response: TApiIssuesList): boolean;

    /// DELETE /api/issues/{id}
    /// Deletes a specific issue by ID
    function DeleteIssue(const IssueId: Int64): boolean;

    /// DELETE /api/issues
    /// Clears the in-memory issues buffer
    function ClearIssues: boolean;
  end;

  { Log Service API }
  IApiLogService = interface(IInvokable)
    ['{8C3D0E1F-2A4B-3C5D-6E7F-0A1B2C3D4E5F}']

    /// GET /api/logs
    /// Returns recent log lines
    function GetLogs(const Lines: integer): RawJSON;

    /// DELETE /api/logs
    /// Clears logs (if supported) or returns error
    function ClearLogs: boolean;
  end;

  { File Browser API }
  IApiBrowserService = interface(IInvokable)
    ['{9A0B1C2D-3E4F-5A6B-7C8D-9E0F1A2B3C4D}']

    /// GET /api/browser/{site}
    /// Returns directory listing for a specific path
    /// Query params: path (default /), refresh (bool)
    function GetPath(const SiteName: RawUTF8; const Path: RawUTF8; ForceRefresh: boolean): RawJSON;
  end;

  { IMDB Database API }
  IApiImdbService = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7A8B-9C0D-1E2F3A4B5C6D}']

    /// GET /api/imdb
    /// Returns all IMDB records
    function GetAllImdbRecords(out Response: TApiImdbRecordList): boolean;

    /// GET /api/imdb/{id}
    /// Returns IMDB record by ID
    function GetImdbRecordById(const ImdbId: RawUTF8; out Response: TApiImdbRecord): boolean;

    /// POST /api/imdb
    /// Creates new IMDB record
    function CreateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                              const Genres, Countries, Languages, ImdbType: RawUTF8;
                              out NewId: RawUTF8): boolean;

    /// PUT /api/imdb/{id}
    /// Updates existing IMDB record
    function UpdateImdbRecord(const ImdbId, Title: RawUTF8; Year, Rating, Votes: integer;
                              const Genres, Countries, Languages, ImdbType: RawUTF8): boolean;

    /// DELETE /api/imdb/{id}
    /// Deletes IMDB record
    function DeleteImdbRecord(const ImdbId: RawUTF8): boolean;
  end;

  { TV Database API }
  IApiTVService = interface(IInvokable)
    ['{B2C3D4E5-F6A7-8B9C-0D1E-2F3A4B5C6D7E}']

    /// Returns all TV show records
    function GetAllTVRecords(out Response: TApiTVRecordList): boolean;

    /// Returns TV show record by TVMaze ID
    function GetTVRecordById(const TVMazeId: RawUTF8; out Response: TApiTVRecord): boolean;

    /// Creates new TV show record
    function CreateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                            Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer;
                            out NewId: RawUTF8): boolean;

    /// Updates existing TV show record
    function UpdateTVRecord(const TVMazeId, Showname, Country, Status, Classification,
                            Network, Genre, Language: RawUTF8; PremieredYear, Rating: integer): boolean;

    /// Deletes TV show record
    function DeleteTVRecord(const TVMazeId: RawUTF8): boolean;
  end;

implementation

{$I ../slftp.inc}

end.
