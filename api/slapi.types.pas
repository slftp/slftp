unit slapi.types;

interface

uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.orm.base;

type
  { API Response Types }

  { Base API response with status }
  TApiResponse = class(TOrm)
  private
    FSuccess: boolean;
    FMessage: RawUTF8;
    FTimestamp: TDateTime;
  published
    property Success: boolean read FSuccess write FSuccess;
    property Message: RawUTF8 read FMessage write FMessage;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  public
    constructor Create; override;
  end;

  { API Error response }
  TApiError = class(TApiResponse)
  private
    FErrorCode: RawUTF8;
    FDetails: RawUTF8;
  published
    property ErrorCode: RawUTF8 read FErrorCode write FErrorCode;
    property Details: RawUTF8 read FDetails write FDetails;
  end;

  { System Status Response }
  TApiSystemStatus = class(TOrm)
  private
    FVersion: RawUTF8;
    FUptime: Int64;
    FSitesCount: integer;
    FSitesUp: integer;
    FSitesDown: integer;
    FQueueSize: integer;
    FQueueSizeMax: integer;
    FActiveTasks: integer;
    FLoadAvgCurrent1: Double;
    FLoadAvgCurrent5: Double;
    FLoadAvgCurrent15: Double;
    FLoadAvgPeak1: Double;
    FLoadAvgPeak5: Double;
    FLoadAvgPeak15: Double;
    FLoadAvgAvailable: boolean;
    FCpuLoadCurrent: integer;
    FCpuLoadMax: integer;
    FCpuLoadAvailable: boolean;
    FPerformanceLevel: integer;
    FDirlistPerSecond: Double;
    FDirlistPerSecondMax: Double;
  published
    property Version: RawUTF8 read FVersion write FVersion;
    property Uptime: Int64 read FUptime write FUptime;
    property SitesCount: integer read FSitesCount write FSitesCount;
    property SitesUp: integer read FSitesUp write FSitesUp;
    property SitesDown: integer read FSitesDown write FSitesDown;
    property QueueSize: integer read FQueueSize write FQueueSize;
    property QueueSizeMax: integer read FQueueSizeMax write FQueueSizeMax;
    property ActiveTasks: integer read FActiveTasks write FActiveTasks;
    property LoadAvgCurrent1: Double read FLoadAvgCurrent1 write FLoadAvgCurrent1;
    property LoadAvgCurrent5: Double read FLoadAvgCurrent5 write FLoadAvgCurrent5;
    property LoadAvgCurrent15: Double read FLoadAvgCurrent15 write FLoadAvgCurrent15;
    property LoadAvgPeak1: Double read FLoadAvgPeak1 write FLoadAvgPeak1;
    property LoadAvgPeak5: Double read FLoadAvgPeak5 write FLoadAvgPeak5;
    property LoadAvgPeak15: Double read FLoadAvgPeak15 write FLoadAvgPeak15;
    property LoadAvgAvailable: boolean read FLoadAvgAvailable write FLoadAvgAvailable;
    property CpuLoadCurrent: integer read FCpuLoadCurrent write FCpuLoadCurrent;
    property CpuLoadMax: integer read FCpuLoadMax write FCpuLoadMax;
    property CpuLoadAvailable: boolean read FCpuLoadAvailable write FCpuLoadAvailable;
    property PerformanceLevel: integer read FPerformanceLevel write FPerformanceLevel;
    property DirlistPerSecond: Double read FDirlistPerSecond write FDirlistPerSecond;
    property DirlistPerSecondMax: Double read FDirlistPerSecondMax write FDirlistPerSecondMax;
  end;

  { Site Status Enum }
  TApiSiteStatus = (
    asUnknown,
    asUp,
    asDown,
    asTempDown,
    asMarkedDown
  );

  { Site Info Response }
  TApiSiteInfo = class(TOrm)
  private
    FName: RawUTF8;
    FUsername: RawUTF8;
    FStatus: RawUTF8;
    FSlots: integer;
    FFreeSlots: integer;
    FSslEnabled: boolean;
    FSslMethod: integer;
    FSslFxp: integer;
    FSiteFullName: RawUTF8;
    FSiteLinkSpeed: RawUTF8;
    FSiteSize: RawUTF8;
    FSiteNotes: RawUTF8;
    FIdent: RawUTF8;
    FSiteInfos: RawUTF8;
    FMaxUpPerRip: integer;
    FFeatures: RawUTF8;
    FBncs: RawUTF8;
    FAffils: RawUTF8;
    FMaxIdle: integer;
    FIdleInterval: integer;
    FLegacyCwd: boolean;
    FAutoBncTestInterval: integer;
    FAutoDirlistInterval: integer;
    FAutoIndexInterval: integer;
    FAutoNukeInterval: integer;
    FCountry: RawUTF8;
    FDirlistPriority: integer;
    FNewdirDirlistReadd: integer;
    FGlobalDirlistInterval: integer;
    FPerformanceAdjustedDirlist: boolean;
    FSkipBeingUploadedFiles: integer;
    FKillConnectionOnStalledTransferSeconds: integer;
    FDestinationQueueLimit: integer;
    FUseForNFOdownload: integer;
  published
    property Name: RawUTF8 read FName write FName;
    property Username: RawUTF8 read FUsername write FUsername;
    property Status: RawUTF8 read FStatus write FStatus;
    property Slots: integer read FSlots write FSlots;
    property FreeSlots: integer read FFreeSlots write FFreeSlots;
    property SslEnabled: boolean read FSslEnabled write FSslEnabled;
    property SslMethod: integer read FSslMethod write FSslMethod;
    property SslFxp: integer read FSslFxp write FSslFxp;
    property SiteFullName: RawUTF8 read FSiteFullName write FSiteFullName;
    property SiteLinkSpeed: RawUTF8 read FSiteLinkSpeed write FSiteLinkSpeed;
    property SiteSize: RawUTF8 read FSiteSize write FSiteSize;
    property SiteNotes: RawUTF8 read FSiteNotes write FSiteNotes;
    property Ident: RawUTF8 read FIdent write FIdent;
    property SiteInfos: RawUTF8 read FSiteInfos write FSiteInfos;
    property MaxUpPerRip: integer read FMaxUpPerRip write FMaxUpPerRip;
    property Features: RawUTF8 read FFeatures write FFeatures;
    property Bncs: RawUTF8 read FBncs write FBncs;
    property Affils: RawUTF8 read FAffils write FAffils;
    property MaxIdle: integer read FMaxIdle write FMaxIdle;
    property IdleInterval: integer read FIdleInterval write FIdleInterval;
    property LegacyCwd: boolean read FLegacyCwd write FLegacyCwd;
    property AutoBncTestInterval: integer read FAutoBncTestInterval write FAutoBncTestInterval;
    property AutoDirlistInterval: integer read FAutoDirlistInterval write FAutoDirlistInterval;
    property AutoIndexInterval: integer read FAutoIndexInterval write FAutoIndexInterval;
    property AutoNukeInterval: integer read FAutoNukeInterval write FAutoNukeInterval;
    property Country: RawUTF8 read FCountry write FCountry;
    property DirlistPriority: integer read FDirlistPriority write FDirlistPriority;
    property NewdirDirlistReadd: integer read FNewdirDirlistReadd write FNewdirDirlistReadd;
    property GlobalDirlistInterval: integer read FGlobalDirlistInterval write FGlobalDirlistInterval;
    property PerformanceAdjustedDirlist: boolean read FPerformanceAdjustedDirlist write FPerformanceAdjustedDirlist;
    property SkipBeingUploadedFiles: integer read FSkipBeingUploadedFiles write FSkipBeingUploadedFiles;
    property KillConnectionOnStalledTransferSeconds: integer read FKillConnectionOnStalledTransferSeconds write FKillConnectionOnStalledTransferSeconds;
    property DestinationQueueLimit: integer read FDestinationQueueLimit write FDestinationQueueLimit;
    property UseForNFOdownload: integer read FUseForNFOdownload write FUseForNFOdownload;
  end;

  { Text File Response (e.g. rules files) }
  TApiTextFile = class(TOrm)
  private
    FSiteName: RawUTF8;
    FPath: RawUTF8;
    FExists: boolean;
    FMd5: RawUTF8;
    FContent: RawUTF8;
  published
    property SiteName: RawUTF8 read FSiteName write FSiteName;
    property Path: RawUTF8 read FPath write FPath;
    property Exists: boolean read FExists write FExists;
    property Md5: RawUTF8 read FMd5 write FMd5;
    property Content: RawUTF8 read FContent write FContent;
  end;

  { Rules validation response }
  TApiRulesValidation = class(TOrm)
  private
    FOk: boolean;
    FErrors: RawJSON;
  published
    property Ok: boolean read FOk write FOk;
    property Errors: RawJSON read FErrors write FErrors;
  end;

  { Rules save response }
  TApiRulesSaveResult = class(TOrm)
  private
    FOk: boolean;
    FMessage: RawUTF8;
    FPath: RawUTF8;
    FMd5: RawUTF8;
    FErrors: RawJSON;
  published
    property Ok: boolean read FOk write FOk;
    property Message: RawUTF8 read FMessage write FMessage;
    property Path: RawUTF8 read FPath write FPath;
    property Md5: RawUTF8 read FMd5 write FMd5;
    property Errors: RawJSON read FErrors write FErrors;
  end;

  { Sites List Response }
  TApiSitesList = class(TOrm)
  private
    FTotal: integer;
    FUp: integer;
    FDown: integer;
    FSites: RawJSON;
  published
    property Total: integer read FTotal write FTotal;
    property Up: integer read FUp write FUp;
    property Down: integer read FDown write FDown;
    property Sites: RawJSON read FSites write FSites;
  end;

  { Site credits/ratio as returned by SITE STAT }
  TApiSiteCredits = class(TOrm)
  private
    FSiteName: RawUTF8;
    FOk: boolean;
    FMessage: RawUTF8;
    FCredits: RawUTF8;
    FRatio: RawUTF8;
    FStatLine: RawUTF8;
  published
    property SiteName: RawUTF8 read FSiteName write FSiteName;
    property Ok: boolean read FOk write FOk;
    property Message: RawUTF8 read FMessage write FMessage;
    property Credits: RawUTF8 read FCredits write FCredits;
    property Ratio: RawUTF8 read FRatio write FRatio;
    property StatLine: RawUTF8 read FStatLine write FStatLine;
  end;

  /// Site user info as returned by SITE USER
  TApiSiteUserInfo = class(TOrm)
  private
    FSiteName: RawUTF8;
    FUserName: RawUTF8;
    FOk: boolean;
    FMessage: RawUTF8;
    FOutput: RawUTF8;
  published
    property SiteName: RawUTF8 read FSiteName write FSiteName;
    property UserName: RawUTF8 read FUserName write FUserName;
    property Ok: boolean read FOk write FOk;
    property Message: RawUTF8 read FMessage write FMessage;
    property Output: RawUTF8 read FOutput write FOutput;
  end;

  { Task Status }
  TApiTaskStatus = (
    atsPending,
    atsInProgress,
    atsCompleted,
    atsFailed
  );

  { Task Info }
  TApiTaskInfo = class(TOrm)
  private
    FUid: Int64;
    FType: RawUTF8;
    FSite1: RawUTF8;
    FSite2: RawUTF8;
    FStatus: RawUTF8;
    FCreated: TDateTime;
    FStarted: TDateTime;
    FCompleted: TDateTime;
    FName: RawUTF8;
    FError: RawUTF8;
  published
    property Uid: Int64 read FUid write FUid;
    property TaskType: RawUTF8 read FType write FType;
    property Site1: RawUTF8 read FSite1 write FSite1;
    property Site2: RawUTF8 read FSite2 write FSite2;
    property Status: RawUTF8 read FStatus write FStatus;
    property Created: TDateTime read FCreated write FCreated;
    property Started: TDateTime read FStarted write FStarted;
    property Completed: TDateTime read FCompleted write FCompleted;
    property Name: RawUTF8 read FName write FName;
    property Error: RawUTF8 read FError write FError;
  end;

  { Queue Stats }
  TApiQueueStats = class(TOrm)
  private
    FTotalTasks: integer;
    FRaceTasks: integer;
    FDirlistTasks: integer;
    FAutoTasks: integer;
    FOtherTasks: integer;
  published
    property TotalTasks: integer read FTotalTasks write FTotalTasks;
    property RaceTasks: integer read FRaceTasks write FRaceTasks;
    property DirlistTasks: integer read FDirlistTasks write FDirlistTasks;
    property AutoTasks: integer read FAutoTasks write FAutoTasks;
    property OtherTasks: integer read FOtherTasks write FOtherTasks;
  end;

  { Site Routes }
  TApiSiteRoutes = class(TOrm)
  private
    FRoutes: RawJSON;
  published
    property Routes: RawJSON read FRoutes write FRoutes;
  end;

  { Rank Info }
  TApiRankInfo = class(TOrm)
  private
    FSitename: RawUTF8;
    FSection: RawUTF8;
    FScore: integer;
  published
    property Sitename: RawUTF8 read FSitename write FSitename;
    property Section: RawUTF8 read FSection write FSection;
    property Score: integer read FScore write FScore;
  end;

  { IRC Network Info }
  TApiIrcNetwork = class(TOrm)
  private
    FName: RawUTF8;
    FConnected: boolean;
    FChannels: integer;
    FCurrentServer: RawUTF8;
    FHost: RawUTF8;
    FPort: integer;
    FStatus: RawUTF8;
    FNickname: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName;
    property Connected: boolean read FConnected write FConnected;
    property Channels: integer read FChannels write FChannels;
    property CurrentServer: RawUTF8 read FCurrentServer write FCurrentServer;
    property Host: RawUTF8 read FHost write FHost;
    property Port: integer read FPort write FPort;
    property Status: RawUTF8 read FStatus write FStatus;
    property Nickname: RawUTF8 read FNickname write FNickname;
  end;

  { IRC Network Config }
  TApiIrcNetworkConfig = class(TOrm)
  private
    FName: RawUTF8;
    FHost: RawUTF8;
    FPort: integer;
    FSsl: boolean;
    FPassword: RawUTF8;
    FNick: RawUTF8;
    FIdent: RawUTF8;
    FUser: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName;
    property Host: RawUTF8 read FHost write FHost;
    property Port: integer read FPort write FPort;
    property Ssl: boolean read FSsl write FSsl;
    property Password: RawUTF8 read FPassword write FPassword;
    property Nick: RawUTF8 read FNick write FNick;
    property Ident: RawUTF8 read FIdent write FIdent;
    property User: RawUTF8 read FUser write FUser;
  end;

  { Speed Test Result }
  TApiSpeedTestResult = class(TOrm)
  private
    FSourceSite: RawUTF8;
    FDestSite: RawUTF8;
    FSpeed: Double;
    FTimestamp: TDateTime;
  published
    property SourceSite: RawUTF8 read FSourceSite write FSourceSite;
    property DestSite: RawUTF8 read FDestSite write FDestSite;
    property Speed: Double read FSpeed write FSpeed;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

  { Release Site Info - which site has the release }
  TApiReleaseSite = class(TOrm)
  private
    FSiteName: RawUTF8;
    FComplete: boolean;
    FFileCount: integer;
    FPercent: integer;
  published
    property SiteName: RawUTF8 read FSiteName write FSiteName;
    property Complete: boolean read FComplete write FComplete;
    property FileCount: integer read FFileCount write FFileCount;
    property Percent: integer read FPercent write FPercent;
  end;

  { Release Site Details }
  TApiReleaseSiteDetail = class(TOrm)
  private
    FSiteName: RawUTF8;
    FComplete: boolean;
    FFileCount: integer;
    FTotalFiles: integer;
    FFilesRacedByMe: integer;
    FPercent: Double;
    FStatus: RawUTF8;
  published
    property SiteName: RawUTF8 read FSiteName write FSiteName;
    property Complete: boolean read FComplete write FComplete;
    property FileCount: integer read FFileCount write FFileCount;
    property TotalFiles: integer read FTotalFiles write FTotalFiles;
    property FilesRacedByMe: integer read FFilesRacedByMe write FFilesRacedByMe;
    property Percent: Double read FPercent write FPercent;
    property Status: RawUTF8 read FStatus write FStatus;
  end;

  { Release Info for Dashboard }
  TApiReleaseInfo = class(TOrm)
  private
    FReleaseName: RawUTF8;
    FSection: RawUTF8;
    FAdded: TDateTime;
    FPazoId: integer;
    FReady: boolean;
    FStopped: boolean;
    FQueueNumber: integer;
    FSiteDetails: RawJSON; // JSON array of TApiReleaseSiteDetail
    FTotalFiles: integer;
    FErrorReason: RawUTF8;
    FDetectedToAddedMs: integer;
    FAddedToDirlistMs: integer;
    FAddedToFirstTaskMs: integer;
    FAddedToLastTaskMs: integer;
  published
    property ReleaseName: RawUTF8 read FReleaseName write FReleaseName;
    property Section: RawUTF8 read FSection write FSection;
    property Added: TDateTime read FAdded write FAdded;
    property PazoId: integer read FPazoId write FPazoId;
    property Ready: boolean read FReady write FReady;
    property Stopped: boolean read FStopped write FStopped;
    property QueueNumber: integer read FQueueNumber write FQueueNumber;
    property SiteDetails: RawJSON read FSiteDetails write FSiteDetails;
    property TotalFiles: integer read FTotalFiles write FTotalFiles;
    property ErrorReason: RawUTF8 read FErrorReason write FErrorReason;
    property DetectedToAddedMs: integer read FDetectedToAddedMs write FDetectedToAddedMs;
    property AddedToDirlistMs: integer read FAddedToDirlistMs write FAddedToDirlistMs;
    property AddedToFirstTaskMs: integer read FAddedToFirstTaskMs write FAddedToFirstTaskMs;
    property AddedToLastTaskMs: integer read FAddedToLastTaskMs write FAddedToLastTaskMs;
  end;

  { List of Releases }
  TApiReleasesList = class(TOrm)
  private
    FReleases: RawJSON;
    FTotal: integer;
  published
    property Releases: RawJSON read FReleases write FReleases;
    property Total: integer read FTotal write FTotal;
  end;

  { Issues Summary }
  TApiIssuesSummary = class(TOrm)
  private
    FWindowSeconds: integer;
    FTotal: integer;
    FSkip: integer;
    FDontMatch: integer;
    FMissingSection: integer;
    FNuke: integer;
  published
    property WindowSeconds: integer read FWindowSeconds write FWindowSeconds;
    property Total: integer read FTotal write FTotal;
    property Skip: integer read FSkip write FSkip;
    property DontMatch: integer read FDontMatch write FDontMatch;
    property MissingSection: integer read FMissingSection write FMissingSection;
    property Nuke: integer read FNuke write FNuke;
  end;

  { List of Issues }
  TApiIssuesList = class(TOrm)
  private
    FTotal: integer;
    FIssues: RawJSON;
  published
    property Total: integer read FTotal write FTotal;
    property Issues: RawJSON read FIssues write FIssues;
  end;

  { IMDB Record }
  TApiImdbRecord = class(TOrm)
  private
    FImdbId: RawUTF8;
    FTitle: RawUTF8;
    FYear: integer;
    FRating: integer;
    FVotes: integer;
    FGenres: RawUTF8;
    FCountries: RawUTF8;
    FLanguages: RawUTF8;
    FImdbType: RawUTF8;
    FCreationTime: TDateTime;
    FUpdatedTime: TDateTime;
  published
    property ImdbId: RawUTF8 read FImdbId write FImdbId;
    property Title: RawUTF8 read FTitle write FTitle;
    property Year: integer read FYear write FYear;
    property Rating: integer read FRating write FRating;
    property Votes: integer read FVotes write FVotes;
    property Genres: RawUTF8 read FGenres write FGenres;
    property Countries: RawUTF8 read FCountries write FCountries;
    property Languages: RawUTF8 read FLanguages write FLanguages;
    property ImdbType: RawUTF8 read FImdbType write FImdbType;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property UpdatedTime: TDateTime read FUpdatedTime write FUpdatedTime;
  end;

  { List of IMDB Records }
  TApiImdbRecordList = class(TOrm)
  private
    FTotal: integer;
    FRecords: RawJSON;
  published
    property Total: integer read FTotal write FTotal;
    property Records: RawJSON read FRecords write FRecords;
  end;

  { TV Show Record }
  TApiTVRecord = class(TOrm)
  private
    FTVMazeId: RawUTF8;
    FShowname: RawUTF8;
    FCountry: RawUTF8;
    FStatus: RawUTF8;
    FClassification: RawUTF8;
    FNetwork: RawUTF8;
    FGenre: RawUTF8;
    FLanguage: RawUTF8;
    FPremieredYear: integer;
    FRating: integer;
    FLastUpdated: integer;
    FCreatedAt: integer;
  published
    property TVMazeId: RawUTF8 read FTVMazeId write FTVMazeId;
    property Showname: RawUTF8 read FShowname write FShowname;
    property Country: RawUTF8 read FCountry write FCountry;
    property Status: RawUTF8 read FStatus write FStatus;
    property Classification: RawUTF8 read FClassification write FClassification;
    property Network: RawUTF8 read FNetwork write FNetwork;
    property Genre: RawUTF8 read FGenre write FGenre;
    property Language: RawUTF8 read FLanguage write FLanguage;
    property PremieredYear: integer read FPremieredYear write FPremieredYear;
    property Rating: integer read FRating write FRating;
    property LastUpdated: integer read FLastUpdated write FLastUpdated;
    property CreatedAt: integer read FCreatedAt write FCreatedAt;
  end;

  { List of TV Show Records }
  TApiTVRecordList = class(TOrm)
  private
    FTotal: integer;
    FRecords: RawJSON;
  published
    property Total: integer read FTotal write FTotal;
    property Records: RawJSON read FRecords write FRecords;
  end;

  { ORM Models for TV Database }
  TInfos = class(TOrm)
  private
    FTVMazeId: Integer;
    FTVDbId: Integer;
    FTVRageId: Integer;
    FPremieredYear: Integer;
    FCountry: RawUTF8;
    FStatus: RawUTF8;
    FClassification: RawUTF8;
    FNetwork: RawUTF8;
    FGenre: RawUTF8;
    FEndedYear: Integer;
    FLastUpdated: Integer;
    FNextDate: Integer;
    FNextSeason: Integer;
    FNextEpisode: Integer;
    FRating: Integer;
    FAirdays: RawUTF8;
    FTVLanguage: RawUTF8;
  published
    property TVMazeId: Integer index 1 read FTVMazeId write FTVMazeId stored AS_UNIQUE;
    property TVDbId: Integer read FTVDbId write FTVDbId;
    property TVRageId: Integer read FTVRageId write FTVRageId;
    property PremieredYear: Integer read FPremieredYear write FPremieredYear;
    property Country: RawUTF8 read FCountry write FCountry;
    property Status: RawUTF8 read FStatus write FStatus;
    property Classification: RawUTF8 read FClassification write FClassification;
    property Network: RawUTF8 read FNetwork write FNetwork;
    property Genre: RawUTF8 read FGenre write FGenre;
    property EndedYear: Integer read FEndedYear write FEndedYear;
    property LastUpdated: Integer read FLastUpdated write FLastUpdated;
    property NextDate: Integer read FNextDate write FNextDate;
    property NextSeason: Integer read FNextSeason write FNextSeason;
    property NextEpisode: Integer read FNextEpisode write FNextEpisode;
    property Rating: Integer read FRating write FRating;
    property Airdays: RawUTF8 read FAirdays write FAirdays;
    property TVLanguage: RawUTF8 read FTVLanguage write FTVLanguage;
  end;

  TSeries = class(TOrm)
  private
    FRip: RawUTF8;
    FShowname: RawUTF8;
    FRipCountry: RawUTF8;
    FTVMazeUrl: RawUTF8;
    FTVMazeIdRef: Integer;
  published
    property Rip: RawUTF8 index 1 read FRip write FRip stored AS_UNIQUE;
    property Showname: RawUTF8 read FShowname write FShowname;
    property RipCountry: RawUTF8 read FRipCountry write FRipCountry;
    property TVMazeUrl: RawUTF8 read FTVMazeUrl write FTVMazeUrl;
    property TVMazeIdRef: Integer read FTVMazeIdRef write FTVMazeIdRef;
  end;

implementation

uses
  SysUtils,
  DateUtils;

{$I ../slftp.inc}

{ TApiResponse }

constructor TApiResponse.Create;
begin
  inherited Create;
  FTimestamp := Now;
  FSuccess := True;
end;

end.
