unit slapi.types;

interface

uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.orm.core;

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
    FActiveTasks: integer;
  published
    property Version: RawUTF8 read FVersion write FVersion;
    property Uptime: Int64 read FUptime write FUptime;
    property SitesCount: integer read FSitesCount write FSitesCount;
    property SitesUp: integer read FSitesUp write FSitesUp;
    property SitesDown: integer read FSitesDown write FSitesDown;
    property QueueSize: integer read FQueueSize write FQueueSize;
    property ActiveTasks: integer read FActiveTasks write FActiveTasks;
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
    FSslFxp: integer;
    FFeatures: RawUTF8;
    FBncs: RawUTF8;
    FMaxIdle: integer;
    FIdleInterval: integer;
    FLegacyCwd: boolean;
  published
    property Name: RawUTF8 read FName write FName;
    property Username: RawUTF8 read FUsername write FUsername;
    property Status: RawUTF8 read FStatus write FStatus;
    property Slots: integer read FSlots write FSlots;
    property FreeSlots: integer read FFreeSlots write FFreeSlots;
    property SslEnabled: boolean read FSslEnabled write FSslEnabled;
    property SslFxp: integer read FSslFxp write FSslFxp;
    property Features: RawUTF8 read FFeatures write FFeatures;
    property Bncs: RawUTF8 read FBncs write FBncs;
    property MaxIdle: integer read FMaxIdle write FMaxIdle;
    property IdleInterval: integer read FIdleInterval write FIdleInterval;
    property LegacyCwd: boolean read FLegacyCwd write FLegacyCwd;
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

implementation

uses
  SysUtils,
  DateUtils;

{$I slftp.inc}

{ TApiResponse }

constructor TApiResponse.Create;
begin
  inherited Create;
  FTimestamp := Now;
  FSuccess := True;
end;

end.
