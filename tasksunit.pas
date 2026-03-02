unit tasksunit;

interface

uses Classes, globals;

type
  // this is all or part of the job
  TTask = class
  private
    FIsNotifyTask: Boolean;
  public
    site1: String;
    ssite1: Pointer;
    slot1: Pointer;
    slot1name: String;
    site2: String;
    ssite2: Pointer;
    slot2: Pointer;
    slot2name: String;

    netname, channel: String;

    dontremove: Boolean;
    wantedslot: String;

    created: TDateTime; //< datetime when it was added
    assigned: TDateTime;
    startat: TDateTime; //< datetime when it should start (it's delayed)

    response: String;
    announce: String;

    ready: Boolean; //< ready to free because the task is done
    readyerror: Boolean;

    uid: UInt64;
    time: TDateTime; //< some time value

    wanted_up: Boolean;
    wanted_dn: Boolean;

    TryToAssign : Integer;

    constructor Create(const netname, channel, site1: String); overload;
    constructor Create(const netname, channel, site1, site2: String); overload;

    function Execute(slot: Pointer): Boolean; virtual; abstract;

    // the slot parameter is the calling slot here
    function Name: String; virtual; abstract;
    function Fullname: String; virtual;
    function UidText: String;
    function ScheduleText: String;
    function IsReadyToBeExecuted: boolean; virtual;
    procedure DebugTask;
    procedure EnableNotify;
    property IsNotifyTask: Boolean read FIsNotifyTask;
  end;

procedure Tasks_Init;
procedure Tasks_Uninit;

function ShouldWaitForComplete(const aDirType: TDirType; const aWaitTypes: String): boolean;

const
  MaxNumberErrors = 3;

var
  GlConvertFilenamesToLowercase: boolean;
  GlTaskSiteNfoReaddAttempts: integer;
  GlTaskSiteNfoReaddInterval: integer;
  GlTaskPretimeReaddAttempts: integer;
  GlTaskPretimeReaddInterval: integer;
  GlTaskRaceAutoRuleAdd: boolean;
  GlTaskRaceBadCrcEvents: integer;
  GlTaskRaceAutoRemoveDeniedRoutes: boolean;
  GlPostCrcErrorsToIRC: boolean;
  glMaxSimCooldownSpam: Boolean;         //< Cache for spamcfg 'taskrace'/'maxsim_cooldown'
  glTaskraceFilenameNotAllowed: Boolean; //< Cache for spamcfg 'taskrace'/'filename_not_allowed'
  glTaskraceCantCreateDir: Boolean;      //< Cache for spamcfg 'taskrace'/'cant_create_dir'
  glTaskraceDenyingCreationOf: Boolean;  //< Cache for spamcfg 'taskrace'/'denying_creation_of'
  glTaskraceMkdir: Boolean;              //< Cache for spamcfg 'taskrace'/'mkdir'
  glWaitForCompleteSubdirTypes: String;

implementation

uses SysUtils, Contnrs, SyncObjs, debugunit, queueunit, sitesunit, configunit, notify, mrdohutils;

const
  section = 'tasks';

var
  uidg: UInt64 = 1;
  uid_lock: TCriticalSection;

function CsvContainsToken(const aCsv, aToken: String): boolean;
var
  list: TStringList;
  i: integer;
begin
  Result := False;
  if (aCsv = '') or (aToken = '') then
    Exit;

  list := TStringList.Create;
  try
    list.Delimiter := ',';
    list.StrictDelimiter := True;
    list.DelimitedText := LowerCase(aCsv);
    for i := 0 to list.Count - 1 do
    begin
      if Trim(list[i]) = LowerCase(aToken) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    list.Free;
  end;
end;

function ShouldWaitForComplete(const aDirType: TDirType; const aWaitTypes: String): boolean;
begin
  case aDirType of
    IsSample: Result := CsvContainsToken(aWaitTypes, 'sample');
    IsProof: Result := CsvContainsToken(aWaitTypes, 'proof');
    IsSubs: Result := CsvContainsToken(aWaitTypes, 'subs');
    IsCovers: Result := CsvContainsToken(aWaitTypes, 'covers');
  else
    Result := False;
  end;
end;

constructor TTask.Create(const netname, channel, site1: String);
begin
  Create(netname, channel, site1, '');
end;

constructor TTask.Create(const netname, channel, site1, site2: String);
begin
  created := Now();
  assigned := 0;
  self.netname := netname;
  self.channel := channel;
  time := 0;
  TryToAssign := 0;

  response := '';
  wantedslot := '';
  slot1 := nil;
  slot2 := nil;
  self.site1 := site1;
  self.site2 := site2;

  ready := False;
  readyerror := False;

  startat := 0;
  announce := '';
  slot1name := '';
  slot2name := '';
  FIsNotifyTask := False;

  ssite1 := FindSiteByName('', site1);
  if ssite1 = nil then
    readyerror := True;

  if site2 <> '' then
  begin
    ssite2 := FindSiteByName('', site2);
    if ssite2 = nil then
      readyerror := True;
  end;

  uid_lock.Enter;
  try
    uid := uidg;
    inc(uidg);
  finally
    uid_lock.Leave;
  end;
end;

procedure TTask.DebugTask;
begin
  Debug(dpSpam, section, '%s', [Fullname]);
end;

function TTask.Fullname: String;
begin
  try
    Result := Format('#%d (%s): %s [%d] [%d]', [uid, site1, name, TryToAssign, Ord(IsReadyToBeExecuted)]);
  except
    Result := 'TTask';
  end;
end;

function TTask.ScheduleText: String;
begin
  Result := '';
  if startat <> 0 then
    Result := ' ' + TimeToStr(startat);
end;

function TTask.UidText: String;
begin
  Result := '#' + IntToStr(uid);
end;


function TTask.IsReadyToBeExecuted: boolean;
begin
  Result := True;
end;

procedure TTask.EnableNotify;
begin
  FIsNotifyTask := True;
end;

procedure Tasks_Init;
begin
  uid_lock := TCriticalSection.Create;
  GlConvertFilenamesToLowercase := config.ReadBool('taskrace', 'convert_filenames_to_lowercase', True);
  GlTaskSiteNfoReaddAttempts := config.ReadInteger('tasksitenfo', 'readd_attempts', 5);
  GlTaskSiteNfoReaddInterval := config.ReadInteger('tasksitenfo', 'readd_interval', 3);
  GlTaskPretimeReaddAttempts := config.ReadInteger('taskpretime', 'readd_attempts', 5);
  GlTaskPretimeReaddInterval := config.ReadInteger('taskpretime', 'readd_interval', 3);
  GlTaskRaceAutoRuleAdd := config.ReadBool('taskrace', 'autoruleadd', True);
  GlTaskRaceBadCrcEvents := config.ReadInteger('taskrace', 'badcrcevents', 15);
  GlTaskRaceAutoRemoveDeniedRoutes := config.ReadBool('taskrace', 'auto_remove_denied_routes', False);
  GlPostCrcErrorsToIRC := spamcfg.readbool('taskrace', 'crc_error', True);
  glMaxSimCooldownSpam := spamcfg.ReadBool('taskrace', 'maxsim_cooldown', True);
  glTaskraceFilenameNotAllowed := spamcfg.ReadBool('taskrace', 'filename_not_allowed', True);
  glTaskraceCantCreateDir := spamcfg.ReadBool('taskrace', 'cant_create_dir', True);
  glTaskraceDenyingCreationOf := spamcfg.ReadBool('taskrace', 'denying_creation_of', True);
  glTaskraceMkdir := spamcfg.ReadBool('taskrace', 'mkdir', True);
  glWaitForCompleteSubdirTypes := config.ReadString('taskrace', 'wait_for_complete_subdir_types', '');
end;

procedure Tasks_Uninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  uid_lock.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

end.