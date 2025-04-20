unit tasksunit;

interface

uses Classes;

type
  // this is all or part of the job
  TTask = class
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
  end;

procedure Tasks_Init;
procedure Tasks_Uninit;

const
  MaxNumberErrors = 3;

implementation

uses SysUtils, Contnrs, SyncObjs, debugunit, queueunit, sitesunit;

const
  section = 'tasks';

var
  uidg: UInt64 = 1;
  uid_lock: TCriticalSection;

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

procedure Tasks_Init;
begin
  uid_lock := TCriticalSection.Create;
end;

procedure Tasks_Uninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  uid_lock.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

end.