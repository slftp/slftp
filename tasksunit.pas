unit tasksunit;

interface

uses Classes;

type
  // ez az ose az osszes feladatnak
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

    readydel: Boolean;
    readydelat: TDateTime;

    uid: Integer;
    ido: TDateTime;

    dependencies: TStringList;

    wanted_up: Boolean;
    wanted_dn: Boolean;

    TryToAssign : Integer;

    constructor Create(const netname, channel: String; site1: String); overload;
    constructor Create(const netname, channel: String; site1, site2: String); overload;
    destructor Destroy; override;



    function Execute(slot: Pointer): Boolean; virtual; abstract;

    // a slot parameter itt a calling slot
    function Name: String; virtual; abstract;
    function Fullname: String; virtual;
    function UidText: String;
    function ScheduleText: String;
    procedure DebugTask;
  end;

procedure Tasks_Init;
procedure Tasks_Uninit;
function FindTaskByUidText(uidtext: String): TTask;


implementation

uses SysUtils, SyncObjs, debugunit, queueunit, sitesunit;

const
  section = 'tasks';

var
  uidg: Integer = 1;
  uid_lock: TCriticalSection;

constructor TTask.Create(const netname, channel: String; site1: String);
begin
  Create(netname, channel, site1, '');
end;

constructor TTask.Create(const netname, channel: String; site1, site2: String);
begin
  created := Now();
  assigned := 0;
  self.netname := netname;
  self.channel := channel;
  ido := 0;
  TryToAssign := 0;

  response := '';
  wantedslot := '';
  slot1 := nil;
  slot2 := nil;
  self.site1 := site1;
  self.site2 := site2;

  ready := False;
  readyerror := False;
  readydel := False;

  startat := 0;
  announce := '';
  slot1name := '';
  slot2name := '';
  dependencies := TStringList.Create;

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

destructor TTask.Destroy;
begin
  dependencies.Free;

  inherited;
end;

function TTask.Fullname: String;
var
  s_dep: String;
begin
  try
    s_dep := Format('[%s]', [dependencies.DelimitedText]);
    Result := Format('#%d (%s): %s [%d] %s', [uid, site1, name, TryToAssign, s_dep]);
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

function FindTaskByUidText(uidtext: String): TTask;
var
  i: Integer;
begin
  Result := nil;

  for i := tasks.Count - 1 downto 0 do
  begin
    try
      if ((TTask(tasks[i]).UidText = uidtext) and (not TTask(tasks[i]).ready) and (not TTask(tasks[i]).readyerror)) then
      begin
        Result := TTask(tasks[i]);
        break;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] FindTaskByUidText: %s', [e.Message]));
        Result := nil;
      end;
    end;
  end;
end;

end.
