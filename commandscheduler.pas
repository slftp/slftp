unit commandscheduler;

{
  TCommandScheduler separates Dirlist and Mkdir commands from the main task queue.
  
  Instead of creating TPazoDirlistTask / TPazoMkdirTask TTask objects that compete
  with Race tasks for queue slots and sorting CPU, commands are stored as lightweight
  TCommandRequest records in a per-site scheduler.
  
  TSiteSlot.Execute checks the scheduler first (commands before transfers),
  analogous to cbftp's SiteLogic handling STAT/LIST/MKD before the Engine
  issues transfers.
}

interface

uses
  Classes, SysUtils, DateUtils, Generics.Collections, SyncObjs, slcriticalsection2, dirlist, pazo;

type
  TCommandType = (ctDirlist, ctMkdir, ctSfvDownload, ctNfoDownload, ctCwd, ctRaw, ctLogin);

  TCommandRequest = record
    // Identity (for deduplication)
    pazo_id: Integer;
    pazo: TPazo; // direct reference to avoid lookup
    dir: String;
    site: String;

    // Scheduling
    startat: TDateTime;
    priority: Integer;
    created: TDateTime;
    command_type: TCommandType;

    // Execution context
    netname: String;
    channel: String;
    is_pre: Boolean;
    is_from_incomplete_filler: Boolean;
    depending_on_dirlist: TDirList; // for mkdir

    // Command-specific fields
    sfv_filename: String; // for ctSfvDownload
    cmd: String;          // for ctRaw
    attempt: Integer;      // for retry logic (SFV, NFO)

    // Unique id for tracking
    uid: UInt64;

    procedure Init(const aPazo: TPazo; const aDir, aSite: String;
                   const aCommandType: TCommandType; const aStartAt: TDateTime;
                   const aNetname, aChannel: String; const aIsPre: Boolean = False;
                   const aIsFromIncompleteFiller: Boolean = False;
                   const aDependingOnDirlist: TDirList = nil); overload;
  procedure Init(const aPazoID: Integer; const aDir, aSite: String;
                   const aCommandType: TCommandType; const aStartAt: TDateTime;
                   const aNetname, aChannel: String; const aIsPre: Boolean = False;
                   const aIsFromIncompleteFiller: Boolean = False;
                   const aDependingOnDirlist: TDirList = nil); overload;
  end;

  TCommandScheduler = class
  private
    fLock: TSlCriticalSection2;
    fDirlistRequests: TList<TCommandRequest>;
    fMkdirRequests: TList<TCommandRequest>;
    fOtherRequests: TList<TCommandRequest>; // SFV, NFO, CWD, Raw, Login
    fPazoDirCount: TDictionary<Integer, Integer>;
    fUidLock: TCriticalSection;
    fNextUid: UInt64;

    function FindRequestIndex(const aList: TList<TCommandRequest>;
      const aPazoID: Integer; const aDir, aSite: String): Integer; overload;
    function FindRequestIndexEx(const aList: TList<TCommandRequest>;
      const aPazoID: Integer; const aDir, aSite, aExtraKey: String): Integer; overload;
    function GetDirlistCount: Integer;
    function GetMkdirCount: Integer;
    function GetOtherCount: Integer;
    function GetTotalCount: Integer;
    procedure IncrementPazoDirCount(const aPazoID: Integer);
    procedure DecrementPazoDirCount(const aPazoID: Integer);
    function GetPazoDirCount(const aPazoID: Integer): Integer;

    function InternalAddRequest(const aList: TList<TCommandRequest>;
      const aReq: TCommandRequest): Boolean;
    function InternalGetNextRequest(const aList: TList<TCommandRequest>;
      out aReq: TCommandRequest): Boolean;
    procedure InternalCompleteRequest(const aList: TList<TCommandRequest>;
      const aReq: TCommandRequest);
    procedure InternalRemoveByIndex(const aList: TList<TCommandRequest>;
      const aIndex: Integer);
    procedure InternalCleanup(const aList: TList<TCommandRequest>;
      const aMaxAgeMinutes: Integer);
    procedure InternalRemoveByPazo(const aList: TList<TCommandRequest>;
      const aPazoID: Integer);
  public
    constructor Create(const aSiteName: String);
    destructor Destroy; override;

    // Schedule a command. Returns False if duplicate or cap reached.
    function ScheduleDirlist(const aReq: TCommandRequest): Boolean;
    function ScheduleMkdir(const aReq: TCommandRequest): Boolean;
    function ScheduleCommand(const aReq: TCommandRequest): Boolean;

    // Get next ready request (startat <= Now, no dependencies for mkdir)
    function GetNextDirlist(out aReq: TCommandRequest): Boolean;
    function GetNextMkdir(out aReq: TCommandRequest): Boolean;
    function GetNextCommand(const aCommandType: TCommandType; out aReq: TCommandRequest): Boolean;

    // Mark complete / remove
    procedure CompleteDirlist(const aReq: TCommandRequest);
    procedure CompleteMkdir(const aReq: TCommandRequest);
    procedure CompleteCommand(const aReq: TCommandRequest);

    // Cleanup old requests
    procedure Cleanup(const aMaxAgeMinutes: Integer = 15);

    // Remove all requests for a pazo
    procedure RemoveByPazo(const aPazoID: Integer);

    // Check if a request exists
    function HasDirlist(const aPazoID: Integer; const aDir, aSite: String): Boolean;
    function HasMkdir(const aPazoID: Integer; const aDir, aSite: String): Boolean;
    function HasCommand(const aCommandType: TCommandType; const aPazoID: Integer; const aDir: String): Boolean;

    property DirlistCount: Integer read GetDirlistCount;
    property MkdirCount: Integer read GetMkdirCount;
    property OtherCount: Integer read GetOtherCount;
    property TotalCount: Integer read GetTotalCount;
  end;

implementation

uses debugunit;

const
  section = 'commandscheduler';
  cMaxPazoDirCount = 50; // per-pazo dirlist cap to prevent explosion

var
  gUidLock: SyncObjs.TCriticalSection;
  gNextGlobalUid: UInt64 = 1;

function GenerateUid: UInt64;
begin
  gUidLock.Acquire;
  try
    Result := gNextGlobalUid;
    Inc(gNextGlobalUid);
  finally
    gUidLock.Release;
  end;
end;

{ TCommandRequest }

procedure TCommandRequest.Init(const aPazo: TPazo; const aDir, aSite: String;
  const aCommandType: TCommandType; const aStartAt: TDateTime;
  const aNetname, aChannel: String; const aIsPre: Boolean = False;
  const aIsFromIncompleteFiller: Boolean = False;
  const aDependingOnDirlist: TDirList = nil);
begin
  pazo := aPazo;
  if aPazo <> nil then
    pazo_id := aPazo.pazo_id
  else
    pazo_id := -1;
  dir := aDir;
  site := aSite;
  command_type := aCommandType;
  startat := aStartAt;
  netname := aNetname;
  channel := aChannel;
  is_pre := aIsPre;
  is_from_incomplete_filler := aIsFromIncompleteFiller;
  depending_on_dirlist := aDependingOnDirlist;
  sfv_filename := '';
  cmd := '';
  attempt := 0;
  created := Now();
  priority := 0;
  uid := GenerateUid;
end;

procedure TCommandRequest.Init(const aPazoID: Integer; const aDir, aSite: String;
  const aCommandType: TCommandType; const aStartAt: TDateTime;
  const aNetname, aChannel: String; const aIsPre: Boolean = False;
  const aIsFromIncompleteFiller: Boolean = False;
  const aDependingOnDirlist: TDirList = nil);
begin
  pazo := nil;
  pazo_id := aPazoID;
  dir := aDir;
  site := aSite;
  command_type := aCommandType;
  startat := aStartAt;
  netname := aNetname;
  channel := aChannel;
  is_pre := aIsPre;
  is_from_incomplete_filler := aIsFromIncompleteFiller;
  depending_on_dirlist := aDependingOnDirlist;
  sfv_filename := '';
  cmd := '';
  attempt := 0;
  created := Now();
  priority := 0;
  uid := GenerateUid;
end;

{ TCommandScheduler }

constructor TCommandScheduler.Create(const aSiteName: String);
begin
  inherited Create;
  fLock := TSlCriticalSection2.Create(Format('CommandScheduler_%s', [aSiteName]));
  fDirlistRequests := TList<TCommandRequest>.Create;
  fMkdirRequests := TList<TCommandRequest>.Create;
  fOtherRequests := TList<TCommandRequest>.Create;
  fPazoDirCount := TDictionary<Integer, Integer>.Create;
  fNextUid := 1;
end;

destructor TCommandScheduler.Destroy;
begin
  fDirlistRequests.Free;
  fMkdirRequests.Free;
  fOtherRequests.Free;
  fPazoDirCount.Free;
  fLock.Free;
  inherited;
end;

function TCommandScheduler.FindRequestIndex(const aList: TList<TCommandRequest>;
  const aPazoID: Integer; const aDir, aSite: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to aList.Count - 1 do
  begin
    if (aList[i].pazo_id = aPazoID) and (aList[i].dir = aDir) and (aList[i].site = aSite) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCommandScheduler.FindRequestIndexEx(const aList: TList<TCommandRequest>;
  const aPazoID: Integer; const aDir, aSite, aExtraKey: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to aList.Count - 1 do
  begin
    if (aList[i].pazo_id = aPazoID) and (aList[i].dir = aDir) and (aList[i].site = aSite) and (aList[i].cmd = aExtraKey) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TCommandScheduler.GetDirlistCount: Integer;
begin
  fLock.Enter('GetDirlistCount');
  try
    Result := fDirlistRequests.Count;
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetMkdirCount: Integer;
begin
  fLock.Enter('GetMkdirCount');
  try
    Result := fMkdirRequests.Count;
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetOtherCount: Integer;
begin
  fLock.Enter('GetOtherCount');
  try
    Result := fOtherRequests.Count;
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetTotalCount: Integer;
begin
  fLock.Enter('GetTotalCount');
  try
    Result := fDirlistRequests.Count + fMkdirRequests.Count + fOtherRequests.Count;
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.IncrementPazoDirCount(const aPazoID: Integer);
var
  fCount: Integer;
begin
  if not fPazoDirCount.TryGetValue(aPazoID, fCount) then
    fCount := 0;
  fPazoDirCount[aPazoID] := fCount + 1;
end;

procedure TCommandScheduler.DecrementPazoDirCount(const aPazoID: Integer);
var
  fCount: Integer;
begin
  if fPazoDirCount.TryGetValue(aPazoID, fCount) then
  begin
    if fCount <= 1 then
      fPazoDirCount.Remove(aPazoID)
    else
      fPazoDirCount[aPazoID] := fCount - 1;
  end;
end;

function TCommandScheduler.GetPazoDirCount(const aPazoID: Integer): Integer;
begin
  if not fPazoDirCount.TryGetValue(aPazoID, Result) then
    Result := 0;
end;

function TCommandScheduler.InternalAddRequest(
  const aList: TList<TCommandRequest>; const aReq: TCommandRequest): Boolean;
var
  fIdx: Integer;
begin
  Result := False;

  // Deduplicate: reject if same (pazo_id, dir) already exists
  // For Raw commands, also check cmd field
  if aReq.command_type = ctRaw then
    fIdx := FindRequestIndexEx(aList, aReq.pazo_id, aReq.dir, aReq.site, aReq.cmd)
  else
    fIdx := FindRequestIndex(aList, aReq.pazo_id, aReq.dir, aReq.site);

  if fIdx >= 0 then
  begin
    case aReq.command_type of
      ctDirlist:
        Debug(dpSpam, section, '[DEDUP] Rejecting DIRLIST for pazo %d dir %s (already scheduled)',
          [aReq.pazo_id, aReq.dir]);
      ctMkdir:
        Debug(dpSpam, section, '[DEDUP] Rejecting MKDIR for pazo %d dir %s (already scheduled)',
          [aReq.pazo_id, aReq.dir]);
      ctSfvDownload:
        Debug(dpSpam, section, '[DEDUP] Rejecting SFV for pazo %d dir %s file %s (already scheduled)',
          [aReq.pazo_id, aReq.dir, aReq.sfv_filename]);
      ctNfoDownload:
        Debug(dpSpam, section, '[DEDUP] Rejecting NFO for pazo %d dir %s (already scheduled)',
          [aReq.pazo_id, aReq.dir]);
      ctCwd:
        Debug(dpSpam, section, '[DEDUP] Rejecting CWD for dir %s (already scheduled)',
          [aReq.dir]);
      ctRaw:
        Debug(dpSpam, section, '[DEDUP] Rejecting RAW for dir %s cmd %s (already scheduled)',
          [aReq.dir, aReq.cmd]);
      ctLogin:
        Debug(dpSpam, section, '[DEDUP] Rejecting LOGIN for site %s (already scheduled)',
          [aReq.site]);
    end;
    Exit;
  end;

  // Cap check for dirlists
  if (aReq.command_type = ctDirlist) and (GetPazoDirCount(aReq.pazo_id) >= cMaxPazoDirCount) then
  begin
    Debug(dpSpam, section, '[CAP] Rejecting DIRLIST for pazo %d dir %s (cap %d reached)',
      [aReq.pazo_id, aReq.dir, cMaxPazoDirCount]);
    Exit;
  end;

  aList.Add(aReq);

  if aReq.command_type = ctDirlist then
    IncrementPazoDirCount(aReq.pazo_id);

  Result := True;
end;

function TCommandScheduler.InternalGetNextRequest(
  const aList: TList<TCommandRequest>; out aReq: TCommandRequest): Boolean;
var
  i, fBestIdx: Integer;
  fNow: TDateTime;
  fBestReq: TCommandRequest;
begin
  Result := False;
  fBestIdx := -1;
  fNow := Now();

  for i := 0 to aList.Count - 1 do
  begin
    // Skip if not ready yet (delayed start)
    if aList[i].startat > fNow then
      Continue;

    // For mkdir, check dependency
    if (aList[i].command_type = ctMkdir) and (aList[i].depending_on_dirlist <> nil) then
    begin
      if aList[i].depending_on_dirlist.need_mkdir and not aList[i].depending_on_dirlist.error then
        Continue; // dependency not satisfied
    end;

    // Pick by priority, then by creation time (FIFO within same priority)
    if fBestIdx < 0 then
    begin
      fBestIdx := i;
      fBestReq := aList[i];
    end
    else if aList[i].priority < fBestReq.priority then
    begin
      fBestIdx := i;
      fBestReq := aList[i];
    end
    else if (aList[i].priority = fBestReq.priority) and
            (aList[i].created < fBestReq.created) then
    begin
      fBestIdx := i;
      fBestReq := aList[i];
    end;
  end;

  if fBestIdx >= 0 then
  begin
    aReq := fBestReq;
    Result := True;
  end;
end;

procedure TCommandScheduler.InternalCompleteRequest(
  const aList: TList<TCommandRequest>; const aReq: TCommandRequest);
var
  fIdx: Integer;
begin
  fIdx := FindRequestIndex(aList, aReq.pazo_id, aReq.dir, aReq.site);
  if fIdx >= 0 then
  begin
    if aList[fIdx].command_type = ctDirlist then
      DecrementPazoDirCount(aList[fIdx].pazo_id);
    aList.Delete(fIdx);
  end;
end;

procedure TCommandScheduler.InternalRemoveByIndex(
  const aList: TList<TCommandRequest>; const aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex >= aList.Count) then
    Exit;
  if aList[aIndex].command_type = ctDirlist then
    DecrementPazoDirCount(aList[aIndex].pazo_id);
  aList.Delete(aIndex);
end;

procedure TCommandScheduler.InternalCleanup(const aList: TList<TCommandRequest>;
  const aMaxAgeMinutes: Integer);
var
  i: Integer;
  fCutoff: TDateTime;
begin
  fCutoff := IncMinute(Now(), -aMaxAgeMinutes);
  i := aList.Count - 1;
  while i >= 0 do
  begin
    if aList[i].created < fCutoff then
    begin
      if aList[i].command_type = ctDirlist then
        Debug(dpSpam, section, '[CLEANUP] Removing stale DIRLIST for pazo %d dir %s (age %d min)',
          [aList[i].pazo_id, aList[i].dir, MinutesBetween(Now(), aList[i].created)])
      else
        Debug(dpSpam, section, '[CLEANUP] Removing stale MKDIR for pazo %d dir %s (age %d min)',
          [aList[i].pazo_id, aList[i].dir, MinutesBetween(Now(), aList[i].created)]);
      InternalRemoveByIndex(aList, i);
    end;
    Dec(i);
  end;
end;

procedure TCommandScheduler.InternalRemoveByPazo(
  const aList: TList<TCommandRequest>; const aPazoID: Integer);
var
  i: Integer;
begin
  i := aList.Count - 1;
  while i >= 0 do
  begin
    if aList[i].pazo_id = aPazoID then
      InternalRemoveByIndex(aList, i);
    Dec(i);
  end;
end;

function TCommandScheduler.ScheduleDirlist(const aReq: TCommandRequest): Boolean;
begin
  fLock.Enter('ScheduleDirlist');
  try
    Result := InternalAddRequest(fDirlistRequests, aReq);
    if Result then
      Debug(dpSpam, section, '[SCHEDULE] DIRLIST pazo=%d dir=%s site=%s priority=%d',
        [aReq.pazo_id, aReq.dir, aReq.site, aReq.priority]);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.ScheduleMkdir(const aReq: TCommandRequest): Boolean;
begin
  fLock.Enter('ScheduleMkdir');
  try
    Result := InternalAddRequest(fMkdirRequests, aReq);
    if Result then
      Debug(dpSpam, section, '[SCHEDULE] MKDIR pazo=%d dir=%s site=%s',
        [aReq.pazo_id, aReq.dir, aReq.site]);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetNextDirlist(out aReq: TCommandRequest): Boolean;
begin
  fLock.Enter('GetNextDirlist');
  try
    Result := InternalGetNextRequest(fDirlistRequests, aReq);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetNextMkdir(out aReq: TCommandRequest): Boolean;
begin
  fLock.Enter('GetNextMkdir');
  try
    Result := InternalGetNextRequest(fMkdirRequests, aReq);
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.CompleteDirlist(const aReq: TCommandRequest);
begin
  fLock.Enter('CompleteDirlist');
  try
    InternalCompleteRequest(fDirlistRequests, aReq);
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.CompleteMkdir(const aReq: TCommandRequest);
begin
  fLock.Enter('CompleteMkdir');
  try
    InternalCompleteRequest(fMkdirRequests, aReq);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.ScheduleCommand(const aReq: TCommandRequest): Boolean;
begin
  fLock.Enter('ScheduleCommand');
  try
    Result := InternalAddRequest(fOtherRequests, aReq);
    if Result then
      Debug(dpSpam, section, '[SCHEDULE] %s pazo=%d dir=%s site=%s',
        [GetEnumName(TypeInfo(TCommandType), Ord(aReq.command_type)),
         aReq.pazo_id, aReq.dir, aReq.site]);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.GetNextCommand(const aCommandType: TCommandType;
  out aReq: TCommandRequest): Boolean;
var
  i: Integer;
  fNow: TDateTime;
  fBestIdx: Integer;
  fBestReq: TCommandRequest;
begin
  fLock.Enter('GetNextCommand');
  try
    Result := False;
    fBestIdx := -1;
    fNow := Now();

    for i := 0 to fOtherRequests.Count - 1 do
    begin
      if fOtherRequests[i].command_type <> aCommandType then
        Continue;
      if fOtherRequests[i].startat > fNow then
        Continue;

      if fBestIdx < 0 then
      begin
        fBestIdx := i;
        fBestReq := fOtherRequests[i];
      end
      else if fOtherRequests[i].priority < fBestReq.priority then
      begin
        fBestIdx := i;
        fBestReq := fOtherRequests[i];
      end
      else if (fOtherRequests[i].priority = fBestReq.priority) and
              (fOtherRequests[i].created < fBestReq.created) then
      begin
        fBestIdx := i;
        fBestReq := fOtherRequests[i];
      end;
    end;

    if fBestIdx >= 0 then
    begin
      aReq := fBestReq;
      Result := True;
    end;
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.CompleteCommand(const aReq: TCommandRequest);
var
  i: Integer;
begin
  fLock.Enter('CompleteCommand');
  try
    for i := 0 to fOtherRequests.Count - 1 do
    begin
      if (fOtherRequests[i].uid = aReq.uid) then
      begin
        fOtherRequests.Delete(i);
        Exit;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.Cleanup(const aMaxAgeMinutes: Integer = 15);
begin
  fLock.Enter('Cleanup');
  try
    InternalCleanup(fDirlistRequests, aMaxAgeMinutes);
    InternalCleanup(fMkdirRequests, aMaxAgeMinutes);
    InternalCleanup(fOtherRequests, aMaxAgeMinutes);
  finally
    fLock.Leave;
  end;
end;

procedure TCommandScheduler.RemoveByPazo(const aPazoID: Integer);
begin
  fLock.Enter('RemoveByPazo');
  try
    InternalRemoveByPazo(fDirlistRequests, aPazoID);
    InternalRemoveByPazo(fMkdirRequests, aPazoID);
    InternalRemoveByPazo(fOtherRequests, aPazoID);
    fPazoDirCount.Remove(aPazoID);
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.HasDirlist(const aPazoID: Integer; const aDir, aSite: String): Boolean;
begin
  fLock.Enter('HasDirlist');
  try
    Result := FindRequestIndex(fDirlistRequests, aPazoID, aDir, aSite) >= 0;
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.HasMkdir(const aPazoID: Integer; const aDir, aSite: String): Boolean;
begin
  fLock.Enter('HasMkdir');
  try
    Result := FindRequestIndex(fMkdirRequests, aPazoID, aDir, aSite) >= 0;
  finally
    fLock.Leave;
  end;
end;

function TCommandScheduler.HasCommand(const aCommandType: TCommandType;
  const aPazoID: Integer; const aDir: String): Boolean;
var
  i: Integer;
begin
  fLock.Enter('HasCommand');
  try
    Result := False;
    for i := 0 to fOtherRequests.Count - 1 do
    begin
      if (fOtherRequests[i].command_type = aCommandType) and
         (fOtherRequests[i].pazo_id = aPazoID) and
         (fOtherRequests[i].dir = aDir) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    fLock.Leave;
  end;
end;

initialization
  gUidLock := SyncObjs.TCriticalSection.Create;

finalization
  gUidLock.Free;

end.
