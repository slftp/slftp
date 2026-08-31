unit slapi.issues;

interface

uses
  SysUtils,
  DateUtils,
  SyncObjs,
  Generics.Collections,
  slcriticalsection2;

type
  TIssueEvent = record
    Id: Int64;
    TsUnix: Int64;
    IssueType: string; // e.g. SKIP, DONT_MATCH, MISSING_SECTION
    Section: string;
    ReleaseName: string;
    SiteName: string;
    Reason: string;
    KbEvent: string;
  end;

  TIssueEvents = array of TIssueEvent;

  TIssuesStore = class
  private
    FLock: TSlCriticalSection2;
    FEvents: TIssueEvents;
    FHead: integer;
    FCount: integer;
    FNextId: Int64;
    FCapacity: integer;
    FDedupLastSeen: TDictionary<string, Int64>;
    FDedupDefaultTtlSeconds: integer;
    FActiveUntil: TDateTime; //< collection is only active for a window after the last read via the API
    procedure AppendEvent(const aEvent: TIssueEvent);
    function ShouldDropByDedup(const aKey: string; const aNowUnix: Int64; const aTtlSeconds: integer): boolean;
    class function NormalizeType(const aIssueType: string): string; static;
    class function CsvContainsType(const aTypesCsvUpper: string; const aIssueTypeUpper: string): boolean; static;
  public
    constructor Create(const aCapacity: integer = 5000; const aDedupDefaultTtlSeconds: integer = 6 * 3600);
    destructor Destroy; override;

    procedure Clear;
    procedure AddIssue(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
      const aDedupKey: string = ''; const aDedupTtlSeconds: integer = 0);
    procedure DeleteIssue(const aIssueId: Int64);
    function GetSnapshot(const aLimit: integer; const aSinceUnix: Int64; const aTypesCsv: string): TIssueEvents;
    procedure GetCounts(const aWindowSeconds: integer; out aTotal, aSkip, aDontMatch, aMissingSection, aNuke: integer);
  end;

function IssuesStore: TIssuesStore;
procedure SlapiIssues_LogIssue(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
  const aDedupKey: string; const aDedupTtlSeconds: integer);

implementation

const
  CIssuesOnDemandMinutes = 10; //< how long issue collection stays active after the last read via the API

var
  glIssuesStore: TIssuesStore = nil;

function IssuesStore: TIssuesStore;
begin
  Result := glIssuesStore;
end;

procedure SlapiIssues_LogIssue(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
  const aDedupKey: string; const aDedupTtlSeconds: integer);
begin
  glIssuesStore.AddIssue(aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent, aDedupKey, aDedupTtlSeconds);
end;

{ TIssuesStore }

constructor TIssuesStore.Create(const aCapacity: integer; const aDedupDefaultTtlSeconds: integer);
begin
  inherited Create;
  FLock := TSlCriticalSection2.Create('issues');
  FCapacity := aCapacity;
  if FCapacity < 100 then
    FCapacity := 100;
  SetLength(FEvents, FCapacity);
  FHead := 0;
  FCount := 0;
  FNextId := 1;
  FActiveUntil := 0;
  FDedupLastSeen := TDictionary<string, Int64>.Create;
  FDedupDefaultTtlSeconds := aDedupDefaultTtlSeconds;
end;

destructor TIssuesStore.Destroy;
begin
  FDedupLastSeen.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TIssuesStore.AppendEvent(const aEvent: TIssueEvent);
var
  idx: integer;
begin
  if FCount < FCapacity then
  begin
    idx := (FHead + FCount) mod FCapacity;
    Inc(FCount);
  end
  else
  begin
    idx := FHead;
    FHead := (FHead + 1) mod FCapacity;
  end;

  FEvents[idx] := aEvent;
end;

function TIssuesStore.ShouldDropByDedup(const aKey: string; const aNowUnix: Int64; const aTtlSeconds: integer): boolean;
var
  lastSeen: Int64;
  ttl: integer;
begin
  Result := False;
  if aKey = '' then
    Exit;

  ttl := aTtlSeconds;
  if ttl <= 0 then
    ttl := FDedupDefaultTtlSeconds;

  if FDedupLastSeen.TryGetValue(aKey, lastSeen) then
  begin
    if (aNowUnix - lastSeen) < ttl then
    begin
      Result := True;
      Exit;
    end;
  end;

  FDedupLastSeen.AddOrSetValue(aKey, aNowUnix);
end;

class function TIssuesStore.NormalizeType(const aIssueType: string): string;
begin
  Result := UpperCase(Trim(aIssueType));
end;

class function TIssuesStore.CsvContainsType(const aTypesCsvUpper: string; const aIssueTypeUpper: string): boolean;
var
  hay: string;
begin
  if aTypesCsvUpper = '' then
  begin
    Result := True;
    Exit;
  end;

  hay := ',' + aTypesCsvUpper + ',';
  Result := Pos(',' + aIssueTypeUpper + ',', hay) > 0;
end;

procedure TIssuesStore.Clear;
begin
  FLock.Enter('issues_clear');
  try
    FHead := 0;
    FCount := 0;
    FNextId := 1;
    FDedupLastSeen.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TIssuesStore.AddIssue(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
  const aDedupKey: string; const aDedupTtlSeconds: integer);
var
  ev: TIssueEvent;
  nowUnix: Int64;
  issueTypeUpper: string;
begin
  // collection is activated on demand for a short window when the issues are
  // read via the API - skip all work while nobody is looking (unlocked read
  // of FActiveUntil is fine here, a missed or extra issue does not matter)
  if Now > FActiveUntil then
    Exit;

  issueTypeUpper := NormalizeType(aIssueType);
  nowUnix := DateTimeToUnix(Now, False);

  FLock.Enter('issues_add');
  try
    if ShouldDropByDedup(aDedupKey, nowUnix, aDedupTtlSeconds) then
      Exit;

    ev.Id := FNextId;
    Inc(FNextId);
    ev.TsUnix := nowUnix;
    ev.IssueType := issueTypeUpper;
    ev.Section := aSection;
    ev.ReleaseName := aReleaseName;
    ev.SiteName := aSiteName;
    ev.Reason := aReason;
    ev.KbEvent := aKbEvent;
    AppendEvent(ev);
  finally
    FLock.Leave;
  end;
end;

procedure TIssuesStore.DeleteIssue(const aIssueId: Int64);
var
  i: integer;
  idx: integer;
begin
  FLock.Enter('issues_delete');
  try
    for i := 0 to FCount - 1 do
    begin
      idx := (FHead + i) mod FCapacity;
      if FEvents[idx].Id = aIssueId then
      begin
        FEvents[idx].Id := 0; // Mark as deleted
        Exit;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TIssuesStore.GetSnapshot(const aLimit: integer; const aSinceUnix: Int64; const aTypesCsv: string): TIssueEvents;
var
  limit: integer;
  i: integer;
  idx: integer;
  added: integer;
  ev: TIssueEvent;
  typesUpper: string;
begin
  limit := aLimit;
  if limit <= 0 then
    limit := 100;
  if limit > 5000 then
    limit := 5000;

  typesUpper := UpperCase(StringReplace(Trim(aTypesCsv), ' ', '', [rfReplaceAll]));

  SetLength(Result, 0);

  FLock.Enter('issues_snapshot');
  try
    // someone is reading the issues via the API - keep collection active for a while
    FActiveUntil := IncMinute(Now, CIssuesOnDemandMinutes);

    if FCount = 0 then
      Exit;

    SetLength(Result, 0);
    added := 0;

    // iterate newest -> oldest
    for i := 0 to FCount - 1 do
    begin
      idx := (FHead + FCount - 1 - i) mod FCapacity;
      ev := FEvents[idx];

      // Skip deleted issues
      if ev.Id = 0 then
        Continue;

      if (aSinceUnix > 0) and (ev.TsUnix < aSinceUnix) then
        Break;

      if not CsvContainsType(typesUpper, UpperCase(ev.IssueType)) then
        Continue;

      SetLength(Result, added + 1);
      Result[added] := ev;
      Inc(added);
      if added >= limit then
        Break;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TIssuesStore.GetCounts(const aWindowSeconds: integer; out aTotal, aSkip, aDontMatch, aMissingSection, aNuke: integer);
var
  windowSeconds: integer;
  nowUnix: Int64;
  minUnix: Int64;
  i: integer;
  idx: integer;
  ev: TIssueEvent;
  t: string;
begin
  windowSeconds := aWindowSeconds;
  if windowSeconds <= 0 then
    windowSeconds := 24 * 3600;

  nowUnix := DateTimeToUnix(Now, False);
  minUnix := nowUnix - windowSeconds;

  aTotal := 0;
  aSkip := 0;
  aDontMatch := 0;
  aMissingSection := 0;
  aNuke := 0;

  FLock.Enter('issues_counts');
  try
    // someone is reading the issues via the API - keep collection active for a while
    FActiveUntil := IncMinute(Now, CIssuesOnDemandMinutes);

    for i := 0 to FCount - 1 do
    begin
      idx := (FHead + i) mod FCapacity;
      ev := FEvents[idx];

      // Skip deleted issues
      if ev.Id = 0 then
        Continue;

      if ev.TsUnix < minUnix then
        Continue;

      Inc(aTotal);
      t := UpperCase(ev.IssueType);
      if t = 'SKIP' then
        Inc(aSkip)
      else if (t = 'DONT_MATCH') or (t = 'DONTMATCH') then
        Inc(aDontMatch)
      else if (t = 'MISSING_SECTION') or (t = 'MISSING_SECTION_DIR') then
        Inc(aMissingSection)
      else if t = 'NUKE' then
        Inc(aNuke);
    end;
  finally
    FLock.Leave;
  end;
end;

initialization
  glIssuesStore := TIssuesStore.Create;

finalization
  glIssuesStore.Free;
  glIssuesStore := nil;

end.
