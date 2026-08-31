unit slapi.speedtest;

interface

uses
  Classes, SysUtils, Generics.Collections, slcriticalsection2, mormot.core.json,
  mormot.core.variants, mormot.core.text, RegExpr{$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  TSpeedTestType = (stLocal, stOut, stIn, stCleanup, stMatrix);
  TSpeedTestStatus = (stsRunning, stsFinished, stsError, stsAborted);
  TTransferStatus = (tsRunning, tsSuccess, tsFailed, tsNoFile);

  TSpeedTestResult = class
  public
    Source: string;
    Destination: string;
    Speed: string;
    Amount: string;
    Time: string;
    Success: boolean;
    Message: string;
    Status: TTransferStatus;
    StartTime: TDateTime;
    EndTime: TDateTime;
  end;

  TSpeedTestContext = class
  public
    TestID: string;
    TestType: TSpeedTestType;
    Status: TSpeedTestStatus;
    /// True if an abort was requested for this test.
    Aborted: boolean;
    Log: TStringList;
    Results: TObjectList<TSpeedTestResult>;
    Lock: TSLCriticalSection2;
    Thread: TThread;
    ResultMsg: string;
    Timestamp: TDateTime;
    SitesWithoutFiles: TStringList; // Sites that have no speedtest file
    constructor Create(const ID: string; AType: TSpeedTestType);
    destructor Destroy; override;
    procedure AddLog(const Msg: string);
    function GetLogJson: string;
    function GetResultsJson: string;
  end;

  TSpeedTestManager = class
  private
    FTests: TObjectDictionary<string, TSpeedTestContext>;
    FLock: TSLCriticalSection2;
    class var FInstance: TSpeedTestManager;
    class var FInstanceLock: TSLCriticalSection2;
    constructor Create;
  public
    class function Instance: TSpeedTestManager;
    destructor Destroy; override;
    
    function StartLocal(const SiteName: string): string;
    function StartOut(const Source, DestSites: string): string;
    function StartIn(const Dest, SourceSites: string): string;
    function StartCleanup(const Sites: string): string;
    function StartMatrix(const IncludeSites, ExcludeSites: string): string;

    function GetLog(const TestID: string): string;
    function GetStatus(const TestID: string): string; // JSON with log + results
    function GetSpeedTestSites: string; // JSON array of sites with SPEEDTEST section
    /// Aborts a running matrix speedtest.
    function AbortTest(const TestID: string): boolean;

    procedure LogHook(const Net, Chan, Msg: string);
    procedure CleanupOldTests;
  end;

{ Installs the IRC log hook which feeds the output of API speedtests
  ('API-' prefixed networks) into the speedtest manager.
  Only needed when the REST API is enabled, as API speedtests are the only
  source of such networks. }
procedure SpeedTestInstallLogHook;

implementation

uses
  irccommands.speed, debugunit, irc, configunit, sitesunit;

const
  section = 'slapi.speedtest';

type
  TSpeedTestThread = class(TThread)
  private
    FContext: TSpeedTestContext;
    FParams: TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(Context: TSpeedTestContext; const Params: array of string);
    destructor Destroy; override;
  end;

{ TSpeedTestContext }

constructor TSpeedTestContext.Create(const ID: string; AType: TSpeedTestType);
begin
  TestID := ID;
  TestType := AType;
  Status := stsRunning;
  Aborted := False;
  Log := TStringList.Create;
  Results := TObjectList<TSpeedTestResult>.Create;
  Lock := TSLCriticalSection2.Create('SpeedTestCtx-' + ID);
  SitesWithoutFiles := TStringList.Create;
  SitesWithoutFiles.Duplicates := dupIgnore;
  SitesWithoutFiles.Sorted := True;
  Timestamp := Now;
end;

destructor TSpeedTestContext.Destroy;
begin
  if Thread <> nil then
  begin
    Lock.Enter('Destroy-Abort');
    try
      Aborted := True;
    finally
      Lock.Leave;
    end;
    Thread.FreeOnTerminate := False;
    Thread.WaitFor;
    FreeAndNil(Thread);
  end;
  Log.Free;
  Results.Free;
  SitesWithoutFiles.Free;
  Lock.Free;
  inherited;
end;

procedure TSpeedTestContext.AddLog(const Msg: string);
begin
  Lock.Enter('AddLog');
  try
    Log.Add(Msg);
  finally
    Lock.Leave;
  end;
end;

function TSpeedTestContext.GetLogJson: string;
var
  ja: TDocVariantData;
  i: integer;
begin
  Lock.Enter('GetLogJson');
  try
    ja.InitFast(dvArray);
    for i := 0 to Log.Count - 1 do
      ja.AddItem(Log[i]);
    Result := ja.ToJSON;
  finally
    Lock.Leave;
  end;
end;

function TSpeedTestContext.GetResultsJson: string;
var
  ja: TDocVariantData;
  v: variant;
  i: integer;
  res: TSpeedTestResult;
  statusStr: string;
begin
  Lock.Enter('GetResultsJson');
  try
    ja.InitFast(dvArray);
    for i := 0 to Results.Count - 1 do
    begin
      res := Results[i];
      TDocVariant.New(v);
      TDocVariantData(v).AddValue('source', res.Source);
      TDocVariantData(v).AddValue('destination', res.Destination);
      TDocVariantData(v).AddValue('speed', res.Speed);
      TDocVariantData(v).AddValue('amount', res.Amount);
      TDocVariantData(v).AddValue('time', res.Time);
      TDocVariantData(v).AddValue('success', res.Success);
      TDocVariantData(v).AddValue('message', res.Message);

      case res.Status of
        tsRunning: statusStr := 'running';
        tsSuccess: statusStr := 'success';
        tsFailed: statusStr := 'failed';
        tsNoFile: statusStr := 'no_file';
      end;
      TDocVariantData(v).AddValue('status', statusStr);
      TDocVariantData(v).AddValue('startTime', DateTimeToStr(res.StartTime));
      if res.EndTime > 0 then
        TDocVariantData(v).AddValue('endTime', DateTimeToStr(res.EndTime))
      else
        TDocVariantData(v).AddValue('endTime', '');

      ja.AddItem(v);
    end;
    Result := ja.ToJSON;
  finally
    Lock.Leave;
  end;
end;

{ TSpeedTestThread }

constructor TSpeedTestThread.Create(Context: TSpeedTestContext; const Params: array of string);
var
  i: integer;
begin
  inherited Create(True);
  FContext := Context;
  FParams := TStringList.Create;
  for i := Low(Params) to High(Params) do
    FParams.Add(Params[i]);
  FreeOnTerminate := True;
end;

destructor TSpeedTestThread.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TSpeedTestThread.Execute;
var
  netname: string;
  pairs: TStringList;
  i: integer;
  pair, source, dest: string;
  colonPos: integer;
  res: TSpeedTestResult;
  k: integer;
  found: boolean;
  function IsAborted: boolean;
  begin
    FContext.Lock.Enter('IsAborted');
    try
      Result := FContext.Aborted;
    finally
      FContext.Lock.Leave;
    end;
  end;
begin
  netname := 'API-' + FContext.TestID;
  try
    try
      case FContext.TestType of
        stLocal:
          if FParams.Count > 0 then
            IrcSpeedTestLocal(netname, 'OUTPUT', FParams[0]);
        stOut:
          if FParams.Count > 1 then
            IrcSpeedTestOut(netname, 'OUTPUT', FParams[0] + ' ' + FParams[1]);
        stIn:
          if FParams.Count > 1 then
            IrcSpeedTestIn(netname, 'OUTPUT', FParams[0] + ' ' + FParams[1]);
        stCleanup:
          if FParams.Count > 0 then
            IrcSpeedTestCleanup(netname, 'OUTPUT', FParams[0])
          else
            IrcSpeedTestCleanup(netname, 'OUTPUT', '');
        stMatrix:
          if FParams.Count > 0 then
          begin
            // Parse pairs: "siteA:siteB|siteC:siteD|..."
            pairs := TStringList.Create;
            try
              pairs.Delimiter := '|';
              pairs.StrictDelimiter := True;
              pairs.DelimitedText := FParams[0];

              for i := 0 to pairs.Count - 1 do
              begin
                if IsAborted then
                  Break;

                pair := pairs[i];
                colonPos := Pos(':', pair);
                if colonPos > 0 then
                begin
                  source := Copy(pair, 1, colonPos - 1);
                  dest := Copy(pair, colonPos + 1, Length(pair));

                  // Check if source site has no files
                  FContext.Lock.Enter('CheckNoFile');
                  try
                    if FContext.SitesWithoutFiles.IndexOf(source) >= 0 then
                    begin
                      // Skip test, create no_file result immediately
                      FContext.AddLog(Format('[%d/%d] Skipping %s -> %s (no file on source)', [i + 1, pairs.Count, source, dest]));
                      res := TSpeedTestResult.Create;
                      res.Source := source;
                      res.Destination := dest;
                      res.Status := tsNoFile;
                      res.Success := False;
                      res.Message := 'Source site has no speedtest file';
                      res.StartTime := Now;
                      res.EndTime := Now;
                      FContext.Results.Add(res);
                      Continue;
                    end;
                  finally
                    FContext.Lock.Leave;
                  end;

                  FContext.Lock.Enter('AddRes');
                  try
                    found := False;
                    for k := FContext.Results.Count - 1 downto 0 do
                    begin
                      if (FContext.Results[k].Source = source) and
                        (FContext.Results[k].Destination = dest) then
                      begin
                        found := True;
                        Break;
                      end;
                    end;

                    if not found then
                    begin
                      res := TSpeedTestResult.Create;
                      res.Source := source;
                      res.Destination := dest;
                      res.Status := tsRunning;
                      res.StartTime := Now;
                      res.Message := 'Running...';
                      FContext.Results.Add(res);
                    end;
                  finally
                    FContext.Lock.Leave;
                  end;

                  FContext.AddLog(Format('[%d/%d] Testing %s -> %s', [i + 1, pairs.Count, source, dest]));
                  IrcSpeedTestOut(netname, 'OUTPUT', source + ' ' + dest);
                  if IsAborted then
                    Break;
                  Sleep(2000); // Small delay between tests
                end;
              end;
            finally
              pairs.Free;
            end;
          end;
      end;

      FContext.Lock.Enter('ThreadFinish');
      try
        if not FContext.Aborted then
          FContext.Status := stsFinished;
      finally
        FContext.Lock.Leave;
      end;
    finally
      if FContext.TestType = stMatrix then
      begin
        if IsAborted then
          SpeedTestMatrixStop(FContext.TestID, netname, 'OUTPUT');
        SpeedTestMatrixUnregister(FContext.TestID);
      end;
    end;
  except
    on E: Exception do
    begin
      FContext.AddLog('EXCEPTION: ' + E.Message);
      FContext.Lock.Enter('ThreadError');
      try
        if FContext.Aborted then
          FContext.Status := stsAborted
        else
          FContext.Status := stsError;
        if not FContext.Aborted then
          FContext.ResultMsg := E.Message;
      finally
        FContext.Lock.Leave;
      end;
    end;
  end;
end;

{ TSpeedTestManager }

constructor TSpeedTestManager.Create;
begin
  FTests := TObjectDictionary<string, TSpeedTestContext>.Create([doOwnsValues]);
  FLock := TSLCriticalSection2.Create('SpeedTestMgr');
end;

destructor TSpeedTestManager.Destroy;
begin
  FTests.Free;
  FLock.Free;
  inherited;
end;

class function TSpeedTestManager.Instance: TSpeedTestManager;
begin
  if FInstance = nil then
  begin
    FInstanceLock.Enter('Instance');
    try
      if FInstance = nil then
        FInstance := TSpeedTestManager.Create;
    finally
      FInstanceLock.Leave;
    end;
  end;
  Result := FInstance;
end;

function TSpeedTestManager.StartLocal(const SiteName: string): string;
var
  ctx: TSpeedTestContext;
  t: TSpeedTestThread;
  id: string;
begin
  try
    Debug(dpMessage, section, Format('Starting Local Speedtest for %s', [SiteName]));
    CleanupOldTests;
    id := IntToStr(GetTickCount64);
    ctx := TSpeedTestContext.Create(id, stLocal);
    
    FLock.Enter('StartLocal');
    try
      FTests.Add(id, ctx);
    finally
      FLock.Leave;
    end;
    
    t := TSpeedTestThread.Create(ctx, [SiteName]);
    ctx.Thread := t;
    t.Start;
    
    Result := id;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSpeedTestManager.StartLocal: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TSpeedTestManager.StartOut(const Source, DestSites: string): string;
var
  ctx: TSpeedTestContext;
  t: TSpeedTestThread;
  id: string;
begin
  try
    Debug(dpMessage, section, Format('Starting Outbound Speedtest %s -> %s', [Source, DestSites]));
    CleanupOldTests;
    id := IntToStr(GetTickCount64);
    ctx := TSpeedTestContext.Create(id, stOut);
    
    FLock.Enter('StartOut');
    try
      FTests.Add(id, ctx);
    finally
      FLock.Leave;
    end;
    
    t := TSpeedTestThread.Create(ctx, [Source, DestSites]);
    ctx.Thread := t;
    t.Start;
    
    Result := id;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSpeedTestManager.StartOut: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TSpeedTestManager.StartIn(const Dest, SourceSites: string): string;
var
  ctx: TSpeedTestContext;
  t: TSpeedTestThread;
  id: string;
begin
  try
    Debug(dpMessage, section, Format('Starting Inbound Speedtest %s -> %s', [SourceSites, Dest]));
    CleanupOldTests;
    id := IntToStr(GetTickCount64);
    ctx := TSpeedTestContext.Create(id, stIn);
    
    FLock.Enter('StartIn');
    try
      FTests.Add(id, ctx);
    finally
      FLock.Leave;
    end;
    
    t := TSpeedTestThread.Create(ctx, [Dest, SourceSites]);
    ctx.Thread := t;
    t.Start;
    
    Result := id;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSpeedTestManager.StartIn: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TSpeedTestManager.StartCleanup(const Sites: string): string;
var
  ctx: TSpeedTestContext;
  t: TSpeedTestThread;
  id: string;
begin
  try
    Debug(dpMessage, section, Format('Starting Speedtest Cleanup for %s', [Sites]));
    CleanupOldTests;
    id := IntToStr(GetTickCount64);
    ctx := TSpeedTestContext.Create(id, stCleanup);
    
    FLock.Enter('StartCleanup');
    try
      FTests.Add(id, ctx);
    finally
      FLock.Leave;
    end;
    
    t := TSpeedTestThread.Create(ctx, [Sites]);
    ctx.Thread := t;
    t.Start;
    
    Result := id;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TSpeedTestManager.StartCleanup: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TSpeedTestManager.GetLog(const TestID: string): string;
var
  ctx: TSpeedTestContext;
begin
  Result := '[]';
  FLock.Enter('GetLog');
  try
    if FTests.TryGetValue(TestID, ctx) then
    begin
      // Safe to access context outside main lock? 
      // Yes, context has its own lock.
    end
    else
      Exit;
  finally
    FLock.Leave;
  end;
  
  if ctx <> nil then
    Result := ctx.GetLogJson;
end;

function TSpeedTestManager.GetStatus(const TestID: string): string;
var
  ctx: TSpeedTestContext;
  doc: variant;
begin
  TDocVariant.New(doc);
  doc.status := 'unknown';
  Result := VariantSaveJSON(doc);
  
  FLock.Enter('GetStatus');
  try
    if FTests.TryGetValue(TestID, ctx) then
    begin
      // Found
    end
    else
      Exit;
  finally
    FLock.Leave;
  end;
  
  if ctx <> nil then
  begin
    ctx.Lock.Enter('GetStatusCtx');
    try
      case ctx.Status of
        stsRunning: doc.status := 'running';
        stsFinished: doc.status := 'finished';
        stsError: doc.status := 'error';
        stsAborted: doc.status := 'aborted';
      end;
      if ctx.ResultMsg <> '' then
        doc.message := ctx.ResultMsg;
      
      // Include structured results
      doc.results := _JsonFast(ctx.GetResultsJson);
      
    finally
      ctx.Lock.Leave;
    end;
    Result := VariantSaveJSON(doc);
  end;
end;

procedure TSpeedTestManager.LogHook(const Net, Chan, Msg: string);
var
  id: string;
  ctx: TSpeedTestContext;
  re: TRegExpr;
  res: TSpeedTestResult;
  i: integer;
  found: boolean;
  src, dst: string;
  function EnsureRunningResult(const aSrc, aDst: string): boolean;
  var
    j: integer;
  begin
    Result := False;
    ctx.Lock.Enter('EnsureRunningRes');
    try
      for j := ctx.Results.Count - 1 downto 0 do
      begin
        if (ctx.Results[j].Source = aSrc) and (ctx.Results[j].Destination = aDst) then
        begin
          // Do not override terminal states.
          Result := True;
          Exit;
        end;
      end;

      res := TSpeedTestResult.Create;
      res.Source := aSrc;
      res.Destination := aDst;
      res.Status := tsRunning;
      res.StartTime := Now;
      res.Message := 'Running...';
      ctx.Results.Add(res);
      Result := True;
    finally
      ctx.Lock.Leave;
    end;
  end;
begin
  if Pos('API-', Net) = 1 then
  begin
    id := Copy(Net, 5, Length(Net));

    FLock.Enter('LogHook');
    try
      if not FTests.TryGetValue(id, ctx) then
        ctx := nil;
    finally
      FLock.Leave;
    end;

    if ctx <> nil then
    begin
      ctx.AddLog(Msg);

      // Parse for structured results
      re := TRegExpr.Create;
      try
        // Speedtesting (Running) - Outbound: "Speedtesting A -> B ->> path"
        re.Expression := 'Speedtesting (.+) -> (.+) ->> .+';
        if re.Exec(Msg) then
        begin
          EnsureRunningResult(re.Match[1], re.Match[2]);
          Exit;
        end;

        // Testing (Running) - Matrix log: "[n/m] Testing A -> B"
        re.Expression := '(\[\d+/\d+\] )?Testing (.+) -> (.+)';
        if re.Exec(Msg) then
        begin
          EnsureRunningResult(re.Match[2], re.Match[3]);
          Exit;
        end;

        // Speedtesting (Running) - Inbound: "Speedtesting A -> B (using file / bytes)"
        re.Expression := 'Speedtesting (.+) -> (.+) \(using .+\)';
        if re.Exec(Msg) then
        begin
          EnsureRunningResult(re.Match[1], re.Match[2]);
          Exit;
        end;

        // FXP Success: A -> B => X kB/s (YmB sent in Zs)
        re.Expression := '(.+) -> (.+) => ([\d\.]+) kB/s \(([\d\.]+)mB sent in ([\d\.]+)s\)';
        if re.Exec(Msg) then
        begin
          src := re.Match[1];
          dst := re.Match[2];

          // Find existing result and update it
          ctx.Lock.Enter('UpdateRes');
          try
            found := False;
            for i := ctx.Results.Count - 1 downto 0 do
            begin
              if (ctx.Results[i].Source = src) and (ctx.Results[i].Destination = dst) then
              begin
                ctx.Results[i].Speed := re.Match[3];
                ctx.Results[i].Amount := re.Match[4] + ' MB';
                ctx.Results[i].Time := re.Match[5] + ' s';
                ctx.Results[i].Status := tsSuccess;
                ctx.Results[i].Success := True;
                ctx.Results[i].Message := 'Success';
                ctx.Results[i].EndTime := Now;
                found := True;
                Break;
              end;
            end;

            if not found then
            begin
              res := TSpeedTestResult.Create;
              res.Source := src;
              res.Destination := dst;
              res.Speed := re.Match[3];
              res.Amount := re.Match[4] + ' MB';
              res.Time := re.Match[5] + ' s';
              res.Status := tsSuccess;
              res.Success := True;
              res.Message := 'Success';
              res.StartTime := Now;
              res.EndTime := Now;
              ctx.Results.Add(res);
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

        // FXP Failure: A -> B failed
        re.Expression := '(.+) -> (.+) failed';
        if re.Exec(Msg) then
        begin
          src := re.Match[1];
          dst := re.Match[2];

          ctx.Lock.Enter('UpdateRes');
          try
            found := False;
            for i := ctx.Results.Count - 1 downto 0 do
            begin
              if (ctx.Results[i].Source = src) and (ctx.Results[i].Destination = dst) then
              begin
                ctx.Results[i].Status := tsFailed;
                ctx.Results[i].Success := False;
                ctx.Results[i].Message := 'Failed';
                ctx.Results[i].EndTime := Now;
                found := True;
                Break;
              end;
            end;

            if not found then
            begin
              res := TSpeedTestResult.Create;
              res.Source := src;
              res.Destination := dst;
              res.Status := tsFailed;
              res.Success := False;
              res.Message := 'Failed';
              res.StartTime := Now;
              res.EndTime := Now;
              ctx.Results.Add(res);
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

        // Local Success: slFtp -> B is X kB/s (uploaded Ymb in Zs)
        re.Expression := 'slFtp -> (.+) is ([\d\.]+) kB/s \(uploaded ([\d\.]+)mb in ([\d\.]+)s\)';
        if re.Exec(Msg) then
        begin
          src := 'slFtp';
          dst := re.Match[1];

          ctx.Lock.Enter('UpdateRes');
          try
            found := False;
            for i := ctx.Results.Count - 1 downto 0 do
            begin
              if (ctx.Results[i].Source = src) and (ctx.Results[i].Destination = dst) then
              begin
                ctx.Results[i].Speed := re.Match[2];
                ctx.Results[i].Amount := re.Match[3] + ' MB';
                ctx.Results[i].Time := re.Match[4] + ' s';
                ctx.Results[i].Status := tsSuccess;
                ctx.Results[i].Success := True;
                ctx.Results[i].Message := 'Success';
                ctx.Results[i].EndTime := Now;
                found := True;
                Break;
              end;
            end;

            if not found then
            begin
              res := TSpeedTestResult.Create;
              res.Source := src;
              res.Destination := dst;
              res.Speed := re.Match[2];
              res.Amount := re.Match[3] + ' MB';
              res.Time := re.Match[4] + ' s';
              res.Status := tsSuccess;
              res.Success := True;
              res.Message := 'Success';
              res.StartTime := Now;
              res.EndTime := Now;
              ctx.Results.Add(res);
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

        // Can't dirlist error (SPEEDTEST directory doesn't exist or access denied)
        re.Expression := 'Can''t dirlist (.+) in (.+)\.';
        if re.Exec(Msg) then
        begin
          src := Trim(re.Match[2]); // Site name
          ctx.Lock.Enter('UpdateRes');
          try
            // Find all running results with this source and mark as failed
            for i := 0 to ctx.Results.Count - 1 do
            begin
              if (ctx.Results[i].Status = tsRunning) and (ctx.Results[i].Source = src) then
              begin
                ctx.Results[i].Status := tsFailed;
                ctx.Results[i].Success := False;
                ctx.Results[i].Message := 'SPEEDTEST directory not found on source site';
                ctx.Results[i].EndTime := Now;

                // Add to sites without files to skip further tests
                if ctx.SitesWithoutFiles.IndexOf(src) = -1 then
                  ctx.SitesWithoutFiles.Add(src);
              end;
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

        // No suitable file found error
        re.Expression := 'No suitable file found on site .* for speedtesting';
        if re.Exec(Msg) then
        begin
          // Try to find the last "Testing A -> B" to get source and dest
          ctx.Lock.Enter('UpdateRes');
          try
            if ctx.Results.Count > 0 then
            begin
              // Update last running result
              for i := ctx.Results.Count - 1 downto 0 do
              begin
                if ctx.Results[i].Status = tsRunning then
                begin
                  ctx.Results[i].Status := tsNoFile;
                  ctx.Results[i].Success := False;
                  ctx.Results[i].Message := 'No speedtest file found in SPEEDTEST directory';
                  ctx.Results[i].EndTime := Now;

                  // Add source to sites without files list
                  if ctx.SitesWithoutFiles.IndexOf(ctx.Results[i].Source) = -1 then
                    ctx.SitesWithoutFiles.Add(ctx.Results[i].Source);

                  Break;
                end;
              end;
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

        // General "aborted" errors
        if Pos('Speedtest aborted', Msg) > 0 then
        begin
          ctx.Lock.Enter('UpdateRes');
          try
            if ctx.Results.Count > 0 then
            begin
              for i := ctx.Results.Count - 1 downto 0 do
              begin
                if ctx.Results[i].Status = tsRunning then
                begin
                  ctx.Results[i].Status := tsFailed;
                  ctx.Results[i].Success := False;
                  if ctx.Results[i].Message = 'Running...' then
                    ctx.Results[i].Message := 'Speedtest aborted';
                  ctx.Results[i].EndTime := Now;
                  Break;
                end;
              end;
            end;
          finally
            ctx.Lock.Leave;
          end;
          Exit;
        end;

      finally
        re.Free;
      end;
    end;
  end;
end;

function TSpeedTestManager.GetSpeedTestSites: string;
var
  i: integer;
  s: TSite;
  ja: TDocVariantData;
begin
  ja.InitFast(dvArray);

  if sitesunit.sites <> nil then
  begin
    for i := 0 to sitesunit.sites.Count - 1 do
    begin
      s := TSite(sitesunit.sites[i]);
      if (s <> nil) and (s.sectiondir['SPEEDTEST'] <> '') and (s.Name <> sitesunit.getAdminSiteName) then
        ja.AddItem(s.Name);
    end;
  end;

  Result := ja.ToJSON;
end;

function TSpeedTestManager.AbortTest(const TestID: string): boolean;
var
  ctx: TSpeedTestContext;
  i: integer;
  stopped: integer;
begin
  Result := False;
  ctx := nil;

  FLock.Enter('AbortTest');
  try
    if not FTests.TryGetValue(TestID, ctx) then
      Exit;
  finally
    FLock.Leave;
  end;

  if ctx = nil then
    Exit;

  ctx.Lock.Enter('AbortTestCtx');
  try
    if ctx.Status <> stsRunning then
      Exit;
    if ctx.TestType <> stMatrix then
      Exit;

    ctx.Aborted := True;
    ctx.Status := stsAborted;
    ctx.ResultMsg := 'Matrix speedtest aborted';

    for i := ctx.Results.Count - 1 downto 0 do
    begin
      if ctx.Results[i].Status = tsRunning then
      begin
        ctx.Results[i].Status := tsFailed;
        ctx.Results[i].Success := False;
        ctx.Results[i].Message := 'Aborted';
        ctx.Results[i].EndTime := Now;
      end;
    end;
  finally
    ctx.Lock.Leave;
  end;

  ctx.AddLog('Matrix speedtest aborted by user');
  stopped := SpeedTestMatrixStop(TestID, 'API-' + TestID, 'OUTPUT');
  if stopped > 0 then
    ctx.AddLog(Format('Stopped %d speedtest transfer(s)', [stopped]));

  Result := True;
end;

function TSpeedTestManager.StartMatrix(const IncludeSites, ExcludeSites: string): string;
var
  ctx: TSpeedTestContext;
  t: TSpeedTestThread;
  id: string;
  sitelist: TStringList;
  includeList: TStringList;
  excludeList: TStringList;
  skippedSites: TStringList;
  i, j: integer;
  s: TSite;
  source, dest: string;
  allPairs: string;
begin
  id := '';
  try
    Debug(dpMessage, section, 'Starting Matrix Speedtest');
    CleanupOldTests;
    id := IntToStr(GetTickCount64);
    ctx := TSpeedTestContext.Create(id, stMatrix);
    SpeedTestMatrixRegister(id);

    // Collect all sites with SPEEDTEST (skip PermDown and admin site)
    sitelist := TStringList.Create;
    includeList := TStringList.Create;
    excludeList := TStringList.Create;
    skippedSites := TStringList.Create;
    try
      includeList.Delimiter := ' ';
      includeList.StrictDelimiter := True;
      excludeList.Delimiter := ' ';
      excludeList.StrictDelimiter := True;

      if Trim(IncludeSites) <> '' then
        includeList.DelimitedText := StringReplace(Trim(IncludeSites), ',', ' ', [rfReplaceAll]);
      if Trim(ExcludeSites) <> '' then
        excludeList.DelimitedText := StringReplace(Trim(ExcludeSites), ',', ' ', [rfReplaceAll]);

      if sitesunit.sites <> nil then
      begin
        for i := 0 to sitesunit.sites.Count - 1 do
        begin
          s := TSite(sitesunit.sites[i]);
          if (s <> nil) and (s.sectiondir['SPEEDTEST'] <> '') and (s.Name <> sitesunit.getAdminSiteName) then
          begin
            if s.PermDown then
              skippedSites.Add(s.Name)
            else
            begin
              if (includeList.Count > 0) and (includeList.IndexOf(s.Name) < 0) then
                Continue;
              if (excludeList.IndexOf(s.Name) >= 0) then
                Continue;
              sitelist.Add(s.Name);
            end;
          end;
        end;
      end;

      // Log skipped sites once
      if skippedSites.Count > 0 then
        ctx.AddLog(Format('Skipping %d PermDown sites: %s', [skippedSites.Count, skippedSites.CommaText]));
      if includeList.Count > 0 then
        ctx.AddLog(Format('Matrix include filter: %s', [includeList.CommaText]));
      if excludeList.Count > 0 then
        ctx.AddLog(Format('Matrix exclude filter: %s', [excludeList.CommaText]));

      // Build all combinations (source -> dest where source <> dest)
      allPairs := '';
      for i := 0 to sitelist.Count - 1 do
      begin
        source := sitelist[i];
        for j := 0 to sitelist.Count - 1 do
        begin
          dest := sitelist[j];
          if source <> dest then
          begin
            if allPairs <> '' then
              allPairs := allPairs + '|';
            allPairs := allPairs + source + ':' + dest;
          end;
        end;
      end;

      ctx.AddLog(Format('Matrix test: %d sites, %d transfers', [sitelist.Count, sitelist.Count * (sitelist.Count - 1)]));
    finally
      sitelist.Free;
      includeList.Free;
      excludeList.Free;
      skippedSites.Free;
    end;

    FLock.Enter('StartMatrix');
    try
      FTests.Add(id, ctx);
    finally
      FLock.Leave;
    end;

    // Start thread with pairs
    t := TSpeedTestThread.Create(ctx, [allPairs]);
    ctx.Thread := t;
    t.Start;

    Result := id;
  except
    on E: Exception do
    begin
      if id <> '' then
        SpeedTestMatrixUnregister(id);
      Debug(dpError, section, Format('[EXCEPTION] TSpeedTestManager.StartMatrix: %s', [E.Message]));
      raise;
    end;
  end;
end;

procedure TSpeedTestManager.CleanupOldTests;
var
  key: string;
  keysToRemove: TList<string>;
  matrixToRemove: TList<string>;
  ctx: TSpeedTestContext;
begin
  // Remove tests older than 1 hour
  keysToRemove := TList<string>.Create;
  matrixToRemove := TList<string>.Create;
  try
    FLock.Enter('Cleanup');
    try
      for key in FTests.Keys do
      begin
        ctx := FTests[key];
        if (Now - ctx.Timestamp) > (1/24) then // 1 hour
        begin
          keysToRemove.Add(key);
          if ctx.TestType = stMatrix then
            matrixToRemove.Add(key);
        end;
      end;

      for key in keysToRemove do
        FTests.Remove(key);
    finally
      FLock.Leave;
    end;
    for key in matrixToRemove do
      SpeedTestMatrixUnregister(key);
  finally
    keysToRemove.Free;
    matrixToRemove.Free;
  end;
end;

procedure SpeedTestLogHookWrapper(const Net, Chan, Msg: string);
begin
  if TSpeedTestManager.FInstance <> nil then
    TSpeedTestManager.FInstance.LogHook(Net, Chan, Msg);
end;

procedure SpeedTestInstallLogHook;
begin
  irc.GlIrcLogHook := SpeedTestLogHookWrapper;
end;

initialization
  TSpeedTestManager.FInstanceLock := TSLCriticalSection2.Create('SpeedTestInstance');
  TSpeedTestManager.Instance;

finalization
  if TSpeedTestManager.FInstance <> nil then
    FreeAndNil(TSpeedTestManager.FInstance);
  FreeAndNil(TSpeedTestManager.FInstanceLock);
    
end.
