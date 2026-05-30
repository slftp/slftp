unit cbftpmaincache;

interface

uses
  SysUtils, Classes, DateUtils, Math,
  mormot.core.base, mormot.core.unicode, mormot.core.json,
  slcriticalsection2;

type
  TCbftpMainJobSiteProgress = record
    Site: string;
    FilesDone: Integer;
    FilesTotal: Integer;
    BytesDone: Int64;
    BytesTotal: Int64;
    Completed: Boolean;
  end;

  TCbftpMainJobInternal = record
    Name: string;
    Section: string;
    Started: TDateTime;
    TimeSpentMs: Int64;
    Status: string;
    Sites: array of TCbftpMainJobSiteProgress;
  end;

  TCbftpMainSiteEntry = record
    Name: string;
    LoginsActive: Integer;
    LoginsMax: Integer;
    UploadsActive: Integer;
    UploadsMax: Integer;
    DownloadsActive: Integer;
    DownloadsMax: Integer;
    Up: Boolean;
    Down: Boolean;
    Disabled: Boolean;
    Up24hrBytes: Int64;
    Down24hrBytes: Int64;
    AllUpBytes: Int64;
    AllDownBytes: Int64;
    Priority: string;
  end;

procedure CbftpMainCacheInit;
procedure CbftpMainCacheDone;

procedure CbftpMainCacheAddJob(const aName, aSection: string; aStarted: TDateTime);
procedure CbftpMainCacheUpdateJobProgress(const aName, aSite: string; aFilesDone, aFilesTotal: Integer; aBytesDone, aBytesTotal: Int64);
procedure CbftpMainCacheUpdateJobCompleted(const aName, aSite: string; aTimeSpentMs: Int64; aFilesDone: Integer; aBytesDone: Int64);
procedure CbftpMainCacheUpdateJobDone(const aName, aStatus: string);
procedure CbftpMainCacheRemoveJob(const aName: string);
procedure CbftpMainCacheClearJobs;

procedure CbftpMainCacheUpdateSite(const aSite: TCbftpMainSiteEntry);
procedure CbftpMainCacheUpdateSiteDisabled(const aName: string; aDisabled: Boolean);
procedure CbftpMainCacheClearSites;

procedure CbftpMainCacheRefreshSites;

function CbftpMainCacheGetJson: RawUtf8;

implementation

uses
  debugunit, cbftpclient, uLkJSON;

const
  section = 'cbftpmaincache';
  MAX_JOBS = 50;

var
  GlJobs: array of TCbftpMainJobInternal;
  GlSites: array of TCbftpMainSiteEntry;
  GlCacheLock: TSLCriticalSection2;
  GlLastSiteRefresh: TDateTime = 0;
  GlRefreshLock: TSLCriticalSection2;

procedure CbftpMainCacheInit;
begin
  GlCacheLock := TSLCriticalSection2.Create('CbftpMainCache');
  GlRefreshLock := TSLCriticalSection2.Create('CbftpMainRefresh');
  SetLength(GlJobs, 0);
  SetLength(GlSites, 0);
end;

procedure CbftpMainCacheDone;
begin
  SetLength(GlJobs, 0);
  SetLength(GlSites, 0);
  FreeAndNil(GlRefreshLock);
  FreeAndNil(GlCacheLock);
end;

procedure CbftpMainCacheRefreshSites;
var
  fClient: TCbftpClient;
  fSitesJson, fSiteJson: RawUtf8;
  fJs, fJsSite: TlkJSONbase;
  fJsArr: TlkJSONlist;
  fJsObj: TlkJSONbase;
  fSiteName: string;
  fSiteEntry: TCbftpMainSiteEntry;
  fI: Integer;
  fField: TlkJSONbase;

  function _GetStr(const aObj: TlkJSONbase; const aKey: string): string;
  begin
    Result := '';
    if aObj = nil then
      Exit;
    fField := TlkJSONobject(aObj).Field[aKey];
    if (fField <> nil) and (fField.SelfType <> jsNull) then
      Result := fField.Value;
  end;

  function _GetBool(const aObj: TlkJSONbase; const aKey: string): Boolean;
  begin
    Result := False;
    if aObj = nil then
      Exit;
    fField := TlkJSONobject(aObj).Field[aKey];
    if fField <> nil then
      Result := (fField.Value = True) or (fField.Value = 'true') or (fField.Value = '1');
  end;

begin
  GlRefreshLock.Enter('RefreshSites');
  try
    if (GlLastSiteRefresh > 0) and (SecondsBetween(Now, GlLastSiteRefresh) < 5) then
      Exit;

    fClient := GlCbftpClient;
    if fClient = nil then
      Exit;

    try
      // Fetch site list
      fSitesJson := fClient.GetSites('');
      if fSitesJson <> '' then
      begin
        fJs := TlkJSON.ParseText(AnsiString(fSitesJson));
        if (fJs <> nil) and (fJs is TlkJSONlist) then
        begin
          fJsArr := TlkJSONlist(fJs);
          for fI := 0 to fJsArr.Count - 1 do
          begin
            fSiteName := fJsArr.Child[fI].Value;
            if fSiteName = '' then
              Continue;

            // Fetch site details
            fSiteJson := fClient.GetSite(StringToUtf8(fSiteName));
            if fSiteJson = '' then
              Continue;

            FillChar(fSiteEntry, SizeOf(fSiteEntry), 0);
            fSiteEntry.Name := fSiteName;

            fJsSite := TlkJSON.ParseText(AnsiString(fSiteJson));
            if (fJsSite <> nil) and (fJsSite is TlkJSONObject) then
            begin
              fJsObj := fJsSite;
              fSiteEntry.Disabled := _GetBool(fJsObj, 'disabled');
              fSiteEntry.Priority := _GetStr(fJsObj, 'priority');

              // UP/DOWN based on allow_upload / allow_download config
              fSiteEntry.Up := _GetStr(fJsObj, 'allow_upload') = 'YES';
              fSiteEntry.Down := (_GetStr(fJsObj, 'allow_download') = 'YES') or (_GetStr(fJsObj, 'allow_download') = 'MATCH_ONLY');

              // Connection limits from site config
              fField := TlkJSONobject(fJsObj).Field['max_logins'];
              if fField <> nil then
                fSiteEntry.LoginsMax := StrToIntDef(fField.Value, 0);
              fField := TlkJSONobject(fJsObj).Field['max_sim_up'];
              if fField <> nil then
                fSiteEntry.UploadsMax := StrToIntDef(fField.Value, 0);
              fField := TlkJSONobject(fJsObj).Field['max_sim_down'];
              if fField <> nil then
                fSiteEntry.DownloadsMax := StrToIntDef(fField.Value, 0);

              // Live stats from cbftp's var.* fields
              fField := TlkJSONobject(fJsObj).Field['var'];
              if (fField <> nil) and (fField is TlkJSONObject) then
              begin
                fJsObj := fField;
                fField := TlkJSONobject(fJsObj).Field['current_logins'];
                if fField <> nil then
                  fSiteEntry.LoginsActive := StrToIntDef(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['current_up'];
                if fField <> nil then
                  fSiteEntry.UploadsActive := StrToIntDef(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['current_down'];
                if fField <> nil then
                  fSiteEntry.DownloadsActive := StrToIntDef(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['size_up_all'];
                if fField <> nil then
                  fSiteEntry.AllUpBytes := StrToInt64Def(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['size_up_24h'];
                if fField <> nil then
                  fSiteEntry.Up24hrBytes := StrToInt64Def(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['size_down_all'];
                if fField <> nil then
                  fSiteEntry.AllDownBytes := StrToInt64Def(fField.Value, 0);
                fField := TlkJSONobject(fJsObj).Field['size_down_24h'];
                if fField <> nil then
                  fSiteEntry.Down24hrBytes := StrToInt64Def(fField.Value, 0);
              end;

              fJsSite.Free;
            end;

            CbftpMainCacheUpdateSite(fSiteEntry);
          end;
          fJs.Free;
        end
        else if fJs <> nil then
          fJs.Free;
      end;

      GlLastSiteRefresh := Now;
    except
      on E: Exception do
        Debug(dpError, section, Format('RefreshSites error: %s', [E.Message]));
    end;
  finally
    GlRefreshLock.Leave;
  end;
end;

function _FindJobIndex(const aName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(GlJobs) do
  begin
    if GlJobs[i].Name = aName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function _FindSiteIndex(const aName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(GlSites) do
  begin
    if GlSites[i].Name = aName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function _FindJobSiteIndex(var aJob: TCbftpMainJobInternal; const aSite: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(aJob.Sites) do
  begin
    if aJob.Sites[i].Site = aSite then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function _FormatBytes(aBytes: Int64): string;
const
  UNITS: array[0..5] of string = ('B', 'KB', 'MB', 'GB', 'TB', 'PB');
var
  fSize: Double;
  fUnitIdx: Integer;
begin
  if aBytes <= 0 then
  begin
    Result := '0 B';
    Exit;
  end;
  fSize := aBytes;
  fUnitIdx := 0;
  while (fSize >= 1024) and (fUnitIdx < High(UNITS)) do
  begin
    fSize := fSize / 1024;
    Inc(fUnitIdx);
  end;
  if fUnitIdx = 0 then
    Result := Format('%d %s', [aBytes, UNITS[fUnitIdx]])
  else if fSize >= 100 then
    Result := Format('%.1f %s', [fSize, UNITS[fUnitIdx]])
  else
    Result := Format('%.2f %s', [fSize, UNITS[fUnitIdx]]);
end;

function _FormatBytesGbStyle(aBytes: Int64): string;
var
  fGb: Double;
begin
  if aBytes <= 0 then
  begin
    Result := '0 B';
    Exit;
  end;
  fGb := aBytes / (1024 * 1024 * 1024);
  if fGb >= 1024 then
    Result := Format('%.2f TB', [fGb / 1024])
  else
    Result := Format('%.2f GB', [fGb]);
end;

procedure CbftpMainCacheAddJob(const aName, aSection: string; aStarted: TDateTime);
var
  fIdx: Integer;
begin
  GlCacheLock.Enter('AddJob');
  try
    fIdx := _FindJobIndex(aName);
    if fIdx >= 0 then
      Exit; // already exists

    if Length(GlJobs) >= MAX_JOBS then
    begin
      // remove oldest (first) job
      if Length(GlJobs) > 0 then
      begin
        Move(GlJobs[1], GlJobs[0], (Length(GlJobs) - 1) * SizeOf(TCbftpMainJobInternal));
        SetLength(GlJobs, Length(GlJobs) - 1);
      end;
    end;

    fIdx := Length(GlJobs);
    SetLength(GlJobs, fIdx + 1);
    GlJobs[fIdx].Name := aName;
    GlJobs[fIdx].Section := aSection;
    GlJobs[fIdx].Started := aStarted;
    GlJobs[fIdx].TimeSpentMs := 0;
    GlJobs[fIdx].Status := 'RUNNING';
    SetLength(GlJobs[fIdx].Sites, 0);
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheUpdateJobProgress(const aName, aSite: string; aFilesDone, aFilesTotal: Integer; aBytesDone, aBytesTotal: Int64);
var
  fJobIdx, fSiteIdx: Integer;
begin
  GlCacheLock.Enter('UpdateJobProgress');
  try
    fJobIdx := _FindJobIndex(aName);
    if fJobIdx < 0 then
      Exit;

    fSiteIdx := _FindJobSiteIndex(GlJobs[fJobIdx], aSite);
    if fSiteIdx < 0 then
    begin
      fSiteIdx := Length(GlJobs[fJobIdx].Sites);
      SetLength(GlJobs[fJobIdx].Sites, fSiteIdx + 1);
      GlJobs[fJobIdx].Sites[fSiteIdx].Site := aSite;
    end;

    GlJobs[fJobIdx].Sites[fSiteIdx].FilesDone := aFilesDone;
    GlJobs[fJobIdx].Sites[fSiteIdx].FilesTotal := aFilesTotal;
    GlJobs[fJobIdx].Sites[fSiteIdx].BytesDone := aBytesDone;
    GlJobs[fJobIdx].Sites[fSiteIdx].BytesTotal := aBytesTotal;
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheUpdateJobCompleted(const aName, aSite: string; aTimeSpentMs: Int64; aFilesDone: Integer; aBytesDone: Int64);
var
  fJobIdx, fSiteIdx: Integer;
begin
  GlCacheLock.Enter('UpdateJobCompleted');
  try
    fJobIdx := _FindJobIndex(aName);
    if fJobIdx < 0 then
      Exit;

    fSiteIdx := _FindJobSiteIndex(GlJobs[fJobIdx], aSite);
    if fSiteIdx < 0 then
    begin
      fSiteIdx := Length(GlJobs[fJobIdx].Sites);
      SetLength(GlJobs[fJobIdx].Sites, fSiteIdx + 1);
      GlJobs[fJobIdx].Sites[fSiteIdx].Site := aSite;
    end;

    GlJobs[fJobIdx].Sites[fSiteIdx].Completed := True;
    GlJobs[fJobIdx].Sites[fSiteIdx].FilesDone := aFilesDone;
    GlJobs[fJobIdx].Sites[fSiteIdx].BytesDone := aBytesDone;
    if GlJobs[fJobIdx].TimeSpentMs < aTimeSpentMs then
      GlJobs[fJobIdx].TimeSpentMs := aTimeSpentMs;
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheUpdateJobDone(const aName, aStatus: string);
var
  fJobIdx, fSiteIdx: Integer;
begin
  GlCacheLock.Enter('UpdateJobDone');
  try
    fJobIdx := _FindJobIndex(aName);
    if fJobIdx < 0 then
      Exit;
    GlJobs[fJobIdx].Status := aStatus;
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheRemoveJob(const aName: string);
var
  fJobIdx: Integer;
begin
  GlCacheLock.Enter('RemoveJob');
  try
    fJobIdx := _FindJobIndex(aName);
    if fJobIdx < 0 then
      Exit;
    if fJobIdx < High(GlJobs) then
      Move(GlJobs[fJobIdx + 1], GlJobs[fJobIdx], (Length(GlJobs) - fJobIdx - 1) * SizeOf(TCbftpMainJobInternal));
    SetLength(GlJobs, Length(GlJobs) - 1);
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheClearJobs;
begin
  GlCacheLock.Enter('ClearJobs');
  try
    SetLength(GlJobs, 0);
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheUpdateSite(const aSite: TCbftpMainSiteEntry);
var
  fIdx: Integer;
begin
  GlCacheLock.Enter('UpdateSite');
  try
    fIdx := _FindSiteIndex(aSite.Name);
    if fIdx < 0 then
    begin
      fIdx := Length(GlSites);
      SetLength(GlSites, fIdx + 1);
    end;
    GlSites[fIdx] := aSite;
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheUpdateSiteDisabled(const aName: string; aDisabled: Boolean);
var
  fIdx: Integer;
begin
  GlCacheLock.Enter('UpdateSiteDisabled');
  try
    fIdx := _FindSiteIndex(aName);
    if fIdx >= 0 then
      GlSites[fIdx].Disabled := aDisabled;
  finally
    GlCacheLock.Leave;
  end;
end;

procedure CbftpMainCacheClearSites;
begin
  GlCacheLock.Enter('ClearSites');
  try
    SetLength(GlSites, 0);
  finally
    GlCacheLock.Leave;
  end;
end;

function CbftpMainCacheGetJson: RawUtf8;
var
  fJobsJson, fSitesJson: RawUtf8;
  fJobJson: RawUtf8;
  fSiteJson: RawUtf8;
  fSiteList: string;
  i, j: Integer;
  fSitesTotal, fSitesDone: Integer;
  fSizeBytes: Int64;
  fWorstPct, fAvgPct, fBestPct: Integer;
  fPctSum, fPctCount: Integer;
  fPct: Integer;
  fElapsedSec: Int64;
begin
  GlCacheLock.Enter('GetJson');
  try
    // Build jobs JSON
    fJobsJson := '';
    for i := 0 to High(GlJobs) do
    begin
      fSitesTotal := Length(GlJobs[i].Sites);
      fSitesDone := 0;
      fSizeBytes := 0;
      fPctSum := 0;
      fPctCount := 0;
      fWorstPct := 100;
      fBestPct := 0;
      fSiteList := '';

      for j := 0 to High(GlJobs[i].Sites) do
      begin
        if fSiteList <> '' then
          fSiteList := fSiteList + ',';
        fSiteList := fSiteList + GlJobs[i].Sites[j].Site;

        if GlJobs[i].Sites[j].Completed then
          Inc(fSitesDone);

        if GlJobs[i].Sites[j].BytesTotal > fSizeBytes then
          fSizeBytes := GlJobs[i].Sites[j].BytesTotal;

        if GlJobs[i].Sites[j].BytesTotal > 0 then
          fPct := Round((GlJobs[i].Sites[j].BytesDone / GlJobs[i].Sites[j].BytesTotal) * 100)
        else if GlJobs[i].Sites[j].FilesTotal > 0 then
          fPct := Round((GlJobs[i].Sites[j].FilesDone / GlJobs[i].Sites[j].FilesTotal) * 100)
        else if GlJobs[i].Sites[j].Completed then
          fPct := 100
        else
          fPct := 0;

        if fPct > 100 then
          fPct := 100;

        Inc(fPctSum, fPct);
        Inc(fPctCount);

        if fPct < fWorstPct then
          fWorstPct := fPct;
        if fPct > fBestPct then
          fBestPct := fPct;
      end;

      if fPctCount > 0 then
        fAvgPct := Round(fPctSum / fPctCount)
      else
        fAvgPct := 0;

      if fSitesTotal = 0 then
      begin
        fWorstPct := 0;
        fAvgPct := 0;
        fBestPct := 0;
      end;

      fElapsedSec := Round((Now - GlJobs[i].Started) * 24 * 60 * 60);
      if GlJobs[i].TimeSpentMs > 0 then
        fElapsedSec := GlJobs[i].TimeSpentMs div 1000;

      fJobJson := FormatUtf8(
        '{"name":"%","section":"%","started":"%","use_sec":%,' +
        '"size":"%","size_bytes":%,"worst_pct":%,"avg_pct":%,"best_pct":%,' +
        '"status":"%","done":"%/%","sites":"%","sites_total":%,"sites_done":%}',
        [StringToUtf8(GlJobs[i].Name),
         StringToUtf8(GlJobs[i].Section),
         StringToUtf8(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', GlJobs[i].Started)),
         fElapsedSec,
         StringToUtf8(_FormatBytes(fSizeBytes)),
         fSizeBytes,
         fWorstPct,
         fAvgPct,
         fBestPct,
         StringToUtf8(GlJobs[i].Status),
         fSitesDone,
         fSitesTotal,
         StringToUtf8(fSiteList),
         fSitesTotal,
         fSitesDone],
        []);

      if fJobsJson <> '' then
        fJobsJson := fJobsJson + ',';
      fJobsJson := fJobsJson + fJobJson;
    end;

    // Build sites JSON
    fSitesJson := '';
    for i := 0 to High(GlSites) do
    begin
      fSiteJson := FormatUtf8(
        '{"name":"%","logins_active":%,"logins_max":%,' +
        '"uploads_active":%,"uploads_max":%,' +
        '"downloads_active":%,"downloads_max":%,' +
        '"up":%,"down":%,"disabled":%,' +
        '"up24hr":"%","down24hr":"%",' +
        '"allup":"%","alldown":"%","priority":"%"}',
        [StringToUtf8(GlSites[i].Name),
         GlSites[i].LoginsActive,
         GlSites[i].LoginsMax,
         GlSites[i].UploadsActive,
         GlSites[i].UploadsMax,
         GlSites[i].DownloadsActive,
         GlSites[i].DownloadsMax,
         GlSites[i].Up,
         GlSites[i].Down,
         GlSites[i].Disabled,
         StringToUtf8(_FormatBytesGbStyle(GlSites[i].Up24hrBytes)),
         StringToUtf8(_FormatBytesGbStyle(GlSites[i].Down24hrBytes)),
         StringToUtf8(_FormatBytesGbStyle(GlSites[i].AllUpBytes)),
         StringToUtf8(_FormatBytesGbStyle(GlSites[i].AllDownBytes)),
         StringToUtf8(GlSites[i].Priority)],
        []);

      if fSitesJson <> '' then
        fSitesJson := fSitesJson + ',';
      fSitesJson := fSitesJson + fSiteJson;
    end;

    Result := FormatUtf8('{"jobs":[%],"sites":[%],"updated":"%"}',
      [fJobsJson, fSitesJson, StringToUtf8(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now))],
      []);
  finally
    GlCacheLock.Leave;
  end;
end;

initialization
  CbftpMainCacheInit;

finalization
  CbftpMainCacheDone;

end.
