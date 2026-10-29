unit dirliststats;

interface

type
  { Statistics for one release directory on one site. File names are not stored. }
  TDirlistStatsRecord = record
    FSequence: Integer; //< order in which this snapshot was collected
    FSection: String; //< section containing the release
    FSiteName: String; //< site containing the directory
    FDir: String; //< directory relative to the release root; empty for the root
    FZeroByteFiles: Integer; //< files with a size of zero bytes
    FPositiveByteFiles: Integer; //< files with a size greater than zero bytes
  end;

  { Snapshot of the collected dirlist statistics for one release. }
  TDirlistStats = TArray<TDirlistStatsRecord>;

{ Adds one dirlist snapshot. The first 50 snapshots per site are kept for each release. }
procedure DirlistStatsAdd(const aReleaseName, aSection, aSiteName, aDir: String;
  const aZeroByteFiles, aPositiveByteFiles: Integer);

{ Returns a thread-safe snapshot of the release statistics.
  @param(aSiteName Optional site filter)
  @param(aTruncated @true if more than 50 snapshots matched the query) }
function DirlistStatsGet(const aReleaseName, aSiteName: String; out aStats: TDirlistStats;
  out aTruncated: Boolean): Boolean;

implementation

uses
  SysUtils, SyncObjs, Generics.Collections;

const
  cMaxDirlistStatsPerSite = 50;

type
  TDirlistStatsCollection = class
  public
    Items: TList<TDirlistStatsRecord>;
    Truncated: Boolean;
    TruncatedSites: TDictionary<String, Boolean>;
    NextSequence: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

var
  glDirlistStatsLock: TCriticalSection;
  glDirlistStats: TObjectDictionary<String, TDirlistStatsCollection>;

constructor TDirlistStatsCollection.Create;
begin
  inherited Create;
  Items := TList<TDirlistStatsRecord>.Create;
  Truncated := False;
  TruncatedSites := TDictionary<String, Boolean>.Create;
  NextSequence := 0;
end;

destructor TDirlistStatsCollection.Destroy;
begin
  Items.Free;
  TruncatedSites.Free;
  inherited Destroy;
end;

procedure DirlistStatsAdd(const aReleaseName, aSection, aSiteName, aDir: String;
  const aZeroByteFiles, aPositiveByteFiles: Integer);
var
  fCollection: TDirlistStatsCollection;
  fRecord: TDirlistStatsRecord;
  fReleaseKey: String;
  fSiteStatsCount: Integer;
  i: Integer;
begin
  if aReleaseName = '' then
    Exit;

  fReleaseKey := UpperCase(aReleaseName);
  glDirlistStatsLock.Enter;
  try
    if not glDirlistStats.TryGetValue(fReleaseKey, fCollection) then
    begin
      fCollection := TDirlistStatsCollection.Create;
      glDirlistStats.Add(fReleaseKey, fCollection);
    end;

    fSiteStatsCount := 0;
    for i := 0 to fCollection.Items.Count - 1 do
    begin
      fRecord := fCollection.Items[i];
      if SameText(fRecord.FSiteName, aSiteName) then
        Inc(fSiteStatsCount);
    end;

    if fSiteStatsCount >= cMaxDirlistStatsPerSite then
    begin
      fCollection.Truncated := True;
      fCollection.TruncatedSites.AddOrSetValue(UpperCase(aSiteName), True);
      Exit;
    end;

    Inc(fCollection.NextSequence);
    fRecord.FSequence := fCollection.NextSequence;
    fRecord.FSection := aSection;
    fRecord.FSiteName := aSiteName;
    fRecord.FDir := aDir;
    fRecord.FZeroByteFiles := aZeroByteFiles;
    fRecord.FPositiveByteFiles := aPositiveByteFiles;
    fCollection.Items.Add(fRecord);
  finally
    glDirlistStatsLock.Leave;
  end;
end;

function DirlistStatsGet(const aReleaseName, aSiteName: String; out aStats: TDirlistStats;
  out aTruncated: Boolean): Boolean;
var
  fCollection: TDirlistStatsCollection;
  fItems: TList<TDirlistStatsRecord>;
  fRecord: TDirlistStatsRecord;
  i: Integer;
begin
  Result := False;
  aStats := nil;
  aTruncated := False;

  glDirlistStatsLock.Enter;
  try
    if not glDirlistStats.TryGetValue(UpperCase(aReleaseName), fCollection) then
      Exit;

    if aSiteName = '' then
      aTruncated := fCollection.Truncated
    else
      fCollection.TruncatedSites.TryGetValue(UpperCase(aSiteName), aTruncated);

    fItems := TList<TDirlistStatsRecord>.Create;
    try
      for i := 0 to fCollection.Items.Count - 1 do
      begin
        fRecord := fCollection.Items[i];
        if ((aSiteName <> '') and (not SameText(fRecord.FSiteName, aSiteName))) then
          Continue;

        if fItems.Count >= cMaxDirlistStatsPerSite then
        begin
          aTruncated := True;
          Break;
        end;
        fItems.Add(fRecord);
      end;

      aStats := fItems.ToArray;
      Result := fItems.Count > 0;
    finally
      fItems.Free;
    end;
  finally
    glDirlistStatsLock.Leave;
  end;
end;

initialization
  glDirlistStatsLock := TCriticalSection.Create;
  glDirlistStats := TObjectDictionary<String, TDirlistStatsCollection>.Create([doOwnsValues]);

finalization
  glDirlistStats.Free;
  glDirlistStatsLock.Free;

end.
