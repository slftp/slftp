unit dirliststats;

interface

type
  { Statistics for one release directory on one site. File names are not stored. }
  TDirlistStatsRecord = record
    FSection: String; //< section containing the release
    FSiteName: String; //< site containing the directory
    FDir: String; //< directory relative to the release root; empty for the root
    FZeroByteFiles: Integer; //< files with a size of zero bytes
    FPositiveByteFiles: Integer; //< files with a size greater than zero bytes
  end;

  { Snapshot of the collected dirlist statistics for one release. }
  TDirlistStats = TArray<TDirlistStatsRecord>;

{ Adds or updates statistics for one release directory. At most 50 unique
  directories are kept per release. }
procedure DirlistStatsAdd(const aReleaseName, aSection, aSiteName, aDir: String;
  const aZeroByteFiles, aPositiveByteFiles: Integer);

{ Returns a thread-safe snapshot of the release statistics.
  @param(aTruncated @true if more than 50 unique directories were seen) }
function DirlistStatsGet(const aReleaseName: String; out aStats: TDirlistStats;
  out aTruncated: Boolean): Boolean;

implementation

uses
  SysUtils, SyncObjs, Generics.Collections;

const
  cMaxDirlistStatsPerRelease = 50;

type
  TDirlistStatsCollection = class
  public
    Items: TList<TDirlistStatsRecord>;
    Truncated: Boolean;
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
end;

destructor TDirlistStatsCollection.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure DirlistStatsAdd(const aReleaseName, aSection, aSiteName, aDir: String;
  const aZeroByteFiles, aPositiveByteFiles: Integer);
var
  fCollection: TDirlistStatsCollection;
  fRecord: TDirlistStatsRecord;
  fReleaseKey: String;
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

    for i := 0 to fCollection.Items.Count - 1 do
    begin
      fRecord := fCollection.Items[i];
      if SameText(fRecord.FSection, aSection) and
         SameText(fRecord.FSiteName, aSiteName) and
         SameText(fRecord.FDir, aDir) then
      begin
        fRecord.FZeroByteFiles := aZeroByteFiles;
        fRecord.FPositiveByteFiles := aPositiveByteFiles;
        fCollection.Items[i] := fRecord;
        Exit;
      end;
    end;

    if fCollection.Items.Count >= cMaxDirlistStatsPerRelease then
    begin
      fCollection.Truncated := True;
      Exit;
    end;

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

function DirlistStatsGet(const aReleaseName: String; out aStats: TDirlistStats;
  out aTruncated: Boolean): Boolean;
var
  fCollection: TDirlistStatsCollection;
  i: Integer;
begin
  Result := False;
  aStats := nil;
  aTruncated := False;

  glDirlistStatsLock.Enter;
  try
    if not glDirlistStats.TryGetValue(UpperCase(aReleaseName), fCollection) then
      Exit;

    SetLength(aStats, fCollection.Items.Count);
    for i := 0 to fCollection.Items.Count - 1 do
      aStats[i] := fCollection.Items[i];
    aTruncated := fCollection.Truncated;
    Result := True;
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
