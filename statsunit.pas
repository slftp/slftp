unit statsunit;

interface

uses
  Classes, mormot.orm.core, mormot.core.base, mormot.orm.base;

type
  TSQLSitesRecord = class(TOrmNoCase)
  private
    FName: RawUTF8; //< sitename
  published
    property Name: RawUTF8 read FName write FName stored AS_UNIQUE;
  end;

  TSQLSectionRecord = class(TOrmNoCase)
  private
    FSection: RawUTF8; //< sectionname
  published
    property Section: RawUTF8 read FSection write FSection stored AS_UNIQUE;
  end;

  TSQLFileInfoRecord = class(TOrmNoCase)
  private
    FReleaseName: RawUTF8; //< releasename
    FFileName: RawUTF8; //< filename
    FFileSize: Int64; //< filesize
    FTimeStamp: TDateTime; //< creation time of the entry
  published
    property ReleaseName: RawUTF8 read FReleaseName write FReleaseName;
    property FileName: RawUTF8 read FFileName write FFileName;
    property FileSize: Int64 read FFileSize write FFileSize;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
  end;

  TSQLStatsRecord = class(TOrm)
  private
    FSrcSite: TSQLSitesRecord; //< reference to source sitename
    FDstSite: TSQLSitesRecord; //< reference to destination sitename
    FSection: TSQLSectionRecord; //< reference to sectionname
    FFileInfo: TSQLFileInfoRecord; //< reference to file infos
  published
    property SrcSiteRec: TSQLSitesRecord read FSrcSite write FSrcSite;
    property DstSiteRec: TSQLSitesRecord read FDstSite write FDstSite;
    property SectionRec: TSQLSectionRecord read FSection write FSection;
    property FileInfoRec: TSQLFileInfoRecord read FFileInfo write FFileInfo;
  end;

  { Used to store race stat info of a transfer for later saving into the DB }
  TStatRaceRecord = record
     FSrcSite, FDstSite, FSection, FRls, FFilename: String;
     FFilesize: Int64;
   end;

  { Consumes the race stats queue and writes the stats into the DB }
  TWriteStatsToDBThread = class(TThread)
  public
    constructor Create;
    procedure Execute; override;
    destructor Destroy; override;
  end;

{ Just a helper function to initialize @link(ORMStatsDB) }
procedure statsInit;

{ Just a helper function to free @link(ORMStatsDB) }
procedure statsUninit;

{ Checks if stats database client/server is active
  @returns(@true if @link(ORMStatsDB) is not nil, @false otherwise.) }
function IsStatsDatabaseActive: Boolean;

{ Queue the raced file and appropriate infos to be added into database
  @param(aSrcSite source sitename)
  @param(aDstSite destination sitename)
  @param(aSection sectionname)
  @param(aRls releasename)
  @param(aFilename name of transfered file)
  @param(aFilesize filesize of transfered file) }
procedure statsProcessRace(const aSrcSite, aDstSite, aSection, aRls, aFilename: String; const aFilesize: Int64);

{ Removes site from database, resets all site fields to 0, fileinfo are deleted if src and dst site were removed and
  if the fileinfo is not used for any other site which is not deleted.
  @param(aSitename sitename)
  @returns(@true if deletion was successful, @false if some problem occured) }
function RemoveStats(const aSitename: String): Boolean; overload;

{ Shows (detailed) race infos for sites and total race amount of the day, month or year
  @param(aNetname irc netname)
  @param(aChannel irc channel)
  @param(aSitename sitename)
  @param(aPeriod SQL start of period: YEAR, MONTH, DAY)
  @param(aDetailed if @true it shows detailed traffic info, if @false it shows only total in/out) }
procedure StatRaces(const aNetname, aChannel, aSitename, aPeriod: String; const aDetailed: Boolean);

{ Returns race statistics (same data as @link(StatRaces)) as JSON for the Web API
  @param(aSitename sitename or '*' for all)
  @param(aPeriod period: YEAR, MONTH, DAY; anything else defaults to DAY)
  @param(aDetailed if @true it includes per-site breakdown)
  @returns(JSON object with sites and totals; or an error if stats are disabled) }
function StatsGetRaceStatsJson(const aSitename, aPeriod: String; const aDetailed: Boolean): RawJSON;

{ Returns recent raced file entries as JSON for the Web API
  @param(aLimit max number of items; defaults to 200; capped to 5000)
  @param(aSinceUnix only return entries with timestamp >= since (unix seconds); 0 disables filter)
  @returns(JSON object with enabled/error/items) }
function StatsGetRecentRacesJson(const aPage: integer; const aPageSize: integer; const aSinceUnix: Int64): RawJSON;

{ Returns recent raced file entries for a given release as JSON for the Web API
  @param(aRelease release name)
  @param(aPage page number (1..5))
  @param(aPageSize items per page (max 500))
  @param(aSinceUnix only return entries with timestamp >= since (unix seconds); 0 disables filter)
  @returns(JSON object with enabled/error/items) }
function StatsGetReleaseRacesJson(const aRelease: String; const aPage: integer; const aPageSize: integer; const aSinceUnix: Int64): RawJSON;

{ Creates a backup of stats-database - this is needed because the file is in use and can't be copied
  @param(aPath path where the backup should be stored in the filesystem with last slash, e.g. /path/to/file/)
  @param(aFileName filename including fileextension) }
procedure doStatsBackup(const aPath, aFileName: String);

implementation

uses
  SysUtils, Contnrs, Generics.Collections, dbhandler, debugunit, configunit, sitesunit, irc, mystrings, slcriticalsection2, DateUtils, mormot.rest.sqlite3, mormot.core.unicode, mormot.core.os, mormot.db.raw.sqlite3, mormot.core.text, mormot.core.json, mormot.core.variants;

const
  section = 'stats';

var
  ORMStatsDB: TSQLRestClientDB; //< Rest Client for all database interactions
  ORMStatsModel: TSQLModel; //< SQL ORM model for stats database
  glStatRaceQueue: TQueue<TStatRaceRecord>; //< StatRace records to be written into the DB
  glStatRaceLock: TSlCriticalSection2; //< Lock for the race stats queue
  glLastStatsCleanTime: TDateTime;  //< When was the stats DB last cleaned from old entries
  glTWriteStatsThreadRunning: boolean = False; //< True if the thread which writes stats is running
  glWriteStatsThreadShouldStop: boolean = False; //< True if the thread which writes stats should terminate
  glDeleteAfterDays: integer;

function _GetMinFilesize: Int64; inline;
begin
  Result := config.ReadInteger(section, 'min_filesize', 100000);
end;

procedure statsInit;
var
  fDBName: String;
begin
  if not config.ReadBool(section, 'enabled', True) then
    Exit;

  glLastStatsCleanTime := MinDateTime;
  fDBName := Trim(config.ReadString(section, 'database', 'stats.db'));
  glDeleteAfterDays := config.ReadInteger(Section, 'delete_after_days', 0);

  ORMStatsModel := TSQLModel.Create([TSQLStatsRecord, TSQLSitesRecord, TSQLSectionRecord, TSQLFileInfoRecord]);
  try
    ORMStatsDB := CreateORMSQLite3DB(ORMStatsModel, fDBName, '');
    glStatRaceQueue := TQueue<TStatRaceRecord>.Create;
    glStatRaceLock := TSlCriticalSection2.Create('glStatRaceLock');
    TWriteStatsToDBThread.Create;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] statsInit: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure statsUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if Assigned(ORMStatsDB) then
  begin
    glWriteStatsThreadShouldStop := True;

    while glTWriteStatsThreadRunning do
      Sleep(100);

    FreeAndNil(ORMStatsDB);
  end;
  if Assigned(ORMStatsModel) then
  begin
    ORMStatsModel.Free;
  end;
  if glStatRaceLock <> nil then
    glStatRaceLock.Free;
  if glStatRaceQueue <> nil then
    glStatRaceQueue.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

function IsStatsDatabaseActive: Boolean;
begin
  if (ORMStatsDB = nil) then
    Result := False
  else
    Result := True;
end;

function StatsGetRaceStatsJson(const aSitename, aPeriod: String; const aDetailed: Boolean): RawJSON;
type
  TDirectionStats = record
    Bytes: Int64;
    Files: Int64;
  end;
  TFileSizeStats = record
    InBytes: Int64;
    OutBytes: Int64;
    InFiles: Int64;
    OutFiles: Int64;
  end;
  TStatsDirection = (stFrom, stTo);
  TDirEntry = record
    Site: String;
    Bytes: Int64;
    Files: Int64;
  end;
var
  fPeriod, fSQLPeriod, fSiteFilter: String;
  fAllBytes: Int64;
  fAllFiles: Int64;

  function _SQLPeriodFromPeriod(const aPeriod: String): String;
  begin
    if (aPeriod = 'MONTH') then
      Result := 'start of month'
    else if (aPeriod = 'YEAR') then
      Result := 'start of year'
    else
      Result := 'start of day';
  end;

  procedure _InitTotals(out aTotals: TFileSizeStats);
  begin
    aTotals.InBytes := 0;
    aTotals.OutBytes := 0;
    aTotals.InFiles := 0;
    aTotals.OutFiles := 0;
  end;

  procedure _GetTransferStats(const aSitename, aSQLPeriod: String; out aTotals: TFileSizeStats);
  var
    fStatsRec: TSQLStatsRecord;
  begin
    _InitTotals(aTotals);

    fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client,
      '(DstSiteRec.Name = ? OR SrcSiteRec.Name = ?) AND timestamp > date(?, ?)',
      [], [aSitename, aSitename, 'now', aSQLPeriod]);
    try
      while fStatsRec.FillOne do
      begin
        if aSitename = UTF8ToString(fStatsRec.DstSiteRec.Name) then
        begin
          aTotals.InBytes := aTotals.InBytes + fStatsRec.FileInfoRec.FileSize;
          Inc(aTotals.InFiles);
        end
        else if aSitename = UTF8ToString(fStatsRec.SrcSiteRec.Name) then
        begin
          aTotals.OutBytes := aTotals.OutBytes + fStatsRec.FileInfoRec.FileSize;
          Inc(aTotals.OutFiles);
        end;
      end;
    finally
      fStatsRec.Free;
    end;
  end;

  procedure _GetDetailedTransferStats(const aSitename, aSQLPeriod: String; const aDirection: TStatsDirection;
    out aEntries: TArray<TDirEntry>);
  var
    fStatsRec: TSQLStatsRecord;
    fSiteStats: TDictionary<String, TDirectionStats>;
    fPair: TPair<String, TDirectionStats>;
    fStats: TDirectionStats;
    fOtherSite: String;
    fEntry: TDirEntry;

    procedure _SortEntries(var a: TArray<TDirEntry>);
      procedure QuickSort(L, R: Integer);
      var
        I, J: Integer;
        Pivot: TDirEntry;
        Tmp: TDirEntry;
      begin
        I := L;
        J := R;
        Pivot := a[(L + R) div 2];
        repeat
          while (a[I].Bytes > Pivot.Bytes) or
                ((a[I].Bytes = Pivot.Bytes) and (CompareText(a[I].Site, Pivot.Site) < 0)) do
            Inc(I);
          while (a[J].Bytes < Pivot.Bytes) or
                ((a[J].Bytes = Pivot.Bytes) and (CompareText(a[J].Site, Pivot.Site) > 0)) do
            Dec(J);
          if I <= J then
          begin
            Tmp := a[I];
            a[I] := a[J];
            a[J] := Tmp;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if L < J then
          QuickSort(L, J);
        if I < R then
          QuickSort(I, R);
      end;
    begin
      if Length(a) > 1 then
        QuickSort(0, High(a));
    end;

  var
    i: integer;
  begin
    SetLength(aEntries, 0);
    fSiteStats := TDictionary<String, TDirectionStats>.Create;
    try
      case aDirection of
        stFrom:
          begin
            fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client,
              'SrcSiteRec.Name = ? AND timestamp > date(?, ?)',
              [], [aSitename, 'now', aSQLPeriod]);
            try
              while fStatsRec.FillOne do
              begin
                if aSitename <> UTF8ToString(fStatsRec.SrcSiteRec.Name) then
                  Continue;
                fOtherSite := UTF8ToString(fStatsRec.DstSiteRec.Name);
                if not fSiteStats.TryGetValue(fOtherSite, fStats) then
                begin
                  fStats.Bytes := 0;
                  fStats.Files := 0;
                end;
                fStats.Bytes := fStats.Bytes + fStatsRec.FileInfoRec.FileSize;
                Inc(fStats.Files);
                fSiteStats.AddOrSetValue(fOtherSite, fStats);
              end;
            finally
              fStatsRec.Free;
            end;
          end;

        stTo:
          begin
            fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client,
              'DstSiteRec.Name = ? AND timestamp > date(?, ?)',
              [], [aSitename, 'now', aSQLPeriod]);
            try
              while fStatsRec.FillOne do
              begin
                if aSitename <> UTF8ToString(fStatsRec.DstSiteRec.Name) then
                  Continue;
                fOtherSite := UTF8ToString(fStatsRec.SrcSiteRec.Name);
                if not fSiteStats.TryGetValue(fOtherSite, fStats) then
                begin
                  fStats.Bytes := 0;
                  fStats.Files := 0;
                end;
                fStats.Bytes := fStats.Bytes + fStatsRec.FileInfoRec.FileSize;
                Inc(fStats.Files);
                fSiteStats.AddOrSetValue(fOtherSite, fStats);
              end;
            finally
              fStatsRec.Free;
            end;
          end;
      end;

      SetLength(aEntries, fSiteStats.Count);
      i := 0;
      for fPair in fSiteStats do
      begin
        fEntry.Site := fPair.Key;
        fEntry.Bytes := fPair.Value.Bytes;
        fEntry.Files := fPair.Value.Files;
        aEntries[i] := fEntry;
        Inc(i);
      end;
      _SortEntries(aEntries);
    finally
      fSiteStats.Free;
    end;
  end;

  procedure _WriteDirArray(const aFieldName: RawUTF8; const aEntries: TArray<TDirEntry>; var aFirstField: Boolean;
    const aWriter: TJsonWriter);
  var
    i: integer;
  begin
    if not aFirstField then
      aWriter.AddComma;
    aFirstField := False;
    aWriter.AddFieldName(aFieldName);
    aWriter.AddDirect('[');
    for i := 0 to High(aEntries) do
    begin
      if i > 0 then
        aWriter.AddComma;
      aWriter.AddDirect('{');
      aWriter.AddFieldName('site');
      aWriter.AddJsonString(UTF8Encode(aEntries[i].Site));
      aWriter.AddComma;
      aWriter.AddFieldName('bytes');
      aWriter.Add(aEntries[i].Bytes);
      aWriter.AddComma;
      aWriter.AddFieldName('files');
      aWriter.Add(aEntries[i].Files);
      aWriter.AddDirect('}');
    end;
    aWriter.AddDirect(']');
  end;

  function _NormalizePeriod(const aPeriod: String): String;
  var
    p: String;
  begin
    p := UpperCase(Trim(aPeriod));
    if (p <> 'YEAR') and (p <> 'MONTH') then
      p := 'DAY';
    Result := p;
  end;

var
  temp: TTextWriterStackBuffer;
  w: TJsonWriter;
  i: integer;
  s: TSite;
  fTotals: TFileSizeStats;
  fInBySite, fOutBySite: TArray<TDirEntry>;
  fFirstSite, fFirstField: Boolean;
begin
  fPeriod := _NormalizePeriod(aPeriod);
  fSQLPeriod := _SQLPeriodFromPeriod(fPeriod);
  fSiteFilter := Trim(aSitename);
  if fSiteFilter = '' then
    fSiteFilter := '*';

  w := TJsonWriter.CreateOwnedStream(temp);
  try
    w.AddDirect('{');
    w.AddFieldName('enabled');
    w.Add(IsStatsDatabaseActive);
    w.AddComma;
    w.AddFieldName('site');
    w.AddJsonString(UTF8Encode(fSiteFilter));
    w.AddComma;
    w.AddFieldName('period');
    w.AddJsonString(UTF8Encode(fPeriod));
    w.AddComma;
    w.AddFieldName('sqlPeriod');
    w.AddJsonString(UTF8Encode(fSQLPeriod));
    w.AddComma;
    w.AddFieldName('detailed');
    w.Add(aDetailed);

    if not IsStatsDatabaseActive then
    begin
      w.AddComma;
      w.AddFieldName('error');
      w.AddJsonString(UTF8Encode('Stats are disabled.'));
      w.AddDirect('}');
      w.SetText(RawUtf8(Result));
      Exit;
    end;

    w.AddComma;
    w.AddFieldName('sites');
    w.AddDirect('[');

    fAllBytes := 0;
    fAllFiles := 0;
    fFirstSite := True;

    if fSiteFilter = '*' then
    begin
      if sites <> nil then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          s := TSite(sites.Items[i]);
          if s = nil then
            Continue;
          if (s.Name = getAdminSiteName) then
            Continue;

          _GetTransferStats(s.Name, fSQLPeriod, fTotals);
          Inc(fAllBytes, fTotals.InBytes + fTotals.OutBytes);
          Inc(fAllFiles, fTotals.InFiles + fTotals.OutFiles);

          if not fFirstSite then
            w.AddComma;
          fFirstSite := False;

          w.AddDirect('{');
          fFirstField := True;

          w.AddFieldName('name');
          w.AddJsonString(UTF8Encode(s.Name));
          fFirstField := False;

          w.AddComma;
          w.AddFieldName('inBytes');
          w.Add(fTotals.InBytes);
          w.AddComma;
          w.AddFieldName('outBytes');
          w.Add(fTotals.OutBytes);
          w.AddComma;
          w.AddFieldName('inFiles');
          w.Add(fTotals.InFiles);
          w.AddComma;
          w.AddFieldName('outFiles');
          w.Add(fTotals.OutFiles);

          if aDetailed then
          begin
            _GetDetailedTransferStats(s.Name, fSQLPeriod, stTo, fInBySite);
            _GetDetailedTransferStats(s.Name, fSQLPeriod, stFrom, fOutBySite);
            fFirstField := False;
            _WriteDirArray('inBySite', fInBySite, fFirstField, w);
            _WriteDirArray('outBySite', fOutBySite, fFirstField, w);
          end;

          w.AddDirect('}');
        end;
      end;
    end
    else
    begin
      _GetTransferStats(fSiteFilter, fSQLPeriod, fTotals);
      Inc(fAllBytes, fTotals.InBytes + fTotals.OutBytes);
      Inc(fAllFiles, fTotals.InFiles + fTotals.OutFiles);

      w.AddDirect('{');
      fFirstField := True;
      w.AddFieldName('name');
      w.AddJsonString(UTF8Encode(fSiteFilter));
      fFirstField := False;
      w.AddComma;
      w.AddFieldName('inBytes');
      w.Add(fTotals.InBytes);
      w.AddComma;
      w.AddFieldName('outBytes');
      w.Add(fTotals.OutBytes);
      w.AddComma;
      w.AddFieldName('inFiles');
      w.Add(fTotals.InFiles);
      w.AddComma;
      w.AddFieldName('outFiles');
      w.Add(fTotals.OutFiles);
      if aDetailed then
      begin
        _GetDetailedTransferStats(fSiteFilter, fSQLPeriod, stTo, fInBySite);
        _GetDetailedTransferStats(fSiteFilter, fSQLPeriod, stFrom, fOutBySite);
        fFirstField := False;
        _WriteDirArray('inBySite', fInBySite, fFirstField, w);
        _WriteDirArray('outBySite', fOutBySite, fFirstField, w);
      end;
      w.AddDirect('}');
    end;

    w.AddDirect(']');
    w.AddComma;
    w.AddFieldName('totalBytes');
    w.Add(fAllBytes);
    w.AddComma;
    w.AddFieldName('totalFiles');
    w.Add(fAllFiles);
    w.AddDirect('}');
    w.SetText(RawUtf8(Result));
  finally
    w.Free;
  end;
end;

function StatsGetRecentRacesJson(const aPage: integer; const aPageSize: integer; const aSinceUnix: Int64): RawJSON;
var
  page: integer;
  pageSize: integer;
  offset: integer;
  statsRec: TSQLStatsRecord;
  sinceDt: TDateTime;
  sinceIso: RawUTF8;
  whereSql: RawUTF8;
  resultDoc: variant;
  row: variant;
  itemsVar: variant;
  itemsArr: TDocVariantData absolute itemsVar;
begin
  page := aPage;
  if page <= 0 then
    page := 1;
  if page > 5 then
    page := 5;

  pageSize := aPageSize;
  if pageSize <= 0 then
    pageSize := 500;
  if pageSize > 500 then
    pageSize := 500;

  offset := (page - 1) * pageSize;

  TDocVariant.New(resultDoc);
  TDocVariantData(resultDoc).AddValue('enabled', IsStatsDatabaseActive);
  TDocVariantData(resultDoc).AddValue('error', '');
  TDocVariantData(resultDoc).AddValue('page', page);
  TDocVariantData(resultDoc).AddValue('pageSize', pageSize);
  TDocVariantData(resultDoc).AddValue('maxPages', 5);
  itemsArr.InitFast(dvArray);

  if not IsStatsDatabaseActive then
  begin
    TDocVariantData(resultDoc).AddValue('error', 'stats disabled');
    TDocVariantData(resultDoc).AddValue('items', itemsVar);
    Result := VariantSaveJSON(resultDoc);
    Exit;
  end;

  try
    if aSinceUnix > 0 then
    begin
      sinceDt := UnixToDateTime(aSinceUnix, False);
      sinceIso := DateToIso8601(sinceDt, False);
      whereSql := StringToUTF8(Format('timestamp >= ? order by timestamp desc limit %d offset %d', [pageSize, offset]));
      statsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, whereSql, [], [sinceIso]);
    end
    else
    begin
      whereSql := StringToUTF8(Format('1=1 order by timestamp desc limit %d offset %d', [pageSize, offset]));
      statsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, whereSql, [], []);
    end;
    try
      while statsRec.FillOne do
      begin
        TDocVariant.New(row);
        TDocVariantData(row).AddValue('Id', statsRec.ID);
        TDocVariantData(row).AddValue('TsUnix', DateTimeToUnix(statsRec.FileInfoRec.TimeStamp, False));
        TDocVariantData(row).AddValue('SrcSite', statsRec.SrcSiteRec.Name);
        TDocVariantData(row).AddValue('DstSite', statsRec.DstSiteRec.Name);
        TDocVariantData(row).AddValue('Section', statsRec.SectionRec.Section);
        TDocVariantData(row).AddValue('Release', statsRec.FileInfoRec.ReleaseName);
        TDocVariantData(row).AddValue('FileName', statsRec.FileInfoRec.FileName);
        TDocVariantData(row).AddValue('SizeBytes', statsRec.FileInfoRec.FileSize);
        itemsArr.AddItem(row);
      end;
    finally
      statsRec.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] StatsGetRecentRacesJson: %s', [E.Message]));
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(E.Message));
    end;
  end;

  TDocVariantData(resultDoc).AddValue('items', itemsVar);
  Result := VariantSaveJSON(resultDoc);
end;

function StatsGetReleaseRacesJson(const aRelease: String; const aPage: integer; const aPageSize: integer; const aSinceUnix: Int64): RawJSON;
var
  page: integer;
  pageSize: integer;
  offset: integer;
  statsRec: TSQLStatsRecord;
  sinceDt: TDateTime;
  sinceIso: RawUTF8;
  releaseUtf8: RawUTF8;
  whereSql: RawUTF8;
  resultDoc: variant;
  row: variant;
  itemsVar: variant;
  itemsArr: TDocVariantData absolute itemsVar;
begin
  page := aPage;
  if page <= 0 then
    page := 1;
  if page > 5 then
    page := 5;

  pageSize := aPageSize;
  if pageSize <= 0 then
    pageSize := 500;
  if pageSize > 500 then
    pageSize := 500;

  offset := (page - 1) * pageSize;

  TDocVariant.New(resultDoc);
  TDocVariantData(resultDoc).AddValue('enabled', IsStatsDatabaseActive);
  TDocVariantData(resultDoc).AddValue('error', '');
  TDocVariantData(resultDoc).AddValue('page', page);
  TDocVariantData(resultDoc).AddValue('pageSize', pageSize);
  TDocVariantData(resultDoc).AddValue('maxPages', 5);
  TDocVariantData(resultDoc).AddValue('release', StringToUTF8(Trim(aRelease)));
  itemsArr.InitFast(dvArray);

  if not IsStatsDatabaseActive then
  begin
    TDocVariantData(resultDoc).AddValue('error', 'stats disabled');
    TDocVariantData(resultDoc).AddValue('items', itemsVar);
    Result := VariantSaveJSON(resultDoc);
    Exit;
  end;

  if Trim(aRelease) = '' then
  begin
    TDocVariantData(resultDoc).AddValue('error', 'release required');
    TDocVariantData(resultDoc).AddValue('items', itemsVar);
    Result := VariantSaveJSON(resultDoc);
    Exit;
  end;

  try
    releaseUtf8 := StringToUTF8(Trim(aRelease));

    if aSinceUnix > 0 then
    begin
      sinceDt := UnixToDateTime(aSinceUnix, False);
      sinceIso := DateToIso8601(sinceDt, False);
      whereSql := StringToUTF8(Format('FileInfoRec.ReleaseName = ? AND timestamp >= ? order by timestamp desc limit %d offset %d', [pageSize, offset]));
      statsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, whereSql, [], [releaseUtf8, sinceIso]);
    end
    else
    begin
      whereSql := StringToUTF8(Format('FileInfoRec.ReleaseName = ? order by timestamp desc limit %d offset %d', [pageSize, offset]));
      statsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, whereSql, [], [releaseUtf8]);
    end;

    try
      while statsRec.FillOne do
      begin
        TDocVariant.New(row);
        TDocVariantData(row).AddValue('Id', statsRec.ID);
        TDocVariantData(row).AddValue('TsUnix', DateTimeToUnix(statsRec.FileInfoRec.TimeStamp, False));
        TDocVariantData(row).AddValue('SrcSite', statsRec.SrcSiteRec.Name);
        TDocVariantData(row).AddValue('DstSite', statsRec.DstSiteRec.Name);
        TDocVariantData(row).AddValue('Section', statsRec.SectionRec.Section);
        TDocVariantData(row).AddValue('Release', statsRec.FileInfoRec.ReleaseName);
        TDocVariantData(row).AddValue('FileName', statsRec.FileInfoRec.FileName);
        TDocVariantData(row).AddValue('SizeBytes', statsRec.FileInfoRec.FileSize);
        itemsArr.AddItem(row);
      end;
    finally
      statsRec.Free;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] StatsGetReleaseRacesJson: %s', [E.Message]));
      TDocVariantData(resultDoc).AddValue('error', UTF8Encode(E.Message));
    end;
  end;

  TDocVariantData(resultDoc).AddValue('items', itemsVar);
  Result := VariantSaveJSON(resultDoc);
end;

procedure writeStatsToDB(const aStatRaceRecord: TStatRaceRecord);
var
  fSrcSiteRec, fDstSiteRec: TSQLSitesRecord;
  fSectionRec: TSQLSectionRecord;
  fFileInfoRec: TSQLFileInfoRecord;
  fStatsRec: TSQLStatsRecord;
begin
  // we only need the ID
  fSrcSiteRec := TSQLSitesRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'Name = ?', [aStatRaceRecord.FSrcSite], 'ID');
  fDstSiteRec := TSQLSitesRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'Name = ?', [aStatRaceRecord.FDstSite], 'ID');
  fSectionRec := TSQLSectionRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'Section = ?', [aStatRaceRecord.FSection], 'ID');
  try
    if not fSrcSiteRec.FillOne then
    begin
      fSrcSiteRec.Name := StringToUTF8(aStatRaceRecord.FSrcSite);

      if ORMStatsDB.Add(fSrcSiteRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add srcsite %s to database!', [aStatRaceRecord.FSrcSite]);
        exit;
      end;
    end;

    if not fDstSiteRec.FillOne then
    begin
      fDstSiteRec.Name := StringToUTF8(aStatRaceRecord.FDstSite);

      if ORMStatsDB.Add(fDstSiteRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add dstsite %s to database!', [aStatRaceRecord.FDstSite]);
        exit;
      end;
    end;

    if not fSectionRec.FillOne then
    begin
      fSectionRec.Section := StringToUTF8(aStatRaceRecord.FSection);

      if ORMStatsDB.Add(fSectionRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add section %s to database!', [aStatRaceRecord.FSection]);
        exit;
      end;
    end;

    // we only need the ID
    fFileInfoRec := TSQLFileInfoRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'ReleaseName = ? AND FileName = ?', [aStatRaceRecord.FRls, aStatRaceRecord.FFilename], 'ID');
    try
      if not fFileInfoRec.FillOne then
      begin
        fFileInfoRec.ReleaseName := StringToUTF8(aStatRaceRecord.FRls);
        fFileInfoRec.FileName := StringToUTF8(aStatRaceRecord.FFilename);
        fFileInfoRec.FileSize := aStatRaceRecord.FFilesize;
        fFileInfoRec.TimeStamp := Now;

        if ORMStatsDB.Add(fFileInfoRec, True, False) = 0 then
        begin
          Debug(dpError, section, '[statsProcessRace] Could not add %s file info for %s (%d) to database!', [aStatRaceRecord.FRls, aStatRaceRecord.FFilename, aStatRaceRecord.FFilesize]);
          exit;
        end;
      end;

      // prevent duplicate entries
      fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'SrcSiteRec = ? AND DstSiteRec = ? AND SectionRec = ? AND FileInfoRec = ?', [fSrcSiteRec.ID, fDstSiteRec.ID, fSectionRec.ID, fFileInfoRec.ID], 'ID');
      try
        if not fStatsRec.FillOne then
        begin
          fStatsRec.SrcSiteRec := fSrcSiteRec.AsTOrm;
          fStatsRec.DstSiteRec := fDstSiteRec.AsTOrm;
          fStatsRec.SectionRec := fSectionRec.AsTOrm;
          fStatsRec.FileInfoRec := fFileInfoRec.AsTOrm;

          if ORMStatsDB.Add(fStatsRec, True, False) = 0 then
          begin
            Debug(dpError, section, '[statsProcessRace] Could not add stats record for %s %s (%d) to database!', [aStatRaceRecord.FRls, aStatRaceRecord.FFilename, aStatRaceRecord.FFilesize]);
            exit;
          end;
        end;
      finally
        fStatsRec.Free;
      end;
    finally
      fFileInfoRec.Free;
    end;
  finally
    fSrcSiteRec.Free;
    fDstSiteRec.Free;
    fSectionRec.Free;
  end;
end;

procedure statsProcessRace(const aSrcSite, aDstSite, aSection, aRls, aFilename: String; const aFilesize: Int64);
var
  fStatRaceRecord: TStatRaceRecord;
begin
  if not IsStatsDatabaseActive then
  begin
    Debug(dpSpam, section, '[statsProcessRace] stats disabled.');
    Exit;
  end;

  if (aFilesize < _GetMinFilesize) then
  begin
    Debug(dpSpam, section, Format('[statsProcessRace] Filesize %d for %s is too small', [aFilesize, aFilename]));
    Exit;
  end;

  // fill record
  fStatRaceRecord.FSrcSite := aSrcSite;
  fStatRaceRecord.FDstSite := aDstSite;
  fStatRaceRecord.FSection := aSection;
  fStatRaceRecord.FRls := aRls;
  fStatRaceRecord.FFilename := aFileName;
  fStatRaceRecord.FFilesize := aFilesize;

  try
    glStatRaceLock.Enter('statsProcessRace');
    try
      glStatRaceQueue.Enqueue(fStatRaceRecord);
    finally
      glStatRaceLock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] QueueStats: %s', [e.Message]));
    end;
  end;
end;

function RemoveStats(const aSitename: String): Boolean; overload;
var
  fStatsRec: TSQLStatsRecord;
  fFileInfoIDs: TList<Integer>;
  fItem, fID: Integer;
  fOnlyUsedForDeletedSites: Boolean;
begin
  Result := False;

  if not IsStatsDatabaseActive then
  begin
    Debug(dpSpam, section, '[RemoveStats] stats disabled.');
    exit;
  end;

  { delete sitename from site table }
  try
    if not ORMStatsDB.Delete(TSQLSitesRecord, 'Name = ?', [aSitename]) then
    begin
      Debug(dpError, section, '[RemoveStats] Could not remove %s!', [aSitename]);
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveStats: %s', [e.Message]));
      exit;
    end;
  end;

  { delete fileinfo table entries only if not referenced more than once }
  fFileInfoIDs := TList<Integer>.Create;
  try
    // get all file IDs where src and dst are already deleted
    fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'SrcSiteRec = ? AND DstSiteRec = ?', [], [0, 0]);
    try
      while fStatsRec.FillOne do
      begin
        fID := TID(fStatsRec.FileInfoRec);
        if not fFileInfoIDs.Contains(fID) then
          fFileInfoIDs.Add(fID);
      end;
    finally
      fStatsRec.Free;
    end;

    for fItem in fFileInfoIDs do
    begin
      fOnlyUsedForDeletedSites := True;

      // try to get entry which use the same FileInfo Record but at least one site is still there
      fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB.Client, '(SrcSiteRec <> ? OR DstSiteRec <> ?) AND FileInfoRec = ?', [], [0, 0, fItem]);
      try
        if fStatsRec.FillOne then
        begin
          fOnlyUsedForDeletedSites := False;
        end;
      finally
        fStatsRec.Free;
      end;

      // remove items from db which are used only for deleted sites
      if fOnlyUsedForDeletedSites then
      begin
        if not ORMStatsDB.Delete(TSQLFileInfoRecord, 'ID = ?', [fItem]) then
        begin
          Debug(dpError, Section, '[RemoveStats] Could not delete fileinfo ID %d!', [fItem]);
          Exit;
        end;
      end;
    end;
  finally
    fFileInfoIDs.Free;
  end;

  if not ORMStatsDB.Delete(TSQLStatsRecord, 'SrcSiteRec = ? AND DstSiteRec = ? AND FileInfoRec = ?', [0, 0, 0]) then
  begin
    Debug(dpError, section, '[RemoveStats] Could not delete stats record!');
    exit;
  end;

  Result := True;
end;

procedure StatRaces(const aNetname, aChannel, aSitename, aPeriod: String; const aDetailed: Boolean);
type
  TFileSizeStats = record
    FilesCountIn: Int64;
    FilesCountOut: Int64;
    SizeIn: Double;
    SizeOut: Double;
  end;
  TStatsDirection = (stFrom, stTo);
var
  s: TSite;
  i: integer;
  fSQLPeriod: String;
  fFileSizeStats: TFileSizeStats;
  fAllFilesTransfered: Int64;
  fAllSizeTransfered: Double;
  fSizeAllUnit: String;

  function GetSQLPeriod(const aPeriod: String): String;
  begin
    if (aPeriod = 'MONTH') then
    begin
      Result := 'start of month';
    end
    else if (aPeriod = 'YEAR') then
    begin
      Result := 'start of year';
    end
    else
    begin
      Result := 'start of day';
    end;
  end;

  procedure InitValues(out aFileSizeStats: TFileSizeStats);
  begin
    aFileSizeStats.FilesCountIn := 0;
    aFileSizeStats.FilesCountOut := 0;
    aFileSizeStats.SizeIn := 0;
    aFileSizeStats.SizeOut := 0;
  end;

  procedure GetTransferStats(const aSitename, aSQLPeriod: String; out aFileSizeStats: TFileSizeStats);
  var
    fStatsRec: TSQLStatsRecord;
  begin
    InitValues(aFileSizeStats);

    fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, 
      '(DstSiteRec.Name = ? OR SrcSiteRec.Name = ?) AND timestamp > date(?, ?)',
      [], [aSitename, aSitename, 'now', aSQLPeriod]);
    try
      while fStatsRec.FillOne do
      begin
        if aSitename = UTF8ToString(fStatsRec.DstSiteRec.Name) then
        begin
          aFileSizeStats.SizeIn := aFileSizeStats.SizeIn + fStatsRec.FileInfoRec.FileSize;
          Inc(aFileSizeStats.FilesCountIn);
        end
        else if aSitename = UTF8ToString(fStatsRec.SrcSiteRec.Name) then
        begin
          aFileSizeStats.SizeOut := aFileSizeStats.SizeOut + fStatsRec.FileInfoRec.FileSize;
          Inc(aFileSizeStats.FilesCountOut);
        end;
      end;
    finally
      fStatsRec.Free;
    end;
  end;

  procedure GetDetailedTransferStats(const aNetname, aChannel, aSitename, aSQLPeriod: String; const aDirection: TStatsDirection);
  var
    fStatsRec: TSQLStatsRecord;
    fSiteInfosList: TDictionary<String, TFileSizeStats>;
    fFileSizeStats: TFileSizeStats;
    fListItem: TPair<String, TFileSizeStats>;
    fSitename, fSizeUnit: String;
    fSize: Double;
  begin
    fSiteInfosList := TDictionary<String, TFileSizeStats>.Create;
    try
      case aDirection of
        stFrom:
          begin
            // input site is source
            fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client,
              'SrcSiteRec.Name = ? AND timestamp > date(?, ?)',
              [], [aSitename, 'now', aSQLPeriod]);
            try
              while fStatsRec.FillOne do
              begin
                if aSitename = UTF8ToString(fStatsRec.SrcSiteRec.Name) then
                begin
                  fSitename := UTF8ToString(fStatsRec.DstSiteRec.Name);
                  if not fSiteInfosList.ContainsKey(fSitename) then
                  begin
                    InitValues(fFileSizeStats);

                    fFileSizeStats.SizeOut := fFileSizeStats.SizeOut + fStatsRec.FileInfoRec.FileSize;
                    Inc(fFileSizeStats.FilesCountOut);

                    fSiteInfosList.Add(fSitename, fFileSizeStats);
                  end
                  else
                  begin
                    fFileSizeStats := fSiteInfosList.Items[fSitename];

                    fFileSizeStats.SizeOut := fFileSizeStats.SizeOut + fStatsRec.FileInfoRec.FileSize;
                    Inc(fFileSizeStats.FilesCountOut);

                    fSiteInfosList.AddOrSetValue(fSitename, fFileSizeStats);
                  end;
                end;
              end;
            finally
              fStatsRec.Free;
            end;

            for fListItem in fSiteInfosList do
            begin
              fSize := fListItem.Value.SizeOut;
              RecalcSizeValueAndUnit(fSize, fSizeUnit, 0);
              irc_addtext(aNetname, aChannel, Format('  <b>to</b> %s: %.2f %s (%d files)', [fListItem.Key, fSize, fSizeUnit, fListItem.Value.FilesCountOut]));
            end;
          end;

        stTo:
          begin
            // input site is destination
            fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client,
              'DstSiteRec.Name = ? AND timestamp > date(?, ?)',
              [], [aSitename, 'now', aSQLPeriod]);
            try
              while fStatsRec.FillOne do
              begin
                if aSitename = UTF8ToString(fStatsRec.DstSiteRec.Name) then
                begin
                  fSitename := UTF8ToString(fStatsRec.SrcSiteRec.Name);
                  if not fSiteInfosList.ContainsKey(fSitename) then
                  begin
                    InitValues(fFileSizeStats);

                    fFileSizeStats.SizeIn := fFileSizeStats.SizeIn + fStatsRec.FileInfoRec.FileSize;
                    Inc(fFileSizeStats.FilesCountIn);

                    fSiteInfosList.Add(fSitename, fFileSizeStats);
                  end
                  else
                  begin
                    fFileSizeStats := fSiteInfosList.Items[fSitename];

                    fFileSizeStats.SizeIn := fFileSizeStats.SizeIn + fStatsRec.FileInfoRec.FileSize;
                    Inc(fFileSizeStats.FilesCountIn);

                    fSiteInfosList.AddOrSetValue(fSitename, fFileSizeStats);
                  end;
                end;
              end;
            finally
              fStatsRec.Free;
            end;

            for fListItem in fSiteInfosList do
            begin
              fSize := fListItem.Value.SizeIn;
              RecalcSizeValueAndUnit(fSize, fSizeUnit, 0);
              irc_addtext(aNetname, aChannel, Format('  <b>from</b> %s: %.2f %s (%d files)', [fListItem.Key, fSize, fSizeUnit, fListItem.Value.FilesCountIn]));
            end;
          end;
      end;
    finally
      fSiteInfosList.Free;
    end;
  end;

  procedure PrintStatsToIRC(const aSitename, aSQLPeriod: String; var aFileSizeStats: TFileSizeStats);
  var
    fSizeInUnit, fSizeOutUnit: String;
  begin
    RecalcSizeValueAndUnit(aFileSizeStats.SizeIn, fSizeInUnit, 0);
    RecalcSizeValueAndUnit(aFileSizeStats.SizeOut, fSizeOutUnit, 0);
    irc_addtext(aNetname, aChannel, Format('%s race stats of site: <b><c7>%s</c></b>', [aSQLPeriod, aSitename]));

    irc_addtext(aNetname, aChannel, Format('TOTAL <b>in</b>: <c9>%.2f</c> %s (%d files)', [aFileSizeStats.SizeIn, fSizeInUnit, aFileSizeStats.FilesCountIn]));
    if aDetailed then
    begin
      GetDetailedTransferStats(aNetname, aChannel, aSitename, aSQLPeriod, stTo);
    end;

    irc_addtext(aNetname, aChannel, Format('TOTAL <b>out</b>: <c4>%.2f</c> %s (%d files)', [aFileSizeStats.SizeOut, fSizeOutUnit, aFileSizeStats.FilesCountOut]));
    if aDetailed then
    begin
      GetDetailedTransferStats(aNetname, aChannel, aSitename, aSQLPeriod, stFrom);
    end;
  end;

begin
  if not IsStatsDatabaseActive then
  begin
    Debug(dpSpam, section, '[StatRaces] stats disabled.');
    irc_addtext(aNetname, aChannel, 'Stats are disabled.');
    exit;
  end;

  fSQLPeriod := GetSQLPeriod(aPeriod);

  if aSitename = '*' then
  begin
    fAllFilesTransfered := 0;
    fAllSizeTransfered := 0;

    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;

      GetTransferStats(s.Name, fSQLPeriod, fFileSizeStats);

      // in and out values will have the same total amount
      Inc(fAllFilesTransfered, fFileSizeStats.FilesCountIn + fFileSizeStats.FilesCountOut);
      fAllSizeTransfered := fAllSizeTransfered + fFileSizeStats.SizeIn + fFileSizeStats.SizeOut;

      PrintStatsToIRC(s.Name, fSQLPeriod, fFileSizeStats);
    end;

    RecalcSizeValueAndUnit(fAllSizeTransfered, fSizeAllUnit, 0);
    irc_addtext(aNetname, aChannel, Format('<b>Total In + Out:</b> <c07>%.2f</c> %s (%d files)', [fAllSizeTransfered, fSizeAllUnit, fAllFilesTransfered]));
  end
  else
  begin
    s := FindSiteByName('', aSitename);
    GetTransferStats(s.Name, fSQLPeriod, fFileSizeStats);
    PrintStatsToIRC(s.Name, fSQLPeriod, fFileSizeStats);
  end;
end;

procedure doStatsBackup(const aPath, aFileName: String);
begin
  if not IsStatsDatabaseActive then
  begin
    Debug(dpSpam, section, '[doStatsBackup] stats disabled.');
    exit;
  end;

  if ORMStatsDB.DB.BackupBackground(aPath + aFileName, -1, 0, nil) then
    ORMStatsDB.DB.BackupBackgroundWaitUntilFinished(5);
end;

constructor TWriteStatsToDBThread.Create;
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('StatsWriter', self.ThreadID);
  {$ENDIF}
  FreeOnTerminate := True;
  glTWriteStatsThreadRunning := True;
end;

destructor TWriteStatsToDBThread.Destroy;
begin
  glTWriteStatsThreadRunning := False;
end;

procedure TWriteStatsToDBThread.Execute;
var
  fStatRaceQueue: TQueue<TStatRaceRecord>;
  i: Integer;
  fRec: TSQLFileInfoRecord;
  fCleanDate: TDateTime;
begin
  while IsStatsDatabaseActive do
  begin

    //only sleep if the thread should not stop, else finish work as fast as possible
    if not glWriteStatsThreadShouldStop then
      sleep(1000);

    try
      // replace glStatRaceLock with a new queue and process the records of the existing one
      fStatRaceQueue := glStatRaceQueue;

      // lock here to be sure the enqueuing threads don't use the old reference while we're iterating
      glStatRaceLock.Enter('Execute');
      try
        glStatRaceQueue := TQueue<TStatRaceRecord>.Create;
      finally
        glStatRaceLock.Leave;
      end;

      //if the thread should stop and there are no more items to process, break the loop
      if glWriteStatsThreadShouldStop and (fStatRaceQueue.Count = 0) then
        break;

      if fStatRaceQueue.Count > 0 then
        Debug(dpSpam, Section, Format('Write %d stats entries to the DB', [fStatRaceQueue.Count]));

      try
        ORMStatsDB.DB.TransactionBegin;
        try
          while fStatRaceQueue.Count > 0 do
          begin
            writeStatsToDB(fStatRaceQueue.Dequeue);
          end;
          ORMStatsDB.DB.Commit;
        except
          on e: Exception do
          begin
            Debug(dpError, Section, Format('[EXCEPTION] WriteRaceStats DB: %s', [e.Message]));
            ORMStatsDB.DB.Rollback;
          end;
        end;
      finally
        fStatRaceQueue.Free;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] WriteRaceStats: %s', [e.Message]));
      end;
    end;

    try
      if (glDeleteAfterDays > 0) and not glWriteStatsThreadShouldStop then
      begin

        // clean the stats DB of old entries once each day
        if (DaysBetween(glLastStatsCleanTime, Today()) > 0) then
        begin
          i := 0;
          fCleanDate := IncDay(Today(), glDeleteAfterDays * -1);

          // only delete 1000 at a time
          fRec := TSQLFileInfoRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'TimeStamp < ? limit 1000', [DateToIso8601(fCleanDate, False)]);
          try
            ORMStatsDB.DB.TransactionBegin;
            try
              while not glWriteStatsThreadShouldStop and fRec.FillOne do
              begin
                ORMStatsDB.Delete(TSQLStatsRecord, 'FileInfoRec = ?', [fRec.ID]);
                ORMStatsDB.Delete(TSQLFileInfoRecord, 'ID = ?', [fRec.ID]);
                i := i + 1;
              end;
              ORMStatsDB.DB.Commit;
            except
              on e: Exception do
              begin
                Debug(dpError, Section, Format('[EXCEPTION] Clean Stats DB (rollback): %s', [e.Message]));
                ORMStatsDB.DB.Rollback;
              end
            end;
          finally
            fRec.Free;
          end;

          // if no more entries have been found today, check again tomorrow
          if i = 0 then
          begin
            glLastStatsCleanTime := Today();
            Debug(dpSpam, Section, 'Finished cleaning old stats for today, now do "pragma optimize"');
            ORMStatsDB.Execute('pragma optimize;');
          end
          else
            Debug(dpSpam, Section, Format('Cleaned %d entries from stats db which are older than %d days', [i, glDeleteAfterDays]));

        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, Section, Format('[EXCEPTION] Clean Stats DB: %s', [e.Message]));
      end;
    end;
  end;
end;

end.
