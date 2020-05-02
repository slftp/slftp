unit statsunit;

interface

uses
  SynCommons, mORMot;

type
  TSQLSitesRecord = class(TSQLRecordNoCase)
  private
    FName: RawUTF8; //< sitename
  published
    property Name: RawUTF8 read FName write FName stored AS_UNIQUE;
  end;

  TSQLSectionRecord = class(TSQLRecordNoCase)
  private
    FSection: RawUTF8; //< sectionname
  published
    property Section: RawUTF8 read FSection write FSection stored AS_UNIQUE;
  end;

  TSQLFileInfoRecord = class(TSQLRecordNoCase)
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

  TSQLStatsRecord = class(TSQLRecord)
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

{ Just a helper function to initialize @link(ORMStatsDB) }
procedure statsInit;

{ Just a helper function to free @link(ORMStatsDB) }
procedure statsUninit;

{ Add the raced file and appropriate infos into database
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

{ Creates a backup of stats-database - this is needed because the file is in use and can't be copied
  @param(aPath path where the backup should be stored in the filesystem with last slash, e.g. /path/to/file/)
  @param(aFileName filename including fileextension) }
procedure doStatsBackup(const aPath, aFileName: String);

implementation

uses
  SysUtils, Classes, Contnrs, Generics.Collections, dbhandler, mORMotSQLite3, debugunit, configunit, sitesunit, irc, mystrings;

const
  section = 'stats';

var
  ORMStatsDB: TSQLRestClientDB; //< Rest Client for all database interactions
  ORMStatsModel: TSQLModel; //< SQL ORM model for stats database

function _GetMinFilesize: Int64; inline;
begin
  Result := config.ReadInteger(section, 'min_filesize', 100000);
end;

procedure statsInit;
var
  fDBName: String;
begin
  fDBName := Trim(config.ReadString(section, 'database', 'stats.db'));
  if fDBName = '' then
    exit;

  ORMStatsModel := TSQLModel.Create([TSQLStatsRecord, TSQLSitesRecord, TSQLSectionRecord, TSQLFileInfoRecord]);
  try
    ORMStatsDB := CreateORMSQLite3DB(ORMStatsModel, fDBName, '');
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
    ORMStatsDB.Free;
  end;
  if Assigned(ORMStatsModel) then
  begin
    ORMStatsModel.Free;
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

procedure statsProcessRace(const aSrcSite, aDstSite, aSection, aRls, aFilename: String; const aFilesize: Int64);
var
  fSrcSiteRec, fDstSiteRec: TSQLSitesRecord;
  fSectionRec: TSQLSectionRecord;
  fFileInfoRec: TSQLFileInfoRecord;
  fStatsRec: TSQLStatsRecord;
begin

  if (ORMStatsDB = nil) then
  begin
    Debug(dpSpam, section, '[statsProcessRace] stats disabled.');
    exit;
  end;

  if (aFilesize < _GetMinFilesize) then
  begin
    Debug(dpSpam, section, Format('[statsProcessRace] Filesize %d for %s is too small', [aFilesize, aFilename]));
    exit;
  end;

  // we only need the ID
  fSrcSiteRec := TSQLSitesRecord.CreateAndFillPrepare(ORMStatsDB, 'Name = ?', [aSrcSite], 'ID');
  fDstSiteRec := TSQLSitesRecord.CreateAndFillPrepare(ORMStatsDB, 'Name = ?', [aDstSite], 'ID');
  fSectionRec := TSQLSectionRecord.CreateAndFillPrepare(ORMStatsDB, 'Section = ?', [aSection], 'ID');
  try
    if not fSrcSiteRec.FillOne then
    begin
      fSrcSiteRec.Name := StringToUTF8(aSrcSite);

      if ORMStatsDB.Add(fSrcSiteRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add srcsite %s to database!', [aSrcSite]);
        exit;
      end;
    end;

    if not fDstSiteRec.FillOne then
    begin
      fDstSiteRec.Name := StringToUTF8(aDstSite);

      if ORMStatsDB.Add(fDstSiteRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add dstsite %s to database!', [aDstSite]);
        exit;
      end;
    end;

    if not fSectionRec.FillOne then
    begin
      fSectionRec.Section := StringToUTF8(aSection);

      if ORMStatsDB.Add(fSectionRec, True, False) = 0 then
      begin
        Debug(dpError, section, '[statsProcessRace] Could not add section %s to database!', [aSection]);
        exit;
      end;
    end;

    // we only need the ID
    fFileInfoRec := TSQLFileInfoRecord.CreateAndFillPrepare(ORMStatsDB, 'ReleaseName = ? AND FileName = ?', [aRls, aFilename], 'ID');
    try
      if not fFileInfoRec.FillOne then
      begin
        fFileInfoRec.ReleaseName := StringToUTF8(aRls);
        fFileInfoRec.FileName := StringToUTF8(aFilename);
        fFileInfoRec.FileSize := aFilesize;
        fFileInfoRec.TimeStamp := Now;

        if ORMStatsDB.Add(fFileInfoRec, True, False) = 0 then
        begin
          Debug(dpError, section, '[statsProcessRace] Could not add %s file info for %s (%d) to database!', [aRls, aFilename, aFilesize]);
          exit;
        end;
      end;

      // prevent duplicate entries
      fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB, 'SrcSiteRec = ? AND DstSiteRec = ? AND SectionRec = ? AND FileInfoRec = ?', [fSrcSiteRec.ID, fDstSiteRec.ID, fSectionRec.ID, fFileInfoRec.ID], 'ID');
      try
        if not fStatsRec.FillOne then
        begin
          fStatsRec.SrcSiteRec := fSrcSiteRec.AsTSQLRecord;
          fStatsRec.DstSiteRec := fDstSiteRec.AsTSQLRecord;
          fStatsRec.SectionRec := fSectionRec.AsTSQLRecord;
          fStatsRec.FileInfoRec := fFileInfoRec.AsTSQLRecord;

          if ORMStatsDB.Add(fStatsRec, True, False) = 0 then
          begin
            Debug(dpError, section, '[statsProcessRace] Could not add stats record for %s %s (%d) to database!', [aRls, aFilename, aFilesize]);
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

function RemoveStats(const aSitename: String): Boolean; overload;
var
  fStatsRec: TSQLStatsRecord;
  fFileInfoIDs: TList<Integer>;
  fItem, fID: Integer;
  fOnlyUsedForDeletedSites: Boolean;
begin
  Result := False;

  if (ORMStatsDB = nil) then
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
    fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB, 'SrcSiteRec = ? AND DstSiteRec = ?', [], [0, 0]);
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
      fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB, '(SrcSiteRec <> ? OR DstSiteRec <> ?) AND FileInfoRec = ?', [], [0, 0, fItem]);
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
          Debug(dpError, section, '[RemoveStats] Could not delete fileinfo ID %d!', [fItem]);
          exit;
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

    if (ORMStatsDB = nil) then
    begin
      Debug(dpSpam, section, '[GetTransferStats] stats disabled.');
      exit;
    end;

    InitValues(aFileSizeStats);

    fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB,
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

    if (ORMStatsDB = nil) then
    begin
      Debug(dpSpam, section, '[GetDetailedTransferStats] stats disabled.');
      exit;
    end;

    fSiteInfosList := TDictionary<String, TFileSizeStats>.Create;
    try
      case aDirection of
        stFrom:
        begin
          // input site is source
          fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB,
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
          fStatsRec := TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB,
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
      Inc(fAllFilesTransfered, fFileSizeStats.FilesCountIn);
      fAllSizeTransfered := fAllSizeTransfered + fFileSizeStats.SizeIn;

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
  if ORMStatsDB.DB.BackupBackground(aPath + aFileName, -1, 0, nil) then
    ORMStatsDB.DB.BackupBackgroundWaitUntilFinished(5);
end;

end.
