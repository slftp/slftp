unit backupunit;

interface

procedure BackupBackup;

function ircBackup(out backupError: string): boolean;

var
  backup_last_backup: TDateTime;

implementation

uses
  Classes, SysUtils, configunit, debugunit, LibTar, mystrings,
  statsunit, indexer, dbtvinfo, dbaddpre, StrUtils, globals, Generics.Collections
  {$IFDEF MSWINDOWS}
    , Windows
  {$ENDIF};

const
  section = 'backup';

var
  _backuperror: string;

{$I common.inc}

function createBackup(custom: boolean = False): boolean;
var
  fRtplPath, fBackupFileName, fFileName, fBackupPath, fDatabasePath: string;
  i: integer;
  sr: TSearchRec;
  skipfiles: TStringList;
begin
  debug(dpMessage, section, 'Backup process started.');
  Result := False;
  _backuperror := '';

  fBackupFileName := config.ReadString(section, 'backup_dir', 'backup');
  fBackupPath := MyIncludeTrailingSlash(fBackupFileName);

  if not DirectoryExists(fBackupPath) then
    Mkdir(fBackupPath);

  ForceDirectories(fBackupFileName);

  skipfiles := TStringList.Create;
  skipfiles.CaseSensitive := False;
  try
    skipfiles.CommaText := config.ReadString('backup', 'skipfiles', '');

    try
      if custom then
        fBackupFileName := fBackupPath + 'slftp-custom-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar'
      else
        fBackupFileName := fBackupPath + 'slftp-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar';

      with TTarWriter.Create(fBackupFileName) do
      begin
        // adding common files
        for I := 0 to cFilecount do
          if ( FileExists(commonFiles[i]) and (skipfiles.IndexOf(commonFiles[i]) = -1) ) then
            AddFile(commonFiles[i]);

        // adding ini files
        for I := 0 to iFilecount do
          if ( FileExists(iniFiles[i]) and (skipfiles.IndexOf(iniFiles[i]) = -1) ) then
            AddFile(iniFiles[i]);

        // adding generated files
        for I := 0 to gFilecount do
          if ( FileExists(generatedFiles[i]) and (skipfiles.IndexOf(generatedFiles[i]) = -1) ) then
            AddFile(generatedFiles[i]);

        fDatabasePath := MyIncludeTrailingSlash(DATABASEFOLDERNAME);

        // adding stats database
        fFileName := Trim(config.ReadString('stats', 'database', 'stats.db'));
        if IsStatsDatabaseActive and ( FileExists(fDatabasePath + fFileName) and (skipfiles.IndexOf(fFileName) = -1) ) then
        begin
          doStatsBackup(fDatabasePath, fFileName + '.bak');
          AddFile(fDatabasePath + fFileName + '.bak', fDatabasePath + fFileName);
          DeleteFile({$IFDEF UNICODE}PChar{$ELSE}PAnsiChar{$ENDIF}(fDatabasePath + fFileName + '.bak'));
        end;

        // adding indexer database (only at startup)
        if not IndexerAlive then
        begin
          fFileName := Trim(config.ReadString('indexer', 'database', 'indexes.db'));
          if ( FileExists(fDatabasePath + fFileName) and (skipfiles.IndexOf(fFileName) = -1) ) then
            AddFile(fDatabasePath + fFileName);
        end;

        // adding pretime database (only at startup)
        if not AddPreDbAlive then
        begin
          fFileName := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));
          if ( FileExists(fDatabasePath + fFileName) and (skipfiles.IndexOf(fFileName) = -1) ) then
            AddFile(fDatabasePath + fFileName);
        end;

        // adding tvinfo database (only at startup)
        if not TVInfoDbAlive then
        begin
          fFileName := Trim(config.ReadString('tasktvinfo', 'database', 'tvinfos.db'));
          if ( FileExists(fDatabasePath + fFileName) and (skipfiles.IndexOf(fFileName) = -1) ) then
            AddFile(fDatabasePath + fFileName);
        end;

        (*
        // adding imdb database (soon) (only at startup)
        if not IMDbInfoDbAlive then
        begin
          fileName := Trim(config.ReadString('taskimdb', 'database', 'imdb.db'));
          if ( FileExists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;
        *)

        // adding rtpl dir
        fRtplPath := MyIncludeTrailingSlash('rtpl');
        if not DirectoryExists(fRtplPath) then
          Mkdir(fRtplPath);

        ForceDirectories(fRtplPath);
        if FindFirst(fRtplPath + '*.*', faAnyFile - faDirectory, sr) = 0 then
        begin
          repeat
            AddFile(fRtplPath + sr.Name, 'rtpl/' + sr.name);
          until FindNext(sr) <> 0;
          {$IFDEF MSWINDOWS}
            SysUtils.FindClose(sr);
          {$ELSE}
            FindClose(sr);
          {$ENDIF}
        end;
        Free;
      end;

      debug(dpMessage, section, 'Backup process finished.');
      Result := True;
    except on E: Exception do
      begin
        debug(dpError, section, '[EXCEPTION] backup failed: ' + e.Message);
        _backuperror := e.Message;
      end;
    end;

  finally
    skipfiles.Free;
  end;

end;

procedure DeleteOldBackups(s: String);
type
  TFileInfoRec = record
    filename: String;
    age: TDateTime;
  end;
var
  sr: TSearchRec;
  records: TList<TFileInfoRec>;
  rec, oldest_record: TFileInfoRec;
begin
  records := TList<TFileInfoRec>.Create;
  try
    s := MyIncludeTrailingSlash(s);
    ForceDirectories(s);

    if FindFirst(s + '*.tar', faAnyFile, sr) < 0 then
    begin
      // no old backups found
      Debug(dpMessage, '<- BACKUP ->', 'No existing backup found');
      exit;
    end
    else
    begin
      repeat
        // enumerate existing backups
        if (not AnsiContainsText(sr.Name, '-custom-')) then
        begin
          Debug(dpMessage, '<- BACKUP ->', 'found backup: ' + sr.Name);
          rec.filename := sr.Name;
          rec.age := sr.TimeStamp;
          records.Add(rec);
        end
        else
        begin
          Debug(dpMessage, '<- BACKUP ->', 'ignored: ' + sr.Name);
        end;
      until FindNext(sr) <> 0;

      {$IFDEF MSWINDOWS}SysUtils.{$ENDIF}FindClose(sr);

      // remove oldest backups until we have reach the max backups we want to keep
      while ((records.Count > 0) and (records.Count > config.ReadInteger(section, 'keep_backups', 10))) do
      begin
        oldest_record := records.First;
        for rec in records do
        begin
          if (rec.age < oldest_record.age) then
          begin
            oldest_record := rec;
          end;
        end;

        // delete the file
        try
          DeleteFile({$IFDEF UNICODE}PChar{$ELSE}PAnsiChar{$ENDIF}(s + oldest_record.filename));
        except
          on e: Exception do
            Debug(dpError, section, Format('[EXCEPTION] DeleteOldBackups: %s', [e.Message]));
        end;

        records.Remove(oldest_record);
      end;
    end;
  finally
    records.Free;
  end;
end;

procedure BackupBackup;
var
  s: String;
begin
  try
    debug(dpMessage, section, 'Backup process started.');

    s := config.ReadString(section, 'backup_dir', 'backup');

    // create backup dir if it doesn't exists
    if not DirectoryExists(s) then
      Mkdir(s);

    // backup
    if createBackup(False) then
    begin
      backup_last_backup := Now;
      DeleteOldBackups(s);
    end
    else
    begin
      debug(dpMessage, section, 'Backup process Failed!');
    end;
  except
    on E: Exception do
      Debug(dpError, section, Format('[EXCEPTION] BackupBackup: %s', [e.Message]));
  end;
end;

function ircBackup(out backupError: string): boolean;
begin
  Result := True;
  if createBackup(True) then
    backup_last_backup := Now
  else
  begin
    backupError := _backuperror;
    Result := False;
  end;
end;

initialization
  backup_last_backup := Now;
end.

