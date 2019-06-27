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
  s, bName, filename, filepath: string;
  i: integer;
  sr: TSearchRec;
  skipfiles: TStringList;
begin
  _backuperror := '';
  result := False;
  bName := config.ReadString(section, 'backup_dir', 'backup');
  if not DirectoryExists(bName) then
    Mkdir(bName);

  debug(dpMessage, section, 'Backup process started.');
  bName := MyIncludeTrailingSlash(bName);
  ForceDirectories(bName);

  skipfiles := TStringList.Create;
  skipfiles.CaseSensitive := False;
  try
    skipfiles.CommaText := config.ReadString('backup', 'skipfiles', '');

    try
      if custom then
        bName := bName + 'slftp-custom-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar'
      else
        bName := bName + 'slftp-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar';

      with TTarWriter.Create(bName) do
      begin
        //adding common files
        for I := 0 to cFilecount do
          if ( fileexists(commonFiles[i]) and (skipfiles.IndexOf(commonFiles[i]) = -1) ) then
            AddFile(commonFiles[i]);

        //adding ini files
        for I := 0 to iFilecount do
          if ( fileexists(iniFiles[i]) and (skipfiles.IndexOf(iniFiles[i]) = -1) ) then
            AddFile(iniFiles[i]);

        //adding generated files
        for I := 0 to gFilecount do
          if ( fileexists(generatedFiles[i]) and (skipfiles.IndexOf(generatedFiles[i]) = -1) ) then
            AddFile(generatedFiles[i]);

        filepath := MyIncludeTrailingSlash(DATABASEFOLDERNAME);

        //adding indexer database (only at startup)
        if not IndexerAlive then
        begin
          fileName := Trim(config.ReadString('indexer', 'database', 'indexes.db'));
          if ( fileexists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;

        // adding stats database (only at startup)
        if not StatsAlive then
        begin
          fileName := Trim(config.ReadString('stats', 'database', 'stats.db'));
          if ( fileexists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;

        // adding pretime database (only at startup)
        if not AddPreDbAlive then
        begin
          fileName := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));
          if ( fileexists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;

        // adding tvinfo database (only at startup)
        if not TVInfoDbAlive then
        begin
          fileName := Trim(config.ReadString('tasktvinfo', 'database', 'tvinfos.db'));
          if ( fileexists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;

        (*
        // adding imdb database (soon) (only at startup)
        if not IMDbInfoDbAlive then
        begin
          fileName := Trim(config.ReadString('taskimdb', 'database', 'imdb.db'));
          if ( fileexists(filepath + fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(filepath + fileName);
        end;
        *)

        // adding rtpl dir
        s := MyIncludeTrailingSlash('rtpl');
        if not DirectoryExists(s) then
          Mkdir(s);

        ForceDirectories(s);
        if FindFirst(s + '*.*', faAnyFile - faDirectory, sr) = 0 then
        begin
          repeat
            AddFile(s + sr.Name, 'rtpl/' + sr.name);
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
var
  sr: TSearchRec;
  files: TStringList;
  ages: TList<integer>;
  i: integer;
  oldest_date, oldest_index: integer;
begin
  files := TStringList.Create;
  ages := TList<integer>.Create;
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
          files.Add(sr.Name);
          ages.Add(sr.Time);
        end
        else
        begin
          Debug(dpMessage, '<- BACKUP ->', 'ignored: ' + sr.Name);
        end;
      until FindNext(sr) <> 0;
      {$IFDEF MSWINDOWS}
        SysUtils.FindClose(sr);
      {$ELSE}
        FindClose(sr);
      {$ENDIF}

      // remove oldest backups until we have reach the max backups  we want
      while (files.Count > config.ReadInteger(section, 'keep_backups', 10)) do
      begin
        oldest_index := -1;
        oldest_date := 0;
        for i := 0 to ages.Count - 1 do
        begin
          if ((oldest_date = 0) or (ages[i] < oldest_date)) then
          begin
            oldest_index := i;
            oldest_date := ages[i];
          end;
        end;

        // no backup left to delete
        if oldest_index < 0 then
          Break;

        // delete the file
        try
          {$IFDEF MSWINDOWS}
            DeleteFile({$IFDEF UNICODE}PChar(s + files[oldest_index]){$ELSE}PAnsiChar(s + files[oldest_index]){$ENDIF});
          {$ELSE}
            DeleteFile(s + files[oldest_index]);
          {$ENDIF}
        except
          on e: Exception do
            Debug(dpError, section, Format('[EXCEPTION] DeleteOldBackups: %s', [e.Message]));
        end;

        // remove file from list
        files.BeginUpdate;
        try
          try
            files.Delete(oldest_index);
          except
            continue;
          end;
        finally
          files.EndUpdate;
        end;
        ages.Delete(oldest_index);
      end;
    end;
  finally
    ages.Free;
    files.Free;
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

