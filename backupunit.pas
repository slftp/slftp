unit backupunit;

interface

procedure BackupBackup;

function ircBackup(out backupError: string): boolean;

var
  backup_last_backup: TDateTime;

{$I common.inc}

implementation

uses Classes, SysUtils, configunit, debugunit, LibTar, mystrings, uintlist,
  statsunit, indexer, dbtvinfo, dbaddpre, slvision, StrUtils
{$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  section = 'backup';

var
  _backuperror: string;

function createBackup(custom: boolean = False): boolean;
var
  s, bName, filename: string;
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

        //adding databases
        if not IndexerAlive then
        begin
          fileName := Trim(config.ReadString('indexer', 'database', 'disabled'));
          if ( fileexists(fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(fileName);
        end;

        if not StatsAlive then
        begin
          fileName := Trim(config.ReadString('stats', 'database', 'disabled'));
          if ( fileexists(fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(fileName);
        end;

        if not AddPreDbAlive then
        begin
          fileName := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));
          if ( fileexists(fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(fileName);
        end;

        if not TVInfoDbAlive then
        begin
          fileName := Trim(config.ReadString('tasktvinfo', 'database', 'tvinfos.db'));
          if ( fileexists(fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(fileName);
        end;

        (*
        if not IMDbInfoDbAlive then
        begin
          fileName := Trim(config.ReadString('taskimdb', 'database', 'imdb.db'));
          if ( fileexists(fileName) and (skipfiles.IndexOf(fileName) = -1) ) then
            AddFile(fileName);
        end;
        *)

        //split_site_data folder
        if config.ReadBool('sites', 'split_site_data', False) then
        begin
          s := MyIncludeTrailingSlash('rtpl');
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

procedure DeleteOldBackups(s: AnsiString);
var
  sr: TSearchRec;
  files: TStringList;
  ages: TIntList;
  i: integer;
  oldest, oldesti: integer;
begin
  files := TStringList.Create;
  ages := TIntList.Create;
  try
    s := MyIncludeTrailingSlash(s);
    ForceDirectories(s);

    if FindFirst(s + '*.tar', faAnyFile, sr) = 0 then
    begin
      repeat
        if (AnsiEndsStr(sr.Name, '.tar') or (not AnsiContainsStr(sr.Name, '-custom-'))) then
        begin
          Debug(dpMessage, '<- BACKUP ->', 'adding: ' + sr.Name);
          files.Add(sr.Name);
          ages.Add(sr.Time);
        end
        else
          Debug(dpMessage, '<- BACKUP ->', 'skip: ' + sr.Name);
      until FindNext(sr) <> 0;
{$IFDEF MSWINDOWS}
      SysUtils.FindClose(sr);
{$ELSE}
      FindClose(sr);
{$ENDIF}

      while (files.Count > config.ReadInteger(section, 'keep_backups', 30)) do
      begin
        //we search for the oldest
        oldesti := -1;
        oldest := 0;
        for i := 0 to ages.Count - 1 do
        begin
          if ((oldest = 0) or (ages[i] < oldest)) then
          begin
            oldesti := i;
            oldest := ages[i];
          end;
        end;

        if oldesti < 0 then
          Break; // wtf?

{$IFDEF MSWINDOWS}
        DeleteFile(PAnsiChar(s + files[oldesti]));
{$ELSE}
        DeleteFile(s + files[oldesti]);
{$ENDIF}

        ages.Delete(oldesti);
        files.BeginUpdate;
        try
          files.Delete(oldesti);
        finally
          files.EndUpdate;
        end;
      end;
    end
    else
      Debug(dpMessage, '<- BACKUP ->', ' !!! no files added !!! ');

  finally
    ages.Free;
    files.Free;
  end;
end;

procedure BackupBackup;
var
  s: AnsiString;
begin
  try
    debug(dpMessage, section, 'Backup process started.');

    s := config.ReadString(section, 'backup_dir', 'backup');
    if not DirectoryExists(s) then
      Mkdir(s);
    DeleteOldBackups(s);

    if createBackup(False) then
      backup_last_backup := Now
    else
      debug(dpMessage, section, 'Backup process Failed!');

  except on E: Exception do
    Debug(dpError, section, Format('[EXCEPTION] IrcSpread.AddSitesForSpread: %s', [e.Message]));
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

