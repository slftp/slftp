unit backupunit;

interface

procedure BackupBackup;

function ircBackup(out backupError:string):boolean;

var
  backup_last_backup: TDateTime;

  {$I common.inc}

implementation

uses Classes, SysUtils, configunit, debugunit, LibTar, mystrings, uintlist,
  statsunit, indexer, dbtvinfo, slvision, StrUtils
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

//PathDelim
const
  section = 'backup';

  var _backuperror:string;


function createBackup(custom:boolean = False):boolean;
var bName:string; i:integer;
begin
  _backuperror:='';
  result:=False;
  bName := config.ReadString(section, 'backup_dir', 'backup');
  if not DirectoryExists(bName) then
    Mkdir(bName);
  debug(dpMessage, section, 'Backup process started.');
  bName := MyIncludeTrailingSlash(bName);
  ForceDirectories(bName);
  try
  if custom then bName := bName + 'slftp-custom-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar'
  else bName := bName + 'slftp-backup-' + FormatDateTime('yyyy-mm-dd-hhnnss', Now) + '.tar';
    with TTarWriter.Create(bName) do
    begin

    //adding common files
     for I := 0 to cFilecount do
       if fileexists(commonFiles[i]) then AddFile(commonFiles[i]);
    //adding ini files
     for I := 0 to iFilecount do
       if fileexists(iniFiles[i]) then AddFile(iniFiles[i]);


    //adding generated files
     for I := 0 to gFilecount do
       if fileexists(generatedFiles[i]) then AddFile(generatedFiles[i]);

    //adding databases
      if not IndexerAlive then
      begin
        if fileexists(config.ReadString('indexer', 'database', 'nonexist')) then
          AddFile(config.ReadString('indexer', 'database', 'nonexist'));
      end;

      if not StatsAlive then
      begin
        if fileexists(config.ReadString('stats', 'database', 'nonexist')) then
          AddFile(config.ReadString('stats', 'database', 'nonexist'));
      end;

      if not TVInfoDbAlive then
      begin
      if fileexists(config.ReadString('tasktvinfo', 'database', 'nonexist')) then
          AddFile(config.ReadString('tasktvinfo', 'database', 'nonexist'));
      end;
(*
      if not IMDbInfoDbAlive then
      begin
      if fileexists(config.ReadString('taskimdb', 'database', 'nonexist')) then
          AddFile(config.ReadString('taskimdb', 'database', 'nonexist'));
      end;
*)

       Free;
    end;
  debug(dpMessage, section, 'Backup process finished.');
  result:=True;
  except on E: Exception do begin
  debug(dpError, section, '[EXCEPTION] backup failed: ' + e.Message);
  _backuperror:=e.Message;
  end;
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
  s := MyIncludeTrailingSlash(s);
  ForceDirectories(s);
  try
    if FindFirst(s + '*.tar', faAnyFile, sr) = 0 then
    begin
      repeat
        if (AnsiEndsStr(sr.Name,'.tar') or (not AnsiContainsStr(sr.Name,'-custom-'))) then
//        if ((Pos('.tar', sr.Name) = length(sr.Name) - 3) and (Pos('custom',sr.Name) <> 7)) then
        begin
        Debug(dpMessage,'<- BACKUP ->','adding: '+sr.Name);
          files.Add(sr.Name);
          ages.Add(sr.Time);
        end else         Debug(dpMessage,'<- BACKUP ->','skip: '+sr.Name);
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
    end else Debug(dpMessage,'<- BACKUP ->',' !!! no files added !!! ');

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
  s := config.ReadString(section, 'backup_dir', 'backup');
  if not DirectoryExists(s) then
    Mkdir(s);
    debug(dpMessage, section, 'Backup process started.');
    DeleteOldBackups(s);
    if createBackup(False) then
    backup_last_backup := Now
    else debug(dpMessage, section, 'Backup process Failed!');
except on E: Exception do
  Debug(dpError, section,Format('[EXCEPTION] IrcSpread.AddSitesForSpread: %s',[e.Message]));
end;
end;

function ircBackup(out backupError:string):boolean;
var s:ansistring;
begin
Result := True;
s := config.ReadString(section, 'backup_dir', 'backup');
if createBackup(true) then
  backup_last_backup:= Now
else begin
  backupError:=_backuperror;
  result:=False;
end;
end;

initialization
  backup_last_backup := Now;
end.

