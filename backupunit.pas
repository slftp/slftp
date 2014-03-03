unit backupunit;

interface

procedure BackupBackup;

function CustomBackup(var error: string): boolean;

type
  TSLBackup = class
  private
    fCustom: boolean;
  public
    constructor Create(custombackup: boolean = False);
    function Filename: string;
    function FilePath: string;
    function Backup: boolean;
  end;

var
  backup_last_backup: TDateTime;

implementation

uses Classes, SysUtils, configunit, debugunit, LibTar, mystrings, uintlist,
  statsunit, indexer
  {$IFDEF MSWINDOWS},Windows{$ENDIF}
  ;

//PathDelim
const
  section = 'backup';

function MakeBackup: boolean;
(*
[backup]
# all listed files will NOT added to the backup. 
skipfiles=sqlite3.dll,ssleay32.dll,libmysql.dll,libeay32.dll
*)
var
  path, fname: string;
  tar: TTarWriter;
  Res: TSearchRec;
  EOFound: boolean;
  skipfiles: TStringList;
  I: integer;
begin
  skipfiles := TStringList.Create;
  skipfiles.CommaText := config.ReadString('backup', 'skipfiles', '');
  fname     := Format('slftp-backup-%s.tar', [FormatDateTime('yyyymmddhhnnss', Now)]);
  path      := config.ReadString(section, 'backup_dir', '');
  if not DirectoryExists(path) then
    Mkdir(path);
  path := MyIncludeTrailingSlash(path);
  ForceDirectories(path);
  tar     := TTarWriter.Create(path + fname);
  EOFound := False;
  try
    if FindFirst(Path + '*.*', faanyfile - fadirectory, Res) < 0 then begin
    result:=false;
      exit
end  else
      while not EOFound do
      begin
        if skipfiles.Text <> '' then
        begin
          for I := 0 to skipfiles.Count - 1 do
            if lowercase(Res.Name) <> lowercase(skipfiles.Strings[i]) then
              tar.AddFile(Res.Name, Res.Name);
        end
        else
          tar.AddFile(Res.Name, Res.Name);
        EOFound := FindNext(Res) <> 0;
      end;//while not EOFound do begin
  finally
    Result := True;
  end;

  {$IFDEF MSWINDOWS}
  SysUtils.FindClose(Res);
  {$ELSE}
  FindClose(Res);  
  {$ENDIF}
  tar.Free;
  skipfiles.Free;
end;

procedure CreateBackup(s: string);
var
  vbname: string;
begin
  vbname := s + 'slftp-backup-' + FormatDateTime('yyyymmddhhnnss', Now) + '.tar';
  try
    with TTarWriter.Create(vbname) do
    begin
      if not IndexerAlaive then
      begin
        if fileexists(config.ReadString('indexer', 'database', 'nonexist')) then
          AddFile(config.ReadString('indexer', 'database', 'nonexist'));
      end;

      if not StatsAlaive then
      begin
        if fileexists(config.ReadString('stats', 'database', 'nonexist')) then
          AddFile(config.ReadString('stats', 'database', 'nonexist'));
      end;


      if fileexists('mirktrade.conf') then
        AddFile('mirktrade.conf');
      if fileexists('sites.dat') then
        AddFile('sites.dat');
      if fileexists('slftp.chans') then
        AddFile('slftp.chans');
      if fileexists('slftp.history') then
        AddFile('slftp.history');
      if fileexists('slftp.ini') then
        AddFile('slftp.ini');
      if fileexists('slftp.cini') then
        AddFile('slftp.cini');
      if fileexists('slftp.kb') then
        AddFile('slftp.kb');
      if fileexists('slftp.news') then
        AddFile('slftp.news');
      if fileexists('slftp.nukequeue') then
        AddFile('slftp.nukequeue');
      if fileexists('slftp.pem') then
        AddFile('slftp.pem');
      if fileexists('slftp.precatcher') then
        AddFile('slftp.precatcher');
      if fileexists('slftp.ranks') then
        AddFile('slftp.ranks');
      if fileexists('slftp.renames') then
        AddFile('slftp.renames');
      if fileexists('slftp.rules') then
        AddFile('slftp.rules');
      if fileexists('slftp.skip') then
        AddFile('slftp.skip');
      if fileexists('slftp.speedstats') then
        AddFile('slftp.speedstats');
      if fileexists('slftp.languages') then
        AddFile('slftp.languages');
      if fileexists('slftp.knowngroups') then
        AddFile('slftp.knowngroups');
      //<!-- mr.dOH files
      if fileexists('slftp.socks5') then
        AddFile('slftp.socks5');
      if fileexists('languagebase.slftp') then
        AddFile('languagebase.slftp');
      if fileexists('slftp.imdbcountries') then
        AddFile('slftp.imdbcountries');
      if fileexists('slftp.spamconf') then
        AddFile('slftp.spamconf');
      if fileexists('slftp.skipgroups') then
        AddFile('slftp.skipgroups');
      if fileexists('slftp.preurls') then
        AddFile('slftp.preurls');
      //----------------------------------------------------------------------------->
      Free;
    end;
  except
    on e: Exception do
      debug(dpError, section, 'backup failed: ' + e.Message);
  end;
end;

procedure DeleteOldBackups(s: string);
var
  sr:    TSearchRec;
  files: TStringList;
  ages:  TIntList;
  i:     integer;
  oldest, oldesti: integer;
begin
  files := TStringList.Create;
  ages  := TIntList.Create;
  if FindFirst(s + '*.tar', faAnyFile, sr) = 0 then
  begin
    repeat
      if Pos('.tar', sr.Name) = length(sr.Name) - 3 then
      begin
        files.Add(sr.Name);
        ages.Add(sr.Time);
      end;
    until FindNext(sr) <> 0;
  {$IFDEF MSWINDOWS}
  SysUtils.FindClose(sr);
  {$ELSE}
  FindClose(sr);
  {$ENDIF}

    while (files.Count > config.ReadInteger(section, 'keep_backups', 30)) do
    begin
      // megkeressuk a legregebbit
      oldesti := -1;
      oldest  := 0;
      for i := 0 to ages.Count - 1 do
      begin
        if ((oldest = 0) or (ages[i] < oldest)) then
        begin
          oldesti := i;
          oldest  := ages[i];
        end;
      end;

      if oldesti < 0 then
        Break; // wtf?

  {$IFDEF MSWINDOWS}
DeleteFile(PAnsiChar(s + files[oldesti]));
  {$ELSE}
DeleteFile(s + files[oldesti]);
  {$ENDIF}


      files.Delete(oldesti);
      ages.Delete(oldesti);
    end;
  end;
  ages.Free;
  files.Free;
end;

procedure BackupBackup;
var
  s: string;
begin
  s := config.ReadString(section, 'backup_dir', '');
  if s <> '' then
  begin
    if not DirectoryExists(s) then
      Mkdir(s);
    debug(dpMessage, section, 'Backup process started.');
    s := MyIncludeTrailingSlash(s);
    ForceDirectories(s);

    CreateBackup(s);
    //if not  MakeBackup then debug(dpMessage, section, 'Backup process Failed!');
    DeleteOldBackups(s);

    debug(dpMessage, section, 'Backup process finished.');
  end;

  backup_last_backup := Now;
end;


function CustomBackup(var error: string): boolean;
var
  cb: TTarWriter;
  s:  string;
begin
  //indexerUninit;
  //statsUninit;
  Result := False;
  s      := 'custom_' + config.ReadString(section, 'backup_dir', '') + 's';
  if not DirectoryExists(s) then
    Mkdir(s);
  s := MyIncludeTrailingSlash(s);
  ForceDirectories(s);
  cb := TTarWriter.Create(s + 'slbackup-' + FormatDateTime('mmdd_hhnnss', Now) + '.tar');
  try
(*
if fileexists(config.ReadString('indexer', 'database', 'nonexist')) then
     cb.AddFile(config.ReadString('indexer', 'database', 'nonexist'));
      if fileexists(config.ReadString('stats', 'database', 'nonexist')) then
      cb.AddFile(config.ReadString('stats', 'database', 'nonexist'));
      *)
    if fileexists('mirktrade.conf') then
      cb.AddFile('mirktrade.conf');
    if fileexists('sites.dat') then
      cb.AddFile('sites.dat');
    if fileexists('slftp.chans') then
      cb.AddFile('slftp.chans');
    if fileexists('slftp.history') then
      cb.AddFile('slftp.history');
    if fileexists('slftp.ini') then
      cb.AddFile('slftp.ini');
    if fileexists('slftp.cini') then
      cb.AddFile('slftp.cini');
    if fileexists('slftp.kb') then
      cb.AddFile('slftp.kb');
    if fileexists('slftp.news') then
      cb.AddFile('slftp.news');
    if fileexists('slftp.nukequeue') then
      cb.AddFile('slftp.nukequeue');
    if fileexists('slftp.pem') then
      cb.AddFile('slftp.pem');
    if fileexists('slftp.precatcher') then
      cb.AddFile('slftp.precatcher');
    if fileexists('slftp.ranks') then
      cb.AddFile('slftp.ranks');
    if fileexists('slftp.renames') then
      cb.AddFile('slftp.renames');
    if fileexists('slftp.rules') then
      cb.AddFile('slftp.rules');
    if fileexists('slftp.skip') then
      cb.AddFile('slftp.skip');
    if fileexists('slftp.speedstats') then
      cb.AddFile('slftp.speedstats');
    if fileexists('slftp.languages') then
      cb.AddFile('slftp.languages');
    if fileexists('slftp.knowngroups') then
      cb.AddFile('slftp.knowngroups');
    if fileexists('slftp.socks5') then
      cb.AddFile('slftp.socks5');
    if fileexists('imdbcountrys.nwo') then
      cb.AddFile('imdbcountrys.nwo');
    if fileexists('languagebase.slftp') then
      cb.AddFile('languagebase.slftp');
    if fileexists('slftp.preurls') then
      cb.AddFile('slftp.preurls');
    cb.Free;
    Result := True;
    //statsStart;
    //indexerStart;
  except
    on e: Exception do
      error := e.Message;
  end;
  if error = '' then
    Result := True;
end;


{TSLBackup}

constructor TSLBackup.Create(custombackup: boolean = False);
begin
  fCustom := custombackup;
end;

function TSLBackup.Filename: string;
begin
  //result:=Format('slftp-backup-%d',[Datetimetounix(now)]);
  Result := Format('slftp-backup-%s', [FormatDateTime('yyyymmddhhnnss', Now)]);
end;

function TSLBackup.FilePath: string;
var
  s: string;
begin
  s := config.ReadString(section, 'backup_dir', '');
  s := MyIncludeTrailingSlash(s);
  ForceDirectories(s);
  Result := s;
end;

function TSLBackup.Backup: boolean;
var
  slb: TTarWriter;
  cf:  string;
  x:   TStringList;
begin
  x      := TStringList.Create;
  slb    := TTarWriter.Create(FilePath + Filename + '.tar');
  cf     := config.ReadString(section, 'files', '');
  if uppercase(cf) <> '!ALL!' then
    x.CommaText := cf;
  try
    try
//
    Result := True;
    except
      on e: Exception do begin
        debug(dpError, section, 'backup failed: ' + e.Message);
        Result := False;
      end;
    end;
  finally
  x.Free;
  slb.Free;
  end;
  backup_last_backup := Now;
end;

initialization
  backup_last_backup := Now;
end.

