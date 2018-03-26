unit debugunit;

interface

type
  TDebugPriority = (dpError, dpMessage, dpSpam, dpNone);

procedure Debug(priority: TDebugPriority; section, msg: String); overload;
procedure Debug(priority: TDebugPriority; const section, FormatStr: String;
  const Args: array of const); overload;

procedure DebugInit;
procedure DebugUnInit;

procedure OpenLogFile;
procedure CloseLogFile;

procedure HandleDebugFile;

function Debug_logfilename: String;
function Debug_flushlines: integer;
function Debug_categorystr: String;
function Debug_verbosity: TDebugPriority;
function Debug_MaxFileSize: integer;
function Debug_EncryptOldFiles: boolean;
function Debug_CompressOldFiles: boolean;

function LogTail(lines: Integer): String;
function FileTail(lines: Integer; filename: String): String;

function Hide_plain_text: boolean;

var
  debug_verbose: boolean;

implementation

uses configunit, SysUtils, Classes
{$IFDEF MSWINDOWS}
  , Windows
{$ELSE}
{$IFDEF FPC}
  , pthreads
{$ELSE}
  , Libc
{$ENDIF}
{$ENDIF}
  , SyncObjs, LibTar, DateUtils, irc, mystrings;

const
  section = 'debug';

var
  f:     TextFile;
  //    flushlines: Integer;
  Lines: integer = 0;
  //    verbosity: TDebugPriority = dpError;
  //    categorystr: string;
  debug_lock: TCriticalSection;


function MyGetCurrentProcessId(): longword;
begin
{$IFDEF MSWINDOWS}
  Result := GetCurrentThreadId;
{$ELSE}
  Result:= LongWord(pthread_self());
{$ENDIF}
end;

procedure Debug(priority: TDebugPriority; section, msg: String); overload;
var
  nowstr: String;
begin
  //HandleDebugFile;
  if debug_verbosity = TDebugPriority(3) then
    exit;

  if (priority <> dpError) and (Debug_verbosity < priority) and
    (Debug_categorystr <> ',all,') and
    (0 = Pos(',' + section + ',', Debug_categorystr)) then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  debug_lock.Enter;
  try
    try
      WriteLn(f, Format('%s (%s) [%-12s] %s',
        [nowstr, IntToHex(MyGetCurrentProcessId(), 2), section, msg]));
      Inc(Lines);
      if Lines >= Debug_flushlines then
      begin
        Flush(f);
        Lines := 0;
      end;
    except
      on e: Exception do
      begin
        //irc_Adderror(Format('<c4>[EXCEPTION]</c> Debug: %s', [e.Message]));
        exit;
      end;
    end;
  finally
    debug_lock.Leave;
  end;
end;

procedure Debug(priority: TDebugPriority; const section, FormatStr: String; const Args: array of const); overload;
begin
  try
    Debug(priority, section, Format(FormatStr, Args));
  except
    on e: Exception do
    begin
      irc_Adderror(Format('<c4>[EXCEPTION]</c> Debug: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure OpenLogFile;
begin
  Assignfile(f, Debug_logfilename);
  FileMode := fmShareDenyNone;
  try
    if FileExists(Debug_logfilename) then
      Append(f)
    else
      Rewrite(f);
  except
    begin
      Writeln('Couldnt open logfile! It might be too huge?');
      halt;
    end;
  end;
end;

procedure CloseLogFile;
begin
  Closefile(f);
end;

procedure DebugInit;
begin
  debug_lock := TCriticalSection.Create();
  debug_verbose := debug_verbosity = dpSpam;
  OpenLogFile;
end;

procedure DebugUninit;
begin
  CloseLogFile;
  debug_lock.Free;
end;

function ArchiveOldBackup: boolean;
var
  tar: TTarWriter;
begin
  tar := TTarWriter.Create('slftp_' + FormatDateTime('yyyymmddhhnnss', Now) + '_log.tar');
  try
    tar.AddFile('slftp.log', 'slftp.log');
  finally
    tar.Free;
  end;
  Result := True;
end;

// -10 = nothing done, -5 = file not found, 1= biggern as, 0= smaller as
function CheckLogFileSize: integer;
var
  trec: TSearchRec;
begin
  //result:=-10;
  if FindFirst('slftp.log', faAnyFile - fadirectory, trec) = 0 then
  begin
    if trec.Size > Debug_MaxFileSize then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := -5;
  SysUtils.FindClose(trec);
end;

procedure HandleDebugFile;
begin

  debug_lock.Enter;
  try
    if CheckLogFileSize = 1 then
    begin
      irc_addtext('CONSOLE', 'ADMIN', 'Backing up current logfile...');
      CloseLogFile;
      if ArchiveOldBackup then
      begin
        irc_addtext('CONSOLE', 'ADMIN', 'Logfile backed up successfully!');
        ArchiveOldBackup;
        irc_addtext('CONSOLE', 'ADMIN', 'Removing current logfile');
        DeleteFile('slftp.log');
        irc_addtext('CONSOLE', 'ADMIN', 'Creating a new fresh logfile...');
        OpenLogFile;
        irc_addtext('CONSOLE', 'ADMIN', 'Ok!');
      end;
    end;
  finally
    debug_lock.Leave;
  end;
end;

function LogTail(lines: Integer): String;
begin
  Result := '';

  debug_lock.Enter;
  CloseLogFile;
  try
    Result := FileTail(lines, Debug_logfilename);
  finally
    debug_lock.Leave;
    OpenLogFile;
  end;
end;

function FileTail(lines: Integer; filename: String): String;
var
  s: TStream;
  c: char;
  l: integer;
begin
  s := TFileStream.Create(filename, fmOpenRead, fmShareDenyNone);
  try
    s.Seek(0, soEnd);
    l := 0;
    while (l <= lines) and (s.Position > 0) do
    begin
      s.Seek(-2, soCurrent);
      s.Read(c, SizeOf(byte));
      if c = #10 then Inc(l);
    end;
    s.Seek(1, soCurrent);
    l := s.Size - s.Position;
    SetLength(Result, l);
    s.Read(Result[1], l);
  finally
    s.Free;
  end;
end;

function Hide_plain_text: boolean;
begin
  Result := config.ReadBool(section, 'hide_plain_text', True);
end;

function Debug_logfilename: String;
begin
  Result := config.ReadString(section, 'debugfile',
    ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function Debug_flushlines: integer;
begin
  Result := config.ReadInteger(section, 'flushlines', 16);
end;

function Debug_categorystr: String;
begin
  Result := ',' + LowerCase(config.ReadString(section, 'categories', 'verbose')) + ',';
end;

function Debug_verbosity: TDebugPriority;
begin
  Result := TDebugPriority(config.ReadInteger(section, 'verbosity', 0));
end;

function Debug_MaxFileSize: integer;
begin
  Result := config.ReadInteger(section, 'max_file_size', 0);
end;

function Debug_EncryptOldFiles: boolean;
begin
  Result := config.ReadBool(section, 'encrypt_old_files', False);
end;

function Debug_CompressOldFiles: boolean;
begin
  Result := config.ReadBool(section, 'compress_old_files', True);
end;

end.

