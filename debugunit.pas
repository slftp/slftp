unit debugunit;

interface

type
  TDebugPriority = (dpError, dpMessage, dpSpam, dpNone);

procedure Debug(priority: TDebugPriority; section, msg: AnsiString); overload;
procedure Debug(priority: TDebugPriority; const section, FormatStr: AnsiString;
  const Args: array of const); overload;
procedure DebugInit;
procedure DebugUnInit;

procedure HandleDebugFile;


function Debug_logfilename: AnsiString;
function Debug_flushlines: integer;
function Debug_categorystr: AnsiString;
function Debug_verbosity: TDebugPriority;
function Debug_MaxFileSize: integer;

function Debug_EncryptOldFiles: boolean;
function Debug_CompressOldFiles: boolean;

function Hide_plain_text: boolean;


var
  debug_verbose: boolean;

implementation

uses configunit, SysUtils
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

procedure Debug(priority: TDebugPriority; section, msg: AnsiString); overload;
var
  nowstr: AnsiString;
begin
  //HandleDebugFile;
  if debug_verbosity = TDebugPriority(3) then
    exit;

  if (priority <> dpError) and (Debug_verbosity < priority) and
    (Debug_categorystr <> ',all,') and
    (0 = Pos(',' + section + ',', Debug_categorystr)) then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  try
    debug_lock.Enter;
    try
      WriteLn(f, Format('%s (%s) [%-12s] %s',
        [nowstr, IntToHex(MyGetCurrentProcessId(), 2), section, msg]));
      Inc(Lines);
      if Lines >= Debug_flushlines then
      begin
        Flush(f);
        Lines := 0;
      end;
    finally
      debug_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      //irc_Adderror(Format('<c4>[EXCEPTION]</c> Debug: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure Debug(priority: TDebugPriority; const section, FormatStr: AnsiString;
  const Args: array of const); overload;
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


procedure DebugInit;
//var logfilename: string;
begin
  debug_lock    := TCriticalSection.Create();
  debug_verbose := debug_verbosity = dpSpam;

  Assignfile(f, Debug_logfilename);
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

procedure DebugUninit;
begin
  Closefile(f);
  debug_lock.Free;
end;


function ArchivOldBackup: boolean;
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
  if CheckLogFileSize = 1 then
  begin
    irc_addtext('CONSOLE', 'ADMIN', 'Backup old logfile...');
    if ArchivOldBackup then
    begin
      irc_addtext('CONSOLE', 'ADMIN', 'Archiv created!');
      Closefile(f);
      ArchivOldBackup;
      irc_addtext('CONSOLE', 'ADMIN', 'Delete old file');
      DeleteFile('slftp.log');
      irc_addtext('CONSOLE', 'ADMIN', 'Create a new one...');
      Rewrite(f);
      irc_addtext('CONSOLE', 'ADMIN', 'Ok!');
    end;
  end;
  debug_lock.Leave;
end;



function Hide_plain_text: boolean;
begin
  Result := config.ReadBool(section, 'hide_plain_text', True);
end;


function Debug_logfilename: AnsiString;
begin
  Result := config.ReadString(section, 'debugfile',
    ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function Debug_flushlines: integer;
begin
  Result := config.ReadInteger(section, 'flushlines', 16);
end;

function Debug_categorystr: AnsiString;
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

(*

function Debug_



max_file_size=
encrypt_old_files=
compress_old_files=

*)

end.

