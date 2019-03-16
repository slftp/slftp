unit debugunit;

interface

type
  {
  @value(dpError logs only important issues/errors)
  @value(dpMessage logs important and general stuff)
  @value(dpSpam logs everything)
  @value(dpNone logs nothing)
  }
  TDebugPriority = (dpError, dpMessage, dpSpam, dpNone);

{ Just a helper function to initialize @link(debug_lock) and calls @link(_OpenLogFile) afterwards }
procedure DebugInit;
{ Just a helper function to call @link(_CloseLogFile) and free @link(debug_lock) afterwards }
procedure DebugUnInit;
{ Writes given data to logfile
  @param(priority priority of message)
  @param(section debug section for message)
  @param(msg output text with infos/errors/etc) }
procedure Debug(const priority: TDebugPriority; const section, msg: String); overload;
{ Writes given data to logfile with support for RTL Format() function
  @param(priority priority of message)
  @param(section debug section for message)
  @param(FormatStr formatting text for RTL Format())
  @param(Args formatting argments for RTL Format()) }
procedure Debug(const priority: TDebugPriority; const section, FormatStr: String; const Args: array of const); overload;
{ Reads up to @link(lines) lines from logfile
  @param(lines number of lines to read from logfile)
  @returns(logfile lines) }
function LogTail(const lines: Integer): String;

implementation

uses
  SysUtils, Classes, SyncObjs, DateUtils, configunit, irc, IdGlobal;

const
  section = 'debug';

var
  f: TextFile;
  Lines: integer = 0;
  debug_lock: TCriticalSection;

function _GetDebugLogFileName: String;
begin
  Result := config.ReadString(section, 'debugfile', ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function _GetDebugFlushLinesCount: integer;
begin
  Result := config.ReadInteger(section, 'flushlines', 16);
end;

function _GetDebugVerbosity: TDebugPriority;
begin
  Result := TDebugPriority(config.ReadInteger(section, 'verbosity', 0));
end;

function _GetDebugMaxFileSize: integer;
begin
  Result := config.ReadInteger(section, 'max_file_size', 0);
end;

function _GetDebugCategories: String;
begin
  Result := ',' + LowerCase(config.ReadString(section, 'categories', 'verbose')) + ',';
end;

procedure _OpenLogFile;
begin
  Assignfile(f, _GetDebugLogFileName);
  FileMode := fmShareDenyNone;
  try
    if FileExists(_GetDebugLogFileName) then
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

procedure _CloseLogFile;
begin
  Closefile(f);
end;

function _FileTail(const lines: Integer; const filename: String): String;
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

procedure DebugInit;
begin
  debug_lock := TCriticalSection.Create;
  _OpenLogFile;
end;

procedure DebugUninit;
begin
  _CloseLogFile;
  debug_lock.Free;
end;

procedure Debug(const priority: TDebugPriority; const section, msg: String); overload;
var
  nowstr: String;
begin
  if _GetDebugVerbosity = dpNone then
    exit;

  if (priority <> dpError) and (_GetDebugVerbosity < priority) and (_GetDebugCategories <> ',all,') and (0 = Pos(',' + section + ',', _GetDebugCategories)) then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  debug_lock.Enter;
  try
    try
      WriteLn(f, Format('%s (%s) [%-12s] %s', [nowstr, IntToHex(IdGlobal.CurrentThreadId, 2), section, msg]));
      Inc(Lines);
      if Lines >= _GetDebugFlushLinesCount then
      begin
        Flush(f);
        Lines := 0;
      end;
    except
      on e: Exception do
      begin
        irc_Adderror(Format('<c4>[EXCEPTION]</c> Debug: %s', [e.Message]));
        exit;
      end;
    end;
  finally
    debug_lock.Leave;
  end;
end;

procedure Debug(const priority: TDebugPriority; const section, FormatStr: String; const Args: array of const); overload;
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

function LogTail(const lines: Integer): String;
begin
  Result := '';

  debug_lock.Enter;
  try
    _CloseLogFile;
    try
      Result := _FileTail(lines, _GetDebugLogFileName);
    finally
      _OpenLogFile;
    end;
  finally
    debug_lock.Leave;
  end;
end;

end.

