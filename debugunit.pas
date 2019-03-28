unit debugunit;

interface

type
  {
  @value(dpError logs only important issues/errors = 0)
  @value(dpMessage logs errors, important and general stuff = 1)
  @value(dpSpam logs everything = 2)
  @value(dpNone logs nothing = 3)
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
{ Reads up to @link(aMaxLinesToRead) lines from logfile
  @param(aMaxLinesToRead number of lines to read from logfile)
  @returns(logfile lines) }
function LogTail(const aMaxLinesToRead: Integer): String;

implementation

uses
  SysUtils, Classes, StrUtils, SyncObjs, DateUtils, configunit, irc, IdGlobal;

const
  section = 'debug';

var
  f: TextFile;
  debug_lock: TCriticalSection;

function _GetDebugLogFileName: String;
begin
  Result := config.ReadString(section, 'debugfile', ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function _GetDebugVerbosity: TDebugPriority; inline;
begin
  Result := TDebugPriority(config.ReadInteger(section, 'verbosity', 0));
end;

function _GetDebugCategories: String; inline;
begin
  Result := ',' + LowerCase(config.ReadString(section, 'categories', 'verbose')) + ',';
end;

procedure _OpenLogFile;
begin
  Assignfile(f, _GetDebugLogFileName);
  try
    if FileExists(_GetDebugLogFileName) then
      Append(f)
    else
      Rewrite(f);
  except
    begin
      Writeln('Could not open logfile! It might be too huge?');
      halt;
    end;
  end;
end;

procedure _CloseLogFile;
begin
  Closefile(f);
end;

function _FileTail(const aMaxLinesToRead: Integer; const aFilename: String): String;
var
  fStream: TFileStream;
  fLinesRead, fBytesToEnd: Integer;
  c: Byte;
  fResultBytes: TBytes;
begin
  Result := '';

  fStream := TFileStream.Create(aFilename, fmOpenRead, fmShareDenyNone);
  try
    fLinesRead := 0;
    // go to end of file
    fStream.Seek(0, soFromEnd);
    while ((fLinesRead < aMaxLinesToRead) and (fStream.Seek(-2, soFromCurrent) >= 0)) do
    begin
      fStream.Read(c, SizeOf(c));
      // check if line feed #10 -> new line detected
      if c = 10 then
      begin
        Inc(fLinesRead);
      end;
    end;

    // file has less lines than we want to read
    if fLinesRead < aMaxLinesToRead then
    begin
      fStream.Position := 0;
    end;

    // filesize - current position = begin of x last line
    fBytesToEnd := fStream.Size - fStream.Position;
    SetLength(fResultBytes, fBytesToEnd);

    fStream.ReadBuffer(fResultBytes, fBytesToEnd);
  finally
    fStream.Free;
  end;

  // convert bytearray to string
  Result := TEncoding.UTF8.GetString(fResultBytes);
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

  if (_GetDebugVerbosity < priority) then
    exit;

  if (_GetDebugCategories <> ',verbose,') and (not {$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(_GetDebugCategories, section)) then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  debug_lock.Enter;
  try
    try
      WriteLn(f, Format('%s (%s) [%-25s] %s', [nowstr, IntToHex(IdGlobal.CurrentThreadId, 4), section, msg]));
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

function LogTail(const aMaxLinesToRead: Integer): String;
begin
  Result := '';

  debug_lock.Enter;
  try
    _CloseLogFile;
    try
      Result := _FileTail(aMaxLinesToRead, _GetDebugLogFileName);
    finally
      _OpenLogFile;
    end;
  finally
    debug_lock.Leave;
  end;
end;

end.
