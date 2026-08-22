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
{ Writes Debug Level to config file and global variable
  @param(netname for irc output)
  @param(channel name for irc output)
  @param(new debug value) }
function WriteDebugVerbosity(const netname, channel, params: String): boolean;
{ Returns the currently used debug verbosity / priority }
function GetDebugVerbosity: TDebugPriority;

implementation

uses
  SysUtils, Classes, StrUtils, SyncObjs, DateUtils, configunit, irc, IdGlobal, slcriticalsection2;

const
  section = 'debug';
  C_IO_ERROR_CIRCUIT_SECONDS = 60; //< how long Debug output is suppressed after an I/O error (circuit breaker)

var
  f: TextFile;
  debug_lock: TSlCriticalSection2;
  glCachedDebugPriority: TDebugPriority = dpError;
  glCachedDebugCategories: string = ',verbose,';
  glFlushLines: boolean = False;
  glLastIOErrorTime: TDateTime = 0;

function _GetDebugLogFileName: String;
begin
  Result := config.ReadString(section, 'debugfile', ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function GetDebugVerbosity: TDebugPriority;
begin
  Result := glCachedDebugPriority;
end;

function WriteDebugVerbosity(const netname, channel, params: String): boolean;
var
  val: integer;
begin
  Result := False;
  val := StrToIntDef(params, -1);
  if val = -1 then
  begin

    case glCachedDebugPriority of
      dpError: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      dpMessage: irc_Addtext(Netname, Channel, 'Only Logging Errors and common Messages.');
      dpSpam: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      dpNone: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else if (val <= 3) then
  begin
    config.WriteInteger('debug', 'verbosity', val);
    config.UpdateFile;
    glCachedDebugPriority := TDebugPriority(val);
    case glCachedDebugPriority of
      dpError: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      dpMessage: irc_Addtext(Netname, Channel, 'Only Logging Errors and common Messages.');
      dpSpam: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      dpNone: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else
  begin
    irc_Addtext(Netname, Channel, '<c4>Syntax error</c>, unknown verbosity.');
    Result := False;
    Exit;
  end;
end;

function _GetDebugCategories: String; inline;
begin
  Result := glCachedDebugCategories;
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
  fStream: TStream;
  fLinesDone, fBytesToEnd: Integer;
  fCurrentByte: Byte;
  fResultBytes: TBytes;
begin
  Result := '';

  fStream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyNone);
  try
    fStream.Seek(0, soEnd);
    fLinesDone := 0;

    while (fLinesDone < aMaxLinesToRead) and (fStream.Seek(-2, soCurrent) >= 0) do
    begin
      fStream.Read(fCurrentByte, SizeOf(fCurrentByte));
      // line feed #10 -> new line detected
      if fCurrentByte = 10 then
        Inc(fLinesDone);
    end;

    if fLinesDone < aMaxLinesToRead then
      fStream.Position := 0;

    // filesize - current position = begin of x-th last line
    fBytesToEnd := fStream.Size - fStream.Position;
    SetLength(fResultBytes, fBytesToEnd);

    fStream.ReadBuffer(fResultBytes[0], fBytesToEnd);
  finally
    fStream.Free;
  end;

  {$IFDEF UNICODE}
    // convert bytearray to string (2-byte char)
    Result := TEncoding.UTF8.GetString(fResultBytes);
  {$ELSE}
    SetLength(Result, fBytesToEnd);
    move(fResultBytes[0], Result[1], fBytesToEnd);
  {$ENDIF}
end;

procedure DebugInit;
begin
  glCachedDebugPriority := TDebugPriority(config.ReadInteger(section, 'verbosity', 0));
  glCachedDebugCategories := ',' + LowerCase(config.ReadString(section, 'categories', 'verbose')) + ',';
  glFlushLines := config.ReadBool(section, 'flushlines', False);
  _OpenLogFile;
  debug_lock := TSlCriticalSection2.Create('debug_lock');
end;

procedure DebugUninit;
begin
  _CloseLogFile;
  debug_lock.Free;
end;

function IsInIOErrorState: Boolean;
begin
  Result := False;

  if (glLastIOErrorTime > 0) then
  begin
    if (SecondsBetween(Now, glLastIOErrorTime) < C_IO_ERROR_CIRCUIT_SECONDS) then
      Result := True
    else
      glLastIOErrorTime := 0;
  end;
end;

procedure Debug(const priority: TDebugPriority; const section, msg: String); overload;
var
  nowstr, logtext: String;
begin
  if glCachedDebugPriority = dpNone then
    exit;

  if (glCachedDebugPriority < priority) then
    exit;

  if (_GetDebugCategories <> ',verbose,') and (not {$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(_GetDebugCategories, section)) then
    exit;

  // circuit breaker: skip logging if we recently hit an I/O error
  if IsInIOErrorState then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  logtext := Format('%s (%s) [%-25s] %s', [nowstr, 'NA', section, msg]);

  if debug_lock = nil then  // has not been initialized yet
    exit;

  debug_lock.Enter('Debug');
  try
    try
      WriteLn(f, logtext);
      if glFlushLines then
      begin
        Flush(f); // flush TextFile buffer
        {$IFDEF FPC}
        // it seems, we need both Flush and FileFlush on Linux
        FileFlush(TTextRec(f).Handle); // ensure kernel flush to avoid truncated lines
        {$ENDIF}
      end;
    except
      on e: EInOutError do
      begin
        glLastIOErrorTime := Now;
        exit;
      end;
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
      // suppress error reporting while the I/O circuit breaker is open
      if IsInIOErrorState then
        exit;

      irc_Adderror(Format('<c4>[EXCEPTION]</c> Debug: %s', [e.Message]));
      exit;
    end;
  end;
end;

function LogTail(const aMaxLinesToRead: Integer): String;
begin
  Result := '';

  debug_lock.Enter('LogTail');
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
