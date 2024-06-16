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

implementation

uses
  SysUtils, Classes, StrUtils, SyncObjs, DateUtils, configunit, irc, IdGlobal;

const
  section = 'debug';

var
  f: TextFile;
  debug_lock: TCriticalSection;
  glCachedDebugPriority: TDebugPriority;

function _GetDebugLogFileName: String;
begin
  Result := config.ReadString(section, 'debugfile', ExtractFilePath(ParamStr(0)) + 'slftp.log');
end;

function _GetDebugVerbosity: TDebugPriority; inline;
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
  debug_lock := TCriticalSection.Create;
  glCachedDebugPriority := TDebugPriority(config.ReadInteger(section, 'verbosity', 0));
  _OpenLogFile;
end;

procedure DebugUninit;
begin
  _CloseLogFile;
  debug_lock.Free;
end;

procedure Debug(const priority: TDebugPriority; const section, msg: String); overload;
var
  nowstr, logtext: String;
begin
  if _GetDebugVerbosity = dpNone then
    exit;

  if (_GetDebugVerbosity < priority) then
    exit;

  if (_GetDebugCategories <> ',verbose,') and (not {$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(_GetDebugCategories, section)) then
    exit;

  DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
  logtext := Format('%s (%s) [%-25s] %s', [nowstr, 'NA', section, msg]);
  debug_lock.Enter;
  try
    try
      WriteLn(f, logtext);
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
