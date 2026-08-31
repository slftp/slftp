{ ****************************************************************************

 - Soulless robotic engine aka SLFTP

 - Description: Stall detection and self-contained diagnostic reports.
   A background thread monitors heartbeats of the main thread and of all
   registered worker threads (queue, slot, irc, kb) plus the REST API request
   counters. When a heartbeat stays silent longer than its threshold, a single
   self-contained report file (watchdog.<timestamp>.log) is written next to the
   binary and announced on the error channels, so users can simply send that
   file for analysis. The same report can be generated on demand through the
   'watchdog' IRC command.

 ****************************************************************************

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS       *
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE       *
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE        *
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR      *
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF     *
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,    *
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE     *
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,        *
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       *

*****************************************************************************}

{ @abstract(Stall detection and self-contained diagnostic reports.) }

unit watchdog;

interface

uses
  Classes, SysUtils, Contnrs, Generics.Collections;

type
  { Heartbeat marker for one monitored thread or subsystem.
    Instances are owned by the registering thread; create them with
    @link(WatchdogNewParticipant) and free them with @link(WatchdogReleaseParticipant). }
  TWatchdogParticipant = class
  protected
    FName: String;
    FStallSeconds: integer;
    FLastBeatTick: Int64;
    FInfo: ShortString;
  public
    constructor Create(const aName: String; const aStallSeconds: integer);
    { Records that the owning thread is alive. Safe to call from the owning thread only. }
    procedure Beat;
    { Stores a short state description shown in the report. Uses a ShortString buffer
      so that diagnostic reads from other threads can never corrupt memory. }
    procedure SetInfo(const aInfo: String);
    property Name: String read FName;
    property StallSeconds: integer read FStallSeconds write FStallSeconds;
    property LastBeatTick: Int64 read FLastBeatTick;
    property Info: ShortString read FInfo;
  end;

{ Returns true, if watchdog monitoring is enabled in the config. }
function WatchdogEnabled: boolean;

{ Creates and registers a new heartbeat participant.
  @param(aName unique participant name shown in reports)
  @param(aStallSeconds seconds of silence after which the participant counts as stalled) }
function WatchdogNewParticipant(const aName: String; const aStallSeconds: integer = 120): TWatchdogParticipant;

{ Unregisters and frees a heartbeat participant. }
procedure WatchdogReleaseParticipant(var aParticipant: TWatchdogParticipant); overload;

{ Same as above for thread classes which store the participant as a plain TObject field. }
procedure WatchdogReleaseParticipant(var aParticipant: TObject); overload;

{ Records a heartbeat of the main/UI thread. }
procedure WatchdogMainThreadBeat;

{ Counts a started REST API request (paired with @link(WatchdogApiRequestEnd)). }
procedure WatchdogApiRequestStart(const aUrl: String);

{ Counts a finished REST API request. }
procedure WatchdogApiRequestEnd;

{ Returns true if a new report may be written, honoring the report_cooldown setting.
  Always true if no report was written yet. }
function WatchdogReportDue: boolean;

{ Writes a full diagnostic report to watchdog.<timestamp>.log next to the binary.
  Starts the report cooldown clock regardless of the write result.
  @param(aReason reason why the report was written)
  @returns(the written filename or an empty string on failure) }
function WatchdogGenerateReport(const aReason: String): String;

type
  { Handler which receives watchdog log messages, registered by the application to keep this unit free of dependencies on the debug/irc units. }
  TWatchdogLogProc = procedure(const aMsg: String);
  { Handler which is informed about an automatic report, e.g. to announce it on IRC. }
  TWatchdogNotifyProc = procedure(const aReason, aReportFile: String);
  { Handler which returns the full version string for the report header. }
  TWatchdogVersionFunc = function: String;
  { Handler which appends one application inventory section (queues, sites, irc, kb) to the report. }
  TWatchdogInventoryProc = procedure(const aOutput: TStrings);

{ Registers the application handlers. Without them the watchdog stays functional but logs nothing,
  announces nothing and writes a report header without the version string. }
procedure WatchdogSetHandlers(const aLog: TWatchdogLogProc; const aNotify: TWatchdogNotifyProc; const aVersion: TWatchdogVersionFunc);

{ Registers an application inventory section which is appended to every report.
  Intended to be called from unit initialization sections of the monitored subsystems. }
procedure WatchdogRegisterInventory(const aName: String; aProc: TWatchdogInventoryProc);

procedure WatchdogInit;
procedure WatchdogStart;
procedure WatchdogUninit;

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  DateUtils, Math, StrUtils, configunit, slcriticalsection2;

const
  CReportPrefix = 'watchdog.';
  CReportSuffix = '.log';

type
  TWatchdogThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  glLock: TSlCriticalSection2;
  glParticipants: TObjectList;
  glUninited: boolean;
  glEnabled: boolean;
  glCheckInterval: integer;
  glMainThreadStall: integer;
  glApiStall: integer;
  glReportCooldown: integer;
  glKeepReports: integer;
  glMainThreadBeat: Int64;
  glApiInFlight: integer;
  glApiBusySinceTick: Int64;
  glApiLastEndTick: Int64;
  glApiLastUrl: ShortString;
  glLastReportTick: Int64;
  glStartTick: Int64;
  glWatchdogThread: TWatchdogThread;
  glLogProc: TWatchdogLogProc;
  glNotifyProc: TWatchdogNotifyProc;
  glVersionFunc: TWatchdogVersionFunc;
  glInventoryNames: TStringList;
  glInventoryProcs: TList<TWatchdogInventoryProc>;

procedure WatchdogSetHandlers(const aLog: TWatchdogLogProc; const aNotify: TWatchdogNotifyProc; const aVersion: TWatchdogVersionFunc);
begin
  glLogProc := aLog;
  glNotifyProc := aNotify;
  glVersionFunc := aVersion;
end;

procedure WatchdogRegisterInventory(const aName: String; aProc: TWatchdogInventoryProc);
begin
  if glInventoryNames = nil then
  begin
    glInventoryNames := TStringList.Create;
    glInventoryProcs := TList<TWatchdogInventoryProc>.Create;
  end;

  if glInventoryNames.IndexOf(aName) <> -1 then
    exit;

  glInventoryNames.Add(aName);
  glInventoryProcs.Add(aProc);
end;

procedure _Log(const aMsg: String);
begin
  if Assigned(glLogProc) then
    glLogProc(aMsg);
end;

procedure _EnsureInit;
begin
  // after WatchdogUninit the unit must not allocate state again
  if (glLock <> nil) or glUninited then
    exit;

  glLock := TSlCriticalSection2.Create('Watchdog');
  glParticipants := TObjectList.Create(False);
end;

function WatchdogEnabled: boolean;
begin
  Result := glEnabled;
end;

constructor TWatchdogParticipant.Create(const aName: String; const aStallSeconds: integer);
begin
  inherited Create;
  FName := aName;
  FStallSeconds := aStallSeconds;
  FLastBeatTick := GetTickCount64;
  FInfo := '';
end;

procedure TWatchdogParticipant.Beat;
begin
  FLastBeatTick := GetTickCount64;
end;

procedure TWatchdogParticipant.SetInfo(const aInfo: String);
begin
  FInfo := ShortString(aInfo);
end;

function WatchdogNewParticipant(const aName: String; const aStallSeconds: integer = 120): TWatchdogParticipant;
begin
  Result := TWatchdogParticipant.Create(aName, aStallSeconds);

  _EnsureInit;

  if glLock <> nil then
  begin
    glLock.Enter('WatchdogNewParticipant');
    try
      glParticipants.Add(Result);
    finally
      glLock.Leave;
    end;
  end;
end;

procedure WatchdogReleaseParticipant(var aParticipant: TWatchdogParticipant);
begin
  if aParticipant = nil then
    exit;

  if glLock <> nil then
  begin
    glLock.Enter('WatchdogReleaseParticipant');
    try
      glParticipants.Remove(aParticipant);
    finally
      glLock.Leave;
    end;
  end;

  FreeAndNil(aParticipant);
end;

procedure WatchdogReleaseParticipant(var aParticipant: TObject);
begin
  if aParticipant = nil then
    exit;

  if glLock <> nil then
  begin
    glLock.Enter('WatchdogReleaseParticipant');
    try
      glParticipants.Remove(aParticipant);
    finally
      glLock.Leave;
    end;
  end;

  FreeAndNil(aParticipant);
end;

procedure WatchdogMainThreadBeat;
begin
  glMainThreadBeat := GetTickCount64;
end;

procedure WatchdogApiRequestStart(const aUrl: String);
begin
  _EnsureInit;

  if glLock = nil then
    exit;

  glLock.Enter('WatchdogApiRequestStart');
  try
    Inc(glApiInFlight);
    if glApiInFlight = 1 then
      glApiBusySinceTick := GetTickCount64;
    glApiLastUrl := ShortString(aUrl);
  finally
    glLock.Leave;
  end;
end;

procedure WatchdogApiRequestEnd;
begin
  if glLock = nil then
    exit;

  glLock.Enter('WatchdogApiRequestEnd');
  try
    if glApiInFlight > 0 then
      Dec(glApiInFlight);
    if glApiInFlight = 0 then
    begin
      glApiLastUrl := '';
      glApiLastEndTick := GetTickCount64;
    end;
  finally
    glLock.Leave;
  end;
end;

function _TickAgeSeconds(const aTick: Int64): Double;
begin
  if aTick <= 0 then
    Result := -1
  else
    Result := (GetTickCount64 - aTick) / 1000.0;
end;

function WatchdogReportDue: boolean;
begin
  // glLastReportTick = 0 is the "no report written yet" sentinel
  if (glReportCooldown = 0) or (glLastReportTick = 0) then
    Result := True
  else
    Result := _TickAgeSeconds(glLastReportTick) >= glReportCooldown;
end;

procedure _MarkReport;
begin
  if glLock <> nil then
  begin
    glLock.Enter('WatchdogMarkReport');
    try
      glLastReportTick := GetTickCount64;
    finally
      glLock.Leave;
    end;
  end
  else
    glLastReportTick := GetTickCount64;
end;

procedure _AppendHeader(const aOutput: TStringList; const aReason: String);
var
  fUptimeSeconds: Int64;
begin
  aOutput.Add(StringOfChar('=', 100));
  aOutput.Add('slftp watchdog report');
  aOutput.Add(StringOfChar('=', 100));
  aOutput.Add('time: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now()));
  if Assigned(glVersionFunc) then
    aOutput.Add('version: ' + glVersionFunc());
  fUptimeSeconds := (GetTickCount64 - glStartTick) div 1000;
  aOutput.Add(Format('uptime: %dd %dh %dm %ds', [fUptimeSeconds div 86400,
    (fUptimeSeconds mod 86400) div 3600, (fUptimeSeconds mod 3600) div 60, fUptimeSeconds mod 60]));
  aOutput.Add('trigger: ' + aReason);
  aOutput.Add(StringOfChar('-', 100));
end;

procedure _AppendHeartbeats(const aOutput: TStringList);
var
  fObject: TObject;
  fParticipant: TWatchdogParticipant;
  fAge: Double;
  fStalled: boolean;
begin
  aOutput.Add('[heartbeats]');

  fAge := _TickAgeSeconds(glMainThreadBeat);
  fStalled := (fAge > glMainThreadStall);
  aOutput.Add(Format('  %-30s last beat %.1fs ago (threshold %ds)%s',
    ['mainthread', fAge, glMainThreadStall, IfThen(fStalled, ' [STALL]', '')]));

  if glLock <> nil then
  begin
    glLock.Enter('WatchdogReport heartbeats');
    try
      for fObject in glParticipants do
      begin
        try
          fParticipant := TWatchdogParticipant(fObject);
          fAge := _TickAgeSeconds(fParticipant.LastBeatTick);
          fStalled := (fAge > fParticipant.StallSeconds);
          aOutput.Add(Format('  %-30s last beat %.1fs ago (threshold %ds)%s info: %s',
            [fParticipant.Name, fAge, fParticipant.StallSeconds,
             IfThen(fStalled, ' [STALL]', ''), string(fParticipant.Info)]));
        except
          on e: Exception do
            aOutput.Add('  <error reading participant: ' + e.Message + '>');
        end;
      end;
    finally
      glLock.Leave;
    end;

    glLock.Enter('WatchdogReport api');
    try
      if glApiInFlight > 0 then
        aOutput.Add(Format('  %-30s in-flight %d, continuously busy for %.1fs, last request: %s',
          ['api', glApiInFlight, _TickAgeSeconds(glApiBusySinceTick), string(glApiLastUrl)]))
      else if glApiLastEndTick = 0 then
        aOutput.Add(Format('  %-30s idle (no requests yet)', ['api']))
      else
        aOutput.Add(Format('  %-30s idle (last request %.1fs ago)',
          ['api', _TickAgeSeconds(glApiLastEndTick)]));
    finally
      glLock.Leave;
    end;
  end;

  aOutput.Add('');
end;

procedure _AppendLocks(const aOutput: TStringList);
begin
  aOutput.Add('[locks]');
  aOutput.Add('  note: mode=plain has no timeout, a stuck owner blocks all other users of that lock.');
  aOutput.Add('  note: owner fields are a hint (thread which entered last); lastenter >> lastleave means it never left.');
  try
    WriteCriticalSection2States(aOutput);
  except
    on e: Exception do
      aOutput.Add('  <error reading lock states: ' + e.Message + '>');
  end;
  aOutput.Add('');
end;

{$IFDEF UNIX}
function _ReadFirstLine(const aFilename: String): String;
var
  fFile: TextFile;
begin
  Result := '';

  AssignFile(fFile, aFilename);
  {$I-}
  Reset(fFile);
  {$I+}
  if IOResult <> 0 then
    exit;

  try
    ReadLn(fFile, Result);
  finally
    CloseFile(fFile);
  end;
end;

procedure _AppendOsThreads(const aOutput: TStringList);
var
  fSearchRec: TSearchRec;
  fTaskPath, fStatLine, fComm, fState, fWchan: String;
  fAfterComm: String;
  fFields: TStringList;
  fStack: TextFile;
  fLine: String;
  fStackCount: integer;
begin
  aOutput.Add('[threads: os]');

  fFields := TStringList.Create;
  try
    if FindFirst('/proc/self/task' + PathDelim + '*', faAnyFile, fSearchRec) = 0 then
    try
      repeat
        if (fSearchRec.Name = '.') or (fSearchRec.Name = '..') then
          Continue;

        fTaskPath := '/proc/self/task/' + fSearchRec.Name;
        fComm := _ReadFirstLine(fTaskPath + '/comm');
        fWchan := _ReadFirstLine(fTaskPath + '/wchan');
        fStatLine := _ReadFirstLine(fTaskPath + '/stat');

        // stat format: pid (comm) state rest - skip past the closing bracket of comm
        fState := '?';
        if RPos(')', fStatLine) > 0 then
        begin
          fAfterComm := Copy(fStatLine, RPos(')', fStatLine) + 2, MaxInt);
          fFields.Clear;
          fFields.StrictDelimiter := True;
          fFields.Delimiter := ' ';
          fFields.DelimitedText := fAfterComm;
          if fFields.Count > 0 then
            fState := fFields[0];
        end;

        aOutput.Add(Format('  tid %-7s %-16s state=%s wchan=%s',
          [fSearchRec.Name, fComm, fState, fWchan]));

        // kernel stack of the thread; needs sufficient privileges, degrade gracefully
        fStackCount := 0;
        AssignFile(fStack, fTaskPath + '/stack');
        {$I-}
        Reset(fStack);
        {$I+}
        if IOResult = 0 then
        try
          while (fStackCount < 3) and (not EOF(fStack)) do
          begin
            ReadLn(fStack, fLine);
            aOutput.Add('    stack: ' + fLine);
            Inc(fStackCount);
          end;
        finally
          CloseFile(fStack);
        end;
      until FindNext(fSearchRec) <> 0;
    finally
      FindClose(fSearchRec);
    end;
  finally
    fFields.Free;
  end;

  aOutput.Add('');
end;

procedure _AppendDescriptors(const aOutput: TStringList);
var
  fSearchRec: TSearchRec;
  fTotal, fSockets: integer;
  fLinkTarget: String;
begin
  aOutput.Add('[descriptors]');

  fTotal := 0;
  fSockets := 0;
  if FindFirst('/proc/self/fd' + PathDelim + '*', faAnyFile, fSearchRec) = 0 then
  try
    repeat
      if (fSearchRec.Name = '.') or (fSearchRec.Name = '..') then
        Continue;
      Inc(fTotal);
      fLinkTarget := fpReadLink('/proc/self/fd/' + fSearchRec.Name);
      if Pos('socket:[', fLinkTarget) > 0 then
        Inc(fSockets);
    until FindNext(fSearchRec) <> 0;
  finally
    FindClose(fSearchRec);
  end;

  aOutput.Add(Format('  open file descriptors: %d (sockets: %d)', [fTotal, fSockets]));
  aOutput.Add('');
end;
{$ENDIF}

procedure _AppendAppInventory(const aOutput: TStringList);
var
  i: integer;
  fProc: TWatchdogInventoryProc;
begin
  if glInventoryNames = nil then
    exit;

  for i := 0 to glInventoryProcs.Count - 1 do
  begin
    aOutput.Add('[inventory: ' + glInventoryNames[i] + ']');
    fProc := glInventoryProcs[i];
    try
      fProc(aOutput);
    except
      on e: Exception do
        aOutput.Add('  <error reading inventory: ' + e.Message + '>');
    end;
    aOutput.Add('');
  end;
end;

procedure _AppendLogTail(const aOutput: TStringList);
var
  fStream: TFileStream;
  fFilename: String;
  fBufferSize, fBytesRead: integer;
  fBuffer: TBytes;
  fText: String;
  fLines: TStringList;
  fIndex, fFirst: integer;
begin
  aOutput.Add('[log tail]');

  fFilename := config.ReadString('debug', 'debugfile', ExtractFilePath(ParamStr(0)) + 'slftp.log');
  if not FileExists(fFilename) then
  begin
    aOutput.Add('  log file not found: ' + fFilename);
    exit;
  end;

  fStream := nil;
  fLines := TStringList.Create;
  try
    try
      fStream := TFileStream.Create(fFilename, fmOpenRead or fmShareDenyNone);
      fBufferSize := Min(fStream.Size, 32 * 1024);
      fStream.Seek(-fBufferSize, soEnd);
      SetLength(fBuffer, fBufferSize);
      fBytesRead := fStream.Read(fBuffer[0], fBufferSize);
      SetLength(fText, fBytesRead);
      if fBytesRead > 0 then
        Move(fBuffer[0], fText[1], fBytesRead);
      fLines.Text := fText;

      fFirst := Max(0, fLines.Count - 80);
      for fIndex := fFirst to fLines.Count - 1 do
        aOutput.Add('  ' + fLines[fIndex]);
    except
      on e: Exception do
        aOutput.Add('  <error reading log: ' + e.Message + '>');
    end;
  finally
    fStream.Free;
    fLines.Free;
  end;
end;

function WatchdogGenerateReport(const aReason: String): String;
var
  fOutput: TStringList;
  fNowStr: String;
  fFilename, fOldFile: String;
  fSearchRec: TSearchRec;
  fReportFiles: TStringList;
begin
  Result := '';

  _MarkReport;

  fOutput := TStringList.Create;
  try
    _AppendHeader(fOutput, aReason);
    _AppendHeartbeats(fOutput);
    _AppendLocks(fOutput);
    {$IFDEF UNIX}
    _AppendOsThreads(fOutput);
    _AppendDescriptors(fOutput);
    {$ENDIF}
    _AppendAppInventory(fOutput);
    _AppendLogTail(fOutput);
    fOutput.Add(StringOfChar('=', 100));

    DateTimeToString(fNowStr, 'yyyymmdd_hhnnss_zzz', Now());
    fFilename := ExtractFilePath(ParamStr(0)) + CReportPrefix + fNowStr + CReportSuffix;
    try
      fOutput.SaveToFile(fFilename);
      Result := fFilename;
    except
      on e: Exception do
      begin
        _Log('Watchdog: unable to write report: ' + e.Message);
        exit;
      end;
    end;

    // prune old reports, keep the most recent glKeepReports ones
    if glKeepReports > 0 then
    begin
      fReportFiles := TStringList.Create;
      try
        if FindFirst(ExtractFilePath(ParamStr(0)) + CReportPrefix + '*' + CReportSuffix, faAnyFile, fSearchRec) = 0 then
        try
          repeat
            fReportFiles.Add(fSearchRec.Name);
          until FindNext(fSearchRec) <> 0;
        finally
          FindClose(fSearchRec);
        end;

        fReportFiles.Sort;
        while fReportFiles.Count > glKeepReports do
        begin
          fOldFile := ExtractFilePath(ParamStr(0)) + fReportFiles[0];
          DeleteFile(fOldFile);
          fReportFiles.Delete(0);
        end;
      finally
        fReportFiles.Free;
      end;
    end;
  finally
    fOutput.Free;
  end;
end;

procedure TWatchdogThread.Execute;
var
  fObject: TObject;
  fParticipant: TWatchdogParticipant;
  fAge: Double;
  fWorstName: String;
  fWorstAge: Double;
  fReason: String;
  fReportFile: String;
  i: integer;
begin
  while not Terminated do
  begin
    fWorstName := '';
    fWorstAge := 0;

    try
      if glEnabled and (glLock <> nil) then
      begin
        glLock.Enter('WatchdogThread check');
        try
          fAge := _TickAgeSeconds(glMainThreadBeat);
          if fAge > glMainThreadStall then
          begin
            fWorstName := 'mainthread';
            fWorstAge := fAge;
          end;

          for fObject in glParticipants do
          begin
            try
              fParticipant := TWatchdogParticipant(fObject);
              fAge := _TickAgeSeconds(fParticipant.LastBeatTick);
              if fAge > fParticipant.StallSeconds then
              begin
                if fAge > fWorstAge then
                begin
                  fWorstName := fParticipant.Name;
                  fWorstAge := fAge;
                end;
              end;
            except
            end;
          end;

          if glApiInFlight > 0 then
          begin
            fAge := _TickAgeSeconds(glApiBusySinceTick);
            if (fAge > glApiStall) and (fAge > fWorstAge) then
            begin
              fWorstName := 'api (' + string(glApiLastUrl) + ')';
              fWorstAge := fAge;
            end;
          end;
        finally
          glLock.Leave;
        end;

        if fWorstName <> '' then
        begin
          if WatchdogReportDue then
          begin
            fReason := Format('%s shows no progress for %.0fs', [fWorstName, fWorstAge]);
            fReportFile := WatchdogGenerateReport(fReason);
            if fReportFile <> '' then
            begin
              _Log('Watchdog: ' + fReason + ' - report written: ' + fReportFile);
              if Assigned(glNotifyProc) then
                glNotifyProc(fReason, fReportFile);
            end;
          end;
        end;
      end;
    except
      on e: Exception do
        _Log('[EXCEPTION] TWatchdogThread.Execute : ' + e.Message);
    end;

    // sleep in short slices so shutdown is not delayed by a full check interval
    for i := 1 to glCheckInterval * 4 do
    begin
      if Terminated then
        break;
      Sleep(250);
    end;
  end;
end;

procedure WatchdogInit;
const
  CConfigSection = 'watchdog';
begin
  glEnabled := config.ReadBool(CConfigSection, 'enabled', True);
  glCheckInterval := Max(1, config.ReadInteger(CConfigSection, 'check_interval', 5));
  glMainThreadStall := Max(1, config.ReadInteger(CConfigSection, 'main_stall', 10));
  glApiStall := Max(1, config.ReadInteger(CConfigSection, 'api_stall', 300));
  glReportCooldown := Max(0, config.ReadInteger(CConfigSection, 'report_cooldown', 900));
  glKeepReports := config.ReadInteger(CConfigSection, 'keep_reports', 10);

  _EnsureInit;
  glUninited := False;
  glStartTick := GetTickCount64;
  glMainThreadBeat := glStartTick;
  glLastReportTick := 0;
  glApiLastEndTick := 0;
end;

procedure WatchdogStart;
begin
  if (not glEnabled) or (glWatchdogThread <> nil) then
    exit;

  glWatchdogThread := TWatchdogThread.Create(True);
  glWatchdogThread.FreeOnTerminate := False;
  glWatchdogThread.Start;
end;

procedure WatchdogUninit;
begin
  if glWatchdogThread <> nil then
  begin
    glWatchdogThread.Terminate;
    glWatchdogThread.WaitFor;
    FreeAndNil(glWatchdogThread);
  end;

  glUninited := True;

  FreeAndNil(glParticipants);
  FreeAndNil(glInventoryNames);
  FreeAndNil(glInventoryProcs);
  FreeAndNil(glLock);
end;

end.