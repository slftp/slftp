unit slthread;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { Base class for all long-running slftp threads.

    Every instance self-registers in a global registry on creation and
    unregisters again on destruction. During application shutdown
    @link(TSlThread.SignalAll) asks every registered thread to terminate by
    calling the virtual @link(TSlThread.SignalStop) (descendants override it
    to also wake the thread, e.g. by setting an event or closing a socket),
    and @link(TSlThread.WaitAll) waits for the registry to empty, returning
    the names of threads which did not stop within the timeout. }
  TSlThread = class(TThread)
  private
    FThreadName: String; //< human readable name used in shutdown reports
  public
    { @param(aThreadName name of the thread, shown in shutdown reports)
      @param(aCreateSuspended if @true, the thread is created suspended) }
    constructor Create(const aThreadName: String; const aCreateSuspended: Boolean = False); reintroduce; virtual;
    destructor Destroy; override;

    { Called during application shutdown to ask the thread to terminate.
      The default implementation just calls Terminate; descendants which
      block on events or sockets should override it to also wake the thread.
      Must be safe to call on a thread which is already finishing. }
    procedure SignalStop; virtual;

    property ThreadName: String read FThreadName; //< name shown in shutdown reports

    { Calls SignalStop on every registered thread. }
    class procedure SignalAll;

    { Waits until every registered thread has terminated or the timeout is reached.
      @param(aTimeoutSeconds maximum time to wait)
      @returns(names of threads still registered after the timeout, empty if all stopped) }
    class function WaitAll(const aTimeoutSeconds: Integer): TStringList;
  end;

implementation

uses
  debugunit;

const
  section = 'slthread';

var
  // NOTE: plain TCriticalSection on purpose instead of TSlCriticalSection2:
  // the registry must still work while other units are being finalized
  // (stragglers unregistering late), SlCriticalSection2Uninit may already
  // have run at that point.
  glThreadRegistry: TList = nil; //< holds all living TSlThread instances
  glThreadRegistryLock: TCriticalSection = nil; //< protects glThreadRegistry

procedure _RegistryAdd(const aThread: TSlThread);
begin
  if glThreadRegistryLock = nil then
    exit;
  glThreadRegistryLock.Enter;
  try
    glThreadRegistry.Add(aThread);
  finally
    glThreadRegistryLock.Leave;
  end;
end;

procedure _RegistryRemove(const aThread: TSlThread);
begin
  if glThreadRegistryLock = nil then
    exit;
  glThreadRegistryLock.Enter;
  try
    glThreadRegistry.Remove(aThread);
  finally
    glThreadRegistryLock.Leave;
  end;
end;

{ TSlThread }

constructor TSlThread.Create(const aThreadName: String; const aCreateSuspended: Boolean = False);
begin
  FThreadName := aThreadName;
  // register before inherited Create: with aCreateSuspended=False and
  // FreeOnTerminate the thread may in theory already be finished and
  // destroyed by the time inherited Create returns
  _RegistryAdd(self);
  inherited Create(aCreateSuspended);
end;

destructor TSlThread.Destroy;
begin
  _RegistryRemove(self);
  inherited;
end;

procedure TSlThread.SignalStop;
begin
  Terminate;
end;

class procedure TSlThread.SignalAll;
var
  i: Integer;
  fThread: TSlThread;
begin
  if glThreadRegistryLock = nil then
    exit;
  glThreadRegistryLock.Enter;
  try
    // A thread destroying itself blocks on this lock before unregistering,
    // so the list cannot change while we hold it. SignalStop calls must
    // tolerate threads which are already on their way out.
    for i := glThreadRegistry.Count - 1 downto 0 do
    begin
      fThread := TSlThread(glThreadRegistry[i]);
      try
        fThread.SignalStop;
      except
        on e: Exception do
          Debug(dpError, section, Format('[EXCEPTION] SignalAll %s: %s', [fThread.FThreadName, e.Message]));
      end;
    end;
  finally
    glThreadRegistryLock.Leave;
  end;
end;

class function TSlThread.WaitAll(const aTimeoutSeconds: Integer): TStringList;
var
  fDeadline: TDateTime;
  i: Integer;
begin
  Result := TStringList.Create;
  if glThreadRegistryLock = nil then
    exit;

  fDeadline := Now + (aTimeoutSeconds / SecsPerDay);
  while True do
  begin
    glThreadRegistryLock.Enter;
    try
      if glThreadRegistry.Count = 0 then
        exit;

      if Now >= fDeadline then
      begin
        for i := 0 to glThreadRegistry.Count - 1 do
          Result.Add(TSlThread(glThreadRegistry[i]).ThreadName);
        exit;
      end;
    finally
      glThreadRegistryLock.Leave;
    end;

    Sleep(50);
  end;
end;

initialization
  glThreadRegistryLock := TCriticalSection.Create;
  glThreadRegistry := TList.Create;

finalization
  // threads are expected to be gone by now (WaitAll ran); a late straggler
  // unregistering after this point is a no-op because of the nil guards
  FreeAndNil(glThreadRegistry);
  FreeAndNil(glThreadRegistryLock);

end.
