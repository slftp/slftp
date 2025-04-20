unit slcriticalsection2;

interface

uses
  SyncObjs, Generics.Collections;

{
  TslCriticalSection
  Provides a possibility for a critical section to have a timeout when trying to enter.
}


type
  TslCriticalSection2 = class
  private
    FInternalCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FLockCount: integer;
    FLockOwningThreadID: TThreadID;
    FName, FCurrentCodeSegmentName: string;
    FUseTimeoutLocking: boolean;
    FLockOwnerNameStack: TStack<string>;
    function GetCurrentLockOwnerName: string;
  public
    constructor Create(const aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
    destructor Destroy; override;
    function Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean; overload;
    function Enter(const aLockOwnerName: string): boolean; overload;
    procedure Leave;
    procedure SetCurrentCodeSegment(const aSegmentName: string);
    property CurrentLockOwnerName: string read GetCurrentLockOwnerName;
  end;

  procedure SlCriticalSection2Init(const aLockingTimeout: integer);
  procedure SlCriticalSection2Uninit;
  function GetUseTimeoutLocking: boolean;


implementation
  uses
    SysUtils, IdGlobal, debugunit;

  var
    glUseTimeoutLocking: boolean;
    glDefaultLockingTimeout: integer;
    glUsedCriticalSectionNames: TDictionary<string, integer>;
    glUsedCriticalSectionNamesLock: TCriticalSection;
    glDebugSection: string = 'slcriticalsection2';


  procedure SlCriticalSection2Init(const aLockingTimeout: integer);
  begin
    if aLockingTimeout > 0 then
    begin
      glUseTimeoutLocking := True;
      glDefaultLockingTimeout := aLockingTimeout;
    end
    else
      glUseTimeoutLocking := False;

    glUsedCriticalSectionNames := TDictionary<string, integer>.Create;
    glUsedCriticalSectionNamesLock := TCriticalSection.Create;
  end;

  procedure SlCriticalSection2Uninit;
  begin
    glUseTimeoutLocking := False;
    glUsedCriticalSectionNames.Free;
    glUsedCriticalSectionNamesLock.Free;
  end;

  constructor TslCriticalSection2.Create(const aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
  begin
    FName := aName;
    if glUseTimeoutLocking Or aAlwaysUseTimeoutLocking then
    begin
      FUseTimeoutLocking := True;

      // make sure a TslCriticalSection2 only exists once with the same name, because of the named mutex
      glUsedCriticalSectionNamesLock.Enter;
      try
        if glUsedCriticalSectionNames.ContainsKey(aName) then
        begin
          raise Exception.Create(Format('SL Critical section with name %s already exists.', [aName]));
        end;
        glUsedCriticalSectionNames.Add(aName, 0);
      finally
        glUsedCriticalSectionNamesLock.Leave;
      end;

      FEvent := TEvent.Create(nil, False, True, 'SLFTP_' + aName);
      FLockCount := 0;
      FLockOwningThreadID := 0;
      FCurrentCodeSegmentName := '';
      FLockOwnerNameStack := TStack<string>.Create;
    end
    else
    begin
      FUseTimeoutLocking := False;
      FInternalCriticalSection := TCriticalSection.Create;
    end;
  end;

  destructor TslCriticalSection2.Destroy;
  begin
    if FUseTimeoutLocking then
    begin
      FEvent.Free;
      FLockOwnerNameStack.Free;

      glUsedCriticalSectionNamesLock.Enter;
      try
        glUsedCriticalSectionNames.Remove(self.FName);
      finally
        glUsedCriticalSectionNamesLock.Leave;
      end;
    end
    else
    begin
      FInternalCriticalSection.Free;
    end;
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string): boolean;
  begin
    Result := self.Enter(aLockOwnerName, glDefaultLockingTimeout);
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean;
  begin
    if FUseTimeoutLocking then
    begin
      // allow for the same thread to enter multiple times
      if FLockOwningThreadID = IdGlobal.CurrentThreadId then
      begin
        FLockCount := fLockCount + 1;
        Result := True;
        FLockOwnerNameStack.Push(aLockOwnerName);
      end
      else
      begin
        case FEvent.WaitFor(aTimeoutMs) of
          wrSignaled:
          {$IFDEF WINDOWS}
          wrIOCompletion:
          {$ENDIF}
          begin
            FLockOwningThreadID := IdGlobal.CurrentThreadId;
            Result := True;
            FLockOwnerNameStack.Push(aLockOwnerName);
          end;
          wrTimeout:
          begin
            if aRaiseExceptionOnFail then
            begin
              raise Exception.Create(Format('Unable to acquire lock ''%s'' (%s) by %s thread within %d ms. Lock is held by thread %s (%d) - %s (%s)', [FName, aLockOwnerName, IntToHex(IdGlobal.CurrentThreadId, 4), aTimeoutMs, IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName, FCurrentCodeSegmentName]));
            end;
            Result := False;
          end;
          wrAbandoned:
            raise Exception.Create(Format('Mutex abandoned when trying to lock: %s', [aLockOwnerName]));
          wrError:
            raise Exception.Create(Format('Error when trying to lock: %s', [aLockOwnerName]));
        else
          raise Exception.Create(Format('Unknown wait result when trying to lock: %s', [aLockOwnerName]));
        end;
      end
    end
    else
    begin
      FInternalCriticalSection.Enter;
      Result := True;
    end;
  end;

  procedure TslCriticalSection2.Leave;
  begin
    if FUseTimeoutLocking then
    begin
      if FLockOwningThreadID = 0 then
        raise Exception.Create(Format('Trying to leave lock by thread %s but it has not been entered before', [IntToHex(IdGlobal.CurrentThreadId, 4)]));

      if FLockOwningThreadID <> IdGlobal.CurrentThreadId then
        raise Exception.Create(Format('Trying to leave lock by thread %s but it is held by thread %s (%d) - %s', [IntToHex(IdGlobal.CurrentThreadId, 4), IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName]));

      if FLockCount > 0 then
      begin
        FLockCount := FLockCount - 1;
        FLockOwnerNameStack.Pop;
      end
      else
      begin
        FLockOwningThreadID := 0;
        FLockOwnerNameStack.Pop;
        FCurrentCodeSegmentName := '';
        FEvent.SetEvent;
      end;
    end
    else
    begin
      FInternalCriticalSection.Leave;
    end;
  end;

  procedure TslCriticalSection2.SetCurrentCodeSegment(const aSegmentName: string);
  begin
    if FUseTimeoutLocking then
    begin
      if FLockOwningThreadID = 0 then
      begin
        Debug(dpError, glDebugSection, Format('Tried to notify code segment ''%s'', but lock is not held by any thread.', [aSegmentName]));
        exit;
      end;

      if FLockOwningThreadID <> IdGlobal.CurrentThreadId then
      begin
        Debug(dpError, glDebugSection, Format('Tried to notify code segment ''%s'', but lock is by another thread %s (%d) - %s.', [IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName]));
        exit;
      end;

      FCurrentCodeSegmentName := aSegmentName;
    end;
  end;

  function GetUseTimeoutLocking: boolean;
  begin
    Result := glUseTimeoutLocking;
  end;

  function TSlCriticalSection2.GetCurrentLockOwnerName;
  begin
    if FUseTimeoutLocking and (FLockOwnerNameStack.Count > 0) then
      Result := FLockOwnerNameStack.Peek
    else
      Result := '';
  end;
end.

