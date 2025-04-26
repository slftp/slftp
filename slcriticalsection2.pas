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
    procedure InitNoTimeoutLocking;
    procedure FreeObjects;
  public
    constructor Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
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
    glUseTimeoutLocking: boolean = True;
    glDefaultLockingTimeout: integer;
    glUsedCriticalSections: TDictionary<string, TslCriticalSection2>;
    glUsedCriticalSectionsLock: TCriticalSection;
    glDebugSection: string = 'slcriticalsection2';
    glIsInitialized: boolean = False;


  procedure SlCriticalSection2Init(const aLockingTimeout: integer);
  var
    fExistingCs: TSlCriticalSection2;
  begin
    if aLockingTimeout > 0 then
    begin
      glUseTimeoutLocking := True;
      glDefaultLockingTimeout := aLockingTimeout;
    end
    else
      glUseTimeoutLocking := False;

    if not glIsInitialized then
    begin
      glUsedCriticalSections := TDictionary<string, TslCriticalSection2>.Create;
      glUsedCriticalSectionsLock := TCriticalSection.Create;
      glIsInitialized := True;
    end
    else if aLockingTimeout = 0 then
    begin
      // happens at startup when a TslCriticalSection2 is created before initialization then it will have called init already with timeout locking enabled
      // So if timeout locking is being disabled now, we will change the existing locks to be non timeout locks as well.
      glUsedCriticalSectionsLock.Enter;
      try
        for fExistingCs in glUsedCriticalSections.Values do
        begin
          fExistingCs.FreeObjects;
          fExistingCs.InitNoTimeoutLocking;
        end;
      finally
        glUsedCriticalSectionsLock.Leave;
      end;
    end;
  end;

  procedure SlCriticalSection2Uninit;
  begin
    glUseTimeoutLocking := False;
    FreeAndNil(glUsedCriticalSections);
    FreeAndNil(glUsedCriticalSectionsLock);
    glIsInitialized := False;
  end;

  procedure TslCriticalSection2.InitNoTimeoutLocking;
  begin
    FUseTimeoutLocking := False;
    FInternalCriticalSection := TCriticalSection.Create;
  end;

  constructor TslCriticalSection2.Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
  begin
    if not glIsInitialized then // happens at startup when a TslCriticalSection2 is created before initialization
    begin
      // init with timeout locking enabled and then if it will be initialized again with timeout locking disabled, change the existing instances
      SlCriticalSection2Init(60000);
    end;

    aName := aName.Replace('\', '_'); // backslash not allowed on windows

    FName := aName;
    if glUseTimeoutLocking Or aAlwaysUseTimeoutLocking then
    begin
      // make sure a TslCriticalSection2 only exists once with the same name, because of the named mutex
      glUsedCriticalSectionsLock.Enter;
      try
        if glUsedCriticalSections.ContainsKey(aName) then
        begin
          raise Exception.Create(Format('SL Critical section with name %s already exists.', [aName]));
        end;
        glUsedCriticalSections.Add(aName, self);
      finally
        glUsedCriticalSectionsLock.Leave;
      end;

      FUseTimeoutLocking := True;
      FEvent := TEvent.Create(nil, False, True, 'SLFTP_' + aName);
      FLockCount := 0;
      FLockOwningThreadID := 0;
      FCurrentCodeSegmentName := '';
      FLockOwnerNameStack := TStack<string>.Create;
    end
    else
    begin
      self.InitNoTimeoutLocking;
    end;
  end;

  procedure TSlCriticalSection2.FreeObjects;
  begin
    if FUseTimeoutLocking then
    begin
      FEvent.Free;
      FLockOwnerNameStack.Free;

      if glUsedCriticalSectionsLock <> nil then
      begin
        glUsedCriticalSectionsLock.Enter;
        try
          glUsedCriticalSections.Remove(self.FName);
        finally
          glUsedCriticalSectionsLock.Leave;
        end;
      end;
    end
    else
    begin
      FInternalCriticalSection.Free;
    end;
  end;

  destructor TslCriticalSection2.Destroy;
  begin
    self.FreeObjects;
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

