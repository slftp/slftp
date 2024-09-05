unit slcriticalsection2;

interface

uses
  SyncObjs;

{
  TslCriticalSection
  Provices a possibility for a critical section to have a timeout when trying to enter.
}


type
  TslCriticalSection2 = class
  private
    fInternalCriticalSection: TCriticalSection;
    fEvent: TEvent;
    fLockCount: integer;
    fLockOwningThreadID: TThreadID;
    fCurrentLockOwnerName: string;
  public
    constructor Create(const aName: string);
    destructor Destroy;
    function Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal = 10000; const aRaiseExceptionOnFail: boolean = True): boolean;
    procedure Leave;
  end;

  procedure SlCriticalSection2Init(const aUseTimeoutLocking: boolean);
  procedure SlCriticalSection2Uninit;


implementation
  uses
    Generics.Collections, SysUtils, IdGlobal;

  var
    glUseTimeoutLocking: boolean;
    glUsedCriticalSectionNames: TList<string>;
    glUsedCriticalSectionNamesLock: TCriticalSection;


  procedure SlCriticalSection2Init(const aUseTimeoutLocking: boolean);
  begin
    glUseTimeoutLocking := aUseTimeoutLocking;
    if glUseTimeoutLocking then
    begin
      glUsedCriticalSectionNames := TList<string>.Create;
      glUsedCriticalSectionNamesLock := TCriticalSection.Create;
    end;
  end;

  procedure SlCriticalSection2Uninit;
  begin
    if glUseTimeoutLocking then
    begin
      glUsedCriticalSectionNames.Free;
      glUsedCriticalSectionNamesLock.Free;
    end;
  end;

  constructor TslCriticalSection2.Create(const aName: string);
  begin
    if glUseTimeoutLocking then
    begin
      // make sure a TslCriticalSection2 only exists once with the same name, because of the named mutex
      glUsedCriticalSectionNamesLock.Enter;
      try
        if glUsedCriticalSectionNames.Contains(aName) then
        begin
          raise Exception.Create(Format('SL Critical section with name %s already exists.', [aName]));
        end;
      finally
        glUsedCriticalSectionNamesLock.Leave;
      end;

      fEvent := TEvent.Create(nil, False, True, aName);
      fLockCount := 0;
      fLockOwningThreadID := 0;
    end
    else
    begin
      fInternalCriticalSection := TCriticalSection.Create;
    end;
  end;

  destructor TslCriticalSection2.Destroy;
  begin
    if glUseTimeoutLocking then
    begin
      fEvent.Free;
    end
    else
    begin
      fInternalCriticalSection.Free;
    end;
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal = 10000; const aRaiseExceptionOnFail: boolean = True): boolean;
  begin
    if glUseTimeoutLocking then
    begin
      // allow for the same thread to enter multiple times
      if fLockOwningThreadID = IdGlobal.CurrentThreadId then
      begin
        fLockCount := fLockCount + 1;
        Result := True;
        fCurrentLockOwnerName := aLockOwnerName;
      end
      else
      begin
        case fEvent.WaitFor(aTimeoutMs) of
          wrSignaled:
          {$IFDEF WINDOWS}
          wrIOCompletion:
          {$ENDIF}
          begin
            fLockOwningThreadID := IdGlobal.CurrentThreadId;
            Result := True;
            fCurrentLockOwnerName := aLockOwnerName;
          end;
          wrTimeout:
          begin
            if aRaiseExceptionOnFail then
            begin
              raise Exception.Create(Format('Unable to acquire lock by thread %s is held by thread %s (%d) - %s', [IntToHex(IdGlobal.CurrentThreadId, 4), IntToHex(fLockOwningThreadID, 4), fLockCount, fCurrentLockOwnerName]));
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
      fInternalCriticalSection.Enter;
    end;
  end;

  procedure TslCriticalSection2.Leave;
  begin
    if glUseTimeoutLocking then
    begin
      if fLockCount > 0 then
      begin
        fLockCount := fLockCount - 1;
      end
      else
      begin
        fLockOwningThreadID := 0;
        fCurrentLockOwnerName := '';
        fEvent.SetEvent;
      end;
    end
    else
    begin
      fInternalCriticalSection.Leave;
    end;
  end;
end.
