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
    FInternalCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FLockCount: integer;
    FLockOwningThreadID: TThreadID;
    FName, FCurrentLockOwnerName: string;
    FUseTimeoutLocking: boolean;
  public
    constructor Create(const aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
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
    glUsedCriticalSectionNames := TList<string>.Create;
    glUsedCriticalSectionNamesLock := TCriticalSection.Create;
  end;

  procedure SlCriticalSection2Uninit;
  begin
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
        if glUsedCriticalSectionNames.Contains(aName) then
        begin
          raise Exception.Create(Format('SL Critical section with name %s already exists.', [aName]));
        end;
        glUsedCriticalSectionNames.Add(aName);
      finally
        glUsedCriticalSectionNamesLock.Leave;
      end;

      FEvent := TEvent.Create(nil, False, True, aName);
      FLockCount := 0;
      FLockOwningThreadID := 0;
    end
    else
    begin
      FUseTimeoutLocking := False;
      FInternalCriticalSection := TCriticalSection.Create;
    end;
  end;

  destructor TslCriticalSection2.Destroy;
  var
    i: integer;
  begin
    if FUseTimeoutLocking then
    begin
      FEvent.Free;

      glUsedCriticalSectionNamesLock.Enter;
      if glUsedCriticalSectionNames.Count > 0 then
      begin
        try
          for i := glUsedCriticalSectionNames.Count -1 to 0 do
          begin
            if glUsedCriticalSectionNames[i] = self.FName then
            begin
              glUsedCriticalSectionNames.Delete(i);
            end;
          end;
        finally
          glUsedCriticalSectionNamesLock.Leave;
        end;
      end;
    end
    else
    begin
      FInternalCriticalSection.Free;
    end;
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal = 10000; const aRaiseExceptionOnFail: boolean = True): boolean;
  begin
    if FUseTimeoutLocking then
    begin
      // allow for the same thread to enter multiple times
      if FLockOwningThreadID = IdGlobal.CurrentThreadId then
      begin
        FLockCount := fLockCount + 1;
        Result := True;
        FCurrentLockOwnerName := aLockOwnerName;
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
            FCurrentLockOwnerName := aLockOwnerName;
          end;
          wrTimeout:
          begin
            if aRaiseExceptionOnFail then
            begin
              raise Exception.Create(Format('Unable to acquire lock ''%s'' by thread %s is held by thread %s (%d) - %s', [FName, IntToHex(IdGlobal.CurrentThreadId, 4), IntToHex(FLockOwningThreadID, 4), FLockCount, FCurrentLockOwnerName]));
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
    end;
  end;

  procedure TslCriticalSection2.Leave;
  begin
    if FUseTimeoutLocking then
    begin
      if FLockCount > 0 then
      begin
        FLockCount := FLockCount - 1;
      end
      else
      begin
        FLockOwningThreadID := 0;
        FcurrentLockOwnerName := '';
        FEvent.SetEvent;
      end;
    end
    else
    begin
      FInternalCriticalSection.Leave;
    end;
  end;
end.
