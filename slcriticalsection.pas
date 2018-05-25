unit slcriticalsection;

interface

uses SyncObjs, Contnrs;

type
  TslCriticalSection = class
  private
    rc: Integer;
    rt: LongWord;
    w: TObjectList;
    l: TCriticalSection;
    slshutdown: Boolean;
    name: String;
  public
    constructor Create(name: String = '');
    destructor Destroy; override;
    function Enter(sname: String = ''): Boolean;
    procedure Leave;
  end;

implementation

uses
  Classes, Types, SysUtils
  {$IFDEF MSWINDOWS}
    , Windows
  {$ELSE}
    {$IFDEF FPC}
      , pthreads
    {$ELSE}
      , Libc
    {$ENDIF}
  {$ENDIF}
  , irc, debugunit;


var
  ec: Cardinal = 0;

function MyGetCurrentProcessId(): LongWord;
begin
{$IFDEF MSWINDOWS}
  Result:= GetCurrentThreadId;
{$ELSE}
  Result:= LongWord(pthread_self());
{$ENDIF}
end;


{ TslCriticalSection }

constructor TslCriticalSection.Create(name: String = '');
begin
  self.name :=  name;
  l:= TCriticalSection.Create;
  w:= TObjectList.Create(False);
end;

destructor TslCriticalSection.Destroy;
var i: Integer;
    wait_read: Integer;
begin
  wait_read:= 0;
  
  slshutdown:= True;
  l.Enter;
  for i:= 0 to w.Count -1 do
    TEvent(w[i]).SetEvent;
  l.Leave;

  while(true) do
  begin
    l.Enter;
    i:= w.Count;
    l.Leave;

    Inc(wait_read);
    if (wait_read > 250) then
    begin
      debugunit.Debug(dpError, 'criticalSection', '[iNFO] TslCriticalSection.Destroy count break', []);
      break;
    end;
    if i > 0 then sleep(50) else Break;
  end;

  w.Free;
  l.Free;
  inherited;
end;

function TslCriticalSection.Enter(sname: String = ''): Boolean;
label ujra;
var procId: LongWord;
    event: TEvent;
begin
  Result:= False;
  procId:= MyGetCurrentProcessId;

ujra:
  if slshutdown then exit;

  l.Enter;
  if (rt = 0) then
    rt:= procId;

  if (rt = procId) then
  begin
    inc(rc);
    l.Leave;
    Result:= True;
  end else
  begin
(*
    inc(ec);
    event:= TEvent.Create(nil, False, False, IntToStr(ec));
    w.Add(event);
    l.Leave;
    event.WaitFor($FFFFFFFF);

    l.Enter;
    w.Remove(event);
    l.Leave;
    event.Free;

    goto ujra;
*)

    inc(ec);
    event:= TEvent.Create(nil, False, False, IntToStr(ec)+'-'+name+'-'+sname);
    w.Add(event);
    l.Leave;
    try
      try
        case event.WaitFor(30 * 1000) of
          wrSignaled : { Event fired. Normal exit. }
          begin
            l.Enter;
            w.Remove(event);
            l.Leave;
          end;
          else { Timeout reach }
          begin
            Debug(dpError, 'criticalSection','TslCriticalSection.Enter: Force Leave ('+IntToStr(procId)+') Timeout 30sec: '+name+'('+sname+')');
            l.Enter;
            w.Remove(event);
            l.Leave;
            if w.Count > 0 then
              TEvent(w[0]).SetEvent;
          end;
        end;
      finally
        event.Free;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'criticalSection', Format('[EXCEPTION] TslCriticalSection.Enter : %s', [e.Message]));
      end;
    end;
    goto ujra;
  end;
end;

procedure TslCriticalSection.Leave;
var procId: LongWord;
begin
  procId:= MyGetCurrentProcessId;
  try
    l.Enter;
    try
      if rt = procId then
      begin
        dec(rc);
        if rc <= 0 then
        begin
          rc:= 0;
          rt:= 0;
          if w.Count > 0 then
            TEvent(w[0]).SetEvent;
        end;
      end;
    finally
      l.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'criticalSection', Format('[EXCEPTION] TslCriticalSection.Leave : %s', [e.Message]));
      exit;
    end;
  end;
end;

end.
