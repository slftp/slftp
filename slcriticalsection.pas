unit slcriticalsection;

interface

uses
  SyncObjs, Contnrs, IdGlobal;

type
  TslCriticalSection = class
  private
    rc: Integer;
    rt: TIdThreadId;
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
  Classes, Types, SysUtils, irc, debugunit;

var
  ec: Cardinal = 0;

{ TslCriticalSection }

constructor TslCriticalSection.Create(name: String = '');
begin
  self.name :=  name;
  l := TCriticalSection.Create;
  w := TObjectList.Create(False);
end;

destructor TslCriticalSection.Destroy;
var
  i: Integer;
  wait_read: Integer;
begin
  wait_read := 0;
  slshutdown := True;

  l.Enter;
  try
    for i := 0 to w.Count - 1 do
      TEvent(w[i]).SetEvent;
  finally
    l.Leave;
  end;

  while(true) do
  begin
    l.Enter;
    try
      i := w.Count;
    finally
      l.Leave;
    end;

    Inc(wait_read);
    if (wait_read > 250) then
    begin
      debugunit.Debug(dpError, 'criticalSection', '[iNFO] TslCriticalSection.Destroy count break', []);
      break;
    end;

    if i > 0 then
      sleep(50)
    else
      Break;
  end;

  w.Free;
  l.Free;
  inherited;
end;

function TslCriticalSection.Enter(sname: String = ''): Boolean;
label
  ujra;
var
  procId: TIdThreadId;
  event: TEvent;
begin
  Result := False;
  procId := IdGlobal.CurrentThreadId;

ujra:
  if slshutdown then exit;

  l.Enter;
  if (rt = 0) then
    rt := procId;

  if (rt = procId) then
  begin
    inc(rc);
    l.Leave;
    Result := True;
  end
  else
  begin
    inc(ec);
    event := TEvent.Create(nil, False, False, Format('%d-%s-%s', [ec, name, sname]));
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
            Debug(dpError, 'criticalSection', 'TslCriticalSection.Enter: Force Leave (%d) Timeout 30sec: %s (%s)', [procId, name, sname]);
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
var
  procId: TIdThreadId;
begin
  procId := IdGlobal.CurrentThreadId;
  try
    l.Enter;
    try
      if rt = procId then
      begin
        dec(rc);
        if rc <= 0 then
        begin
          rc := 0;
          rt := 0;
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
