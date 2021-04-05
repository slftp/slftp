unit notify;

interface

uses Classes, syncobjs, tasksunit, Contnrs;

type
  TSiteResponse = class
  private
    FSitename: String;
    FSlotname: String;
    FResponse: String;
    FTime: TDateTime;
  public
    constructor Create(const sitename, slotname, response: String; const time: TDateTime);
  published
    property sitename: String read FSitename; //< sitename
    property slotname: String read FSlotname; //< slotname
    property response: String read FResponse; //< site response
    property time: TDateTime read FTime; //< time value
  end;

  TTaskNotify = class
  private
    tnno: Integer;
  public
    event: TEvent;
    tasks: TList;
    responses: TObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

procedure NotifyInit;
procedure NotifyUninit;
procedure TaskReady(t: TTask);
function AddNotify: TTaskNotify;
procedure RemoveTN(tn: TTaskNotify);

var
  tasknotifies: TObjectList;
  FLockObject: TCriticalSection;

implementation

uses SysUtils, Types, irc, debugunit;

const
  section = 'notify';

var
  glTaskNumber: Integer; //< unique number used to identify the task event

{ TSiteResponse }

constructor TSiteResponse.Create(const sitename, slotname, response: String; const time: TDateTime);
begin
  FSitename := sitename;
  FSlotname := slotname;
  FResponse := response;
  FTime := time;
end;

{ TTaskNotify }

constructor TTaskNotify.Create;
begin
  responses := TObjectList.Create;
  tasks := TList.Create;
  self.tnno := {$IFDEF FPC}InterlockedIncrement{$ELSE}AtomicIncrement{$ENDIF}(glTaskNumber);
  event := TEvent.Create(nil, False, False, 'taskno' + IntToStr(tnno));
end;

destructor TTaskNotify.Destroy;
begin
  tasks.Free;
  event.Free;
  responses.Free;
  inherited;
end;

procedure NotifyInit;
begin
  tasknotifies := TObjectList.Create;
  FLockObject := TCriticalSection.Create;
  glTaskNumber := 0;
end;

procedure NotifyUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  FLockObject.Enter;
  try
    tasknotifies.Free;
  finally
    FLockObject.Leave;
  end;
  FLockObject.Free;
  Debug(dpSpam, section, 'Uninit2');  
end;

procedure TaskReady(t: TTask);
var
  i: integer;
  tn: TTaskNotify;
begin
  FLockObject.Enter;
  try
    for i := tasknotifies.Count - 1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      try
        tn := TTaskNotify(tasknotifies[i]);
        if tn.tasks.IndexOf(t) <> -1 then
        begin
          tn.tasks.Remove(t);

          if (t.response <> '') then
            tn.responses.Add(TSiteResponse.Create(t.site1, t.slot1name, t.response, t.time));

          if ((t.announce <> '') and (not t.readyerror)) then
            irc_addtext(t, t.announce);

          if tn.tasks.Count = 0 then
            tn.event.SetEvent;
        end;
      except
        on e: Exception do begin
          Debug(dpError, section, Format('[EXCEPTION] TaskReady: %s', [e.Message]));
          Continue;
        end;
      end;
     end;
  finally
    FLockObject.Leave;
  end;
end;

function AddNotify: TTaskNotify;
begin
  Result := TTaskNotify.Create;
  FLockObject.Enter;
  try
    tasknotifies.Add(Result);
  finally
    FLockObject.Leave;
  end;
end;

procedure RemoveTN(tn: TTaskNotify);
begin
  FLockObject.Enter;
  try
    try
      tasknotifies.Remove(tn);
    except
      on e: Exception do begin
        Debug(dpError, section, Format('[EXCEPTION] RemoveTN: %s', [e.Message]));
        Exit;
      end;
    end;
  finally
    FLockObject.Leave;
  end;
end;

end.
