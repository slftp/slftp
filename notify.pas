unit notify;

// EZT AZ UNItOT A QUEUE_LOCK LEZARASA UTAN SZABAD CSAK HASZNALNI
// THIS QUEUE_LOCK after the close of the Unite should only be used

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

implementation

uses SysUtils, Types, irc, debugunit;

const
  section = 'notify';

var
  gtnno: Integer;

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
  self.tnno := gtnno;
  event := TEvent.Create(nil, False, False, 'taskno' + IntToStr(tnno));
  inc(gtnno);
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
  gtnno := 0;
end;

procedure NotifyUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  tasknotifies.Free;
  Debug(dpSpam, section, 'Uninit2');  
end;

procedure TaskReady(t: TTask);
var
  i: integer;
  tn: TTaskNotify;
begin
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
end;

function AddNotify: TTaskNotify;
begin
  Result := TTaskNotify.Create;
  tasknotifies.Add(Result);
end;

procedure RemoveTN(tn: TTaskNotify);
begin
  try
    tasknotifies.Remove(tn);
  except
    exit;
  end;
end;

end.