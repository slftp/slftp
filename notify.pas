unit notify;

// EZT AZ UNItOT A QUEUE_LOCK LEZARASA UTAN SZABAD CSAK HASZNALNI
// THIS QUEUE_LOCK after the close of the Unite should only be used

interface

uses Classes, syncobjs, tasksunit, Contnrs;

type
  TSiteResponse  = class
    sitename: AnsiString;
    slotname: AnsiString;
    response: AnsiString;

    ido: TDateTime;

    constructor Create(sitename, slotname, response: AnsiString; ido: TDateTime);
  end;
  TTaskNotify = class
    event: TEvent;
    tasks: TList;
    tnno: Integer;

    responses: TObjectList;

    constructor Create;
    destructor Destroy; override;
  end;

var tasknotifies: TObjectList;

procedure TaskReady(t: TTask);
function AddNotify: TTaskNotify;
procedure RemoveTN(tn: TTaskNotify);
procedure NotifyInit;
procedure NotifyUninit;

implementation

uses SysUtils, irc, debugunit;

const section = 'notify';

var gtnno: Integer;

function AddNotify: TTaskNotify;
begin
  Result:= TTaskNotify.Create;
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

constructor TTaskNotify.Create;
begin
  responses:= TObjectList.Create;
  tasks:= TList.Create;
  self.tnno:= gtnno;
  event:= TEvent.Create(nil, False, False, 'taskno'+IntToStr(tnno));
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
  tasknotifies:= TObjectList.Create;
  gtnno:= 0;
end;
procedure NotifyUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  tasknotifies.Free;
  Debug(dpSpam, section, 'Uninit2');  
end;

procedure TaskReady(t: TTask);
var i: integer;
    tn: TTaskNotify;
begin
 for i:= tasknotifies.Count-1 downto 0 do
 begin
   try if i < 0 then Break; except Break; end;
   try
     tn:= TTaskNotify(tasknotifies[i]);
     if -1 <> tn.tasks.IndexOf(t) then
     begin
       tn.tasks.Remove(t);

       if (t.response <> '') then
         tn.responses.Add(TSiteResponse.Create(t.site1, t.slot1name, t.response, t.ido));

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

{ TSiteResponse }

constructor TSiteResponse.Create(sitename, slotname, response: AnsiString; ido: TDateTime);
begin
  self.sitename:= sitename;
  self.slotname:= slotname;
  self.response:= response;
  self.ido:= ido;
end;

end.
