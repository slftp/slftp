unit slvision;

interface

uses Classes, types, SyncObjs, Contnrs;

type
  TslRect = class
  private
    fLeft:   integer;
    fRight:  integer;
    fTop:    integer;
    fBottom: integer;
    procedure setBottom(const Value: integer);
    procedure setLeft(const Value: integer);
    procedure setRight(const Value: integer);
    procedure setTop(const Value: integer);
  public
    Width:  integer;
    Height: integer;
    property Left: integer Read fLeft Write setLeft;
    property Right: integer Read fRight Write setRight;
    property Top: integer Read fTop Write setTop;
    property Bottom: integer Read fBottom Write setBottom;
    constructor Create(Left, Top, Right, Bottom: integer);
    procedure Assign(src: TslRect); overload;
    procedure Assign(Left, Top, Right, Bottom: integer); overload;
  end;

  TslConsoleTask = class
    procedure Execute(); virtual; abstract;
    procedure OnConleTaskAdded(v_queue: TObjectList); virtual;
  end;

  TslVisible = (slvParent, slvHidden, slvVisible);

  TslControl = class;

  TslEvent = procedure(Sender: TslControl) of object;

  TslTimer = class
  private
    lastFire: TDateTime;
  protected
    procedure Tick;
  public
    interval: integer;
    procedure OnTimer; virtual; abstract;
  end;

  TslHorizontalAlignment = (slHALeft, slHARight);
  TslVerticalAlignment = (slVATop, slVABottom);
  TslModalResult = (mrNone, mrOk, mrCancel, mrOther);

  TslControl = class
  private
    function TabKey: boolean;
    procedure SetModalResult(mr: TslModalResult);
  protected
    directAddressing: boolean;
    sli:     integer;
    focus:   TslControl;
    focused: boolean;

    ca: TslRect; // clientarea

    ffVisible: TslVisible;
    fParent:   TslControl;
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); virtual;
    procedure SetVisible(Value: TslVisible); virtual;
    function GetVisible: TslVisible;
    procedure SetColors; virtual;
    procedure VisibilityChanged(c: TslControl); virtual;
    function TakesInputFocus(): TslControl; virtual;
    function KeyEvent(c: char; extended: boolean): boolean; virtual;

    procedure LostFocus(); virtual;
    procedure Write(ca: TslRect; const x, y: integer; s: string; hossz: integer = 0);
      overload; virtual;
    procedure Write(const x, y: integer; s: string; hossz: integer = 0); overload;
    procedure Write(const x, y: integer; c: char); overload;
    function BackgroundCharacter: char; virtual;
  public
    children: TObjectList;
    procedure SetParent(control: TslControl);
    constructor Create(parent: TslControl);
    destructor Destroy; override;
    procedure DelControl(control: TslControl);
    procedure AddControl(control: TslControl); virtual;
    property Parent: TslControl Read fParent;
    procedure Repaint; virtual;
    function Name: string;
    procedure Dump(prefix: string = '');
    function FocusedControl: TslControl;
    function app: TslControl;
    procedure GotoXy(ca: TslRect; x, y: integer); overload; virtual;
    procedure GotoXy(x, y: integer); overload;
    property Visible: TslVisible Read GetVisible Write SetVisible;
  end;

  TslBackground = class(TslControl)
  public
    procedure Repaint; override;
  end;

  TslMainBackground = class(TslBackground)
  public
    procedure SetColors; override;
  end;


  TslFixWidthPanel = class(TslControl)
  protected
    fixedWidth: integer;
  public
    constructor Create(Width: integer; parent: TslControl);
  end;

  TslFixHeightPanel = class(TslControl)
  protected
    fixedHeight: integer;
  public
    constructor Create(Height: integer; parent: TslControl);
  end;

  TslAlignedControl = class(TslBackground)
  protected
    fixedWidth:  integer;
    fixedHeight: integer;
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    procedure SizeChanged;
    function BackgroundCharacter: char; override;
  public
    Left, Top, Right, Bottom: integer;
    constructor Create(Width, Height: integer; parent: TslControl); overload;
    function Width: integer; virtual;
    function Height: integer; virtual;
    function MaxWidth: integer; virtual;
    function MaxHeight: integer; virtual;
  end;


  TslEdit      = class;
  TslOnKeyDown = function(Sender: TslEdit; c: char;
    extended: boolean): boolean of object;

  TslEdit = class(TslAlignedControl)
  private
    fText:      string;
    fcursor:    integer;
    fwindow:    integer;
    fOnKeyDown: TslOnKeyDown;
    lastcharonly: boolean;
    procedure SetText(Value: string);
    procedure SetCursor(Value: integer);
  public
    maxwidth:     integer;
    passwordchar: char;
    constructor Create(Left, Top, Width: integer; parent: TslControl);
    procedure SetColors; override;
    procedure Repaint; override;
    function TakesInputFocus: TslControl; override;
    function KeyEvent(c: char; extended: boolean): boolean; override;
    property Text: string Read fText Write SetText;
    property Cursor: integer Read fCursor Write SetCursor;
    property Window: integer Read fWindow;
    property OnKeyDown: TslOnKeyDown Read fOnKeyDown Write fOnKeyDown;
  end;

  TslCommandEvent = procedure(Sender: TslEdit; const command: string) of object;

  TslCommandEdit = class(TslEdit)
  private
    lastCommandIndex: integer;
    fOnCommand: TslCommandEvent;
  public
    fCommands:   TStringList;
    maxcommands: integer;
    destructor Destroy; override;
    constructor Create(Left, Top, Width: integer; parent: TslControl);
    function KeyEvent(c: char; extended: boolean): boolean; override;
    property OnCommand: TslCommandEvent Read fOnCommand Write fOnCommand;
  end;

  TslTextAlignment = (sltaLeft, sltaCenter, sltaRight);

  TslLabel = class(TslAlignedControl)
  protected
    fCaption: TStringList;
    function GetCaption(): string;
    procedure SetCaption(Value: string);
  public
    Alignment: TslTextAlignment;
    constructor Create(Caption: string; parent: TslControl); overload;
    destructor Destroy; override;
    property Caption: string Read GetCaption Write SetCaption;
    procedure Repaint; override;
  end;

  TslButton = class(TslLabel)
  private
    modalResult: TslModalResult;
    fOnClick:    TslEvent;
  public
    procedure SetColors; override;
    constructor Create(modalResult: TslModalResult; parent: TslControl);
    function TakesInputFocus(): TslControl; override;
    function KeyEvent(c: char; extended: boolean): boolean; override;
    property OnClick: TslEvent Read fOnClick Write fOnClick;
    function Width: integer; override;
  end;

  TslTextBox = class(TslAlignedControl)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  public
    updateing: boolean;
    fText:     TStringList;
    maxlines:  integer;
    procedure AddLine(const s: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadFromFile(filename: string);
    constructor Create(Width, Height: integer; parent: TslControl); overload;
    destructor Destroy; override;
    procedure Repaint; override;
    function MaxWidth: integer; override;
    function MaxHeight: integer; override;
    property Text: string Read GetText Write SetText;
  end;


  TslHorizontalScrollbar = class;
  TslVerticalScrollbar   = class;

  TslScrollArea = class(TslBackGround)
    // ennek kell lennie maxwidth maxheight szamolasnak
  private
    fMaxHeight, fMaxWidth: integer;
  protected
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
  public
    hScrollBar: TslHorizontalScrollbar;
    vScrollBar: TslVerticalScrollbar;
    procedure GotoXy(ca: TslRect; x, y: integer); override;
    procedure Write(ca: TslRect; const x, y: integer; s: string;
      hossz: integer = 0); override;
    function MaxWidth: integer;
    function MaxHeight: integer;
    function Width: integer;
    function Height: integer;
    procedure SizeChanged;
  end;

  TslScrollbar = class(TslAlignedControl)
  private
    fPosition: integer;
    atmax:     boolean;
    procedure SetPosition(Value: integer);
  public
    noatmax:    boolean;
    scrollArea: TslScrollArea;
    constructor Create(Width, Height: integer; parent: TslControl); overload;
    property Position: integer Read fPosition Write SetPosition;
  end;

  TslHorizontalScrollbar = class(TslScrollbar)
    procedure SetColors; override;
    constructor Create(parent: TslControl);
    function Width: integer; override;
    procedure Repaint; override;
    function KeyEvent(c: char; extended: boolean): boolean; override;
  end;

  TslVerticalScrollbar = class(TslScrollbar)
    procedure SetColors; override;
    constructor Create(parent: TslControl);
    function Height: integer; override;
    procedure Repaint; override;
    function KeyEvent(c: char; extended: boolean): boolean; override;
  end;

  TslScrollControl = class(TslAlignedControl)
    // ez csinal ket scrollbart
  private
    rest: TslScrollArea;
    hScrollBar: TslHorizontalScrollbar;
    vScrollBar: TslVerticalScrollbar;
  public
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    constructor Create(Width, Height: integer; parent: TslControl); overload;
    procedure AddControl(control: TslControl); override;
    function KeyEvent(c: char; extended: boolean): boolean; override;
  end;


  TslFrame = class(TslControl)
  public
    procedure Repaint; override;
  end;

  TslWindow = class(TslScrollControl)
  private
    fTitle: TslLabel;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    modal: boolean;
    modalResult: TslModalResult;
    function TakesInputFocus(): TslControl; override;
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    procedure SetColors; override;
    constructor Create(Width, Height: integer; title: string; parent: TslControl);
    function ShowModal: TslModalResult;
    property Title: string Read GetTitle Write SetTitle;
  end;

  TslCommandWindow = class(TslWindow)
  private
    rest2: TslControl;
  public
    textbox:     TslTextBox;
    commandedit: TslCommandEdit;

    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    constructor Create(Width, Height: integer; title, Caption: string;
      parent: TslControl);
  end;

  TslAlignedPanel = class(TslControl)
  protected
    fAlignedPanel: TslControl;
    rest: TslControl;
  public
    procedure AddControl(control: TslControl); override;
  end;

  TslHorizontalAlignedPanel = class(TslAlignedPanel)
  protected
    fAlign: TslHorizontalAlignment;
  public
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    constructor Create(panel: TslFixWidthPanel; alignment: TslHorizontalAlignment;
      parent: TslControl); overload;
    constructor Create(panel: TslFixWidthPanel; alignment: TslHorizontalAlignment;
      rest, parent: TslControl); overload;
    property Alignment: TslHorizontalAlignment Read fAlign;
  end;

  TslVerticalAlignedPanel = class(TslAlignedPanel)
  protected
    fAlign: TslVerticalAlignment;
  public
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    constructor Create(panel: TslFixHeightPanel; alignment: TslVerticalAlignment;
      parent: TslControl); overload;
    constructor Create(panel: TslFixHeightPanel; alignment: TslVerticalAlignment;
      rest, parent: TslControl); overload;
    property Alignment: TslVerticalAlignment Read fAlign;
  end;



  TslMenubar = class(TslFixHeightPanel)
  protected
    procedure SetColors; override;
  public
    constructor Create(parent: TslControl);
    procedure Repaint; override;
  end;

  TslMutualVisibilityControl = class(TslBackground)
  private
    visibleControl: TslControl;
  public
    function KeyEvent(c: char; extended: boolean): boolean; override;
    procedure VisibilityChanged(c: TslControl); override;
    procedure AddControl(control: TslControl); override;
  end;

  TslApplication = class(TslControl)
  private
    fOnExit: TslEvent;
    fOnShow: TslEvent;
    fLocalConsoleTasks: TObjectList;
    fConsoleTasks: TObjectList;
    fLocalConsoleToUpdate: TStringList;
    fConsoleToUpdate: TStringList;
    procedure slOnResize;
    procedure slOnCtrlC;
    procedure ShowMessage(const s: string);
    procedure CopyConsoleTasks;
    function InputQuery(const title, Caption: string; var reply: string;
      password: boolean = False): boolean;
  protected
    procedure SetColors; override;
    function BackgroundCharacter: char; override;
  public
    shouldquit: boolean;
    l_clock:    TslLabel;
    l_uptime:   TslLabel;
    l_infos:    TslLabel;
    timers:     TObjectList;
    menubartop: TslMenubar;
    menubarbottom: TslMenubar;
    procedure Write(ca: TslRect; const x, y: integer; s: string;
      hossz: integer = 0); override;
    procedure GetClientArea(ca: TslRect; whowantstoknow: TslControl); override;
    destructor Destroy; override;
    constructor Create; overload;
    constructor Create(Width, Height: integer); overload;
    procedure Run;
    procedure Repaint; override;
    procedure ProcessMessages;
    procedure GotoXy(ca: TslRect; x, y: integer); override;
    procedure AddConsoleTask(t: TslConsoleTask);
    property OnExit: TslEvent Read fOnExit Write fOnExit;
    property OnShow: TslEvent Read fOnShow Write fOnShow;
  end;

  TslRemoveEarlierTask = class(TslConsoleTask)
    procedure OnConleTaskAdded(v_queue: TObjectList); override;
  end;

  TslRepaintTask = class(TslRemoveEarlierTask)
    procedure Execute; override;
  end;

  TslTextBoxTask = class(TslConsoleTask)
  public
    remove:      boolean;
    textbox:     TslTextBox;
    windowTitle: string;
    constructor Create(windowTitle: string; textbox: TslTextBox);
  end;

  TslClockTimer = class(TslTimer)
  private
    l_clock: TslLabel;
  public
    constructor Create(l_clock: TslLabel);
    procedure OnTimer; override;
  end;

  TslUptimeTimer = class(TslTimer)
  private
    l_uptime: TslLabel;
  public
    constructor Create(l_uptime: TslLabel);
    procedure OnTimer; override;
  end;

  TslInfosTimer = class(TslTimer)
  private
    l_infos: TslLabel;
  public
    constructor Create(l_infos: TslLabel);
    procedure OnTimer; override;
  end;


procedure ShowMessage(const s: string);
function InputQuery(const title, Caption: string; var reply: string;
  password: boolean = False; replyfile: string = ''): boolean;
procedure SetFocus(control: TslControl);

var
  slVisionFrequency: integer = 50;
  slVisionThreadFrequency: integer = 4; // 4 * 50
  slApp: TslApplication = nil;
  slvision_lock: TCriticalSection;

implementation

uses slconsole, SysUtils, DateUtils, Math
{$IFDEF MSWINDOWS}
  , Clipbrd
{$ENDIF}
  , debugunit, mystrings, mainthread, configunit, kb, irc, rulesunit,
  speedstatsunit, ranksunit, notify;

var
  slig, lvtf: integer;


procedure ShowMessage(const s: string);
begin
  if slApp <> nil then
    slApp.ShowMessage(s);
end;

function ReadFile(const filename: string): string;
var
  f: TextFile;
  s: string;
begin
  Result := '';
  AssignFile(f, filename);
  reset(f);
  while not EOF(f) do
  begin
    ReadLn(f, s);
    Result := Result + s + #13#10;
  end;
  CloseFile(f);
end;

function InputQuery(const title, Caption: string; var reply: string;
  password: boolean = False; replyfile: string = ''): boolean;
begin
  Result := False;

  if ((replyfile <> '') and (FileExists(replyfile))) then
  begin
    reply  := Trim(ReadFile(replyfile));
    Result := True;
    exit;
  end;

  if slApp <> nil then
    Result := slApp.InputQuery(title, Caption, reply, password);
end;

{ TslApplication }

constructor TslApplication.Create;
begin
  Create(slscreen.Getwidth, slscreen.GetHeight);
end;

constructor TslApplication.Create(Width, Height: integer);
begin
  slApp  := self;
  timers := TObjectList.Create;

  slscreen.SetResolution(Width, Height);
  inherited Create(nil);
  slscreen.OnResize := slOnResize;
  slscreen.OnCtrlC  := slOnCtrlC;

  menubarbottom := TslMenubar.Create(nil);

  menubartop := TslMenubar.Create(nil);
  TslVerticalAlignedPanel.Create(menubartop, slVATop,
    TslVerticalAlignedPanel.Create(menubarbottom, slVABottom, nil), self);



  l_clock  := TslLabel.Create('', menubartop);
  l_clock.Right := 2;
  l_uptime := TslLabel.Create('', menubartop);
  l_uptime.Left := 2;
  l_infos  := TslLabel.Create('', menubarbottom);
  l_infos.Right := 2;

  fLocalConsoleTasks := TObjectList.Create(True);
  fConsoleTasks      := TObjectList.Create(False);
  fLocalConsoleToUpdate := TStringList.Create();
  fConsoleToUpdate   := TStringList.Create();
  fConsoleToUpdate.Duplicates := dupIgnore;
  fConsoleToUpdate.Sorted := True;


  timers.Add(TslClockTimer.Create(l_clock));
end;

procedure TslApplication.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  ca.Left   := 1;
  ca.Right  := slScreen.GetWidth;
  ca.Top    := 1;
  ca.Bottom := slScreen.GetHeight;
end;

procedure TslApplication.Repaint;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;

  slscreen.ClrScr;

  inherited;
end;

procedure TslApplication.ProcessMessages;
var
  c: char;
  extended: boolean;
  i: integer;
  k: boolean;
  t: TslConsoleTask;
begin
  Inc(lvtf);
  if lvtf >= slVisionThreadFrequency then
  begin
    lvtf := 0;
    try
      slvision_lock.Enter();
      try
        CopyConsoleTasks;

        for i := 0 to fLocalConsoleToUpdate.Count - 1 do
        begin
          try
            if i > fLocalConsoleToUpdate.Count then
              Break;
          except
            Break;
          end;
          TslTextBox(fLocalConsoleToUpdate.Objects[i]).BeginUpdate;
        end;

        while (fLocalConsoleTasks.Count > 0) do
        begin
          try
            t := TslConsoleTask(fLocalConsoleTasks[0]);
            t.Execute;
            fLocalConsoleTasks.Remove(t);
          except
            Break;
          end;
        end;

        for i := 0 to fLocalConsoleToUpdate.Count - 1 do
        begin
          try
            if i > fLocalConsoleToUpdate.Count then
              Break;
          except
            Break;
          end;
          TslTextBox(fLocalConsoleToUpdate.Objects[i]).EndUpdate;
        end;

        fLocalConsoleToUpdate.Clear;
      finally
        slvision_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, 'slvision',
          Format('[EXCEPTION] TslApplication.ProcessMessages: %s', [e.Message]));
        fLocalConsoleToUpdate.Clear;
        Exit;
      end;
    end;
  end;

  try
    k := slScreen.keypressed;

    if k then
    begin
      extended := False;
      c := slScreen.ReadKey;
      if c = #0 then
      begin
        c := slScreen.ReadKey;
        extended := True;
      end;

      if ((extended) and (c = #45)) then // alt+x
      begin
        shouldquit := True;
        exit;
      end;
      try
        KeyEvent(c, extended);
      except
        on E: Exception do
          Debug(dpError, 'slvision', Format(
            '[EXCEPTION] TslApplication.ProcessMessages.KeyEvent: %s', [e.Message]));
      end;

    end;

    try
      for i := 0 to timers.Count - 1 do
      begin
        TslTimer(timers[i]).Tick;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, 'slvision',
          Format('[EXCEPTION] TslApplication.ProcessMessages TslTimer: %s', [e.Message]));
      end;
    end;

    if not k then
    begin
      Sleep(slVisionFrequency);
    end;

  except
    on E: Exception do
      Debug(dpError, 'slvision', Format(
        '[EXCEPTION] TslApplication.ProcessMessages.slScreen.keypressed: %s',
        [e.Message]));
  end;

end;

procedure TslApplication.Run;
begin
  Visible := slvVisible;

  try
    if assigned(fOnShow) then
      fOnShow(self);
  except
    on E: Exception do
      Debug(dpError, 'slvision', '[EXCEPTION] Run.assigned(fOnShow) %s',
        [e.Message]);
  end;


  while not shouldquit do
  begin
    try
      ProcessMessages;
    except
      on e: Exception do
      begin
        Debug(dpError, 'slvision', '[EXCEPTION] Run %s', [e.Message]);
      end;
    end;
  end;

  if Assigned(fOnExit) then
    fOnExit(self);
end;

procedure TslApplication.SetColors;
begin
  slscreen.NormVideo;
end;


procedure TslApplication.slOnCtrlC;
begin
  shouldquit := True;
end;

procedure TslApplication.slOnResize;
begin
  Repaint;
end;

destructor TslApplication.Destroy;
begin
  timers.Free;
  CopyConsoleTasks;
  fConsoleTasks.Free;
  fLocalConsoleTasks.Free;
  fConsoleToUpdate.Free;
  fLocalConsoleToUpdate.Free;

  inherited;
end;

procedure TslApplication.Write(ca: TslRect; const x, y: integer;
  s: string; hossz: integer);
var
  mx, vx: integer;
begin
  if hossz = 0 then
    hossz := Length(s);
  if (x > ca.Width) then
    exit;
  if ((y > ca.Height) or (y <= 0)) then
    exit;

  vx := x;
  if vx <= 0 then
  begin
    Delete(s, 1, abs(x) + 1);
    Dec(hossz, abs(x) + 1);
    vx := 1;
  end;

  mx := ca.Width;

  {$IFDEF MSWINDOWS}
  if ca.Top + y - 1 >= slscreen.GetHeight then
  begin
    if ca.Left + mx - 1 >= slscreen.GetWidth then
      Dec(mx);
  end;
  {$ENDIF}

  if hossz > mx - vx + 1 then
    s := Copy(s, 1, mx - vx + 1);
  if s = '' then
    exit;

  slscreen.gotoxy(ca.left + vx - 1, ca.top + y - 1);
  slscreen.Write(s);
end;

function TslApplication.BackgroundCharacter: char;
begin
{$IFDEF MSWINDOWS}
  Result := Chr(176);
{$ELSE}
  Result:= ' ';
{$ENDIF}
end;

procedure TslApplication.ShowMessage(const s: string);
var
  l: TslLabel;
  w: TslWindow;
begin
  l     := TslLabel.Create(s, nil);
  l.Top := 2;

  w := TslWindow.Create(l.Width + 8, l.Height + 6, 'Message', nil);
  w.Visible := slvHidden;

  l.SetParent(w);



  with TslButton.Create(mrOk, w) do
    Bottom := 2;

  //  dump;

  w.ShowModal;
  w.Free;
end;


procedure TslApplication.GotoXy(ca: TslRect; x, y: integer);
begin
  slScreen.GotoXY(ca.Left + x - 1, ca.Top + y - 1);
end;

function TslApplication.InputQuery(const title, Caption: string;
  var reply: string; password: boolean): boolean;
var
  w: TslWindow;
  m: integer;
  e: TslEdit;
begin
  m := 20; // ez a min
  if (length(title) > m) then
    m := length(title);
  if (length(Caption) > m) then
    m := length(Caption);

  w := TslWindow.Create(m + 4, 8, title, nil);
  with TslLabel.Create(Caption, w) do
  begin
    Left := 2;
    Top  := 2;
  end;


  with TslButton.Create(mrOk, w) do
  begin
    Left   := 4;
    Bottom := 2;
  end;

  with TslButton.Create(mrCancel, w) do
  begin
    Right  := 4;
    Bottom := 2;
  end;

  e := TslEdit.Create(2, 3, m, w);
  if password then
    e.passwordchar := '*';

  Result := mrOk = w.showmodal;
  reply  := e.Text;
  w.Free;
end;

procedure TslApplication.AddConsoleTask(t: TslConsoleTask);
var
  i: integer;
begin
  try
    slvision_lock.Enter();
    try

      try
        fConsoleTasks.Add(t);
      except
        on E: Exception do
          Debug(dpError, 'slvision', Format(
            '[EXCEPTION] TslApplication.AddConsoleTask.fConsoleTasks.Add: %s',
            [e.Message]));
      end;

      try
        t.OnConleTaskAdded(fConsoleTasks);
      except
        on E: Exception do
          Debug(dpError, 'slvision', Format(
            '[EXCEPTION] TslApplication.AddConsoleTask.t.OnConleTaskAdded: %s',
            [e.Message]));
      end;
      if t is TslTextBoxTask then
      begin
        with TslTextBoxTask(t) do
          if textbox <> nil then
          begin
            if remove then
            begin
              i := fConsoleToUpdate.IndexOf(windowTitle);
              if i >= 0 then
              begin
                try
                  fConsoleToUpdate.Delete(i);
                except
                  on E: Exception do
                    Debug(dpError, 'slvision',
                      Format(
                      '[EXCEPTION] TslApplication.AddConsoleTask.fConsoleToUpdate.Delete: %s',
                      [e.Message]));
                end;
              end;
            end
            else
            begin
              try
                fConsoleToUpdate.AddObject(windowTitle, textBox);
              except
                on E: Exception do
                  Debug(dpError, 'slvision',
                    Format(
                    '[EXCEPTION] TslApplication.AddConsoleTask.fConsoleToUpdate.AddObject: %s',
                    [e.Message]));
              end;

            end;
          end;
      end;
    finally
      slvision_lock.Leave();
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slvision',
        Format('[EXCEPTION] TslApplication.AddConsoleTask: %s', [e.Message]));
    end;
  end;
end;

procedure TslApplication.CopyConsoleTasks;
begin
  try
    fLocalConsoleTasks.Assign(fConsoleTasks);
    fConsoleTasks.Clear;
    fLocalConsoleToUpdate.Assign(fConsoleToUpdate);
    fConsoleToUpdate.Clear;
  except
    on e: Exception do
    begin
      fConsoleTasks.Clear;
      fConsoleToUpdate.Clear;
      fLocalConsoleTasks.Clear;
      fLocalConsoleToUpdate.Clear;
      Debug(dpError, 'slvision',
        Format('[EXCEPTION] TslApplication.CopyConsoleTasks: %s', [e.Message]));
    end;
  end;
end;

{ TslControl }

procedure TslControl.AddControl(control: TslControl);
var
  c: TslAlignedPanel;
begin
  control.fParent := self;
  if ((children.Count = 1) and (children[0] is TslAlignedPanel)) then
  begin
    c := TslAlignedPanel(children[0]);
    c.AddControl(control);
  end
  else
  begin
    children.Add(control);
    SetFocus(control);
  end;

end;

constructor TslControl.Create(parent: TslControl);
begin
  children := TObjectList.Create(False);
  Inc(slig);
  sli := slig;
  ca  := TslRect.Create(0, 0, 0, 0);
  SetParent(parent);
end;


procedure TslControl.DelControl(control: TslControl);
begin
  try
    children.Remove(control);
    if focus = control then
      focus := nil;
  except
    exit;
  end;
end;

destructor TslControl.Destroy;
begin
  SetParent(nil);
  while (children.Count > 0) do
    children[0].Free;
  children.Free;
  ca.Free;
  inherited;
end;

procedure TslControl.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  if parent <> nil then
    parent.GetClientArea(ca, self);
end;

function TslControl.TabKey: boolean;
var
  i, j: integer;
  control, w: TslControl;
begin
  Result := False;

  i := children.IndexOf(focus);
  for j := i + 1 to children.Count - 1 do
  begin
    control := TslControl(children[j]);
    w := control.TakesInputFocus;
    if w <> nil then
    begin
      SetFocus(w);
      Result := True;
      exit;
    end;
  end;

  for j := 0 to i do
  begin
    control := TslControl(children[j]);
    w := control.TakesInputFocus;
    if w <> nil then
    begin
      SetFocus(w);
      Result := True;
      exit;
    end;
  end;

end;

function TslControl.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := False;

  if Visible <> slvVisible then
    exit;


  if focus <> nil then
    Result := focus.KeyEvent(c, extended);

  if Result then
    exit;

  // tab lekezelese
  if c = #9 then
  begin
    Result := TabKey();
    if Result then
      exit;
  end;

end;

procedure TslControl.LostFocus();
begin
  if focused then
  begin
    focused := False;
    if focus <> nil then
      focus.LostFocus;
  end
  else
    Repaint;
end;

procedure TslControl.Repaint;
var
  i: integer;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;

  for i := 0 to children.Count - 1 do
    TslControl(children[i]).Repaint;
end;

procedure TslControl.SetColors;
begin
  if parent <> nil then
    parent.SetColors;
end;

procedure SetFocus(control: TslControl);
var
  prev: TslControl;
  i:    integer;
begin
  if control = nil then
    exit;
  if control.ffVisible = slvHidden then
    exit;
  control := control.TakesInputFocus;
  if nil = control then
    exit;

  control.app.LostFocus;


  prev := nil;
  while (True) do
  begin
    control.focused := True;
    control.focus   := prev;
    if prev = nil then
      control.Repaint
    else
    begin
      i := control.children.IndexOf(prev);
      if ((i <> -1) and (i <> control.children.Count - 1)) then
        control.children.Move(i, control.children.Count - 1);
    end;

    if control.Parent = nil then
      Break;
    prev    := control;
    control := control.Parent;
  end;

end;

procedure TslControl.SetParent(control: TslControl);
begin
  if fparent <> nil then
    fparent.DelControl(self);

  if control <> nil then
    control.AddControl(self)
  else
    fParent := nil;
end;

procedure TslControl.SetVisible(Value: TslVisible);
begin
  if Value <> ffvisible then
  begin
    ffvisible := Value;

    if ffVisible = slvVisible then
      SetFocus(FocusedControl);

    if parent <> nil then
      parent.VisibilityChanged(self)
    else
      Repaint;
  end;
end;

function TslControl.TakesInputFocus(): TslControl;
var
  c: TslControl;
  i: integer;
begin
  Result := nil;
  for i := 0 to children.Count - 1 do
  begin
    c      := TslControl(children[i]);
    Result := c.TakesInputFocus();
    if Result <> nil then
      exit;
  end;
end;

procedure TslControl.VisibilityChanged(c: TslControl);
begin
  Repaint;
end;

function TslControl.Name: string;
begin
  Result := ClassName;
  if focused then
    Result := Result + ' F';
  if focus <> nil then
    Result := Result + ' (#' + IntToStr(focus.sli) + ')';
end;

procedure TslControl.Dump(prefix: string);
var
  i: integer;
  c: TslControl;
begin
  WriteLn(Format('%5s %s%s', ['#' + IntToStr(sli), prefix, Name]));
  for i := 0 to children.Count - 1 do
  begin
    c := TslControl(children[i]);
    c.Dump(prefix + '  ');
  end;
end;

procedure TslControl.Write(ca: TslRect; const x, y: integer; s: string;
  hossz: integer = 0);
begin
  if parent <> nil then
    parent.Write(ca, x, y, s, hossz);
end;

procedure TslControl.Write(const x, y: integer; s: string; hossz: integer = 0);
begin
  Write(ca, x, y, s, hossz);
end;

procedure TslControl.Write(const x, y: integer; c: char);
begin
  Write(x, y, c, 1);
end;


function TslControl.GetVisible: TslVisible;
begin
  Result := ffVisible;
  if ffVisible = slvParent then
  begin
    if parent <> nil then
      Result := parent.Visible
    else
      Result := slvHidden;
  end;
end;


function TslControl.BackgroundCharacter: char;
begin
  if parent <> nil then
    Result := parent.BackgroundCharacter
  else
    Result := ' ';
end;

function TslControl.FocusedControl: TslControl;
begin
  Result := self;
  if ((focus <> nil)) then//(focused) and 
    Result := focus.FocusedControl;
end;

function TslControl.app: TslControl;
begin
  Result := self;
  if parent <> nil then
    Result := parent.app;
end;

procedure TslControl.GotoXy(ca: TslRect; x, y: integer);
begin
  if parent <> nil then
    parent.GotoXy(ca, x, y);
end;

procedure TslControl.GotoXy(x, y: integer);
begin
  GotoXy(ca, x, y);
end;

{ TslAlignedPanel }

procedure TslAlignedPanel.AddControl(control: TslControl);
begin
  if rest <> nil then
    rest.AddControl(control)
  else
    inherited;
end;

constructor TslHorizontalAlignedPanel.Create(panel: TslFixWidthPanel;
  alignment: TslHorizontalAlignment; parent: TslControl);
begin
  inherited Create(parent);

  fAlign := alignment;
  fAlignedPanel := panel;
  panel.SetParent(self);
  rest := TslMainBackground.Create(self);
end;

constructor TslHorizontalAlignedPanel.Create(panel: TslFixWidthPanel;
  alignment: TslHorizontalAlignment; rest, parent: TslControl);
begin
  inherited Create(parent);

  fAlign := alignment;
  fAlignedPanel := panel;
  panel.SetParent(self);
  rest.SetParent(self);
  self.rest := rest;
end;

procedure TslHorizontalAlignedPanel.GetClientArea(ca: TslRect;
  whowantstoknow: TslControl);
begin
  inherited GetClientArea(ca, whowantstoknow);
  if whowantstoknow = fAlignedPanel then
  begin
    case fAlign of
      slHALeft: ca.Right := ca.Left + TslFixWidthPanel(fAlignedPanel).fixedWidth - 1;
      slHARight: ca.Left := ca.Right - TslFixWidthPanel(fAlignedPanel).fixedWidth + 1;
    end;
  end
  else
  begin
    case fAlign of
      slHALeft: ca.Left   := ca.Left + TslFixWidthPanel(fAlignedPanel).fixedWidth;
      slHARight: ca.Right := ca.Right - TslFixWidthPanel(fAlignedPanel).fixedWidth;
    end;
  end;

end;




{ TslMenubar }

constructor TslMenubar.Create(parent: TslControl);
begin
  inherited Create(1, parent);
end;

procedure TslMenubar.Repaint;
var
  i: integer;
  s: string;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;

  GetClientArea(ca, self);

  s := '';
  for i := 1 to ca.Width do
    s := s + ' ';


  for i := 1 to ca.Height do
  begin
    Write(1, i, s);
  end;


  inherited;

end;

procedure TslMenubar.SetColors;
begin
  slScreen.TextColor(slcBlack);
  slScreen.TextBackground(slcLightGray);
end;

{ TslFixWidthPanel }

constructor TslFixWidthPanel.Create(Width: integer; parent: TslControl);
begin
  fixedWidth := Width;
  inherited Create(parent);
end;

{ TslFixHeightPanel }

constructor TslFixHeightPanel.Create(Height: integer; parent: TslControl);
begin
  fixedHeight := Height;
  inherited Create(parent);
end;

{ TslFixPanel }

function TslAlignedControl.BackgroundCharacter: char;
begin
  Result := ' ';
end;

constructor TslAlignedControl.Create(Width, Height: integer; parent: TslControl);
begin
  fixedWidth  := Width;
  fixedHeight := Height;
  inherited Create(parent);
end;

{ TslVerticalAlignedPanel }

constructor TslVerticalAlignedPanel.Create(panel: TslFixHeightPanel;
  alignment: TslVerticalAlignment; parent: TslControl);
begin
  inherited Create(parent);

  fAlign := alignment;
  fAlignedPanel := panel;
  panel.SetParent(self);
  rest := TslMainBackground.Create(self);
end;

constructor TslVerticalAlignedPanel.Create(panel: TslFixHeightPanel;
  alignment: TslVerticalAlignment; rest, parent: TslControl);
begin
  inherited Create(parent);

  fAlign := alignment;
  fAlignedPanel := panel;
  panel.SetParent(self);
  rest.SetParent(self);
  self.rest := rest;
end;


procedure TslVerticalAlignedPanel.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  inherited GetClientArea(ca, whowantstoknow);
  if whowantstoknow = fAlignedPanel then
  begin
    case fAlign of
      slVATop: ca.Bottom := ca.Top + TslFixHeightPanel(fAlignedPanel).fixedHeight - 1;
      slVABottom: ca.Top := ca.Bottom - TslFixHeightPanel(fAlignedPanel).fixedHeight + 1;
    end;
  end
  else
  begin
    case fAlign of
      slVATop: ca.Top := ca.Top + TslFixHeightPanel(fAlignedPanel).fixedHeight;
      slVABottom: ca.Bottom := ca.Bottom - TslFixHeightPanel(fAlignedPanel).fixedHeight;
    end;
  end;
end;

{ TslLabel }

constructor TslLabel.Create(Caption: string; parent: TslControl);
begin
  fCaption := TStringList.Create;
  inherited Create(Length(Caption), 1, parent);

  self.Caption := Caption;
end;

destructor TslLabel.Destroy;
begin
  fCaption.Free;
  inherited;
end;

function TslLabel.GetCaption: string;
begin
  Result := fCaption.Text;
end;

procedure TslLabel.Repaint;
var
  i: integer;
  w: integer;
begin
  if Visible <> slvVisible then
    exit;

  SetColors; // ez beallitja hatterszint meg ilyesmit

  GetClientArea(ca, self);

  w := 1;
  for i := 0 to fCaption.Count - 1 do
  begin
    case Alignment of
      sltaCenter: w := (ca.Width - Length(fcaption[i])) div 2 + 1;
      sltaRight: w  := ca.Width - Length(fcaption[i]) + 1;
    end;

    Write(w, i + 1, fcaption[i]);
  end;

end;

procedure TslLabel.SetCaption(Value: string);
var
  lx, ly, i: integer;
begin
  if fcaption.Text = Value then
    exit;

  fCaption.Text := Value;
  lx := 0;
  for i := 0 to fCaption.Count - 1 do
    if length(fcaption[i]) > lx then
      lx := length(fcaption[i]);

  ly := fCaption.Count;

  if ((lx <> fixedWidth) or (ly <> fixedHeight)) then
  begin
    fixedWidth  := lx;
    fixedHeight := ly;
    SizeChanged;
    if parent <> nil then
      parent.Repaint;
  end
  else
    Repaint;
(*
  {$IFDEF MSWINDOWS}
    Repaint;
  {$ELSE}
  if parent <> nil then
    parent.Repaint;
  {$ENDIF}
*)
end;


procedure TslAlignedControl.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
var
  r: TslRect;
  d: integer;
begin
  inherited GetClientArea(ca, self);

  r := TslRect.Create(0, 0, 0, 0);

  if (((Left <> 0) and (Right <> 0)) or ((Left <= 0) and (Right <= 0))) then
  begin
    // horizontalis kozepre igazitas.

    d      := (ca.Width - Width) div 2;
    r.Left := ca.Left + d;
    r.Right := r.Left + Width - 1;
  end
  else
  if Left <> 0 then
  begin
    // horizontalis balra igazitas
    r.Left  := ca.Left + Left - 1;
    r.Right := r.Left + Width - 1;
  end
  else
  begin
    // horizontalis jobbra igazitas
    r.Right := ca.Right - Right + 1;
    r.Left  := r.Right - Width + 1;
  end;

  if r.Left < ca.Left then
    r.Left := ca.Left;
  if r.Right > ca.Right then
    r.Right := ca.Right;

  // most vertikalisan ugyanez
  if (((Top <> 0) and (Bottom <> 0)) or ((Top <= 0) and (Bottom <= 0))) then
  begin
    // horizontalis kozepre igazitas.


    d     := (ca.Height - Height) div 2;
    r.Top := ca.Top + d;
    r.Bottom := r.Top + Height - 1;
  end
  else
  if Top <> 0 then
  begin
    // horizontalis felulre igazitas
    r.Top    := ca.Top + Top - 1;
    r.Bottom := r.Top + Height - 1;
  end
  else
  begin
    // horizontalis alulra igazitas
    r.Bottom := ca.Bottom - Bottom + 1;
    r.Top    := r.Bottom - Height + 1;
  end;

  if r.Top < ca.Top then
    r.Top := ca.Top;
  if r.Bottom > ca.Bottom then
    r.Bottom := ca.Bottom;

  ca.Assign(r);

  r.Free;
end;



{ TslBackground }

procedure TslBackground.Repaint;
var
  s: string;
  i: integer;
  c: char;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;
  GetclientArea(ca, self);

  s := '';
  c := BackgroundCharacter;
  for i := 1 to ca.Width do
    s := s + c;

  if self is TslScrollArea then
    directAddressing := True;

  for i := 1 to ca.Height do
  begin
    Write(1, i, s);
  end;
  if self is TslScrollArea then
    directAddressing := False;

  inherited;

end;



{ TslTimer }


procedure TslTimer.Tick;
begin
  if interval = 0 then
    exit;

  if (lastFire <> 0) and (MilliSecondsBetween(Now, lastFire) < Interval) then
    exit;

  lastFire := Now;
  OnTimer();
end;

{ TslClockTimer }

constructor TslClockTimer.Create(l_clock: TslLabel);
begin
  self.l_clock := l_clock;
  interval     := 1000;
  inherited Create;
end;

procedure TslClockTimer.OnTimer;
var
  x, y: integer;
begin
  slScreen.GetCursorPos(x, y);
  l_clock.Caption := FormatDateTime('hh:nn:ss', Now);
  slScreen.GotoXY(x, y);
end;

{ TslUptimeTimer }

constructor TslUptimeTimer.Create(l_uptime: TslLabel);
begin
  self.l_uptime := l_uptime;
  interval      := 1000;
  inherited Create;
end;

procedure TslUptimeTimer.OnTimer;
var
  x, y: integer;
begin
  slScreen.GetCursorPos(x, y);
  try
    l_uptime.Caption := Format('UP: %s', [DateTimeAsString(started, True)]);
  except
    // nothing
  end;
  slScreen.GotoXY(x, y);
end;

{ TslInfosTimer }

constructor TslInfosTimer.Create(l_infos: TslLabel);
begin
  self.l_infos := l_infos;
  interval     := 1000;
  inherited Create;
end;

procedure TslInfosTimer.OnTimer;
var
  x, y:    integer;
  i, i_chans: integer;
  s_rules: string;
begin
  slScreen.GetCursorPos(x, y);
  try
    i_chans := 0;
    for i := 0 to myIrcThreads.Count - 1 do
    begin
      i_chans := i_chans + TMyIrcThread(myIrcThreads[i]).channels.Count;
    end;

    s_rules := '';
    if rules.Count > 0 then
    begin
      if s_rules <> '' then
        s_rules := s_rules + ', ';
      s_rules := s_rules + Format('%d rules', [rules.Count]);
    end;
    if rtpl.Count > 0 then
    begin
      if s_rules <> '' then
        s_rules := s_rules + ', ';
      s_rules := s_rules + Format('%d rtpl', [rtpl.Count]);
    end;

    l_infos.Caption := Format(
      'KB: %d / IRC: %d net, %d chan / Rules: %s / Stats: %d speed, %d ranks',
      [kb_list.Count, myIrcThreads.Count, i_chans, s_rules, speedstats.Count,
      ranks.Count]);
  except
    // dont know
  end;
  slScreen.GotoXY(x, y);
end;

{ TslWindow }


constructor TslWindow.Create(Width, Height: integer; title: string; parent: TslControl);
var
  tmp: TslScrollArea;
begin
  inherited Create(Width, Height, parent);

  tmp  := rest;
  rest := nil;

  TslFrame.Create(self);
  children.Move(children.Count - 1, 0);

  fTitle     := TslLabel.Create(' ' + title + ' ', self);
  fTitle.Top := 1;
  children.Move(children.Count - 1, 1);

  rest := tmp;

  hScrollBar.Left := 2;
  vScrollBar.Top  := 2;
end;


procedure TslWindow.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  inherited GetClientArea(ca, whowantstoknow);

  if whowantstoknow = rest then
  begin
    ca.Top  := ca.Top + 1;
    ca.Left := ca.Left + 1;
  end;
end;

function TslWindow.GetTitle: string;
begin
  Result := Trim(fTitle.Caption);
end;

procedure TslFrame.Repaint;
{$IFDEF MSWINDOWS}
const
  frameChars: array[False..True, 1..6] of char = (
    (chr(196), chr(179), chr(218), chr(191), chr(192), chr(217)),
    (chr(205), chr(186), chr(201), chr(187), chr(200), chr(188))
    );
 {$ELSE}
const frameChars : array[false..true, 1..6] of char = (
    ('-', '|', ',', '.',  '`', '´'),
    ('-', '|', ',', '.',  '`', '´')
 );
{$ENDIF}

var
  i: integer;
  s: string;
  focused: boolean;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;
  GetClientArea(ca, self);

  focused := parent.focused;

  s := '';
  for i := 2 to ca.Width - 1 do
    s := s + frameChars[focused, 1];

  Write(2, 1, s);
  Write(2, ca.Height, s);

  for i := 2 to ca.Height - 1 do
  begin
    Write(1, i, frameChars[focused, 2]);
    Write(ca.Width, i, frameChars[focused, 2]);
  end;

  Write(1, 1, frameChars[focused, 3]);
  Write(ca.Width, 1, frameChars[focused, 4]);
  Write(1, ca.Height, frameChars[focused, 5]);
  Write(ca.Width, ca.Height, frameChars[focused, 6]);

end;


procedure TslWindow.SetColors;
begin
{$IFDEF MSWINDOWS}
  slScreen.TextColor(slcWhite);
  slScreen.TextBackground(slcBlue);
{$ELSE}
  slScreen.NormVideo;
{$ENDIF}
end;


function TslAlignedControl.Height: integer;
begin
  inherited GetClientArea(ca, self);
  Result := fixedHeight;
  if Result = 0 then
    Result := ca.Height;

end;


function TslAlignedControl.MaxHeight: integer;
begin
  Result := fixedHeight;
  if Top > 1 then
    Inc(Result, Top - 1);
end;

function TslAlignedControl.MaxWidth: integer;
begin
  Result := fixedWidth;
  if Left > 1 then
    Inc(Result, Left - 1);
end;


procedure TslAlignedControl.SizeChanged;
begin
  if ((parent <> nil) and (parent is TslScrollArea)) then
    TslScrollArea(parent).SizeChanged;
end;

function TslAlignedControl.Width: integer;
begin
  inherited GetClientArea(ca, self);
  Result := fixedWidth;
  if Result = 0 then
    Result := ca.Width;
end;

procedure TslWindow.SetTitle(const Value: string);
begin
  fTitle.Caption := ' ' + Value + ' ';
end;



function TslWindow.ShowModal: TslModalResult;
var
  pf: TslControl;
begin
  pf := slApp.FocusedControl;

  if parent = nil then
    SetParent(slApp);

  Visible := slvVisible;
  modal   := True;
  modalResult := mrNone;
  while ((modalResult = mrNone) and (not slApp.shouldquit)) do
    slApp.ProcessMessages;
  Result  := modalResult;
  modal   := False;
  Visible := slvHidden;
  SetFocus(pf);
end;

function TslWindow.TakesInputFocus: TslControl;
begin
  Result := FocusedControl;
end;

{ TslRect }

procedure TslRect.Assign(Left, Top, Right, Bottom: integer);
begin
  fLeft := Left;
  self.Right := Right;
  fTop := Top;
  self.Bottom := Bottom;
end;

procedure TslRect.Assign(src: TslRect);
begin
  Assign(src.Left, src.Top, src.Right, src.Bottom);
end;

constructor TslRect.Create(Left, Top, Right, Bottom: integer);
begin
  Assign(Left, Top, Right, Bottom);
end;

procedure TslRect.setBottom(const Value: integer);
begin
  if Value <> fBottom then
  begin
    fBottom := Value;
    Height  := fBottom - fTop + 1;
  end;
end;

procedure TslRect.setLeft(const Value: integer);
begin
  if Value <> fLeft then
  begin
    fLeft := Value;
    Width := fRight - fLeft + 1;
  end;
end;

procedure TslRect.setRight(const Value: integer);
begin
  if Value <> fRight then
  begin
    fRight := Value;
    Width  := fRight - fLeft + 1;
  end;
end;

procedure TslRect.setTop(const Value: integer);
begin
  if Value <> fTop then
  begin
    fTop   := Value;
    Height := fBottom - fTop + 1;
  end;
end;

{ TslHorizontalScrollbar }

constructor TslHorizontalScrollbar.Create(parent: TslControl);
begin
  inherited Create(0, 1, parent);
  Bottom := 1;
  Left   := 1;
end;

function TslHorizontalScrollbar.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := False;
  if not extended then
    exit;

  Result := True;
  if c = #132 then
    Position := Position - scrollArea.Width
  else
  if c = #118 then
    Position := Position + scrollArea.Width
  else
    Result   := False;
end;

procedure TslHorizontalScrollbar.Repaint;
var
  s:    string;
  i:    integer;
  m, c: integer;
  a, x: integer;
  p:    char;
begin
  if Visible <> slvVisible then
    exit;

  m := scrollArea.MaxWidth;
  c := scrollArea.Width;
  if ((Position + c - 1 >= m) and (not noatmax)) then
  begin
    fPosition := m - c + 1;
    atmax     := True;
  end
  else
    atmax := False;
  if Position < 1 then
    fPosition := 1;
  if c >= m then
    exit;


  SetColors;
  GetClientArea(ca, self);


{$IFDEF MSWINDOWS}
  Write(1, 1, chr(17));
  s := '';
  for i := 2 to ca.Width - 1 do
    s := s + chr(177);
  Write(2, 1, s);
  Write(ca.Width, 1, chr(16));
  p := chr(254);
{$ELSE}
  write(1, 1, '<');
  s:= '';
  for i:= 2 to ca.width-1 do
    s:= s + ' ';
  write(2, 1, s);
  write(ca.width, 1, '>');
  p:= '*';
{$ENDIF}

  if Position = 1 then
  begin
    Write(2, 1, p);
  end
  else
  if Position + c - 1 >= m then
  begin
    Write(ca.Width - 1, 1, p);
  end
  else
  begin
    // ki kell szamolni

    // eloszor kiszamoljuk hogy Position-nel hanyadik
    a := (Position + (Position + c - 1)) div 2;
    x := Round((a / m) * (ca.Width - 2));

    Write(1 + x, 1, p);
  end;

end;

procedure TslHorizontalScrollbar.SetColors;
begin
  slScreen.TextBackground(slcCyan);
  slScreen.TextColor(slcBlue);
end;

function TslHorizontalScrollbar.Width: integer;
begin
  Result := inherited Width;
  Dec(Result, Left + Right);
  if Result < 0 then
    Result := 0;
end;

{ TslVerticalScrollbar }

constructor TslVerticalScrollbar.Create(parent: TslControl);
begin
  inherited Create(1, 0, parent);
  Right := 1;
  Top   := 1;
end;

function TslVerticalScrollbar.Height: integer;
begin
  Result := inherited Height;
  Dec(Result, Top + Bottom);
  if Result < 0 then
    Result := 0;
end;

function TslVerticalScrollbar.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := False;
  if not extended then
    exit;

  Result := True;

  if ((extended) and (c = #71)) then // home
    Position := 1
  else
  if ((extended) and (c = #79)) then // end
    Position := scrollArea.MaxHeight
  else
  if c = #73 then
    Position := Position - scrollArea.Height // page up
  else
  if c = #81 then
    Position := Position + scrollArea.Height // page down
  else
    Result   := False;
end;

procedure TslVerticalScrollbar.Repaint;
var
  i: integer;
  a, m, c: integer;
  x: integer;
  p: char;
begin
  if Visible <> slvVisible then
    exit;

  m := scrollArea.MaxHeight;
  c := scrollArea.Height;
  if ((Position + c - 1 >= m) and (not noatmax)) then
  begin
    fPosition := m - c + 1;
    atmax     := True;
  end
  else
    atmax := False;
  if Position < 1 then
    fPosition := 1;
  if c >= m then
    exit;

  SetColors;
  GetClientArea(ca, self);

{$IFDEF MSWINDOWS}
  Write(1, 1, chr(30));
  for i := 2 to ca.Height - 1 do
    Write(1, i, chr(177));
  Write(1, ca.Height, chr(31));
  p := chr(254);
{$ELSE}
  write(1, 1, '^');
  for i:= 2 to ca.height-1 do
    write(1, i, ' ');
  write(1, ca.height, 'v');
  p:= '*';
{$ENDIF}

  if Position = 1 then
  begin
    Write(1, 2, p);
  end
  else
  if Position + c - 1 >= m then
  begin
    Write(1, ca.Height - 1, p);
  end
  else
  begin
    // ki kell szamolni

    // eloszor kiszamoljuk hogy Position-nel hanyadik
    a := (Position + (Position + c - 1)) div 2;
    x := Round((a / m) * (ca.Height - 2));

    Write(1, 1 + x, p);
  end;

end;

procedure TslVerticalScrollbar.SetColors;
begin
  slScreen.TextBackground(slcCyan);
  slScreen.TextColor(slcBlue);
end;

{ TslScrollControl }

procedure TslScrollControl.AddControl(control: TslControl);
begin
  if rest <> nil then
    rest.AddControl(control)
  else
    inherited;
end;

constructor TslScrollControl.Create(Width, Height: integer; parent: TslControl);
begin
  inherited Create(Width, Height, parent);

  hScrollBar := TslHorizontalScrollbar.Create(self);
  hScrollBar.noatmax := True;
  vScrollBar := TslVerticalScrollbar.Create(self);


  rest := TslScrollArea.Create(self);
  rest.hScrollBar := hScrollBar;
  rest.vScrollBar := vScrollBar;

  hScrollBar.scrollArea := rest;
  vScrollBar.scrollArea := rest;
end;

procedure TslScrollControl.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  inherited GetClientArea(ca, whowantstoknow);

  if whowantstoknow = rest then
  begin
    ca.Bottom := ca.Bottom - 1;
    ca.Right  := ca.Right - 1;
  end;
end;

function TslScrollControl.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := inherited KeyEvent(c, extended);

  if Result then
    exit;

  if hScrollBar <> nil then
    Result := hScrollBar.KeyEvent(c, extended);

  if Result then
    exit;

  if vScrollBar <> nil then
    Result := vScrollBar.KeyEvent(c, extended);

  if Result then
    exit;
end;

{ TslScrollArea }


procedure TslScrollArea.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  if -1 <> children.IndexOf(whowantstoknow) then
  begin
    ca.Left   := 1;
    ca.Top    := 1;
    ca.Right  := MaxWidth;
    ca.Bottom := MaxHeight;
    if self.ca.Width > ca.Right then
      ca.Right := self.ca.Width;
    if self.ca.Height > ca.Bottom then
      ca.Bottom := self.ca.Height;
  end
  else
    inherited GetClientArea(ca, whowantstoknow);
end;

procedure TslScrollArea.GotoXy(ca: TslRect; x, y: integer);
var
  nx, ny: integer;
begin
  if not directaddressing then
  begin
    nx := ca.Left + x - 1 - hScrollBar.position + 1;
    ny := ca.Top + y - 1 - vScrollBar.position + 1;
  end
  else
  begin
    nx := x;
    ny := y;
  end;

  inherited GotoXy(self.ca, nx, ny);
end;

function TslScrollArea.Height: integer;
begin
  GetClientArea(ca, self);
  Result := ca.Height;
end;


function TslScrollArea.MaxHeight: integer;
var
  i: integer;
  c: TslAlignedControl;
begin
  Result := 0;
  for i := 0 to children.Count - 1 do
    if children[i] is TslAlignedControl then
    begin
      c := TslAlignedControl(children[i]);
      if c.MaxHeight > Result then
        Result := c.MaxHeight;
    end;
  fMaxHeight := Result;
end;

function TslScrollArea.MaxWidth: integer;
var
  i: integer;
  c: TslAlignedControl;
begin
  Result := 0;
  for i := 0 to children.Count - 1 do
    if children[i] is TslAlignedControl then
    begin
      c := TslAlignedControl(children[i]);
      if c.MaxWidth > Result then
        Result := c.MaxWidth;
    end;
  fMaxWidth := Result;

end;

procedure TslScrollArea.SizeChanged;
var
  a: integer;
begin
  a := fMaxWidth;
  if ((a <> MaxWidth) and (hScrollBar <> nil)) then
  begin
    if hScrollBar.atmax then
      hScrollBar.position := a
    else
      hScrollBar.Repaint;
  end;
  a := fMaxHeight;
  if ((a <> MaxHeight) and (vScrollBar <> nil)) then
  begin
    if vScrollBar.atmax then
      vScrollBar.position := a
    else
      vScrollBar.Repaint;
  end;
end;

function TslScrollArea.Width: integer;
begin
  GetClientArea(ca, self);
  Result := ca.Width;
end;

procedure TslScrollArea.Write(ca: TslRect; const x, y: integer;
  s: string; hossz: integer);
var
  nx, ny: integer;
begin
  if not directaddressing then
  begin
    nx := ca.Left + x - 1 - hScrollBar.position + 1;
    ny := ca.Top + y - 1 - vScrollBar.position + 1;
  end
  else
  begin
    nx := x;
    ny := y;
  end;

  inherited Write(self.ca, nx, ny, s, hossz);

end;

{ TslScrollbar }

constructor TslScrollbar.Create(Width, Height: integer; parent: TslControl);
begin
  inherited Create(Width, Height, parent);
  position := 1;
end;

procedure TslScrollbar.SetPosition(Value: integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> fPosition then
  begin
    fPosition := Value;
    if scrollArea <> nil then
    begin
      Repaint; // azert ezt rajzoljuk ujra eloszor mert igy a position maxolva / minelve lesz
      scrollArea.Repaint;
    end;
  end;
end;

{ TslMutualVisibilityControl }

procedure TslMutualVisibilityControl.AddControl(control: TslControl);
begin
  inherited;
  if ((children.Count > 1) and (control.ffVisible = slvParent)) then
    TslControl(children[children.Count - 2]).Visible := slvHidden;
  if control.ffVisible = slvParent then
    visibleControl := control;
end;

function TslMutualVisibilityControl.KeyEvent(c: char; extended: boolean): boolean;
var
  i: integer;
begin
  Result := False;

  if Visible <> slvVisible then
    exit;


  if focus <> nil then
    Result := focus.KeyEvent(c, extended);

  if Result then
    exit;

  if children.Count <= 1 then
    exit;

  if c = #9 then
  begin
    i := children.IndexOf(visibleControl);
    if i = -1 then
      exit;
    if i = children.Count - 1 then
      i := 0
    else
      Inc(i);

    TslControl(children[i]).Visible := slvVisible;
    //    SetFocus(TslControl(children[i]));
    Result := True;
  end;
end;

procedure TslMutualVisibilityControl.VisibilityChanged(c: TslControl);
var
  i: integer;
begin
  if c.Visible <> slvHidden then
  begin
    visibleControl := c;
    for i := 0 to children.Count - 1 do
      if children[i] <> c then
        TslControl(children[i]).Visible := slvHidden;
  end
  else
    Repaint;

end;

{ TslTextBox }

procedure TslTextBox.AddLine(const s: string);
begin
  fText.Add(s);
  if not updateing then
    EndUpdate;
end;

procedure TslTextBox.BeginUpdate;
begin
  updateing := True;
end;

constructor TslTextBox.Create(Width, Height: integer; parent: TslControl);
begin
  fText := TStringList.Create;
  inherited Create(parent);
end;

destructor TslTextBox.Destroy;
begin
  fText.Free;
  inherited;
end;

procedure TslTextBox.EndUpdate;
begin
  if ftext = nil then
    Exit;
  updateing := False;
  if maxlines <> 0 then
    while (ftext.Count > maxlines) do
      ftext.Delete(0);

  if parent is TslScrollArea then
    TslScrollArea(parent).SizeChanged;

  Repaint;
end;

function TslTextBox.GetText: string;
begin
  Result := fText.Text;
end;

procedure TslTextBox.LoadFromFile(filename: string);
var
  f: TextFile;
  s: string;
begin
  if not FileExists(filename) then
    exit;

  BeginUpdate;
  AssignFile(f, filename);
  Reset(f);
  while not EOF(f) do
  begin
    Readln(f, s);
    AddLine(s);
  end;
  CloseFile(f);
  EndUpdate;
end;

function TslTextBox.MaxHeight: integer;
begin
  Result := ftext.Count;
end;

function TslTextBox.MaxWidth: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to ftext.Count - 1 do
    if length(ftext[i]) > Result then
      Result := length(ftext[i]);
end;

procedure TslTextBox.Repaint;
var
  i, boffset: integer;
begin
  if Visible <> slvVisible then
    exit;

  inherited;

  boffset := 0;
  if ca.Height > ftext.Count then
    boffset := ca.Height - ftext.Count;
  for i := 0 to ftext.Count - 1 do
    Write(1, 1 + i + boffset, ftext[i]);

end;

procedure TslTextBox.SetText(const Value: string);
begin
  fText.Text := Value;
  EndUpdate;
end;

{ TslButton }

constructor TslButton.Create(modalResult: TslModalResult; parent: TslControl);
const
  ModalCaptions: array[0..3] of string = ('', 'Ok', 'Cancel', '-');
begin
  self.modalResult := modalResult;
  inherited Create(ModalCaptions[integer(modalResult)], parent);
  Alignment := sltaCenter;
end;

procedure TslControl.SetModalResult(mr: TslModalResult);
var
  a: TslControl;
begin
  if (mr = mrNone) then
    exit;
  a := parent;
  while (a <> nil) do
  begin
    if (a is TslWindow) then
    begin
      if not TslWindow(a).modal then
        exit;
      TslWindow(a).modalResult := mr;
      Break;
    end;
    a := a.Parent;
  end;
end;

function TslButton.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := False;
  if ((c = #13) and (not extended)) then
  begin
    if Assigned(fonclick) then
      fonclick(self);

    SetModalResult(modalresult);

    Result := True;
  end;
end;


procedure TslButton.SetColors;
begin
  if focused then
    slScreen.TextColor(slcWhite)
  else
    slScreen.TextColor(slcBlack);

  slScreen.TextBackground(slcGreen);
end;

function TslButton.TakesInputFocus: TslControl;
begin
  Result := FocusedControl;
end;

function TslButton.Width: integer;
begin
  Result := fixedWidth + 4;
end;


{ TslMainBackground }


procedure TslMainBackground.SetColors;
begin
{$IFDEF MSWINDOWS}
  slscreen.TextColor(slcBlue);
  slscreen.TextBackground(slcLightGray);
{$ELSE}
  slscreen.NormVideo;
{$ENDIF}
end;

{ TslEdit }

constructor TslEdit.Create(Left, Top, Width: integer; parent: TslControl);
begin
  inherited Create(Width, 1, parent);
  passwordchar := #0;
  self.Left := Left;
  self.Top := Top;
  cursor := 1;
end;

function TslEdit.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := True;
{$IFDEF MSWINDOWS}
  if ((not extended) and (c = #22)) then // ctrl + v 
  begin
    Text := Text + clipboard.AsText;
  end
  else
{$ENDIF}
  if ((Text <> '') and (extended) and (c = #71)) then // home
  begin
    cursor := 1;
  end
  else
  if ((Text <> '') and (extended) and (c = #79)) then // end
  begin
    cursor := length(ftext) + 1;
  end
  else
  if ((extended) and (c = #75)) then // balnyil
  begin
    cursor := cursor - 1;
  end
  else
  if ((extended) and (c = #77)) then // jobbnyil
  begin
    cursor := cursor + 1;
  end
  else
  if ((extended) and (c = #83)) then // delete
  begin
    Delete(fText, Cursor, 1);
    Repaint;
  end
  else
  if ((not extended) and (c = #8)) then // backspace
  begin
    if cursor > 1 then
    begin
      Delete(fText, Cursor - 1, 1);
      cursor := cursor - 1;
    end;
  end
  else
  if ((not extended) and (c = #13)) then // enter
  begin
    SetModalResult(mrOk);
  end
  else
  if ((not extended) and (c = #27)) then // escape
  begin
    SetModalResult(mrCancel);
  end
  else
  if ((not extended) and (c >= #32)) then
  begin
    if ((maxwidth = 0) or (maxwidth < length(ftext))) then
    begin
      insert(c, fText, cursor);
      if cursor = length(ftext) then
        lastcharonly := True;
      cursor := cursor + 1;
    end;
  end
  else
  if Assigned(fOnKeyDown) then
    Result := fOnKeyDown(self, c, extended)
  else
    Result := False;
end;

procedure TslEdit.Repaint;
var
  i: integer;
  l: integer;
  c: char;
begin
  if Visible <> slvVisible then
    exit;

  SetColors;

  GetClientArea(ca, self);

  c := ' ';
  if window <> 0 then
{$IFDEF MSWINDOWS}
    c := chr(17);
{$ELSE}
    c:= '<';
{$ENDIF}
  Write(1, 1, c);

  l := length(fText);
  if not lastcharonly then
  begin
    for i := 2 to ca.Width - 1 do
    begin
      if l >= fWindow + i - 1 then
      begin
        if passwordchar = #0 then
          c := fText[fWindow + i - 1]
        else
          c := passwordchar;
      end
      else
        c := ' ';
      Write(i, 1, c);
    end;
  end
  else
  if ftext <> '' then
  begin
    if passwordchar = #0 then
      c := fText[l]
    else
      c := passwordchar;

    Write(1 + l - fWindow, 1, c);
  end;


  lastcharonly := False;




  c := ' ';
  if l - window >= ca.Width then
{$IFDEF MSWINDOWS}
    c := chr(16);
{$ELSE}
    c:= '>';
{$ENDIF}
  Write(ca.Width, 1, c);

  gotoxy(cursor - Window + 1, 1);

end;

procedure TslEdit.SetColors;
begin
{$IFDEF MSWINDOWS}
  slScreen.TextColor(slcWhite);
  slScreen.TextBackground(slcBlack);
{$ELSE}
  slScreen.HighVideo;
{$ENDIF}
end;

procedure TslEdit.SetCursor(Value: integer);
var
  w: integer;
begin
  if Value <= 0 then
    exit;
  if Value > length(ftext) + 1 then
    exit;

  fCursor := Value;


  w := Width - 2;
  if length(fText) >= w then
  begin
    if (Cursor - Window >= w) or (cursor - Window <= 1) then
    begin
      lastcharonly := False;
      fWindow      := fCursor - 5;
      if fWindow < 0 then
        fWindow := 0;
    end;
  end
  else
    fWindow := 0;
  Repaint;
end;

procedure TslEdit.SetText(Value: string);
begin
  fText := Value;
  if maxwidth > 0 then
    fText := Copy(fText, 1, maxwidth);
  cursor := Length(fText) + 1;
end;

function TslEdit.TakesInputFocus: TslControl;
begin
  Result := self;
end;

{ TslCommandEdit }

constructor TslCommandEdit.Create(Left, Top, Width: integer; parent: TslControl);
begin
  fCommands := TStringList.Create;
  lastCommandIndex := -1;
  inherited Create(Left, Top, Width, Parent);
end;

destructor TslCommandEdit.Destroy;
begin
  fCommands.Free;
  inherited;
end;

function TslCommandEdit.KeyEvent(c: char; extended: boolean): boolean;
begin
  Result := True;
  if ((extended) and (c = #72)) then // felfele nyil -- up arrow
  begin

    if lastCommandIndex <= 0 then
      lastCommandIndex := fCommands.Count - 1
    else
      Dec(lastCommandIndex);
    if lastCommandIndex < 0 then
      exit;

    Text := fCommands[lastCommandIndex];
  end
  else
  if ((extended) and (c = #80)) then // lefele nyil  -- down Arrow
  begin
    if lastCommandIndex >= fCommands.Count then
    begin
      Text := '';
      exit;
    end;
    Inc(lastCommandIndex);

    if lastCommandIndex >= fCommands.Count then
      exit;

    Text := fCommands[lastCommandIndex];
  end
  else
  if ((not extended) and (c = #13)) then // enter
  begin
    if Text = '' then
      exit;

    fCommands.Add(Text);
    if ((maxcommands <> 0) and (fcommands.Count > maxcommands)) then
      fcommands.Delete(0);
    lastCommandIndex := -1;

    if Assigned(fOnCommand) then
      fOnCommand(self, Text);
    Text := '';

  end
  else
    Result := False;

  if Result then
    exit;

  Result := inherited KeyEvent(c, extended);
end;

{ TslCommandWindow }

constructor TslCommandWindow.Create(Width, Height: integer;
  title, Caption: string; parent: TslControl);
var
  cp:  TslFixHeightPanel;
  fw:  TslFixWidthPanel;
  tmp: TslScrollArea;
begin
  inherited Create(Width, Height, title, parent);

  rest.SetParent(nil);
  tmp  := rest;
  rest := nil;

  cp := TslFixHeightPanel.Create(1, nil);

  fw := TslFixWidthPanel.Create(Length(Caption) + 1, nil);
  TslLabel.Create(Caption, fw);

  commandedit := TslCommandEdit.Create(1, 1, 0, nil);
  TslHorizontalAlignedPanel.Create(fw, slHALeft, commandedit, cp);

  rest2 := TslVerticalAlignedPanel.Create(cp, slVABottom, tmp, self);
  rest  := tmp;

  textbox := TslTextBox.Create(0, 0, self);

end;



procedure TslCommandWindow.GetClientArea(ca: TslRect; whowantstoknow: TslControl);
begin
  inherited GetClientArea(ca, whowantstoknow);

  if whowantstoknow = rest2 then
  begin
    ca.Top    := ca.Top + 1;
    ca.Left   := ca.Left + 1;
    ca.Right  := ca.Right - 1;
    ca.Bottom := ca.Bottom - 1;
  end;

end;

{ TslConsoleTask }

procedure TslConsoleTask.OnConleTaskAdded(v_queue: TObjectList);
begin
  // nothing.
end;

{ TslRemoveEarlierTask }

procedure TslRemoveEarlierTask.OnConleTaskAdded(v_queue: TObjectList);
var
  i: integer;
begin
  try
    for i := 0 to v_queue.Count - 2 do
    begin
      if v_queue[i].ClassName = ClassName then
      begin
        v_queue.Delete(i);
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slvision',
        Format('[EXCEPTION] TslRemoveEarlierTask.OnConleTaskAdded : %s', [e.Message]));
    end;
  end;
end;

{ TslRepaintTask }

procedure TslRepaintTask.Execute;
begin
  try
    slApp.Repaint;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slvision', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TslTextBoxTask }

constructor TslTextBoxTask.Create(windowTitle: string; textbox: TslTextBox);
begin
  self.windowTitle := windowTitle;
  self.textbox     := textbox;
end;

initialization
  slvision_lock := TCriticalSection.Create();
  slig := 0;
  lvtf := 0;

finalization
  slscreen.normvideo;
  slscreen.clrscr;
  slvision_lock.Free;
end.

