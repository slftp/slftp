unit console;

interface

procedure Console_QueueAdd(name, task: String);
procedure Console_QueueDel(name: String);
procedure Console_Slot_Add(name, s: String); overload;
procedure Console_Slot_Close(const name: String);

procedure Console_Slot_Add(name, FormatStr: String; const Args: array of const);overload;
procedure ConsoleStart;
procedure Console_SiteStat(allsites, upsites, downsites, unknown: Cardinal);
//procedure Console_QueueStat(queuedb: Cardinal);overload;
procedure Console_QueueStat(queuedb, t_race, t_dir, t_auto, t_other: Cardinal);overload;
procedure console_addline(const windowtitle, msg: String);
procedure console_repaint();
procedure console_delwindow(const windowtitle: String);
procedure console_add_dummywindow(const windowtitle: String);
procedure console_add_ircwindow(const windowtitle: String);
procedure console_add_sitewindow(const windowtitle: String);
function console_showwindow(const windowtitle: String): Boolean;
function console_windows: String;

procedure ConsoleInit;
procedure ConsoleUninit;

function ReadAppQueueCaption: String;
function ReadAppSitesCaption: String;

var
  no_console_msg: Boolean;
  no_console_queue: Boolean;
  no_console_slot: Boolean;

implementation

uses slvision, slconsole, mystrings, queueunit, debugunit, configunit, sitesunit,
     Contnrs, versioninfo, SysUtils, mainthread, Classes, irc, taskraw, slhelper,
     kb, StrUtils, encinifile, dateutils, mrdohutils, SyncObjs
     {$IFDEF MSWINDOWS},Windows {$ENDIF};

const section = 'console';

type
  TSiteStatTask = class(TslRemoveEarlierTask)
  private
    allsites, upsites, downsites, unknown: Cardinal;
  public
    constructor Create(allsites, upsites, downsites, unknown: Cardinal);
    procedure Execute; override;
  end;
  TQueueStatTask = class(TslRemoveEarlierTask)
  private
    queue, t_race, t_dir, t_auto, t_other: Cardinal;
  public
    //constructor Create(queue: Cardinal);overload;
    constructor Create(queue, t_race, t_dir, t_auto, t_other: Cardinal);overload;
    procedure Execute; override;
  end;
  TslCommandWindowTask = class(TslTextBoxTask)
  private
    windowtitle: String;
  public
    constructor Create(const windowtitle: String);
    function FindWindow: TslCommandWindow;
  end;
  TShowWindowTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;
  TDelWindowTask = class(TslCommandWindowTask)
    constructor Create(const windowtitle: String);
    procedure Execute; override;
  end;
  TAddIrcWindowTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;
  TAddSiteWindowTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;
  TAddDummyWindowTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;

  (*
  TTextBoxBeginUpdateTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;
  TTextBoxEndUpdateTask = class(TslCommandWindowTask)
    procedure Execute; override;
  end;
  *)
  TTextBoxAddLineTask = class(TslCommandWindowTask)
  private
    msg: String;
  public
    constructor Create(const windowtitle, msg: String);
    procedure Execute; override;
(*    procedure OnConleTaskAdded(queue: TObjectList); override; *)
  end;
  TItemManageTask = class(TTextBoxAddLineTask)
  private
    name: String;
  end;
  TQueueItemAddTask = class(TItemManageTask)
  public
    constructor Create(const name, msg: String);
    procedure Execute; override;
  end;
  TQueueItemDelTask = class(TItemManageTask)
  public
    constructor Create(const name: String);
    procedure Execute; override;
  end;
  TSlotItemAddTask = class(TItemManageTask)
  public
    constructor Create(const name, msg: String);
    procedure Execute; override;
  end;
  TSlotItemDelTask = class(TItemManageTask)
  public
    constructor Create(const name: String);
    procedure Execute; override;
  end;

  TMainTimer = class(TslTimer)
  public
    procedure OnTimer; override;
  end;
  TMySlApp = class(TslApplication)
  private
    vl: TslLabel;
    cw: TslCommandWindow;
    slots: TslCommandWindow;
    queue: TslCommandWindow;
    dir: String;
    main_timer: TMainTimer;
    inited: Boolean;
    sitesstat: TslLabel;
    m: TslMutualVisibilityControl;
    function OnKeyDown(sender: TslEdit; c: Char; extended: Boolean): Boolean;
    procedure OnAdminCommand(sender: TslEdit; const command: String);
    procedure OnIrcCommand(Sender: TslEdit; const command: String);
    procedure OnSiteCommand(Sender: TslEdit; const command: String);
  public
    queuestat: TslLabel;
    function AddIrcWindow(const netname: String): TslCommandWindow;
    function AddDummyWindow(const netname: String): TslCommandWindow;
    function AddSiteWindow(const netname: String): TslCommandWindow;
    procedure MyOnExit(sender: TslControl);
    procedure MyOnShow(sender: TslControl);
    constructor Create;
    destructor Destroy; override;
    function KeyEvent(c: Char; extended: Boolean): Boolean; override;
  end;

var app: TMySlApp;
  add_time_stamp: Boolean;

procedure ConsoleInit;
begin
  add_time_stamp := config.ReadBool('console','add_time_stamp', False);
  no_console_msg := config.ReadBool('console','no_console_msg', False);
  no_console_queue := config.ReadBool('console','no_console_queue', False);
  no_console_slot := config.ReadBool('console','no_console_slot', False);
end;

procedure ConsoleUninit;
begin

end;

function ReadAppQueueCaption: String;
begin
  result := app.queuestat.caption;
end;
function ReadAppSitesCaption: String;
begin
result := app.sitesstat.caption;
end;

function consolestrip(const s: String): String;
var
  i, skip: Integer;
begin
  Result := '';
  skip := 0;
  for i:= 1 to length(s) do
    if (skip = 0) then
    begin
      if ((ord(s[i]) >= 32) or (s[i] in [#13,#10])) then
      begin
        if (ord(s[i]) <> 255) then
        begin
          Result:= Result + s[i]
        end
        else
          Result:= Result + ' ';
      end else
      begin
        if ((s[i] = #3) and (i < length(s) -2)) then
        begin
          if IsANumber(s[i+1]) then
          begin
            if IsANumber(s[i+2]) then
              skip:= 2
            else
              skip:= 1;
          end;
        end;
      end;
    end else
      dec(skip);
end;

function MyFindWindow(const windowtitle: String): TslCommandWindow;
var
  i: Integer;
  t: TslCommandWindow;
begin
  Result := nil;
  for i := 0 to app.m.children.Count - 1 do
  begin
    try if i > app.m.children.Count then Break; except Break; end;
    t := TslCommandWindow( app.m.children[i] );
    if AnsiSameText(t.Title, windowtitle) then
    begin
      Result := t;
      exit;
    end;
  end;
end;

procedure console_repaint();
begin
  if app <> nil then
  begin
    try
      app.AddConsoleTask(TslRepaintTask.Create);
    except
      on e: Exception do
      begin
        Debug(dpError, 'console', Format('[EXCEPTION] console_repaint: %s', [e.Message]));
      end;
    end;
  end;
end;

procedure console_addline(const windowtitle, msg: String);
var w: String;
begin
  if app = nil then
    exit;

  w := windowtitle;
  if w = '' then
    w := 'Admin';

(*
  if (no_console_msg and (UpperCase(w) <> 'ADMIN') and (UpperCase(w) <> 'QUEUE') and (UpperCase(w) <> 'SLOTS')) then exit;
  if (no_console_queue and (UpperCase(w) = 'QUEUE')) then exit;
  if (no_console_slot and (UpperCase(w) = 'SLOTS')) then exit;
*)

  try
    if add_time_stamp then
      //app.AddConsoleTask(TTextBoxAddLineTask.Create(w, Format('[%s] %s',[FormatDateTime('hh:nn:ss', now),msg])))
      app.AddConsoleTask(TTextBoxAddLineTask.Create(w, Format('[%s] %s',[FormatDateTime('hh:nn:ss', now), wraptext(msg, (slScreen.GetWidth() - 2))])))
    else
      //app.AddConsoleTask(TTextBoxAddLineTask.Create(w, msg));
      app.AddConsoleTask(TTextBoxAddLineTask.Create(w, wraptext(msg, (slScreen.GetWidth() - 2))));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] console_addline: %s', [e.Message]));
    end;
  end;
end;

procedure console_add_ircwindow(const windowtitle: String);
begin
  if (no_console_msg) then
    exit;

  if app = nil then
    exit;

  slvision_lock.Enter();
  try
    if nil = MyFindWindow(windowtitle) then
      app.AddIrcWindow(windowtitle);
  finally
    slvision_lock.Leave;
  end;
end;

procedure console_add_dummywindow(const windowtitle: String);
begin
  if app = nil then
    exit;

  if (no_console_queue and (UpperCase(windowtitle) = 'QUEUE')) then
    exit;
  if (no_console_slot and (UpperCase(windowtitle) = 'SLOTS')) then
    exit;

  slvision_lock.Enter();
  try
    if nil = MyFindWindow(windowtitle) then
      app.AddDummyWindow(windowtitle);
  finally
    slvision_lock.Leave;
  end;
end;

procedure console_add_sitewindow(const windowtitle: String);
begin
  if (no_console_msg) then exit;

  if app = nil then exit;
  slvision_lock.Enter();
  try
    if nil = MyFindWindow(windowtitle) then
      app.AddSiteWindow(windowtitle);
  finally
    slvision_lock.Leave;
  end;
end;

function console_windows: String;
var
  i: Integer;
begin
  Result := '';
  if app = nil then
    exit;

  try
    slvision_lock.Enter();
    try
      for i:= 0 to app.m.children.Count-1 do
      begin
        try if i > app.m.children.Count then Break; except Break; end;
        Result := Result + TslWindow(app.m.children[i]).Title + #13#10;
      end;
    finally
      slvision_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Result := '';
      Debug(dpError, section, Format('[EXCEPTION] console_windows: %s', [e.Message]));
    end;
  end;
end;

procedure console_delwindow(const windowtitle: String);
var
  t: TslCommandWindow;
begin
  if app = nil then exit;

  try
    slvision_lock.Enter();
    try
      t := MyFindWindow(windowtitle);
      if t <> nil then t.Free;
    finally
      slvision_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] console_delwindow: %s', [e.Message]));
    end;
  end;
end;

function console_showwindow(const windowtitle: String): Boolean;
begin
  if app <> nil then
  begin
    try
      app.AddConsoleTask(TShowWindowTask.Create(windowtitle));
    except
      on e: Exception do
      begin
        Debug(dpError, 'console', Format('[EXCEPTION] console_showwindow: %s', [e.Message]));
      end;
    end;
  end;
  Result := True;
end;

procedure ConsoleStart;
begin
  app := TMySlApp.Create;
  with app do
  begin
    try
      try
        Run;
      finally
        if inited then
        begin
          Debug(dpError, section, 'slFtp exiting');
          QueueFire;
          SlotsFire;
        end;
        Free;
      end;
    except on e: Exception do
      begin
        Debug(dpError, section, 'slFtp exiting because of %s', [e.Message]);
      end;
    end;
  end;
end;



procedure Console_SiteStat(allsites, upsites, downsites, unknown: Cardinal);
begin
  try
    if app <> nil then
      app.AddConsoleTask(TSiteStatTask.Create(allsites,upsites,downsites,unknown));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_SiteStat %s', [e.Message]);
    end;
  end;
end;

(*
procedure Console_QueueStat(queuedb: Cardinal);overload;
begin
  try
    if app <> nil then
      app.AddConsoleTask(TQueueStatTask.Create(queuedb));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_QueueStat %s', [e.Message]);
    end;
  end;
end;
*)
procedure Console_QueueStat(queuedb, t_race, t_dir, t_auto, t_other: Cardinal);overload;
begin
  try
    if app <> nil then
      app.AddConsoleTask(TQueueStatTask.Create(queuedb, t_race, t_dir, t_auto, t_other));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_QueueStat %s', [e.Message]);
    end;
  end;
end;

procedure Console_Slot_Add(name, FormatStr: String; const Args: array of const);
begin
  if (no_console_slot) then exit;
  try
    Console_Slot_add(name, Format(FormatStr, Args));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_Slot_Add %s', [e.Message]);
    end;
  end;
end;

procedure Console_Slot_Add(name, s: String);
begin
  if (no_console_slot) then exit;
  try
    if app <> nil then
      app.AddConsoleTask(TSlotItemAddTask.Create(name, s));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_Slot_Add %s', [e.Message]);
    end;
  end;
end;

procedure Console_Slot_Close(const name: String);
begin
  if (no_console_slot) then exit;
  try
    if app <> nil then
      app.AddConsoleTask(TSlotItemDelTask.Create(name));
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_Slot_Close %s', [e.Message]);
    end;
  end;
end;

procedure Console_QueueAdd(name, task: String);
begin
  if (no_console_queue) then exit;

  try
    slvision_lock.Enter();
    try
      if app <> nil then
        app.AddConsoleTask(TQueueItemAddTask.Create(name, task));
    finally
      slvision_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_QueueAdd %s', [e.Message]);
    end;
  end;
end;

procedure Console_QueueDel(name: String);
begin
  if (no_console_queue) then exit;

  try
    slvision_lock.Enter();
    try
      if app <> nil then
        app.AddConsoleTask(TQueueItemDelTask.Create(name));
    finally
      slvision_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] Console_QueueDel %s', [e.Message]);
    end;
  end;
end;


{ TMySlApp }
procedure TMySlApp.OnAdminCommand(sender: TslEdit; const command: String);
begin
  if 1 = Pos(irccmdprefix, command) then
  begin
    debug(dpMessage, section, command);
    IrcProcessCommand('CONSOLE', TslWindow(sender.parent.parent.parent.parent).Title, Copy(command, length(irccmdprefix) + 1, 10000));
  end;
end;

constructor TMySlApp.Create;
begin
  inherited Create(80, 25);//config.ReadInteger(section, 'height', 50)

  dir := ExtractFilePath(ParamStr(0));

  OnExit := MyOnexit;
  OnShow := MyOnShow;
  vl := TslLabel.Create(GetFullVersionString, menubartop);

  m := TslMutualVisibilityControl.Create(self);

  cw := TslCommandWindow.Create(0, 0, 'Admin', 'Command:', m);

  cw.commandedit.OnCommand := OnAdminCommand;
  cw.commandedit.OnKeyDown := OnKeyDown;

  queuestat := TslLabel.Create('', menubarbottom);
  sitesstat := TslLabel.Create('', menubarbottom);
  sitesstat.Left := 2;

  inited := False;
  main_timer := TMainTimer.Create;
  timers.Add(main_timer);
end;

destructor TMySlApp.Destroy;
begin
  if inited then
  begin
    Main_Stop;
    Main_Uninit;

    DebugUninit;
    ConfigUninit;
  end;
  inherited;
end;

procedure TMySlApp.MyOnExit;
var
  x: TEncStringlist;
begin
  vl.Caption := 'slFtp exiting';
  slshutdown := True;

  x := TEncStringList.Create(passphrase);
  try
    x.assign( cw.commandedit.fCommands );
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.history');
  finally
    x.Free;
  end;
end;


procedure TMySlApp.MyOnShow(sender: TslControl);
label
  ujra;
var
  fs, p, p2: String;
  x: TEncStringList;
begin
  fs := CommonFileCheck;
  if fs <> '' then
  begin
    ShowMessage(fs);
    shouldquit := True;
    exit;
  end;

ujra:
  if not fileexists(dir + 'sites.dat') then
  begin
    if not InputQuery('1st time password','Enter your password:', p, True, dir + 'masterpass.txt') then
    begin
      shouldquit := True;
      exit;
    end;

    if not InputQuery('Password','Repeat:', p2, True, dir + 'masterpass.txt') then
    begin
      shouldquit := True;
      exit;
    end;

    if p <> p2 then
    begin
      ShowMessage('Passwords dont match!');
      goto ujra;
    end;

    p2 := 'r89v234ur8weurw8ehjrusdhfiusehfr3489rhweiufhsdufhsdehr9384h5v239842v384h';

  end
  else
  begin
    if not InputQuery('Password','Enter your password:', p, True, dir + 'masterpass.txt') then
    begin
      shouldquit := True;
      exit;
    end;
  end;

  if p = '' then
  begin
    ShowMessage('Empty password!');
    goto ujra;
  end;


  // now you can configinit and the other basic stuff ...
  if not ConfigInit(p) then
  begin
    ShowMessage('Cant load config file, wrong password?');
    shouldquit := True;
    exit;
  end;

  DebugInit;

  slscreen.SetResolution(config.ReadInteger(section, 'width', 80), config.ReadInteger(section, 'height', 25));
  vl.Caption := GetConsoleTitle;

  if not ReadSites then
  begin
    ShowMessage('Negative on that Houston!');
    shouldquit := True;
    exit;
  end;

  p := Main_Init;
  if p <> '' then
  begin
    ShowMessage(p);
    shouldquit := True;
    exit;
  end;

  if not no_console_slot then
  begin
    slots := AddDummyWindow('Slots');
  end;

  if not no_console_queue then
  begin
    queue := AddDummyWindow('Queue');
  end;

  cw.textbox.maxlines := config.ReadInteger(section, 'maxlines', 1000);
  cw.commandedit.maxcommands := config.ReadInteger(section, 'history_maxlines', 100);

  irc_Addtext('', '', '%s started', [GetFullVersionString]);

  Main_Run;
  main_timer.Interval := 100;
  inited := True;

  try
    x := TEncStringList.Create(passphrase);
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.history');
      cw.commandedit.fCommands.Assign(x);
    finally
      x.Free;
    end;
  except on e: Exception do
    begin
      ShowMessage('slftp.history is corrupt! Please remove it!');
      Debug(dpError, 'console', 'slftp.history is corrupt! Please remove it');
    end;
  end;

  if config.ReadBool('console', 'show_uptime', False) then
    timers.Add( TslUptimeTimer.Create(l_uptime) );

  if config.ReadBool('console', 'show_infos', False) then
    timers.Add( TslInfosTimer.Create(l_infos) );
end;

function TMySlApp.AddIrcWindow(const netname: String): TslCommandWindow;
begin
  Result := TslCommandWindow.Create(0,0,netname, 'Text:', nil);
  Result.textbox.maxlines := config.ReadInteger(section, 'maxlines', 1000);
  Result.Visible := slvHidden;
  Result.commandedit.OnCommand := OnIrcCommand;
  Result.SetParent(m);
end;

function TMySlApp.KeyEvent(c: Char; extended: Boolean): Boolean;
begin
  if ((c = #27) and (inited)) then
  begin
    cw.Visible := slvVisible;
    Result := True;
    exit;
  end;

  Result := inherited KeyEvent(c, extended);
end;

procedure TMySlApp.OnIrcCommand(Sender: TslEdit; const command: String);
var t: String;
begin
  t:= TslWindow(sender.parent.parent.parent.parent).Title;
  if AnsiSameText(command, '/names') then
  begin
    IrcProcessCommand('CONSOLE', t, 'names '+t);
  end else
    irc_addtext(SubString(t, ' ', 1), SubString(t, ' ', 2), command);
end;

function TMySlApp.AddSiteWindow(const netname: String): TslCommandWindow;
begin
  Result:= TslCommandWindow.Create(0,0,netname, 'Command:', nil);
  Result.textbox.maxlines:= config.ReadInteger(section, 'maxlines', 1000);
  Result.Visible:= slvHidden;
  Result.commandedit.OnCommand:= OnSiteCommand;
  Result.SetParent(m);
end;

procedure TMySlApp.OnSiteCommand(Sender: TslEdit; const command: String);
var s,t: String;
    rt: TRawTask;
begin
  t := TslWindow(sender.parent.parent.parent.parent).Title;
  s := SubString(t, '/', 1);

  rt := TRawTask.Create('','', s, '', command);
  rt.wantedslot := t;
  try
    AddTask(rt);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TMySlApp.OnSiteCommand.AddTask: %s', [e.Message]));
    end;
  end;
end;

function TMySlApp.AddDummyWindow(const netname: String): TslCommandWindow;
begin
  Result:= nil;
  if (no_console_queue and (UpperCase(netname) = 'QUEUE')) then exit;
  if (no_console_slot and (UpperCase(netname) = 'SLOTS')) then exit;

  Result:= TslCommandWindow.Create(0,0,netname, 'This is a dummy edit control, dont type anything.', nil);
  Result.textbox.maxlines:= config.ReadInteger(section, 'maxlines', 1000);
  Result.Visible:= slvHidden;
  Result.SetParent(m);
end;

function TMySlApp.OnKeyDown(sender: TslEdit; c: Char; extended: Boolean): Boolean;
var
  x, y: TStringList;
  i, j: Integer;
  l, ki: Integer;
  word: String;
begin
  Result := False;

  if ((c = #9) and (not extended) and (sender.Text <> '')) then
  begin
    Result := True;
    x := TStringList.Create;
    y := TStringList.Create;
    try
      x.Delimiter := ' ';
      x.DelimitedText := sender.Text;
      l := 1;
      ki := -1;
      word := '';
      for i := 0 to x.Count - 1 do
      begin
        if l + length(x[i]) >= sender.Cursor then
        begin
          ki := i;
          word := x[i];
          Break;
        end;
        inc(l, length(x[i])+1);
      end;


      if word <> '' then
      begin
        word := Copy(word, 1, sender.Cursor - l);

        (*
        for i := 0 to sectionhelper.Count - 1 do
          if AnsiStartsText(word, sectionhelper.Names[i]) then
          begin
            y.Add(sectionhelper.Names[i]);
          end;
        *)

        if y.Count > 0 then
        begin
          word := y[0];
          i := 1;
          while (i <= length(word)) do
          begin

            for j := 1 to y.Count - 1 do
            begin
              if not AnsiSameText(Copy(word, 1, i), Copy(y[j], 1, i)) then
              begin
                word := Copy(word, 1, i-1);
                Break;
              end;
            end;

            inc(i);
          end;

          if word <> '' then
          begin
            x[ki] := word;
            sender.Text := x.DelimitedText;
            sender.Cursor := l + length(word);
          end;
        end;
      end;
    finally
      y.Free;
      x.Free;
    end;
  end;
end;

{ TMainTimer }

procedure TMainTimer.OnTimer;
begin
  try
    Main_Iter;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] Main_Iter: %s', [e.Message]));
    end;
  end;
end;


{ TSiteStatTask }

constructor TSiteStatTask.Create(allsites, upsites, downsites, unknown: Cardinal);
begin
  self.allsites := allsites;
  self.upsites := upsites;
  self.downsites := downsites;
  self.unknown := unknown;
end;

procedure TSiteStatTask.Execute;
begin
  try
    app.sitesstat.caption:= Format('SITES: %u/%u/%u/%-10u', [allsites, upsites, downsites, unknown]);
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TQueueStatTask }

(*
constructor TQueueStatTask.Create(queue: Cardinal);
begin
  self.queue:= queue;
  self.t_race:=0;
  self.t_dir:=0;
  self.t_auto:=0;
  self.t_other:=0;
end;
*)

constructor TQueueStatTask.Create(queue, t_race, t_dir, t_auto, t_other: Cardinal);
begin
  self.queue:= queue;
  self.t_race:=t_race;
  self.t_dir:=t_dir;
  self.t_auto:=t_auto;
  self.t_other:=t_other;
end;

procedure TQueueStatTask.Execute;
begin
  try
    app.queuestat.Caption:= 'QUEUE: '+IntToStr(queue)+' (Race:'+IntToStr(t_race)+' Dir:'+IntToStr(t_dir)+' Auto:'+IntToStr(t_auto)+' Other:'+IntToStr(t_other)+')';
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] TQueueStatTask.Execute: %s', [e.Message]));
    end;
  end;
end;

{ TslTextBoxTask }

constructor TslCommandWindowTask.Create(const windowtitle: String);
var w: TslCommandWindow;

begin
  self.windowtitle:= windowtitle;
  w:= FindWindow;
  if w <> nil then
    inherited Create(windowtitle, w.textbox)
  else
    inherited Create(windowtitle, nil)
end;

function TslCommandWindowTask.FindWindow: TslCommandWindow;
begin
  Result:= MyFindWindow(windowtitle);
end;

{ TTextBoxAddLineTask }

constructor TTextBoxAddLineTask.Create(const windowtitle, msg: String);
begin
  inherited Create(windowtitle);
  self.msg:= msg;
end;

procedure TTextBoxAddLineTask.Execute;
var w: TslCommandWindow;
    s, ss: String;
    i: Integer;
begin
  try
    i:=0;
    w:= FindWindow();
    if w = nil then exit;
    w.textbox.BeginUpdate;
    try
      s:= msg;
      while(true) do
      begin
        if (i>200) then Break;
        inc(i);
        ss:= elsosor(s);
        if ss = '' then Break;

        w.textbox.AddLine(consolestrip(ss));
      end;
    finally
      w.textbox.EndUpdate;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] TTextBoxAddLineTask.Execute: %s', [e.Message]));
    end;
  end;
end;

(*
procedure TTextBoxAddLineTask.OnConleTaskAdded(queue: TObjectList);
var i, van: Integer;
begin
  // adni kell beginupdate, endupdate taskokat ha szukseges
  van:= -1;
  for i:= 0 to queue.Count -2 do
    if (queue[i] is TTextBoxAddLineTask) and (TTextBoxAddLineTask(queue[i]).windowtitle = windowtitle) then
    begin
      van:= i;
      break;
    end;

  if van < 0 then exit;
  if (van = 0) or (not (queue[van-1] is TTextBoxBeginUpdateTask)) or (TTextBoxBeginUpdateTask(queue[van-1]).windowtitle <> windowtitle) then
  begin
    // beszurunk beginupdatetaskot es endupdatetaskot is
    queue.Insert(van,  TTextBoxBeginUpdateTask.Create(windowtitle) );
    queue.Add(TTextBoxEndUpdateTask.Create(windowtitle) );
    exit;
  end;

  // most modostani kell az endupdatetask helyet
  van:= -1;
  for i:= queue.Count -2 downto 0 do
    if (queue[i] is TTextBoxEndupdateTask) and (TTextBoxEndupdateTask(queue[i]).windowtitle = windowtitle) then
    begin
      van:= i;
      break;
    end;

  if van = -1 then exit; // ez nem fordulhatna elo
  queue.Move(van, queue.Count-1);
end;

{ TTextBoxBeginUpdateTask }

procedure TTextBoxBeginUpdateTask.Execute;
var w: TslCommandWindow;
begin
  w:= FindWindow();
  if w = nil then exit;
  w.textbox.BeginUpdate;
end;

{ TTextBoxEndUpdateTask }

procedure TTextBoxEndUpdateTask.Execute;
var w: TslCommandWindow;
begin
  w:= FindWindow();
  if w = nil then exit;
  w.textbox.EndUpdate;
end;
*)

{ TDelWindowTask }

constructor TDelWindowTask.Create(const windowtitle: String);
begin
  inherited Create(windowtitle);
  remove:= True;
end;

procedure TDelWindowTask.Execute;
var w: TslCommandWindow;
begin
  try
    w:= FindWindow;
    if w = nil then exit;
    w.Free;
  except on e: Exception do begin
    Debug(dpError, 'console', Format('[EXCEPTION] TDelWindowTask.Execute : %s', [e.Message]));
    end;
  end;
end;

{ TAddIrcWindowTask }

procedure TAddIrcWindowTask.Execute;
var w: TslCommandWindow;
begin
  try
    w:= FindWindow;
    if w <> nil then exit;
    app.AddIrcWindow(windowtitle);
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TAddSiteWindowTask }

procedure TAddSiteWindowTask.Execute;
var w: TslCommandWindow;
begin
  try
    w:= FindWindow;
    if w <> nil then exit;
    app.AddSiteWindow(windowtitle);
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TAddDummyWindowTask }

procedure TAddDummyWindowTask.Execute;
var w: TslCommandWindow;

begin
  try
    w:= FindWindow;
    if w <> nil then exit;
    app.AddDummyWindow(windowtitle);
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TShowWindowTask }

procedure TShowWindowTask.Execute;
var w: TslCommandWindow;
begin
  try
    w:= FindWindow;
    if w = nil then exit;
    w.Visible:= slvVisible;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TQueueItemAddTask }

constructor TQueueItemAddTask.Create(const name, msg: String);
begin
  inherited Create('Queue', msg);
  self.name:= name;
end;

procedure TQueueItemAddTask.Execute;
var i: Integer;
begin
  try
    i:= app.queue.textbox.fText.IndexOfName(name);
    if i <> -1 then
      app.queue.textbox.fText[i]:= name+'='+msg
    else
      app.queue.textbox.fText.Add( name+'='+msg );
    if not app.queue.textbox.updateing then
      app.queue.textbox.EndUpdate;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TQueueItemDelTask }

constructor TQueueItemDelTask.Create(const name: String);
begin
  inherited Create('Queue', '');
  self.name:= name;
end;

procedure TQueueItemDelTask.Execute;
var i: Integer;
begin
  try
    i:= app.queue.textbox.fText.IndexOfName(name);
    if i <> -1 then
      app.queue.textbox.fText.Delete(i);
    if not app.queue.textbox.updateing then
      app.queue.textbox.EndUpdate;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TSlotItemAddTask }

constructor TSlotItemAddTask.Create(const name, msg: String);
begin
  inherited Create('Slots', msg);
  self.name:= name;
end;

procedure TSlotItemAddTask.Execute;
var i: Integer;
begin
  try
    i:= app.slots.textbox.fText.IndexOfName(name);
    if i <> -1 then
      app.slots.textbox.fText[i]:= name+'='+msg
    else
      app.slots.textbox.fText.Add( name+'='+msg );
    if not app.slots.textbox.updateing then
      app.slots.textbox.EndUpdate;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

{ TSlotItemDelTask }

constructor TSlotItemDelTask.Create(const name: String);
begin
  inherited Create('Slots', '');
  self.name:= name;
end;

procedure TSlotItemDelTask.Execute;
var i: Integer;
begin
  try
    i:= app.slots.textbox.fText.IndexOfName(name);
    if i <> -1 then
      app.slots.textbox.fText.Delete(i);
    if not app.slots.textbox.updateing then
      app.slots.textbox.EndUpdate;
  except
    on e: Exception do
    begin
      Debug(dpError, 'console', Format('[EXCEPTION] Execute: %s', [e.Message]));
    end;
  end;
end;

end.
