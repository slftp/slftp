unit slconsole;

interface

uses types;

const
  // Background and foreground colors
  slcBlack        = 0;
  slcBlue         = 1;
  slcGreen        = 2;
  slcCyan         = 3;
  slcRed          = 4;
  slcMagenta      = 5;
  slcBrown        = 6;
  slcLightGray    = 7;

  // Foreground colors
  slcDarkGray     = 8;
  slcLightBlue    = 9;
  slcLightGreen   = 10;
  slcLightCyan    = 11;
  slcLightRed     = 12;
  slcLightMagenta = 13;
  slcYellow       = 14;
  slcWhite        = 15;
  slcBlink        = 128;


type
    TslScreenEvent = procedure of object;

    { TslScreen }

    TslScreen = class
    private
    protected
      procedure UpdateCon; virtual; abstract;
    public
      OnCtrlC: TslScreenEvent;
      OnResize: TslScreenEvent;
      constructor Create;
      procedure ClrScr; virtual;
      function  GetWidth: Integer;
      function  GetHeight: Integer;
      procedure GetResolution(var x, y: Integer); virtual; abstract;
      procedure SetResolution(const x,y : Integer); virtual;
      procedure GetCursorPos(var x,y : Integer); virtual; abstract;
      procedure GotoXY(x, y: SmallInt); virtual; abstract;
      procedure Write(s: String); virtual; abstract;

      function ReadKey: Char; virtual; abstract;
      function KeyPressed: Boolean; virtual; abstract;
      procedure TextColor(Color: Byte); virtual; abstract;
      procedure TextBackground(Color: Byte); virtual; abstract;
      procedure NormVideo; virtual;
      procedure HighVideo; virtual;
      procedure LowVideo; virtual;
      procedure cursoron; virtual; abstract;
      procedure cursoroff; virtual; abstract;
    end;

{$IFNDEF MSWINDOWS}
  function NCurses_version: String;
{$ENDIF}

{ Initializes the @link(slScreen) variable with a Windows or Unix screen }
procedure slConsoleInit;
{ Resets the video, clears the screen and frees the @link(slScreen) variable }
procedure slConsoleUnInit;

var
  slScreen : TslScreen = nil;

implementation

uses
{$IFDEF FPC}
  {$IFDEF UNIX}
    BaseUnix,
    termio,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    ncurses,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}

{$IFNDEF MSWINDOWS}
  slsignals,
{$ENDIF}

  SysUtils;

{$IFDEF MSWINDOWS}
type
    TslWindowsScreen = class(TslScreen)
    private
      hStdIn: THandle;
      hStdout: THandle;
      ConsoleScreenBufferInfo : Console_screen_buffer_info;
       TextAttr: Byte;
      function slKeyPressed: Boolean;
      function slReadKey: Char;
    protected
      procedure UpdateCon; override;
    public
      constructor Create;
      procedure ClrScr; override;
      procedure GetResolution(var x, y: Integer); override;
      procedure SetResolution(const x, y: Integer); override;
      procedure GetCursorPos(var x, y: Integer); override;
      procedure GotoXY(x, y: SmallInt); override;
      procedure Write(s: String); override;

      function ReadKey: Char; override;
      function KeyPressed: Boolean; override;
      procedure TextBackground(Color: Byte); override;
      procedure TextColor(Color: Byte); override;

        procedure cursoron; override;
        procedure cursoroff; override;
    end;
{$ELSE}

const
   KEY_ALTA = 465; { alt/a }
   KEY_ALTB = 466;
   KEY_ALTC = 467;
   KEY_ALTD = 468;
   KEY_ALTE = 469;
   KEY_ALTF = 470;
   KEY_ALTG = 471;
   KEY_ALTH = 472;
   KEY_ALTI = 473;
   KEY_ALTJ = 474;
   KEY_ALTK = 475;
   KEY_ALTL = 476;
   KEY_ALTM = 477;
   KEY_ALTN = 478;
   KEY_ALTO = 479;
   KEY_ALTP = 480;
   KEY_ALTQ = 481;
   KEY_ALTR = 482;
   KEY_ALTS = 483;
   KEY_ALTT = 484;
   KEY_ALTU = 485;
   KEY_ALTV = 486;
   KEY_ALTW = 487;
   KEY_ALTX = 488;
   KEY_ALTY = 489;
   KEY_ALTZ = 490; { alt/z }
   KEY_ALT1 = 491; { alt/1 }
   KEY_ALT2 = 492; { alt/2 }
   KEY_ALT3 = 493; { alt/3 }
   KEY_ALT4 = 494; { alt/4 }
   KEY_ALT5 = 495; { alt/5 }
   KEY_ALT6 = 496; { alt/6 }
   KEY_ALT7 = 497; { alt/7 }
   KEY_ALT8 = 498; { alt/8 }
   KEY_ALT9 = 499; { alt/9 }
   KEY_ALT0 = 500; { alt/0 }
   KEY_ALTMINUS = 501; { alt/- }
   KEY_ALTEQUAL = 502; { alt/= }
   KEY_ALTTAB   = 503; { alt/tab }


type
    TslUnixScreen = class(TslScreen)
    private
       WS: TWinSize;
       w: PWINDOW;
       TextAttr : Byte ;
       prev_textattr : integer;
       cp : array [0..7,0..7] of integer; { color pair array }
       Procedure nWinColor(win : pWindow; att : integer);
       Procedure nWrite(win : pWindow; s : String);
       Function nSetColorPair(att : integer) : integer;
       Function CursesAtts(att : byte) : longint;
       procedure nClrScr(win : pWindow; att : integer);
    protected
      procedure UpdateCon; override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure GetResolution(var x, y: Integer); override;
      procedure GetCursorPos(var x, y: Integer); override;
      procedure GotoXY(x, y: SmallInt); override;
      procedure Write(s: String); override;

      procedure SetResolution(const x, y: Integer); override;
      procedure TextColor(Color: Byte); override;
      procedure TextBackground(Color: Byte); override;
      procedure HighVideo; override;
      procedure LowVideo; override;
      function ReadKey: Char; override;
      function KeyPressed: Boolean; override;
      Procedure ClrScr; override;
      procedure cursoron; override;
      procedure cursoroff; override;
    end;
{$ENDIF}


procedure slConsoleInit;
begin
{$IFDEF MSWINDOWS}
  slScreen := TslWindowsScreen.Create;
{$ELSE}
  slScreen := TslUnixScreen.Create;
{$ENDIF}
end;

procedure slConsoleUnInit;
begin
  slscreen.normvideo;
  slscreen.clrscr;
  FreeAndNil(slScreen);
end;

{ TslScreen }
procedure TslScreen.ClrScr;
var px,py: Integer;
    i: Integer;
    s: String;
begin
  GetResolution(px, py);
  s:= Format('%'+IntToStr(pX)+'s', [' ']);
  for i:= 1 to pY do
  begin
    GotoXY(1, i);
    Write(s);
  end;

  GotoXy(1,1);
end;

(*
{$IFDEF FPC}
function fpcCtrlCHandler(CtrlBreak: boolean): boolean;
begin
  if Assigned(slScreen.onCtrlC) then
    slScreen.onCtrlC();

  Result:= True;
end;
{$ENDIF}
*)


constructor TslScreen.Create;
begin
  UpdateCon;

(*
{$IFDEF FPC}
  SysSetCtrlBreakHandler(@fpcCtrlCHandler);
{$ENDIF}
*)

  inherited;
end;

function TslScreen.GetHeight: Integer;
var px, py: Integer;
begin
  GetResolution(px, py);
  Result:= pY;
end;

function TslScreen.GetWidth: Integer;
var px, py: Integer;
begin
  GetResolution(px, py);
  Result:= pX;
end;


procedure TslScreen.SetResolution(const x, y: Integer);
begin
  UpdateCon;

  if Assigned(OnResize) then
    OnResize();
end;




procedure TslScreen.NormVideo;
begin
  TextColor(7);
  TextBackGround(0);
end;

procedure TslScreen.HighVideo;
begin
  TextColor(7);
  TextBackGround(0);
end;

procedure TslScreen.LowVideo;
begin
  TextColor(7);
  TextBackGround(0);
end;


{$IFDEF MSWINDOWS}

function ConProc(CtrlType : DWord) : Bool; stdcall;
begin
  if Assigned(slScreen.OnCtrlC) then
    slScreen.OnCtrlC;
  Result:= True;
end;


type
  PKey = ^TKey;
  TKey = record
    KeyCode: Smallint;
    Normal: Smallint;
    Shift: Smallint;
    Ctrl: Smallint;
    Alt: Smallint;
  end;

function TranslateKey(const Rec: TInputRecord; State: Integer; Key: PKey; KeyCode: Integer): Smallint;
begin
  if State and (RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED) <> 0 then
    Result := Key^.Alt
  else if State and (RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED) <> 0 then
    Result := Key^.Ctrl
  else if State and SHIFT_PRESSED <> 0 then
    Result := Key^.Shift
  else if KeyCode in [Ord('A')..Ord('Z')] then
    Result := Ord(Rec.Event.KeyEvent.AsciiChar)
  else
    Result := Key^.Normal;
end;

function ConvertKey(const Rec: TInputRecord; Key: PKey): Smallint;
  {$IFDEF INLINES}inline;{$ENDIF}
begin
  if Assigned(Key) then
    Result := TranslateKey(Rec, Rec.Event.KeyEvent.dwControlKeyState,
      Key, Rec.Event.KeyEvent.wVirtualKeyCode)
  else
    Result := -1
end;

const
  CKeys: array[0..88] of TKey = (
    (KeyCode: VK_BACK;     Normal: $8;        Shift: $8;       Ctrl: $7F;  Alt: $10E; ),
    (KeyCode: VK_TAB;      Normal: $9;        Shift: $10F;     Ctrl: $194; Alt: $1A5; ),
    (KeyCode: VK_RETURN;   Normal: $D;        Shift: $D;       Ctrl: $A;   Alt: $1A6),
    (KeyCode: VK_ESCAPE;   Normal: $1B;       Shift: $1B;      Ctrl: $1B;  Alt: $101),
    (KeyCode: VK_SPACE;    Normal: $20;       Shift: $20;      Ctrl: $103; Alt: $20),
    (KeyCode: Ord('0');    Normal: Ord('0');  Shift: Ord(')'); Ctrl: - 1;  Alt: $181),
    (KeyCode: Ord('1');    Normal: Ord('1');  Shift: Ord('!'); Ctrl: - 1;  Alt: $178),
    (KeyCode: Ord('2');    Normal: Ord('2');  Shift: Ord('@'); Ctrl: $103; Alt: $179),
    (KeyCode: Ord('3');    Normal: Ord('3');  Shift: Ord('#'); Ctrl: - 1;  Alt: $17A),
    (KeyCode: Ord('4');    Normal: Ord('4');  Shift: Ord('$'); Ctrl: - 1;  Alt: $17B),
    (KeyCode: Ord('5');    Normal: Ord('5');  Shift: Ord('%'); Ctrl: - 1;  Alt: $17C),
    (KeyCode: Ord('6');    Normal: Ord('6');  Shift: Ord('^'); Ctrl: $1E;  Alt: $17D),
    (KeyCode: Ord('7');    Normal: Ord('7');  Shift: Ord('&'); Ctrl: - 1;  Alt: $17E),
    (KeyCode: Ord('8');    Normal: Ord('8');  Shift: Ord('*'); Ctrl: - 1;  Alt: $17F),
    (KeyCode: Ord('9');    Normal: Ord('9');  Shift: Ord('('); Ctrl: - 1;  Alt: $180),
    (KeyCode: Ord('A');    Normal: Ord('a');  Shift: Ord('A'); Ctrl: $1;   Alt: $11E),
    (KeyCode: Ord('B');    Normal: Ord('b');  Shift: Ord('B'); Ctrl: $2;   Alt: $130),
    (KeyCode: Ord('C');    Normal: Ord('c');  Shift: Ord('C'); Ctrl: $3;   Alt: $12E),
    (KeyCode: Ord('D');    Normal: Ord('d');  Shift: Ord('D'); Ctrl: $4;   Alt: $120),
    (KeyCode: Ord('E');    Normal: Ord('e');  Shift: Ord('E'); Ctrl: $5;   Alt: $112),
    (KeyCode: Ord('F');    Normal: Ord('f');  Shift: Ord('F'); Ctrl: $6;   Alt: $121),
    (KeyCode: Ord('G');    Normal: Ord('g');  Shift: Ord('G'); Ctrl: $7;   Alt: $122),
    (KeyCode: Ord('H');    Normal: Ord('h');  Shift: Ord('H'); Ctrl: $8;   Alt: $123),
    (KeyCode: Ord('I');    Normal: Ord('i');  Shift: Ord('I'); Ctrl: $9;   Alt: $117),
    (KeyCode: Ord('J');    Normal: Ord('j');  Shift: Ord('J'); Ctrl: $A;   Alt: $124),
    (KeyCode: Ord('K');    Normal: Ord('k');  Shift: Ord('K'); Ctrl: $B;   Alt: $125),
    (KeyCode: Ord('L');    Normal: Ord('l');  Shift: Ord('L'); Ctrl: $C;   Alt: $126),
    (KeyCode: Ord('M');    Normal: Ord('m');  Shift: Ord('M'); Ctrl: $D;   Alt: $132),
    (KeyCode: Ord('N');    Normal: Ord('n');  Shift: Ord('N'); Ctrl: $E;   Alt: $131),
    (KeyCode: Ord('O');    Normal: Ord('o');  Shift: Ord('O'); Ctrl: $F;   Alt: $118),
    (KeyCode: Ord('P');    Normal: Ord('p');  Shift: Ord('P'); Ctrl: $10;  Alt: $119),
    (KeyCode: Ord('Q');    Normal: Ord('q');  Shift: Ord('Q'); Ctrl: $11;  Alt: $110),
    (KeyCode: Ord('R');    Normal: Ord('r');  Shift: Ord('R'); Ctrl: $12;  Alt: $113),
    (KeyCode: Ord('S');    Normal: Ord('s');  Shift: Ord('S'); Ctrl: $13;  Alt: $11F),
    (KeyCode: Ord('T');    Normal: Ord('t');  Shift: Ord('T'); Ctrl: $14;  Alt: $114),
    (KeyCode: Ord('U');    Normal: Ord('u');  Shift: Ord('U'); Ctrl: $15;  Alt: $116),
    (KeyCode: Ord('V');    Normal: Ord('v');  Shift: Ord('V'); Ctrl: $16;  Alt: $12F),
    (KeyCode: Ord('W');    Normal: Ord('w');  Shift: Ord('W'); Ctrl: $17;  Alt: $111),
    (KeyCode: Ord('X');    Normal: Ord('x');  Shift: Ord('X'); Ctrl: $18;  Alt: $12D),
    (KeyCode: Ord('Y');    Normal: Ord('y');  Shift: Ord('Y'); Ctrl: $19;  Alt: $115),
    (KeyCode: Ord('Z');    Normal: Ord('z');  Shift: Ord('Z'); Ctrl: $1A;  Alt: $12C),
    (KeyCode: VK_PRIOR;    Normal: $149;      Shift: $149;     Ctrl: $184; Alt: $199),
    (KeyCode: VK_NEXT;     Normal: $151;      Shift: $151;     Ctrl: $176; Alt: $1A1),
    (KeyCode: VK_END;      Normal: $14F;      Shift: $14F;     Ctrl: $175; Alt: $19F),
    (KeyCode: VK_HOME;     Normal: $147;      Shift: $147;     Ctrl: $177; Alt: $197),
    (KeyCode: VK_LEFT;     Normal: $14B;      Shift: $14B;     Ctrl: $173; Alt: $19B),
    (KeyCode: VK_UP;       Normal: $148;      Shift: $148;     Ctrl: $18D; Alt: $198),
    (KeyCode: VK_RIGHT;    Normal: $14D;      Shift: $14D;     Ctrl: $174; Alt: $19D),
    (KeyCode: VK_DOWN;     Normal: $150;      Shift: $150;     Ctrl: $191; Alt: $1A0),
    (KeyCode: VK_INSERT;   Normal: $152;      Shift: $152;     Ctrl: $192; Alt: $1A2),
    (KeyCode: VK_DELETE;   Normal: $153;      Shift: $153;     Ctrl: $193; Alt: $1A3),
    (KeyCode: VK_NUMPAD0;  Normal: Ord('0');  Shift: $152;     Ctrl: $192; Alt: - 1),
    (KeyCode: VK_NUMPAD1;  Normal: Ord('1');  Shift: $14F;     Ctrl: $175; Alt: - 1),
    (KeyCode: VK_NUMPAD2;  Normal: Ord('2');  Shift: $150;     Ctrl: $191; Alt: - 1),
    (KeyCode: VK_NUMPAD3;  Normal: Ord('3');  Shift: $151;     Ctrl: $176; Alt: - 1),
    (KeyCode: VK_NUMPAD4;  Normal: Ord('4');  Shift: $14B;     Ctrl: $173; Alt: - 1),
    (KeyCode: VK_NUMPAD5;  Normal: Ord('5');  Shift: $14C;     Ctrl: $18F; Alt: - 1),
    (KeyCode: VK_NUMPAD6;  Normal: Ord('6');  Shift: $14D;     Ctrl: $174; Alt: - 1),
    (KeyCode: VK_NUMPAD7;  Normal: Ord('7');  Shift: $147;     Ctrl: $177; Alt: - 1),
    (KeyCode: VK_NUMPAD8;  Normal: Ord('8');  Shift: $148;     Ctrl: $18D; Alt: - 1),
    (KeyCode: VK_NUMPAD9;  Normal: Ord('9');  Shift: $149;     Ctrl: $184; Alt: - 1),
    (KeyCode: VK_MULTIPLY; Normal: Ord('*');  Shift: Ord('*'); Ctrl: $196; Alt: $137),
    (KeyCode: VK_ADD;      Normal: Ord('+');  Shift: Ord('+'); Ctrl: $190; Alt: $14E),
    (KeyCode: VK_SUBTRACT; Normal: Ord('-');  Shift: Ord('-'); Ctrl: $18E; Alt: $14A),
    (KeyCode: VK_DECIMAL;  Normal: Ord('.');  Shift: Ord('.'); Ctrl: $153; Alt: $193),
    (KeyCode: VK_DIVIDE;   Normal: Ord('/');  Shift: Ord('/'); Ctrl: $195; Alt: $1A4),
    (KeyCode: VK_F1;       Normal: $13B;      Shift: $154;     Ctrl: $15E; Alt: $168),
    (KeyCode: VK_F2;       Normal: $13C;      Shift: $155;     Ctrl: $15F; Alt: $169),
    (KeyCode: VK_F3;       Normal: $13D;      Shift: $156;     Ctrl: $160; Alt: $16A),
    (KeyCode: VK_F4;       Normal: $13E;      Shift: $157;     Ctrl: $161; Alt: $16B),
    (KeyCode: VK_F5;       Normal: $13F;      Shift: $158;     Ctrl: $162; Alt: $16C),
    (KeyCode: VK_F6;       Normal: $140;      Shift: $159;     Ctrl: $163; Alt: $16D),
    (KeyCode: VK_F7;       Normal: $141;      Shift: $15A;     Ctrl: $164; Alt: $16E),
    (KeyCode: VK_F8;       Normal: $142;      Shift: $15B;     Ctrl: $165; Alt: $16F),
    (KeyCode: VK_F9;       Normal: $143;      Shift: $15C;     Ctrl: $166; Alt: $170),
    (KeyCode: VK_F10;      Normal: $144;      Shift: $15D;     Ctrl: $167; Alt: $171),
    (KeyCode: VK_F11;      Normal: $185;      Shift: $187;     Ctrl: $189; Alt: $18B),
    (KeyCode: VK_F12;      Normal: $186;      Shift: $188;     Ctrl: $18A; Alt: $18C),
    (KeyCode: $DC;         Normal: Ord('\');  Shift: Ord('|'); Ctrl: $1C;  Alt: $12B),
    (KeyCode: $BF;         Normal: Ord('/');  Shift: Ord('?'); Ctrl: - 1;  Alt: $135),
    (KeyCode: $BD;         Normal: Ord('-');  Shift: Ord('_'); Ctrl: $1F;  Alt: $182),
    (KeyCode: $BB;         Normal: Ord('=');  Shift: Ord('+'); Ctrl: - 1;  Alt: $183),
    (KeyCode: $DB;         Normal: Ord('[');  Shift: Ord('{'); Ctrl: $1B;  Alt: $11A),
    (KeyCode: $DD;         Normal: Ord(']');  Shift: Ord('}'); Ctrl: $1D;  Alt: $11B),
    (KeyCode: $BA;         Normal: Ord(';');  Shift: Ord(':'); Ctrl: - 1;  Alt: $127),
    (KeyCode: $DE;         Normal: Ord(''''); Shift: Ord('"'); Ctrl: - 1;  Alt: $128),
    (KeyCode: $BC;         Normal: Ord(',');  Shift: Ord('<'); Ctrl: - 1;  Alt: $133),
    (KeyCode: $BE;         Normal: Ord('.');  Shift: Ord('>'); Ctrl: - 1;  Alt: $134),
    (KeyCode: $C0;         Normal: Ord('`');  Shift: Ord('~'); Ctrl: - 1;  Alt: $129)
  );

var
  ExtendedChar: Char = #0;


function FindKeyCode(KeyCode: Smallint): PKey; {$IFDEF INLINES}inline;{$ENDIF}
var
  I: Integer;
begin
  for I := 0 to High(CKeys) do
    if CKeys[I].KeyCode = KeyCode then
    begin
      Result := @CKeys[I];
      Exit;
    end;
  Result := nil;
end;


function TslWindowsScreen.slKeyPressed: Boolean;
var
  InputRecArray: array of TInputRecord;
  NumRead: DWORD;
  NumEvents: DWORD;
  I: Integer;
  KeyCode: Word;
begin
  Result := False;
  GetNumberOfConsoleInputEvents(hStdIn, NumEvents);
  if NumEvents = 0 then
    Exit;
  SetLength(InputRecArray, NumEvents);
  PeekConsoleInput(hStdIn, InputRecArray[0], NumEvents, NumRead);
  for I := 0 to High(InputRecArray) do
  begin
    if (InputRecArray[I].EventType and Key_Event <> 0) and
       InputRecArray[I].Event.KeyEvent.bKeyDown then
    begin
      KeyCode := InputRecArray[I].Event.KeyEvent.wVirtualKeyCode;
      if not (KeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL]) then
      begin
        if ConvertKey(InputRecArray[I], FindKeyCode(KeyCode)) <> -1 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TslWindowsScreen.slReadKey: Char;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
  KeyCode: Smallint;
begin
  if ExtendedChar <> #0 then
  begin
    Result := ExtendedChar;
    ExtendedChar := #0;
    Exit;
  end
  else
  begin
    Result := #$FF;
    GetConsoleMode(hStdIn, KeyMode);
    SetConsoleMode(hStdIn, 0);
    repeat
      ReadConsoleInput(hStdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and
         InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          // From Delphi 2009 on, Result is WideChar
          Result := Chr(Ord(InputRec.Event.KeyEvent.AsciiChar));
          Break;
        end;
        KeyCode := ConvertKey(InputRec,
          FindKeyCode(InputRec.Event.KeyEvent.wVirtualKeyCode));
        if KeyCode > $FF then
        begin
          ExtendedChar := Chr(KeyCode and $FF);
          Result := #0;
          Break;
        end;
      end;
    until False;
    SetConsoleMode(hStdIn, KeyMode);
  end;
end;


function TslWindowsScreen.KeyPressed: Boolean;
begin
  Result:= slKeyPressed;
end;

procedure TslWindowsScreen.TextBackground(Color: Byte);
begin
  TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
  SetConsoleTextAttribute(hStdOut, TextAttr);
end;

procedure TslWindowsScreen.TextColor(Color: Byte);
begin
  TextAttr := (TextAttr and $F0) or (Color and $0F);
  SetConsoleTextAttribute(hStdOut, TextAttr);
end;

function TslWindowsScreen.ReadKey: Char;
begin
  Result:= slReadKey;
//  if ((Result = #3) and (Assigned(OnCtrlC))) then
//    OnCtrlC();
end;


procedure TslWindowsScreen.cursoroff;
var CursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(hStdOut, CursorInfo);
  CursorInfo.bVisible := false;
  SetConsoleCursorInfo(hStdOut, CursorInfo);
end;

procedure TslWindowsScreen.cursoron;
var CursorInfo: TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(hStdOut, CursorInfo);
  CursorInfo.bVisible := True;
  SetConsoleCursorInfo(hStdOut, CursorInfo);
end;


{ TslWindowsScreen }


constructor TslWindowsScreen.Create;
begin
  hStdin:= GetStdHandle(STD_INPUT_HANDLE) ;
  hStdout:= GetStdHandle(STD_OUTPUT_HANDLE) ;
  SetConsoleCtrlHandler (@ConProc, True);

  inherited;
end;

procedure TslWindowsScreen.UpdateCon;
begin
{$IFDEF FPC}
  GetConsoleScreenBufferInfo(hStdOut, @ConsoleScreenBufferInfo);
{$ELSE}
  GetConsoleScreenBufferInfo(hStdOut, ConsoleScreenBufferInfo);
{$ENDIF}

  TextAttr:= ConsoleScreenBufferInfo.wAttributes and $FF;
end;

procedure TslWindowsScreen.GetCursorPos(var x, y: Integer);
begin
  UpdateCon;
  X:=ConsoleScreenBufferInfo.dwCursorPosition.X + 1;
  Y:=ConsoleScreenBufferInfo.dwCursorPosition.Y + 1;
end;

procedure TslWindowsScreen.GetResolution(var x, y: Integer);
begin
  X:= ConsoleScreenBufferInfo.dwSize.X;
  Y:= ConsoleScreenBufferInfo.dwSize.Y;
end;

procedure TslWindowsScreen.GotoXY(x, y: SmallInt);
var newpos: _COORD;
begin
  NewPos.X := X - 1;
  NewPos.Y := Y - 1;
  SetConsoleCursorPosition(hStdOut, NewPos);
end;

procedure TslWindowsScreen.Write(s: String);
begin
  System.Write(s);
end;


procedure TslWindowsScreen.SetResolution(const x, y: Integer);
var dw: _COORD;
    sr: _SMALL_RECT;
begin
  dw.X:= X;
  dw.Y:= Y;
  sr.Left:= 0;
  sr.Top:= 0;
  sr.Right:= X-1;
  sr.Bottom:= Y-1;
  SetConsoleScreenBufferSize(hStdOut, dw);
  SetConsoleWindowInfo(hStdout, True, sr);


  inherited;
end;

procedure TslWindowsScreen.ClrScr;
var c: _COORD;
    d: DWORD;
    Len: Integer;
begin
c.X:= 0;
  c.Y:= 0;
  Len:= ConsoleScreenBufferInfo.dwSize.X*ConsoleScreenBufferInfo.dwSize.Y;
  FillConsoleOutputCharacterA(hStdOut, ' ', Len, c, d);
  FillConsoleOutputAttribute(hStdOut, TextAttr, Len, c, d);
  GotoXy(1,1);
end;
{$ELSE}

procedure TslUnixScreen.UpdateCon;
begin
{$IFDEF FPC}
  fpioctl(stdinputhandle, TIOCGWINSZ, @WS);
{$ELSE}
  ioctl(0, TIOCGWINSZ, @WS);
{$ENDIF}
  if WS.ws_Col=0 then
    WS.ws_Col:=80;
  if WS.ws_Row=0 then
    WS.ws_Row:=25;
end;
procedure TslUnixScreen.GetResolution(var x, y: Integer);
begin
  X:= WS.ws_Col;
  Y:= WS.ws_Row;
end;

procedure TslUnixScreen.GetCursorPos(var x, y: Integer);
{$IFNDEF FPC}
var cx, cy: Word;
{$ENDIF}
begin
{$IFNDEF FPC}
   getyx(w,cy,cx);
   x:= cx;
   y:= cy;
{$ELSE}
   getyx(w,y,x);
{$ENDIF}
  inc(x);
  inc(y);
end;
procedure TslUnixScreen.GotoXY(x, y: SmallInt);
begin
   wmove(w,y-1,x-1);
   touchwin(w);
   wrefresh(w);
end;



procedure termResize(asig: Longint); cdecl;
begin
  slScreen.SetResolution(0,0);
end;

procedure termCtrlC(sig :longint);cdecl;
begin
  if Assigned(slScreen.OnCtrlC) then
    slScreen.OnCtrlC();
end;


Function nReadkey(win : pWindow) : Char;
var
   c : Char;
   l : longint;
   xtnded : boolean;
Begin
   l := wgetch(win);
   { if it's an extended key, then map to the IBM values }
   if l > 255 then begin
      xtnded := true;
      c := #27;
      Case l of
         KEY_BREAK : Begin xtnded := false; c := #3; End;
         KEY_BACKSPACE : Begin xtnded := false; c := #8; End;
         KEY_IC    : c := #82; { insert }
         KEY_DC    : c := #83; { delete }
         KEY_HOME  : c := #71; { home }
         KEY_END   : c := #79; { end }
         KEY_UP    : c := #72; { up arrow }
         KEY_DOWN  : c := #80; { down arrow }
         KEY_LEFT  : c := #75; { left arrow }
         KEY_RIGHT : c := #77; { right arrow }
         KEY_NPAGE : c := #81; { page down }
         KEY_PPAGE : c := #73; { page up }
         KEY_ALTA  : c := #30; { alt/a }
         KEY_ALTB  : c := #48;
         KEY_ALTC  : c := #46;
         KEY_ALTD  : c := #32;
         KEY_ALTE  : c := #18;
         KEY_ALTF  : c := #33;
         KEY_ALTG  : c := #34;
         KEY_ALTH  : c := #35;
         KEY_ALTI  : c := #23;
         KEY_ALTJ  : c := #36;
         KEY_ALTK  : c := #37;
         KEY_ALTL  : c := #38;
         KEY_ALTM  : c := #50;
         KEY_ALTN  : c := #49;
         KEY_ALTO  : c := #24;
         KEY_ALTP  : c := #25;
         KEY_ALTQ  : c := #16;
         KEY_ALTR  : c := #19;
         KEY_ALTS  : c := #31;
         KEY_ALTT  : c := #20;
         KEY_ALTU  : c := #22;
         KEY_ALTV  : c := #47;
         KEY_ALTW  : c := #17;
         KEY_ALTX  : c := #45;
         KEY_ALTY  : c := #21;
         KEY_ALTZ  : c := #44;  { alt/z }
         KEY_ALT1  : c := #120; { alt/1 }
         KEY_ALT2  : c := #121; { alt/2 }
         KEY_ALT3  : c := #122; { alt/3 }
         KEY_ALT4  : c := #123; { alt/4 }
         KEY_ALT5  : c := #124; { alt/5 }
         KEY_ALT6  : c := #125; { alt/6 }
         KEY_ALT7  : c := #126; { alt/7 }
         KEY_ALT8  : c := #127; { alt/8 }
         KEY_ALT9  : c := #128; { alt/9 }
         KEY_ALT0  : c := #129; { alt/0 }
         KEY_ALTMINUS : c := #130; { alt/- }
         KEY_ALTEQUAL : c := #131; { alt/= }
         KEY_ALTTAB : c := #15; { alt/tab }
      Else
         Begin
            If l = Key_f(1) Then c := #59 Else
            If l = Key_f(2) Then c := #60 Else
            If l = Key_f(3) Then c := #61 Else
            If l = Key_f(4) Then c := #62 Else
            If l = Key_f(5) Then c := #63 Else
            If l = Key_f(6) Then c := #64 Else
            If l = Key_f(7) Then c := #65 Else
            If l = Key_f(8) Then c := #66 Else
            If l = Key_f(9) Then c := #67 Else
            If l = Key_f(10) Then c := #68 Else
            If l = Key_f(11) Then c := #84 Else
            If l = Key_f(12) Then c := #85 Else
            If l = Key_f(13) Then c := #86 Else
            If l = Key_f(14) Then c := #87 Else
            If l = Key_f(15) Then c := #88 Else
            If l = Key_f(16) Then c := #89 Else
            If l = Key_f(17) Then c := #90 Else
            If l = Key_f(18) Then c := #91 Else
            If l = Key_f(19) Then c := #92 Else
            If l = Key_f(20) Then c := #93;
         End;
      End;
      If xtnded Then Begin
         nReadKey := #0;
         ungetch(ord(c));
         Exit;
      End Else
         nReadkey := c;
   End Else
   if l = 10 then
      nReadkey := #13
   else
      nReadkey := Chr(ord(l));
End;


constructor TslUnixScreen.Create;
var s: String;
    i,j: Integer;
begin
  inherited;

  For i := 0 to 7 Do For j := 0 to 7 do cp[i,j] := (i*8)+j;
  TextAttr:= $07;

  slSignal(SIGWINCH, @termResize);
  slSignal(SIGINT, @termCtrlC);
  slSignal(SIGTSTP, @termCtrlC);

  w:= initscr;
  cbreak;
  noecho;
  keypad(w, TRUE);

      { define the the alt'd keysets for ncurses }
      { alt/a .. atl/z }
      for i := ord('a') to ord('z') do Begin
         s := #27+Chr(i)+#0;
         define_key(@s[1],(KEY_ALTA-97)+i);
      End;
      { alt/1 .. alt/9 }
      for i := 1 to 9 do Begin
         s := #27+Chr(48+i)+#0;
         define_key(@s[1],(KEY_ALT1-1)+i);
      End;
      s := #27+'0'+#0; define_key(@s[1],KEY_ALT0);     { alt/0 }
      s := #27+'-'+#0; define_key(@s[1],KEY_ALTMINUS); { alt/- }
      s := #27+'='+#0; define_key(@s[1],KEY_ALTEQUAL); { alt/= }
      s := #27+#9+#0;  define_key(@s[1],KEY_ALTTAB);   { alt/tab }


      s := #27+#91+#49+#126+#0;  define_key(@s[1],KEY_HOME);   { home }
      s := #27+#91+#52+#126+#0;  define_key(@s[1],KEY_END);   { end }

end;

function TslUnixScreen.KeyPressed: Boolean;
var
   lll : longint;
{   fd : fdSet;}
Begin
   Keypressed := FALSE;
   nodelay(w,TRUE);
   lll := wgetch(w);
   If lll <> -1 Then
   Begin // ERR = -(1) from unit ncurses
      ungetch(lll);
      Keypressed := TRUE;
   End;

   nodelay(w,FALSE);

{ Below is more efficient code, but does not work well with
  nReadkey & extended keys because nReadkey's ungetch does not
  force a change in STDIN. So, a "while keypressed" block does
  not produce the expected results when trapping for char(0)
  followed by a second scan code.

   FD_Zero(fd);
   fd_Set(STDIN,fd);
   Keypressed := (Select(STDIN+1,@fd,nil,nil,0) > 0);
}

end;

function TslUnixScreen.ReadKey: Char;
begin
   Readkey := nReadkey(w);
end;

destructor TslUnixScreen.Destroy;
begin
  inherited;
  endwin;
end;

{ see if the specified attribute is high intensity }
Function nIsBold(att : integer) : boolean;
var       fg,bg : integer;                   { foreground & background }
Begin
   bg := att div 16;
   fg := att - (bg * 16);
   nisbold := (fg > 7);
End;

{ map a curses color to an ibm color }
Function c2ibm(c : integer) : integer;
{ ncurses constants
   COLOR_BLACK   = 0;
   COLOR_RED     = 1;
   COLOR_GREEN   = 2;
   COLOR_YELLOW  = 3;
   COLOR_BLUE    = 4;
   COLOR_MAGENTA = 5;
   COLOR_CYAN    = 6;
   COLOR_WHITE   = 7;
}
Var
   att : integer;
Begin
   Case c of
      COLOR_BLACK   : att := slcblack;
      COLOR_RED     : att := slcred;
      COLOR_GREEN   : att := slcgreen;
      COLOR_YELLOW  : att := slcbrown;
      COLOR_BLUE    : att := slcblue;
      COLOR_MAGENTA : att := slcmagenta;
      COLOR_CYAN    : att := slccyan;
      COLOR_WHITE   : att := slclightgray;
      else att := c;
   End;
   c2ibm := att;
End;


Function ibm2c(c : integer) : integer;
Var
   att : integer;
Begin
   Case c of
      slcblack     : att := COLOR_BLACK;
      slcred       : att := COLOR_RED;
      slcgreen     : att := COLOR_GREEN;
      slcbrown     : att := COLOR_YELLOW;
      slcblue      : att := COLOR_BLUE;
      slcmagenta   : att := COLOR_MAGENTA;
      slccyan      : att := COLOR_CYAN;
      slclightgray : att := COLOR_WHITE;
      else att := c;
   End;
   ibm2c := att;
End;

Function TslUnixScreen.nSetColorPair(att : integer) : integer;
var
   i : integer;
   fg,bg : integer;                   { foreground & background }
Begin
   bg := att div 16;
   fg := att - (bg * 16);
   While bg > 7 Do dec(bg,8);
   While fg > 7 Do dec(fg,8);
   bg := ibm2c(bg);
   fg := ibm2c(fg);
   i := cp[bg,fg];
   init_pair(i,fg,bg);
   nSetColorPair := i;
End;

Function TslUnixScreen.CursesAtts(att : byte) : longint;
Var
   atts : longint;
Begin
   atts := COLOR_PAIR(nSetColorPair(att));
   If nIsBold(att) Then atts := atts or A_BOLD;
   If (att and $80) = $80 Then atts := atts or A_BLINK;
   CursesAtts := atts;
End;

Procedure TslUnixScreen.nWinColor(win : pWindow; att : integer);
Begin
   wattrset(win,CursesAtts(att));
   prev_textattr := att;
End;


Procedure TslUnixScreen.TextBackground(Color: byte);
Begin
   TextAttr:=
    ((Color shl 4) and ($f0 and not slcBlink)) or (TextAttr and ($0f OR slcBlink) );
   nWinColor(w,TextAttr);
End;

{ set the text foreground color }
Procedure TslUnixScreen.TextColor(Color: byte);
Begin
   TextAttr := (Color and $8f) or (TextAttr and $70);
   nWinColor(w,TextAttr);
End;

procedure TslUnixScreen.HighVideo;
begin
  TextColor(TextAttr Or $08);
end;

procedure TslUnixScreen.LowVideo;
begin
  TextColor(TextAttr And $77);
end;

{ clear the specified window }
procedure TslUnixScreen.nClrScr(win : pWindow; att : integer);
Begin
   wbkgd(win,CursesAtts(att));
   TouchWin(win);
   werase(win);
   wrefresh(win);
   prev_textattr := att;
End;


Procedure TslUnixScreen.ClrScr;
Begin
   nClrScr(w,TextAttr);
End;

{ write a string to a window at the current cursor position }
Procedure TslUnixScreen.nWrite(win: pWindow; s: String);
Begin
  If TextAttr <> prev_textattr Then
    nWinColor(win,TextAttr);

  waddstr(win, PAnsiChar(s));
  wrefresh(win);
End;

{=========================================================================
  CrtWrite, CrtRead, CrtReturn, CrtClose, CrtOpen, AssignCrt.
  These functions come from the FPC distribution rtl/linux/crt unit.
  These are the hooks into the input/output stream needed for write(ln)
  and read(ln).
 =========================================================================}

{ used by CrtWrite }
Procedure TslUnixScreen.Write(s : String);
Begin
   nWrite(w,s);
End;


procedure TslUnixScreen.cursoroff;
begin
//
end;

procedure TslUnixScreen.cursoron;
begin
//
end;

procedure TslUnixScreen.SetResolution(const x, y: Integer);
begin
  UpdateCon;
  resizeterm(ws.ws_Row, ws.ws_Col);
  inherited;
end;

function NCurses_version: String;
begin
  Result := AnsiString(curses_version());
end;
{$ENDIF}

end.
