(*
  ncurses interface unit

  Parts of this file are translated from ncurses header file curses.h
  Copyright (c) 1998,1999,2000 Free Software Foundation, Inc.
  Translator: Andrei Borovsky - aborovsky@mtu-net.ru

  This traslation is by no means complete.
  I have included here only those ncurses functions that
  are used (or are planned to be used) in my LinCRT unit.
  I have also added here some routines and constants that
  I've found useful with ncurses.
*)

unit kylixncurses;

interface

type
  chtype = Integer;
  {$EXTERNALSYM chtype}
  PChtype = ^chtype;
  attr_t = chtype;
  {$EXTERNALSYM attr_t}


const

  ERR = -1;
  {$EXTERNALSYM ERR}
  OK = 0;
  {$EXTERNALSYM OK}

  BUTTON1_RELEASED = $1;
  {$EXTERNALSYM BUTTON1_RELEASED}
  BUTTON1_PRESSED = $2;
  {$EXTERNALSYM BUTTON1_PRESSED}
  BUTTON1_CLICKED = $4;
  {$EXTERNALSYM BUTTON1_CLICKED}
  BUTTON1_DOUBLE_CLICKED = $8;
  {$EXTERNALSYM BUTTON1_DOUBLE_CLICKED}
  BUTTON2_RELEASED = $40;
  {$EXTERNALSYM BUTTON2_RELEASED}
  BUTTON2_PRESSED = $80;
  {$EXTERNALSYM BUTTON2_PRESSED}
  BUTTON2_CLICKED = $100;
  {$EXTERNALSYM BUTTON2_CLICKED}
  BUTTON2_DOUBLE_CLICKED = $200;
  {$EXTERNALSYM BUTTON2_DOUBLE_CLICKED}
  BUTTON3_RELEASED = $1000;
  {$EXTERNALSYM BUTTON3_RELEASED}
  BUTTON3_PRESSED = $2000;
  {$EXTERNALSYM BUTTON3_PRESSED}
  BUTTON3_CLICKED = $4000;
  {$EXTERNALSYM BUTTON3_CLICKED}
  BUTTON3_DOUBLE_CLICKED = $8000;
  {$EXTERNALSYM BUTTON3_DOUBLE_CLICKED}
  REPORT_MOUSE_POSITION = $8000000;
  {$EXTERNALSYM REPORT_MOUSE_POSITION}

  COLOR_BLACK = 0;
  {$EXTERNALSYM COLOR_BLACK}
  COLOR_RED = 1;
  {$EXTERNALSYM COLOR_RED}
  COLOR_GREEN = 2;
  {$EXTERNALSYM COLOR_GREEN}
  COLOR_YELLOW = 3;
  {$EXTERNALSYM COLOR_YELLOW}
  COLOR_BLUE = 4;
  {$EXTERNALSYM COLOR_BLUE}
  COLOR_MAGENTA = 5;
  {$EXTERNALSYM COLOR_MAGENTA}
  COLOR_CYAN = 6;
  {$EXTERNALSYM COLOR_CYAN}
  COLOR_WHITE = 7;
  {$EXTERNALSYM COLOR_WHITE}

  A_STANDOUT = 1 shl 16;
  {$EXTERNALSYM A_STANDOUT}
  A_UNDERLINE = 1 shl 17;
  {$EXTERNALSYM A_UNDERLINE}
  A_REVERSE = 1 shl 18;
  {$EXTERNALSYM A_REVERSE}
  A_BLINK = 1 shl 19;
  {$EXTERNALSYM A_BLINK}
  A_DIM = 1 shl 20;
  {$EXTERNALSYM A_DIM}
  A_BOLD = 1 shl 21;
  {$EXTERNALSYM A_BOLD}
  A_ALTCHARSET = 1 shl 22;
  {$EXTERNALSYM A_ALTCHARSET}
  A_INVIS = 1 shl 23;
  {$EXTERNALSYM A_INVIS}

const
   KEY_CODE_YES = 256;  { A wchar_t contains a key code    &0400 }
   KEY_MIN = 257;       { Minimum curses key    &0401 }
   KEY_BREAK = 257;     { Break key (unreliable)   &0401 }
   KEY_SRESET = 344;    { Soft (partial) reset (unreliable)    &0530 }
   KEY_RESET = 345;     { Reset or hard reset (unreliable)    &0531 }
   KEY_DOWN = 258;      { down-arrow key    &0402 }
   KEY_UP = 259;        { up-arrow key    &0403 }
   KEY_LEFT = 260;      { left-arrow key    &0404 }
   KEY_RIGHT = 261;     { right-arrow key    &0405 }
   KEY_HOME = 262;      { home key    &0406 }
   KEY_BACKSPACE = 263; { backspace key    &0407 }
   KEY_F0 = 264;        { Function keys.  Space for 64    &0410 }

{ Manually Added KEY_F1 .. KEY_F12 }

   KEY_F1 = KEY_F0 + 1;
   KEY_F2 = KEY_F0 + 2;
   KEY_F3 = KEY_F0 + 3;
   KEY_F4 = KEY_F0 + 4;
   KEY_F5 = KEY_F0 + 5;
   KEY_F6 = KEY_F0 + 6;
   KEY_F7 = KEY_F0 + 7;
   KEY_F8 = KEY_F0 + 8;
   KEY_F9 = KEY_F0 + 9;
   KEY_F10 = KEY_F0 + 10;
   KEY_F11 = KEY_F0 + 11;
   KEY_F12 = KEY_F0 + 12;

function KEY_F(n : Byte) : chtype; 

const
   KEY_DL = 328;        { delete-line key    &0510 }
   KEY_IL = 329;        { insert-line key    &0511 }
   KEY_DC = 330;        { delete-character key    &0512 }
   KEY_IC = 331;        { insert-character key    &0513 }
   KEY_EIC = 332;       { sent by rmir or smir in insert mode    &0514 }
   KEY_CLEAR = 333;     { clear-screen or erase key    &0515 }
   KEY_EOS = 334;       { clear-to-end-of-screen key    &0516 }
   KEY_EOL = 335;       { clear-to-end-of-line key    &0517 }
   KEY_SF = 336;        { scroll-forward key    &0520 }
   KEY_SR = 337;        { scroll-backward key    &0521 }
   KEY_NPAGE = 338;     { next-page key    &0522 }
   KEY_PPAGE = 339;     { previous-page key    &0523 }
   KEY_STAB = 340;      { set-tab key    &0524 }
   KEY_CTAB = 341;      { clear-tab key    &0525 }
   KEY_CATAB = 342;     { clear-all-tabs key    &0526 }
   KEY_ENTER = 343;     { enter/send key    &0527 }
   KEY_PRINT = 346;     { print key    &0532 }
   KEY_LL = 347;        { lower-left key (home down)    &0533 }
   KEY_A1 = 348;        { upper left of keypad    &0534 }
   KEY_A3 = 349;        { upper right of keypad    &0535 }
   KEY_B2 = 350;        { center of keypad    &0536 }
   KEY_C1 = 351;        { lower left of keypad    &0537 }
   KEY_C3 = 352;        { lower right of keypad    &0540 }
   KEY_BTAB = 353;      { back-tab key    &0541 }
   KEY_BEG = 354;       { begin key    &0542 }
   KEY_CANCEL = 355;    { cancel key    &0543 }
   KEY_CLOSE = 356;     { close key    &0544 }
   KEY_COMMAND = 357;   { command key   &0545  }
   KEY_COPY = 358;      { copy key    &0546 }
   KEY_CREATE = 359;    { create key    &0547 }
   KEY_END = 360;       { end key    &0550 }
   KEY_EXIT = 361;      { exit key    &0551 }
   KEY_FIND = 362;      { find key    &0552 }
   KEY_HELP = 363;      { help key    &0553 }
   KEY_MARK = 364;      { mark key    &0554 }
   KEY_MESSAGE = 365;   { message key    &0555 }
   KEY_MOVE = 366;      { move key    &0556 }
   KEY_NEXT = 367;      { next key    &0557 }
   KEY_OPEN = 368;      { open key    &0560 }
   KEY_OPTIONS = 369;   { options key    &0561 }
   KEY_PREVIOUS = 370;  { previous key    &0562 }
   KEY_REDO = 371;      { redo key    &0563 }
   KEY_REFERENCE = 372; { reference key    &0564 }
   KEY_REFRESH = 373;   { refresh key    &0565 }
   KEY_REPLACE = 374;   { replace key    &0566 }
   KEY_RESTART = 375;   { restart key    &0567 }
   KEY_RESUME = 376;    { resume key    &0570 }
   KEY_SAVE = 377;      { save key    &0571 }
   KEY_SBEG = 378;      { shifted begin key    &0572 }
   KEY_SCANCEL = 379;   { shifted cancel key    &0573 }
   KEY_SCOMMAND = 380;  { shifted command key    &0574 }
   KEY_SCOPY = 381;     { shifted copy key    &0575 }
   KEY_SCREATE = 382;   { shifted create key    &0576 }
   KEY_SDC = 383;       { shifted delete-character key    &0577 }
   KEY_SDL = 384;       { shifted delete-line key    &0600 }
   KEY_SELECT = 385;    { select key    &0601 }
   KEY_SEND = 386;      { shifted end key    &0602 }
   KEY_SEOL = 387;      { shifted clear-to-end-of-line key    &0603 }
   KEY_SEXIT = 388;     { shifted exit key    &0604 }
   KEY_SFIND = 389;     { shifted find key    &0605 }
   KEY_SHELP = 390;     { shifted help key    &0606 }
   KEY_SHOME = 391;     { shifted home key    &0607 }
   KEY_SIC = 392;       { shifted insert-character key    &0610 }
   KEY_SLEFT = 393;     { shifted left-arrow key    &0611 }
   KEY_SMESSAGE = 394;  { shifted message key    &0612 }
   KEY_SMOVE = 395;     { shifted move key    &0613 }
   KEY_SNEXT = 396;     { shifted next key    &0614 }
   KEY_SOPTIONS = 397;  { shifted options key    &0615 }
   KEY_SPREVIOUS = 398; { shifted previous key    &0616 }
   KEY_SPRINT = 399;    { shifted print key   &0617 }
   KEY_SREDO = 400;     { shifted redo key    &0620 }
   KEY_SREPLACE = 401;  { shifted replace key    &0621 }
   KEY_SRIGHT = 402;    { shifted right-arrow key    &0622 }
   KEY_SRSUME = 403;    { shifted resume key    &0623 }
   KEY_SSAVE = 404;     { shifted save key    &0624 }
   KEY_SSUSPEND = 405;  { shifted suspend key    &0625 }
   KEY_SUNDO = 406;     { shifted undo key    &0626 }
   KEY_SUSPEND = 407;   { suspend key    &0627 }
   KEY_UNDO = 408;      { undo key     &0630}
   KEY_MOUSE = 409;     { Mouse event has occurred    &0631 }
   KEY_RESIZE = 410;    { Terminal resize event    &0632 }
   KEY_EVENT = 411;     { We were interrupted by an event    &0633 }
   KEY_MAX = 511;       { Maximum key value is 0633    &0777 }


type

  NCURSES_COLOR_T = Word;
  {$EXTERNALSYM NCURSES_COLOR_T}
  NCURSES_SIZE_T = Word;
  {$EXTERNALSYM NCURSES_SIZE_T}
  mmask_t = Integer;
  {$EXTERNALSYM mmask_t}

  MEVENT = record
    id : Word;
    x, y, z : Integer;
    bstate : mmask_t;
  end;
  {$EXTERNALSYM MEVENT}

  ldat = record
    text : PChtype;
    firstchar : NCURSES_SIZE_T;
    lastchar : NCURSES_SIZE_T;
    oldindex : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM ldat}

  PLdat = ^ldat;

  pdat = record
    _pad_y, _pad_x : NCURSES_SIZE_T;
    _pad_top, _pad_left : NCURSES_SIZE_T;
    _pad_bottom, _pad_right : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM pdat}

  PWINDOW = ^_win_st;
  _win_st = record
    _cury, _curx : NCURSES_SIZE_T;
    _maxy, _maxx : NCURSES_SIZE_T;
    _begy, _begx : NCURSES_SIZE_T;
    _flags : Word;
    _attrs : attr_t;
    _bkgd : chtype;
    _notimeout : Boolean;
    _clear : Boolean;
    _leaveok : Boolean;
    _scroll : Boolean;
    _idlok : Boolean;
    _idcok : Boolean;
    _immed : Boolean;
    _sync : Boolean;
    _use_keypad : Boolean;
    _delay : Integer;
    _line : PLdat;
    _regtop : NCURSES_SIZE_T;
    _regbottom : NCURSES_SIZE_T;
    _parx : Integer;
    _pary : Integer;
    _parent : PWINDOW;
    _pad : pdat;
    _yoffset : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM _win_st}

  WINDOW = _win_st;
  {$EXTERNALSYM WINDOW}

  function cbreak : Integer; cdecl;
  {$EXTERNALSYM cbreak}
  function COLOR_PAIR(n : Integer) : Integer;
  function def_prog_mode : Integer; cdecl;
  {$EXTERNALSYM def_prog_mode}
  function define_key(definitions : PChar; keycode : Integer) : Integer; cdecl;
  {$EXTERNALSYM define_key}
  function delwin(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM delwin}
  function echo : Integer; cdecl;
  {$EXTERNALSYM echo}
  function endwin : Integer; cdecl;
  {$EXTERNALSYM endwin}
  function erasechar : Integer; cdecl;
  {$EXTERNALSYM erasechar}
  function getmouse(var event : MEVENT) : Integer; cdecl;
  {$EXTERNALSYM getmouse}
  function getyx(win : PWINDOW; var y, x : Word) : Integer;
  {$EXTERNALSYM getyx}
  function init_pair(pair, f, b : Word) : Integer; cdecl; //i
  {$EXTERNALSYM init_pair}
  function initscr : PWINDOW; cdecl;
  {$EXTERNALSYM initscr}
  function keypad(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM keypad}
  function leaveok(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM leaveok}
  function mousemask(newmask : mmask_t; var oldmask : mmask_t) : mmask_t; cdecl;
  {$EXTERNALSYM mousemask}
  function mvwaddch(win : PWINDOW; y, x : Integer; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM mvwaddch}
  function mvwin(win : PWINDOW; y, x : Integer) : Integer; cdecl;
  {$EXTERNALSYM mvwin}
  function NCURSES_BITS(mask, shift : Integer) : Integer;
  function newwin(nlines, ncols, begin_y, begin_x : Integer) : PWINDOW; cdecl;
  {$EXTERNALSYM newwin}
  function nl : Integer; cdecl;
  {$EXTERNALSYM nl}
  function nocbreak : Integer; cdecl;
  {$EXTERNALSYM nocbreak}
  function nodelay(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM nodelay}
  function noecho : Integer; cdecl;
  {$EXTERNALSYM noecho}
  function notimeout(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM notimeout}
  function pechochar(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM pechochar}
  function resizeterm(lines, columns : Integer) : Integer; cdecl;
  {$EXTERNALSYM resizeterm}
  function scroll(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM scroll}
  function scrollok(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM scrollok}
  function set_attr(win : PWINDOW; attr : Integer) : Integer;
  function start_color : Integer; cdecl;
  {$EXTERNALSYM start_color}
  function timeout(delay : Integer) : Integer; cdecl;
  {$EXTERNALSYM timeout}
  function ungetch(ch : Integer) : Integer; cdecl;
  {$EXTERNALSYM ungetch}
  function unset_attr(win : PWINDOW; attr : Integer) : Integer;
  function waddch(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM waddch}
  function waddstr(win : PWINDOW; str : PChar) : Integer; cdecl;
  {$EXTERNALSYM waddstr}
  function wattr_get(win : PWINDOW; var attrs : attr_t; var pair : Word; opts : Pointer) : Integer; cdecl;
  {$EXTERNALSYM wattr_get}
  function wattrset(win : PWINDOW; attrs : Integer) : Integer;
  {$EXTERNALSYM wattrset}
  function wclear(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wclear}
  function wclrtoeol(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wclrtoeol}
  function wcolor_set(win : PWINDOW; Colors : Word; opts : Pointer) : Integer; cdecl;
  {$EXTERNALSYM wcolor_set}
  function wdelch(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wdelch}
  function wdeleteln(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wdeleteln}
  function wenclose(win : PWindow; y, x : Integer) : Boolean; cdecl;
  {$EXTERNALSYM wenclose}
  function werase(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM werase}
  function wgetch(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wgetch}
  function wgetnstr(win : PWINDOW; str : PChar; n : Integer) : Integer; cdecl;
  {$EXTERNALSYM wgetnstr}
  function winch(win : PWINDOW) : chtype; cdecl;
  {$EXTERNALSYM winch}
  function winsch(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM winsch}
  function winsdelln(win : PWINDOW; n : Integer) : Integer; cdecl;
  {$EXTERNALSYM winsdelln}
  function winsstr(win : PWINDOW; str : PChar) : Integer; cdecl;
  {$EXTERNALSYM winsstr}
  function wmouse_trafo(win : PWINDOW; var pY, pX : Integer; to_screen : Boolean) : Boolean; cdecl;
  {$EXTERNALSYM wmouse_trafo}
  function wmove(win : PWINDOW; y, x : Integer) : Integer; cdecl;
  {$EXTERNALSYM wmove}
  function wrefresh(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wrefresh}
  function wtouchln(_para1:PWINDOW; _para2:Longint; _para3:Longint; _para4:Longint):Longint; cdecl;
  {$EXTERNALSYM wtouchln}
    function wbkgd(_para1:PWINDOW; _para2:chtype):Longint; cdecl;
  {$EXTERNALSYM wbkgd}
  function curses_version():PChar; cdecl;
  {$EXTERNALSYM curses_version}


  function touchwin(win: PWINDOW): longint;
  function getmaxy(win: PWINDOW): Smallint; 


implementation

const

  libname = 'libncurses.so';

  function cbreak : Integer; cdecl; external libname name 'cbreak';
  function def_prog_mode : Integer; cdecl; external libname name 'def_prog_mode';
  function define_key(definitions : PChar; keycode : Integer) : Integer; cdecl; external libname name 'define_key';
  function delwin(win : PWINDOW) : Integer; cdecl; external libname name 'delwin';
  function echo : Integer; cdecl; external libname name 'echo';
  function endwin : Integer; cdecl; external libname name 'endwin';
  function erasechar : Integer; cdecl; external libname name 'erasechar';
  function getmouse(var event : MEVENT) : Integer; cdecl; external libname name 'getmouse';
  function init_pair(pair, f, b : Word) : Integer; cdecl; external libname name 'init_pair';
  function initscr : PWINDOW; cdecl; external libname name 'initscr';
  function keypad(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'keypad';
  function leaveok(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'leaveok';
  function mousemask(newmask : mmask_t; var oldmask : mmask_t) : mmask_t; cdecl; external libname name 'mousemask';
  function mvwaddch(win : PWINDOW; y, x : Integer; ch : chtype) : Integer; cdecl; external libname name 'mvwaddch';
  function mvwin(win : PWINDOW; y, x : Integer) : Integer; cdecl; cdecl; external libname name 'mvwin';
  function newwin(nlines, ncols, begin_y, begin_x : Integer) : PWINDOW; cdecl; external libname name 'newwin';
  function nl : Integer; cdecl; external libname name 'nl';
  function nocbreak : Integer; cdecl; external libname name 'nocbreak';
  function nodelay(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'nodelay';
  function noecho : Integer; cdecl; external libname name 'noecho';
  function notimeout(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'notimeout';
  function pechochar(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'pechochar';
  function resizeterm(lines, columns : Integer) : Integer; cdecl; external libname name 'resizeterm';
  function scroll(win : PWINDOW) : Integer; cdecl; external libname name 'scroll';
  function scrollok(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'scrollok';
  function start_color : Integer; cdecl; external libname name 'start_color';
  function timeout(delay : Integer) : Integer; cdecl; external libname name 'timeout';
  function ungetch(ch : Integer) : Integer; cdecl; cdecl; external libname name 'ungetch';
  function waddch(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'waddch';
  function waddstr(win : PWINDOW; str : PChar) : Integer; cdecl; external libname name 'waddstr';
  function wattr_get(win : PWINDOW; var attrs : attr_t; var pair : Word; opts : Pointer) : Integer; cdecl; external libname name 'wattr_get';
//  function wattrset(win : PWindow; attrs : Integer) : Integer; cdecl; external libname name 'wattrset';
  function wclear(win : PWINDOW) : Integer; cdecl; external libname name 'wclear';
  function wclrtoeol(win : PWINDOW) : Integer; cdecl; external libname name 'wclrtoeol';
  function wcolor_set(win : PWINDOW; Colors : Word; opts: Pointer) : Integer; cdecl; external libname name 'wcolor_set';
  function wdelch(win : PWINDOW) : Integer; cdecl; external libname name 'wdelch';
  function wdeleteln(win :PWINDOW) : Integer; cdecl; external libname name 'wdeleteln';
  function wenclose(win : PWINDOW; y, x : Integer) : Boolean; cdecl; external libname name 'wenclose';
  function werase(win : PWINDOW) : Integer; cdecl; external libname name 'werase';
  function wgetch(win : PWINDOW) : Integer; cdecl; external libname name 'wgetch';
  function wgetnstr(win : PWINDOW; str : PChar; n : Integer) : Integer; cdecl;  external libname name 'wgetnstr';
  function winch(win : PWINDOW) : chtype; cdecl; external libname name 'winch';
  function winsch(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'winsch';
  function winsdelln(win : PWINDOW; n : Integer) : Integer; cdecl; external libname name 'winsdelln';
  function winsstr(win : PWINDOW; str : PChar) : Integer; cdecl; external libname name 'winsstr';
  function wmouse_trafo(win : PWINDOW; var pY, pX : Integer; to_screen : Boolean) : Boolean;  cdecl; external libname name 'wmouse_trafo';
  function wmove(win : PWINDOW; y, x : Integer) : Integer; cdecl; external libname name 'wmove';
  function wrefresh(win : PWINDOW) : Integer; cdecl; external libname name 'wrefresh';
  function wtouchln(_para1:PWINDOW; _para2:Longint; _para3:Longint; _para4:Longint):Longint; cdecl; external libname name 'wtouchln';
  function wbkgd(_para1:PWINDOW; _para2:chtype):Longint; cdecl;external libname name 'wbkgd';
  function curses_version:PChar;cdecl;external  libname name 'curses_version';

  function getyx;
  begin
    y := win._cury;
    x := win._curx;
    Result := 0;
  end;

  function set_attr;
  begin
    win._attrs := win._attrs or attr;
    Result := 0;
  end;

  function unset_attr;
  begin
    win._attrs := win._attrs and not attr;
    Result := 0;
  end;

  function NCURSES_BITS;
  begin
    Result := mask shl (shift+8);
  end;

  function COLOR_PAIR;
  begin
    Result := NCURSES_BITS(n, 0);
  end;

  function wattrset;
  begin
    win._attrs := attrs;
    Result:=0;
  end;

function KEY_F(n : Byte) : chtype;
begin
  KEY_F := KEY_F0 + n;
end;


function getmaxy(win: PWINDOW): Smallint;
begin
  if win<>nil then
    getmaxy:=(win^._maxy) + 1
  else
    getmaxy:=ERR;
end;

function touchwin(win: PWINDOW): longint;
begin
  touchwin:=wtouchln(win,0,getmaxy(win),1);
end;

  
end.

