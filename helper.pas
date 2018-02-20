unit helper;

interface

function MyGetPass(prompt: AnsiString): AnsiString;
function MyGetUsername: AnsiString;

implementation

uses SysUtils, StrUtils,
{$IFDEF MSWINDOWS}
  Windows;
{$ELSE}
  {$IFDEF FPC}
    baseunix, pwd, users;
  {$ELSE}
    Libc;
  {$ENDIF}
{$ENDIF}


{$IFDEF MSWINDOWS}
  function MyGetPass(prompt: AnsiString) : AnsiString;
  var
    OldConsInMode, NewConsInMode: DWORD;
    hConsIn: THANDLE;
  begin
    // Turn off console mode echo, since we don't want clear-screen passwords
    System.Reset(Input); //{GetStdHandle(STD_INPUT_HANDLE)}
    hConsIn := TTextRec(Input).Handle;

    if hConsIn = INVALID_HANDLE_VALUE then
    begin
      WriteLn('can''t get handle of STDIN');
      halt;
    end;

    if not GetConsoleMode(hConsIn, OldConsInMode) then
    begin
      WriteLn('can''t get current Console Mode');
      halt(1);
    end;

    NewConsInMode := OldConsInMode and (not ENABLE_ECHO_INPUT);
    if not SetConsoleMode(hConsIn, NewConsInMode) then
    begin
      WriteLn ('unable to turn off Echo');
      halt(1);
    end;

    // Ask for the password
    write (prompt);
    readln (Result);

    // When echo is off and NewUser hits <RETURN>, CR-LF is not echoed, so do it for him
    writeln;
    if not SetConsoleMode(hConsIn, OldConsInMode) then
    begin
      WriteLn('unable to reset previous console mode');
      halt(1);
    end;
  //CloseHandle (hConsIn); //commented because otherwhise it'll except
  end;
{$ELSE}
  function MyGetPass(prompt: AnsiString): AnsiString;
  begin
    {$IFDEF FPC}
      Result := 'blabla'; //StrPas(GetPass(PChar(prompt)));
    {$ELSE}
      Result:= StrPas(GetPass(PChar(prompt)));
    {$ENDIF}
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  function MyGetUsername: AnsiString;
  var
    buf: array[1..256] of AnsiChar;
    n: Cardinal;
  begin
    Result := '';

    if (False <> GetUserName(@buf, n)) then
      Result := Copy(buf, 1, n-1);
  end;
{$ELSE}
  function MyGetUsername: AnsiString;
  var
    pwentry: PPasswordRecord;
  begin
    Result := '';

    // pick up the current user's username
    {$IFDEF FPC}
      {$IFDEF LINUX}
        pwentry := fpgetpwuid(fpgetuid());
      {$ENDIF}
    {$ELSE}
      pwentry := getpwuid(getuid());
    {$ENDIF}
    if (pwentry = nil) then exit;

    Result := StrPas(pwentry^.pw_name);
  end;
{$ENDIF}

end.
