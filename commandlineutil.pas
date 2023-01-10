unit commandlineutil;

interface

{ Parses the command line input and executes the wanted functionality
    @param(aBinaryName Filename of the executable)
    @param(aCmdLine Command line input) }
procedure ParseCommandLine(const aBinaryName, aCmdLine: String);

implementation

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    process, baseunix, pwd, users,
  {$ENDIF}
  rcmdline, encinifile, versioninfo;

procedure _ShowFullHelpInformation(const aCmdLineReaderHelp, aBinaryName: String);
begin
  WriteLn(aCmdLineReaderHelp);
  WriteLn('Synopsis:');
  WriteLn(Format('  %s -d --pw --infile=slftp.rules --outfile=rules.txt', [aBinaryName]));
  WriteLn(Format('  %s -e --infile=rules.plain --outfile=slftp.rules --pw', [aBinaryName]));
  WriteLn(Format('  %s -d --infile=slftp.rules --outfile=rules.txt --pf=key.txt', [aBinaryName]));
  WriteLn(Format('  %s -e --pf=masterpass.txt --infile=rules.plain --outfile=slftp.rules', [aBinaryName]));
end;

procedure _WipeString(var aPassword: String);
var
  i: integer;
begin
  aPassword := '@';
  for I := 0 to 5 do
    aPassword := aPassword;

  aPassword := '|';
  for I := 0 to 10 do
    aPassword := aPassword;

  aPassword := '^';
  for I := 0 to 15 do
    aPassword := aPassword;

  aPassword := '[';
  for I := 0 to 20 do
    aPassword := aPassword;

  aPassword := '';
end;

function _AskUserForPassword(const aMessagePrompt: String): String;
var
  {$IFDEF MSWINDOWS}
    fConsoleHandle: THANDLE;
    fOldConsoleMode, fNewConsoleMode: DWORD;
  {$ELSE}
    fEchoOff, fEchoOn: TProcess;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    // turn off console mode echo, since we don't want clear-screen passwords
    System.Reset(Input);
    fConsoleHandle := TTextRec(Input).Handle;
    if fConsoleHandle = INVALID_HANDLE_VALUE then
    begin
      WriteLn('can''t get handle of STDIN');
      Halt(1);
    end;
    if not GetConsoleMode(fConsoleHandle, fOldConsoleMode) then
    begin
      WriteLn('can''t get current Console Mode');
      Halt(1);
    end;
    fNewConsoleMode := fOldConsoleMode and (not ENABLE_ECHO_INPUT);
    if not SetConsoleMode(fConsoleHandle, fNewConsoleMode) then
    begin
      WriteLn('unable to turn off echo');
      Halt(1);
    end;

    // ask for the password
    write(aMessagePrompt);
    ReadLn(Result);
    // when echo is off and NewUser hits <RETURN>, CR-LF is not echoed, so do it for him
    writeln;

    if not SetConsoleMode(fConsoleHandle, fOldConsoleMode) then
    begin
      WriteLn('unable to reset previous console mode');
      Halt(1);
    end;
  {$ELSE}
    fEchoOff := TProcess.Create(nil);
    fEchoOn := TProcess.Create(nil);
    try
      fEchoOff.Executable := 'stty';
      fEchoOff.Parameters.add('-echo');
      fEchoOn.Executable := 'stty';
      fEchoOn.Parameters.add('echo');

      fEchoOff.Execute;
      write(aMessagePrompt);
      ReadLn(Result);
      // when echo is off and NewUser hits <RETURN>, CR-LF is not echoed, so do it for him
      WriteLn;

    finally
      fEchoOn.Execute;
      fEchoOff.Free;
      fEchoOn.Free;
    end;
  {$ENDIF}
end;

function _ReadPasswordFile(const aPwFile: String): String;
var
  fStrList: TStringlist;
begin
  fStrList := TStringlist.Create;
  try
    if FileExists(aPwFile) then
    begin
      fStrList.LoadFromFile(aPwFile);
      Result := fStrList.Strings[0];
    end
    else
    begin
      WriteLn(Format('Passwordfile %s does not exist!', [aPwFile]));
      Result := '';
    end;
  finally
    fStrList.Free;
  end;
end;

procedure _DecryptFile(const aInputfile, aOutputfile: String; const aPassword: String);
var
  fEncStrList: TEncStringlist;
  fClearStrList: TStringList;
  fUserPassword: String;
begin
  try
    if aPassword = '' then
      fUserPassword := _AskUserForPassword('Password: ')
    else
      fUserPassword := aPassword;

    try
      fEncStrList := TEncStringlist.Create(fUserPassword);
      fClearStrList := TStringList.Create;
      try
        fEncStrList.LoadFromFile(aInputfile);
        fClearStrList.Assign(fEncStrList);
        fClearStrList.SaveToFile(aOutputfile);
      finally
        fEncStrList.free;
        fClearStrList.free;
      end;
    finally
      _WipeString(fUserPassword);
    end;

    WriteLn('Done.');
  except on E: Exception do
    begin
      WriteLn('Decryption failed with error:');
      WriteLn(E.Message);
    end;
  end;
end;

procedure _EncryptFile(const aInputfile, aOutputfile: String; const aPassword: String);
var
  fEncStrList: TEncStringlist;
  fClearStrList: TStringList;
  fUserPassword: String;
begin
  try
    if aPassword = '' then
    begin
      fUserPassword := _AskUserForPassword('Password: ');
      if _AskUserForPassword('Repeat Password: ') <> fUserPassword then
      begin
        Writeln('Passwords do not match!');
        Halt(1);
      end;
    end
    else
    begin
      fUserPassword := aPassword;
    end;

    try
      fEncStrList := TEncStringlist.Create(fUserPassword);
      fClearStrList := TStringList.Create;
      try
        fClearStrList.LoadFromFile(aInputfile);
        fEncStrList.Assign(fClearStrList);
        fEncStrList.SaveToFile(aOutputfile);
      finally
        fEncStrList.free;
        fClearStrList.free;
      end;
    finally
      _WipeString(fUserPassword);
    end;

    WriteLn('Done.');
  except on E: Exception do
    begin
      WriteLn('Encryption failed with error:');
      WriteLn(E.Message);
    end;
  end;
end;

procedure ParseCommandLine(const aBinaryName, aCmdLine: String);
const
  PW_PWFILE_MODE_ERR = 'Password and passwordfile mode cannot be used at the same time!';
  PWFILE_NOTEXIST_ERR = 'Specified password file does not exist, aborting.';
  PW_MODE_HEADER = '___ using password mode ___';
  PF_MODE_HEADER = '___ using passwordfile mode ___';
var
  fIsHelpMode: Boolean;
  fIsShowVersionMode: Boolean;
  fIsEncryptMode: Boolean;
  fIsDecryptMode: Boolean;
  fIsPasswordMode: Boolean;
  fPasswordFile: String;
  fInputFilename: String;
  fOutputFilename: String;
  fCmdLineReaderHelp: String;
  fCmdLineReader: TCommandLineReader;
  fVersion: string;
  fPassword: String;
begin
  fCmdLineReader := TCommandLineReader.Create;
  try
    try
      fCmdLineReader.allowDOSStyle := True;

      fCmdLineReader.declareFlag('help', 'Show detailed help');
      fCmdLineReader.addAbbreviation('h', 'help');
      fCmdLineReader.declareFlag('version', 'Show version information');
      fCmdLineReader.addAbbreviation('v', 'version');

      fCmdLineReader.beginDeclarationCategory('Funtions for encrypting/decrypting internal files:');
      fCmdLineReader.declareFlag('encrypt', 'Encrypt given file');
      fCmdLineReader.addAbbreviation('e', 'encrypt');
      fCmdLineReader.declareFlag('decrypt', 'Decrypt given file');
      fCmdLineReader.addAbbreviation('d', 'decrypt');
      fCmdLineReader.declareFlag('pw', 'Will ask for the password in the next step (command line prompt)');
      fCmdLineReader.declareFile('pf', 'Filename in which the password is stored as plain text', '');
      fCmdLineReader.declareFile('infile', 'Filename for the input file to be encrypted/decrypted', '');
      fCmdLineReader.declareFile('outfile', 'Filename for the encrypted/decrypted output file', '');

      fCmdLineReader.parse(aCmdLine);
    except
      on e: Exception do
      begin
        WriteLn(e.Message);
        exit;
      end;
    end;

    fIsHelpMode := fCmdLineReader.readFlag('help');
    fIsShowVersionMode := fCmdLineReader.readFlag('version');
    fIsEncryptMode := fCmdLineReader.readFlag('encrypt');
    fIsDecryptMode := fCmdLineReader.readFlag('decrypt');
    fIsPasswordMode := fCmdLineReader.readFlag('pw');
    fPasswordFile := fCmdLineReader.readString('pf');
    fInputFilename := fCmdLineReader.readString('infile');
    fOutputFilename := fCmdLineReader.readString('outfile');

    if fIsHelpMode then
    begin
      fCmdLineReaderHelp := fCmdLineReader.availableOptions;
      _ShowFullHelpInformation(fCmdLineReaderHelp, aBinaryName);
    end
    else if fIsShowVersionMode then
    begin
      fVersion := GetFullVersionString;
      WriteLn(fVersion);
    end
    else if fIsEncryptMode and fIsDecryptMode then
    begin
      WriteLn('Cannot encrypt and decrypt at the same time!');
    end
    else if fInputFilename.IsEmpty or fOutputFilename.IsEmpty then
    begin
      WriteLn('You need to specify an input and output filename!');
    end
    else if fInputFilename = fOutputFilename then
    begin
      WriteLn('Do not use the same input and output filenames! It will destroy your input file if you mistype the password!');
    end
    else if not FileExists(fInputFilename) then
    begin
      WriteLn('Specified input file does not exist.');
    end
    else if FileExists(fOutputFilename) then
    begin
      WriteLn('Specified output file does already exist, aborting.');
    end
    else if not fIsPasswordMode and fPasswordFile.IsEmpty then
    begin
      WriteLn('You have to define the input method for the password!');
    end
    else if fIsEncryptMode then
    begin
      if fIsPasswordMode and fPasswordFile.IsEmpty then
      begin
        WriteLn(PW_MODE_HEADER);
        _EncryptFile(fInputFilename, fOutputFilename, '');
      end
      else if not fIsPasswordMode and not fPasswordFile.IsEmpty then
      begin
        WriteLn(PF_MODE_HEADER);
        if FileExists(fPasswordFile) then
        begin
          fPassword := _ReadPasswordFile(fPasswordFile);
          _EncryptFile(fInputFilename, fOutputFilename, fPassword);
        end
        else
          WriteLn(PWFILE_NOTEXIST_ERR);
      end
      else
      begin
        WriteLn(PW_PWFILE_MODE_ERR);
      end;
    end
    else if fIsDecryptMode then
    begin
      if fIsPasswordMode and fPasswordFile.IsEmpty then
      begin
        WriteLn(PW_MODE_HEADER);
        _DecryptFile(fInputFilename, fOutputFilename, '');
      end
      else if not fIsPasswordMode and not fPasswordFile.IsEmpty then
      begin
        WriteLn(PF_MODE_HEADER);
        if FileExists(fPasswordFile) then
        begin
          fPassword := _ReadPasswordFile(fPasswordFile);
          _DecryptFile(fInputFilename, fOutputFilename, fPassword);
        end
        else
          WriteLn(PWFILE_NOTEXIST_ERR);
      end
      else
      begin
        WriteLn(PW_PWFILE_MODE_ERR);
      end;
    end
    else
    begin
      fCmdLineReaderHelp := fCmdLineReader.availableOptions;
      _ShowFullHelpInformation(fCmdLineReaderHelp, aBinaryName);
    end;
  finally
    fCmdLineReader.Free;
  end;
end;

end.
