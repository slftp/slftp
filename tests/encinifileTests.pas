unit encinifileTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestEncIniFile = class(TTestCase)
  private
    FFileName: String; //< file used by the tests, removed after each test
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWriteSectionValuesSavesAllValues;
    procedure TestWriteSectionValuesDeletesKeys;
    procedure TestWriteSectionValuesKeepsValuesInMemoryOnSaveFailure;
  end;

implementation

uses
  Classes, SysUtils, encinifile;

{ TTestEncIniFile }

procedure TTestEncIniFile.SetUp;
begin
  FFileName := ExtractFilePath(ParamStr(0)) + 'encinifileTests.dat';
  DeleteFile(FFileName);
end;

procedure TTestEncIniFile.TearDown;
begin
  DeleteFile(FFileName);
end;

procedure TTestEncIniFile.TestWriteSectionValuesSavesAllValues;
var
  fIni: TEncIniFile;
  fValues: TStringList;
begin
  fIni := TEncIniFile.Create(FFileName, 'testpassword', True);
  fValues := TStringList.Create;
  try
    fIni.WriteString('ircnet-TEST', 'bnc_host-0', 'old.host');
    fIni.WriteString('ircnet-TEST', 'nick', 'slftp');

    fValues.Add('bnc_host-0=irc.host');
    fValues.Add('bnc_port-0=6697');
    fIni.WriteSectionValues('ircnet-TEST', fValues);
  finally
    fValues.Free;
    fIni.Free;
  end;

  // read it back from disk to make sure everything has been saved
  fIni := TEncIniFile.Create(FFileName, 'testpassword');
  try
    CheckEquals('irc.host', fIni.ReadString('ircnet-TEST', 'bnc_host-0', ''));
    CheckEquals(6697, fIni.ReadInteger('ircnet-TEST', 'bnc_port-0', 0));
    CheckEquals('slftp', fIni.ReadString('ircnet-TEST', 'nick', ''), 'other keys must stay untouched');
  finally
    fIni.Free;
  end;
end;

procedure TTestEncIniFile.TestWriteSectionValuesDeletesKeys;
var
  fIni: TEncIniFile;
  fValues, fDeleteIdents: TStringList;
begin
  fIni := TEncIniFile.Create(FFileName, 'testpassword', True);
  fValues := TStringList.Create;
  fDeleteIdents := TStringList.Create;
  try
    fIni.WriteString('ircnet-TEST', 'bnc_host-0', 'first.host');
    fIni.WriteString('ircnet-TEST', 'bnc_host-1', 'second.host');
    fIni.WriteString('ircnet-TEST', 'nick', 'slftp');

    fValues.Add('bnc_host-0=second.host');
    fDeleteIdents.Add('bnc_host-1');
    fDeleteIdents.Add('not_existing');
    fIni.WriteSectionValues('ircnet-TEST', fValues, fDeleteIdents);
  finally
    fValues.Free;
    fDeleteIdents.Free;
    fIni.Free;
  end;

  // read it back from disk to make sure everything has been saved
  fIni := TEncIniFile.Create(FFileName, 'testpassword');
  try
    CheckEquals('second.host', fIni.ReadString('ircnet-TEST', 'bnc_host-0', ''));
    CheckEquals('', fIni.ReadString('ircnet-TEST', 'bnc_host-1', ''), 'key must be deleted');
    CheckEquals('slftp', fIni.ReadString('ircnet-TEST', 'nick', ''), 'other keys must stay untouched');
  finally
    fIni.Free;
  end;
end;

procedure TTestEncIniFile.TestWriteSectionValuesKeepsValuesInMemoryOnSaveFailure;
var
  fIni: TEncIniFile;
  fValues: TStringList;
  fSaveFailed: Boolean;
begin
  fIni := TEncIniFile.Create(FFileName, 'testpassword', True);
  fValues := TStringList.Create;
  try
    fIni.WriteString('ircnet-TEST', 'bnc_host-0', 'first.host');
    fIni.WriteString('ircnet-TEST', 'bnc_host-1', 'second.host');

    // saving into a non-existing directory fails
    fIni.Rename(ExtractFilePath(ParamStr(0)) + 'encinifileTests-missing-dir' + PathDelim + 'test.dat', False);

    fValues.Add('bnc_host-0=second.host');
    fValues.Add('bnc_host-1=first.host');
    fSaveFailed := False;
    try
      fIni.WriteSectionValues('ircnet-TEST', fValues);
    except
      fSaveFailed := True;
    end;
    CheckTrue(fSaveFailed, 'saving the file should have failed');

    CheckEquals('second.host', fIni.ReadString('ircnet-TEST', 'bnc_host-0', ''));
    CheckEquals('first.host', fIni.ReadString('ircnet-TEST', 'bnc_host-1', ''));
  finally
    fValues.Free;
    // don't try to save into the missing directory again when freeing, also if a check failed
    fIni.AutoUpdate := False;
    fIni.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('encinifile', TTestEncIniFile.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestEncIniFile);
  {$ENDIF}
end.
