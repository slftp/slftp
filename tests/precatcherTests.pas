unit precatcherTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestPrecatcher = class(TTestCase)
  published
    procedure TestFindSection1;
    procedure TestFindSection2;
    procedure TestFindSection3;
    procedure TestFindSection4;
  end;

implementation

uses
  SysUtils, precatcher;

{ TTestPrecatcher }

procedure TTestPrecatcher.TestFindSection1;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEWRACE - ENG-TV - Family.Feud.AU.2016.03.24.WEB.h264-spamTV - started by foob4r';
  fExpectedSection := 'ENGTV';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection2;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'New ChArTs - VA-Swiss_Top_100_Single_Charts_28.11.2021-AUDiAL_iNT - started by boon';
  fExpectedSection := 'MP3';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection3;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEW GER-CHARTS - VA-Swiss_Top_100_Single_Charts_28.11.2021-AUDiAL_iNT - started by foob4r';
  fExpectedSection := 'CHARTS';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

procedure TTestPrecatcher.TestFindSection4;
var
  fInputStr, fExpectedSection: String;
begin
  fInputStr := 'NEW in TV-HD - Tacoma.FD.S03E02.1080p.WEB.h264-KOGi by poweruser';
  fExpectedSection := '';

  CheckEqualsString(fExpectedSection, FindSection(fInputStr), 'Finding section failed!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('precatcher', TTestPrecatcher.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestPrecatcher);
  {$ENDIF}
end.
