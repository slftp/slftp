unit kbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestKb = class(TTestCase)
  published
    procedure TestGetGroupname1;
    procedure TestGetGroupname2;
    procedure TestGetGroupname3;
    procedure TestGetGroupname4;
    procedure TestGetGroupname5;
  end;

implementation

uses
  SysUtils, kb;

{ TTestKb }

procedure TTestKb.TestGetGroupname1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'American.Ninja.Warrior.S11E01.1080p.WEB.h264-TBS';
  fExpectedResultStr := 'TBS';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestKb.TestGetGroupname2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Altstadt_Echo-This_Work_Contains_Lead-(SEMANTICA109)-WEB-2019-AGITB';
  fExpectedResultStr := 'AGITB';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestKb.TestGetGroupname3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz_iNT';
  fExpectedResultStr := 'ZzZz_iNT';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestKb.TestGetGroupname4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Curb.Your.Enthusiasm.S08E06.INTERNAL.FRENCH.720p.WEB.H264-CiELOS';
  fExpectedResultStr := 'CiELOS';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestKb.TestGetGroupname5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Chelsea.on.the.Rocks.2008.DOCU.BDRip.x264.iNT-WaLMaRT';
  fExpectedResultStr := 'WaLMaRT';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('kb', TTestKb.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestKb);
  {$ENDIF}
end.
