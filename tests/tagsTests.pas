unit tagsTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTags = class(TTestCase)
  published
    procedure TestTagComplete1;
    procedure TestTagComplete2;
    procedure TestTagComplete3;
    procedure TestTagComplete4;
    procedure TestTagComplete5;
    procedure TestTagComplete6;
    procedure TestTagComplete7;
    procedure TestTagComplete8;
    procedure TestTagComplete9;
    procedure TestTagComplete10;
    procedure TestTagComplete11;
    procedure TestTagComplete12;
    procedure TestTagComplete13;
    procedure TestTagComplete14;
    procedure TestTagComplete15;
    procedure TestTagComplete16;
  end;

implementation

uses
  SysUtils, tags;

{ TTestTags }

procedure TTestTags.TestTagComplete1;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[##############] -  100% Complete - [xxx]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete2;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[ 23 of 23 files = 100% complete of 335.1MB]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete3;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctINCOMPLETE);
  fResult := Ord(TagComplete('[##::::::::::::] -  18% Complete - [xxx]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete4;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctINCOMPLETE);
  fResult := Ord(TagComplete('[8 of 10 files = 80% complete at 366.8MB]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete5;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctINCOMPLETE);
  fResult := Ord(TagComplete('[::::::::::::::] -   0% Complete - [x]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete6;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctINCOMPLETE);
  fResult := Ord(TagComplete('[ 73%]-[##########::::]-[729mb]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete7;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('<<SL>> 11M 1F of House from 2017 COMPLETE'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete8;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('-> 8128M 86F - DONE <-'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete9;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[>>>>>>>>>>>   ] -  83DONE'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete10;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[Completed! - 1252M 27F]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete11;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('( 122M 13F - COMPLETE )'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete12;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[x] - ( 135M 2F - COMPLETE - Dance Hall 1990 ) - [x]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete13;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[x] - ( 145M 31F - COMPLETE ) - [x]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete14;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[SITE] - ( 2F 12MB 100% COMPLETE ) - ( Reggae from 1970 @ 251kbits VBR-NEW Joint Stereo ) - [SITE]'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete15;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('100% complete'));
  CheckEquals(fExpected, fResult);
end;

procedure TTestTags.TestTagComplete16;
var
  fExpected, fResult: Integer;
begin
  fExpected := Ord(tctCOMPLETE);
  fResult := Ord(TagComplete('[ 132M 8F - COMPLETE ]'));
  CheckEquals(fExpected, fResult);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('tags', TTestTags.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTags);
  {$ENDIF}
end.
