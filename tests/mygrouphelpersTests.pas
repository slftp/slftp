unit mygrouphelpersTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestMyGroupHelpers = class(TTestCase)
  published
    procedure TestGetGroupname1;
    procedure TestGetGroupname2;
    procedure TestGetGroupname3;
    procedure TestGetGroupname4;
    procedure TestGetGroupname5;
    procedure TestRemoveGroupname1;
    procedure TestRemoveGroupname2;
    procedure TestRemoveGroupname3;
    procedure TestRemoveGroupname4;
    procedure TestRemoveGroupname5;
    procedure TestRemoveINT1;
    procedure TestRemoveINT2;
    procedure TestRemoveINT3;
    procedure TestRemoveINT4;
    procedure TestRemoveINT5;
    procedure TestRemoveWEB1;
    procedure TestRemoveWEB2;
    procedure TestRemoveWEB3;
    procedure TestRemoveWEB4;
    procedure TestRemoveWEB5;
  end;

implementation

uses
  SysUtils, mygrouphelpers;

{ TTestMyGroupHelpers }

procedure TTestMyGroupHelpers.TestGetGroupname1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'American.Ninja.Warrior.S11E01.1080p.WEB.h264-TBS';
  fExpectedResultStr := 'TBS';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestMyGroupHelpers.TestGetGroupname2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Altstadt_Echo-This_Work_Contains_Lead-(SEMANTICA109)-WEB-2019-AGITB';
  fExpectedResultStr := 'AGITB';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestMyGroupHelpers.TestGetGroupname3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz_iNT';
  fExpectedResultStr := 'ZzZz_iNT';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestMyGroupHelpers.TestGetGroupname4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Curb.Your.Enthusiasm.S08E06.INTERNAL.FRENCH.720p.WEB.H264-CiELOS';
  fExpectedResultStr := 'CiELOS';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestMyGroupHelpers.TestGetGroupname5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Chelsea.on.the.Rocks.2008.DOCU.BDRip.x264.iNT-WaLMaRT';
  fExpectedResultStr := 'WaLMaRT';

  fOutputStr := GetGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Groupname extracted!');
end;

procedure TTestMyGroupHelpers.TestRemoveGroupname1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'American.Ninja.Warrior.S11E01.1080p.WEB.h264-TBS';
  fExpectedResultStr := 'American.Ninja.Warrior.S11E01.1080p.WEB.h264';

  fOutputStr := RemoveGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Remove of Groupname failed!');
end;

procedure TTestMyGroupHelpers.TestRemoveGroupname2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Borgen.S03E03.iNTERNAL.SUBFRENCH.720p.WEB.H264-CiELOS';
  fExpectedResultStr := 'Borgen.S03E03.iNTERNAL.SUBFRENCH.720p.WEB.H264';

  fOutputStr := RemoveGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Remove of Groupname failed!');
end;

procedure TTestMyGroupHelpers.TestRemoveGroupname3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz_iNT';
  fExpectedResultStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019';

  fOutputStr := RemoveGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Remove of Groupname failed!');
end;

procedure TTestMyGroupHelpers.TestRemoveGroupname4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Maher_Daniel-A_Heart_That_Beats_You_EP-(NO19092)-WEB-2019-ENTANGLE';
  fExpectedResultStr := 'Maher_Daniel-A_Heart_That_Beats_You_EP-(NO19092)-WEB-2019';

  fOutputStr := RemoveGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Remove of Groupname failed!');
end;

procedure TTestMyGroupHelpers.TestRemoveGroupname5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Putney.Swope.1969.720p.BluRay.x264-PSYCHD';
  fExpectedResultStr := 'Putney.Swope.1969.720p.BluRay.x264';

  fOutputStr := RemoveGroupname(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Remove of Groupname failed!');
end;

procedure TTestMyGroupHelpers.TestRemoveINT1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Melanie_Flash_-_Halfway_To_Heaven-WEB-2007-MARiBOR_INT';
  fExpectedResultStr := 'Melanie_Flash_-_Halfway_To_Heaven-WEB-2007-MARiBOR';

  fOutputStr := RemoveINT(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove INT from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveINT2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Jar-Zywo-WEB-PL-2016-I_KnoW_INT';
  fExpectedResultStr := 'Jar-Zywo-WEB-PL-2016-I_KnoW';

  fOutputStr := RemoveINT(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove INT from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveINT3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz_iNT';
  fExpectedResultStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz';

  fOutputStr := RemoveINT(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove INT from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveINT4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'The_Weeknd-Secrets-CONVERT-DVDRip-x264-2017-SRPx_iNT';
  fExpectedResultStr := 'The_Weeknd-Secrets-CONVERT-DVDRip-x264-2017-SRPx';

  fOutputStr := RemoveINT(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove INT from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveINT5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Young.Sheldon.S01E08.PL.iNT.DVDRiP.x264-N0L';
  fExpectedResultStr := 'Young.Sheldon.S01E08.PL.iNT.DVDRiP.x264-N0L';

  fOutputStr := RemoveINT(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove INT from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveWEB1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Los_Cachorros_De_Juan_Villarreal-Lo_Mas_Escuchado_De-ES-WEB-2019-FREGON';
  fExpectedResultStr := 'Los_Cachorros_De_Juan_Villarreal-Lo_Mas_Escuchado_De-ES-WEB-2019-FREGON';

  fOutputStr := RemoveWEB(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove WEB from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveWEB2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Super.Wings.S03E17.720p.WEB.h264-WEBTUBE';
  fExpectedResultStr := 'Super.Wings.S03E17.720p.WEB.h264-WEBTUBE';

  fOutputStr := RemoveWEB(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove WEB from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveWEB3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Tommy_Trash_-_Ripper-(WFH019)-SINGLE-WEB-2019-FMC';
  fExpectedResultStr := 'Tommy_Trash_-_Ripper-(WFH019)-SINGLE-WEB-2019-FMC';

  fOutputStr := RemoveWEB(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove WEB from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveWEB4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Zervos_P.-Call_Out_To_Me-WEB-2012-CRN_WEB';
  fExpectedResultStr := 'Zervos_P.-Call_Out_To_Me-WEB-2012-CRN';

  fOutputStr := RemoveWEB(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove WEB from Groupname!');
end;

procedure TTestMyGroupHelpers.TestRemoveWEB5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Elite.S01E03.1080p.WEB.x264-DeathToWebCrop';
  fExpectedResultStr := 'Elite.S01E03.1080p.WEB.x264-DeathToWebCrop';

  fOutputStr := RemoveWEB(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Failed to remove WEB from Groupname!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mygrouphelpers', TTestMyGroupHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestMyGroupHelpers);
  {$ENDIF}
end.
