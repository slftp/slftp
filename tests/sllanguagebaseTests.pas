unit sllanguagebaseTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLLanguageBase = class(TTestCase)
  published
    procedure TestFindLanguageOnDirectory1;
    procedure TestFindLanguageOnDirectory2;
    procedure TestFindLanguageOnDirectory3;
    procedure TestFindLanguageOnDirectory4;
    procedure TestFindLanguageOnDirectory5;
    procedure TestFindLanguageOnDirectory6;
    procedure TestFindLanguageOnDirectory7;
    procedure TestFindLanguageOnDirectory8;
    procedure TestFindLanguageOnDirectory9;
    procedure TestFindLanguageOnDirectory10;
    procedure TestFindMusicLanguageOnDirectory1;
    procedure TestFindMusicLanguageOnDirectory2;
    procedure TestFindMusicLanguageOnDirectory3;
    procedure TestFindMusicLanguageOnDirectory4;
    procedure TestFindMusicLanguageOnDirectory5;
    procedure TestFindMusicLanguageOnDirectory6;
    procedure TestFindMusicLanguageOnDirectory7;
    procedure TestFindMusicLanguageOnDirectory8;
    procedure TestFindMusicLanguageOnDirectory9;
    procedure TestFindMusicLanguageOnDirectory10;
    procedure TestFindMusicLanguageOnDirectory11;
    procedure TestFindMusicLanguageOnDirectory12;
    procedure TestFindMusicLanguageOnDirectory13;
    procedure TestFindMusicLanguageOnDirectory14;
    procedure TestFindMusicLanguageOnDirectory15;
    procedure TestFindMusicLanguageOnDirectory16;
    procedure TestFindMusicLanguageOnDirectory17;
    procedure TestFindMusicLanguageOnDirectory18;
  end;

implementation

uses
  SysUtils, sllanguagebase;

{ TTestSLLanguageBase }

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'iMPACT.Wrestling.2019.05.24.PDTV.x264-PLUTONiUM';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'IMSI.AnimationLab.5.Plug-In.to.TurboCAD.v1.0.x64.Incl.Keygen-NGEN';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Trust.S01E02.SPANiSH.WEBRip.x264-4FiRE';
  fExpectedResultStr := 'Spanish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Vita.Vidder.S03E04.SWESUB.720p.WEB.h264-45RPM';
  fExpectedResultStr := 'Swedish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Microsoft.Office.Professional.Plus.2016.Arabic.VL.x86.ISO-SCC';
  fExpectedResultStr := 'Arabic';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Frakk.a.macskak.reme.S03E12.Cinkemama.kuldi.HUNGARiAN.DVDRiP.x264-NMHH';
  fExpectedResultStr := 'Hungarian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'The.Mule.2018.MULTi.TRUEFRENCH.1080p.BluRay.x264-THREESOME';
  fExpectedResultStr := 'French';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Formule1.2019.GP06.Monaco.DUTCH.1080p.HDTV.x264-DTOD';
  fExpectedResultStr := 'Dutch';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory9;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Formula.2.2019.Monaco.Grand.Prix.Race.2.DANiSH.720p.HDTV.x264-SKANK';
  fExpectedResultStr := 'Danish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory10;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Spider-Man.Into.The.Spider-Verse.2018.PLDUB.DUAL.2160p.UHD.BluRay.x265-FLAME';
  fExpectedResultStr := 'Polish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA-Sing_Meinen_Song_Das_Tauschkonzert_Volume_6-DE-DELUXE_EDITION-2CD-FLAC-2019-VOLDiES';
  fExpectedResultStr := 'DE';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Stones_Taro-Insane-WEB-2019-KLIN';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Saina_Manotte-Poupee_Kreyol_II-WEB-FR-2019-OND';
  fExpectedResultStr := 'FR';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VA_-_1_Club_Hits_(2008_Best_Of_Dance_House_Electro_Trance_and_Techno_(New_Edition)-WEB-2008-ZzZz';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Ertebrekers-Creme-(0783228)-NL-CD-FLAC-2019-WRE';
  fExpectedResultStr := 'NL';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Lil_Wayne_ft_XXXTentacion-Dont_Cry-DDC-1080p-x264-2019-SRPx';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Momoka_Ariyasu-Catch_Up-JA-BLURAY-RETAIL-x264-2017-DARKFLiX';
  fExpectedResultStr := 'JA';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Alonzo-Suis-Moi-FR-1080p-x264-2017-iUF';
  fExpectedResultStr := 'FR';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory9;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Noodle-No-WEB-2019-KLIN';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory10;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Noodle-NO-WEB-2019-KLIN';
  fExpectedResultStr := 'NO';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory11;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NO-odle-No-WEB-2019-KLIN';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory12;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Noodle-No-WEB-2019-KLIN-NO';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory13;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Noodle-(NO)-WEB-2019-KLIN';
  fExpectedResultStr := 'NO';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory14;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Noodle-NO_Way-WEB-2019-KLIN';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory15;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Josipa_Lisac-Posve_Slobodna_(Suradnje_And_Etno)-2CD-HR-2018-IMT';
  fExpectedResultStr := 'HR';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory16;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Momoiro_Clover_Z-BLAST-JA-BLURAY-RETAIL-x264-2017-DARKFLiX';
  fExpectedResultStr := 'JA';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory17;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Amir-Etats_DAmour_(Paris_A_Nous_Les_Jeux)-FR-720p-x264-2017-PmV';
  fExpectedResultStr := 'FR';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

procedure TTestSLLanguageBase.TestFindMusicLanguageOnDirectory18;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Dej_Loaf-No_Fear-DVDRip-x264-2017-SRPx';
  fExpectedResultStr := 'EN';

  fOutputStr := FindMusicLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong Music language detected!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('SLLanguageBase', TTestSLLanguageBase.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLLanguageBase);
  {$ENDIF}
end.
