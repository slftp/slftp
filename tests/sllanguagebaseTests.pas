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
    procedure TestFindLanguageOnDirectory11;
    procedure TestFindLanguageOnDirectory12;
    procedure TestFindLanguageOnDirectory13;
    procedure TestFindLanguageOnDirectory14;
    procedure TestFindLanguageOnDirectory15;
    procedure TestFindLanguageOnDirectory16;
    procedure TestFindLanguageOnDirectory17;
    procedure TestFindLanguageOnDirectory18;
    procedure TestFindLanguageOnDirectory19;
    procedure TestFindLanguageOnDirectory20;
    procedure TestFindLanguageOnDirectory21;
    procedure TestFindLanguageOnDirectory22;
    procedure TestFindLanguageOnDirectory23;
    procedure TestFindLanguageOnDirectory24;
    procedure TestFindLanguageOnDirectory25;
    procedure TestFindLanguageOnDirectory26;
    procedure TestFindLanguageOnDirectory27;
    procedure TestFindLanguageOnDirectory28;
    procedure TestFindLanguageOnDirectory29;
    procedure TestFindLanguageOnDirectory30;
    procedure TestFindLanguageOnDirectory31;
    procedure TestFindLanguageOnDirectory32;
    procedure TestFindLanguageOnDirectory33;
    procedure TestFindLanguageOnDirectory34;
    procedure TestFindLanguageOnDirectory35;
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

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory11;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'OldYoungLesbianLove.19.07.15.Zazie.Skymm.And.Alexa.Si.Caught.Red.Handed.XXX.2160p.MP4-KTR';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory12;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Dirka.Po.Franciji.2019.5.Etapa.SI.720p.HDTV.x264-RADiOACTiVE';
  fExpectedResultStr := 'Slovenian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory13;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'WTA.French.Open.2019.Final.Ashleigh.Barty.vs.Marketa.Vondrousova.HDTV.x264-WiNNiNG';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory14;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Kinderen.Van.De.Collaboratie.S01E02.FLEMISH.WEB.H264-MERCATOR';
  fExpectedResultStr := 'Flemish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory15;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Just.Another.Immigrant.S01E04.NLSUBBED.WEB.H264-MERCATOR';
  fExpectedResultStr := 'Dutch';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory16;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Divine.Land.Heroes.Vol32.FiNAL.ChineseBiG5.Comic.RETAiL.eBook-OurSky';
  fExpectedResultStr := 'Chinese';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory17;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Rarlab.WinRAR.v3.80.Incl.DOSRAR.BELARUSSIAN.Cracked-F4CG';
  fExpectedResultStr := 'Belarusian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory18;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'The.Amazing.World.Of.Gumball.S01E18.The.Refund.BULGARiAN.WEBRiP.x264-CNBG';
  fExpectedResultStr := 'Bulgarian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory19;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Shrek.2.2004.ESTONiAN.1080p.HDTV.h264-EMX';
  fExpectedResultStr := 'Estonian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory20;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Tinker.Bell.And.The.Secret.Of.The.Wings.2012.iNTERNAL.FiNNiSH.BDRip.x264-PiER';
  fExpectedResultStr := 'Finnish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory21;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Black.Lightning.S02E04.ITALIAN.1080p.WEB.x264-CPY';
  fExpectedResultStr := 'Italian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory22;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Rajd.Polski.Update.1.1.1.PL-PROPHET';
  fExpectedResultStr := 'Polish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory23;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Deep.State.S02E06.POLISH.720p.HDTV.x264-A4O';
  fExpectedResultStr := 'Polish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory24;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Bricsys.BricsCAD.Platinum.v19.2.10.1.PORTUGUESE.LINUX.UBUNTU.X64-AMPED';
  fExpectedResultStr := 'Portuguese';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory25;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'PornMegaLoad.19.05.21.Joana.Bliss.Romanian.Idol.XXX.1080p.MP4-KTR';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory26;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Orizont.2015.ROMANiAN.720p.HDTV.x264-RADiOACTiVE';
  fExpectedResultStr := 'Romanian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory27;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Nubiles.net_19.07.15.Linda.Maers.Russian.Cutie.XXX.IMAGESET-FuGLi';
  fExpectedResultStr := 'Russian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory28;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Zhivaya.priroda.Rossiya.Bashkortostan.Khrebet.Eraktash.15.09.15.2015.RUSSiAN.1080p.HDTV.H264-I_KnoW';
  fExpectedResultStr := 'Russian';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory29;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'FAKings.19.07.12.Paola.Hard.Bae.SPANISH.XXX.720p.MP4-KTR';
  fExpectedResultStr := 'Spanish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory30;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Die.goettliche.Ordnung.2017.Swissgerman.1080p.BluRay.x264-ETM';
  fExpectedResultStr := 'Swissgerman';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory31;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Mit.dem.Bauch.durch.die.Wand.SWiSSGERMAN.DOKU.DVDRiP.x264-DEFLOW';
  fExpectedResultStr := 'Swissgerman';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory32;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Daniel.Spicer.The.Turkish.Psychedelic.Music.Explosion.Anadolu.Psych.1965-1980.2018.RETAiL.ePub.eBook-VENTOLiN';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory33;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Dr.Dolittle.3.2006.iNTERNAL.TURKiSH.DVDRip.X264-MULTiPLY';
  fExpectedResultStr := 'Turkish';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory34;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Marina.Lewycka.-.A.Short.History.Of.Tractors.In.Ukrainian.2006.Retail.EPUB.eBook-BitBook';
  fExpectedResultStr := 'English';

  fOutputStr := FindLanguageOnDirectory(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Wrong language detected!');
end;

procedure TTestSLLanguageBase.TestFindLanguageOnDirectory35;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Boxing.2018.12.02.Deontay.Wilder.Vs.Tyson.Fury.UKRAINIAN.PDTV.x264-PLUTONiUM';
  fExpectedResultStr := 'Ukrainian';

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
