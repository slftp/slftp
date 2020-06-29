unit kb.releaseinfo.MP3Tests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTMP3Release = class(TTestCase)
  published
    procedure TestTMP3Release1;
    procedure TestTMP3Release2;
    procedure TestTMP3Release3;
    procedure TestTMP3Release4;
    procedure TestTMP3Release5;
    procedure TestTMP3Release6;
    procedure TestTMP3Release7;
    procedure TestTMP3Release8;
    procedure TestTMP3Release9;
    procedure TestTMP3Release10;
    procedure TestTMP3Release11;
    procedure TestTMP3Release12;
  end;

implementation

uses
  SysUtils, kb.releaseinfo;

{ TTestTMP3Release }

procedure TTestTMP3Release.TestTMP3Release1;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('VA-Serious_Beats_92-(541833CD)-4CD-FLAC-2019-WRE', 'FLAC');
  try
    CheckEquals(2019, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(4, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('4CD', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckTrue(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release2;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('The_Black_Mandala_-_Paradox-(CS132)-WEB-2020-ZzZz', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('WEB', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release3;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('VA-Ox_Compilation_148-MAG-2020-SDR', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('MAG', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckTrue(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release4;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Humble_Pie-Tourin_The_Official_Bootleg_Box_Set_Volume_4-Boxset-4CD-2019-D2H', 'MP3');
  try
    CheckEquals(2019, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('Bootleg', fTypes, 'types mismatch');
    CheckEquals(4, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('4CD', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckTrue(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release5;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('We_A_Re-Rock_It-SINGLE-WEB-2020-wAx', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('WEB', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release6;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Embodiment_of_Suffering-Revoking_Salvation-(UNG033)-CDEP-FLAC-2019-86D', 'FLAC');
  try
    CheckEquals(2019, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release7;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Moon_Byul-Weird_Day-SINGLE-WEB-KR-2020-TosK', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('KR', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('WEB', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release8;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Toots_And_The_Maytals-Pass_The_Pipe-REISSUE-LP-2019-YARD', 'MP3');
  try
    CheckEquals(2019, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('VINYL', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('LP, REISSUE', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release9;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('VA-Anjunadeep_11-READNFO-WEB-2020-AFO_INT', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('WEB', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckTrue(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release10;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('M.C._Chriscore-Straight_Rap_Demo_EP-DE-PROMO-CD-FLAC-2011-FiXIE', 'FLAC');
  try
    CheckEquals(2011, fClass.mp3year, 'year mismatch');
    CheckEqualsString('DE', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('PROMO, EP, Demo', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release11;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Bryan_Ferry-Live_at_the_Royal_Albert_Hall_1974-WEB-2020-ENTiTLED', 'MP3');
  try
    CheckEquals(2020, fClass.mp3year, 'year mismatch');
    CheckEqualsString('EN', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('WEB', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('Live', fTypes, 'types mismatch');
    CheckEquals(1, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckTrue(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

procedure TTestTMP3Release.TestTMP3Release12;
var
  fClass: TMP3Release;
  fTypes: String;
begin
  fClass := TMP3Release.Create('Julio_Iglesias-The_Greatest_Songs-ES-2CD-FLAC-1999-6DM', 'FLAC');
  try
    CheckEquals(1999, fClass.mp3year, 'year mismatch');
    CheckEqualsString('ES', fClass.mp3lng, 'language mismatch');
    CheckEqualsString('', fClass.mp3genre, 'genre mismatch');
    CheckEqualsString('CD', fClass.mp3source, 'source mismatch');
    fTypes := String.Join(', ', fClass.mp3types.ToArray);
    CheckEqualsString('', fTypes, 'types mismatch');
    CheckEquals(2, fClass.mp3numdisks, 'numdisks mismatch');
    CheckEqualsString('2CD', fClass.mp3numdisksword, 'numdisks_word mismatch');
    CheckFalse(fClass.mp3va, 'va mismatch');
    CheckFalse(fClass.mp3bootleg, 'Bootleg mismatch');
    CheckFalse(fClass.mp3live, 'mp3live mismatch');
  finally
    fClass.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TMP3Release tests', TTestTMP3Release.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTMP3Release);
  {$ENDIF}
end.

