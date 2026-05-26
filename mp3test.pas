program mp3test;

{$MODE Delphi}

uses
  Classes, SysUtils, kb.releaseinfo;

var
  r: TMP3Release;
  failed: Integer = 0;

procedure Test(name, rls: string; expectedSource: string; expectedDisks: Integer; section: string = 'MP3');
var
  actualSource: string;
  actualDisks: Integer;
begin
  r := TMP3Release.Create(rls, section, False);
  try
    actualSource := r.mp3source;
    actualDisks := r.mp3numdisks;
    if (actualSource <> expectedSource) or (actualDisks <> expectedDisks) then
    begin
      WriteLn('FAIL: ', name);
      WriteLn('  rls: ', rls);
      WriteLn('  expected: source=', expectedSource, ' disks=', expectedDisks);
      WriteLn('  actual:   source=', actualSource, ' disks=', actualDisks);
      Inc(failed);
    end
    else
    begin
      WriteLn('PASS: ', name);
    end;
  finally
    r.Free;
  end;
end;

begin
  // Test 1: VA-Serious_Beats_92-(541833CD)-4CD-FLAC-2019-WRE
  Test('TestTMP3Release1', 'VA-Serious_Beats_92-(541833CD)-4CD-FLAC-2019-WRE', 'CD', 4, 'FLAC');

  // Test 2: The_Black_Mandala_-_Paradox-(CS132)-WEB-2020-ZzZz
  Test('TestTMP3Release2', 'The_Black_Mandala_-_Paradox-(CS132)-WEB-2020-ZzZz', 'WEB', 1);

  // Test from f603fa19
  Test('TestTMP3ReleaseGetNumberOfDiscs1', 'This_Is_The_Remix_Again.._(Remixes)-(5054197560477)-WEB-2023-GRP', 'WEB', 1);

  if failed > 0 then
  begin
    WriteLn('Total failed: ', failed);
    Halt(1);
  end
  else
  begin
    WriteLn('All tests passed!');
  end;
end.
