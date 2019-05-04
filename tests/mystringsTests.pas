unit mystringsTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestMyStrings = class(TTestCase)
  published
    procedure TestIsALetter;
    procedure TestIsANumber;
    procedure TestOccurrencesOfNumbers;
  end;

implementation

uses
  SysUtils, mystrings;

{ TTestMyStrings }

procedure TTestMyStrings.TestIsALetter;
begin
  CheckTrue(IsALetter('a'), 'a is letter');
  CheckTrue(IsALetter('g'), 'g is letter');
  CheckTrue(IsALetter('z'), 'z is letter');
  CheckTrue(IsALetter('A'), 'A is letter');
  CheckTrue(IsALetter('K'), 'K is letter');
  CheckTrue(IsALetter('Z'), 'Z is letter');
  CheckFalse(IsALetter(#0), '#0 is not a letter');
  CheckFalse(IsALetter(#50), '#50 is not a letter');
  CheckTrue(IsALetter(#97), 'decimal 97 is a');
  CheckFalse(IsALetter(#255), '#255 is not a letter');
  CheckFalse(IsALetter('0'), '0 is not a letter');
  CheckFalse(IsALetter('3'), '3 is not a letter');
  CheckFalse(IsALetter('9'), '9 is not a letter');
end;

procedure TTestMyStrings.TestIsANumber;
begin
  CheckFalse(IsANumber('a'), 'a is not a number');
  CheckFalse(IsANumber('g'), 'g is not a number');
  CheckFalse(IsANumber('z'), 'z is not a number');
  CheckFalse(IsANumber('A'), 'A is not a number');
  CheckFalse(IsANumber('K'), 'K is not a number');
  CheckFalse(IsANumber('Z'), 'Z is not a number');
  CheckFalse(IsANumber(#0), 'decimal 0 is not a number');
  CheckTrue(IsANumber(#50), 'decimal 50 is a number');
  CheckFalse(IsANumber(#255), 'decimal 255 is not a number');
  CheckTrue(IsANumber('0'), '0 is a number');
  CheckTrue(IsANumber('3'), '3 is a number');
  CheckTrue(IsANumber('9'), '9 is a number');
end;

procedure TTestMyStrings.TestOccurrencesOfNumbers;
begin
  CheckEquals(11, OccurrencesOfNumbers('Greys.Anatomy.S15E15.1080p.HDTV.x264-CRAVERS'), 'Should have 11 numbers');
  CheckEquals(7, OccurrencesOfNumbers('Take.It.or.Leave.It.2018.DVDRip.x264-EMX'), 'Should have 7 numbers');
  CheckEquals(0, OccurrencesOfNumbers('WavSupply.Nick.Mira.Forge.WAV-MASCHiNE'), 'Should have 0 numbers');
  CheckEquals(1, OccurrencesOfNumbers('This is a sentence with 2 questionmarks??'), 'Should have 2 numbers');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings', TTestMyStrings.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestMyStrings);
  {$ENDIF}
end.