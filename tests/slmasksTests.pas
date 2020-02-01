unit slmasksTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTslMask = class(TTestCase)
  published
    procedure TestMatches1;
    procedure TestMatches2;
    procedure TestMatches3;
    procedure TestMatches4;
    procedure TestMatches5;
  end;

implementation

uses
  SysUtils, slmasks;

{ TTestTslMask }

procedure TTestTslMask.TestMatches1;
var
  fMask: TslMask;
begin
  fMask := TslMask.Create('/(\w+)/');
  try
    CheckTrue(fMask.Matches('MyPRE-GRP1'), 'Should match!');
    CheckTrue(fMask.Matches('AnotherOne'), 'Should match!');
    CheckTrue(fMask.Matches('12345-6789'), 'Should match!');
    CheckFalse(fMask.Matches('$%/(=&]'), 'Should not match!');
    CheckTrue(fMask.Matches('123-oops-123'), 'Should match!');
  finally
    fMask.Free;
  end;
end;

procedure TTestTslMask.TestMatches2;
var
  fMask: TslMask;
begin
  fMask := TslMask.Create('*proof*.rar');
  try
    CheckFalse(fMask.Matches('fullsize-my.generation-proof.jpg'), 'Should not match!');
    CheckTrue(fMask.Matches('fullsize-my.generation-proof.rar'), 'Should match!');
    CheckTrue(fMask.Matches('some-proof-text.rar'), 'Should match!');
    CheckTrue(fMask.Matches('some-PrOOf-text.rar'), 'Should match!');
    CheckFalse(fMask.Matches('some-wrong-pr00f-text.rar'), 'Should not match!');
  finally
    fMask.Free;
  end;
end;

procedure TTestTslMask.TestMatches3;
var
  fMask: TslMask;
begin
  fMask := TslMask.Create('/^My/i');
  try
    CheckTrue(fMask.Matches('MY-TEST'), 'Should match!');
    CheckFalse(fMask.Matches('NotMyTest'), 'Should not match!');
    CheckTrue(fMask.Matches('Myth'), 'Should match!');
  finally
    fMask.Free;
  end;
end;

procedure TTestTslMask.TestMatches4;
var
  fMask: TslMask;
begin
  fMask := TslMask.Create('*.mkv');
  try
    CheckTrue(fMask.Matches('utopia-bgsal-s05e01-dl-1080p-sample.mkv'), 'Should match!');
    CheckFalse(fMask.Matches('abbw.20.01.28.elin.intimate.moments-sample.mp4'), 'Should not match!');
    CheckFalse(fMask.Matches('fullsize-my.generation-sample.m2ts'), 'Should not match!');
    CheckTrue(fMask.Matches('roma.2018.1080p.bluray.x264-aaa-sample.mkv'), 'Should match!');
    CheckFalse(fMask.Matches('roma.2018.1080p.bluray.x264-aaa-sample.mkvfile'), 'Should not match!');
  finally
    fMask.Free;
  end;
end;

procedure TTestTslMask.TestMatches5;
var
  fMask: TslMask;
begin
  fMask := TslMask.Create('Sample');
  try
    CheckFalse(fMask.Matches('abbw.20.01.28.elin.intimate.moments-sample.mp4'), 'Should not match!');
    CheckFalse(fMask.Matches('fullsize-my.generation-sample.m2ts'), 'Should not match!');
    CheckTrue(fMask.Matches('Sample'), 'Should match!');
    CheckFalse(fMask.Matches('Samplefile'), 'Should not match!');
    CheckFalse(fMask.Matches('DelSample'), 'Should not match!');
  finally
    fMask.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('slmasks', TTestTslMask.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTslMask);
  {$ENDIF}
end.
