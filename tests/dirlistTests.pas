unit dirlistTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestDirlist = class(TTestCase)
  published
    procedure TestCompleteTag1;
    procedure TestCompleteTag2;
    procedure TestBeingUploadedFlag;
  end;

implementation

uses
  dirlist, SysUtils;

{ TTestDirlistHelpers }

procedure TTestDirlist.TestCompleteTag1;
var
  fResp: TArray<String>;
  fDirlist: TDirlist;
begin
  // glftpd dir
  fResp := TArray<String>.Create('total 126947',
    '-rw-r--r--   1 testuser testgrp     82883 Apr 18 23:49 00-asdf.jpg',
    '-rw-r--r--   1 testuser testgrp  15055165 Apr 18 23:49 02-asdf.mp3',
    'drwxrwxrwx   2 testuser testgrp        10 Apr 18 23:49 [xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]',
    '213 End of Status');

    try
      fDirlist := TDirlist.Create('', nil, nil, String.Join(#13, fResp));
      CheckEquals('[xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]', fDirlist.CompleteDirTag);

      //parse again to see if it's still true
      fDirlist.ParseDirlist(String.Join(#13, fResp));
      CheckEquals('[xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]', fDirlist.CompleteDirTag);

      CheckEquals('00-asdf.jpg', TDirListEntry(fDirlist.entries['00-asdf.jpg']).filename);
      CheckEquals('02-asdf.mp3', TDirListEntry(fDirlist.entries['02-asdf.mp3']).filename);
      CheckEquals(2, fDirlist.entries.Count);
    finally
      if fDirlist <> nil then
        fDirlist.Free;
    end;
end;

procedure TTestDirlist.TestCompleteTag2;
var
  fResp: TArray<String>;
  fDirlist: TDirlist;
begin
  // glftpd file
  fResp := TArray<String>.Create('total 126947',
    '-rw-r--r--   1 testuser testgrp     82883 Apr 18 23:49 00-asdf.jpg',
    '-rw-r--r--   1 testuser testgrp  15055165 Apr 18 23:49 02-asdf.mp3',
    '-rw-r--r--   1 glftpd   glftpd          0 Apr 19 19:14 [xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]',
    '213 End of Status');

    try
      fDirlist := TDirlist.Create('', nil, nil, String.Join(#13, fResp));
      CheckEquals('[xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]', fDirlist.CompleteDirTag);

      //parse again to see if it's still true
      fDirlist.ParseDirlist(String.Join(#13, fResp));
      CheckEquals('[xxx] - ( 11M 1F - COMPLETE - ASDF 1337 ) - [xxx]', fDirlist.CompleteDirTag);

      CheckEquals('00-asdf.jpg', TDirListEntry(fDirlist.entries['00-asdf.jpg']).filename);
      CheckEquals('02-asdf.mp3', TDirListEntry(fDirlist.entries['02-asdf.mp3']).filename);
      CheckEquals(2, fDirlist.entries.Count);
    finally
      if fDirlist <> nil then
        fDirlist.Free;
    end;
end;

procedure TTestDirlist.TestBeingUploadedFlag;
var
  fResp: TArray<String>;
  fDirlist: TDirlist;
begin
  fResp := TArray<String>.Create('total 1',
    '-rw-r-xr-x   1 uploader group        0 Oct  4 12:00 active.bin',
    '-rw-r--r--   1 uploader group      100 Oct  4 12:00 complete.bin',
    'drwxrwxrwx   2 uploader group       10 Oct  4 12:00 Sample',
    '213 End of Status');

  fDirlist := TDirlist.Create('', nil, nil, String.Join(#13, fResp));
  try
    CheckTrue(TDirListEntry(fDirlist.entries['active.bin']).IsBeingUploaded,
      'glFTPd upload marker should be preserved on the dirlist entry');
    CheckFalse(TDirListEntry(fDirlist.entries['complete.bin']).IsBeingUploaded,
      'ordinary files should not be marked as uploading');
    CheckFalse(TDirListEntry(fDirlist.entries['Sample']).IsBeingUploaded,
      'directories should not be marked as file uploads');
  finally
    fDirlist.Free;
  end;
end;


initialization
  {$IFDEF FPC}
    RegisterTest('dirlist', TTestDirlist.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDirlist);
  {$ENDIF}
end.
