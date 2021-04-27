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

      CheckEquals('00-asdf.jpg', TDirListEntry(fDirlist.entries[0]).filename);
      CheckEquals('02-asdf.mp3', TDirListEntry(fDirlist.entries[1]).filename);
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

      CheckEquals('00-asdf.jpg', TDirListEntry(fDirlist.entries[0]).filename);
      CheckEquals('02-asdf.mp3', TDirListEntry(fDirlist.entries[1]).filename);
      CheckEquals(2, fDirlist.entries.Count);
    finally
      if fDirlist <> nil then
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
