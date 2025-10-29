unit slftpUnitTestsSetup;

interface

{* calls might depend on each other, so order might be important *}
procedure InitialConfigSetup;
procedure InitialDebugSetup;
procedure InitialKbSetup;
procedure InitialSLLanguagesSetup;
procedure InitialGlobalskiplistSetup;
procedure InitialTagsSetup;
procedure InitialDirlistSetup;
procedure InitialDbAddImdbSetup;
procedure InitialPrecatcherSetup;
procedure InitialKnownGroupsSetup;
procedure InitialSkiplistSetup;
procedure InitialFakeSetup;
procedure InitialDbAddpreSetup;

implementation

uses
  configunit, debugunit, encinifile, kb, sllanguagebase, globalskipunit, tags, dirlist, dbaddimdb, precatcher, knowngroups, skiplists, fake,
  dbaddpre, dbhandler, ZPlainMySqlDriver, mormot.db.sql.zeos, mormot.db.core;

procedure InitialConfigSetup;
var
  fDefaultPassword: String;
begin
  fDefaultPassword := 'nopw';
  ConfigInit(fDefaultPassword);
end;

procedure InitialDebugSetup;
begin
  DebugInit;
end;

procedure InitialKbSetup;
begin
  kb_Init;
end;

procedure InitialSLLanguagesSetup;
begin
  SLLanguagesInit;
end;

procedure InitialGlobalskiplistSetup;
begin
  Initglobalskiplist;
end;

procedure InitialTagsSetup;
begin
  TagsInit;
end;

procedure InitialDirlistSetup;
begin
  DirlistInit;
end;

procedure InitialDbAddImdbSetup;
begin
  dbaddimdbInit;
end;

procedure InitialPrecatcherSetup;
begin
  Precatcher_Init;
  PrecatcherStart;
end;

procedure InitialKnownGroupsSetup;
begin
  KnowngroupsInit;
  KnowngroupsStart;
end;

procedure InitialSkiplistSetup;
begin
  SkiplistsInit;
  SkiplistStart;
end;

procedure InitialFakeSetup;
begin
  FakesInit;
  FakeStart;
end;

procedure InitialDbAddpreSetup;
begin
  MySQLCon := TSQLDBZEOSConnectionProperties.Create(TSQLDBZEOSConnectionProperties.URI(dMariaDB, 'localhost:3306', MARIADB_LOCATION), 'predb', 'root', 'asdf');
  dbaddpreInit;
  dbaddpreStart;
end;

end.
