# Writing unit tests

Tests live in `tests/` and run with fpcunit on FPC and DUnitX on Delphi. How to build and run them:
[build-and-test](build-and-test.md).

Tests are written once as DUnit-style `TTestCase` classes with `published` test methods and `Check*`
asserts (`CheckEquals`, `CheckTrue`, …). fpcunit provides them natively; on Delphi,
`DUnitX.DUnitCompatibility` maps them to DUnitX. Only the `uses` and the registration differ:

```pascal
uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

...

initialization
  {$IFDEF FPC}
    RegisterTest('tags', TTestTags.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTags);
  {$ENDIF}
```

- Add a new test unit to the `uses` of both `tests/slftpUnitTests.lpr` and `tests/slftpUnitTests.dpr`.
- Use `CheckEquals` for strings too; fpcunit has no `CheckEqualsString`.
- fpcunit has no `SetUpOnce`/`TeardownOnce`. For setup that must run only once per process, guard
  `SetUp` with a unit-global flag on FPC (see `slftpUnitTestsSetupIndyOpenSSL.pas`,
  `ircchansettingsTests.pas`). There is no matching teardown on that side, because the FPC runner skips
  unit finalization.
