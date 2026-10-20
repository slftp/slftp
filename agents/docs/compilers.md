# Delphi and FPC

slFtp is built with two compilers, and every change must work with both:

| Compiler     | Platform     | Program      | CI jobs (build / test)                         |
|--------------|--------------|--------------|------------------------------------------------|
| FPC 3.2+     | Linux        | `slftp.lpr`  | `make slftp_64` / `make test`                  |
| Delphi 10.3+ | Windows      | `slftp.dpr`  | `make.bat slftp_64` / `make.bat test_64`       |

FPC on Windows is not supported, even though `slftp.lpr` contains Windows conditionals. Don't add
FPC-on-Windows code paths or build steps.

FPC compiles in Delphi mode (`-MDelphi`), so most code is shared unchanged. The rest is handled with
conditional compilation.

## You can usually verify only one side

An agent normally has only one compiler available. If you cannot build and test with the other one:

- Check your change against the differences below.
- Say clearly in your summary which compiler you built and tested with, and that the other side is
  unverified until CI has run. Never claim a change compiles with a compiler you did not run.

## Conditional compilation

- `{$IFDEF FPC}` … `{$ELSE}` … `{$ENDIF}` for compiler differences
- `{$IFDEF MSWINDOWS}` / `{$IFDEF UNIX}` for operating-system differences
- `{$IFDEF UNICODE}` for the string type difference (see below)

Keep conditionals small and local, or put them in one helper that both sides call. Prefer mORMot2 or
existing slFtp helpers, which already behave the same on both compilers, over RTL units that differ.

## Known differences

| Topic | FPC | Delphi | Example |
|-------|-----|--------|---------|
| `String` | `AnsiString` (1 byte per char) | `UnicodeString` (UTF-16) | `debugunit.pas`, `sltcp.pas` |
| Comparer callback parameters | `constref` | `const` | `dirlist.pas`, `pazo.pas` |
| Atomic increment/decrement | `InterlockedIncrement` | `AtomicIncrement` | `sitesunit.pas`, `notify.pas` |
| Enum ↔ string via RTTI | `GetEnumName(TypeInfo(T), Integer(x))` | via `TValue` | `mystrings.pas` |
| Base64 | `base64` unit | `NetEncoding` unit | `mystrings.pas` |
| Socket helpers | `sockets` unit | `IdWinsock2` | `slhelper.pas` |
| Memory manager | `cthreads` + `mormot.core.fpcx64mm` (or `cmem`) | `FastMM5` | `slftp.lpr` / `slftp.dpr` |
| Unit tests | fptest | DUnitX | `tests/` |

Patterns from the code:

```pascal
// String type: never assume SizeOf(Char) = 1. Use RawByteString/bytes for network and file data.
{$IFDEF UNICODE}
  ExtractStrings([' ', ',', '|'], [], PChar(affils), List);
{$ELSE}
  ExtractStrings([' ', ',', '|'], [], PAnsiChar(affils), List);
{$ENDIF}

// Comparer for Generics.Collections sorting
function _DirListSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} i1, i2: TDirListEntry): Integer;

// Atomic counter
{$IFDEF FPC}InterlockedIncrement{$ELSE}AtomicIncrement{$ENDIF}(site.fNumDn);
```

## Unit tests for both frameworks

Tests are written once as DUnit-style `TTestCase` classes with `published` test methods and `Check*`
asserts (`CheckEquals`, `CheckEqualsString`, `CheckTrue`, …). On Delphi, `DUnitX.DUnitCompatibility` maps
them to DUnitX. Only the `uses` and the registration differ:

```pascal
uses
  {$IFDEF FPC}
    TestFramework;
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

## Keep both project sides in sync

| Change | FPC side | Delphi side |
|--------|----------|-------------|
| New unit in an existing source folder | nothing (found via search path) | nothing (found via search path) |
| New source folder or library | `Makefile` (`CINCLUDES`), `slftp.lpi`, `slftpUnitTests.lpi` | `slftp.dproj`, `slftpUnitTests.dproj` (`DCC_UnitSearchPath`; `make.bat` reads it) |
| New test unit | `uses` of `tests/slftpUnitTests.lpr` | `uses` of `tests/slftpUnitTests.dpr` |
| Program startup / memory manager | `slftp.lpr` | `slftp.dpr` |

CI uses the `Makefile`, not the `.lpi` files, but keep the `.lpi` files current for Lazarus users.
