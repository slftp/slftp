# Delphi and FPC

slFtp is built with two compilers, and every change must work with both:

| Compiler     | Platform     | Program      | CI jobs (build / test)                         |
|--------------|--------------|--------------|------------------------------------------------|
| FPC 3.2+     | Linux        | `slftp.lpr`  | `make slftp_64` / `make test`                  |
| Delphi 10.3+ | Windows      | `slftp.dpr`  | `make.bat slftp_64` / `make.bat test_64`       |

FPC on Windows is not supported, even though `slftp.lpr` contains Windows conditionals. Don't add
FPC-on-Windows code paths or build steps.

FPC compiles in Delphi mode (`-MDelphi`), so most code is shared unchanged. The rest is handled with
conditional compilation. For tests that run in both test frameworks, see [writing-tests](writing-tests.md).

## You can usually verify only one side

An agent normally has only one compiler available. On Windows, FPC can be added through WSL, see the
skill [test-both-compilers](../skills/test-both-compilers/SKILL.md). If you cannot build and test with
the other one:

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
| Unit tests | fpcunit | DUnitX | `tests/` |

Never assume `SizeOf(Char) = 1`; use `RawByteString` or bytes for network and file data.

## Keep both project sides in sync

| Change | FPC side | Delphi side |
|--------|----------|-------------|
| New unit in an existing source folder | nothing (found via search path) | nothing (found via search path) |
| New source folder or library | `Makefile` (`CINCLUDES`), `slftp.lpi`, `slftpUnitTests.lpi` | `slftp.dproj`, `slftpUnitTests.dproj` (`DCC_UnitSearchPath`; `make.bat` reads it) |
| Program startup / memory manager | `slftp.lpr` | `slftp.dpr` |
| Compiler define for all FPC builds | `Makefile` (`MORMOTFLAGS`), `CustomOptions` in `slftp.lpi` and `slftpUnitTests.lpi` | — |

CI uses the `Makefile`, not the `.lpi` files, but keep the `.lpi` files current for Lazarus users.

## FPC defines

All FPC builds pass these defines. Defines meant for mORMot2 must be passed on the command line or in
the project files; `slftp.inc` does not work for them, because mORMot2 units only include
`mormot.defines.inc`.

- `NOPATCHRTL`: mORMot2 does not patch the FPC RTL. Its patched `clock_gettime` uses coarse clocks,
  which makes short `TEvent.WaitFor` timeouts return early (`tests/slclockTests.pas`).
- `FPCMM_BOOST` (implies `FPCMM_SERVER`): `mormot.core.fpcx64mm` keeps tiny blocks up to 256 bytes
  apart from medium blocks, which reduces lock contention between the hundreds of slFtp threads. Set the
  mode only via this define, never by editing the vendored unit: without it the unit falls back to its
  mono-threaded GUI defaults. `FPCMM_BOOSTER` (128 arenas) needs more memory and its per-thread arena
  selection is experimental. Medium blocks still share one lock, see [architecture](architecture.md#locking).
