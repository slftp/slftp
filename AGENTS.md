# slFtp — Agent Guide

This document is a quick-start reference for AI coding agents working on the slFtp codebase. All information below is derived from the actual project files in the current working tree (`dev` branch); avoid assuming generic Pascal project conventions.

---

## 1. Project Overview

**slFtp** is an open-source, multithreaded FTP client aimed at automated "scene" trading. It is written primarily in Object Pascal and runs natively on Linux (Free Pascal Compiler) and Windows (Delphi).

Key capabilities:

* FTP client with per-site slots, SSL/TLS, SSLFXP, SOCKS5, and ident server support.
* Event-driven autotrading based on IRC announces and directory listings.
* Rule engine with 50+ conditions for MP3, TV, IMDB, 0DAY, MVID, etc.
* Full IRC support (multiple networks/channels, SSL, Blowfish ECB/CBC).
* Knowledge base (`kb.pas`) and per-release routing objects (`pazo.pas`) that coordinate races between source and destination sites.
* Text-mode console UI (`slvision`/`slconsole`) plus an IRC admin backend.
* Event-based task coordination (`TWaitTask` in `taskrace.pas`) for paired race operations.

> **Note on the `api/` directory:** The `dev` branch contains a pre-built `api/` directory with compiled object/unit files (`slapi*.o`, `slapi*.ppu`) only. The Pascal source for the REST API is **not** present on `dev`. The React web UI is also **not** present in this checkout.

---

## 2. Technology Stack

| Component | Technology |
|-----------|------------|
| Language | Object Pascal |
| Linux compiler | Free Pascal Compiler ≥ 3.2.0 (`fpc`) |
| Windows compiler | Delphi 10.3 Rio or newer (`dcc32.exe` / `dcc64.exe`) |
| Mode | `{$MODE Delphi}` |
| Build system | `Makefile` (Linux), `make.bat` + `.dproj` (Windows) |
| Networking | Indy10, custom `sltcp.pas`/`slstack.pas`, OpenSSL via mORMot2 bindings |
| Databases | SQLite3 (embedded), optional MySQL/MariaDB |
| Encryption | Blowfish (custom + OpenSSL EVP), MD5 |

### Bundled library versions

Declared in `slftp.inc`:

```pascal
lib_OpenSSL: string = '1.1.1';
lib_Ncurses: string = 'ncurses 5.5.';
lib_SQLite3: string = '3.35';
```

In practice the project has moved on:

* **OpenSSL 3.x** is the current target. `make.bat` downloads `openssl-3.5`, `.gitlab-ci.yml` packages `openssl-3.5_x64.zip`, and `slssl.pas` loads the legacy provider only on OpenSSL 3.x.
* **SQLite 3.45.3** is packaged by CI (`sqlite-dll-win-x64-3450300.zip`). The `3.35` constant is stale.
* The ncurses constant is also stale; runtime historically required ≥ 5.7.

---

## 3. Repository Layout

```
/                      # Root Pascal units and project files
  slftp.lpr            # FPC/Linux program entry point
  slftp.dpr            # Delphi/Windows program entry point
  slftp.lpi/.dproj     # Lazarus / Delphi project files
  slftp.inc            # Version constants + required library versions
  Makefile             # Linux build orchestration
  make.bat             # Windows build orchestration
  common.inc           # Shared string arrays and constants
  replace_git_commit.pl# Patches SL_REV with current git HEAD
irccommands/           # One unit per IRC command group
rules/                 # Rule condition classes + .inc description files
libs/                  # Vendored third-party libraries (Indy10, mORMot2, Zeos, etc.)
config/                # Default/reference runtime configuration files
helpfiles/             # Text snippets shipped as source for the !help system
docs/                  # Human-readable guides
tests/                 # Test runner + test units + vendored fptest framework
api/                   # Pre-compiled REST API object/unit files (no .pas source on dev)
```

### Key units

| Unit | Responsibility |
|------|----------------|
| `console.pas` | Text-mode UI application (`TMySlApp`) |
| `mainthread.pas` | Application lifecycle, startup/shutdown |
| `configunit.pas` | Loads encrypted/plain INI config |
| `encinifile.pas` | Encrypted INI implementation |
| `sitesunit.pas` | `TSite` / `TSiteSlot` models and slot worker threads |
| `queueunit.pas` | Per-site task queue and scoring/assignment |
| `tasksunit.pas` | Base `TTask` class |
| `taskrace.pas` | Racing tasks including event-based `TWaitTask` |
| `kb.pas` / `pazo.pas` | Knowledge base and per-release routing objects |
| `kb.releaseinfo.pas` | Release type metadata (MP3, MVID, 0DAY, TV, IMDB) |
| `dirlist.pas` / `dirlist.helpers.pas` | FTP listing parsing, completeness, skiplist application |
| `rulesunit.pas` | Expression parser/evaluator for site/section rules |
| `irc.pas` / `irccommandsunit.pas` | IRC layer and command dispatcher |
| `sltcp.pas` / `slstack.pas` / `slssl.pas` | Socket abstraction and TLS |
| `statsunit.pas` | ORM-based stats DB and JSON exporters |
| `dbaddpre.pas` / `dbaddimdb.pas` / `dbtvinfo.pas` | Pre/IMDB/TV info lookup and caching |
| `precatcher.pas` | IRC announce parsing and routing |

### Subdirectory details

* `irccommands/` — split into `general`, `imdb`, `indexer`, `info`, `irc`, `kb`, `misc`, `news`, `prebot`, `precatcher`, `pretime`, `preurl`, `rank`, `reload`, `route`, `rules`, `section`, `site`, `slots`, `socks`, `speed`, `stats`, `test`, `tv`, `windows`, `work`.
* `rules/` — `ruleconditions.common`, `.imdb`, `.mp3`, `.mvid`, `.nfo`, `.tv`, `.zeroday`, each with a matching `.inc` description file.
* `libs/` — `BeRoHighResolutionTimer`, `FLRE`, `FastMM5`, `Indy10`, `LibTar`, `TRegExpr`, `ZeosLib`, `lkJSON`, `mORMot2`, `pasmp`, `rcmdline`.
* `config/` — `slftp.ini`, `slftp.imdbcountries`, `slftp.knowngroups`, `slftp.languagebase`, `slftp.precatcher`, `slftp.skip`, `slftp.skipgroups`, `slftp.spamconf`.
* `helpfiles/` — source `.txt` files for the `!help` system (read from a populated `help/` directory at runtime).

---

## 4. Build System

### Linux / Free Pascal

The canonical build file is `Makefile`. It uses `fpc` with `-MDelphi` compatibility mode.

```bash
# Native release build (default target = clean + slftp)
make

# Explicit architectures
make slftp_32
make slftp_64

# Debug build (useful for gdb)
make slftp_debug
make slftp_64_debug

# Profiling / diagnostics
make slftp_debug_heaptrace   # FPC heap trace (-gh)
make slftp_debug_valgrind    # Valgrind-compatible debug info (-gv)
make slftp_debug_gprof       # gprof profiling (-pg)
make slftp_debug_vtune       # Intel VTune

# Build + install (install target copies slftp to ~/slftp by default)
make all
make all_64

# Clean build artifacts
make clean
```

Compiler flags from `Makefile`:

```makefile
CC = fpc
CFLAGS = -MDelphi -O3 -Xs
CDBFLAGS = -dDEBUG -MDelphi -gl -gp -gw3
CINCLUDES = -Fuirccommands -Furules -Fulibs/BeRoHighResolutionTimer ...
```

### Windows / Delphi

Use `make.bat` from a Windows command prompt:

```cmd
make.bat slftp_64
make.bat slftp_32
make.bat test_64
make.bat clean
```

`make.bat` invokes `dcc32.exe`/`dcc64.exe` on `slftp.dpr`. It extracts unit search paths from `slftp.dproj` via `extractUnitSearchPaths.ps1`.

Windows test targets additionally download OpenSSL 3.5 DLLs from `gitlab.com/slftp/binaries`, copy `libcrypto-3-x64.dll`, `libssl-3-x64.dll`, and `legacy.dll` into `tests\`, compile `tests\taskhttpimdbTests.rc`, then compile and run `tests\slftpUnitTests.dpr`.

### Git revision patch

Both build systems temporarily patch `slftp.inc` to embed the current git HEAD short hash into `SL_REV`:

```bash
make revpatch        # patches SL_REV
make revpatchrevert  # restores SL_REV to ''
```

The normal `make slftp` targets run `revpatch` before compiling and `revpatchrevert` after. `make test` does **not** patch the revision.

### Runtime file locations

At runtime slFtp expects most files in the **same directory as the binary**:

* `slftp.ini` or `slftp.cini` (encrypted) — main config
* `sites.dat` — encrypted site database
* `slftp.kb`, `slftp.rules`, `slftp.chans`
* `rtpl/*.rtpl` (per-site rules), `rtpl/*.settings`, `rtpl/*.chans` (when `split_site_data` is enabled)
* `databases/` folder — SQLite databases (`stats.db`, `tvinfos.db`, etc.)
* `backup/` — runtime backups
* `help/` — populated help text files (source lives in `helpfiles/`)

`Makefile` installs to `~/slftp` by default (`SLFTPPATH`).

---

## 5. Testing

### Test framework

The project uses a vendored DUnit-compatible framework located in `tests/fptest/`.

* Linux runner: `tests/slftpUnitTests.lpr` (defines `TextRunner`).
* Windows runner: `tests/slftpUnitTests.dpr` (uses DUnitX).
* Setup unit: `tests/slftpUnitTestsSetup.pas` initializes slftp subsystems before tests run.
* `tests/slftpUnitTestsSetupIndyOpenSSL.pas` provides a base `TTestIndyOpenSSL` class for OpenSSL-related tests.

### Running tests on Linux

```bash
make test
```

This executes:

1. `make clean`
2. Compiles `tests/slftpUnitTests.lpr`
3. Runs `./tests/slftpUnitTests`
4. Cleans up test artifacts

### Important local precondition

`tests/slftpUnitTests.lpr` calls `CommonFileCheck` (`mrdohutils.pas`), which expects the following files next to the test executable:

* `slftp.ini` or `slftp.cini`
* `slftp.knowngroups`
* `slftp.precatcher`
* `slftp.spamconf`
* `slftp.languagebase`
* `slftp.skip`
* `slftp.imdbcountries`
* `slftp.skipgroups`
* `mirktrade.conf`

The Makefile does **not** copy `config/` into `tests/` automatically; GitLab CI does. Locally you must do:

```bash
cp config/* tests/
make test
```

### Running tests on Windows

```cmd
make.bat test_64
make.bat test_32
```

These targets download OpenSSL 3.5 binaries automatically, copy the required DLLs into `tests\`, compile the `taskhttpimdbTests` resource file, build the DUnitX test runner, and execute it with `--exitbehavior:Continue`. Results are written to `tests\dunitx-results.xml`.

### Writing tests

* Inherit from `TTestCase`.
* Use `CheckEquals`, `CheckTrue`, etc.
* The file `tests/fptest/FPCUnitCompatibleInterface.inc` provides additional `Assert*` wrappers.
* Test data fixtures live in `tests/json/` and `tests/webpages/`.
* `taskhttpimdbTests` embeds JSON/HTML fixtures via `taskhttpimdbTests.rc`/`taskhttpimdbTests.res`.

---

## 6. Code Style Guidelines

Project conventions are documented in `CONTRIBUTING.md`. Highlights:

### Naming

| Scope | Prefix | Example |
|-------|--------|---------|
| Global `interface` vars | `Gl` | `GlSkiplistRegex` |
| Unit-global `implementation` vars | `gl` | `glSQLite3Lock` |
| Class/record fields | `F` | `FNetname` |
| Local variables | `f` | `fChanSettingsObj` |
| Parameters | `a` | `aNetname` |
| Implementation-only functions | `_` | `_findMP3GenreOnAnnounce` |

### Comments

* Use [PasDoc](https://github.com/pasdoc/pasdoc/wiki) style comments.
* Every variable and function in the `interface` section **must** be documented.
* Inline field docs use `//<`:

```pascal
private
  FNetname: String; //< netname of IRC network
```

### General guidelines

* Prefer small, focused functions.
* Avoid code duplication.
* Prefer generic collections (`System.Generics.Collections`).
* Use `const` for parameters where possible.
* Indentation in existing code is 2 spaces, no tabs.

### Commit messages

Format: `<type>(<scope>): <subject>`

Types: `feat`, `fix`, `style`, `refactor`, `perf`, `chore`, `remove`, `update`.

Rules:

* Header max 100 characters.
* Subject in imperative present tense, no trailing dot.
* Footer may contain `BREAKING CHANGE:` and `Closes #123` references.

---

## 7. Versioning and Releases

Version constants are in `slftp.inc`:

```pascal
SL_VERSION: string = '1.5.11b1';
SL_REV: string = '';           // patched at build time with git short hash
HELP_VERSION: string = '0.5.10';
```

* Semantic versioning; beta releases append `b<n>`.
* Releases are created from annotated tags on the `dev` branch.
* Tag names should **not** contain a `v` prefix (e.g. `1.5.7`, not `v1.5.7`).
* After tagging, bump `SL_VERSION`/`HELP_VERSION` in `slftp.inc` on `dev`.

Release flow:

```bash
git checkout dev
# ensure slftp.inc has correct versions
git tag -a 1.5.7 -m "v1.5.7"
git push origin 1.5.7
```

GitLab CI (`.gitlab-ci.yml`) then builds Windows/Linux binaries, runs tests, generates source docs, and publishes packages.

---

## 8. Security Considerations

### Encrypted configuration and site database

* `encinifile.pas` provides `TEncIniFile` with optional Blowfish-CBC encryption + zlib compression.
* `configunit.pas` loads `slftp.cini` if present, otherwise `slftp.ini`.
* `sites.dat` is encrypted with the master passphrase (MD5 of the password entered at startup).
* The raw master password is wiped after hashing (`WipePass` / `_WipeString`).
* `TEncIniFile.Save` writes to `.sltmp` first, then atomically renames to the target.

### IRC encryption

* `ircblowfish.ECB.pas` — ECB mode with custom base64 alphabet.
* `ircblowfish.CBC.pas` — CBC mode via OpenSSL EVP, prefixed with `+OK *`.
* `ircblowfish.plaintext.pas` — no-op fallback.

### SSL/TLS

* `slssl.pas` loads `libcrypto.so` / `libssl.so` from the binary directory first, then falls back to system libraries.
* The project is effectively targeting OpenSSL 3.x; backward compatibility with 1.1.x is provided by mORMot2.
* FTP SSL modes: `sslNone`, `sslImplicitSSL`, `sslAuthSsl`, `sslAuthTLS`.

### Command-line encryption

* `commandlineutil.pas` provides CLI encryption/decryption for files (`-e`, `--infile`, `--outfile`, `--pw`).

---

## 9. Useful Documentation

| File | Topic |
|------|-------|
| `docs/compiling` | Build dependencies and compile instructions |
| `docs/installation` | Installation walkthrough |
| `docs/usage` | Runtime usage and configuration |
| `docs/debug_howto` | Debug builds and gdb |
| `docs/faq` | Frequently asked questions |
| `docs/precatcher` | Precatcher configuration |
| `docs/indexer` | Indexer setup |
| `docs/tvinfo` | TV info lookup |
| `docs/chains` | Transfer chains |
| `CONTRIBUTING.md` | Commit format, coding guidelines, release process |
| `CHANGELOG` | Feature/bug-fix history with legend |

---

## 10. Quick Reference Commands

```bash
# Build and install native binary
make all

# Build 64-bit binary only
make slftp_64

# Debug build
make slftp_debug

# Run unit tests (copy config files first)
cp config/* tests/
make test

# Clean all build artifacts
make clean
```

Windows:

```cmd
make.bat slftp_64
make.bat test_64
make.bat clean
```
