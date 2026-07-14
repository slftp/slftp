# slFtp — Agent Guide

This document is a quick-start orientation for AI coding agents working on the
slFtp ("soulless Ftp") codebase. slFtp is an open-source, multi-threaded FTP
client/autotrader written in Object Pascal. It is designed for the "scene"
ecosystem: it connects to multiple FTP sites, listens to IRC announce bots, and
trades releases between sites based on configurable rules.

## Technology Stack

- **Language:** Object Pascal (Delphi-compatible)
- **Compilers supported:**
  - **Free Pascal Compiler (FPC) 3.2+** on Linux, BSD, and Windows
  - **Embarcadero Delphi 10.3 Rio (10.3.x) or newer** on Windows
- **UI:** Text-mode console UI built on `slvision`/`slconsole`
- **Primary platforms:** Linux x64, Windows x64 (x86/32-bit builds still supported)
- **Key external dependencies:**
  - OpenSSL 1.1.1 / 3.x (TLS/SSL, Indy OpenSSL bindings)
  - ncurses (console UI on Unix)
  - SQLite3 (`libsqlite3.so` / `sqlite3.dll`)
  - MySQL/MariaDB client library (optional, for addpre DB)
- **Version:** defined in `slftp.inc` (`SL_VERSION`, `HELP_VERSION`); git short
  hash is patched into `SL_REV` at build time.

## Project Layout

```
.
├── slftp.lpr                # FPC program entry point
├── slftp.dpr                # Delphi program entry point
├── slftp.inc                # Version constants, library version requirements
├── common.inc               # Shared constants and string tables
├── Makefile                 # FPC build orchestration (Linux/BSD)
├── make.bat                 # Delphi build orchestration (Windows)
├── replace_git_commit.pl    # Patches SL_REV with the current git short hash
├── extractUnitSearchPaths.ps1
│                            # Extracts Delphi unit search path from slftp.dproj
├── *.pas                    # Core application units (~108 units in the root)
├── irccommands/             # IRC command handler units
├── rules/                   # Rule condition implementation units
├── libs/                    # Vendored third-party libraries
│   ├── Indy10/              # Indy networking stack
│   ├── mORMot2/             # ORM/SOA/REST framework used for SQLite/MySQL access
│   ├── ZeosLib/             # Database connectivity
│   ├── FastMM5/             # Memory manager (Delphi builds)
│   ├── FLRE/                # Fast regex engine
│   ├── TRegExpr/            # Regular expressions
│   ├── lkJSON/              # JSON handling
│   ├── LibTar/              # TAR archive support
│   ├── pasmp/               # Parallel-for/multiprocessing helpers
│   ├── BeRoHighResolutionTimer/
│   └── rcmdline/            # Command-line parsing
├── config/                  # Default configuration files
│   ├── slftp.ini            # Main configuration
│   ├── slftp.knowngroups    # Known release group list
│   ├── slftp.precatcher     # IRC announce parsing rules
│   ├── slftp.skip           # Global skip list
│   ├── slftp.skipgroups     # Group skip list
│   ├── slftp.spamconf       # IRC spam filter
│   ├── slftp.languagebase   # Language mappings
│   └── slftp.imdbcountries  # IMDB country mappings
├── helpfiles/               # In-app help text for every IRC command
├── docs/                    # User and developer documentation
└── tests/                   # Unit tests and test fixtures
    ├── slftpUnitTests.lpr   # FPC test runner
    ├── slftpUnitTests.dpr   # Delphi test runner
    ├── slftpUnitTestsSetup.pas
    ├── fptest/              # FPCUnit-compatible test framework
    ├── *.pas                # Individual test units
    ├── json/                # JSON fixtures for IMDB/TV tests
    └── webpages/            # Web-scraping fixtures and Python requirements
```

## Runtime Architecture

slFtp is a long-lived multi-threaded daemon with a console UI.

- **Entry point:** `slftp.lpr` / `slftp.dpr`
  - If command-line arguments are present, the binary runs in CLI utility mode
    (`commandlineutil.pas`) for encrypting/decrypting internal files or showing
    the version.
  - Otherwise it starts the console UI (`ConsoleStart` in `console.pas`).
- **Main lifecycle:** `mainthread.pas` (`Main_Init`, `Main_Run`, `Main_Iter`,
  `Main_Stop`, `Main_Uninit`) initializes TCP/OpenSSL/SQLite/MySQL, then runs
  the main loop.
- **Console UI:** `console.pas` wraps `slvision`/`slconsole` and provides the
  admin window, queue view, slot view, IRC windows, and site windows.
- **IRC layer:** `irc.pas` implements per-network IRC client threads, including
  SSL, blowfish (ECB/CBC), SOCKS5, and channel/role management
  (`ircchansettings.pas`).
- **Site / FTP layer:** `sitesunit.pas` models FTP sites and their slots
  (`TSite`, `TSiteSlot`). Each slot is a TCP thread that logs in, idles, and
  executes tasks.
- **Queue / task layer:** `queueunit.pas` owns one `TQueueThread` per site and
  assigns `TTask` objects to available slots. `tasksunit.pas` is the base task
  class; concrete tasks live in `task*.pas` units (race, dirlist, login, nuke,
  etc.).
- **Knowledge base:** `kb.pas` tracks releases/pazos (`pazo.pas`) and dispatches
  lookup tasks (TV, IMDB, NFO, genre, pretime, fake check, etc.).
- **Precatcher:** `precatcher.pas` parses IRC announce lines into release events.
- **Rules engine:** `rulesunit.pas` evaluates site/section-specific conditions
  (`rules/ruleconditions.*.pas`) to decide whether a release is allowed.
- **Databases:** `dbaddpre`, `dbaddimdb`, `dbtvinfo`, `dbaddnfo`, `dbaddurl`,
  `dbaddgenre` provide local SQLite-backed (and optional MySQL-backed) caches.
- **Utilities:** `mystrings.pas`, `slmasks.pas`, `sllanguagebase.pas`,
  `slcriticalsection2.pas`, `slssl.pas`, `sltcp.pas`, etc.

## Build Commands

### Linux / BSD (FPC)

Default build uses `fpc` with `-MDelphi -O3 -Xs` and links a 64-bit binary:

```bash
make                # clean + build x64 slftp binary
make slftp          # same as default
make slftp_64       # explicit x86_64 build
make slftp_32       # explicit i386 build
make debug          # debug build with lineinfo (-dDEBUG -gl -O1)
make heaptrace      # debug build + heap trace
make valgrind       # debug build + Valgrind support
make gprof          # debug build + profiling
make vtune          # debug build + VTune support
make test           # clean + build and run FPC unit tests
make clean          # remove .o/.ppu/binaries
```

The build temporarily patches the git short hash into `slftp.inc` and reverts it
afterwards via `replace_git_commit.pl`.

### Windows (Delphi)

```cmd
make.bat slftp_64        # 64-bit release build (default)
make.bat slftp_32        # 32-bit release build
make.bat slftp_64_debug  # 64-bit debug build
make.bat test_64         # build and run DUnitX tests
make.bat clean           # clean build artifacts
```

`make.bat` reads the unit search path from `slftp.dproj` using
`extractUnitSearchPaths.ps1`, so the `.dproj` must be kept in sync with the
`Makefile` search paths.

### Runtime Dependencies

On Debian/Ubuntu the documented build dependencies are:

```bash
apt install make binutils zlib1g-dev libncurses-dev libsqlite3-dev libssl-dev fpc-3.2.2
```

OpenSSL and SQLite libraries must be available next to the binary or in the
system library path. See `docs/compiling` and `docs/libinstaller.sh` for
building your own libraries.

## Configuration Files

At runtime slFtp expects a set of plain-text and encrypted configuration files in
its working directory:

- `slftp.ini` — main runtime config (debug, console, IRC, sites, timeouts,
  sections, DB credentials, etc.).
- `slftp.cini` — encrypted master config generated at first run.
- `slftp.knowngroups`, `slftp.precatcher`, `slftp.skip`,
  `slftp.skipgroups`, `slftp.spamconf`, `slftp.languagebase`,
  `slftp.imdbcountries`, `mirktrade.conf` — static data files.
- Generated at runtime: `sites.dat`, `slftp.kb`, `slftp.rules`,
  `slftp.chans`, `slftp.ranks`, `slftp.speedstats`, `slftp.history`,
  `slftp.renames`, `slftp.nukequeue`, `slftp.preurls`, `slftp.socks5`,
  `slftp.news`.

The default versions live in `config/`. Copy them to the runtime directory and
edit `slftp.ini` before starting the binary. On first start slFtp asks for a
master password that is used to encrypt/decrypt internal files.

## Testing

### Framework

- **FPC:** Uses the bundled `fptest` framework (`tests/fptest/`) compatible with
  FPCUnit. Test cases inherit from `TTestCase` and register published methods.
- **Delphi:** Uses DUnitX (`tests/slftpUnitTests.dpr`).

### Test Organization

- `tests/slftpUnitTestsSetup.pas` centralizes initialization of the various
  subsystems before tests run (`InitialConfigSetup`, `InitialDebugSetup`,
  `InitialKbSetup`, etc.).
- New test units must be added to both `tests/slftpUnitTests.lpr` (FPC) and
  `tests/slftpUnitTests.dpr` (Delphi).
- Test fixtures include JSON files in `tests/json/` and web pages in
  `tests/webpages/`.

### Running Tests

```bash
# FPC/Linux
make test

# Windows
make.bat test_64
```

The CI pipeline (`.gitlab-ci.yml`) builds and runs tests on both Linux and
Windows x64.

## Code Style Guidelines

The project follows the conventions documented in `CONTRIBUTING.md`. Highlights:

- **Commit messages:** use `<type>(<scope>): <subject>` form, with types
  `feat`, `fix`, `style`, `refactor`, `perf`, `chore`, `remove`, `update`.
  Keep every line ≤ 100 characters.
- **Comments:** write interface-level documentation in
  [PasDoc](https://github.com/pasdoc/pasdoc/wiki) style. All public variables
  and functions in the `interface` section must be documented.
- **Naming:**
  - Global variables in `interface`: `Gl` prefix + uppercase, e.g.
    `GlSkiplistRegex`.
  - Unit-global variables in `implementation`: `gl` prefix + uppercase, e.g.
    `glSQLite3Lock`.
  - Class/record fields: `F` prefix + uppercase, keep `private` if possible,
    expose via `property`.
  - Local variables: `f` prefix + uppercase (e.g. `fInputStr`); loop indices
    may be `i`, `j`, etc.
  - Parameters: `a` prefix + uppercase (e.g. `aNetname`), prefer `const`.
  - Implementation-only helper functions: `_` prefix (e.g.
    `_findMP3GenreOnAnnounce`).
- **Structure:** prefer small, focused functions; use generic collections over
  legacy container classes; avoid code duplication; avoid global variables
  unless truly necessary.

## Release and Deployment

- Versioning follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
  with beta suffixes like `1.5.8b1`.
- Release builds are produced by GitLab CI (`.gitlab-ci.yml`) only from
  annotated tags on the `dev` branch. CI builds Linux and Windows x64 binaries,
  packages config/docs/helpfiles/sources, uploads them to the GitLab package
  registry, and creates a GitLab release.
- To make a release locally, update `SL_VERSION`/`HELP_VERSION` in `slftp.inc`,
  create an annotated tag, push it, then bump the version in `slftp.inc` for the
  next cycle.

## Security Considerations

- slFtp stores site credentials, IRC passwords, and encrypted configuration in
  `sites.dat` and `slftp.cini`. These files are encrypted with a master
  password that is set on first run. Treat them as sensitive.
- The command-line utility (`slftp -e/-d --infile ... --outfile ... --pw`)
  encrypts/decrypts internal files; when using `--pw` the terminal echo is
  disabled on both Windows and Unix.
- TLS/SSL is provided by OpenSSL. The binary loads `libcrypto`/`libssl` at
  runtime; keep these libraries up to date.
- The optional ident server binds port 113; on Unix this requires root unless
  using a higher port.
- The code base uses `TSlCriticalSection2` with optional timeout locking for
  deadlock debugging (`event_based_locking_timeout` in `slftp.ini`); this is a
  diagnostic feature, not a security boundary.
- Do not commit production config files or `sites.dat` to version control.

## Useful References

- `README.md` — project overview and feature list
- `CONTRIBUTING.md` — full coding and commit conventions
- `docs/compiling` — detailed build instructions for Linux/BSD/Windows
- `docs/fpc_howto` — installing FPC from source/snapshots
- `docs/usage` — getting started with IRC networks, sites, sections, rules,
  and routes
- `docs/debug_howto` — running under `gdb` to capture crash info
- `docs/ircchanroles_description` — meaning of IRC channel roles
- `CHANGELOG` — version history and new settings
- `helpfiles/*.txt` — help text for every IRC command exposed by the bot
