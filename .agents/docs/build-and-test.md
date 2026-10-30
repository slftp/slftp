# Build & test

CI builds and tests with both compilers (FPC on Linux, Delphi on Windows). For code differences, writing
tests that work in both frameworks and keeping the project files in sync, see [compilers](compilers.md).
On Windows, the skill [test-both-compilers](../skills/test-both-compilers/SKILL.md) runs both sides, FPC
through WSL. To run slFtp itself, see the skill [run-slftp](../skills/run-slftp/SKILL.md).

## Linux (FPC)

- Release build: `make slftp_64`; debug build: `make slftp_64_debug` (further targets: see `Makefile`)
- CI builds with `fpc` through the `Makefile`. `slftp.lpi` (Lazarus project) exists but is not used by CI.
- All FPC targets pass `-dNOPATCHRTL` (`MORMOTFLAGS` in the `Makefile`). Don't drop it when adding or
  changing targets; see [compilers](compilers.md#fpc-nopatchrtl).
- Unit tests use fpcunit (ships with FPC) and run via `make test`
  - `make test` copies `config/*` into `tests/` itself; the test binary reads the config files from its
    own directory. CI moves them there in `before_script` instead, so `config/` is empty and the copy is
    skipped. Without the config files the binary exits with code 2.
  - The tests need OpenSSL 3 in `tests/`: `libcrypto.so.3`, `libssl.so.3` and `legacy.so`. They are
    gitignored (`tests/*.so*`, `*.so`), so a fresh clone or worktree lacks them. Copy them from the system,
    as CI does in `before_script`:
    `cp /usr/lib/x86_64-linux-gnu/{libcrypto.so.3,libssl.so.3,ossl-modules/legacy.so} tests/`.
    `slssl.pas` sets the OpenSSL provider search path to the binary's own directory, so without
    `tests/legacy.so` the SSL and IRC Blowfish CBC tests fail with "legacy ssl provider not loaded!".
  - The run is non-interactive and bounded by `timeout 300`; exit code 124 means the binary hung.
  - It writes the plain console output plus a JUnit report to `tests/fpcunit-junit.xml` (gitignored); CI
    uploads it as GitLab test report.
  - Run the binary by hand: `./tests/slftpUnitTests --all --format=plain`, a single suite with
    `--suite=<name>`, list the tests with `-l`. Without arguments it only prints the usage. With
    `--junit=<file>` it always runs all tests and ignores the other options.
  - Exit code: bit 0 is set on failures, bit 1 on errors (0 = all passed).
  - The runner exits via `fpExit` without running unit `finalization` sections (see the comment at the
    end of `tests/slftpUnitTests.lpr`).
- Running `slftp` itself needs the config files from `config/`, the three OpenSSL files as for the tests,
  and `libsqlite3.so` in its directory: `Main_Init` loads SQLite from `./libsqlite3.so` on Linux, so start
  it there with `LD_LIBRARY_PATH=.`. The CI image has no libsqlite3. A `masterpass.txt` in the same
  directory answers the master password prompts (`console.pas`), and the first start creates `sites.dat`
  with it.
- `make clean` removes compiled `.o`/`.ppu` files. They are created next to the sources and are gitignored.
- After switching branches, do not compile single units against existing `.ppu` files — stale units can
  crash fpc. Always do a full `make` build.

## Windows (Delphi)

- `make.bat <target>` with the targets `slftp`, `slftp_32`, `slftp_64`, `slftp_debug`, `slftp_32_debug`,
  `slftp_64_debug`, `clean`, `test`, `test_32`, `test_64` (default `slftp_64`)
- Needs `dcc32.exe`/`dcc64.exe` on `PATH`. Unit search paths are read from `slftp.dproj` by
  `extractUnitSearchPaths.ps1`.
- The test targets download OpenSSL and compile `tests/slftpUnitTests.dpr` (DUnitX).
  - The test binary needs the config files in `tests\`. CI moves `config\*` there; locally **copy** them.
    Without them the runner prints a file check and waits for `<Enter>`.
  - Run from the repository root: `.\make.bat test` in PowerShell, `cmd //c ".\\make.bat test < NUL"` in
    Git Bash. Keep the `.\`: some agent shells set `NoDefaultCurrentDirectoryInExePath`, which stops cmd.exe
    from finding `make.bat` in the current directory.
  - Every run deletes all `.dcu`/`.dll` files in the repository (`del /q /s`), downloads OpenSSL and does a
    full rebuild, so it takes a while. An aborted run can leave a partial `tests\openssl-*.zip` behind; it
    is gitignored and overwritten by the next run.
- `make.bat` must be checked out with CRLF line endings. With LF, cmd.exe resolves `goto` labels wrongly and
  `make.bat test` restarts the test target endlessly instead of finishing. `.gitattributes` enforces CRLF;
  in a checkout made before that, delete `make.bat` and run `git checkout -- make.bat`.
- Running `slftp.exe` needs the config files from `config\`, the OpenSSL DLLs as for the tests and
  SQLite in its directory, which the 64-bit build loads as `sqlite3-64.dll` (mORMot2's
  `SQLITE_LIBRARY_DEFAULT_NAME`), the 32-bit build as `sqlite3.dll`. `make.bat slftp_64` provides
  neither OpenSSL nor SQLite. Missing, slFtp shows the error in a Message window, and Enter there ends
  it.
- Delphi leaves `.dcu` files, `__history/` and `__recovery/` folders behind. They are not part of the
  source; never commit them.

## Tests that need the network

`tests/httpTests.pas` fetches pages from external services (tiffara.com, boxofficemojo.com, tvmaze.com,
httpbin.org). When one of them fails with `HTTP GET reply is empty. ( / 0)`, first check whether the host
still resolves and answers (e.g. with `curl`) before suspecting the code. If a service is gone, switch the
test to a working one.

## Diagnosing CI failures

- Jobs of a pipeline: `https://gitlab.com/api/v4/projects/slftp%2Fslftp/pipelines/<pipeline id>/jobs`
- Job log: `https://gitlab.com/slftp/slftp/-/jobs/<job id>/raw` (the API `trace` endpoint returns nothing
  without login)
- Compare with the last green pipeline on `dev` (`.../pipelines?ref=dev`) and with the commits in between.
  If the failing tests are unrelated to those commits, suspect the environment, e.g. a dead external
  service (see above).

## `slftp.inc`

Both `make` and `make.bat` patch the current git hash into `SL_REV` in `slftp.inc` and reset it after the
build, also when it fails. A modified `slftp.inc` during or after an aborted build is expected — revert it,
never commit it.
`SL_VERSION`/`HELP_VERSION` are only changed as part of the release process (see `CONTRIBUTING.md`).
