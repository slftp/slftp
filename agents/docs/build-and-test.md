# Build & test

CI builds and tests with both compilers (FPC on Linux, Delphi on Windows). For code differences, writing
tests that work in both frameworks and keeping the project files in sync, see [compilers](compilers.md).

## Linux (FPC)

- Release build: `make slftp_64`; debug build: `make slftp_64_debug` (further targets: see `Makefile`)
- CI builds with `fpc` through the `Makefile`. `slftp.lpi` (Lazarus project) exists but is not used by CI.
- Unit tests use fptest (`tests/fptest`) and run via `make test`
  - The test binary needs the config files in `tests/`. CI runs `mv config/* tests/`; locally **copy**
    them instead so `config/` stays intact.
  - The runner waits for `<Enter>` at the end, so run it non-interactively: `make test < /dev/null`
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
    Git Bash.
  - Every run deletes all `.dcu`/`.dll` files in the repository (`del /q /s`), downloads OpenSSL and does a
    full rebuild, so it takes a while. An aborted run can leave a partial `tests\openssl-*.zip` behind; it
    is gitignored and overwritten by the next run.
- `make.bat` must be checked out with CRLF line endings. With LF, cmd.exe resolves `goto` labels wrongly and
  `make.bat test` restarts the test target endlessly instead of finishing. `.gitattributes` enforces CRLF;
  in a checkout made before that, delete `make.bat` and run `git checkout -- make.bat`.
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
build. A modified `slftp.inc` during or after an aborted build is expected — revert it, never commit it.
`SL_VERSION`/`HELP_VERSION` are only changed as part of the release process (see `CONTRIBUTING.md`).
