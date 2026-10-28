---
name: run-slftp
description: Start slFtp in a fresh runtime directory, type commands into its console, read the screen and stop it, with the Linux (FPC) or the Windows (Delphi) build. Use to try a change in the real program, e.g. an IRC command or a startup change, or to reproduce a bug.
metadata:
  fpc: verified
  delphi: verified
---

# Run slFtp

On Linux this drives the FPC build in `tmux`, on Windows the Delphi build in a console window of its
own. Both use the scripts in `.agents/scripts/`; glftpd-e2e uses them too. In WSL, `tmux` sessions end
with the last `wsl.exe` call ([wsl-fpc-setup](../../docs/wsl-fpc-setup.md#pitfalls)), so on Windows
run the Delphi build.

## Requirements

- **Linux:** `tmux`, `libsqlite3-0`, and for the build Docker and `git` as in the requirements of
  [glftpd-e2e](../glftpd-e2e/SKILL.md#requirements)
- **Windows:** Delphi as for `make.bat` ([build-and-test](../../docs/build-and-test.md#windows-delphi)),
  internet access for the first `prepare`

## Run

1. Build:
   - Linux: `bash .agents/scripts/fpc-wsl.sh --export ~/slftp-run/build`: the
     release build of `HEAD` plus uncommitted changes in the CI image, in the clone of
     [test-both-compilers](../test-both-compilers/SKILL.md); the checkout is only read. `<dir>.commit`
     holds the tested commit.
   - Windows: `.\make.bat slftp_64` in the repository root, which builds `slftp.exe` there.
2. Use the commands below, on Linux `bash .agents/scripts/slftp-run.sh <command>`, on Windows
   `powershell -NoProfile -ExecutionPolicy Bypass -File .agents\scripts\slftp-run.ps1 <command>`:

   | Command | Does |
   |---------|------|
   | `prepare` | fresh runtime directory with the build, `config/*`, OpenSSL, SQLite and `masterpass.txt` (password `slftp-run`); no sites yet |
   | `start` | starts slFtp there, waits for its prompt and prints the version line |
   | `send '<line>'` | types the line and Enter into the Admin window |
   | `screen` | prints the screen |
   | `wait '<text>' [<count>] [<seconds>]` | waits until `<count>` lines (default 1) show the text, at most `<seconds>` (default 30); on Windows `-Count` and `-Seconds` |
   | `stop` | types `!die`, waits up to 60 s for the exit (else kills slFtp) and prints the `[EXCEPTION]` lines of `slftp.log` |
   | `kill` | ends slFtp at once, e.g. after a failed start |

   - Runtime directory: `~/slftp-run/run` on Linux (`prepare <build dir>` takes another build than
     `~/slftp-run/build`), `%LOCALAPPDATA%\slftp-run\run` on Windows (`-Dir` for another one, `-Exe`
     for another `slftp.exe`). `prepare` replaces it, including `slftp.log` and `sites.dat`.
   - Windows: the window of slFtp is hidden; `start -Visible` shows it to the user. On Windows,
     `prepare` downloads OpenSSL from the URL that `make.bat` uses and SQLite from sqlite.org, pinned
     in `slftp-run.ps1`. Both are cached in `%LOCALAPPDATA%\slftp-run\dl`; delete that folder to
     download them again.
3. Exit code: `0` done, `1` failed (the output says why), the text did not show up in time, or slFtp
   doesn't run; `10` (Linux) a tool is missing.

## Pitfalls

- The Admin window only executes lines with the IRC command prefix (`!` by default); it silently
  ignores other lines.
- Wait for a reply with `wait`, not with a single `screen`: a single read can catch slFtp redrawing
  its window, which then looks empty. Lines that scrolled out of the Admin window are gone, and slFtp
  clears the screen on exit, so read what you need before `stop`.
- A start error, e.g. a missing library, shows up in a Message window: `start` fails and prints the
  screen. `send` would answer it (Enter clicks Ok), and slFtp then exits; end it with `kill`.
- On Windows every command takes a few seconds (two PowerShell starts). Scripts that need many fast
  reads source `slftp-run.sh` in WSL with `slftp_windows=1`, as `e2e.sh` does, which keeps one
  PowerShell process for the whole run.
- On exit slFtp logs `[EXCEPTION] QueueFire` and `RemovePazo` access violations, with both builds and
  also on `dev`; `stop` lists them, they are no failure of the run. Sometimes it doesn't exit at all
  and repeats `QueueFire` and `TSite.QueueFireInverval` exceptions; `stop` then kills it after 60 s
  and says so.

## Report

4. Report the build (commit, and whether uncommitted changes were included), what you typed and what
   slFtp answered. Say which build you ran; the other one is unverified by the run.
