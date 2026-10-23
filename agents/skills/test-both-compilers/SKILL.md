---
name: test-both-compilers
description: Build slFtp and run the unit tests with FPC and Delphi from a Windows machine, FPC through WSL in the GitLab CI image. Includes uncommitted changes without committing them. Use to verify a change on both compilers before committing or opening a merge request.
metadata:
  fpc: verified
  delphi: verified
---

# Test both compilers from Windows

For a Windows machine with Delphi. On Linux, run `make slftp_64` and `make test` instead
([build-and-test](../../docs/build-and-test.md)) and report Delphi as unverified
([compilers](../../docs/compilers.md)).

The two sides are independent and can run at the same time.

## FPC (in WSL)

1. From the repository root:
   ```powershell
   powershell -NoProfile -ExecutionPolicy Bypass -File agents\skills\test-both-compilers\fpc-wsl.ps1
   ```
   - Builds `HEAD` plus uncommitted changes and untracked (not ignored) files, so nothing needs to be
     committed. `-Ref <ref>` builds only the committed state of that ref.
   - Runs hidden, waits for the run and prints a summary. It takes a few minutes, so run the command in
     the background or with a long timeout (`-TimeoutMinutes`, default 60).
   - The first run (clone, image pull) takes longer and shows a console window the user can follow;
     `-Visible` does that for any run. The window closes itself on success and stays open on failure.
   - Only reads the Windows checkout. Clone, build output, log and exit code are in the WSL home
     (`~/slftp-fpc-build*`). `-Distro <name>` selects a WSL distribution other than the default.
2. Exit code of the command:
   - `0`: build and tests passed
   - `1`: build or tests failed. The summary shows the errors; the full log is `~/slftp-fpc-build.log`
     in WSL.
   - `2`: WSL, Docker or `git.exe` are missing. Set them up with
     [wsl-fpc-setup](../../docs/wsl-fpc-setup.md); its first steps need the user.
   - `3`: the window was closed or the timeout hit before the run finished
   - `4`: another FPC run is active (all runs share one build clone): another worktree or agent, or a
     run that outlived a timeout. Retry when it has finished.

## Delphi

3. Copy `config\*` into `tests\` unless it is there already (the copies are gitignored), then run
   `.\make.bat test` from the repository root. It deletes all `.dcu`/`.dll` files, downloads OpenSSL
   and rebuilds everything, so it takes a while. Exit code `0` means passed; details and the Git Bash
   variant: [build-and-test](../../docs/build-and-test.md).

## Report

4. Report both results with the tested commit and whether uncommitted changes were included. A side
   that could not run stays unverified; say so.
