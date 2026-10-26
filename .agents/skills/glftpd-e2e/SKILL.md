---
name: glftpd-e2e
description: Run an end-to-end scenario of slFtp against real glftpd sites with pzs-ng, two or more Docker containers, driven through slFtp's console; e.g. an FXP race between two sites. Use on Linux, or on Windows through WSL, to check a change to what slFtp does on sites (races, dirlists, site handling) before committing or opening a merge request.
metadata:
  fpc: verified
  delphi: unverified
---

# glftpd end-to-end test

Runs on Linux with Docker, and on Windows in WSL. It tests the Linux build of slFtp only: the
scenarios type into its console through `tmux`, which can't drive the Delphi build on Windows. Report
Delphi as unverified for this kind of test.

A run, all on Linux or inside WSL (`~/slftp-e2e/`):

1. Builds the site image `slftp-e2e-glftpd`: glftpd and pzs-ng, both pinned in `e2e.sh` (`gl_url` with
   `gl_sha512`, `pzs_commit`), configured by the files in `glftpd/` (`zsconfig.h` is compiled in).
2. Builds slFtp with `make slftp_64` in the CI image through `fpc-wsl.sh --export` of
   [test-both-compilers](../test-both-compilers/SKILL.md), in its separate clone `~/slftp-fpc-build`:
   the checkout is only read, also on Linux. Both skills share that clone and its lock.
3. Starts the sites the scenario needs: site `<n>` is the container `slftp-e2e-glftpd-<n>` on
   `127.0.0.1:2120+<n>`, user `glftpd`/`glftpd`, TLS required for logins, listings and data, sections
   at `/<section>`. All containers use the host network, so FXP between them stays on 127.0.0.1. If
   one of their ports (also the passive ones from 50000) is in use, e.g. by a glftpd of the host, all
   sites move to the next free block: `2220+<n>` and 51000, and so on. The result notes that.
4. Starts slFtp in a fresh runtime directory with the default config and runs the scenario.
5. Exits slFtp and removes the containers; the image and the downloads stay.

## Requirements

- **Linux:** x86-64, `git`, `tmux`, `curl`, `python3`, `ss` (iproute2) and `libsqlite3-0`, and Docker
  Engine usable without `sudo`: the user is in the group `docker`, so `docker info` works. Rootless
  Docker doesn't fit: the build runs as `--user $(id -u)` and expects its output to belong to the user.
  Installing needs `sudo`, so ask the user, e.g. on Debian or Ubuntu `sudo apt-get install docker.io
  tmux && sudo usermod -aG docker $USER`, then log in again. Run as `root` it works as well.
- **Windows:** WSL with Docker, set up with [wsl-fpc-setup](../../docs/wsl-fpc-setup.md). The other
  tools come with Ubuntu.

## Run

1. From the repository root:
   - Linux:
     ```sh
     bash .agents/skills/glftpd-e2e/e2e.sh race > /dev/null; echo "exit code $?"
     bash .agents/skills/glftpd-e2e/e2e.sh --summary
     ```
     The run prints the full log, hence the `> /dev/null`; `--summary` prints the important lines.
     `e2e.sh --baseline <ref> race` and `e2e.sh race <ref>` are the options below.
   - Windows:
     ```powershell
     powershell -NoProfile -ExecutionPolicy Bypass -File .agents\skills\glftpd-e2e\e2e.ps1 -Scenario race
     ```
     It runs `e2e.sh` in WSL hidden, waits and prints the summary. The first run (downloads, image
     builds, CI image pull) shows a console window the user can follow; `-Visible` does that for any
     run. The window closes itself on success and stays open on failure. `-Distro <name>` selects a
     WSL distribution other than the default.

   Both:
   - Test `HEAD` plus uncommitted changes and untracked (not ignored) files; a ref (`-Ref <ref>`)
     tests only the committed state of that ref.
   - A baseline (`-Baseline <ref>`) also builds `<ref>` and runs the scenario with it first, for a
     before/after comparison. Use it only when the user asks for one, it doubles the build time.
     Compare with the merge base (`git merge-base HEAD dev`), not with `dev` itself, which may have
     moved on.
   - A run with the race scenario takes about a minute, two with a baseline, the first run longer. Run
     it in the background or with a long timeout (`e2e.ps1`: `-TimeoutMinutes`, default 60).
2. Exit code (`e2e.ps1` / `e2e.sh`):
   - `0`: all checks passed
   - `1`: a check or the build failed. The summary shows the `FAIL` lines or the compiler errors. The
     result file `~/slftp-e2e/result-<change|baseline>.txt` holds each check's output and the console at
     the end; `slftp.log` is in `~/slftp-e2e/run-<change|baseline>/`, the full log in
     `~/slftp-e2e/e2e.log`.
   - `2` / `10`: WSL, Docker or a tool the scripts need is missing; the summary names it. See
     "Requirements".
   - `3` (only `e2e.ps1`): the window was closed or the timeout hit before the run finished
   - `4` / `11`: another run is active (all runs share the sites, and the build shares the clone of
     test-both-compilers). Retry when it has finished.


## Scenarios

| Scenario | What it checks |
|----------|----------------|
| `race` | `!spread` of a release from `E2E1` to `E2E2` with SSL FXP: pzs-ng marks it complete on both sites, both have the same files, slFtp reports success |

## Adding a scenario

A scenario is `scenarios/<name>.sh`, sourced by `e2e.sh`. Copy `race.sh` and add a row above. It sets
`sites` (default 1) and `sections` (each becomes `/<section>` on every site and a section in
`slftp.precatcher`), and defines `steps`. `steps` runs while slFtp is up and the sites are empty, and
records results with `check` and `note`. The helpers are documented where `e2e.sh` defines them:
`add_sites`, `console`, `wait_console`, `console_has`, `console_ok`, `make_release`, `ftp_upload`,
`site_exec`, `site_complete`, `site_same_files`, `seconds_since`. A scenario that connects to a site
itself takes the port from `site_port`, never a fixed one.

Pitfalls found so far:

- slFtp only allows a release on a site when a rule matches, e.g. `!ruleadd * <section> if default
  then ALLOW`, and only spreads along routes (`!routeset`).
- `!spread` reports `successfully finished` also when it sent nothing, e.g. when the release is not
  allowed anywhere or the section directory is missing on the destination. Always check the files on
  the sites as well.
- pzs-ng rewrites the `.sfv` on upload, so compare the sites with each other, not with the uploaded
  files.
- A single capture of the console can catch slFtp redrawing its window, which then looks empty: use
  the polling helpers, never one `tmux capture-pane`. Lines that scrolled out of the Admin window are
  gone, and slFtp clears the screen on exit (`e2e.sh` captures it before `!die`).
- The timings include the fixed delay of 0.7 s per console command. They are good for comparing two
  builds, not as absolute numbers.

## Known slFtp issues

On exit slFtp logs `[EXCEPTION] QueueFire: Access violation` and `RemovePazo` exceptions, also on
`dev`. `e2e.sh` lists them as `NOTE` lines, not as failures. If slFtp hasn't exited 60 s after `!die`,
`e2e.sh` kills it and adds a note.

## Disk space

Each run replaces what the previous one left, so nothing piles up:

- `~/slftp-e2e/`, about 200 MB: the downloads, and for `change` and `baseline` the last build, runtime
  directory (with the release the scenario generated) and result
- the image `slftp-e2e-glftpd:<hash of its inputs>`, about 190 MB. After a rebuild (changed pins or
  files in `glftpd/`), `e2e.sh` removes the images of older inputs and the build stages.
- no containers: they are removed at the end of each run

The CI image and `~/slftp-fpc-build` belong to test-both-compilers and stay, as does the base image
`ubuntu:24.04` of the site image (about 80 MB). To remove everything of this skill, delete
`~/slftp-e2e` and the `slftp-e2e-glftpd` images.

## Report

3. Report the tested commits and whether uncommitted changes were included, each `PASS`/`FAIL`, the
   notes (timings, and with a baseline both builds), and the result file path. The run covers only the
   FPC build; say that Delphi is unverified by it.
