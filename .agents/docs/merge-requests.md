# Opening merge requests

This project is hosted on **GitLab** (`gitlab.com/slftp/slftp`), not GitHub — `gh` does not work here.
Use GitLab's CLI, **[`glab`](https://gitlab.com/gitlab-org/cli)**, instead.

## One-time setup

- Install: `winget install --id GLab.GLab --source winget` (Windows) or your platform's package manager.
- Authenticate: `glab auth login` — stores a token in the OS keyring, so this is only needed once per
  machine/user.

## Creating an MR

```
git push -u origin <branch>
glab mr create --source-branch <branch> --target-branch dev \
  --title "..." --description "..."
```

`glab mr create` without `--title`/`--description` opens an interactive prompt; pass both flags to run
non-interactively. Target `dev`, not the repo's default branch display — see `AGENTS.md` ("Integration
branch: `dev`").

## Windows PATH gotcha after installing `glab`

Installing `glab` (or anything else) updates the **registry**, but any process already running —
a CMD or PowerShell window, a terminal multiplexer, an IDE with an integrated terminal — keeps the
`PATH` snapshot it started with. Opening a *new window or tab* inside that same already-running
application does **not** refresh it, because the new window still inherits its parent process's
environment, not the registry.

Symptom: `glab` (or any newly installed CLI) is "not recognized" even in a brand-new-looking window.

Fixes, in order of convenience:
- Fully quit and relaunch the host application (not just open a new window/tab in it).
- Or, without restarting anything, re-read `PATH` from the registry in the current shell:
  - PowerShell: `$env:Path = [System.Environment]::GetEnvironmentVariable('Path','Machine') + ';' + [System.Environment]::GetEnvironmentVariable('Path','User')`
  - cmd.exe: no in-place equivalent — restart the window.
- Or call the binary by its full install path as a one-off (e.g.
  `C:\Users\<user>\AppData\Local\Programs\glab\glab.exe`).
