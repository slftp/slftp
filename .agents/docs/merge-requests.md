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

## Without `glab`

A missing `glab` does not block the MR: use the REST API or push options below. Afterwards, tell the
user that installing `glab` (see [One-time setup](#one-time-setup)) improves the workflow.

Take the first way that works; ask the user nothing before step 3:

1. `GITLAB_TOKEN` is set: use the [REST API](#rest-api).
2. Otherwise use [push options](#push-options) if they can do the job (they can't if the branch is
   already pushed and up to date, or the task needs more than creating the MR, e.g. updating it).
3. Only then ask the user once whether you may read a token from git's credential store and use the
   REST API with it.

### REST API

`glab` is a client for this API, so the API can do everything `glab` does: multi-line descriptions,
updating the MR, pipelines, comments. It needs a token with `api` scope, from one of these sources:

- `GITLAB_TOKEN` (`glab` reads it too).
- git's credential store, **only with the user's permission**: the `password=` line that
  `git credential fill` prints for `protocol=https` and `host=gitlab.com`. Some helpers open a login
  prompt if nothing is stored. The stored secret may not work for the API: a password or a token with
  only repository scopes is rejected with 401 or 403, and an OAuth token from Git Credential Manager
  probably is too.
- a new token the user creates in the browser via
  `https://gitlab.com/-/user_settings/personal_access_tokens?name=slftp-agent&scopes=api` and stores in
  `GITLAB_TOKEN`. OAuth (the browser login of `glab auth login`) would need a registered OAuth
  application, so it is no simpler.

Create the MR with whatever HTTP client and JSON encoder you have:

- `POST https://gitlab.com/api/v4/projects/slftp%2Fslftp/merge_requests`
- header `Authorization: Bearer <token>`
- JSON body with `source_branch`, `target_branch` (`dev`), `title` and `description`

Never print the token or put it on a command line, where the process list shows it. Encode the JSON
with a real encoder instead of escaping it by hand, and keep temporary files outside the repo. The branch
must already be pushed. The response's `web_url` is the MR link. On 401/403 the token is unusable; fall
back to push options.

### Push options

GitLab can create the MR while you push:

```
git push -u origin <branch> -o merge_request.create -o merge_request.target=dev \
  -o "merge_request.title=<commit header>" -o "merge_request.description=<text>" \
  -o merge_request.remove_source_branch
```

Push options must not contain line breaks: use `<br>` for line breaks in the description, or edit it in
the GitLab UI afterwards. The push output prints the MR link.

Push options only reach GitLab together with a ref update. If the branch is already pushed and up to
date, git reports "Everything up-to-date", sends nothing and no MR is created. Then ask about the
credential store (see above); if the user declines, give them the link to create the MR in the browser:
`https://gitlab.com/slftp/slftp/-/merge_requests/new?merge_request[source_branch]=<branch>&merge_request[target_branch]=dev`

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
