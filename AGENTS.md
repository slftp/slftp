# AGENTS.md — slFtp

Entry point for AI coding agents (any tool, any model). This file stays short; details live in
[`agents/`](agents/). `CONTRIBUTING.md` is binding — the docs here only summarize it.

## Project

slFtp is a multithreaded FTP/FXP autotrading client for "the scene", written in Pascal. It is controlled
via IRC bot commands and a text-mode console; there is no GUI.

- **Two compilers, one code base:** FPC 3.2+ (`-MDelphi` mode) on Linux and Delphi 10.3+ on Windows.
  CI builds and tests both. FPC on Windows is not supported.
- Entry points: `slftp.lpr` (FPC) / `slftp.dpr` (Delphi); tests: `tests/slftpUnitTests.lpr` / `.dpr`
- Integration branch: `dev`. Feature branches start from `dev` and are rebased onto it (no merge commits)

## Essential commands

| Task          | Linux (FPC)             | Windows (Delphi)                                    |
|---------------|-------------------------|-----------------------------------------------------|
| Release build | `make slftp_64`         | `make.bat slftp_64`                                 |
| Debug build   | `make slftp_64_debug`   | `make.bat slftp_64_debug`                           |
| Unit tests    | `make test`             | copy `config\*` into `tests\`, then `make.bat test` |
| Clean         | `make clean`            | `make.bat clean`                                    |

The build patches the git hash into `slftp.inc` and reverts it afterwards — never commit that change.
More pitfalls: [build-and-test](agents/docs/build-and-test.md).

## Rules

- **Support both compilers.** Every change must build and pass the tests with FPC *and* Delphi. If you can
  only run one of them, say which one, and say that the other is unverified: [compilers](agents/docs/compilers.md).
- **One topic per branch and per commit.** Put unrelated fixes on their own branches off `dev`.
- **Reuse before writing.** Use existing slFtp helpers (`mystrings.pas`, `slhelper.pas`, `dbhandler.pas`, …)
  first; otherwise prefer mORMot2 over other libraries or new own implementations.
- **Follow the conventions** for naming, PasDoc comments and commit messages: [conventions](agents/docs/conventions.md).
- **Test behavior changes.** Write or adjust unit tests when a function's behavior changes.
- **Don't guess.** If behavior or intent is unclear, ask instead of assuming.
- **Keep changes minimal and surgical.** No speculative features, no drive-by refactoring; every changed
  line traces back to the request: [working-style](agents/docs/working-style.md).

## Index

Read the doc that matches your task before changing code.

| Doc                                                        | Read when                                                        |
|------------------------------------------------------------|------------------------------------------------------------------|
| [agents/docs/working-style.md](agents/docs/working-style.md) | starting any task: assumptions, simplicity, surgical changes, verification |
| [agents/docs/architecture.md](agents/docs/architecture.md) | finding your way around: units, tasks, locking, libraries        |
| [agents/docs/external-software.md](agents/docs/external-software.md) | working on FTP daemon quirks (glFTPd/pzs-ng, DrFTPD, …) or IRC   |
| [agents/docs/compilers.md](agents/docs/compilers.md)       | writing any code: FPC vs. Delphi differences, test framework, project files |
| [agents/docs/build-and-test.md](agents/docs/build-and-test.md) | building, running tests, dealing with build artifacts        |
| [agents/docs/conventions.md](agents/docs/conventions.md)   | writing code, docs or commit messages                            |
| [agents/docs/databases.md](agents/docs/databases.md)       | touching SQLite/MySQL code (mORMot2 parameter binding pitfall)   |
| [agents/docs/merge-requests.md](agents/docs/merge-requests.md) | opening a merge request: GitLab/`glab` setup and usage       |
| [agents/skills/](agents/skills/README.md)                  | optional shortcuts for the most common tasks — check the index   |
| [docs/](docs/)                                             | user/operator docs: runtime files, precatcher, chains, indexer, tvinfo, FAQ |

## Keeping agent instructions current

These instructions are maintained by the agents that use them. As part of every task:

- **Update** AGENTS.md or the matching doc in `agents/docs/` when you change something they describe, or
  when you learn something non-obvious that is missing. Updates that describe your own change belong to
  your change. Corrections of unrelated wrong information are a separate topic and get their own commit.
- **Fix** wrong or outdated information immediately — in docs and in skills — instead of working around it.
- **Skills:** never add one on your own. Suggest it only if it is really worth it, list its advantages and
  disadvantages, and add it only after the user confirms. See [agents/skills/](agents/skills/README.md).

Keep this file an overview and index; put details into `agents/docs/` (new files get a row in the index).
Everything must stay tool- and model-neutral: no instructions that only one agent or model understands.
