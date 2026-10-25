# AGENTS.md — slFtp

Entry point for AI coding agents (any tool, any model). This file stays short; details live in
[`.agents/`](.agents/). `CONTRIBUTING.md` is binding — the docs here only summarize it.

## Project

slFtp is a multithreaded FTP/FXP autotrading client for "the scene", written in Pascal. It is controlled
via IRC bot commands and a text-mode console; there is no GUI.

- Two compilers, one code base: FPC on Linux, Delphi on Windows ([compilers](.agents/docs/compilers.md))
- Entry points: `slftp.lpr` (FPC) / `slftp.dpr` (Delphi); tests: `tests/slftpUnitTests.lpr` / `.dpr`
- Integration branch: `dev`. Feature branches start from `dev`; MRs are squash-merged by default

## Essential commands

| Task          | Linux (FPC)           | Windows (Delphi)          |
|---------------|-----------------------|---------------------------|
| Release build | `make slftp_64`       | `make.bat slftp_64`       |
| Debug build   | `make slftp_64_debug` | `make.bat slftp_64_debug` |
| Unit tests    | `make test`           | `make.bat test`           |

Never commit `slftp.inc` after a build (the build patches the git hash into it). Setup and pitfalls, e.g.
the config files the Windows tests need: [build-and-test](.agents/docs/build-and-test.md).

## Rules

- **Support both compilers.** If you can only build and test with one, say which one and that the other
  is unverified: [compilers](.agents/docs/compilers.md).
- **One topic per branch and per commit.** Unrelated fixes get their own branch off `dev`.
- **Reuse before writing:** slFtp helpers first, then mORMot2: [conventions](.agents/docs/conventions.md).
- **Follow the conventions** for naming, PasDoc comments and commit messages: [conventions](.agents/docs/conventions.md).
- **Test behavior changes.**
- **Offer a CHANGELOG entry** for changes operators notice: ask the user before finishing, never add one
  unasked: [conventions](.agents/docs/conventions.md#changelog).
- **Don't guess.** If behavior or intent is unclear, ask.
- **Keep changes minimal and surgical:** [working-style](.agents/docs/working-style.md).

## Index

Read the doc that matches your task before changing code.

| Doc                                                        | Read when                                                        |
|------------------------------------------------------------|------------------------------------------------------------------|
| [.agents/docs/working-style.md](.agents/docs/working-style.md) | starting any task: assumptions, simplicity, surgical changes, verification |
| [.agents/docs/architecture.md](.agents/docs/architecture.md) | finding your way around: units, tasks, locking, libraries        |
| [.agents/docs/external-software.md](.agents/docs/external-software.md) | working on FTP daemon quirks (glFTPd/pzs-ng, DrFTPD, …) or IRC   |
| [.agents/docs/compilers.md](.agents/docs/compilers.md)       | writing any code: FPC vs. Delphi differences, test framework, project files |
| [.agents/docs/build-and-test.md](.agents/docs/build-and-test.md) | building, running tests, dealing with build artifacts        |
| [.agents/docs/wsl-fpc-setup.md](.agents/docs/wsl-fpc-setup.md) | setting up WSL so that a Windows machine can build with FPC   |
| [.agents/docs/conventions.md](.agents/docs/conventions.md)   | writing code, docs, commit messages or CHANGELOG entries         |
| [.agents/docs/databases.md](.agents/docs/databases.md)       | touching SQLite/MySQL code (mORMot2 parameter binding pitfall)   |
| [.agents/docs/merge-requests.md](.agents/docs/merge-requests.md) | opening a merge request: `glab`, or REST API/push options without it |
| [.agents/skills/](.agents/skills/README.md)                  | optional shortcuts for the most common tasks — check the index   |
| [docs/](docs/)                                             | user/operator docs: runtime files, precatcher, chains, indexer, tvinfo, FAQ |

## Keeping agent instructions current

These instructions are maintained by the agents that use them. As part of every task:

- **Update** AGENTS.md or the matching doc in `.agents/docs/` when you change something they describe, or
  when you learn something non-obvious that a future agent needs to do its task correctly (what belongs
  in a doc: [conventions](.agents/docs/conventions.md#docs)). Updates that describe your own change belong
  to your change. Corrections of unrelated wrong information are a separate topic and get their own commit.
- **Fix** wrong or outdated information immediately — in docs and in skills — instead of working around it.
- **Skills:** never add one on your own. Suggest it only if it is really worth it, list its advantages and
  disadvantages, and add it only after the user confirms. See [.agents/skills/](.agents/skills/README.md).

Keep this file an overview and index: rules as one-line headlines, details only in `.agents/docs/` (new
files get a row in the index), never in both.
Everything must stay tool- and model-neutral: no instructions that only one agent or model understands.
