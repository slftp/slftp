# Skills

A skill is a step-by-step shortcut for one of the most common tasks. It is **not** the only or the
mandatory way to do that task: if a task doesn't fit a skill, or a skill's steps don't fit the situation,
work from `AGENTS.md` and `.agents/docs/` instead. Before starting a task, scan the index below and read a
matching `SKILL.md` completely.

## Index

| Skill | Use when |
|-------|----------|
| [test-both-compilers](test-both-compilers/SKILL.md) | on Windows: build and test a change with FPC (via WSL) and Delphi |
| [glftpd-e2e](glftpd-e2e/SKILL.md) | on Linux, or on Windows in WSL: run slFtp end to end against glftpd sites with pzs-ng, e.g. an FXP race |
| [run-slftp](run-slftp/SKILL.md) | start slFtp, type commands into its console, read the screen and stop it, to try a change in the real program |

## Keeping skills correct

If a skill is wrong or outdated (renamed files, changed commands, missing steps), fix it as part of your
task (see "Keeping agent instructions current" in `AGENTS.md`). Never follow steps you know are wrong.

## Verification status

An agent can usually run only one compiler, so each skill records per compiler whether its current steps
were actually run there (`metadata` in the frontmatter, see "Format"): `verified` or `unverified`.

- **Writing or changing a skill:** whenever you change its steps, files or commands, set the side you
  ran the changed steps on to `verified` and the other side to `unverified` — even if you think your
  change doesn't affect it. Pure wording or typo fixes leave the status unchanged.
- **Using a skill:** if a side is `unverified`, tell the user before you start and check every step
  against the files it names. If the steps worked as written on that side, set it to `verified`; if
  not, fix them first. Commit that skill update on its own (one topic per commit).
- Never set a side to `verified` that you did not run.

## Proposing a new skill

Never add a skill on your own. Suggest one to the user only if it is really worth it:

- the task recurs often and has several non-obvious steps that are easy to get wrong
- the steps are stable, and won't go stale with the next refactoring

When you suggest one, present:

- **Advantages:** e.g. consistent results, fewer forgotten steps (help files, both project files, tests)
- **Disadvantages:** e.g. one more file to maintain, risk of going stale and spreading wrong steps,
  duplicating `.agents/docs/`, agents following it rigidly where it doesn't fit

Add the skill only after the user confirms.

## Format

Each skill is a folder with a `SKILL.md`, following the open Agent Skills format so that any agent can
use it:

```
.agents/skills/<skill-name>/
  SKILL.md        # required
  ...             # optional supporting files (scripts, templates, references)
```

A script that two or more skills use goes into `.agents/scripts/` instead, documented in its own
header; a script only one skill uses stays in that skill's folder.

`.agents/skills/` is the tool-neutral project location that agents with native skill support (e.g.
Codex, Cursor, GitHub Copilot) scan on their own; agents without it use the index above. Keep skills
there.

`SKILL.md` starts with YAML frontmatter, followed by plain Markdown instructions:

```markdown
---
name: <skill-name>           # lowercase, hyphens, same as the folder name
description: <what it does and when to use it, one or two sentences>
metadata:
  fpc: unverified            # verified | unverified, see "Verification status"
  delphi: unverified
---

# <Title>

1. ...
```

Rules:

- Keep it short. Link to `.agents/docs/` for background instead of repeating it.
- Keep it tool-neutral: describe *what* to do and which files and commands to use, not features of one
  specific agent, and don't prescribe helper programs ([conventions](../docs/conventions.md#docs)).
- Cover both compilers: where steps, files or commands differ between FPC (Linux) and Delphi (Windows),
  list both (e.g. `.lpr` and `.dpr`, `make` and `make.bat`). Never write a skill that only works for one.
  You can usually run only one side yourself, so take the other side's steps from the files that define
  them (`Makefile`, `make.bat`, both project files, CI config, `.agents/docs/`) — don't guess them.
- Run the steps on the side you can, and set the verification status (see "Verification status"). When
  you present the skill to the user, say which side is still unverified.
- End the skill with verification for both sides; an agent that can run only one compiler says so.
- Reference files with paths relative to the repository root.
- Add a row to the index above in the same commit.
