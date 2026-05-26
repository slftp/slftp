# AGENTS.md

## Mode

Act as critical mentor, not helper. Challenge assumptions. Prioritize weaknesses. Be direct — no "that's a good idea, but...". Elevate work to 10/10, not "good enough".

## Project

slFtp — multithreaded FTP client in Free Pascal (Delphi-compatible mode). IRC announce-based autotrading, rule engine, ncurses UI, REST API + Web UI.

## Build & Test

```shell
make                        # production build
make debug                  # debug with symbols
make test                   # build + run unit tests — NEVER run bare
# Correct non-interactive invocation (test prompts "Done. press <Enter> key to quit."):
echo "" | timeout 300 make test
make clean
```
Requires FPC 3.2.0+, OpenSSL 1.1.1, SQLite 3.35. Every shell command that could hang MUST use `timeout`.

### Unit tests prerequisite

`make test` builds `tests/slftpUnitTests`, which looks for config files in its own directory (`tests/`). Copy them before running tests:

```shell
cp config/slftp.ini config/slftp.knowngroups config/slftp.precatcher \
   config/slftp.spamconf config/slftp.languagebase config/slftp.skip \
   config/slftp.imdbcountries config/slftp.skipgroups tests/
echo "" | timeout 300 make test
```

Without this step the test runner exits immediately with "slftp inifile (missing or 0 byte)".

## Coding Conventions

| Item | Prefix | Example |
|------|--------|---------|
| Global vars (interface) | `Gl` + uppercase | `GlMainConfig` |
| Unit vars (implementation) | `gl` + uppercase | `glSQLite3Lock` |
| Class fields | `F` + uppercase | `FSitesCount` |
| Local vars | `f` + uppercase | `fChanSettingsObj` |
| Parameters | `a` + uppercase, prefer `const` | `aNetname` |
| Impl-only functions | `_` | `_findMP3GenreOnAnnounce` |

Comments: PasDoc style (`{ @abstract(...) }`). No comments unless requested.

## Workflow

- **Context7:** Always use Context7 MCP to fetch current documentation when working with any library, framework, or API — do not rely on training data.
- **Subagents / Agent Swarm:** Prefer delegating work to focused `Agent` subagents (`explore`, `coder`, `plan`) rather than doing everything inline. Use multiple subagents concurrently for independent investigations. This improves parallelism and keeps context clean.
- **Sequential Thinking:** Use the `sequentialthinking` MCP tool for complex analysis, debugging, design decisions, and multi-step problem solving. Do not rely solely on inline reasoning for non-trivial logic.

## Critical Rules

- **Memory:** FPC has no GC. Every allocation must be freed on ALL exit paths (including exceptions). Use `try...finally`, prefer `FreeAndNil`. A leak in `archive/api_and_webui_broken` killed the entire branch — don't repeat this.
- **Thread safety:** Use `TSlCriticalSection2` from `slcriticalsection2.pas` exclusively. Never standard Pascal sync objects.
- **Strings:** Watch boundaries between Indy10 (byte arrays), mORMot2 (UTF8), and FPC strings. Minimize conversions.
- **WIP logging:** Use `dpError` during development. Clean up before committing.
- **Stick to existing code:** No new patterns, libraries, or paradigms. Mimic surrounding code.
- **Ambiguous requirements:** Stop and ask. No blind coding.
- **Uncertainty:** If you are unsure about ANYTHING — a default value, a behavior, a design decision — **ask the user**. Never guess or decide on your own. Wrong assumptions are worse than a clarifying question.
- **English only:** All code, comments, messages, commits.

## Web UI (`web-ui/`)

Stack: React 19, Mantine 8, TanStack Query 5, React Router 7, Axios, Vite 7, TypeScript 5.9. Use Context7 for library docs.

## Git

### Git identity
```bash
git config user.name "Moe Spinat"
git config user.email "moe201484@protonmail.com"
```

### Commit
```
<type>(<scope>): <subject>
```
Types: `feat`, `fix`, `style`, `refactor`, `perf`, `chore`, `remove`, `update`.

- Never mention AI in commits — no co-author tags, no "Generated with" signatures
- Commit immediately after each logical change
- Never push automatically

### Remotes
Always use SSH. Remote must be configured as `git@gitlab.com:slftp/slftp.git`.

| Remote | URL | Policy |
|--------|-----|--------|
| `origin` | `git@gitlab.com:slftp/slftp.git` | Upstream — only when 100% tested |

### Branches
- **`dev`** — protected, identical to upstream, never modified
- **`api_and_webui`** — active development on `origin`

### Push Policy
Nothing gets pushed to `gitlab-com` without: `make test` passing, memory verified, runtime tested.


### `api_and_webui` branch notes
- Clean rebuild from `dev`. Do NOT cherry-pick/merge from `archive/api_and_webui_broken`.
- `loadmonitorunit.pas` was intentionally not ported.
- Never commit `.md` files (exclude via `.git/info/exclude`).

## Releases

Version in `slftp.inc` (`SL_VERSION`, `HELP_VERSION`). Annotated tags from `dev` only. `SL_REV` patched at build time.
