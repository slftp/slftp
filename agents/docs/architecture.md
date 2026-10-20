# Architecture map

## Units

- `mainthread.pas` — startup and main orchestration; `console.pas`, `slvision.pas` — text-mode UI
- `irc.pas`, `irc.parse.pas`, `irccommandsunit.pas`, `irccommands/` — IRC bot. Every IRC command has a
  help file in `helpfiles/<command>.txt`; add or update it together with the command.
- `sitesunit.pas` — `TSite`/`TSiteSlot` site and slot management
- `tasksunit.pas` (`TTask` base class) and `task*.pas` — task system. Race tasks (`TPazoMkdirTask`,
  `TPazoDirlistTask`, `TPazoRaceTask`) live in `taskrace.pas`.
- `queueunit.pas` — task queue (`QueueFire`, `RemovePazoMKDIR`, …)
- `pazo.pas` — `TPazo`/`TPazoSite`: per-release race state and logic
- `dirlist.pas`, `dirlist.helpers.pas` — parsing and tracking of site directory listings
- `kb.pas`, `kb.releaseinfo.pas` — announce processing and release type detection
- `precatcher.pas` — IRC announce catching; `rulesunit.pas` + `rules/ruleconditions.*.pas` — rule engine
- `sltcp.pas`, `slssl.pas`, `mslproxys.pas` — own socket layer on top of mORMot2 OpenSSL bindings,
  incl. SOCKS5
- `debugunit.pas` — logging via `Debug(dpError | dpMessage | dpSpam, section, msg)`; each unit declares
  its own `const section = '...'`
- `configunit.pas`, `encinifile.pas` — ini-based config (`config/slftp.ini`); runtime files are created
  next to the binary (see `docs/files`)
- `db*.pas` — database units (tvinfo, imdb, pre, genre, nfo, url); `statsunit.pas` — race statistics;
  `indexer.pas` — site index. See [databases](databases.md).

## Adding an IRC command

1. Implement the handler in the matching `irccommands/irccommands.<group>.pas`.
2. Add a `TIrcCommand` record to `ircCommandsArray` in `irccommandsunit.pas` in its help group, and raise
   the array's upper bound (`array[1..N]`) — it is a fixed-size constant array.
3. Add `helpfiles/<command>.txt`.

## Adding tasks

There are two global `AddTask` procedures — know which one you call:

- `sitesunit.AddTask(t, queueFire)` — hands the task to the site of `t.ssite1` (or the admin site)
- `queueunit.AddTask(t)` — adds the task to the global queue; takes the queue `main_lock`

## Locking

- `slcriticalsection2.pas` provides `TSlCriticalSection2`, a named critical section used throughout
  (e.g. `TDirList.dirlist_lock` in `dirlist.pas`).
- **Never call `AddTask` while holding a dirlist lock.** Collect the tasks and add them after leaving the
  lock, as `pazo.pas` and `taskrace.pas` do ("add task outside the dirlist lock to avoid deadlocks with
  the queue lock").

## Libraries (`libs/`)

- **mORMot2** — preferred for new code: ORM, SQLite3, JSON (`mormot.core.json`), crypto, OpenSSL
  bindings (`mormot.lib.openssl11`, used by the socket layer)
- **ZeosLib** — MySQL/MariaDB, used through `mormot.db.sql.zeos` (`dbhandler.pas`, `dbaddpre.pas`)
- **Indy10** — utility functions only (e.g. `IdGlobal`); it is *not* the socket layer
- **TRegExpr** (commonly used) / **FLRE** — regular expressions
- **lkJSON** — legacy JSON; use mORMot2 JSON for new code
- LibTar, BeRoHighResolutionTimer, pasmp (multithreading), rcmdline
- FastMM5 — memory manager, Delphi only (FPC uses `mormot.core.fpcx64mm`, see [compilers](compilers.md))

All libraries must compile with both FPC and Delphi.
- `libs/lib_updater.py` updates the bundled libraries
