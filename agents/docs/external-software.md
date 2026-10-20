# External software

slFtp is a client that talks to other people's servers. This doc lists the third-party server
software it interoperates with, why it matters for the code, and where the related logic lives.
It complements the [Libraries](architecture.md#libraries-libs) section of the architecture map,
which covers code *vendored into* slFtp rather than software it merely talks to over the network.

## FTP server daemons ("ftpds")

slFtp races releases between FTP sites run by third parties, each typically running one of a small
set of scene FTP daemons. Detecting which one a site runs (`TSiteSw` in [`sitesunit.pas`](../../sitesunit.pas))
matters because these daemons disagree on command support, `STAT` output format, and error wording —
see the `GLFTPD`/`DRFTPD` comparison around `sitesunit.pas:2877` and the `sswDrftpd`/`sswGlftpd`
branches scattered through [`taskrace.pas`](../../taskrace.pas) for the PRET and reconnect quirks.

- **[glFTPd](https://www.glftpd.org/)** — the most common daemon on scene sites. Linux-only, written in
  C. Almost always paired with **[pzs-ng](https://github.com/Mysidia/pzs-ng)** ("zipscript"), the addon
  that validates SFV/completion status, generates the directory status messages (`COMPLETE`, race
  stats, nuke reasons) and the genre tags slFtp parses in [`taskgenredirlist.pas`](../../taskgenredirlist.pas)
  and [`dirlist.pas`](../../dirlist.pas). When a comment or test fixture says "glftpd output", it almost
  always means glFTPd + pzs-ng combined, not glFTPd alone.
- **[DrFTPD](https://github.com/drftpd-ng/drftpd)** — a distributed, Java-based daemon that spreads a
  site's storage across multiple slaves. This is why slFtp needs the `PRET` pre-transfer command (see
  `sitesunit.pas:1872` and the `PRET RETR`/`PRET STOR` handling in `taskrace.pas`) and why the code
  works around DrFTPD-specific issues like a working directory getting reset after reconnect or slave
  unavailability masquerading as "out of space" (`taskrace.pas:1963`).
- **ioFTPD, RaidenFTPD, Pure-FTPd** — Windows-oriented (ioFTPD, RaidenFTPD) or general-purpose
  (Pure-FTPd) daemons that occasionally show up on scene sites. slFtp recognizes them (`sswIoftpd`,
  `sswRaidenftpd`, `sswPureFTPd`) but has far less daemon-specific handling for them than for glFTPd and
  DrFTPD — treat any bug involving them as likely under-tested.

When writing code that behaves differently per daemon, branch on `TSite.sw` (see `sitesunit.pas`)
rather than sniffing banners or error strings ad hoc elsewhere.

## IRC

slFtp is a generic IRC client/bot ([`irc.pas`](../../irc.pas), [`irc.parse.pas`](../../irc.parse.pas)) and
does not depend on a specific IRC daemon — scene IRC networks commonly run
**[UnrealIRCd](https://www.unrealircd.org/)** or **[InspIRCd](https://www.inspircd.org/)**, but slFtp
only needs standard IRC protocol plus some IRCv3 bits (see the IRCv3 reference at `irc.parse.pas:66`).

One de facto scene standard slFtp *does* implement is **FiSH/Mircryption-compatible blowfish channel
encryption**, in both ECB and CBC mode (`ircblowfish.ECB.pas`, `ircblowfish.CBC.pas`). This is a
client-side convention (shared by mIRC scripts, irssi plugins, etc.), not an ircd feature — any ircd
works as long as it relays the encrypted `PRIVMSG`/`NOTICE` payload unchanged.

## Libraries

See [Libraries](architecture.md#libraries-libs) in the architecture map for the code vendored under
`libs/` (mORMot2, ZeosLib, Indy10, TRegExpr, FLRE, …) and what each is used for internally.
