---
name: unrealircd
description: Start a local UnrealIRCd test server in Docker, on Linux or on Windows in WSL, with test clients that speak FiSH ECB, and connect slFtp to it, with the FPC or the Delphi build. Use to try a change to slFtp's IRC code against a real server, e.g. message encryption, the echo queue or reconnects.
metadata:
  fpc: verified
  delphi: verified
---

# UnrealIRCd test server

Runs UnrealIRCd, pinned in `.agents/skills/unrealircd/ircd.sh`, in the container `slftp-unrealircd` on the
host network: `127.0.0.1:6667` plain and `127.0.0.1:6697` TLS, with a self-signed certificate. The FPC
build of slFtp reaches it directly, the Delphi build on Windows through the ports WSL forwards to
`127.0.0.1`. The config in `server/unrealircd.conf` has no flood limits, so that clients can send bursts,
and listens only on `127.0.0.1`.

## Requirements

Docker, set up as in the requirements of [glftpd-e2e](../glftpd-e2e/SKILL.md#requirements), and internet
access for the first start. To run slFtp: [run-slftp](../run-slftp/SKILL.md#requirements).

## Run

1. Use the commands below, on Linux `bash .agents/skills/unrealircd/ircd.sh <command>`, on Windows
   `powershell -NoProfile -ExecutionPolicy Bypass -File .agents\skills\unrealircd\ircd.ps1 <command>`:

   | Command | Does |
   |---------|------|
   | `start` | a fresh server, waits until it answers, and fails if another program uses its ports; builds the image first if it is missing, which takes about 3 minutes and on Windows shows a console window that closes itself on success |
   | `stop` | removes the container and with it the test clients |
   | `status` | prints whether the server runs, exit code `1` if not |
   | `client <nick> <#channel> [<blowkey>]` | starts a test client in the container and waits until it is in the channel. It fails and ends again if it doesn't get there, and fails if a client with this nick runs. With a key of 8 to 56 bytes, it encrypts and decrypts with its own FiSH ECB code, independently of slFtp's. |
   | `say <nick> '<text>'` | the client says the text in its channel |
   | `raw <nick> '<line>'` | the client sends the IRC line, e.g. unencrypted text |
   | `feed <nick> <file>` | the client says each line of the file, all in one burst, with its bytes unchanged, also if they aren't UTF-8 |
   | `wait <nick> '<text>' [<count>] [<seconds>]` | waits until `<count>` lines (default 1) of the client's log contain the text, at most `<seconds>` (default 30) |
   | `log <nick>` | prints what the client received: `<sender> <text>`, with a key `<sender> [undecrypted] <text>` for text that isn't valid FiSH, `JOIN <nick>`, `PART <nick>`, `QUIT <nick>`; and `JOINED` once it is in the channel, `DISCONNECTED` when the server closed the connection |

   Exit codes: `0` done; `1` failed, or the text did not show up in time; `2` (Windows) or `10` (Linux)
   WSL or Docker is missing; `3` (Windows) the window was closed or the timeout hit during the build;
   `4` (Windows) or `11` (Linux) another build is active.

2. `start` the server and a `client`, e.g. `client tester '#slftp' <blowkey>`, first: a client's log only
   shows what happens after it has joined.
3. Start slFtp with [run-slftp](../run-slftp/SKILL.md) and type into its console, here for TLS (`1`, plain
   is port 6667 and `0`):
   ```
   !ircnetadd TEST 127.0.0.1:6697 1
   !ircchanadd TEST #slftp
   !ircchanblow TEST #slftp <blowkey>
   !ircchanrole TEST #slftp ADMIN
   ```
4. `wait tester 'JOIN slftp' 1 60`, then for example `say tester '!uptime'` and `log tester`. Compare
   whole lines of the log: slFtp also answers with `Ok.`.
5. `stop` the server when done. On Windows, `start` keeps WSL running until then.

## Pitfalls

- On Windows, Windows PowerShell drops double quotes from arguments, and text can split or get lost: `say`,
  `raw` and `wait` fail on the wrong number of arguments, but not when only the quotes are gone. Use `feed`
  for text with double quotes or with a leading `-`.
- `say`, `raw` and `feed` fail if the client is not running, after 5 s if it has ended, e.g. on a restart.
- On Windows, type into slFtp's console through `slftp-run.ps1`, as in run-slftp. On Linux in WSL, do
  all steps in one `wsl.exe` call: the distribution stops with the last one, see
  [wsl-fpc-setup](../../docs/wsl-fpc-setup.md#pitfalls). `ircd.ps1 start` covers only its own server.
- slFtp only executes encrypted commands. The channel role `ADMIN` allows all of them; others, e.g.
  `GROUP` or `STATS`, allow only some.
- An IRC line has at most 512 bytes, so a test client's encrypted command can hold about 300 characters.
  For longer text, type `!ircsay` into slFtp's console: it splits messages at 250 characters, and each
  part except the last keeps its trailing space.
- Changing a file in `server/` rebuilds the image from scratch. To try a config, mount it over
  `/home/ircd/unrealircd/conf/unrealircd.conf` of the existing image, and wait until the ports answer: the
  server reports most config errors only a few seconds after its start.
- A restart of the container ends the test clients, but not slFtp, which reconnects on its own.

## Known slFtp issues

`!ircjump` logs `[EXCEPTION] TMyIrcThread.Execute`, with both builds and also on `dev`: `IrcJump` removes
the thread from `myIrcThreads`, which owns it, and `TMyIrcThread.Destroy` frees its fields while it still
runs. slFtp reconnects anyway.

## Disk space

The image `slftp-unrealircd:<hash of its inputs>` takes about 240 MB, `~/slftp-unrealircd/` about 15 MB
(download and log). No containers stay after `stop`. After a rebuild, `ircd.sh` removes the images of older
inputs and the build stages, but the base image `ubuntu:24.04` stays. To remove everything of this skill,
delete `~/slftp-unrealircd` and the `slftp-unrealircd` images.

## Report

6. Report the UnrealIRCd version, whether slFtp connected with TLS or plain, what you sent and what the
   client's log shows. Say which compiler's build ran; the other one is unverified unless you ran it too.
