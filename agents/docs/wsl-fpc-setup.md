# WSL setup for FPC builds on Windows

One-time setup that lets a Windows machine build and test with FPC in WSL, in the same Docker image the
GitLab CI uses (`registry.gitlab.com/slftp/ci-images/ubuntu-lts`). It is used by the skill
[test-both-compilers](../skills/test-both-compilers/SKILL.md); come here when its script reports a
missing environment.

Run these long steps in a visible console window so the user can follow the progress: a hidden install
or download shows no output for minutes and looks hung. Let the window close itself on success and stay
open on failure (see step 3). Steps 1 and 2 need the user anyway (admin rights, reboot, Linux password).

## 1. WSL platform (once per machine, admin)

```powershell
wsl --install --no-distribution   # in an elevated PowerShell, then reboot if asked
```

On a machine without WSL, `wsl --install -d Ubuntu` often only installs the platform and never
registers the distribution. Check with `wsl -l -v` and continue with step 2 if it lists nothing.

## 2. Ubuntu (once per Windows user, as that user)

```powershell
wsl --install -d Ubuntu           # normal, non-elevated PowerShell of the user who runs the builds
```

- Distributions are registered per Windows user. Installed from an elevated prompt of another account
  (e.g. a separate `Admin`), it is invisible to the user who builds. Its virtual disk is
  `%LOCALAPPDATA%\wsl\{GUID}\ext4.vhdx` of the installing user; `wsl --unregister Ubuntu` (as that user)
  removes it.
- The first start asks for a Linux user name and password: interactive, the user types them.
- `wsl.exe` prints its own messages in UTF-16; set `WSL_UTF8=1` to read them from scripts.

## 3. Docker (once per distribution)

Needs systemd, which current Ubuntu images enable (`[boot] systemd=true` in `/etc/wsl.conf`). Runs as
`root`, so no password is needed. Replace `<user>` with the Linux user from step 2:

```powershell
$install = 'apt-get update && apt-get install -y docker.io && usermod -aG docker <user> && systemctl enable --now docker'
Start-Process powershell -ArgumentList '-Command', "wsl -d Ubuntu -u root --exec bash -c '$install'; if (`$LASTEXITCODE -ne 0) { Read-Host 'FAILED - press Enter to close' }"
```

Use single quotes inside the `-Command` text: `Start-Process` passes the arguments on unquoted, so
double quotes inside them get lost.

The CI image is pulled by the skill's script on its first run (about 200 MB download, 900 MB on disk).

## Check

```powershell
wsl -d Ubuntu -- docker run --rm registry.gitlab.com/slftp/ci-images/ubuntu-lts fpc -iV
```

prints the FPC version of the CI. The script also needs Windows `git.exe` inside WSL, which works as long
as the Windows `PATH` is appended in WSL (the default).
