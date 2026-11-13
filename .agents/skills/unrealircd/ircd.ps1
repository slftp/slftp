<#
.SYNOPSIS
  Runs a local UnrealIRCd test server in WSL and its test clients, see SKILL.md next to this file.
.DESCRIPTION
  Passes the command to ircd.sh in WSL, see there for the commands. In addition:
    build  runs through wsl-run.ps1: hidden, except for the long first build or with -Visible
    start  builds first if the image is missing, and keeps WSL running as long as the server runs:
           WSL would otherwise shut down soon after the last wsl.exe has ended, and stop the server
  Exit codes: 0 = done, 1 = failed or the text did not show up in time, 2 = WSL environment missing
  (see .agents/docs/wsl-fpc-setup.md), 3 = window closed or timeout before the build finished,
  4 = another build is active.
.PARAMETER Command
  One of the commands of ircd.sh.
.PARAMETER Arguments
  The arguments of the command; feed takes a Windows path.
.PARAMETER Distro
  WSL distribution to use. Default: the WSL default distribution.
.PARAMETER Visible
  build: show the build in a console window even if it is not the first one.
.PARAMETER TimeoutMinutes
  build: give up waiting after this many minutes.
#>
param(
  [Parameter(Mandatory, Position = 0)]
  [ValidateSet('build', 'start', 'stop', 'status', 'client', 'say', 'raw', 'feed', 'wait', 'log')]
  [string]$Command,
  [Parameter(Position = 1, ValueFromRemainingArguments)][string[]]$Arguments = @(),
  [ValidatePattern('^[\w.-]+$')][string]$Distro = '',
  [switch]$Visible,
  [int]$TimeoutMinutes = 30
)

if ($Visible -and $Command -ne 'build') {
  # text like -Visible binds to this switch and would otherwise be lost silently
  Write-Output 'ERROR: -Visible is only for build; text that starts with - needs feed'
  exit 1
}

. "$PSScriptRoot\..\..\scripts\wsl-run.ps1"
[Console]::OutputEncoding = New-Object Text.UTF8Encoding $false
Initialize-Wsl $Distro
$script = ConvertTo-WslPath "$PSScriptRoot\ircd.sh"

if ($Command -eq 'build') {
  Invoke-WslRun -Script "$PSScriptRoot\ircd.sh" -RunArgs @('build') -Title 'slFtp: build UnrealIRCd image (WSL)' `
    -BusyMessage 'ERROR: another build of the UnrealIRCd image is active' -Visible:$Visible -TimeoutMinutes $TimeoutMinutes
}
if ($Command -eq 'start') {
  # ircd.sh only sees the ports in WSL. A Windows program on them would get the connections of the Delphi
  # build instead of the server; wslrelay is the forwarding of WSL, e.g. of the server that start replaces.
  $listeners = Get-NetTCPConnection -State Listen -LocalPort 6667, 6697 -ErrorAction SilentlyContinue |
    Where-Object { $_.LocalAddress -in '0.0.0.0', '127.0.0.1' }
  foreach ($listener in $listeners) {
    $owner = (Get-Process -Id $listener.OwningProcess -ErrorAction SilentlyContinue).ProcessName
    if ($owner -ne 'wslrelay') {
      Write-Output "ERROR: port $($listener.LocalPort) is in use by the Windows program $owner"
      exit 1
    }
  }
  Invoke-Wsl bash $script --initialized
  if ($LASTEXITCODE -ne 0) {
    $buildArgs = @('-TimeoutMinutes', $TimeoutMinutes)
    if ($Distro) { $buildArgs += '-Distro', $Distro }
    powershell -NoProfile -ExecutionPolicy Bypass -File $PSCommandPath build @buildArgs
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  }
}
if ($Command -eq 'feed') { $Arguments[1] = ConvertTo-WslPath $Arguments[1] }

Invoke-Wsl bash $script $Command @Arguments
$exitCode = $LASTEXITCODE
if ($Command -eq 'start' -and $exitCode -eq 0) {
  # ends together with the container, so nothing has to stop it
  Start-Process wsl.exe -WindowStyle Hidden -ArgumentList (@($wslArgs) + '--exec', 'docker', 'wait', 'slftp-unrealircd')
}
exit $exitCode
