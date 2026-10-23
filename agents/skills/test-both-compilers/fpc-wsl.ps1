<#
.SYNOPSIS
  Builds slFtp and runs the unit tests with FPC in WSL, see SKILL.md next to this file.
.DESCRIPTION
  Runs fpc-wsl.sh in a separate process, waits until it has finished and prints a summary. The run is
  hidden, except for the long first run (clone, image pull) or with -Visible: then it shows a console
  window the user can follow, which closes itself on success and stays open on failure.
  The Windows checkout is only read.
  Exit codes: 0 = build and tests passed, 1 = build or tests failed, 2 = WSL environment missing
  (see agents/docs/wsl-fpc-setup.md), 3 = window closed or timeout before the run finished,
  4 = another run is active.
.PARAMETER Ref
  Build only the committed state of this ref. Default: HEAD plus uncommitted changes and untracked files.
.PARAMETER Distro
  WSL distribution to use. Default: the WSL default distribution.
.PARAMETER Visible
  Show the run in a console window even if it is not the first run.
.PARAMETER TimeoutMinutes
  Give up waiting after this many minutes.
#>
param(
  [ValidatePattern('^[\w./@^~-]+$')][string]$Ref = '',
  [ValidatePattern('^[\w.-]+$')][string]$Distro = '',
  [switch]$Visible,
  [int]$TimeoutMinutes = 60
)

$env:WSL_UTF8 = '1' # wsl.exe prints its own messages in UTF-16 otherwise
$wslArgs = @()
if ($Distro) { $wslArgs = @('-d', $Distro) }

function Invoke-Wsl {
  # --exec runs without a shell, so the arguments arrive unchanged
  & wsl.exe @wslArgs --exec @args
}

Invoke-Wsl true *> $null
if ($LASTEXITCODE -ne 0) {
  Write-Output 'ERROR: no usable WSL distribution, see agents/docs/wsl-fpc-setup.md'
  exit 2
}
$script = (Invoke-Wsl wslpath -u "$PSScriptRoot\fpc-wsl.sh") -join ''
Invoke-Wsl bash $script --initialized
if ($LASTEXITCODE -ne 0) { $Visible = $true }

# the run writes its exit code to a file of its own, so a concurrent run cannot be mistaken for it
$runId = [guid]::NewGuid().ToString('N')
$wslCommand = 'wsl.exe ' + ($wslArgs -join ' ') + " --exec bash '$script' --id $runId $Ref"
if ($Visible) {
  $windowCommand = "`$Host.UI.RawUI.WindowTitle = 'slFtp: FPC build + test (WSL)'; $wslCommand; " +
    "if (`$LASTEXITCODE -ne 0) { Read-Host 'FAILED - press Enter to close this window' | Out-Null }"
  $process = Start-Process powershell.exe -PassThru -ArgumentList '-Command', $windowCommand
  Write-Output "Started in a separate window: $wslCommand"
} else {
  $process = Start-Process powershell.exe -PassThru -WindowStyle Hidden -ArgumentList '-Command', $wslCommand
  Write-Output "Started hidden: $wslCommand"
}

$deadline = (Get-Date).AddMinutes($TimeoutMinutes)
$exitCode = ''
while ((Get-Date) -lt $deadline) {
  Start-Sleep -Seconds 5
  $exitCode = (Invoke-Wsl bash $script --status $runId) -join ''
  if ($exitCode -ne '') { break }
  if ($process.HasExited) {
    # the run may have written its exit code just before the process ended
    $exitCode = (Invoke-Wsl bash $script --status $runId) -join ''
    break
  }
}

if ($exitCode -eq '') {
  Write-Output 'ERROR: window closed or timeout before the run finished'
  exit 3
}
Invoke-Wsl bash $script --clear $runId
if ($exitCode -eq '11') {
  # the log belongs to the other run, so no summary
  Write-Output 'ERROR: another FPC run is active (other worktree or agent, or a run that outlived a timeout)'
  exit 4
}
Invoke-Wsl bash $script --summary
if ($exitCode -eq '0') { exit 0 }
Write-Output "FAILED with exit code $exitCode"
if ($exitCode -eq '10') { exit 2 } # environment not set up, see fpc-wsl.sh
exit 1
