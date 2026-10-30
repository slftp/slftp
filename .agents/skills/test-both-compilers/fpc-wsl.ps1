<#
.SYNOPSIS
  Builds slFtp and runs the unit tests with FPC in WSL, see SKILL.md next to this file.
.DESCRIPTION
  Runs .agents/scripts/fpc-wsl.sh in a separate process, waits until it has finished and prints a summary. The run is
  hidden, except for the long first run (clone, image pull) or with -Visible: then it shows a console
  window the user can follow, which closes itself on success and stays open on failure.
  The Windows checkout is only read.
  Exit codes: 0 = build and tests passed, 1 = build or tests failed, 2 = WSL environment missing
  (see .agents/docs/wsl-fpc-setup.md), 3 = window closed or timeout before the run finished,
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

. "$PSScriptRoot\..\..\scripts\wsl-run.ps1"
Initialize-Wsl $Distro
$runArgs = @()
if ($Ref) { $runArgs += $Ref }
Invoke-WslRun -Script "$PSScriptRoot\..\..\scripts\fpc-wsl.sh" -RunArgs $runArgs `
  -Title 'slFtp: FPC build + test (WSL)' `
  -BusyMessage 'ERROR: another FPC run is active (other worktree or agent, or a run that outlived a timeout)' `
  -Visible:$Visible -TimeoutMinutes $TimeoutMinutes
