<#
.SYNOPSIS
  Runs an end-to-end scenario of slFtp against glftpd sites in WSL, see SKILL.md next to this file.
.DESCRIPTION
  Runs e2e.sh in a separate process, waits until it has finished and prints a summary. The run is
  hidden, except for the long first run (downloads, image builds) or with -Visible: then it shows a
  console window the user can follow, which closes itself on success and stays open on failure.
  With -Compiler FPC the Windows checkout is only read. With -Compiler Delphi, make.bat slftp_64
  builds slftp.exe in the checkout first, and slFtp runs on Windows against the sites in WSL.
  Exit codes: 0 = all checks passed, 1 = a check or the build failed, 2 = WSL environment missing
  (see .agents/docs/wsl-fpc-setup.md), 3 = window closed or timeout before the run finished,
  4 = another run is active.
.PARAMETER Scenario
  Name of the scenario, a file in scenarios\ without .sh.
.PARAMETER Compiler
  FPC (the Linux build, in WSL) or Delphi (the Windows build of the checkout).
.PARAMETER Ref
  Test only the committed state of this ref. Default: HEAD plus uncommitted changes and untracked files.
  Not with Delphi.
.PARAMETER Baseline
  Also build this ref and run the scenario with it first, for a before/after comparison. Not with
  Delphi.
.PARAMETER Distro
  WSL distribution to use. Default: the WSL default distribution.
.PARAMETER Visible
  Show the run in a console window even if it is not the first run.
.PARAMETER TimeoutMinutes
  Give up waiting after this many minutes.
#>
param(
  [ValidatePattern('^[\w-]+$')][string]$Scenario = 'race',
  [ValidateSet('FPC', 'Delphi')][string]$Compiler = 'FPC',
  [ValidatePattern('^[\w./@^~-]+$')][string]$Ref = '',
  [ValidatePattern('^[\w./@^~-]+$')][string]$Baseline = '',
  [ValidatePattern('^[\w.-]+$')][string]$Distro = '',
  [switch]$Visible,
  [int]$TimeoutMinutes = 60
)

. "$PSScriptRoot\..\..\scripts\wsl-run.ps1"
Initialize-Wsl $Distro

$runArgs = @()
if ($Baseline) { $runArgs += '--baseline', $Baseline }
$uses = @()
if ($Compiler -eq 'FPC') { $uses = @("$PSScriptRoot\..\..\scripts\fpc-wsl.sh") }
if ($Compiler -eq 'Delphi') {
  if ($Ref -or $Baseline) {
    Write-Output 'ERROR: Delphi tests the working tree of the checkout, no -Ref or -Baseline'
    exit 1
  }
  $repo = (Resolve-Path "$PSScriptRoot\..\..\..").Path
  $build = "$env:LOCALAPPDATA\slftp-e2e\build-change"
  Write-Output 'Building slftp.exe with make.bat slftp_64 in the checkout'
  New-Item -ItemType Directory -Force "$env:LOCALAPPDATA\slftp-e2e" | Out-Null
  Push-Location $repo
  cmd /c ".\make.bat slftp_64 < NUL" > "$env:LOCALAPPDATA\slftp-e2e\make.log" 2>&1
  $makeExitCode = $LASTEXITCODE
  Pop-Location
  if ($makeExitCode -ne 0) {
    Select-String -Path "$env:LOCALAPPDATA\slftp-e2e\make.log" -Pattern 'Error|Fatal' | ForEach-Object { $_.Line }
    Write-Output "FAILED: make.bat slftp_64, log: $env:LOCALAPPDATA\slftp-e2e\make.log"
    exit 1
  }
  powershell -NoProfile -ExecutionPolicy Bypass -File "$repo\.agents\scripts\slftp-run.ps1" prepare -Dir $build
  if ($LASTEXITCODE -ne 0) { exit 1 }
  $commit = (git -C $repo rev-parse --short HEAD)
  if (git -C $repo status --porcelain) { $commit += ' + uncommitted changes' }
  [IO.File]::WriteAllText("$build.commit", "$commit`n")
  $runArgs += '--windows-build', (ConvertTo-WslPath $build)
}
$runArgs += $Scenario
if ($Ref) { $runArgs += $Ref }
Invoke-WslRun -Script "$PSScriptRoot\e2e.sh" -RunArgs $runArgs -Uses $uses `
  -Title "slFtp: glftpd end-to-end test ($Compiler, WSL)" `
  -BusyMessage 'ERROR: another run is active (e2e or FPC build: other worktree or agent, or a run that outlived a timeout)' `
  -Visible:$Visible -TimeoutMinutes $TimeoutMinutes
