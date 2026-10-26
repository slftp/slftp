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

$env:WSL_UTF8 = '1' # wsl.exe prints its own messages in UTF-16 otherwise
$wslArgs = @()
if ($Distro) { $wslArgs = @('-d', $Distro) }

function Invoke-Wsl {
  # --exec runs without a shell, so the arguments arrive unchanged
  & wsl.exe @wslArgs --exec @args
}

Invoke-Wsl true *> $null
if ($LASTEXITCODE -ne 0) {
  Write-Output 'ERROR: no usable WSL distribution, see .agents/docs/wsl-fpc-setup.md'
  exit 2
}
$script = (Invoke-Wsl wslpath -u "$PSScriptRoot\e2e.sh") -join ''
$fpcScript = (Invoke-Wsl wslpath -u "$PSScriptRoot\..\test-both-compilers\fpc-wsl.sh") -join ''
Invoke-Wsl bash $script --initialized
$initialized = $LASTEXITCODE -eq 0
if ($Compiler -eq 'FPC') {
  Invoke-Wsl bash $fpcScript --initialized
  $initialized = $initialized -and $LASTEXITCODE -eq 0
}
if (-not $initialized) { $Visible = $true }

# the run writes its exit code to a file of its own, so a concurrent run cannot be mistaken for it
$runId = [guid]::NewGuid().ToString('N')
$runArgs = "--id $runId"
if ($Baseline) { $runArgs += " --baseline $Baseline" }
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
  $runArgs += " --windows-build '" + ((Invoke-Wsl wslpath -u $build) -join '') + "'"
}
$wslCommand = 'wsl.exe ' + ($wslArgs -join ' ') + " --exec bash '$script' $runArgs $Scenario $Ref"
if ($Visible) {
  $windowCommand = "`$Host.UI.RawUI.WindowTitle = 'slFtp: glftpd end-to-end test ($Compiler, WSL)'; $wslCommand; " +
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
  Write-Output 'ERROR: another run is active (e2e or FPC build: other worktree or agent, or a run that outlived a timeout)'
  exit 4
}
Invoke-Wsl bash $script --summary
if ($exitCode -eq '0') { exit 0 }
Write-Output "FAILED with exit code $exitCode"
if ($exitCode -eq '10') { exit 2 } # environment not set up, see e2e.sh
exit 1
