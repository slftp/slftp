<#
.SYNOPSIS
  Runs a long script in WSL for the Windows entries of the skills: dot-sourced by
  .agents/skills/test-both-compilers/fpc-wsl.ps1 and .agents/skills/glftpd-e2e/e2e.ps1.
.DESCRIPTION
  Initialize-Wsl  selects the WSL distribution and checks that it works, exit code 2 if not
  Invoke-Wsl      runs a command in it
  ConvertTo-WslPath  converts a Windows path to a WSL path
  Invoke-WslRun   runs a script that uses run-protocol.sh next to this file in a separate process,
                  waits until it has finished, prints its summary and exits. The run is hidden,
                  except with -Visible or for the long first run (a script reports --initialized
                  false): then it shows a console window the user can follow, which closes itself
                  on success and stays open on failure.
  Exit codes: 0 = the run passed, 1 = it failed, 2 = WSL environment missing (see
  .agents/docs/wsl-fpc-setup.md), 3 = window closed or timeout before the run finished,
  4 = another run is active.
#>

$env:WSL_UTF8 = '1' # wsl.exe prints its own messages in UTF-16 otherwise
$wslArgs = @()

function Invoke-Wsl {
  # --exec runs without a shell, so the arguments arrive unchanged
  & wsl.exe @wslArgs --exec @args
}

# Initialize-Wsl <distro>: empty for the WSL default distribution
function Initialize-Wsl([string]$Distro) {
  if ($Distro) { $script:wslArgs = @('-d', $Distro) }
  Invoke-Wsl true *> $null
  if ($LASTEXITCODE -ne 0) {
    Write-Output 'ERROR: no usable WSL distribution, see .agents/docs/wsl-fpc-setup.md'
    exit 2
  }
}

function ConvertTo-WslPath([string]$Path) {
  (Invoke-Wsl wslpath -u $Path) -join ''
}

# Invoke-WslRun -Script <.sh> -RunArgs <arguments after --id <id>> -Uses <.sh scripts the run calls,
# whose --initialized also counts> -Title <window title> -BusyMessage <exit code 4 message>
function Invoke-WslRun {
  param(
    [string]$Script,
    [string[]]$RunArgs = @(),
    [string[]]$Uses = @(),
    [string]$Title,
    [string]$BusyMessage,
    [switch]$Visible,
    [int]$TimeoutMinutes
  )
  $wslScript = ConvertTo-WslPath $Script
  foreach ($initScript in @($Script) + $Uses) {
    Invoke-Wsl bash (ConvertTo-WslPath $initScript) --initialized
    if ($LASTEXITCODE -ne 0) { $Visible = $true }
  }

  # the run writes its exit code to a file of its own, so a concurrent run cannot be mistaken for it
  $runId = [guid]::NewGuid().ToString('N')
  $quotedArgs = $RunArgs | ForEach-Object { "'" + ($_ -replace "'", "''") + "'" }
  $wslCommand = 'wsl.exe ' + ($wslArgs -join ' ') + " --exec bash '$wslScript' --id $runId " +
    ($quotedArgs -join ' ')
  if ($Visible) {
    $windowCommand = "`$Host.UI.RawUI.WindowTitle = '$Title'; $wslCommand; " +
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
    $exitCode = (Invoke-Wsl bash $wslScript --status $runId) -join ''
    if ($exitCode -ne '') { break }
    if ($process.HasExited) {
      # the run may have written its exit code just before the process ended
      $exitCode = (Invoke-Wsl bash $wslScript --status $runId) -join ''
      break
    }
  }

  if ($exitCode -eq '') {
    Write-Output 'ERROR: window closed or timeout before the run finished'
    exit 3
  }
  Invoke-Wsl bash $wslScript --clear $runId
  if ($exitCode -eq '11') {
    # the log belongs to the other run, so no summary
    Write-Output $BusyMessage
    exit 4
  }
  Invoke-Wsl bash $wslScript --summary
  if ($exitCode -eq '0') { exit 0 }
  Write-Output "FAILED with exit code $exitCode"
  if ($exitCode -eq '10') { exit 2 } # environment not set up, see the script
  exit 1
}
