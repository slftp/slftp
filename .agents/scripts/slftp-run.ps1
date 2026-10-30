<#
.SYNOPSIS
  Runs the Windows build of slFtp in a runtime directory and drives its console, see
  .agents/skills/run-slftp/SKILL.md.
.DESCRIPTION
  Commands:
    prepare  fresh runtime directory -Dir: slftp.exe (-Exe), config\*, the OpenSSL DLLs,
             sqlite3-64.dll and masterpass.txt. OpenSSL comes from the same URL as in make.bat, SQLite from sqlite.org
             (pinned below); both are cached in %LOCALAPPDATA%\slftp-run\dl.
    start    starts slftp.exe in -Dir in a console window of its own, hidden unless -Visible, waits for
             its prompt and prints the version line
    send     types -Text and Enter into the console
    screen   prints the console screen
    wait     waits until -Count lines of the screen contain -Text (a fixed string) within -Seconds
    stop     types !die, waits up to 60 s for the exit (else kills slFtp), then prints the [EXCEPTION]
             lines of slftp.log
    kill     ends slFtp at once, e.g. after a failed start
    serve    for slftp-run.sh in WSL: reads commands from stdin, one per line as
             <command> TAB <text> TAB <count> TAB <seconds>, and ends the output of each with a line
             "--slftp-run-end <exit code>". Stays attached to the console between commands.
  Exit codes: 0 = done, 1 = failed, text not found in time, or slFtp not running.
.PARAMETER Command
  One of the commands above.
.PARAMETER Text
  send: the line to type, e.g. an IRC command with the ! prefix. wait: the text to wait for.
.PARAMETER Dir
  Runtime directory. Default: %LOCALAPPDATA%\slftp-run\run.
.PARAMETER Exe
  prepare: the slftp.exe to copy. Default: the one make.bat builds in the repository root.
.PARAMETER Count
  wait: number of lines that must contain the text.
.PARAMETER Seconds
  wait: give up after this many seconds.
.PARAMETER Visible
  start: show the console window of slFtp.
#>
param(
  [Parameter(Mandatory)][ValidateSet('prepare', 'start', 'send', 'screen', 'wait', 'stop', 'kill', 'serve')]
  [string]$Command,
  [string]$Text = '',
  [string]$Dir = "$env:LOCALAPPDATA\slftp-run\run",
  [string]$Exe = '',
  [int]$Count = 1,
  [int]$Seconds = 30,
  [switch]$Visible
)

$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path "$PSScriptRoot\..\..").Path
if (-not $Exe) { $Exe = "$repo\slftp.exe" } # $PSScriptRoot is empty in param() defaults
$downloads = "$env:LOCALAPPDATA\slftp-run\dl"
$opensslUrl = 'https://gitlab.com/slftp/binaries/-/raw/main/openssl-3.5_x64.zip'
# pinned; change on purpose and run the skill once to check
$sqliteUrl = 'https://www.sqlite.org/2026/sqlite-dll-win-x64-3540100.zip'
$sqliteSha256 = 'f436f53ad8f965af43f3817b42c99c0689e6787f6b7ace208fe38dab7ffc4971'
$pidFile = Join-Path $Dir 'slftp-run.pid'
$endMarker = '--slftp-run-end'
$utf8 = New-Object Text.UTF8Encoding $false

# ---- prepare ----

# Get-Download <url> [<sha256>]: path of the cached download, downloaded first if missing
function Get-Download([string]$Url, [string]$Sha256 = '') {
  $file = Join-Path $downloads ($Url -replace '.*/', '')
  if (Test-Path $file) { return $file }
  New-Item -ItemType Directory -Force $downloads | Out-Null
  [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
  Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile "$file.part"
  if ($Sha256 -and (Get-FileHash -Algorithm SHA256 "$file.part").Hash -ne $Sha256) {
    Remove-Item "$file.part"
    throw "wrong checksum of $Url"
  }
  Move-Item "$file.part" $file
  return $file
}

# Get-Extracted <zip>: folder with the extracted zip, next to it
function Get-Extracted([string]$Zip) {
  $folder = $Zip -replace '\.zip$', ''
  if (-not (Test-Path $folder)) {
    Expand-Archive $Zip "$folder.part"
    Move-Item "$folder.part" $folder
  }
  return $folder
}

function Invoke-Prepare {
  if (-not (Test-Path $Exe)) { throw "$Exe is missing, build it with make.bat slftp_64 first" }
  if (Test-Path $Dir) {
    # only ever delete a former runtime directory
    if (-not (Test-Path (Join-Path $Dir 'slftp.exe'))) { throw "$Dir exists and is no runtime directory" }
    if (Get-SlftpProcess) { throw "slFtp still runs in $Dir, stop it first" }
    Remove-Item -Recurse -Force $Dir
  }
  $openssl = Get-Extracted (Get-Download $opensslUrl)
  $sqlite = Get-Extracted (Get-Download $sqliteUrl $sqliteSha256)
  New-Item -ItemType Directory $Dir | Out-Null
  Copy-Item $Exe, "$repo\config\*" $Dir
  Copy-Item "$openssl\libcrypto-3-x64.dll", "$openssl\libssl-3-x64.dll", "$openssl\ossl-modules\legacy.dll" $Dir
  # mORMot2 loads SQLite under this name in 64-bit builds (SQLITE_LIBRARY_DEFAULT_NAME)
  Copy-Item "$sqlite\sqlite3.dll" (Join-Path $Dir 'sqlite3-64.dll')
  # console.pas reads the master password from this file instead of prompting; the first start
  # creates sites.dat with it
  [IO.File]::WriteAllText((Join-Path $Dir 'masterpass.txt'), 'slftp-run')
  Write-Output "Prepared $Dir"
}

# ---- console ----

$consoleType = @'
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

public static class SlftpConsole {
  [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Unicode)]
  struct KEY_EVENT_RECORD {
    [FieldOffset(0)] public int bKeyDown;
    [FieldOffset(4)] public ushort wRepeatCount;
    [FieldOffset(6)] public ushort wVirtualKeyCode;
    [FieldOffset(8)] public ushort wVirtualScanCode;
    [FieldOffset(10)] public char UnicodeChar;
    [FieldOffset(12)] public uint dwControlKeyState;
  }
  [StructLayout(LayoutKind.Explicit)]
  struct INPUT_RECORD {
    [FieldOffset(0)] public ushort EventType;
    [FieldOffset(4)] public KEY_EVENT_RECORD KeyEvent;
  }
  [StructLayout(LayoutKind.Sequential)] struct COORD { public short X, Y; }
  [StructLayout(LayoutKind.Sequential)] struct SMALL_RECT { public short Left, Top, Right, Bottom; }
  [StructLayout(LayoutKind.Sequential)]
  struct CONSOLE_SCREEN_BUFFER_INFO {
    public COORD dwSize, dwCursorPosition;
    public ushort wAttributes;
    public SMALL_RECT srWindow;
    public COORD dwMaximumWindowSize;
  }

  [DllImport("kernel32.dll")] static extern bool FreeConsole();
  [DllImport("kernel32.dll")] static extern bool AttachConsole(int pid);
  [DllImport("kernel32.dll")] static extern bool CloseHandle(IntPtr h);
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
  static extern IntPtr CreateFile(string name, uint access, uint share, IntPtr security, uint disposition,
    uint flags, IntPtr template);
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
  static extern bool WriteConsoleInput(IntPtr h, INPUT_RECORD[] records, int count, out int written);
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
  static extern bool ReadConsoleOutputCharacter(IntPtr h, [Out] char[] buffer, int length, COORD start,
    out int read);
  [DllImport("kernel32.dll")]
  static extern bool GetConsoleScreenBufferInfo(IntPtr h, out CONSOLE_SCREEN_BUFFER_INFO info);

  static IntPtr input = IntPtr.Zero, output = IntPtr.Zero;

  static IntPtr Open(string name) {
    // GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, OPEN_EXISTING
    return CreateFile(name, 0xC0000000, 3, IntPtr.Zero, 3, 0, IntPtr.Zero);
  }

  // Attach: leaves the own console and attaches to the one of process pid. A process can only be
  // attached to one console, and the own standard handles of a console stop working with it.
  public static bool Attach(int pid) {
    Detach();
    if (!AttachConsole(pid)) return false;
    input = Open("CONIN$");
    output = Open("CONOUT$");
    return input != new IntPtr(-1) && output != new IntPtr(-1);
  }

  public static void Detach() {
    if (input != IntPtr.Zero) CloseHandle(input);
    if (output != IntPtr.Zero) CloseHandle(output);
    input = output = IntPtr.Zero;
    FreeConsole();
  }

  // Type: the text and Enter as key presses. TslWindowsScreen.KeyPressed in slconsole.pas only
  // accepts keys of its key table and ReadKey then takes the character, so letters and digits get
  // their own key code, Enter VK_RETURN and every other character VK_SPACE.
  public static bool Type(string text) {
    var records = new List<INPUT_RECORD>();
    foreach (char c in text + "\r") {
      ushort key = c == '\r' ? (ushort)0x0D :
        (c < 128 && char.IsLetterOrDigit(c)) ? (ushort)char.ToUpperInvariant(c) : (ushort)0x20;
      foreach (int down in new[] { 1, 0 }) {
        var r = new INPUT_RECORD();
        r.EventType = 1; // KEY_EVENT
        r.KeyEvent.bKeyDown = down;
        r.KeyEvent.wRepeatCount = 1;
        r.KeyEvent.wVirtualKeyCode = key;
        r.KeyEvent.UnicodeChar = c;
        records.Add(r);
      }
    }
    int written;
    return WriteConsoleInput(input, records.ToArray(), records.Count, out written) && written == records.Count;
  }

  // Screen: the lines of the screen buffer, without trailing blanks; null if it can't be read
  public static string[] Screen() {
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleScreenBufferInfo(output, out info)) return null;
    int width = info.dwSize.X, height = info.dwSize.Y, read;
    var buffer = new char[width * height];
    if (!ReadConsoleOutputCharacter(output, buffer, buffer.Length, new COORD(), out read)) return null;
    var lines = new string[height];
    for (int y = 0; y < height; y++) lines[y] = new string(buffer, y * width, width).TrimEnd();
    return lines;
  }
}
'@

# Get-SlftpProcess: the slFtp process of the runtime directory, $null if it doesn't run. A pid file
# that a hard kill left behind can name another process by now, even another slFtp.
function Get-SlftpProcess {
  if (-not (Test-Path $pidFile)) { return $null }
  $process = Get-Process -Id ([int](Get-Content $pidFile)) -ErrorAction SilentlyContinue
  if ($process -and $process.Path -eq [IO.Path]::GetFullPath((Join-Path $Dir 'slftp.exe'))) { return $process }
  return $null
}

# Connect-Slftp: attaches to the console of slFtp unless already attached, false if it doesn't run
function Connect-Slftp {
  $process = Get-SlftpProcess
  if (-not $process) { return $false }
  if ($script:attached -ne $process.Id) {
    if (-not [SlftpConsole]::Attach($process.Id)) { return $false }
    $script:attached = $process.Id
  }
  return $true
}

function Disconnect-Slftp {
  [SlftpConsole]::Detach()
  $script:attached = 0
}

# Get-Screen: the screen lines without the empty ones at the end
function Get-Screen {
  $lines = [SlftpConsole]::Screen()
  if ($null -eq $lines) { return @() }
  $last = $lines.Count - 1
  while ($last -ge 0 -and $lines[$last] -eq '') { $last-- }
  if ($last -lt 0) { return @() }
  return $lines[0..$last]
}

# Wait-Screen <text> <count> <seconds>: true if <count> lines contain the text within <seconds>. It
# polls because a single read can catch slFtp redrawing its window.
function Wait-Screen([string]$Find, [int]$Lines, [int]$Within) {
  $deadline = (Get-Date).AddSeconds($Within)
  do {
    if (@(Get-Screen | Where-Object { $_.Contains($Find) }).Count -ge $Lines) { return $true }
    Start-Sleep -Milliseconds 100
  } while ((Get-Date) -lt $deadline)
  return $false
}

function Invoke-Start {
  if (Get-SlftpProcess) { Write-Line "ERROR: slFtp already runs in $Dir"; return 1 }
  $slftpExe = Join-Path $Dir 'slftp.exe'
  if (-not (Test-Path $slftpExe)) { Write-Line "ERROR: $slftpExe is missing, run prepare first"; return 1 }
  # conhost, not the default terminal (e.g. Windows Terminal): a window of its own that can be hidden,
  # whose screen buffer has the size slFtp sets (width and height in slftp.ini)
  $startArgs = @{ FilePath = 'conhost.exe'; ArgumentList = "`"$slftpExe`""; WorkingDirectory = $Dir; PassThru = $true }
  if (-not $Visible) { $startArgs.WindowStyle = 'Hidden' }
  $conhost = Start-Process @startArgs
  $process = $null
  for ($t = 0; $t -lt 50 -and -not $process; $t++) {
    Start-Sleep -Milliseconds 100
    $process = Get-CimInstance Win32_Process -Filter "ParentProcessId = $($conhost.Id) AND Name = 'slftp.exe'"
  }
  if (-not $process) { Write-Line 'ERROR: slftp.exe did not start'; return 1 }
  [IO.File]::WriteAllText($pidFile, [string]$process.ProcessId)
  if (-not (Connect-Slftp)) { Write-Line 'ERROR: cannot attach to the console of slFtp'; return 1 }
  if (-not (Wait-Screen 'Command:' 1 30)) {
    Write-Line 'ERROR: no prompt within 30 s, the screen:'
    Get-Screen | ForEach-Object { Write-Line $_ }
    return 1
  }
  Start-Sleep -Seconds 3 # until the queue has started
  # a start error, e.g. a missing library, shows up in a Message window over the Admin window
  if (@(Get-Screen | Where-Object { $_.Contains(' Message ') }).Count -gt 0) {
    Write-Line 'ERROR: slFtp shows a message, the screen:'
    Get-Screen | ForEach-Object { Write-Line $_ }
    return 1
  }
  Get-Screen | Select-String -Pattern 'slFtp v\S+ .*git# \S+\)' -AllMatches |
    ForEach-Object { $_.Matches.Value } | Sort-Object -Unique | ForEach-Object { Write-Line $_ }
  return 0
}

function Invoke-Stop {
  $process = Get-SlftpProcess
  if (-not $process -or -not (Connect-Slftp)) { Write-Line "ERROR: slFtp doesn't run in $Dir"; return 1 }
  [SlftpConsole]::Type('!die') | Out-Null
  if (-not $process.WaitForExit(60000)) {
    Write-Line 'NOTE: slFtp did not exit within 60 s after !die, killed it'
    Stop-Process -Id $process.Id -Force
    $process.WaitForExit(5000) | Out-Null
  }
  Disconnect-Slftp
  Remove-Item $pidFile
  $log = Join-Path $Dir 'slftp.log'
  if (Test-Path $log) {
    Select-String -Path $log -SimpleMatch '[EXCEPTION]' | Select-Object -Last 20 |
      ForEach-Object { Write-Line "NOTE: slftp.log: $($_.Line)" }
  }
  return 0
}

# Invoke-Console <command> <text> <count> <seconds>: runs one console command, returns its exit code
function Invoke-Console([string]$Name, [string]$Arg, [int]$Lines, [int]$Within) {
  switch ($Name) {
    'start' { return Invoke-Start }
    'stop' { return Invoke-Stop }
    'kill' {
      $process = Get-SlftpProcess
      Disconnect-Slftp
      if ($process) {
        Stop-Process -Id $process.Id -Force
        $process.WaitForExit(5000) | Out-Null
      }
      if (Test-Path $pidFile) { Remove-Item $pidFile }
      return 0
    }
  }
  if (-not (Connect-Slftp)) { Write-Line "ERROR: slFtp doesn't run in $Dir"; return 1 }
  switch ($Name) {
    'send' {
      if ([SlftpConsole]::Type($Arg)) { return 0 }
      Write-Line 'ERROR: typing into the console failed'
      return 1
    }
    'screen' { Get-Screen | ForEach-Object { Write-Line $_ }; return 0 }
    'wait' {
      if (Wait-Screen $Arg $Lines $Within) { return 0 }
      Write-Line "TIMEOUT after $Within s waiting for '$Arg' x$Lines"
      return 1
    }
  }
  Write-Line "ERROR: unknown command '$Name'"
  return 1
}

# Write-Line: output of a command. Once attached to the console of slFtp, the own console output is
# gone, so serve writes to the standard output stream it opened before.
function Write-Line([string]$Line) {
  if ($script:stdout) { $script:stdout.WriteLine($Line) } else { Write-Output $Line }
}

function Invoke-Serve {
  Add-Type -TypeDefinition $consoleType
  $stdin = New-Object IO.StreamReader([Console]::OpenStandardInput(), $utf8)
  $script:stdout = New-Object IO.StreamWriter([Console]::OpenStandardOutput(), $utf8)
  $script:stdout.AutoFlush = $true
  while ($null -ne ($line = $stdin.ReadLine())) {
    $fields = @($line -split "`t") + @('', '', '', '')
    $lines = 1
    $within = 30
    if ($fields[2]) { $lines = [int]$fields[2] }
    if ($fields[3]) { $within = [int]$fields[3] }
    try {
      $rc = Invoke-Console $fields[0] $fields[1] $lines $within
    } catch {
      Write-Line "ERROR: $_"
      $rc = 1
    }
    Write-Line "$endMarker $rc"
  }
  Disconnect-Slftp
}

# Invoke-Once: runs the command in a serve process of its own, whose console can attach to slFtp
# while this one keeps its output; prints the output and sets $onceExitCode
function Invoke-Once {
  $info = New-Object Diagnostics.ProcessStartInfo 'powershell.exe'
  $info.Arguments = "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`" serve -Dir `"$Dir`""
  if ($Visible) { $info.Arguments += ' -Visible' }
  $info.UseShellExecute = $false
  $info.CreateNoWindow = $true
  $info.RedirectStandardInput = $true
  $info.RedirectStandardOutput = $true
  $info.StandardOutputEncoding = $utf8
  $serve = [Diagnostics.Process]::Start($info)
  $serve.StandardInput.WriteLine("$Command`t$Text`t$Count`t$Seconds")
  $serve.StandardInput.Close()
  $script:onceExitCode = 1
  while ($null -ne ($line = $serve.StandardOutput.ReadLine())) {
    if ($line.StartsWith("$endMarker ")) {
      $script:onceExitCode = [int]$line.Substring($endMarker.Length + 1)
    } else {
      Write-Output $line
    }
  }
  $serve.WaitForExit()
}

switch ($Command) {
  'prepare' {
    try { Invoke-Prepare } catch { Write-Output "ERROR: $_"; exit 1 }
    exit 0
  }
  'serve' { Invoke-Serve; exit 0 }
  default { Invoke-Once; exit $onceExitCode }
}
