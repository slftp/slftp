#!/bin/bash
# Runs slFtp in a runtime directory and drives its console: the Linux build in tmux, or from WSL the
# Windows build through slftp-run.ps1 next to this file. Used by the skills run-slftp (as a command,
# see its SKILL.md) and glftpd-e2e (sourced by e2e.sh).
#
#   slftp-run.sh prepare [<build dir>]  fresh runtime directory ~/slftp-run/run from <build dir>
#                          (default ~/slftp-run/build, e.g. from fpc-wsl.sh --export), the libsqlite3
#                          of the system and masterpass.txt
#   slftp-run.sh start     starts slFtp there in the tmux session slftp-run, waits for its prompt and
#                          prints the version line
#   slftp-run.sh send <line>  types the line and Enter into the console
#   slftp-run.sh screen    prints the console screen
#   slftp-run.sh wait <text> [<count>] [<seconds>]  waits until <count> lines (default 1) contain the
#                          text (a fixed string) within <seconds> (default 30)
#   slftp-run.sh stop      types !die, waits up to 60 s for the exit (else kills slFtp), then prints
#                          the [EXCEPTION] lines of slftp.log
#   slftp-run.sh kill      ends slFtp at once, e.g. after a failed start
#
# Exit codes: 0 = done, 1 = failed, text not found in time, or slFtp not running, 10 = tool missing.
#
# Sourced, it only defines the functions below. Set run_dir (runtime directory) and session (tmux
# session name) first, or slftp_windows=1 for the Windows build: then run_dir is on a Windows drive,
# and one slftp-run.ps1 serve process drives slftp.exe from slftp_start to slftp_stop.

slftp_lib_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# note <text>: information on the run, e.g. a timeout; e2e.sh redefines it to record it
note() { echo "NOTE: $*"; }

# slftp_check_env: exit code 10 if a tool is missing
slftp_check_env() {
  if [ -n "$slftp_windows" ]; then
    command -v powershell.exe >/dev/null || { echo 'ERROR: powershell.exe not found (WSL interop)'; return 10; }
    return 0
  fi
  command -v tmux >/dev/null || { echo 'ERROR: tmux missing'; return 10; }
  # slFtp loads ./libsqlite3.so on Linux, and the CI image has no libsqlite3. ldconfig is in /sbin,
  # which is not in the PATH of normal users on some distributions.
  slftp_sqlite=$(PATH="$PATH:/sbin" ldconfig -p | grep -oE '/\S+/libsqlite3\.so\.0$' | sort | tail -1)
  [ -n "$slftp_sqlite" ] || { echo 'ERROR: libsqlite3 missing (package libsqlite3-0)'; return 10; }
}

# slftp_prepare <build dir>: fresh run_dir with the build (slftp or slftp.exe, config files and
# OpenSSL libraries), on Linux the libsqlite3 of the system, and masterpass.txt
slftp_prepare() {
  [ -f "$1/slftp" ] || [ -f "$1/slftp.exe" ] || { echo "ERROR: no slFtp build in $1"; return 1; }
  # only ever delete a former runtime directory
  if [ -e "$run_dir" ] && [ ! -f "$run_dir/slftp" ] && [ ! -f "$run_dir/slftp.exe" ]; then
    echo "ERROR: $run_dir exists and is no runtime directory"
    return 1
  fi
  # the slFtp of a killed run (the caller holds the lock of the runs): Windows can't delete a running
  # slftp.exe
  if [ -n "$slftp_windows" ] && [ -f "$run_dir/slftp-run.pid" ]; then
    _ps_run kill || return
  fi
  # the conhost of a killed slFtp can hold the directory for a moment, and without slftp.exe the check
  # above would refuse every later run
  rm -rf "$run_dir" || { sleep 2; rm -rf "$run_dir"; } || return
  mkdir -p "$run_dir" && cp -r "$1/." "$run_dir/" || return
  [ -n "$slftp_windows" ] || cp -L "$slftp_sqlite" "$run_dir/libsqlite3.so" || return
  # console.pas reads the master password from this file instead of prompting; the first start
  # creates sites.dat with it
  echo slftp-run > "$run_dir/masterpass.txt"
}

# _ps_run <command>: runs slftp-run.ps1 <command> for run_dir
_ps_run() {
  powershell.exe -NoProfile -ExecutionPolicy Bypass -File "$(wslpath -w "$slftp_lib_dir/slftp-run.ps1")" \
    "$1" -Dir "$(wslpath -w "$run_dir")"
}

# _ps <command> [<text> [<count> [<seconds>]]]: runs a command of the serve process (see
# slftp-run.ps1) and prints its output, NOTE lines through note; its exit code is the command's
_ps() {
  local line
  printf '%s\t%s\t%s\t%s\n' "$1" "$2" "$3" "$4" >&"$_ps_in" || return 1
  while IFS= read -r line <&"$_ps_out"; do
    line=${line%$'\r'}
    case "$line" in
      '--slftp-run-end '*) return "${line#--slftp-run-end }" ;;
      'NOTE: '*) note "${line#NOTE: }" ;;
      *) printf '%s\n' "$line" ;;
    esac
  done
  echo 'ERROR: slftp-run.ps1 serve has ended'
  return 1
}

# pane: the console screen
pane() {
  if [ -n "$slftp_windows" ]; then _ps screen; else tmux capture-pane -p -J -S -3000 -t "$session"; fi
}

# console <text>: types a line into the Admin window, e.g. an IRC command with the ! prefix
console() {
  if [ -n "$slftp_windows" ]; then
    _ps send "$1" || return
  else
    tmux send-keys -t "$session" -l -- "$1" && tmux send-keys -t "$session" Enter || return
  fi
  sleep 0.7
}

# console_has <text> [<seconds>]: true if the console shows the text (fixed string, not a pattern)
# within <seconds> (default 2). It polls because a single capture can catch slFtp redrawing its
# window, which then looks empty. No grep -q here and in the other pipes: it exits early, and with
# pipefail the writer's SIGPIPE fails the pipe.
console_has() {
  local t=0
  until pane | grep -F -- "$1" >/dev/null; do
    sleep 0.1
    t=$((t + 1))
    [ $t -lt $((${2:-2} * 10)) ] || return 1
  done
}

# console_ok: true if no command so far has replied Failed.
console_ok() { ! console_has 'Failed.' 1; }

# wait_console <text> [<count>] [<seconds>]: waits until the console shows the text <count> times
wait_console() {
  local t=0
  while [ "$(pane | grep -cF -- "$1")" -lt "${2:-1}" ]; do
    sleep 0.1
    t=$((t + 1))
    [ $t -lt $((${3:-30} * 10)) ] || { note "TIMEOUT after ${3:-30} s waiting for '$1' x${2:-1}"; return 1; }
  done
  return 0
}

# slftp_start: starts slFtp in run_dir, waits for its prompt and prints the version line
slftp_start() {
  if [ -n "$slftp_windows" ]; then
    coproc _slftp_ps { _ps_run serve; }
    # coproc file descriptors are not available in subshells, as in pane | grep
    exec {_ps_in}>&"${_slftp_ps[1]}" {_ps_out}<&"${_slftp_ps[0]}"
    _ps start
    return
  fi
  tmux kill-session -t "$session" 2>/dev/null
  # the trailing echo and sleep keep the pane, so its exit shows; slFtp clears the screen on exit
  tmux new-session -d -s "$session" -x 250 -y 100 \
    "cd '$run_dir' && LD_LIBRARY_PATH=. ./slftp; echo SLFTP-EXITED rc=\$?; sleep 3600" || return
  wait_console 'Command:' 1 30 || { pane; return 1; }
  sleep 3 # until the queue has started
  # a start error, e.g. a missing library, shows up in a Message window over the Admin window
  if pane | grep -F ' Message ' >/dev/null; then
    echo 'ERROR: slFtp shows a message, the screen:'
    pane
    return 1
  fi
  pane | grep -oE 'slFtp v\S+ .*git# \S+\)' | sort -u
  return 0
}

# slftp_stop: types !die, waits up to 60 s for the exit, else kills slFtp; then notes the
# [EXCEPTION] lines of slftp.log
slftp_stop() {
  local line
  if [ -n "$slftp_windows" ]; then
    _ps stop
    _ps_close
    return 0
  fi
  console '!die'
  if ! wait_console 'SLFTP-EXITED' 1 60; then
    note 'slFtp did not exit within 60 s after !die, killed it'
    pkill -9 -P "$(tmux display -p -t "$session" '#{pane_pid}')" -x slftp
    sleep 1
  fi
  tmux kill-session -t "$session" 2>/dev/null
  grep -F '[EXCEPTION]' "$run_dir/slftp.log" | tail -20 | while IFS= read -r line; do
    note "slftp.log: $line"
  done
  return 0
}

# slftp_kill: ends slFtp at once, also after a failed or no start. On Windows not through the serve
# process: an aborted run's signal can have ended it, or a killed pane can have left a reply in its pipe.
slftp_kill() {
  if [ -n "$slftp_windows" ]; then
    [ -z "$_ps_in" ] || { _ps_close; _ps_run kill; }
  else
    tmux kill-session -t "$session" 2>/dev/null
  fi
  return 0
}

# _ps_close: ends the serve process
_ps_close() {
  exec {_ps_in}>&- {_ps_out}<&-
  _ps_in=''
}

# as a command, see the top of this file
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  run_dir="$HOME/slftp-run/run"
  session=slftp-run
  slftp_check_env || exit
  case "$1" in
    prepare) slftp_prepare "${2:-$HOME/slftp-run/build}" ;;
    start) slftp_start ;;
    send) console "$2" ;;
    screen) pane ;;
    wait) wait_console "$2" "${3:-1}" "${4:-30}" ;;
    stop) slftp_stop ;;
    kill) slftp_kill ;;
    *) echo "usage: see the top of $0"; exit 1 ;;
  esac
fi
