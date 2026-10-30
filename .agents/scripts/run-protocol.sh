#!/bin/bash
# Lock, log and exit code files of the long runs, and the options wsl-run.ps1 next to this file polls
# them with. Sourced by fpc-wsl.sh next to this file and .agents/skills/glftpd-e2e/e2e.sh. Set
# log_file, exitcode_file and summary_pattern first, and for a run with --id <id> also run_id=<id>
# and exitcode_file=<exitcode_file>.<id>, so the caller never reads the result of another run.
#
#   <script> --clear [<id>]   remove the exit code file of the last run (or of run <id>)
#   <script> --status [<id>]  print the exit code of the last run (or of run <id>), nothing while it
#                             is still running
#   <script> --summary        print the lines of the last log that match summary_pattern
#
# Exit code 11: another run is active. The scripts use 10 for an environment that is not set up.

# run_query <option> [<id>]: answers --clear, --status and --summary and exits; returns for any other
# option
run_query() {
  case "$1" in
    --clear) rm -f "$exitcode_file${2:+.$2}"; exit 0 ;;
    --status) [ -f "$exitcode_file${2:+.$2}" ] && cat "$exitcode_file${2:+.$2}"; exit 0 ;;
    --summary)
      grep -E "$summary_pattern" "$log_file"
      echo "Full log: $log_file"
      exit 0 ;;
  esac
}

# run_locked <lock file> <command>...: runs the command with its output also in log_file, writes its
# exit code to exitcode_file and exits with it. Exit code 11 if another run holds the lock, which is
# held until the script exits; then log and exit code of the other run stay alone.
run_locked() {
  local rc
  exec 9>"$1"
  shift
  if ! flock -n 9; then
    echo "ERROR: another run is active, try again when it has finished"
    [ -z "$run_id" ] || echo 11 > "$exitcode_file"
    exit 11
  fi
  rm -f "$exitcode_file"
  set -o pipefail
  "$@" 2>&1 | tee "$log_file"
  rc=$?
  if [ $rc -eq 0 ]; then
    echo "=== PASSED ==="
  else
    echo "=== FAILED with exit code $rc ==="
  fi | tee -a "$log_file"
  echo $rc > "$exitcode_file"
  exit $rc
}
