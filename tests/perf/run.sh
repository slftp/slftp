#!/bin/bash
# Inside the disposable CI worker: same fixture, toolchain, observer and config for A/B/B/A.
set -Eeo pipefail
[ "${SLFTP_PERF_WORKER:-}" = 1 ] && [ -d /work/baseline ] && [ -d /work/branch ] || {
  echo 'ERROR: use tests/perf/linux.sh or ci.sh; run.sh requires the disposable worker'
  exit 1
}
for site in /sites/1 /sites/2 /sites/3 /sites/4; do
  mountpoint -q "$site" || { echo "ERROR: $site is not a disposable volume mount"; exit 1; }
done
scenario=${PERF_SCENARIO:-timing}
baseline_sha="$1"
branch_sha="$2"
root=/work/branch
out=/work/results
mkdir -p "$out"
exec > >(tee "$out/run.log") 2>&1
trap 'echo "ERROR: benchmark driver line $LINENO: $BASH_COMMAND"' ERR
finish() {
  rc=$?
  trap - EXIT
  trap '' INT TERM
  if [ -n "${load_pid:-}" ]; then
    kill "$load_pid" 2>/dev/null || true
    wait "$load_pid" 2>/dev/null || true
  fi
  if [ -n "${trial_out:-}" ] && [ ! -f "$trial_out/result.json" ]; then
    python3 - "$trial_out/result.json" "$rc" <<'PY'
import json, pathlib, sys
pathlib.Path(sys.argv[1]).write_text(json.dumps({"error": "trial driver failed, exit " + sys.argv[2]}))
PY
  fi
  if declare -F slftp_kill >/dev/null; then slftp_kill || true; fi
  if declare -F stop_sites >/dev/null; then stop_sites || true; fi
  python3 "$root/tests/perf/report.py" "$out" "$baseline_sha" "$branch_sha" || rc=1
  exit "$rc"
}
trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
for script in "$root/tests/perf/run.sh" "$root/tests/perf/linux.sh" "$root/.agents/skills/glftpd-e2e/e2e.sh"; do
  bash -n "$script"
done
sh -n "$root/tests/perf/ci.sh"
python3 -m unittest discover -s "$root/tests/perf" -p 'test_*.py'
python3 "$root/tests/perf/workload.py" --prepare /work/payload --scenario "$scenario"
cp /work/payload/manifest.json "$out/manifest.json"

# Reuse the pinned glftpd image and console helpers of the E2E test.
. "$root/.agents/skills/glftpd-e2e/e2e.sh"
slftp_check_env || exit 1
prepare_image || exit 1

# Fresh blocks per trial avoid reusing TIME_WAIT data ports; 100 ports/site was
# sufficient for the original ten-file E2E scenario, not this concurrent load.
site_pasv() { echo "$((pasv_base + 3000 * ($1 - 1)))-$((pasv_base + 3000 * $1 - 1))"; }
choose_ports() {
  port_base=$((2120 + 100 * trial))
  pasv_base=$((17000 + 12000 * (trial - 1)))
  for n in 1 2 3 4; do
    ports_free "$(site_port "$n")-$(site_port "$n")" &&
      ports_free "$(site_pasv "$n")" || { fail 'benchmark port block is occupied'; return 1; }
  done
}

for variant in baseline branch; do
  src="/work/$variant"
  build="/work/build-$variant"
  mkdir -p "$build"
  if [ "$variant" = baseline ]; then source_sha="$baseline_sha"; else source_sha="$branch_sha"; fi
  (cd "$src" && perl replace_git_commit.pl "$source_sha" && make slftp_64) > "$out/build-$variant.log" 2>&1 || {
    tail -40 "$out/build-$variant.log"; exit 1;
  }
  cp "$src/slftp" "$build/"
  # Both binaries read exactly the same configuration.
  cp -r "$root/config/." "$build/"
  cp /usr/lib/x86_64-linux-gnu/libcrypto.so.3 /usr/lib/x86_64-linux-gnu/libssl.so.3 "$build/"
  cp /usr/lib/x86_64-linux-gnu/ossl-modules/legacy.so "$build/"
  sha256sum "$build/slftp" >> "$out/binaries.txt"
done
trial=0
for variant in baseline branch branch baseline; do
  trial=$((trial + 1))
  trial_out="$out/$trial-$variant"
  mkdir -p "$trial_out"
  result="$trial_out/checks.txt"
  session="slftp-perf-$trial"
  run_dir="/work/run-$trial"
  build="/work/build-$variant"
  sites=4
  sections=RACE
  site_volume_prefix=perf-site
  site_max_users=100
  for site in /sites/1 /sites/2 /sites/3 /sites/4; do
    find "$site" -mindepth 1 -delete
    chmod 777 "$site"
  done
  start_sites || exit 1
  slftp_prepare "$build" || exit 1
  # No network metadata lookups: a dedicated synthetic section with the common defaults.
  python3 - "$run_dir" <<'PY'
import pathlib, sys
root = pathlib.Path(sys.argv[1])
path = root / "slftp.precatcher"
text = path.read_text().replace("[sections]", "[sections]\nRACE=RACE")
path.write_text(text)
PY
  slftp_start || { pane; exit 1; }
  pane_pid=$(tmux display -p -t "$session" '#{pane_pid}')
  pid=$(pgrep -P "$pane_pid" -x slftp)
  [ -n "$pid" ] || { echo 'ERROR: slFtp PID not found'; exit 1; }
  timeout --signal=TERM --kill-after=5s 360s python3 "$root/tests/perf/workload.py" --payload /work/payload --out "$trial_out" \
    --ports "$(site_port 1),$(site_port 2),$(site_port 3),$(site_port 4)" \
    --pid "$pid" --session "$session" --scenario "$scenario" > >(tee "$trial_out/workload.log") 2>&1 &
  load_pid=$!
  for i in $(seq 1 100); do
    [ -f "$trial_out/ready" ] && break
    kill -0 "$load_pid" || { cat "$trial_out/workload.log"; exit 1; }
    sleep 0.1
  done
  [ -f "$trial_out/ready" ]
  for n in 1 2 3 4; do
    connect_port=$(site_port "$n")
    if [ "$n" -eq 4 ]; then connect_port=$((connect_port + 2000)); fi
    console "!addsite E2E$n glftpd glftpd 127.0.0.1:$connect_port"
    console "!sslmethod E2E$n 3"
    console "!setdir E2E$n RACE /RACE"
    console "!slots E2E$n 6"
  done
  console '!slots E2E3 1'
  console '!ruleadd * RACE if default then ALLOW'
  console '!routeset * * 5 -a -l'
  console '!routeset * E2E3 9 -a -l'
  console '!ircnetadd PERF 127.0.0.1:16667 0'
  console '!ircchanadd PERF #race'
  console '!catchadd E2E1 PERF #race perfbot NEWDIR NEW RACE'
  console '!catchadd E2E1 PERF #race perfbot COMPLETE COMPLETE RACE'
  console '!auto 1'
  wait_console 'Auto is enabled [1] now!' 1 5
  console '!bnctest'
  for n in 1 2 3 4; do wait_console "SITE E2E$n IS UP" 1 40; done
  console_ok || { pane | tee "$trial_out/console.txt"; echo 'ERROR: setup command failed'; exit 1; }
  touch "$trial_out/go"
  rc=0
  wait "$load_pid" || rc=$?
  load_pid=''
  if [ ! -f "$trial_out/result.json" ]; then
    python3 - "$trial_out/result.json" "$rc" <<'PY'
import json, pathlib, sys
pathlib.Path(sys.argv[1]).write_text(json.dumps({"error": "workload exited without result, exit " + sys.argv[2]}))
PY
  fi
  if [ "$rc" -ne 0 ]; then
    console '!ircstatus'
    console '!catchlist'
    console '!kblist RACE'
    console '!catchtest PERF #race perfbot NEW RACE Perf.Load.Volume00.2026.1080p.WEB.h264-E2EGRP'
    pane | tee "$trial_out/console.txt"
  else
    pane > "$trial_out/console.txt"
  fi
  cp "$run_dir/slftp.log" "$trial_out/slftp.log"
  # Stop outside the timed interval, retaining the known shutdown notes in run.log.
  slftp_stop || exit 1
  stop_sites || exit 1
  if [ "$rc" -ne 0 ]; then
    cat "$trial_out/workload.log"
    echo "ERROR: trial $trial ($variant) failed"
    # Continue from fresh sites so a baseline defect cannot hide candidate behavior.
  fi
done
