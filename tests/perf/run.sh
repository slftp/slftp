#!/bin/bash
# Inside the disposable CI worker: same fixture, toolchain, observer and config for A/B/B/A.
set -Eeo pipefail
baseline_sha="$1"
branch_sha="$2"
root=/work/branch
out=/work/results
mkdir -p "$out"
exec > >(tee "$out/run.log") 2>&1
trap 'echo "ERROR: benchmark driver line $LINENO: $BASH_COMMAND"' ERR
bash -n "$root/tests/perf/run.sh" "$root/tests/perf/ci.sh" "$root/.agents/skills/glftpd-e2e/e2e.sh"
python3 -m unittest discover -s "$root/tests/perf" -p 'test_*.py'
python3 "$root/tests/perf/workload.py" --prepare /work/payload
cp /work/payload/manifest.json "$out/manifest.json"

# Reuse the pinned glftpd image and console helpers of the E2E test.
. "$root/.agents/skills/glftpd-e2e/e2e.sh"
cleanup() { slftp_kill || true; stop_sites || true; }
trap cleanup EXIT INT TERM
slftp_check_env || exit 1
prepare_image || exit 1

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
path = root / "slftp.ini"
text = path.read_text()
# Preserve duplicate keys and the project's own INI syntax; edit only existing timeout keys.
section = ""
lines = []
for line in text.splitlines():
    if line.startswith("["):
        section = line.strip().lower()
    if section == "[timeout]" and line.split("=", 1)[0].strip() in ("connect", "io"):
        line = line.split("=", 1)[0] + "=5"
    lines.append(line)
path.write_text("\n".join(lines) + "\n")
PY
  slftp_start || { pane; exit 1; }
  pane_pid=$(tmux display -p -t "$session" '#{pane_pid}')
  pid=$(pgrep -P "$pane_pid" -x slftp)
  [ -n "$pid" ] || { echo 'ERROR: slFtp PID not found'; exit 1; }
  python3 "$root/tests/perf/workload.py" --payload /work/payload --out "$trial_out" \
    --ports "$(site_port 1),$(site_port 2),$(site_port 3),$(site_port 4)" \
    --pid "$pid" --session "$session" > "$trial_out/workload.log" 2>&1 &
  load_pid=$!
  for i in $(seq 1 100); do
    [ -f "$trial_out/ready" ] && break
    kill -0 "$load_pid" || { cat "$trial_out/workload.log"; exit 1; }
    sleep 0.1
  done
  [ -f "$trial_out/ready" ]
  for n in 1 2 3 4; do
    console "!addsite E2E$n glftpd glftpd 127.0.0.1:$(($(site_port "$n") + 2000))"
    console "!sslmethod E2E$n 3"
    console "!setdir E2E$n RACE /RACE"
    console "!slots E2E$n 6"
  done
  console '!slots E2E3 1'
  console '!ruleadd * RACE if default then ALLOW'
  console '!routeset E2E* E2E* 5 -a -l'
  console '!routeset E2E* E2E3 9 -a -l'
  console '!ircnetadd PERF 127.0.0.1:16667 0'
  console '!ircchanadd PERF #race'
  console '!catchadd E2E1 PERF #race perfbot NEWDIR NEW RACE'
  console '!bnctest'
  for n in 1 2 3 4; do wait_console "SITE E2E$n IS UP" 1 40; done
  console_ok || { pane > "$trial_out/console.txt"; exit 1; }
  touch "$trial_out/go"
  rc=0
  wait "$load_pid" || rc=$?
  pane > "$trial_out/console.txt"
  cp "$run_dir/slftp.log" "$trial_out/slftp.log"
  # Stop outside the timed interval, retaining the known shutdown notes in run.log.
  slftp_stop || exit 1
  stop_sites || exit 1
  if [ "$rc" -ne 0 ]; then
    cat "$trial_out/workload.log"
    echo "ERROR: trial $trial ($variant) failed"
    # Keep running the other trials to retain both sides of a failed comparison.
  fi
done
python3 "$root/tests/perf/report.py" "$out" "$baseline_sha" "$branch_sha"
