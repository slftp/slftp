#!/bin/bash
# Runs an end-to-end scenario inside WSL: slFtp, built with FPC in the GitLab CI image, against glftpd
# sites with pzs-ng in Docker containers, driven through slFtp's console in tmux. Started by e2e.ps1,
# see SKILL.md next to this file. Runs as the normal WSL user and only reads the Windows checkout.
#
#   e2e.sh <scenario>          build HEAD + uncommitted changes + untracked (not ignored) files and run
#                              scenarios/<scenario>.sh with that build
#   e2e.sh <scenario> <ref>    the same with only the committed state of <ref>
#   e2e.sh --baseline <ref> <scenario> [<ref>]  also build <ref> and run the scenario with it first
#   e2e.sh --id <id> ...       write the exit code to a file of its own for run <id>
#   e2e.sh --initialized       exit code 0 if the glftpd image exists (else the run builds it, slow)
#   e2e.sh --clear [<id>]      remove the exit code file of the last run (or of run <id>)
#   e2e.sh --status [<id>]     print the exit code of the last run (or of run <id>), nothing while it
#                              is still running
#   e2e.sh --summary           print the results of the last run
#
# Everything lives in ~/slftp-e2e: downloads (dl/), image build context (image/), slFtp builds
# (build-<name>/), runtime directories (run-<name>/), results (result-<name>.txt), log and exit code.
# Only one run at a time: another one exits with code 11. The slFtp build itself is done by
# test-both-compilers/fpc-wsl.sh and shares its lock. Exit code 10: environment not set up.

skill_dir="$(cd "$(dirname "$0")" && pwd)"
win_repo="$(cd "$skill_dir/../../.." && pwd)"
fpc_script="$skill_dir/../test-both-compilers/fpc-wsl.sh"
work="$HOME/slftp-e2e"
log_file="$work/e2e.log"
exitcode_file="$work/e2e.exitcode"
image=slftp-e2e-glftpd
setup_doc=.agents/docs/wsl-fpc-setup.md

# pinned third-party software; change these on purpose and run the skill once to check
gl_url=https://glftpd.io/files/glftpd-LNX-2.16_3.6.0_x64.tgz
gl_sha512=2269c40146a9b81743ff08711b11f1f28ad1c704a38aa65bd0b8ac53de8290e2750d287dcbb7f5784e63bd961584a11475625650bd662f2b3869fd9f64c43e80
pzs_url=https://github.com/glftpd/pzs-ng
pzs_commit=d69bacaf1f4897ff76d72413e17da9356695be7e
# one image per state of its inputs, so an unchanged image needs no build
image_tag="$image:$({ echo "$gl_url $gl_sha512 $pzs_commit"; cat "$skill_dir"/glftpd/*; } |
  sha256sum | cut -c1-12)"

mkdir -p "$work"
case "$1" in
  --initialized) docker image inspect "$image_tag" >/dev/null 2>&1; exit ;;
  --clear) rm -f "$exitcode_file${2:+.$2}"; exit 0 ;;
  --status) [ -f "$exitcode_file${2:+.$2}" ] && cat "$exitcode_file${2:+.$2}"; exit 0 ;;
  --summary)
    # steps, checks, notes, result files and the compiler errors of a failed build
    grep -E '^=== |^ERROR|^(PASS|FAIL|NOTE):|^Result file:|Error:|Fatal:' "$log_file"
    echo "Full log: $log_file"
    exit 0 ;;
esac
while [ $# -gt 0 ]; do
  case "$1" in
    --id) run_id="$2"; exitcode_file="$exitcode_file.$2"; shift 2 ;;
    --baseline) baseline_ref="$2"; shift 2 ;;
    *) break ;;
  esac
done
scenario="$1"
ref="$2"

step() { echo; echo "=== $* ==="; }
fail() { echo "ERROR: $*"; return 1; }
win_git() { (cd "$win_repo" && git.exe "$@"); }

check_env() {
  local missing='' tool
  for tool in git.exe git docker tmux curl python3; do
    command -v "$tool" >/dev/null || missing="$missing $tool"
  done
  [ -z "$missing" ] || { fail "missing in WSL:$missing, see $setup_doc"; return 10; }
  docker info >/dev/null 2>&1 || { fail "docker not usable by $(id -un), see $setup_doc"; return 10; }
  # slFtp loads ./libsqlite3.so on Linux, and the CI image has no libsqlite3
  sqlite_lib=$(ldconfig -p | grep -oE '/\S+/libsqlite3\.so\.0$' | sort | tail -1)
  [ -n "$sqlite_lib" ] || { fail "libsqlite3 missing in WSL (package libsqlite3-0)"; return 10; }
  [ -f "$skill_dir/scenarios/$scenario.sh" ] || fail "unknown scenario '$scenario', see $skill_dir/scenarios"
}

# Builds the site image unless it exists for the current inputs: downloads glftpd and pzs-ng
# (verified by checksum and commit hash) and builds it, then removes the images of older inputs and
# the build stages. Downloads are only extracted and copied here; their code runs inside the build.
prepare_image() {
  local gl_tgz="$work/dl/${gl_url##*/}" pzs_dir="$work/dl/pzs-ng" context="$work/image"
  step "prepare glftpd image $image_tag"
  if docker image inspect "$image_tag" >/dev/null 2>&1; then echo 'up to date'; return 0; fi
  mkdir -p "$work/dl" || return
  if ! { [ -f "$gl_tgz" ] && echo "$gl_sha512  $gl_tgz" | sha512sum -c --quiet; }; then
    curl -sSfo "$gl_tgz.part" "$gl_url" || fail "download of $gl_url failed" || return
    echo "$gl_sha512  $gl_tgz.part" | sha512sum -c --quiet || fail "wrong checksum of $gl_url" || return
    mv "$gl_tgz.part" "$gl_tgz" || return
  fi
  [ -d "$pzs_dir/.git" ] || git init -q "$pzs_dir" || return
  if [ "$(git -C "$pzs_dir" rev-parse -q --verify HEAD)" != "$pzs_commit" ]; then
    git -C "$pzs_dir" fetch -q --depth 1 "$pzs_url" "$pzs_commit" &&
      git -C "$pzs_dir" checkout -q -f --detach "$pzs_commit" || fail "fetch of pzs-ng failed" || return
  fi
  rm -rf "$context" && mkdir -p "$context/glftpd" "$context/pzs-ng" || return
  tar -xzf "$gl_tgz" -C "$context/glftpd" --strip-components=1 && rm -rf "$context/glftpd/docs" &&
    git -C "$pzs_dir" archive "$pzs_commit" | tar -xf - -C "$context/pzs-ng" &&
    cp "$skill_dir"/glftpd/* "$context/" || return
  docker build -q --force-rm -t "$image_tag" "$context" || fail "image build failed" || return
  rm -rf "$context"
  docker images "$image" --format '{{.Repository}}:{{.Tag}}' | grep -vxF "$image_tag" |
    xargs -r docker rmi >/dev/null
  docker image prune -f --filter label=slftp-e2e >/dev/null
}

# Builds slFtp with fpc-wsl.sh into build-<name>; ref empty = HEAD + local changes
build_slftp() {
  local name="$1" build_ref="$2" commit rc
  commit=$(win_git rev-parse --short "${build_ref:-HEAD}" | tr -d '\r')
  [ -n "$build_ref" ] || [ -z "$(win_git status --porcelain | tr -d '\r')" ] ||
    commit="$commit + uncommitted changes"
  echo "$commit" > "$work/build-$name.commit"
  step "build $name: ${build_ref:-HEAD} ($commit)"
  rm -rf "$work/build-$name"
  # without its final PASSED line, which would read like the result of the whole run
  bash "$fpc_script" --export "$work/build-$name" $build_ref | sed '/^=== PASSED ===$/d'
  rc=${PIPESTATUS[0]}
  [ $rc -eq 0 ] || [ $rc -eq 10 ] || [ $rc -eq 11 ] || rc=1
  [ $rc -ne 11 ] || fail "another FPC run is active (shared build clone), try again later"
  return $rc
}

# ---- sites: site <n> is container slftp-e2e-glftpd-<n> on port 2120+<n>, slFtp site name E2E<n> ----

site_port() { echo $((2120 + $1)); }

# start_sites: starts E2E1..E2E<sites>, each with the scenario's section directories
start_sites() {
  local i port section t
  step "start $sites glftpd sites"
  stop_sites
  for i in $(seq 1 "$sites"); do
    port=$(site_port "$i")
    docker run -d --name "$image-$i" --network host -e PORT="$port" \
      -e PASV_PORTS="$((49900 + 100 * i))-$((49999 + 100 * i))" "$image_tag" >/dev/null || return
    for section in $sections; do site_exec "$i" "mkdir -m777 /glftpd/site/$section" || return; done
  done
  for i in $(seq 1 "$sites"); do
    t=0
    until curl -s --ssl-reqd -k --user glftpd:glftpd "ftp://127.0.0.1:$(site_port "$i")/" >/dev/null; do
      t=$((t + 1))
      [ $t -lt 50 ] || { fail "site $i does not answer on port $(site_port "$i")"; return; }
      sleep 0.2
    done
    echo "E2E$i: glftpd on 127.0.0.1:$(site_port "$i")"
  done
}

stop_sites() {
  local ids
  ids=$(docker ps -aq --filter "name=^$image-[0-9]+$")
  [ -z "$ids" ] || docker rm -f $ids >/dev/null
}

# site_exec <n> <shell command>: runs the command in the container of site <n>; the site root is
# /glftpd/site, so the FTP path /RACE is /glftpd/site/RACE there
site_exec() { docker exec "$image-$1" sh -c "$2"; }

# ftp_upload <n> <ftp dir> <local dir> <file>...: uploads the files (names relative to <local dir>)
# over TLS in the given order
ftp_upload() {
  local n="$1" ftp_dir="$2" local_dir="$3" list
  shift 3
  list=$(IFS=,; echo "$*")
  (cd "$local_dir" && curl -sS --ssl-reqd -k --user glftpd:glftpd --ftp-create-dirs -T "{$list}" \
    "ftp://127.0.0.1:$(site_port "$n")$ftp_dir/")
}

# site_complete <n> <ftp dir>: true if pzs-ng has marked the release as complete
site_complete() { site_exec "$1" "ls '/glftpd/site$2'" | grep -F -- '- COMPLETE ) -' >/dev/null; }

# site_same_files <n> <m> <ftp dir>: true if <ftp dir> has exactly the same files with the same content
# on both sites (pzs-ng's own entries are ignored). Compare sites, not a site with the uploaded files:
# pzs-ng rewrites the .sfv on upload.
site_same_files() {
  local list="cd '/glftpd/site$3' && find . -maxdepth 1 -type f ! -name '.*' | sort | xargs sha256sum"
  diff <(site_exec "$1" "$list") <(site_exec "$2" "$list")
}

# make_release <dir> <volumes> <MB per volume>: a rar release with random data, .sfv and .nfo
make_release() {
  local dir="$1" i
  rm -rf "$dir" && mkdir -p "$dir" || return
  for i in $(seq 1 "$2"); do
    head -c "$3M" /dev/urandom > "$dir/$(printf 'e2egrp.r%02d' $((i - 1)))" || return
  done
  mv "$dir/$(printf 'e2egrp.r%02d' $(($2 - 1)))" "$dir/e2egrp.rar" &&
    echo 'e2e test release' > "$dir/e2egrp.nfo" &&
    python3 -I - "$dir" > "$dir/e2egrp.sfv" <<'PY'
import os, sys, zlib
d = sys.argv[1]
for f in sorted(os.listdir(d)):
    if f.startswith("e2egrp.r"):
        with open(os.path.join(d, f), "rb") as h:
            print(f"{f} {zlib.crc32(h.read()):08x}")
PY
}

# ---- slFtp console in tmux ----

pane() { tmux capture-pane -p -J -S -3000 -t "$session"; }

# console <text>: types a line into the Admin window, e.g. an IRC command with the ! prefix
console() { tmux send-keys -t "$session" -l -- "$1"; tmux send-keys -t "$session" Enter; sleep 0.7; }

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

# add_sites: adds E2E1..E2E<sites> with AUTH TLS and the scenario's sections at /<section>, and waits
# until slFtp has logged in to all of them
add_sites() {
  local i section
  for i in $(seq 1 "$sites"); do
    console "!addsite E2E$i glftpd glftpd 127.0.0.1:$(site_port "$i")"
    console "!sslmethod E2E$i 3"
    for section in $sections; do console "!setdir E2E$i $section /$section"; done
  done
  console_ok || { pane | grep -B3 -F 'Failed.'; fail 'site setup failed'; return; }
  console '!bnctest'
  for i in $(seq 1 "$sites"); do wait_console "SITE E2E$i IS UP" 1 30 || return; done
}

start_slftp() {
  local section
  rm -rf "$run_dir" && mkdir -p "$run_dir" &&
    cp -r "$work/build-$name/." "$run_dir/" && cp -L "$sqlite_lib" "$run_dir/libsqlite3.so" || return
  # console.pas reads the master password from this file instead of prompting; the first start
  # creates sites.dat with it
  echo e2epass > "$run_dir/masterpass.txt"
  # !setdir only accepts sections that the precatcher knows
  for section in $sections; do
    sed -i "/^\[sections\]$/a $section=$section" "$run_dir/slftp.precatcher" || return
  done
  tmux kill-session -t "$session" 2>/dev/null
  # the trailing echo and sleep keep the pane, so its exit shows; slFtp clears the screen on exit
  tmux new-session -d -s "$session" -x 250 -y 100 \
    "cd '$run_dir' && LD_LIBRARY_PATH=. ./slftp; echo SLFTP-EXITED rc=\$?; sleep 3600" || return
  wait_console 'Command:' 1 30 || { pane; return 1; }
  sleep 3 # until the queue has started
  pane | grep -oE 'slFtp v\S+ .*git# \S+\)' | sort -u
  return 0
}

stop_slftp() {
  sleep 1 # no new lines for a while, so the window is not being redrawn
  { echo '---- console ----'; pane | grep -vE '^\|\s*\|$' | sed 's/  \+/ /g'; } >> "$result"
  console '!die'
  if ! wait_console 'SLFTP-EXITED' 1 60; then
    note 'slFtp did not exit within 60 s after !die, killed it'
    pkill -9 -P "$(tmux display -p -t "$session" '#{pane_pid}')" -x slftp
    sleep 1
  fi
  grep -F '[EXCEPTION]' "$run_dir/slftp.log" | tail -20 | sed 's/^/NOTE: slftp.log: /' | tee -a "$result"
  tmux kill-session -t "$session" 2>/dev/null
}

# ---- results ----

# check <description> <command>...: runs the command and records PASS or FAIL
check() {
  local description="$1"
  shift
  if "$@" >> "$result" 2>&1; then
    echo "PASS: $description" | tee -a "$result"
  else
    echo "FAIL: $description" | tee -a "$result"
    failed=1
  fi
}

# note <text>: records information that is no check, e.g. a timing
note() { echo "NOTE: $*" | tee -a "$result"; }

# seconds_since <EPOCHREALTIME value>: elapsed seconds with two decimals
seconds_since() { awk -v a="$1" -v b="$EPOCHREALTIME" 'BEGIN { printf "%.2f", b - a }'; }

# Runs the scenario with build-<name>; exit code 1 if a check failed
run_scenario() {
  name="$1"
  session="slftp-e2e-$name"
  run_dir="$work/run-$name"
  result="$work/result-$name.txt"
  failed=0
  sites=1
  sections=''
  # shellcheck source=/dev/null
  . "$skill_dir/scenarios/$scenario.sh" || return
  : > "$result"
  step "scenario $scenario with build $name ($(cat "$work/build-$name.commit"))"
  start_sites && start_slftp || { tmux kill-session -t "$session" 2>/dev/null; stop_sites; return 1; }
  steps || { fail "scenario $scenario stopped before its end"; failed=1; }
  stop_slftp
  stop_sites
  echo "Result file: $result"
  return $failed
}

main() {
  check_env && prepare_image || return
  if [ -n "$baseline_ref" ]; then
    build_slftp baseline "$baseline_ref" || return
  fi
  build_slftp change "$ref" || return
  local rc=0
  if [ -n "$baseline_ref" ]; then
    run_scenario baseline || rc=1
  fi
  run_scenario change || rc=1
  return $rc
}

exec 9>"$work/e2e.lock"
if ! flock -n 9; then
  echo "ERROR: another run is active, try again when it has finished"
  [ -z "$run_id" ] || echo 11 > "$exitcode_file"
  exit 11
fi
rm -f "$exitcode_file"
set -o pipefail
main 2>&1 | tee "$log_file"
rc=$?
if [ $rc -eq 0 ]; then
  echo "=== PASSED ==="
else
  echo "=== FAILED with exit code $rc ==="
fi | tee -a "$log_file"
echo $rc > "$exitcode_file"
exit $rc
