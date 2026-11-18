#!/bin/bash
# Runs a local UnrealIRCd test server in a Docker container on Linux or in WSL, with test clients inside
# the container, see SKILL.md next to this file. On Windows, ircd.ps1 calls this script.
#
#   ircd.sh build             builds the image unless it exists for the current inputs
#   ircd.sh start             builds if needed and starts the server, waits until it answers
#   ircd.sh stop              removes the container, and with it the test clients
#   ircd.sh status            prints whether the server runs; exit code 1 if not
#   ircd.sh client <nick> <channel> [<blowkey>]  starts a test client, waits until it is in the channel
#   ircd.sh say <nick> <text>   the client says the text in its channel
#   ircd.sh raw <nick> <line>   the client sends the IRC line
#   ircd.sh feed <nick> <file>  the client says each line of the file, all in one burst
#   ircd.sh wait <nick> <text> [<count>] [<seconds>]  waits until <count> lines (default 1) of the
#                             client's log contain the text (a fixed string) within <seconds> (default 30)
#   ircd.sh log <nick>        prints what the client received
#   ircd.sh --initialized     exit code 0 if the image exists (else start builds it, slow)
#   ircd.sh --id <id> build   for wsl-run.ps1: write the exit code to a file of its own for run <id>
#   ircd.sh --clear | --status | --summary  see .agents/scripts/run-protocol.sh
#
# Everything on disk lives in ~/slftp-unrealircd (download, build context, log and exit code). Exit
# codes: 0 = done, 1 = failed or the text did not show up in time, 10 = environment not set up,
# 11 = another build is active.

skill_dir="$(cd "$(dirname "$0")" && pwd)"
work="$HOME/slftp-unrealircd"
log_file="$work/build.log"
exitcode_file="$work/build.exitcode"
image=slftp-unrealircd
container=slftp-unrealircd
setup_doc=.agents/skills/unrealircd/SKILL.md
summary_pattern='^=== |^ERROR|error:'
# shellcheck source=../../scripts/run-protocol.sh
. "$skill_dir/../../scripts/run-protocol.sh"

# pinned third-party software; change these on purpose and run the skill once to check
ircd_url=https://www.unrealircd.org/downloads/unrealircd-6.2.7.tar.gz
ircd_sha256=e075375f469309bf1d54134bc914ec50a4ae3b09da6d2db2c11f9fa88d649370
# one image per state of its inputs, so an unchanged image needs no build
image_tag="$image:$({ echo "$ircd_url $ircd_sha256"; cat "$skill_dir"/server/*; } | sha256sum | cut -c1-12)"

fail() { echo "ERROR: $*"; return 1; }

check_env() {
  command -v docker >/dev/null || { fail "docker missing, see $setup_doc"; return 10; }
  docker info >/dev/null 2>&1 || { fail "docker not usable by $(id -un), see $setup_doc"; return 10; }
}

# Downloads the sources (verified by checksum) and builds the image unless it exists for the current
# inputs, then removes the images of older inputs and the build stages. The sources are only extracted
# here; their code runs inside the build. Not quiet, so that the first build, about 3 minutes, shows that
# it advances.
build() {
  local tgz="$work/dl/${ircd_url##*/}" context="$work/image"
  check_env || return
  echo "=== image $image_tag ==="
  if docker image inspect "$image_tag" >/dev/null 2>&1; then echo 'up to date'; return 0; fi
  mkdir -p "$work/dl" || return
  if ! { [ -f "$tgz" ] && echo "$ircd_sha256  $tgz" | sha256sum -c --quiet; }; then
    command -v curl >/dev/null || { fail "curl missing, see $setup_doc"; return 10; }
    curl -sSfo "$tgz.part" "$ircd_url" || fail "download of $ircd_url failed" || return
    echo "$ircd_sha256  $tgz.part" | sha256sum -c --quiet || fail "wrong checksum of $ircd_url" || return
    mv "$tgz.part" "$tgz" || return
  fi
  rm -rf "$context" && mkdir -p "$context/unrealircd" || return
  tar -xzf "$tgz" -C "$context/unrealircd" --strip-components=1 && cp "$skill_dir"/server/* "$context/" || return
  docker build --force-rm -t "$image_tag" "$context" || fail 'image build failed' || return
  rm -rf "$context"
  stop_server # a stopped container, e.g. of a failed start, would keep its image
  docker images "$image" --format '{{.Repository}}:{{.Tag}}' | grep -vxF "$image_tag" |
    xargs -r docker rmi >/dev/null
  docker image prune -f --filter label=slftp-unrealircd >/dev/null
}

running() { [ "$(docker inspect -f '{{.State.Running}}' "$container" 2>/dev/null)" = true ]; }

stop_server() { docker rm -f "$container" >/dev/null 2>&1; return 0; }

# port_in_use <port>: true if something answers on the port of 127.0.0.1
port_in_use() { (exec 3<>"/dev/tcp/127.0.0.1/$1") 2>/dev/null; }

# start_server: a fresh server on the host network, so that the ports are on 127.0.0.1. The wait below
# can't tell it from another program on these ports, which would make the server exit. A missing image is
# built by the command build, which holds the build lock, unlike the function.
start_server() {
  local t=0 port
  docker image inspect "$image_tag" >/dev/null 2>&1 || bash "$0" build || return
  stop_server
  for port in 6667 6697; do
    if port_in_use "$port"; then fail "port $port is in use by another program"; return; fi
  done
  docker run -d --name "$container" --network host "$image_tag" >/dev/null || return
  until docker exec "$container" python3 -c \
      'import socket; [socket.create_connection(("127.0.0.1", p)).close() for p in (6667, 6697)]' 2>/dev/null; do
    t=$((t + 1))
    if [ $t -ge 50 ] || ! running; then
      docker logs --tail 30 "$container" 2>&1
      fail 'the server did not start, see above; are the ports 6667 and 6697 in use?'
      return
    fi
    sleep 0.2
  done
  echo 'UnrealIRCd on 127.0.0.1: 6667 (plain), 6697 (TLS)'
}

# wait_log <nick> <text> <count> <seconds>
wait_log() {
  local end=$((SECONDS + $4))
  # the count is empty, so the test fails, as long as the log doesn't exist
  until [ "$(docker exec "$container" grep -cF -- "$2" "/tmp/$1.log" 2>/dev/null)" -ge "$3" ] 2>/dev/null; do
    [ $SECONDS -lt $end ] || { echo "TIMEOUT after $4 s waiting for '$2' x$3 in the log of $1"; return 1; }
    sleep 0.2
  done
}

# to_client <nick>: passes stdin to the command FIFO of the client. The FIFO of a client that has ended
# has no reader, so the timeout ends the wait for one.
to_client() {
  docker exec -i "$container" timeout 5 sh -c '[ -p "$1" ] && cat > "$1"' _ "/tmp/$1.in" ||
    fail "client $1 is not running"
}

# client_running <nick>: true if a client reads the FIFO; without a reader, opening it blocks
client_running() {
  docker exec "$container" timeout 1 sh -c '[ -p "$1" ] && : > "$1"' _ "/tmp/$1.in" 2>/dev/null
}

# start_client <nick> <channel> [<blowkey>]: a second client with the same nick would empty the log of
# the first one and take its commands. A client that didn't get into the channel ends again, also if
# the server refused its nick, so that nothing is left to take the commands of a later one.
start_client() {
  # slFtp repeats a shorter key and the client doesn't; the Blowfish of the client takes at most 56 bytes
  local key_len
  key_len=$(printf %s "$3" | wc -c)
  if [ -n "$3" ] && { [ "$key_len" -lt 8 ] || [ "$key_len" -gt 56 ]; }; then
    fail 'a blowkey needs 8 to 56 bytes'
    return
  fi
  client_running "$1" && { fail "client $1 is already running"; return; }
  docker exec -d "$container" python3 /ircclient.py "$@" || return
  wait_log "$1" JOINED 1 10 && return
  printf 'raw QUIT\n' | to_client "$1"
  return 1
}

# Arguments that a shell in between has split or stripped must not pass as shorter text.
bad_args() {
  fail "wrong arguments for $1, see the top of $0; on Windows, text with double quotes needs feed"
  exit 1
}

mkdir -p "$work"
run_query "$@"
case "$1" in
  --initialized) docker image inspect "$image_tag" >/dev/null 2>&1; exit ;;
  --id) run_id="$2"; exitcode_file="$exitcode_file.$2"; shift 2 ;;
esac

case "$1" in
  build) run_locked "$work/build.lock" build ;;
  start) start_server ;;
  stop) stop_server ;;
  status) if running; then echo running; else echo 'not running'; exit 1; fi ;;
  client) start_client "$2" "$3" ${4:+"$4"} ;;
  say) [ $# -eq 3 ] || bad_args "$1"; printf 'say %s\n' "$3" | to_client "$2" ;;
  raw) [ $# -eq 3 ] || bad_args "$1"; printf 'raw %s\n' "$3" | to_client "$2" ;;
  # $a\ ends a last line without a line break, which the client would not process
  feed)
    [ $# -eq 3 ] || bad_args "$1"
    # checked here, as the exit code of the pipe is the one of to_client
    [ -f "$3" ] && [ -r "$3" ] || { fail "cannot read the file $3"; exit 1; }
    sed -e 's/^/say /' -e '$a\' "$3" | to_client "$2" ;;
  wait)
    [ $# -ge 3 ] && [ $# -le 5 ] || bad_args "$1"
    case "${4:-1}${5:-30}" in *[!0-9]*) bad_args "$1" ;; esac
    wait_log "$2" "$3" "${4:-1}" "${5:-30}" ;;
  log) docker exec "$container" cat "/tmp/$2.log" ;;
  *) echo "usage: see the top of $0"; exit 1 ;;
esac
