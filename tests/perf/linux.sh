#!/bin/bash
# Native Linux entry point. All fixture containers/volumes live in a disposable nested daemon.
set -euo pipefail
if [ "${1:-}" = --help ]; then
  echo 'Usage: bash tests/perf/linux.sh [baseline-ref=origin/dev] [candidate-ref=HEAD] [new-output-dir] [timing|progressive-cd1|active-recovery]'
  echo 'Uses committed snapshots only. Fetch refs first. Requires Linux x86-64 and rootful Docker.'
  exit 0
fi
[ "$(uname -s)" = Linux ] && [ "$(uname -m)" = x86_64 ] || {
  echo 'ERROR: this benchmark requires Linux x86-64'; exit 1;
}
scenario=${4:-timing}
case "$scenario" in timing|progressive-cd1|active-recovery) ;; *) echo "Invalid scenario: $scenario"; exit 1;; esac
baseline=$(git rev-parse --verify "${1:-origin/dev}^{commit}")
branch=$(git rev-parse --verify "${2:-HEAD}^{commit}")
docker info >/dev/null
out=${3:-"$PWD/perf-results-$(date +%Y%m%d-%H%M%S)"}
mkdir "$out"
out=$(realpath "$out")
exec > >(tee "$out/driver.log") 2>&1
scratch=$(mktemp -d)
daemon_id=''
network_id=''
d() { docker exec "$daemon_id" docker "$@"; }
cleanup() {
  rc=$?
  trap - EXIT
  trap '' INT TERM
  if [ -n "$daemon_id" ]; then
    docker exec "$daemon_id" mkdir -p /tmp/perf-results || true
    if d cp slftp-perf-worker:/work/results/. /tmp/perf-results/; then
      docker cp "$daemon_id:/tmp/perf-results/." "$out/" || rc=1
    elif [ "$rc" -eq 0 ]; then
      rc=1
    fi
    docker rm -fv "$daemon_id" >/dev/null || true
  fi
  if [ -n "$network_id" ]; then docker network rm "$network_id" >/dev/null || true; fi
  rm -rf -- "$scratch"
  echo "Results: $out (exit $rc)"
  exit "$rc"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
printf 'branch=%s\nbaseline=%s\nscenario=%s\n' "$branch" "$baseline" "$scenario" > "$out/commits.txt"
git rev-list --left-right --count "$baseline...$branch" >> "$out/commits.txt"
git archive "$baseline" > "$scratch/baseline.tar"
git archive "$branch" > "$scratch/branch.tar"
mkdir "$scratch/image"
git show "$branch:tests/perf/Dockerfile" > "$scratch/image/Dockerfile"
network_id=$(docker network create "slftp-perf-${scratch##*/}")
# No host Docker socket or TCP port is exposed to the worker.
daemon_id=$(docker run -d --privileged --network "$network_id" \
  -e DOCKER_TLS_CERTDIR= docker:27.5.1-dind --tls=false)
for i in $(seq 1 60); do
  if d info >/dev/null 2>&1; then break; fi
  sleep 1
done
d info >/dev/null || { docker logs "$daemon_id"; exit 1; }
docker exec "$daemon_id" mkdir -p /tmp/perf
docker cp "$scratch/." "$daemon_id:/tmp/perf/"
d build --build-arg "CI_IMAGE=${SLFTP_CI_IMAGE:-registry.gitlab.com/slftp/ci-images/ubuntu-lts}" \
  -t slftp-perf-worker /tmp/perf/image
d create --name slftp-perf-worker --network host \
  -e DOCKER_HOST=tcp://127.0.0.1:2375 -e DOCKER_TLS_CERTDIR= \
  -e SLFTP_PERF_WORKER=1 -e PERF_SCENARIO="$scenario" \
  -v perf-site-1:/sites/1 -v perf-site-2:/sites/2 \
  -v perf-site-3:/sites/3 -v perf-site-4:/sites/4 slftp-perf-worker sleep infinity
d start slftp-perf-worker
d cp /tmp/perf/baseline.tar slftp-perf-worker:/work/baseline.tar
d cp /tmp/perf/branch.tar slftp-perf-worker:/work/branch.tar
d exec slftp-perf-worker sh -c 'mkdir baseline branch; tar -xf baseline.tar -C baseline; tar -xf branch.tar -C branch'
d exec slftp-perf-worker bash /work/branch/tests/perf/run.sh "$baseline" "$branch"
