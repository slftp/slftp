#!/bin/sh
# Runs only in the disposable Docker-in-Docker service of this CI job.
set -eu
apk add --no-cache git
printf '%s' "$CI_REGISTRY_PASSWORD" | docker login -u "$CI_REGISTRY_USER" --password-stdin "$CI_REGISTRY"
mkdir -p perf-results
git fetch --no-tags origin dev
baseline=$(git rev-parse FETCH_HEAD)
branch=$(git rev-parse HEAD)
printf 'branch=%s\nbaseline=%s\n' "$branch" "$baseline" > perf-results/commits.txt
git rev-list --left-right --count "$baseline...$branch" >> perf-results/commits.txt
scratch=$(mktemp -d)
git archive "$baseline" > "$scratch/baseline.tar"
git archive "$branch" > "$scratch/branch.tar"
docker build --build-arg CI_IMAGE="$SLFTP_CI_IMAGE" -t slftp-perf-worker -f tests/perf/Dockerfile tests/perf
for n in 1 2 3 4; do docker volume create "perf-site-$n" > /dev/null; done
docker create --name slftp-perf-worker --network host \
  -e DOCKER_HOST=tcp://127.0.0.1:2375 -e DOCKER_TLS_CERTDIR= \
  -v perf-site-1:/sites/1 -v perf-site-2:/sites/2 \
  -v perf-site-3:/sites/3 -v perf-site-4:/sites/4 \
  slftp-perf-worker sleep infinity > /dev/null
cleanup() { docker rm -f slftp-perf-worker > /dev/null 2>&1 || true; }
trap cleanup EXIT INT TERM
docker start slftp-perf-worker > /dev/null
docker cp "$scratch/baseline.tar" slftp-perf-worker:/work/baseline.tar
docker cp "$scratch/branch.tar" slftp-perf-worker:/work/branch.tar
docker exec slftp-perf-worker sh -c 'mkdir baseline branch; tar -xf baseline.tar -C baseline; tar -xf branch.tar -C branch'
rc=0
docker exec slftp-perf-worker bash /work/branch/tests/perf/run.sh "$baseline" "$branch" || rc=$?
docker cp slftp-perf-worker:/work/results/. perf-results/ || rc=1
cat perf-results/summary.md 2>/dev/null || true
exit "$rc"
