# Reproducible race comparison

The GitLab job **performance-race** runs automatically after every push to
**codex/runtime-config-cache** (also on a manually started pipeline for that branch).
It compares the pushed commit against **dev resolved once at job start**. The exact
SHAs and the number of commits unique to each side are retained in commits.txt.
If dev has advanced, synchronize the branch before attributing a difference to
the cache implementation.

The job uses an isolated Docker-in-Docker daemon on the Linux SaaS runner. A worker
derived from the project's FPC CI image builds both source snapshots with the same
toolchain. No token, Git credential file or .git directory is copied into images.
The pinned glftpd+pzs-ng fixture and the console helpers come from glftpd-e2e.
The existing FPC and Delphi build/unit-test jobs still run alongside this job.
The performance comparison itself currently measures the **FPC/Linux** binaries.

## Workload

- Four real glftpd sites, TLS on control and data connections and SSL FXP.
  The fixture allows 100 concurrent site logins (50 for the test user), so its
  packaged 15-user site limit does not reject the progressive upload producers.
- Each trial uses a fresh block of 3,000 passive ports per site, avoiding
  TIME_WAIT leftovers from earlier trials. The uploader retries only temporary
  PASV 425 responses, before sending STOR; retry counts are kept in each result.
- Three progressive root releases with six deterministic 512-KiB volumes each,
  and three one-volume CD1 releases. All 63 destination payloads are checked.
  This is a bounded CI workload calibrated against dev, not a saturation/soak test.
- Releases are announced one second apart through a local IRC protocol fixture
  and slFtp's actual precatcher. Files arrive progressively over FTPS.
  A real COMPLETE announce follows each finished source upload.
  Setup explicitly enables the autotrader with !auto 1 and checks its confirmation;
  a fresh profile otherwise ignores automatic announcements.
- E2E2 already has some of the destination directories.
- E2E3 has one slot and the highest fixed route rank; other sites have six slots.
- E2E4 has a transparent TCP proxy that delays server-to-client control chunks by
  25 ms and drops control connections for six seconds. This preserves end-to-end
  TLS, including session reuse. It is not a precise WAN RTT or bandwidth emulator.
  The other three sites use the original direct FTP connection path.
- A recovery preflight drops E2E4's control link for six seconds, marks the site down
  and verifies the DOWN -> UP transition via bnctest before timing starts. New
  races must then transfer on that site. This checks reconnect and new work;
  resuming an already-running race across the outage is a separate correctness case.
- The order is dev / branch / branch / dev. Every trial resets sites, databases,
  runtime files and caches. Both binaries use the same configuration from the
  pushed commit, including its normal connection/I/O timeout values.

Each trial has a 150-second observation deadline, excluding preparation and shutdown.
Upload workers are daemon threads, so blocked uploads cannot extend process shutdown.
An outer 360-second process timeout also bounds setup and unexpected blocking I/O.
The job has a 30-minute limit to also bound first-time image downloads and builds.
No local Windows/WSL installation is needed for the automatic comparison.

## Results

The multi-volume CD1 scenario is executable with scenario `progressive-cd1`. Its original
reproduction is in
[job 17915499576](https://gitlab.com/slftp/slftp/-/jobs/17915499576). On `dev`
`0de97be2`, each of the three targets received only the first volume of each
progressively uploaded CD1 release; all root volumes arrived. `TDirList.Complete`
has a fallback which can mark an SFV-bearing subdirectory complete as soon as it
has a useful file. Keep this correctness case separate from the timing baseline;
the one-volume CD1 case still exercises real nested directory creation and listing.

An active control outage also left already announced releases incomplete on E2E4
in [job 17915686029](https://gitlab.com/slftp/slftp/-/jobs/17915686029). The other
two targets completed all their files. The executable `active-recovery` scenario repeats this class of failure by dropping the
control link during active work, without re-announcing releases. Both reproduction
scenarios use the same A/B/B/A runner and correctness checks, separately from `timing`.

Open the performance-race job. Its log prints the comparison; download its
perf-results artifacts for:

- summary.md: medians, min/max ranges, relative differences and per-site latency.
- comparison.json: exact SHAs, all trials and individual file timelines.
- junit.xml: correctness outcomes visible in GitLab's test results.
- Per-trial result.json, workload.log, console.txt and slftp.log; build logs and
  run.log for preparation, execution and shutdown diagnostics.

Measured values: elapsed time, producer duration, drain time after the producer,
payload files/s and MiB/s, slFtp CPU milliseconds per
payload file, peak resident memory, p95 source-ready to destination-appearance delay
and p95 payload completion time (all expected bytes, not the STATS announcement).
The timing interval ends only after all producers finished and all expected full-size
files remained unchanged for at least 500 ms. CPU uses the same interval, including
this stability window. Observed deletion, truncation or replacement resets the affected
file timestamps; verification rejects files that changed after measurement. Changes
entirely between samples can still be missed. This is not confirmation of slFtp task
teardown, FTP acknowledgement processing or the STATS event.
The filesystem observer targets 50-ms sampling;
each result includes the actual maximum sampling gap. CPU sampling reads only the
slFtp process. Hash checks happen after the measured interval.

Incorrect/missing payloads, timeouts or an unavailable environment fail the job
and invalidate the speed comparison. Functional trial failures still allow the other
variant to run with fresh sites. Setup/build failures stop the run and leave unrun
trials marked skipped in JUnit; the overall comparison still fails. Malformed results
are reported as failures without suppressing the summary or JUnit report.
Performance deltas alone do not fail it.
The unit tests of the report calculation run in the worker before the load.

The producer's pacing can limit throughput; this is a scheduling/
CPU comparison, not a maximum network bandwidth benchmark. Actual source availability
depends on upload completion and host load; compare producer duration and source timestamps
as well as throughput. Shared CI hardware is
noisy, so repeat before treating small changes as regressions. This fixture covers
glftpd, not DrFTPD slave/PRET behavior, external metadata services or long-term leaks.
Internal task creation/assignment, queue wakes/sorts, locks and blocked reasons
need a separate instrumented run; the external delays do not claim to measure them.

## Run on Linux

Use a Linux x86-64 machine with Git, Bash and rootful Docker Engine. The account must
be able to run Docker and start a privileged Docker-in-Docker container. Use a dedicated
test machine/VM. The worker never receives the host Docker socket, and no daemon TCP
port is published. All site volumes and fixed container names belong to the nested
daemon, which is removed at the end.

From a checkout containing these scripts:

```sh
git fetch origin
bash tests/perf/linux.sh origin/dev HEAD
```

Only committed snapshots are tested. The baseline defaults to the local `origin/dev`
ref and the candidate to `HEAD`; fetching is explicit. For reproducible follow-ups,
pass the two commit hashes recorded by the previous run instead of moving refs.
An optional third argument names a new result directory. Existing directories are
rejected. The default is `perf-results-<timestamp>` in the current directory.

Separate correctness reproductions:

```sh
bash tests/perf/linux.sh origin/dev HEAD ./perf-cd1 progressive-cd1
bash tests/perf/linux.sh origin/dev HEAD ./perf-recovery active-recovery
```

Results, including failed trials, are exported before cleanup. Inspect `summary.md`,
`comparison.json`, `junit.xml`, `driver.log` and each trial's logs. Exit code 0 means
all trials were valid; a performance improvement is not required. A nonzero exit
means the comparison cannot be used as a speed claim. Repeat complete runs to assess
noise; two samples per variant remain insufficient for small differences.

Do not invoke `run.sh` directly on a host. It requires the worker marker and mounted
disposable site volumes. `ci.sh` is the GitLab/Alpine entry point; `linux.sh` prepares
the equivalent isolated environment without GitLab credentials.

The report/observer tests require Python 3.9 or newer:

```sh
python3 -m unittest discover -s tests/perf -p 'test_*.py'
```

A dedicated Linux GitLab Runner can execute the same job after each push: use the
Docker executor with privileged Docker-in-Docker support, assign a unique runner tag,
and set that tag on `performance-race`. The job has `needs: []` because it builds both
snapshots itself. For a manual reproduction pipeline set `PERF_SCENARIO` to
`progressive-cd1` or `active-recovery`; the default is `timing`.

## Implementation

ci.sh packages the two Git snapshots and copies them into a disposable worker.
run.sh reuses glftpd-e2e as a library for pinned server images and console control.
workload.py generates the fixture, drives FTPS/IRC and samples process/filesystem
state. report.py rejects invalid trials and computes the paired report.
test_report.py covers units, percentile tails and invalid-comparison handling.
No production Pascal code or new library is required.
