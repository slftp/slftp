# Temporary queue diagnostics

Branch `codex/task-triggered-queue-diagnostics` adds measurement hooks to
`codex/task-triggered-queue`. It does not fix or change scheduling decisions.
The diagnostic build is enabled by `queuediagnostics.inc`; the normal feature
branch does not contain these files or hooks. CI release binaries on this
diagnostic branch include the measurements.

## Enable the output

Use the same sites, configuration and release build as the comparison run.
Set these values in your runtime configuration (do not commit credentials):

```ini
[debug]
verbosity=1
categories=queue-diag
flushlines=0
```

`categories=verbose` also includes these records, but logs other sections too.
Each initialized site writes one `[QUEUE-DIAG]` record to `slftp.log` after at
least ten seconds, on its next existing queue pass. Idle queues can therefore
report every fifteen seconds. There is no new timer, thread or wake for reporting.
Counters reset after each record. Use the actual `window_ms`, not a fixed ten
seconds, when calculating rates. A final partial window is not flushed on exit.

## Read the measurements

| Fields | Interpretation |
| --- | --- |
| `passes`, `wake_event`, `wake_deadline`, `wake_fallback` | Actual queue activity and why waits returned. Fallback means the 15-second wait cap expired; a distant deadline may also be capped this way. |
| `signal_*` | Signal requests by origin. Several requests can produce one actual event wake. `external` includes existing batch, start/stop and MKDIR calls. |
| `tasks`, `waiting`, `free_slots`, `due_unassigned`, `oldest_due_ms` | Snapshot at report time. Due tasks may still be blocked; free slots can be offline or limited by upload/download caps. Age starts at task creation, including intentional delays. |
| `fallback_with_free_slot_and_due_work` | Number of fallback returns where free slots and due unassigned work coexist. Investigate missing wakes or readiness changes; this alone does not prove runnable work was ignored. |
| `sorts`, `sort_requests`, `inserted`, `inserted_assigned`, `inserted_wait`, `promoted` | Sort runs versus explicit requests and insertions. Already assigned tasks and internal wait tasks still dirty the current queue; many sorts per transferred file identify this overhead. |
| `assign_attempts`, `assigned_races`, `retry_delay` | Assignment attempts, successful race assignments and hits of the maxassign threshold. Capacity/dependency waits no longer consume this threshold: only assignment exceptions count. `retry_delay` includes the remove/finish action when maxassign_delay is zero. |
| `zero_waits`, `future_zero_waits` | Uncapped waits calculated as zero milliseconds. A deadline crossed during processing can legitimately cause one zero wait. A future deadline must never produce one; `future_zero_waits` must stay zero. |
| `clock_mode` | `default_rtl` is the normal Linux/FPC build; `nopatchrtl` disables mORMot RTL patches globally. Other platforms report `native`. The label describes the build, not a measured clock resolution. |
| `early_timeouts`, `early_timeout_missing_sum_us`, `wait_errors` | A positive timeout returned before its requested duration, allowing 100 us or 0.1 percent tolerance, whichever is larger. Signals and zero waits are excluded. Missing time is accumulated only for these early returns. Errors include abandoned and other unexpected wait results. |
| `wait_*`, `timeout_requested_*`, `timeout_elapsed_*` | `wait` measures every event wait, excluding the diagnostic lock afterwards. Requested and elapsed timeout samples cover the same positive `wrTimeout` returns, after the wait cap was applied. Compare their averages; signaled waits may legitimately be shorter. |
| `clock_lag_*` | Linux/FPC only: lower bound on how far the RTL's `CLOCK_MONOTONIC` reading trails a direct libc reading of that same clock. A coarse-clock redirect can produce a positive lag. Zero can mean no lag or elapsed measurement overhead; unavailable/error samples are excluded. |
| `source_full`, `destination_full`, `destination_lock`, `busy_cache`, `source_limit`, `destination_limit`, `active_file`, `cooldown`, `dependency` | Counts at selected blocking checks, across both direct assignment and scheduler execution. These are repeated checks, not distinct tasks or an exhaustive partition of all attempts. |
| `dependency_changed_during_registration` | Readiness was false before subscription and true immediately afterwards. Shows the check/subscribe race; another pending signal may still cover it. The diagnostic read does not retry or wake the task. |
| `delayed_race_insertions`, `initial_race_delay_sum_ms` | Race tasks inserted with future startat, and total initial delay. Divide sum by count for the mean configured initial delay observed. |
| `completed_races`, `completed_declared_bytes` | Successfully sent files counted at queue cleanup; dupes and aborted tasks are excluded. Bytes use the final file-size metadata, not measured socket bytes; resume traffic and incomplete files can differ. |
| `*_n`, `*_sum_us`, `*_max_us` | Duration count, total and maximum in microseconds. Average milliseconds = sum_us / n / 1000, when n is nonzero. |
| `pass_*`, `queue_lock_wait_*`, `sort_*`, `cleanup_*`, `console_add_*` | Time spent in a pass, waiting for the main queue lock at insertion/dispatch, sorting, cleanup and console insertion respectively. Pass time includes blocking and is not CPU time. Nested timings overlap; do not add them together. |
| `source_refill_*`, `destination_refill_*` | Time from a slot's most recent release to its next race assignment. Includes legitimate lack of work or limits; the first assignment of a never-released slot is excluded. Destination samples belong to the assigning source queue. |

Completed files/s = completed_races * 1000 / window_ms.
Declared MiB/s = completed_declared_bytes * 1000 / window_ms / 1048576.

## Diagnose individual pending tasks

The existing report now adds `blocked_<reason>` snapshot counts for pending tasks,
including delayed tasks separately as `blocked_planned_delay`. These are task
counts, unlike repeated blocking-check counters such as `dependency`/`busy_cache`.
The reason is the last observed first failed check, not every simultaneous blocker.
`not_checked` means dispatch has not yet checked the task; `unclassified` marks
an assignment exit without a more specific diagnostic result. `check_age_ms`
shows how old the last observation is. Source saturation can stop iteration;
the report then labels otherwise unchecked tasks `source_slots_full`.

At most three longest examples (pending and terminal combined) are emitted per
site/window, once at least two seconds of pending time have been observed.
`block_samples_omitted` counts additional sample candidates, not distinct
omitted tasks over the lifetime of the process. Short-lived or replaced examples
may be absent. Existing reports/wakes drive logging; no timer is added.

`[QUEUE-BLOCK]` identifies a pending task by UID, type, source and destination.
Reasons distinguish MKDIR/dependency, source/destination capacity, absence of an
online slot, transfer limits, cooldown, active transfer, destination lock/cache,
dirlist concurrency, unavailable requested slot and planned delay.
`[QUEUE-RESUME]` records `outcome=assigned` or `outcome=removed` before the task
can be freed. Removal is not a successful transfer. Terminal samples are bounded,
immutable and may be emitted at the next report, not at the moment of completion.
`observed_at_us` is their monotonic sample time; no task pointer is retained.

`blocked_ms` starts at observation/insertion and excludes recorded planned-delay
segments. It is a lower-bound observation of pending time, not proof of runnable
work. `task_age_ms` starts at creation, while `planned_remaining_ms` describes
the current intentional delay. `reason_ms` is time in the current reason;
`previous_reason` and `changes` show transitions. Reason changes retain the last
relevant event so a subsequent different blocker does not erase release evidence.

`cache_reason` preserves the cause remembered by the destination cache;
`cache_retry_remaining_ms` and `destination_revision` describe its last observed
expiry and revision. `limit_used`/`limit_max` describe the actual failing limit,
or -1 if not applicable. Free-slot counts include offline slots; an online-slot
reason is based on the actual slot search, not inferred from those counts.

`event_kind=dependency_ready` is a producer timestamp from a dirlist's transition
to the existing readiness predicate (`not need_mkdir or error`). Error readiness
does not imply a successful MKDIR. `source_notification`/`destination_notification`
are the latest site notification timestamps, not proof of capacity or all-task
readiness. `event_reason` links an event to the reason it was observed against.
`event_to_check_ms` measures its first subsequent task check; newer relevant
notifications can replace the stored event. `check_to_assign_ms` spans from that
check to assignment and may include additional legitimate blocking. -1 means
no event measurement is available; it must not be treated as zero delay.

The detail lines use the same `queue-diag` category and verbosity as the existing
summary. There are no release/file names or credentials. Hooks, inline task state,
dirlist timestamps and cache cause fields compile out with
`DISABLE_QUEUE_DIAGNOSTICS`, using the existing single switch.

Capture several comparable races, including small files, large files and multiple
sources competing for one destination. Preserve both the diagnostic log and the
usual race statistics. No filenames, release names or credentials are added to
diagnostic records; site names are included.

## Verify the Linux clock correction

The ordinary CI binary now preserves the clock ID in mORMot's FPC clock redirect
and retains the libc/vDSO fast path and other RTL optimizations. Earlier builds
substituted a coarse clock: the CI probe returned early on all 500 waits of 1 ms,
and live logs also showed positive timeout returns before their requested duration.
The additional `buildlinux64-queue-clock-comparison` job runs a standalone probe
twice and publishes its logs plus `slftp_x64-<commit>-nopatchrtl` as artifacts.
Both binaries use this same commit and measurement code. The probe creates only
an unsignaled event: 500 waits of 1 ms, then 100 waits of 5 ms, without FTP, site
configuration or credentials. Early returns are observations, not assertion
failures; unexpected event results make the probe fail.

For a local probe (rebuilds all units to avoid mixed compiler flags):

```sh
make queue_wait_probe
./tests/queuewaitprobe
make queue_wait_probe CFLAGS="-MDelphi -O3 -Xs -Ct- -dNOPATCHRTL"
./tests/queuewaitprobe
```

Run the normal binary and then the `nopatchrtl` comparison binary with the same
configuration and comparable releases, one process at a time. Preserve both
logs. After the clock correction, both builds should show no systematic clock lag
or early timeout returns in the probe. Check whether the high
`wake_deadline`/`passes` rates also fall during real work. If early timeouts remain,
preserve the requested/elapsed timings and clock lag for further investigation.

`NOPATCHRTL` disables other mORMot RTL optimizations too, so a throughput change
alone does not isolate this clock problem. The clock/wait observations and the
network-free probe are the relevant evidence. The default build contains the
targeted production correction; the broad `NOPATCHRTL` flag remains a diagnostic
comparison only. Permanent regression coverage is in `tests/slclockTests.pas`.

## Disable or remove

For a comparison without measurement overhead, comment out the single
`{$DEFINE QUEUE_DIAGNOSTICS}` line in `queuediagnostics.inc` and rebuild all units.
Alternatively define `DISABLE_QUEUE_DIAGNOSTICS` globally for the compiler.
The unit, fields, counters and hooks are then compiled out. The diagnostic build
does add clock reads, atomic signal increments, one short queue-lock acquisition
after waits, and bounded snapshots/logging. Do not treat its throughput as an
uninstrumented benchmark or silently assume measurement overhead is zero.

Use the original feature branch for the implementation. Preserve the clock fix
and its permanent regression tests when removing diagnostics. The clock comparison introduces
one standalone probe, the `queue_wait_probe` Makefile target, and one CI job;
remove these together with the diagnostic hooks. The production clock fix in the
vendored library must remain. No sample-config changes are required. Diagnostic
clock reads run only in diagnostic builds.
