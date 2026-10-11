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
| `source_full`, `destination_full`, `destination_lock`, `busy_cache`, `source_limit`, `destination_limit`, `active_file`, `cooldown`, `dependency` | Counts at selected blocking checks, across both direct assignment and scheduler execution. These are repeated checks, not distinct tasks or an exhaustive partition of all attempts. |
| `dependency_changed_during_registration` | Readiness was false before subscription and true immediately afterwards. Shows the check/subscribe race; another pending signal may still cover it. The diagnostic read does not retry or wake the task. |
| `delayed_race_insertions`, `initial_race_delay_sum_ms` | Race tasks inserted with future startat, and total initial delay. Divide sum by count for the mean configured initial delay observed. |
| `completed_races`, `completed_declared_bytes` | Successfully sent files counted at queue cleanup; dupes and aborted tasks are excluded. Bytes use the final file-size metadata, not measured socket bytes; resume traffic and incomplete files can differ. |
| `*_n`, `*_sum_us`, `*_max_us` | Duration count, total and maximum in microseconds. Average milliseconds = sum_us / n / 1000, when n is nonzero. |
| `pass_*`, `queue_lock_wait_*`, `sort_*`, `cleanup_*`, `console_add_*` | Time spent in a pass, waiting for the main queue lock at insertion/dispatch, sorting, cleanup and console insertion respectively. Pass time includes blocking and is not CPU time. Nested timings overlap; do not add them together. |
| `source_refill_*`, `destination_refill_*` | Time from a slot's most recent release to its next race assignment. Includes legitimate lack of work or limits; the first assignment of a never-released slot is excluded. Destination samples belong to the assigning source queue. |

Completed files/s = completed_races * 1000 / window_ms.
Declared MiB/s = completed_declared_bytes * 1000 / window_ms / 1048576.

Capture several comparable races, including small files, large files and multiple
sources competing for one destination. Preserve both the diagnostic log and the
usual race statistics. No filenames, release names or credentials are added to
diagnostic records; site names are included.

## Disable or remove

For a comparison without measurement overhead, comment out the single
`{$DEFINE QUEUE_DIAGNOSTICS}` line in `queuediagnostics.inc` and rebuild all units.
Alternatively define `DISABLE_QUEUE_DIAGNOSTICS` globally for the compiler.
The unit, fields, counters and hooks are then compiled out. The diagnostic build
does add clock reads, atomic signal increments, one short queue-lock acquisition
after waits, and bounded snapshots/logging. Do not treat its throughput as an
uninstrumented benchmark or silently assume measurement overhead is zero.

All instrumentation, tests and this document are kept in a dedicated diagnostic
commit. Use the original feature branch for the implementation, or revert the
diagnostic commit if it was cherry-picked. No Makefile, CI, sample-config or
vendored-library changes are required.
