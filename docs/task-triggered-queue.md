# Task-triggered queue

Each site keeps its existing queue thread and auto-reset event. The main thread no
longer fires all queues at the legacy `queue_fire` interval.

- `AddTask` signals accepted, immediately due tasks by default. This also applies to
  NFO, SFV, IMDB, TV and other metadata tasks on their respective site/admin queue.
- Race task batches retain their direct assignment fast path and explicitly use
  `AddTask(task, False)`. The existing end-of-batch `QueueSort` / `QueueFire` wakes
  the queue once. Callers suppressing signals must provide their own completion
  signal. The task's final `startat` and notification settings must be configured
  before insertion.
- A newly earlier delayed task interrupts an existing wait. The queue waits until
  the earliest future task/retry, relevant source/destination MAXSIM cooldown, or
  slot idle/quit deadline. A failed nonblocking destination-lock acquisition uses
  a 25 ms retry deadline; slot/dependency-blocked tasks do not use that retry.
  Due but blocked work waits for a state change instead of
  repeatedly scheduling its expired deadline.
- Future waits round up to at least one millisecond. While all source slots are
  occupied, task deadlines wait for a slot completion instead of waking the queue.
- On Linux/x86_64, mORMot's FPC clock redirection preserves the requested clock
  while retaining the libc/vDSO fast path. Substituting a coarse clock caused
  positive event timeouts to expire early; no global RTL optimizations are disabled.
- Site-wide destination blocks survive unrelated queue passes. Slot completion or
  an upload-limit change invalidates them immediately; cooldown/lock deadlines and
  the housekeeping cap provide bounded retries. Release-specific blocks are not
  cached for the entire destination.
- `maxassign` counts assignment exceptions only. A full/offline slot, upload/download
  limit or dependency wait cannot consume retries or impose `maxassign_delay`.
- Slot completion wakes its own queue. Each destination has a small
  one-shot set of source queues that could not proceed. Sources subscribe before
  checking availability; every slot completion takes and wakes this set after
  slot release, including sources trading other releases. No global scan is needed.
- QueueSort marks sorting pending. Insertions and promoted delayed tasks also mark
  it pending. The original priority comparator runs once before dispatch when a
  slot is available, and does not run on unchanged housekeeping passes.
- Signals received during processing remain pending until the next wait; repeated
  signals coalesce in the existing auto-reset event.

`[queue] queue_fallback_ms=15000` is the safety/housekeeping fallback. Supported
values are 10000 through 15000 ms; invalid values use 15000 ms. Legacy `queue_fire`
is ignored, so existing `queue_fire=900` installations do not keep polling. Normal
fallback expiry is no longer reported as an IRC error. QueueClean retains its
existing independent interval. Queue counts are refreshed on queue passes, so idle
statistics can be up to one fallback interval old.

Queue construction is suspended; AddSite starts it after registration and site
initialization. Shutdown wakes and joins queue workers before freeing their state.
Site slots finish before the owning queue's task lists are freed.

## Verification

`tests/queueunitTests.pas` covers retained/coalesced events, default insertion and
explicit batching, earlier deadlines interrupting waits, deadline maturity,
coalesced sorts, fractional waits, capacity revision/expiry, exception backoff and
stop signals. It uses the production insertion,
wait, promotion and sort methods without opening FTP connections.
`tests/slclockTests.pas` also checks Linux RTL monotonic/realtime precision against
direct libc readings and the actual duration of positive event timeouts.

Run `make test` on Linux or `make.bat test_64` on Windows. Also run a live race
against the same sites/configuration on dev and this branch. Compare time to first
assignment, transfer throughput, queue CPU samples, sort samples and wake counts
in VTune. Include idle, an announce burst, full destination slots, mkdir failure,
delayed NFO/dirlist retries, both MAXSIM directions, and shutdown during work.
Passing unit tests does not establish a real-site throughput or CPU improvement.

With no tasks, slots or deadlines, the nominal fallback changes from 900 ms to
15000 ms: about 16.7 times fewer timer-driven queue passes (94% fewer). Real queues
also wake for work and keepalives; this is not a measured CPU reduction.
