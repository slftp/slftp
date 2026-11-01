# Working style

How to approach a task. These guidelines favor caution over speed; for trivial tasks, use judgment.
`CONTRIBUTING.md` and [conventions](conventions.md) win on any conflict.

## 1. Think before coding

Don't assume, don't hide confusion, surface tradeoffs.

- State your assumptions explicitly. If you are unsure, ask.
- If a request can be read in several ways, present the readings instead of silently picking one.
- If a simpler approach exists, say so. Push back when it is warranted.
- If something is unclear, stop, name what is confusing and ask.

## 2. Simplicity first

Write the minimum code that solves the problem, nothing speculative.

- No features beyond what was asked.
- No abstractions for code that is used only once.
- No "flexibility" or configurability (new `slftp.ini` options, IRC command parameters, …) that was not
  requested.
- No error handling for scenarios that cannot happen.
- If 200 lines could be 50, rewrite them. Ask yourself whether a senior engineer would call it
  overcomplicated.

## 3. Surgical changes

Touch only what you must and clean up only your own mess.

- Don't "improve" adjacent code, comments or formatting, and don't refactor code that isn't broken.
  "Don't hack something in — refactor" ([conventions](conventions.md)) applies to the code your change
  needs; it is no license to rework unrelated code.
- Match the style of the surrounding code. The naming conventions apply to the code you write; don't
  rename existing identifiers just to make them conform.
- If you notice unrelated dead code or bugs, mention them instead of changing them. If they are worth
  fixing, they get their own branch off `dev` (one topic per branch).
- Remove the `uses` entries, variables and functions that *your* change left unused; leave pre-existing
  dead code alone unless asked.

The test: every changed line traces directly back to the request.

## 4. Goal-driven execution

Define success criteria and loop until they are verified.

- "Add validation" → write tests for invalid input, then make them pass.
- "Fix the bug" → write a test that reproduces it, then make it pass.
- "Refactor X" → make sure the tests pass before and after.

Verification in slFtp means building and running the unit tests with both compilers (see
[build-and-test](build-and-test.md)); if you can only run one, say which one and that the other is
unverified.

For tasks with several steps, state a short plan first:

```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you work on your own; weak ones ("make it work") need constant clarification.

These guidelines work if diffs contain fewer unnecessary changes, fewer rewrites are needed because of
overcomplication, and clarifying questions come before the implementation instead of after mistakes.
