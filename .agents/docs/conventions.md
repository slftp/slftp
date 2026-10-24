# Conventions

Summary of `CONTRIBUTING.md`, which is binding and wins on any conflict.

## Code

- Names
  - `GlFoo` — globals in the `interface` section; `glFoo` — unit globals in the `implementation` section
  - `FFoo` — class/record fields (private where possible; expose via `property`, write access only if needed)
  - `fFoo` — local variables (`i`, `j` are fine for indices)
  - `aFoo` — parameters; use `const` where useful
  - `_foo` — functions that exist only in the `implementation` section
- Comments are PasDoc. Everything in the `interface` section **must** be documented: `//<` after fields,
  `@param(...)`/`@returns(...)` for functions.
- Reuse existing slFtp helpers (`mystrings.pas`, `slhelper.pas`, `dbhandler.pas`, …) first; otherwise
  prefer mORMot2 over other libraries or new own implementations.
- Prefer generic classes (`Generics.Collections`) over old non-generic containers.
- Write several small functions instead of one big one; avoid duplication.
- Don't hack something in — refactor and write proper code.
- Write or adjust tests when a function's behavior changes.

## Docs

- Write down only what a reader needs to use or change the code correctly: behavior, constraints and
  pitfalls that still exist. How a change works belongs in code comments, why it was made in the commit
  message. Pitfalls your change removed don't belong in the docs.
- Prefer adjusting an existing sentence to adding a paragraph.
- Reference code by symbol name (`TSiteSlot.ProcessFeat`, `parseIrcJOIN`), never by line number —
  line numbers go stale with every edit above them.
- Only link to external URLs you have verified actually resolve.

## Commits

```
<type>(<scope>): <subject>

<body>

<footer>
```

- `type`: `feat`, `fix`, `style`, `refactor`, `perf`, `chore`, `remove`, `update`
- `scope` (optional): unit, class or function, e.g. `precatcher`, `TPazo`, `RemovePazoMKDIR`
- `subject`: imperative, present tense ("change", not "changed"), no trailing dot
- `body`: motivation for the change and how it differs from the previous behavior
- `footer`: `Closes #123, #245` and `BREAKING CHANGE: ...` blocks
- No line longer than 100 characters
- Reverts start with `revert` + the reverted header; the body says `This reverts commit <hash>.`

## Branches

- Branch off `dev`, one topic per branch. To update a branch, rebasing onto `dev` is preferred, but
  merging `dev` in is fine: GitLab squash-merges MRs by default, so the branch history never reaches `dev`.
- The project is on GitLab (`gitlab.com/slftp/slftp`): changes go in as merge requests targeting `dev`,
  titled like the commit header. GitHub tools (`gh`) don't apply. If `glab` isn't installed, create the
  merge request with push options:
  ```
  git push -u origin <branch> -o merge_request.create -o merge_request.target=dev \
    -o "merge_request.title=<commit header>" -o "merge_request.description=<text>" \
    -o merge_request.remove_source_branch
  ```
  Push options must not contain newlines: use `<br>` for line breaks in the description, or edit it
  in the web UI afterwards.
- Releases are annotated tags made from `dev` only (process: `CONTRIBUTING.md`).
