#!/bin/bash
# Builds slFtp and runs the unit tests with FPC inside WSL, in the GitLab CI image (same steps as the
# CI jobs buildlinux64 and testlinux64). Started by fpc-wsl.ps1, see SKILL.md next to this file; on
# Linux, glftpd-e2e and run-slftp use its --export. Runs as the normal user and only reads the
# checkout this script lives in.
#
#   fpc-wsl.sh             build HEAD + uncommitted changes + untracked (not ignored) files
#   fpc-wsl.sh <ref>       build only the committed state of <ref>
#   fpc-wsl.sh --id <id> [<ref>]  same, but write the exit code to a file of its own for run <id>
#   fpc-wsl.sh --export <dir> [<ref>]  same sources, but only the release build, no unit tests: copies
#                          the slftp binary, config/ and the OpenSSL libraries into <dir> (glftpd-e2e,
#                          run-slftp)
#                          and writes the tested state to <dir>.commit: the short commit hash, with
#                          " + uncommitted changes" if there were any
#   fpc-wsl.sh --initialized  exit code 0 if the build clone exists (else the run clones, slow)
#   fpc-wsl.sh --clear [<id>]   remove the exit code file of the last run (or of run <id>)
#   fpc-wsl.sh --status [<id>]  print the exit code of the last run (or of run <id>), nothing while it
#                               is still running
#   fpc-wsl.sh --summary   print the important lines of the last log
#
# Clone, log and exit code live in the home directory: ~/slftp-fpc-build, ~/slftp-fpc-build.{log,exitcode}
# Only one run at a time: another one exits with code 11 and leaves clone, log and exit code alone.
# The exit code of a run with --id goes to ~/slftp-fpc-build.exitcode.<id>, so the caller never reads
# the result of another run.

repo="$(cd "$(dirname "$0")/../../.." && pwd)"
build_dir="$HOME/slftp-fpc-build"
log_file="$build_dir.log"
exitcode_file="$build_dir.exitcode"
image=registry.gitlab.com/slftp/ci-images/ubuntu-lts
# A checkout on a Windows drive (WSL) is read with Windows git and its own settings (autocrlf, stat
# info), any other checkout with git. Neither writes to it.
if command -v wslpath >/dev/null && [[ "$(wslpath -w "$repo" 2>/dev/null)" != '\\'* ]]; then
  repo_git=git.exe
  setup_doc=.agents/docs/wsl-fpc-setup.md
else
  repo_git=git
  setup_doc=.agents/skills/glftpd-e2e/SKILL.md
fi

case "$1" in
  --initialized) [ -d "$build_dir/.git" ]; exit ;;
  --clear) rm -f "$exitcode_file${2:+.$2}"; exit 0 ;;
  --status) [ -f "$exitcode_file${2:+.$2}" ] && cat "$exitcode_file${2:+.$2}"; exit 0 ;;
  --summary)
    # steps, compiler errors, failed test suites/tests and the test totals
    grep -E '^=== |^ERROR|Error:|Fatal:|Fail| [EF]:[1-9]|Number of' "$log_file"
    echo "Full log: $log_file"
    exit 0 ;;
  --id) run_id="$2"; exitcode_file="$exitcode_file.$2"; shift 2 ;;
  --export) export_dir="$(realpath -m "$2")"; shift 2 ;;
esac
ref="$1"

step() { echo; echo "=== $* ==="; }
fail() { echo "ERROR: $*"; return 1; }
src_git() { (cd "$repo" && "$repo_git" "$@"); }

# exit code 10: environment not set up (make itself uses 2)
check_env() {
  local missing=''
  command -v git >/dev/null || missing="git not installed"
  command -v "$repo_git" >/dev/null || missing="$repo_git not found"
  command -v docker >/dev/null || missing="docker not installed"
  [ -n "$missing" ] || docker info >/dev/null 2>&1 || missing="docker not usable by $(id -un)"
  [ -z "$missing" ] || { fail "$missing, see $setup_doc"; return 10; }
}

# Checks out the committed state in the build clone and, without <ref>, adds the local changes
sync_sources() {
  local common commit
  common=$(src_git rev-parse --path-format=absolute --git-common-dir | tr -d '\r') || return
  [ "$repo_git" = git ] || common=$(wslpath -u "$common") || return
  commit=$(src_git rev-parse --verify --quiet "${ref:-HEAD}^{commit}" | tr -d '\r')
  [ -n "$commit" ] || fail "unknown ref '${ref:-HEAD}' in $repo" || return
  [ -d "$build_dir" ] || git init -q "$build_dir" || return
  [ -d "$build_dir/.git" ] || fail "$build_dir exists but is not a git repository" || return
  cd "$build_dir" || return

  step "fetch ${ref:-HEAD} ($commit) from $repo"
  # a commit hash is only fetchable when upload-pack allows it; a worktree has no own object store
  git fetch --upload-pack='git -c uploadpack.allowAnySHA1InWant=true upload-pack' \
    "$common" "$commit" || return
  git checkout -q -f --detach "$commit" || return
  git clean -fdq || return # untracked files of earlier runs; ignored build output stays
  git log -1 --oneline
  tested=$(git rev-parse --short HEAD)

  [ -z "$ref" ] || return 0
  step "apply uncommitted changes and untracked files"
  # the options override user settings that break git apply (external diff, color, prefixes, ...)
  src_git diff --binary --no-ext-diff --no-textconv --no-color --no-relative --src-prefix=a/ \
    --dst-prefix=b/ "$commit" > "$build_dir.patch" || return
  if [ -s "$build_dir.patch" ]; then git apply --whitespace=nowarn "$build_dir.patch" || return; fi
  src_git ls-files -o --exclude-standard -z > "$build_dir.untracked" || return
  if [ -s "$build_dir.untracked" ]; then
    (cd "$repo" && tar --null -T "$build_dir.untracked" -cf -) | tar -xf - || return
  fi
  [ ! -s "$build_dir.patch" ] && [ ! -s "$build_dir.untracked" ] || tested="$tested + uncommitted changes"
  git status --short
}

build_and_test() {
  step "update CI image $image"
  docker pull -q "$image" || echo "WARNING: pull failed, using the local image"
  step "make slftp_64 + make test"
  # same user as the clone owner, so the build output stays deletable and git trusts the repository
  docker run --rm --user "$(id -u):$(id -g)" -e HOME=/tmp -v "$build_dir":/src -w /src "$image" sh -c '
    make clean >/dev/null &&
    make slftp_64 && echo "=== BUILD OK ===" &&
    cp /usr/lib/x86_64-linux-gnu/libcrypto.so.3 /usr/lib/x86_64-linux-gnu/libssl.so.3 tests/ &&
    cp /usr/lib/x86_64-linux-gnu/ossl-modules/legacy.so tests/ &&
    make test < /dev/null'
}

build_and_export() {
  step "update CI image $image"
  docker pull -q "$image" || echo "WARNING: pull failed, using the local image"
  step "make slftp_64, copy it to $export_dir"
  mkdir -p "$export_dir" && echo "$tested" > "$export_dir.commit" || return
  docker run --rm --user "$(id -u):$(id -g)" -e HOME=/tmp -v "$build_dir":/src -v "$export_dir":/out \
    -w /src "$image" sh -c '
    make clean >/dev/null &&
    make slftp_64 && echo "=== BUILD OK ===" &&
    cp slftp /out/ && cp -r config/. /out/ &&
    cp /usr/lib/x86_64-linux-gnu/libcrypto.so.3 /usr/lib/x86_64-linux-gnu/libssl.so.3 /out/ &&
    cp /usr/lib/x86_64-linux-gnu/ossl-modules/legacy.so /out/'
}

run() {
  check_env && sync_sources || return
  if [ -n "$export_dir" ]; then build_and_export; else build_and_test; fi
}

# exit code 11: another run is active; the lock is held until this script exits
exec 9>"$build_dir.lock"
if ! flock -n 9; then
  echo "ERROR: another run is active, try again when it has finished"
  [ -z "$run_id" ] || echo 11 > "$exitcode_file"
  exit 11
fi
rm -f "$exitcode_file"
set -o pipefail
run 2>&1 | tee "$log_file"
rc=$?
if [ $rc -eq 0 ]; then
  echo "=== PASSED ==="
else
  echo "=== FAILED with exit code $rc ==="
fi | tee -a "$log_file"
echo $rc > "$exitcode_file"
exit $rc
