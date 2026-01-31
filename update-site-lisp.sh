#!/usr/bin/env bash
set -u -o pipefail

usage() {
  cat <<'EOF'
Usage:
  update-site-lisp.sh [PATH] [--rebase|--ff-only|--merge] [--stash] [--submodules]

Defaults:
  PATH: ~/.emacs.d/site-lisp
  mode: --ff-only

Behavior:
  - For every git repo under PATH:
    * fetch --all --prune
    * ensure it is on branch main or master (prefers origin/main, then origin/master)
    * pull to update
  - Skips dirty repos unless --stash is provided.
  - Prints Skipped/Failed repos at the end.

Examples:
  ./update-site-lisp.sh
  ./update-site-lisp.sh --stash
  ./update-site-lisp.sh ~/.emacs.d/site-lisp --rebase
EOF
}

SITE_LISP="${HOME}/.emacs.d/site-lisp"
MODE="ff-only"     # ff-only | rebase | merge
STASH=0            # 0/1
SUBMODULES=0       # 0/1

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help) usage; exit 0 ;;
    --rebase) MODE="rebase"; shift ;;
    --ff-only) MODE="ff-only"; shift ;;
    --merge) MODE="merge"; shift ;;
    --stash) STASH=1; shift ;;
    --submodules) SUBMODULES=1; shift ;;
    --path) SITE_LISP="$2"; shift 2 ;;
    *)
      if [[ -d "$1" ]]; then
        SITE_LISP="$1"; shift
      else
        echo "Unknown argument: $1" >&2
        usage
        exit 2
      fi
      ;;
  esac
done

if [[ ! -d "$SITE_LISP" ]]; then
  echo "Directory not found: $SITE_LISP" >&2
  exit 1
fi

echo "Site-lisp root: $SITE_LISP"
echo "Mode: $MODE, stash: $STASH, submodules: $SUBMODULES"
echo

declare -A REPOS=()
while IFS= read -r -d '' gitpath; do
  repo="$(dirname "$gitpath")"
  REPOS["$repo"]=1
done < <(find "$SITE_LISP" -mindepth 1 -maxdepth 4 -name .git -print0 2>/dev/null)

if [[ ${#REPOS[@]} -eq 0 ]]; then
  echo "No git repos found under: $SITE_LISP"
  exit 0
fi

declare -a SKIPPED_LIST=()
declare -a FAILED_LIST=()

updated=0
skipped=0
failed=0

record_skipped() { SKIPPED_LIST+=("$1|$2"); ((skipped++)); }
record_failed()  { FAILED_LIST+=("$1|$2");  ((failed++)); }

# Pick target branch based on remote refs; prefer main then master.
pick_target_branch() {
  local repo="$1"
  if git -C "$repo" show-ref --verify --quiet refs/remotes/origin/main; then
    echo "main"
    return 0
  fi
  if git -C "$repo" show-ref --verify --quiet refs/remotes/origin/master; then
    echo "master"
    return 0
  fi
  return 1
}

# Ensure we are on target branch and tracking origin/<branch>.
ensure_on_branch() {
  local repo="$1" branch="$2"

  # If local branch exists, switch to it; otherwise create and track remote.
  if git -C "$repo" show-ref --verify --quiet "refs/heads/$branch"; then
    git -C "$repo" switch "$branch" >/dev/null 2>&1
  else
    git -C "$repo" switch -C "$branch" --track "origin/$branch" >/dev/null 2>&1
  fi
}

for repo in "${!REPOS[@]}"; do
  echo "==> $repo"

  if ! git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "  - not a git work tree, skip"
    record_skipped "$repo" "not a git work tree"
    echo
    continue
  fi

  # Skip dirty repos unless --stash
  dirty="$(git -C "$repo" status --porcelain)"
  stashed=0
  if [[ -n "$dirty" ]]; then
    if [[ "$STASH" -eq 1 ]]; then
      msg="auto-stash before site-lisp update $(date -Is 2>/dev/null || date)"
      if git -C "$repo" stash push -u -m "$msg" >/dev/null 2>&1; then
        stashed=1
        echo "  - dirty: stashed"
      else
        echo "  ! failed to stash"
        record_failed "$repo" "failed to stash dirty worktree"
        echo
        continue
      fi
    else
      echo "  - dirty (uncommitted changes), skip (use --stash)"
      record_skipped "$repo" "dirty worktree (use --stash)"
      echo
      continue
    fi
  fi

  # Fetch first so we can see origin/main or origin/master even if detached.
  if ! git -C "$repo" fetch --all --prune; then
    echo "  ! fetch failed"
    record_failed "$repo" "git fetch failed"
    # restore stash if we created one
    if [[ "$stashed" -eq 1 ]]; then
      echo "  - attempting to restore stash..."
      git -C "$repo" stash pop >/dev/null 2>&1 || echo "  ! stash pop had conflicts; resolve manually"
    fi
    echo
    continue
  fi

  # Force onto main/master (prefers main)
  target=""
  if target="$(pick_target_branch "$repo")"; then
    if ! ensure_on_branch "$repo" "$target"; then
      echo "  ! failed to switch to $target"
      record_failed "$repo" "failed to switch to $target"
      if [[ "$stashed" -eq 1 ]]; then
        echo "  - attempting to restore stash..."
        git -C "$repo" stash pop >/dev/null 2>&1 || echo "  ! stash pop had conflicts; resolve manually"
      fi
      echo
      continue
    fi
    echo "  - on branch: $target"
  else
    echo "  - no origin/main or origin/master, skip"
    record_skipped "$repo" "no origin/main or origin/master"
    if [[ "$stashed" -eq 1 ]]; then
      echo "  - attempting to restore stash..."
      git -C "$repo" stash pop >/dev/null 2>&1 || echo "  ! stash pop had conflicts; resolve manually"
    fi
    echo
    continue
  fi

  # Pull update
  pull_ok=1
  case "$MODE" in
    ff-only) git -C "$repo" pull --ff-only || pull_ok=0 ;;
    rebase)  git -C "$repo" pull --rebase  || pull_ok=0 ;;
    merge)   git -C "$repo" pull           || pull_ok=0 ;;
    *)       echo "  ! unknown mode: $MODE"; pull_ok=0 ;;
  esac

  if [[ "$pull_ok" -ne 1 ]]; then
    echo "  ! pull failed"
    record_failed "$repo" "git pull failed (mode=$MODE)"
    if [[ "$stashed" -eq 1 ]]; then
      echo "  - attempting to restore stash..."
      git -C "$repo" stash pop >/dev/null 2>&1 || echo "  ! stash pop had conflicts; resolve manually"
    fi
    echo
    continue
  fi

  if [[ "$SUBMODULES" -eq 1 ]]; then
    if ! git -C "$repo" submodule update --init --recursive >/dev/null 2>&1; then
      echo "  ! submodule update failed (continue)"
    fi
  fi

  if [[ "$stashed" -eq 1 ]]; then
    if ! git -C "$repo" stash pop; then
      echo "  ! stash pop had conflicts; resolve manually in: $repo"
    else
      echo "  - stash restored"
    fi
  fi

  echo "  - updated"
  ((updated++))
  echo
done

echo "Done."
echo "Updated: $updated, Skipped: $skipped, Failed: $failed"
echo

if [[ ${#SKIPPED_LIST[@]} -gt 0 ]]; then
  echo "=== Skipped repos ==="
  for item in "${SKIPPED_LIST[@]}"; do
    repo="${item%%|*}"
    reason="${item#*|}"
    echo "- $repo  ($reason)"
  done
  echo
fi

if [[ ${#FAILED_LIST[@]} -gt 0 ]]; then
  echo "=== Failed repos ==="
  for item in "${FAILED_LIST[@]}"; do
    repo="${item%%|*}"
    reason="${item#*|}"
    echo "- $repo  ($reason)"
  done
  echo
fi

[[ "$failed" -eq 0 ]]
