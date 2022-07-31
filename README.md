# git-utils
Small helpers for using git

## `sub-rebase`

Rebase gradually: replay branch commits over newer commits on target branch, to handle the conflicts in smaller units.

See post: [Break big merges to smaller pieces](https://yairchu.github.io/posts/split-merge-to-smaller-pieces)

## `sub-merge.sh`

A merging variant of `sub-rebase.sh`

## `git-w.py`

A tool to normalize line endings to reduce diffs.
It may normalize line endings to either '\n' (unix) or '\r\n' (windows)
depending on which one best reduces the diff size.

## `submodules-dedup.py`

A tool to avoid recloning submodules that are repeated across different repositories.
