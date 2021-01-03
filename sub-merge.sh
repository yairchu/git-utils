#!/bin/bash

BASE=${1:-master}
for COMMIT in $(git rev-list ..$BASE --reverse)
do
    if git merge --no-commit $COMMIT >/dev/null 2>/dev/null; then
        git reset --hard >/dev/null
    else
        echo Conflicts when merging with $COMMIT, merging with its parent
        git reset --hard >/dev/null
        git merge --no-edit $COMMIT^
        echo Merging with $COMMIT
        git merge $COMMIT
        exit 1
    fi
done
echo No conflicts, merging!
git merge --no-edit $BASE
