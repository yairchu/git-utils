#!/bin/bash

GIT_FOLDER=$(git rev-parse --show-toplevel)/.git
DEFAULT_BRANCH=$([ -f "${GIT_FOLDER}"/refs/heads/master ] && echo master || echo main)
BASE=${1:-$DEFAULT_BRANCH}
for COMMIT in $(git rev-list .."$BASE" --reverse)
do
    COMMAND="git merge --no-commit $COMMIT"
    $COMMAND >/dev/null
    STATUS=$?
    while [ $STATUS == 128 ]; do
        echo "Git fatal failure (probably another program using git at the same time)"
        echo "Applying git reset --hard and retrying"
        echo
        git reset --hard >/dev/null
        $COMMAND >/dev/null
        STATUS=$?
    done
    if [ $STATUS == 0 ]; then
        echo No conflicts! git reset --hard
        git reset --hard >/dev/null
    else
        echo Conflicts when merging with "$COMMIT", merging with its parent
        git reset --hard >/dev/null
        git merge --no-edit "$COMMIT"^
        echo Merging with "$COMMIT"
        git merge "$COMMIT"
        exit 1
    fi
done
echo No conflicts, merging!
git merge --no-edit "$BASE"
