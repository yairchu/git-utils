#!/bin/bash

# If resuming on previous rebase and solved all conflicts, do the "git rebase --continue" action
git status | grep "all conflicts fixed: run \"git rebase --continue\"" && git rebase --continue

GIT_FOLDER=`git rev-parse --show-toplevel`/.git
DEFAULT_BRANCH=`[ -f ${GIT_FOLDER}/refs/heads/master ] && echo master || echo main`
BASE=${1:-$DEFAULT_BRANCH}
while true
do
    # Find next commit to rebase to
    NEXT_COMMIT=
    COMMON_ANCESTOR=$(git merge-base HEAD $BASE)
    for COMMIT in $(git rev-list ..$BASE --reverse)
    do
        if [ $(git merge-base HEAD $COMMIT) == $COMMON_ANCESTOR ]
        then
            # Rebasing on this commit will strictly progress towards our goal
            NEXT_COMMIT=$COMMIT
            break
        fi
        # The commit is not strictly ahead of BASE
        echo Skipping $COMMIT as it is not strictly ahead of $BASE
    done

    [ "$NEXT_COMMIT" == "" ] && echo "Done" && exit
    echo Rebasing over $NEXT_COMMIT
    git rebase $NEXT_COMMIT || exit 1
done