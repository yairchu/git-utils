#!/bin/bash

BASE=${1:-master}
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