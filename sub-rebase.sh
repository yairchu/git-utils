#!/bin/bash

# If resuming on previous rebase and solved all conflicts, do the "git rebase --continue" action
git status | grep "all conflicts fixed: run \"git rebase --continue\"" && git rebase --continue

GIT_FOLDER=$(git rev-parse --show-toplevel)/.git
DEFAULT_BRANCH=$([ -f ${GIT_FOLDER}/refs/heads/master ] && echo master || echo main)
BASE=${1:-$DEFAULT_BRANCH}
while true
do
    # Find next commit to rebase to
    NEXT_COMMIT=
    COMMON_ANCESTOR=$(git merge-base HEAD $BASE)

    # Find all files touched by commits in the current branch
    CHANGED_FILES=$(git log --pretty=format: --name-only $BASE..HEAD | sort -u)

    for COMMIT in $(git rev-list ..$BASE --reverse)
    do
        if [ $(git merge-base HEAD $COMMIT) != $COMMON_ANCESTOR ]
        then
            # Skipping $COMMIT as it is not strictly ahead of $BASE
            continue
        fi

        # $COMMIT is a suitable candidate for rebasing

        if [ "$NEXT_COMMIT" != "" ]
        then
            # Checking if $COMMIT is a better candidate than $NEXT_COMMIT

            CHANGES_TO_CUR=$(git diff --name-only $COMMON_ANCESTOR..$COMMIT | sort -u)
            OVERLAP=$(comm -12 <(echo "$CHANGED_FILES") <(echo "$CHANGES_TO_CUR"))
            if [ "$OVERLAP" != "" ]
            then
                # Possible conflicts with $COMMIT, rebasing to $NEXT_COMMIT first
                break
            fi
        fi

        # Rebasing on this commit will strictly progress towards our goal
        NEXT_COMMIT=$COMMIT
    done

    [ "$NEXT_COMMIT" == "" ] && echo "Done" && exit
    NEXT_COMMIT_DATE=$(git show -s --format=%cd --date=short $NEXT_COMMIT)
    echo Rebasing over $NEXT_COMMIT \($NEXT_COMMIT_DATE\)
    git rebase $NEXT_COMMIT || exit 1
done
