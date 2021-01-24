module GitUtils (firstCommitInPathTo, cmd) where

import Control.Lens.Operators
import Control.Monad
import Data.Foldable.Extra
import System.Process

cmd :: String -> IO String
cmd x = readCreateProcess (shell x) ""

commonAncestor = cmd . ("git merge-base HEAD " <>)

isStrictlyAhead base commit = cmd ("git merge-base HEAD " <> commit) <&> (== base)

commitsTo = fmap (reverse . lines) . cmd . ("git rev-list .." <>)

firstCommitInPathTo base =
    do
        a <- commonAncestor base
        commitsTo base >>= findM (isStrictlyAhead a)
