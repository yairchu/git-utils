{-# LANGUAGE LambdaCase #-}

module GitUtils (firstCommitInPathTo, cmd, defaultBranch) where

import Data.Foldable.Extra (findM)
import Data.Functor ((<&>))
import System.Directory
import System.Process

cmd :: String -> IO String
cmd x = readCreateProcess (shell x) ""

commonAncestor :: String -> IO String
commonAncestor = cmd . ("git merge-base HEAD " <>)

isStrictlyAhead :: String -> String -> IO Bool
isStrictlyAhead base commit = cmd ("git merge-base HEAD " <> commit) <&> (== base)

commitsTo :: String -> IO [String]
commitsTo = fmap (reverse . lines) . cmd . ("git rev-list .." <>)

firstCommitInPathTo :: String -> IO (Maybe String)
firstCommitInPathTo base = do
    a <- commonAncestor base
    commitsTo base >>= findM (isStrictlyAhead a)

gitFolder :: IO String
gitFolder = cmd "git rev-parse --show-toplevel"

defaultBranch :: IO String
defaultBranch =
    gitFolder >>= doesFileExist . (<> "/.git/refs/heads/master") <&>
    \case
    True -> "master"
    False -> "main"
