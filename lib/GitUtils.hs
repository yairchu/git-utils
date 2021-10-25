{-# LANGUAGE LambdaCase #-}

module GitUtils (firstCommitInPathTo, cmd, defaultBranch) where

import Data.Foldable.Extra (findM)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.Exit
import System.Process

cmd :: String -> IO (Maybe String)
cmd x =
    f <$> readCreateProcessWithExitCode (shell x) ""
    where
        f (ExitSuccess, r, _) = Just r
        f _ = Nothing

commonAncestor :: String -> IO String
commonAncestor = fmap fromJust . cmd . ("git merge-base HEAD " <>)

isStrictlyAhead :: String -> String -> IO Bool
isStrictlyAhead base commit = cmd ("git merge-base HEAD " <> commit) <&> (== Just base)

commitsTo :: String -> IO [String]
commitsTo = fmap (reverse . lines . fromJust) . cmd . ("git rev-list .." <>)

firstCommitInPathTo :: String -> IO (Maybe String)
firstCommitInPathTo base = do
    a <- commonAncestor base
    commitsTo base >>= findM (isStrictlyAhead a)

gitFolder :: IO String
gitFolder = cmd "git rev-parse --show-toplevel" <&> takeWhile (`notElem` "\r\n") . fromJust

defaultBranch :: IO String
defaultBranch =
    gitFolder >>= doesFileExist . (<> "/.git/refs/heads/master") <&>
    \case
    True -> "master"
    False -> "main"
