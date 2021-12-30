{-# LANGUAGE LambdaCase #-}

module GitUtils (firstCommitInPathTo, cmd, defaultBranch, defaultBaseBranch) where

import Control.Arrow (second)
import Data.Foldable.Extra (findM)
import Data.Functor ((<&>))
import Data.List (isInfixOf, dropWhileEnd)
import Data.Maybe (fromJust, fromMaybe)
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
commitsTo dst =
    cmd command
    <&> reverse . lines . fromMaybe ("error from " <> show command)
    where
        command = "git rev-list .." <> dst

firstCommitInPathTo :: String -> IO (Maybe String)
firstCommitInPathTo base = do
    a <- commonAncestor base
    commitsTo base >>= findM (isStrictlyAhead a)

defaultBranch :: IO String
defaultBranch =
    cmd "git branch" <&>
    \case
    Just x | isInfixOf " master\n" x -> "master"
    _ -> "main"

currentBranch :: IO String
currentBranch = cmd "git branch --show-current" <&> fromJust <&> dropWhileEnd (`elem` "\n\r")

branchRemotes :: IO [(String, String)]
branchRemotes =
    cmd "git branch --format='%(refname:short) %(upstream:short)'"
    <&> fromMaybe []
    <&> lines
    <&> map (second (drop 1) . span (/= ' '))

defaultBaseBranch :: IO String
defaultBaseBranch = do
    d <- defaultBranch
    c <- currentBranch
    if d == c
        then branchRemotes <&> lookup c <&> fromMaybe (error ("No remote branch for: " <> c))
        else pure d
