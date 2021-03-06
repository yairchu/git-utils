#!/usr/bin/env stack

{-# LANGUAGE LambdaCase #-}

import GitUtils

import Control.Monad (when)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as O
import System.Exit (ExitCode(..))
import System.Process

opts :: O.ParserInfo String
opts =
    O.info
    (O.helper <*>
        (O.optional (O.strArgument (O.metavar "BASE" <> O.help "Base branch to rebase over"))
            <&> fromMaybe "master"))
    (O.fullDesc <> O.progDesc "Gradually rebase to handle less merge conflicts at a time")

subRebase :: String -> IO ()
subRebase base =
    firstCommitInPathTo base >>=
    \case
    Nothing -> putStrLn "Done"
    Just commit -> do
        putStrLn ("Rebasing over " <> commit)
        spawnCommand ("git rebase " <> commit) >>= waitForProcess
            >>= (`when` subRebase base) . (== ExitSuccess)

allConflictsFixed :: IO Bool
allConflictsFixed =
    cmd "git status" <&> isInfixOf "all conflicts fixed: run \"git rebase --continue\""

main :: IO ()
main = do
    base <- O.execParser opts
    allConflictsFixed >>= (`when` callCommand "git rebase --continue")
    subRebase base
