#!/usr/bin/env stack

{-# LANGUAGE LambdaCase #-}

import GitUtils

import Control.Monad (when)
import Control.Monad.Extra (fromMaybeM)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import qualified Options.Applicative as O
import System.Exit (ExitCode(..))
import System.Process

opts :: O.ParserInfo (Maybe String)
opts =
    O.info
    (O.helper <*>
        O.optional (O.strArgument (O.metavar "BASE" <> O.help "Base branch to rebase over")))
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
    cmd "git status" <&> isInfixOf "all conflicts fixed: run \"git rebase --continue\"" . fromJust

main :: IO ()
main = do
    base <- fromMaybeM defaultBaseBranch (O.execParser opts)
    putStrLn ("Rebasing onto: " <> base)
    allConflictsFixed >>= (`when` callCommand "git rebase --continue")
    subRebase base
