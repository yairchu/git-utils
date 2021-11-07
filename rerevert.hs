{-# LANGUAGE LambdaCase #-}

import GitUtils

import Data.Maybe (fromMaybe)

main :: IO ()
main =
    cmd "git log --grep=\"This reverts commit \" -n 1" >>=
    \case
    Nothing -> putStrLn "git log failed?"
    Just [] -> putStrLn "No reverts before this point"
    Just ("commit" : revertCommit : ws@(_:_)) ->
        do
            _ <- cmd ("git checkout " <> revertCommit <> "^")
            let reverted = init (last ws)
            cmd ("git revert " <> reverted)
        >>=
        \case
        Just{} -> cmd "git checkout HEAD^" >> main
        Nothing ->
            do
                putStrLn "Rerevert:\n"
                cmd "git status" >>= putStr . fromMaybe ""
    Just{} -> putStrLn "Couldn't parse commit message"
    . fmap words
