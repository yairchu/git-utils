{-# LANGUAGE LambdaCase #-}

import GitUtils

import Data.Maybe (fromMaybe)

main :: IO ()
main =
    cmd "git log --merges --pretty=format:%P -n 1" >>=
    \case
    Nothing -> putStrLn "git log failed?"
    Just [] -> putStrLn "No merges before this point"
    Just [_] -> putStrLn "Error: Merge of single parent?"
    Just [x, y] ->
        do
            _ <- cmd ("git checkout " <> x)
            cmd ("git merge " <> y) >>=
                \case
                Just{} -> cmd "git checkout HEAD^" >> main
                Nothing ->
                    do
                        putStrLn "Remerge:\n"
                        cmd "git status" >>= putStr . fromMaybe ""
    Just{} -> putStrLn "Merge of more than two parents not supported!"
    . fmap words
