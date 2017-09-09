module Main where

import Control.Concurrent.Async (mapConcurrently)

import DupCheck

main :: IO ()
main = do
  a <- getOptions
  case a of
    (_, Just msg) -> putStrLn msg
    (ops, _) -> do
      files <- listDirectories (dirs ops)
      pairs <- mapConcurrently md5sum files
      let dups = listDuplicates pairs
      mapM_ print pairs
      mapM_ print dups

