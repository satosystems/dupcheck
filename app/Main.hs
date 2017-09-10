module Main where

import Data.List (intersperse)
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
          dups' = intersperse [""] dups
      mapM_ (\list -> mapM_ putStrLn list) dups'

