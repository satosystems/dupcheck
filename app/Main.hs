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
      print pairs
      let fromJustKey (Just digest, value) = (digest, value)
          filterNotNothing (key, _) = key /= Nothing
          filteredPairs = map fromJustKey $ filter filterNotNothing pairs
          dups = listDuplicates filteredPairs
          dupLists = intersperse [""] $ map snd dups
      mapM_ (\list -> mapM_ putStrLn list) dupLists

