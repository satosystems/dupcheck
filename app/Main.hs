module Main where

import DupCheck

main :: IO ()
main = do
  a <- getOptions
  case a of
    (_, Just msg) -> putStrLn msg
    (ops, _) -> do
      files <- listDirectories (dirs ops)
      pairs <- mapM md5sum files
      mapM_ print pairs

