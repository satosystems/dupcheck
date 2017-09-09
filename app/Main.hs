module Main where

import DupCheck

main :: IO ()
main = do
  a <- getOptions
  case a of
    (_, Just msg) -> putStrLn msg
    (ops, _) -> listDirectories (dirs ops) >>= mapM_ putStrLn

