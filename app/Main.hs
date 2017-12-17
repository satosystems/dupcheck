{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Digest.Pure.MD5 (MD5Digest)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO ( hClose
                 , hPutStrLn
                 , openTempFile
                 , stderr
                 )
import System.Directory ( getTemporaryDirectory
                        , removeFile
                        )

import DupCheck

main :: IO ()
main = do
  arg <- getOptions
  case arg of
    (_, Just msg) -> hPutStrLn stderr msg
    (ops, _) -> do
      tempDir <- getTemporaryDirectory
      (path, handle) <- openTempFile tempDir "dupcheck-.tmp"
      listFileSize handle (dirs ops)
      hClose handle
      contents <- readFile path
      let toTuple t = let ts = T.splitOn ":" t
                      in (ts !! 0, ts !! 1)
          sizes = map toTuple (T.lines (T.pack contents))
          sortedSizes = sortBy (comparing fst) sizes
      digests <- if sortedSizes == []
        then return []
        else getDigests False (head sortedSizes) (tail sortedSizes) []
      let sortedDigests = sortBy (comparing fst) digests
      if sortedDigests == []
        then return ()
        else printDups False (head sortedDigests) (tail sortedDigests)
      removeFile path
 where
  getDigests :: Bool -> (T.Text, T.Text) -> [(T.Text, T.Text)] -> [(MD5Digest, T.Text)] -> IO [(MD5Digest, T.Text)]
  getDigests True (_, value) [] digests = md5sum' value >>= \md -> return $ appendDigest md value digests
  getDigests False _ [] digests = return digests
  getDigests b (key, value) ((k, v):tuples) digests
    | key == k = md5sum' value >>= \md -> getDigests True (k, v) tuples (appendDigest md value digests)
    | b = md5sum' value >>= \md -> getDigests False (k, v) tuples (appendDigest md value digests)
    | otherwise = getDigests False (k, v) tuples digests
  appendDigest :: Maybe MD5Digest -> T.Text -> [(MD5Digest, T.Text)] -> [(MD5Digest, T.Text)]
  md5sum' :: T.Text -> IO (Maybe MD5Digest)
  md5sum' path = md5sum (T.unpack path)
  appendDigest Nothing _ digests = digests
  appendDigest (Just digest) filePath digests = (digest, filePath):digests
  printDups :: Bool -> (MD5Digest, T.Text) -> [(MD5Digest, T.Text)] -> IO ()
  printDups True (_, value) [] = TIO.putStrLn value
  printDups False _ [] = return ()
  printDups b (key, value) ((k, v):tuples)
    | key == k = TIO.putStrLn value >> printDups True (k, v) tuples
    | b = TIO.putStrLn value >> TIO.putStr "\n" >> printDups False (k, v) tuples
    | otherwise = printDups False (k, v) tuples

