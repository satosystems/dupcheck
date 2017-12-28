{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DupCheck (main) where

import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BI
import qualified Data.ByteString.Lazy as LBI
import Data.Digest.Pure.MD5 ( MD5Context
                            , MD5Digest
                            , md5Finalize
                            , md5InitialContext
                            , md5Update
                            )
import Data.List (sortBy)
import Data.List.Unique (sortUniq)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Paths_dupcheck (version)
import System.Console.CmdArgs ( Data
                              , Typeable
                              , (&=)
                              , args
                              , cmdArgs
                              , summary
                              )
import System.IO ( Handle
                 , IOMode(ReadMode)
                 , hClose
                 , hPutStrLn
                 , openFile
                 , openTempFile
                 , stderr
                 )
import System.Directory ( doesDirectoryExist
                        , getFileSize
                        , getTemporaryDirectory
                        , listDirectory
                        , pathIsSymbolicLink
                        , removeFile
                        )

data Options = Options
  { dirs :: [FilePath]
  } deriving (Show, Data, Typeable)

options :: Options
options = Options
  { dirs = [] &= args
  } &= summary ("dupcheck " ++ showVersion version)

getOptions :: IO (Options, Maybe String)
getOptions = do
  ops <- cmdArgs options
  mErrorMessage <- checkOptions ops
  return (ops, mErrorMessage)
 where
  checkOptions :: Options -> IO (Maybe String)
  checkOptions ops | dirs ops == [] = return $ Just "Please specify target directories"
                   | otherwise = do
    bs <- mapM doesDirectoryExist $ dirs ops
    let isValid = and bs
    return $ if isValid then Nothing else Just "Specified arguments are not directory"

listFileSize :: Handle -> [FilePath] -> IO ()
listFileSize handle directories = listFiles (sortUniq $ map removeFileSeparator directories)
 where
  removeFileSeparator :: FilePath -> FilePath
  removeFileSeparator filePath | last filePath == '/' = init filePath
                               | otherwise = filePath
  listFiles :: [FilePath] -> IO ()
  listFiles [] = return ()
  listFiles (d:ds) = do
    listed <- (listDirectory d) `E.catch` (const $ return [] :: IOError -> IO [FilePath])
    mapM_ listFile listed
    listFiles ds
   where
    listFile :: FilePath -> IO ()
    listFile filePath = do
      let path = d ++ "/" ++ filePath
      isSymbolicLink <- pathIsSymbolicLink path
      if isSymbolicLink then return () else do
        isDirectory <- doesDirectoryExist path
        if isDirectory then listFileSize handle [path] else do
          size <- (getFileSize path) `E.catch` (const $ return (-1) :: IOError -> IO Integer)
          case size of
            (-1) -> return ()
            _ -> hPutStrLn handle (show size ++ ":" ++ path)

md5sum :: FilePath -> IO (Maybe MD5Digest)
md5sum filePath = do
  handle <- openFile filePath ReadMode
  digest <- (md5sum' handle md5InitialContext) `E.catch` (const $ return Nothing :: IOError -> IO (Maybe MD5Digest))
  hClose handle
  return digest
 where
  blockSize = 64
  md5sum' :: Handle -> MD5Context -> IO (Maybe MD5Digest)
  md5sum' handle context = do
    contents <- BI.hGet handle blockSize
    if contents == BI.empty || BI.length contents /= blockSize
      then return (Just $ md5Finalize context contents)
      else md5sum' handle $ md5Update context contents

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

