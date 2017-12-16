{-# LANGUAGE DeriveDataTypeable #-}

module DupCheck
  ( Options(..)
  , getOptions
  , listDirectories
  , listDuplicates
  , md5sum
  ) where

import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BI
import qualified Data.ByteString.Lazy as LBI
import Data.Digest.Pure.MD5 ( MD5Digest(..)
                            , md5
                            , md5Finalize
                            , md5InitialContext
                            , md5Update
                            )
import Data.List.Unique (sortUniq)
import Data.Version (showVersion)
import Paths_dupcheck (version)
import System.Console.CmdArgs ( Data
                              , Typeable
                              , (&=)
                              , args
                              , cmdArgs
                              , summary
                              )
import System.IO ( IOMode(ReadMode)
                 , hClose
                 , openFile
                 )
import System.Directory ( doesDirectoryExist
                        , listDirectory
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

listDirectories :: [FilePath] -> IO [FilePath]
listDirectories directories = listFiles (sortUniq $ map removeFileSeparator directories) []
 where
  removeFileSeparator :: FilePath -> FilePath
  removeFileSeparator filePath | last filePath == '/' = init filePath
                               | otherwise = filePath
  listFiles :: [FilePath] -> [FilePath] -> IO [FilePath]
  listFiles [] files = return files
  listFiles (d:ds) files = do
    listedFilesOrDirectories <- (listDirectory d) `E.catch` onError
    listedFiles <- mapM listFiles' listedFilesOrDirectories
    listFiles ds $ files ++ concat listedFiles
   where
    onError :: IOError -> IO [FilePath]
    onError _ = return []
    listFiles' :: FilePath -> IO [FilePath]
    listFiles' filePath = do
      let path = d ++ "/" ++ filePath
      b <- doesDirectoryExist path
      if b then listDirectories [path] else return [path]

md5sum :: FilePath -> IO (Maybe MD5Digest, FilePath)
md5sum filePath = readFile' filePath >>= \mc -> return (md5' mc, filePath)
 where
  md5' :: Maybe BI.ByteString -> Maybe MD5Digest
  md5' Nothing = Nothing
  md5' (Just contents) = Just (md5 (LBI.fromStrict contents))
  readFile' :: FilePath -> IO (Maybe BI.ByteString)
  readFile' filePath = (BI.readFile filePath >>= return . Just) `E.catch` onError
   where
    onError :: IOError -> IO (Maybe BI.ByteString)
    onError _ = return Nothing

listDuplicates :: [(MD5Digest, FilePath)] -> [(MD5Digest, [FilePath])]
listDuplicates [] = []
listDuplicates pairs = listDups (head pairs) (tail pairs) []
 where
  listDups :: (MD5Digest, FilePath) -> [(MD5Digest, FilePath)] -> [(MD5Digest, [FilePath])] -> [(MD5Digest, [FilePath])]
  listDups _ [] dups = dups
  listDups (digest, file) rest dups = case removedDuplicates of
    [] -> added
    (x:xs) -> listDups x
      (if filtered == [] then xs else filter (\(key, _) -> digest /= key) xs)
      added
   where
    filtered :: [FilePath]
    filtered = map snd (filter (\(key, _) -> key == digest) rest)
    removedDuplicates :: [(MD5Digest, FilePath)]
    removedDuplicates = filter (\(key, _) -> key /= digest) rest
    added :: [(MD5Digest, [FilePath])]
    added = if filtered == [] then dups else (digest, file:filtered):dups

