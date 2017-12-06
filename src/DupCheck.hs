{-# LANGUAGE DeriveDataTypeable #-}

module DupCheck
  ( Options(..)
  , getOptions
  , listDirectories
  , listDuplicates
  , md5sum
  ) where

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
    listedFilesOrDirectories <- listDirectory d
    listedFiles <- mapM listFiles' listedFilesOrDirectories
    listFiles ds $ files ++ concat listedFiles
   where
    listFiles' :: FilePath -> IO [FilePath]
    listFiles' filePath = do
      let path = d ++ "/" ++ filePath
      b <- doesDirectoryExist path
      if b then listDirectories [path] else return [path]

md5sum :: FilePath -> IO (MD5Digest, FilePath)
md5sum file = BI.readFile file >>= \contents -> return (md5 (LBI.fromStrict contents), file)

listDuplicates :: [(MD5Digest, FilePath)] -> [(MD5Digest, [FilePath])]
listDuplicates [] = []
listDuplicates pairs = listDups (head pairs) (tail pairs) []
 where
  listDups :: (MD5Digest, FilePath) -> [(MD5Digest, FilePath)] -> [(MD5Digest, [FilePath])] -> [(MD5Digest, [FilePath])]
  listDups _ [] dups = dups
  listDups (digest, file) rest dups =
    listDups (head rest)
             (if filtered == [] then (tail rest) else filter (\(key, _) -> digest /= key) rest)
             (if filtered == [] then dups else (digest, file:filtered):dups)
   where
    filtered :: [FilePath]
    filtered = map snd (filter (\(key, _) -> key == digest) rest)

