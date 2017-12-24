{-# LANGUAGE DeriveDataTypeable #-}

module DupCheck
  ( Options(..)
  , getOptions
  , listFileSize
  , md5sum
  ) where

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
import System.IO ( Handle
                 , IOMode(ReadMode)
                 , hClose
                 , hPutStrLn
                 , openFile
                 )
import System.Directory ( doesDirectoryExist
                        , getFileSize
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
      b <- doesDirectoryExist path
      if b then listFileSize handle [path] else do
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

