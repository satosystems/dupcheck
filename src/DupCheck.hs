{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DupCheck (main) where

import Control.Exception.Safe (catch)
import Control.Monad (when)
import Data.Foldable (foldrM)
import Database.SQLite.Simple ( Connection
                              , FromRow(..)
                              , SQLError
                              , ToRow(..)
                              , close
                              , execute
                              , execute_
                              , open
                              , query_
                              )
import Database.SQLite.Simple.FromRow (field)
import qualified Data.ByteString as BI
import Data.Digest.Pure.MD5 ( MD5Context
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
                              , def
                              , help
                              , name
                              , summary
                              )
import System.IO ( Handle
                 , IOMode(ReadMode)
                 , hClose
                 , hFlush
                 , hPutStr
                 , hPutStrLn
                 , openFile
                 , openBinaryTempFile
                 , stderr
                 , stdout
                 )
import System.Directory ( doesDirectoryExist
                        , getFileSize
                        , getTemporaryDirectory
                        , listDirectory
                        , pathIsSymbolicLink
                        , removeFile
                        )

data File = File String Integer (Maybe String) deriving Show

instance FromRow File where
  fromRow = File <$> field <*> field <*> field

instance ToRow File where
  toRow (File path size digest) = toRow (path, size, digest)

data Options = Options
  { dirs :: [FilePath]
  , debug :: Bool
  } deriving (Show, Data, Typeable)

options :: Options
options = Options
  { dirs = [] &= args
  , debug = def &= name "d" &= help "keep temporary SQLite3 database"
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

listFileSize :: Connection -> [FilePath] -> IO Integer
listFileSize conn directories = listFiles conn (sortUniq $ map removeFileSeparator directories) 0
 where
  removeFileSeparator :: FilePath -> FilePath
  removeFileSeparator filePath | last filePath == '/' = init filePath
                               | otherwise = filePath
  listFiles :: Connection -> [FilePath] -> Integer -> IO Integer
  listFiles _ [] n = return n
  listFiles conn (d:ds) n = do
    listed <- (listDirectory d) `catch` (const $ return [] :: IOError -> IO [FilePath])
    n' <- foldrM listFile n listed
    listFiles conn ds n'
   where
    listFile :: FilePath -> Integer -> IO Integer
    listFile filePath n = do
      let path = d ++ "/" ++ filePath
      isSymbolicLink <- pathIsSymbolicLink path
      if isSymbolicLink then return n else do
        isDirectory <- doesDirectoryExist path
        if isDirectory then listFiles conn [path] n else do
          size <- (getFileSize path) `catch` (const $ return (-1) :: IOError -> IO Integer)
          case size of
            (-1) -> return n
            _ -> do
              execute conn "insert into file values (?, ?, ?)" (File path size Nothing)
              let n' = n + 1
              let num = show n'
              let len = if n == 0 then 0 else length $ show n
              hPutStr stdout $ (take len $repeat '\b') ++ num
              hFlush stdout
              return n'

updateDigest :: Connection -> FilePath -> IO ()
updateDigest conn  filePath = do
  handle <- openFile filePath ReadMode
  digest <- (md5sum handle md5InitialContext) `catch` (const $ return () :: IOError -> IO ())
  hClose handle
  return digest
 where
  blockSize = 64
  md5sum :: Handle -> MD5Context -> IO ()
  md5sum handle context = do
    contents <- BI.hGet handle blockSize
    if contents == BI.empty || BI.length contents /= blockSize
      then execute conn "update file set digest = ? where path = ?" (show (md5Finalize context contents), filePath)
      else md5sum handle $ md5Update context contents

main :: IO ()
main = do
  arg <- getOptions
  case arg of
    (_, Just msg) -> hPutStrLn stderr msg
    (ops, _) -> do
      tempDir <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile tempDir "dupcheck-.sqlite3"
      hClose handle
      (debug ops) `when` putStrLn path
      conn <- open path
      (execute_ conn "create table file (path text primary key, size integer not null, digest text)") `catch` (const $ return () :: SQLError -> IO ())
      putStr "File size checking: "
      listFileSize conn (dirs ops)
      rs1 <- query_ conn "select * from file where size in (select size from file group by size having count(*) > 1)" :: IO [File]
      let maxCount = show $ length rs1
      putStr $ "\nCalculating: 0/" ++ maxCount
      _ <- foldrM (\(File path _ _) n -> do
        updateDigest conn path
        hPutStr stdout $ (take (length (show n ++ "/" ++ maxCount)) $ repeat '\b') ++ show (n + 1) ++ "/" ++ maxCount
        hFlush stdout
        return $ n + 1) 0 rs1
      putStr "\n----------------------------------------\n"
      rs2 <- query_ conn "select * from file table1 where exists (select * from file table2 where table1.digest = table2.digest group by table2.digest having count(table2.digest) > 1) order by table1.digest, table1.path" :: IO [File]
      close conn
      printDups rs2
      (debug ops) `when` removeFile path
 where
  printDups :: [File] -> IO ()
  printDups files = printDups' Nothing files
   where
    printDups' :: Maybe String -> [File] -> IO ()
    printDups' _ [] = return ()
    printDups' Nothing ((File path _ digest):files) = putStrLn path >> printDups' digest files
    printDups' d ((File path _ digest):files) | d == digest = putStrLn path >> printDups' digest files
                                              | otherwise = putStrLn ('\n':path) >> printDups' digest files

