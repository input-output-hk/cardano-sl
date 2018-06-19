module Benchmark.FileSystem 
  ( 
    copy,
    removeTreeIfExists,
    exists,
    getTemporaryDirectory,
    module Filesystem,
    module Filesystem.Path.CurrentOS
  )
  where

import Benchmark.Prelude hiding (stripPrefix, last)
import Filesystem.Path.CurrentOS
import Filesystem
import qualified System.Directory as Directory
import Debug.Trace
import qualified Data.List as List



removeTreeIfExists :: FilePath -> IO ()
removeTreeIfExists path = removeTree path `catch` \e -> case e of
  _ | isDoesNotExistError e -> return ()
    | otherwise -> throwIO e

exists :: FilePath -> IO Bool
exists path = do
  isDir <- isDirectory path
  isFile <- isFile path
  return $ isDir || isFile 

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = 
  Directory.getTemporaryDirectory >>= return . decodeString

copy :: FilePath -> FilePath -> IO ()
copy from to = do
  isDir <- isDirectory from
  if isDir
    then copyDirectory from to
    else copyFile from to

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory path path' = do
  members <- listDirectory path
  let members' = do
        member <- members
        let relative = 
              fromMaybe (error "Unexpectedly empty member path") $
              last member
        return $ path' <> relative
  sequence_ $ zipWith copy members members'

last :: FilePath -> Maybe FilePath
last p = case splitDirectories p of
  [] -> Nothing
  l -> Just $ List.last l
