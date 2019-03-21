{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pos.DB.Rocks.Functions
import System.Environment
import qualified Database.RocksDB as Rocks
import           Pos.DB.Rocks.Types

main :: IO ()
main = do
  [ dbroot ] <- getArgs
  db@DB{rocksReadOpts,rocksDB} <- openRocksDB dbroot
  let
    createIter :: IO Rocks.Iterator
    createIter = Rocks.createIter rocksDB rocksReadOpts
    releaseIter i = Rocks.releaseIter i
    iterKeyPrefix = ""
    action iter = do
      Rocks.iterSeek iter (iterKeyPrefix)
      produce iter
    produce :: Rocks.Iterator -> IO ()
    produce it = do
      entryStr <- Rocks.iterEntry it
      case entryStr of
        Nothing -> pure ()
        Just e -> do
          print e
          Rocks.iterNext it
          produce it
  iter <- createIter
  action iter
  releaseIter iter
  closeRocksDB db
  pure ()
