module Test.Pos.DB.Rocks.Functions
       ( tests
       ) where

import           Universum

import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Pos.DB.Rocks.Functions (openRocksDB, closeRocksDB)

checkOpenRocksDB :: FilePath -> IO Bool
checkOpenRocksDB name =
  withSystemTempDirectory "rocksdb-test" $ \ dirPath -> do
    let dbPath = dirPath </> name
    openRocksDB dbPath >>= closeRocksDB
    entries <- listDirectory dirPath
    pure (entries == [name])

tests :: IO Bool
tests = checkOpenRocksDB "ŮŝĕŗŃåɱĕ"
