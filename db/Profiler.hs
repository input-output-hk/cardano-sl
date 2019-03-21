module Main (main) where

import System.Environment (getArgs)
import Pos.DB.Epoch.Index (getEpochBlundOffset, mkIndexCache)
import Pos.Core (unsafeMkLocalSlotIndex)
import           Data.Ix (range)
import Data.Word (Word16, Word64)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
  -- this program will test for a basic edgecase the changes add (exceptions mustnt leave it stuck in Opening)
  -- it will then read the given index file in-full, 10 times in a row
  -- TODO: write proper testcases for the changes
  [ index ] <- getArgs
  indexCache <- mkIndexCache 10
  let
    getSlotOffset :: Word16 -> IO ()
    getSlotOffset slot = do
      _offset <- getEpochBlundOffset indexCache index (unsafeMkLocalSlotIndex 21600 slot)
      pure ()
    doFullRun :: Word16 -> IO ()
    doFullRun _ = mapM_ getSlotOffset (range (0,21599))
  err <- try (getEpochBlundOffset indexCache "does-not-exist" (unsafeMkLocalSlotIndex 21600 0)) :: IO (Either SomeException (Maybe Word64))
  print err
  err2 <- try (getEpochBlundOffset indexCache "does-not-exist" (unsafeMkLocalSlotIndex 21600 0)) :: IO (Either SomeException (Maybe Word64))
  print err2
  print "normal run"
  mapM_ doFullRun (range (1,100))
  putStrLn "hello world"
