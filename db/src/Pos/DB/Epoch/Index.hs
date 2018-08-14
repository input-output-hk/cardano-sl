{-# LANGUAGE DeriveGeneric #-}

module Pos.DB.Epoch.Index
       ( writeEpochIndex
       , getEpochBlockOffset
       , SlotIndexOffset (..)
       ) where

import           Universum

import           Data.Binary (Binary, decode, encode)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           System.IO (IOMode (..), SeekMode (..), hSeek, withBinaryFile)

import           Pos.Core (LocalSlotIndex (..), SlotCount, localSlotIndices)

data SlotIndexOffset = SlotIndexOffset
    { sioSlotIndex :: !Word16
    , sioOffset    :: !Word64
    } deriving (Eq, Generic, Show)

instance Binary SlotIndexOffset

-- | Write a list of @SlotIndexOffset@s to a dense @Binary@ representation
--
--   To make it dense we pad the list with @maxBound :: Word64@ whenever we see
--   a missing @LocalSlotIndex@
writeEpochIndex :: SlotCount -> FilePath -> [SlotIndexOffset] -> IO ()
writeEpochIndex epochSlots path =
    withBinaryFile path WriteMode
        . flip B.hPutBuilder
        . foldMap (B.lazyByteString . encode . sioOffset)
        . padIndex epochSlots

-- | Pad a list of @SlotIndexOffset@s ordered by @LocalSlotIndex@
padIndex :: SlotCount -> [SlotIndexOffset] -> [SlotIndexOffset]
padIndex epochSlots = go
    (   flip SlotIndexOffset maxBound
    .   getSlotIndex
    <$> localSlotIndices epochSlots
    )
  where
    go [] _  = []
    go xs [] = xs
    go (x : xs) (y : ys) | sioSlotIndex x == sioSlotIndex y = y : go xs ys
                         | otherwise                        = x : go xs (y : ys)

getSlotIndexOffsetN :: FilePath -> LocalSlotIndex -> IO Word64
getSlotIndexOffsetN path (UnsafeLocalSlotIndex i) =
    withBinaryFile path ReadMode $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral i * 8)
        decode <$> BL.hGet h 8

getEpochBlockOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlockOffset fpath lsi = do
    off <- getSlotIndexOffsetN fpath lsi
    pure $ if off == maxBound then Nothing else Just off
