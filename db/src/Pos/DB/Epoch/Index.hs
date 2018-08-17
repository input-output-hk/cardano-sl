{-# LANGUAGE DeriveGeneric #-}

module Pos.DB.Epoch.Index
       ( writeEpochIndex
       , getEpochBlundOffset
       , SlotIndexOffset (..)
       ) where

import           Universum

import           Data.Binary (Binary, decode, encode)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import           Formatting (build, sformat, (%))
import           System.IO (IOMode (..), SeekMode (..), hSeek, withBinaryFile)

import           Pos.Core (LocalSlotIndex (..), SlotCount, localSlotIndices)


-- When we store all blocks for an epoch in a "epoch" file we need a fast and
-- simple way of extracting any single block from the epoch file without decoding
-- the whole file.
--
-- We do this by keeping a separate index file that for each slot, can give the
-- offset in the file where that block occurs. There are 21600 slots/blocks per
-- epoch (10 * blkSecurityParam) and in the first 62 epochs, the smallest number
-- of blocks in an epoch was 21562. The means the most disk storage efficient
-- and quickest to access way to store the slot index to file offset mapping in
-- file is as a dense vector of 64 bit file offsets indexed by the slot index,
-- even if that means that the file has to have sentinel values inserted at empty
-- slot indices.
--
-- We use 'maxBound' as the sentinel value. On read, if we get a value of
-- 'maxBound' we return 'Nothing', otherwise the offset is returned wrapped
-- in a 'Just'.


header :: LBS.ByteString
header = "Epoch Index v1\n\n"

headerLength :: Num a => a
headerLength = fromIntegral $ LBS.length header

hCheckHeader :: FilePath -> Handle -> IO ()
hCheckHeader fpath h = do
    hSeek h AbsoluteSeek 0
    headerBytes <- LBS.hGet h headerLength
    when (headerBytes /= header) $ error $ sformat
        ("Invalid header in epoch index file " % build)
        fpath

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
        . flip Builder.hPutBuilder
        . (Builder.lazyByteString header <>)
        . foldMap (Builder.lazyByteString . encode . sioOffset)
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
getSlotIndexOffsetN fpath (UnsafeLocalSlotIndex i) =
    withBinaryFile fpath ReadMode $ \h -> do
        hCheckHeader fpath h
        hSeek h AbsoluteSeek (headerLength + fromIntegral i * 8)
        decode <$> LBS.hGet h 8

getEpochBlundOffset :: FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlundOffset fpath lsi = do
    off <- getSlotIndexOffsetN fpath lsi
    -- 'maxBound' is the sentinel value which means there is no block
    -- in the epoch file for the specified 'LocalSlotIndex'.
    pure $ if off == maxBound then Nothing else Just off
