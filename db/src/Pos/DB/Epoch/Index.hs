{-# LANGUAGE DeriveGeneric #-}

module Pos.DB.Epoch.Index
       ( writeEpochIndex
       , getEpochBlundOffset
       , SlotIndexOffset (..)
       , mkIndexCache
       , clearIndexCache
       , IndexCache
       ) where

import           Universum

import           Control.Monad.STM (retry)
import           Data.Binary (Binary, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Formatting (build, sformat, (%))
import           System.IO (IOMode (..), withBinaryFile)

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


header :: BS.ByteString
header = "Epoch Index v1\n\n"

headerLength :: Num a => a
headerLength = fromIntegral $ BS.length header

hCheckHeader :: FilePath -> ByteString -> IO ()
hCheckHeader fpath body = do
    -- TODO, this doesnt need IO anymore
    let headerBytes = BS.take headerLength body
    when (headerBytes /= header) $ error $ sformat
        ("Invalid header in epoch index file " % build)
        fpath

mkIndexCache :: MonadIO m => Int -> m IndexCache
mkIndexCache maxOpenFiles = do
    cache <- newTVarIO mempty
    pure $ IndexCache cache maxOpenFiles

clearIndexCache :: IndexCache -> IO ()
clearIndexCache indexCache = do
  let
    clearCache :: STM ()
    clearCache = do
      writeTVar (icCache indexCache) mempty
      pure ()
  atomically clearCache

data IndexCache = IndexCache
    { icCache        :: TVar (Map FilePath MaybeCacheEntry)
    , icMaxOpenFiles :: Int
    }

data MaybeCacheEntry = Opening | Opened CacheEntry

newtype CacheEntry = CacheEntry
    { ceBody :: ByteString
    }

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
        . (Builder.byteString header <>)
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

getCachedHandle :: IndexCache -> FilePath -> IO CacheEntry
getCachedHandle indexCache fpath = do
  let
    -- check the Map inside the TVar to see if the file is already open
    -- if it isnt, atomically flag it as Opening
    checkCache :: STM (Maybe CacheEntry)
    checkCache = do
      cache <- readTVar (icCache indexCache)
      case Map.lookup fpath cache of
        Just (Opened cacheEntry) -> pure $ Just cacheEntry
        Just Opening -> retry
        Nothing -> do
          writeTVar (icCache indexCache) (Map.insert fpath Opening cache)
          pure Nothing
    -- open the file, and update the cache
    openAndUpdateCache :: IO CacheEntry
    openAndUpdateCache = do
      body <- BS.readFile fpath
      hCheckHeader fpath body
      let cacheEntry = CacheEntry body
      atomically $ insertNewHandle cacheEntry
      pure cacheEntry
    -- after opening, update the entry to contain the CacheEntry
    insertNewHandle :: CacheEntry -> STM ()
    insertNewHandle cacheEntry = do
      cache <- readTVar (icCache indexCache)
      let
        cache' = Map.insert fpath (Opened cacheEntry) cache
        cache'' = if Map.size cache' > (icMaxOpenFiles indexCache) then
                      -- TODO, implement an LRU cache, to drop things more inteligently
                      Map.drop ( (Map.size cache') - (icMaxOpenFiles indexCache) ) cache'
                    else
                      cache'
      writeTVar (icCache indexCache) cache''
    -- if an exception is caught while opening, cancel the Opening state
    cancelOpening :: STM ()
    cancelOpening = do
      cache <- readTVar (icCache indexCache)
      writeTVar (icCache indexCache) (Map.delete fpath cache)
  maybeEntry <- atomically checkCache
  case maybeEntry of
    Just cacheEntry -> pure cacheEntry
    Nothing -> do
      openAndUpdateCache `onException` atomically cancelOpening

getSlotIndexOffsetN :: IndexCache -> FilePath -> LocalSlotIndex -> IO Word64
getSlotIndexOffsetN indexCache fpath (UnsafeLocalSlotIndex i) = do
    cacheEntry <- getCachedHandle indexCache fpath
    let
      (_, end) = BS.splitAt (headerLength + fromIntegral i * 8) (ceBody cacheEntry)
    pure $ decode $ LBS.fromStrict $ BS.take 8 end

getEpochBlundOffset :: IndexCache -> FilePath -> LocalSlotIndex -> IO (Maybe Word64)
getEpochBlundOffset indexCache fpath lsi = do
    off <- getSlotIndexOffsetN indexCache fpath lsi
    -- 'maxBound' is the sentinel value which means there is no block
    -- in the epoch file for the specified 'LocalSlotIndex'.
    pure $ if off == maxBound then Nothing else Just off
