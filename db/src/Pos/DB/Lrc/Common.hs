
-- | Common functions used by different parts of LRC DB.

module Pos.DB.Lrc.Common
       (
         -- * Getters
         getEpoch

         -- * Initialization
       , prepareLrcCommon

       -- * Helpers
       , dbHasKey
       , getBi
       , putBi
       , putBatch
       , putBatchBi
       , delete
       , toRocksOps

       -- * Operations
       , putEpoch
       ) where

import           Universum

import qualified Database.RocksDB as Rocks

import           Pos.Binary.Class (Bi, serialize')
import           Pos.Core.Slotting (EpochIndex)
import           Pos.DB.Class (DBTag (LrcDB), MonadDB (dbDelete, dbWriteBatch),
                     MonadDBRead (dbGet))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi, dbPutBi)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

dbHasKey :: MonadDBRead m => ByteString -> m Bool
dbHasKey key = isJust <$> dbGet LrcDB key

getBi
    :: (MonadDBRead m, Bi v)
    => ByteString -> m (Maybe v)
getBi = dbGetBi LrcDB

putBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
putBi = dbPutBi LrcDB

putBatch :: MonadDB m => [Rocks.BatchOp] -> m ()
putBatch = dbWriteBatch LrcDB

putBatchBi
     :: (MonadDB m, Bi v)
     => [(ByteString, v)] -> m ()
putBatchBi = putBatch . toRocksOps

delete :: (MonadDB m) => ByteString -> m ()
delete = dbDelete LrcDB

toRocksOps :: Bi v => [(ByteString, v)] -> [Rocks.BatchOp]
toRocksOps ops =
    [Rocks.Put key (serialize' value) | (key, value) <- ops]

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get epoch up to which LRC is definitely known.
getEpoch :: MonadDBRead m => m EpochIndex
getEpoch = maybeThrow (DBMalformed "no epoch in LRC DB") =<< getEpochMaybe

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put epoch up to which all LRC data is computed. Caller must ensure
-- that all LRC data for this epoch has been put already.
putEpoch :: MonadDB m => EpochIndex -> m ()
putEpoch = putBi epochKey

----------------------------------------------------------------------------
-- Common initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into LRC DB.
prepareLrcCommon :: (MonadDB m) => m ()
prepareLrcCommon =
    whenNothingM_ getEpochMaybe $
        putEpoch 0

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

epochKey :: ByteString
epochKey = "c/epoch"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getEpochMaybe :: MonadDBRead m => m (Maybe EpochIndex)
getEpochMaybe = getBi epochKey
