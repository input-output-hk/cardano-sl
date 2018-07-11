
-- | Common functions used by different parts of LRC DB.

module Pos.Lrc.DB.Common
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

import           Pos.Binary.Class (Bi)
import           Pos.Core.Configuration (CoreConfiguration)
import           Pos.Core.Slotting (EpochIndex)
import           Pos.DB (dbSerializeValue)
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
    => CoreConfiguration -> ByteString -> m (Maybe v)
getBi cc = dbGetBi cc LrcDB

putBi
    :: (MonadDB m, Bi v)
    => CoreConfiguration -> ByteString -> v -> m ()
putBi cc = dbPutBi cc LrcDB

putBatch :: MonadDB m => [Rocks.BatchOp] -> m ()
putBatch = dbWriteBatch LrcDB

putBatchBi
     :: (MonadDB m, Bi v)
     => CoreConfiguration -> [(ByteString, v)] -> m ()
putBatchBi cc = putBatch . toRocksOps cc

delete :: (MonadDB m) => ByteString -> m ()
delete = dbDelete LrcDB

toRocksOps :: Bi v => CoreConfiguration -> [(ByteString, v)] -> [Rocks.BatchOp]
toRocksOps cc ops =
    [Rocks.Put key (dbSerializeValue cc value) | (key, value) <- ops]

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get epoch up to which LRC is definitely known.
getEpoch :: MonadDBRead m => CoreConfiguration -> m EpochIndex
getEpoch cc =
  getEpochMaybe cc
     >>= maybeThrow (DBMalformed "no epoch in LRC DB")

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put epoch up to which all LRC data is computed. Caller must ensure
-- that all LRC data for this epoch has been put already.
putEpoch :: MonadDB m => CoreConfiguration -> EpochIndex -> m ()
putEpoch cc = putBi cc epochKey

----------------------------------------------------------------------------
-- Common initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into LRC DB.
prepareLrcCommon :: MonadDB m => CoreConfiguration -> m ()
prepareLrcCommon cc =
    whenNothingM_ (getEpochMaybe cc) $
        putEpoch cc 0

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

epochKey :: ByteString
epochKey = "c/epoch"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getEpochMaybe :: MonadDBRead m => CoreConfiguration -> m (Maybe EpochIndex)
getEpochMaybe cc = getBi cc epochKey
