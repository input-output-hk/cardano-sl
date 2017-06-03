{-# LANGUAGE ScopedTypeVariables #-}

-- | Common functions used by different parts of LRC DB.

module Pos.Lrc.DB.Common
       (
         -- * Getters
         getEpoch
       , getEpochDefault

         -- * Initialization
       , prepareLrcCommon

       -- * Helpers
       , getBi
       , putBi
       , delete

       -- * Operations
       , putEpoch
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core  ()
import           Pos.Core.Types   (EpochIndex)
import           Pos.DB.Class     (MonadRealDB, getLrcDB)
import           Pos.DB.Error     (DBError (DBMalformed))
import           Pos.DB.Functions (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.Util.Util    (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadRealDB m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getLrcDB

putBi
    :: (MonadRealDB m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getLrcDB

delete :: (MonadRealDB m) => ByteString -> m ()
delete k = rocksDelete k =<< getLrcDB

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get epoch up to which LRC is definitely known.
getEpoch :: MonadRealDB m => m EpochIndex
getEpoch = maybeThrow (DBMalformed "no epoch in LRC DB") =<< getEpochMaybe

-- It's a workaround and I would like to get rid of it in future (@gromak).
-- | Get epoch up to which LRC is definitely known or 0.
getEpochDefault :: MonadRealDB m => m EpochIndex
getEpochDefault = fromMaybe 0 <$> getEpochMaybe

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put epoch up to which all LRC data is computed. Caller must ensure
-- that all LRC data for this epoch has been put already.
putEpoch :: MonadRealDB m => EpochIndex -> m ()
putEpoch = putBi epochKey

----------------------------------------------------------------------------
-- Common initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into LRC DB.
prepareLrcCommon :: MonadRealDB m => m ()
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

getEpochMaybe :: MonadRealDB m => m (Maybe EpochIndex)
getEpochMaybe = getBi epochKey
