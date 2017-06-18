{-# LANGUAGE ScopedTypeVariables #-}

-- | Common functions used by different parts of LRC DB.

module Pos.Lrc.DB.Common
       (
         -- * Getters
         getEpoch

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
import           Pos.DB.Class     (DBTag (LrcDB), MonadDB (dbDelete), MonadDBRead)
import           Pos.DB.Error     (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi, dbPutBi)
import           Pos.Util.Util    (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDBRead m, Bi v)
    => ByteString -> m (Maybe v)
getBi = dbGetBi LrcDB

putBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
putBi = dbPutBi LrcDB

delete :: (MonadDB m) => ByteString -> m ()
delete = dbDelete LrcDB

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
prepareLrcCommon :: (MonadDB m, MonadDBRead m) => m ()
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
