{-# LANGUAGE RankNTypes #-}

-- | Common functionality related to Misc DB.

module Pos.DB.Misc.Common
       ( miscGetBi
       , miscPutBi
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Core.Configuration (CoreConfiguration)
import           Pos.DB.Class (DBTag (..), MonadDB, MonadDBRead)
import           Pos.DB.Functions (dbGetBi, dbPutBi)

miscGetBi :: (MonadDBRead m, Bi v) => CoreConfiguration -> ByteString -> m (Maybe v)
miscGetBi cc = dbGetBi cc MiscDB

miscPutBi :: (MonadDB m, Bi v) => CoreConfiguration -> ByteString -> v -> m ()
miscPutBi cc = dbPutBi cc MiscDB
