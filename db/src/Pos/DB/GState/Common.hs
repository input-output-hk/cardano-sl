{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common functions used by different parts of GState DB.

module Pos.DB.GState.Common
       (
         -- * Helpers
         gsGetBi
       , gsPutBi
       , gsDelete
       , writeBatchGState
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.DB.BatchOp (RocksBatchOp (..), dbWriteBatch')
import           Pos.DB.Class (DBTag (..), MonadDB (dbDelete), MonadDBRead (..))
import           Pos.DB.Functions (dbGetBi, dbPutBi)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

gsGetBi
    :: (MonadDBRead m, Bi v)
    => ByteString -> m (Maybe v)
gsGetBi k = dbGetBi GStateDB k

gsPutBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
gsPutBi = dbPutBi GStateDB

gsDelete :: (MonadDB m) => ByteString -> m ()
gsDelete = dbDelete GStateDB

writeBatchGState :: (RocksBatchOp a, MonadDB m) => [a] -> m ()
writeBatchGState = dbWriteBatch' GStateDB
