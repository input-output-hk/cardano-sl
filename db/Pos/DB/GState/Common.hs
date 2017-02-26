{-# LANGUAGE ScopedTypeVariables #-}

-- | Common functions used by different parts of GState DB.

module Pos.DB.GState.Common
       (
         -- * Getters
         getTip
       , getBot

         -- * Initialization
       , prepareGStateCommon

         -- * Helpers
       , gsGetBi
       , gsPutBi
       , gsDelete
       , writeBatchGState

         -- * Operations
       , CommonOp (..)
       ) where

import qualified Data.Text.Buildable
import qualified Database.RocksDB    as Rocks
import           Formatting          (bprint, (%))
import           Universum

import           Pos.Binary.Class    (Bi, encodeStrict)
import           Pos.Binary.Crypto   ()
import           Pos.Crypto          (shortHashF)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Error        (DBError (DBMalformed))
import           Pos.DB.Functions    (RocksBatchOp (..), rocksDelete, rocksGetBi,
                                      rocksPutBi, rocksWriteBatch)
import           Pos.Types.Core      (HeaderHash)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

gsGetBi
    :: (MonadDB m, Bi v)
    => ByteString -> m (Maybe v)
gsGetBi k = rocksGetBi k =<< getUtxoDB

gsPutBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
gsPutBi k v = rocksPutBi k v =<< getUtxoDB

gsDelete :: (MonadDB m) => ByteString -> m ()
gsDelete k = rocksDelete k =<< getUtxoDB

writeBatchGState :: (RocksBatchOp a, MonadDB m) => [a] -> m ()
writeBatchGState batch = rocksWriteBatch batch =<< getUtxoDB

----------------------------------------------------------------------------
-- TODO: remove
----------------------------------------------------------------------------

maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: (MonadDB m) => m HeaderHash
getTip = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from GState DB.
getBot :: (MonadDB m) => m HeaderHash
getBot = maybeThrow (DBMalformed "no bot in GState DB") =<< getBotMaybe

----------------------------------------------------------------------------
-- Common operations
----------------------------------------------------------------------------

data CommonOp = PutTip HeaderHash

instance Buildable CommonOp where
    build (PutTip h) = bprint ("PutTip ("%shortHashF%")") h

instance RocksBatchOp CommonOp where
    toBatchOp (PutTip h) = [Rocks.Put tipKey (encodeStrict h)]

----------------------------------------------------------------------------
-- Common initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into GState DB.
prepareGStateCommon
    :: forall m.
       MonadDB m
    => HeaderHash -> m ()
prepareGStateCommon initialTip = do
    putIfEmpty getTipMaybe putGenesisTip
    putIfEmpty getBotMaybe putGenesisBot
  where
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    putGenesisTip = putTip initialTip
    putGenesisBot = putBot initialTip

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

tipKey :: ByteString
tipKey = "c/tip"

botKey :: ByteString
botKey = "c/bot"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getTipMaybe :: MonadDB m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

getBotMaybe :: MonadDB m => m (Maybe HeaderHash)
getBotMaybe = gsGetBi botKey

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

putBot :: MonadDB m => HeaderHash -> m ()
putBot = gsPutBi botKey
