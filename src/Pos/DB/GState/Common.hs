{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common functions used by different parts of GState DB.

module Pos.DB.GState.Common
       ( CommonOp (..)
       , getTip
       , getBot

       , prepareGStateCommon

       , getBi
       , putBi
       , delete
       , writeBatchGState
       ) where

import qualified Database.RocksDB as Rocks
import           Universum

import           Pos.Binary.Class (Bi, encodeStrict)
import           Pos.DB.Class     (MonadDB, getUtxoDB)
import           Pos.DB.Error     (DBError (DBMalformed))
import           Pos.DB.Functions (RocksBatchOp (..), rocksDelete, rocksGetBi, rocksPutBi,
                                   rocksWriteBatch)
import           Pos.Types        (HeaderHash)
import           Pos.Util         (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getUtxoDB

putBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getUtxoDB

delete :: (MonadDB ssc m) => ByteString -> m ()
delete k = rocksDelete k =<< getUtxoDB

writeBatchGState :: (RocksBatchOp a, MonadDB ssc m) => [a] -> m ()
writeBatchGState batch = rocksWriteBatch (map toBatchOp batch) =<< getUtxoDB

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current TIP from Utxo DB.
getTip :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getTip = maybeThrow (DBMalformed "no tip in Utxo DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from Utxo DB
getBot :: (MonadThrow m, MonadDB ssc m) => m (HeaderHash ssc)
getBot = maybeThrow (DBMalformed "no bot in Utxo DB") =<< getBotMaybe

----------------------------------------------------------------------------
-- Common operations
----------------------------------------------------------------------------

data CommonOp ssc = PutTip (HeaderHash ssc)

instance RocksBatchOp (CommonOp ssc) where
    toBatchOp (PutTip h) = Rocks.Put tipKey (encodeStrict h)

----------------------------------------------------------------------------
-- Common initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into GState DB.
prepareGStateCommon
    :: forall ssc m.
       MonadDB ssc m
    => HeaderHash ssc -> m ()
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

getTipMaybe :: MonadDB ssc m => m (Maybe (HeaderHash ssc))
getTipMaybe = getUtxoDB >>= rocksGetBi tipKey

getBotMaybe :: MonadDB ssc m => m (Maybe (HeaderHash ssc))
getBotMaybe = getUtxoDB >>= rocksGetBi botKey

putTip :: MonadDB ssc m => HeaderHash ssc -> m ()
putTip = putBi tipKey

putBot :: MonadDB ssc m => HeaderHash ssc -> m ()
putBot = putBi botKey
