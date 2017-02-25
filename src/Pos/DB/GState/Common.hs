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
       , getBi
       , putBi
       , delete
       , writeBatchGState

         -- * Operations
       , CommonOp (..)
       ) where

import qualified Data.Text.Buildable
import qualified Database.RocksDB    as Rocks
import           Formatting          (bprint, (%))
import           Universum

import           Pos.Binary.Class    (Bi, encodeStrict)
import           Pos.Crypto          (shortHashF)
import           Pos.DB.Class        (MonadDB, getUtxoDB)
import           Pos.DB.Error        (DBError (DBMalformed))
import           Pos.DB.Functions    (RocksBatchOp (..), rocksDelete, rocksGetBi,
                                      rocksPutBi, rocksWriteBatch)
import           Pos.Types           (HeaderHash)
import           Pos.Util            (maybeThrow)

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
writeBatchGState batch = rocksWriteBatch batch =<< getUtxoDB

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: (MonadDB ssc m) => m HeaderHash
getTip = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from GState DB.
getBot :: (MonadDB ssc m) => m HeaderHash
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
    :: forall ssc m.
       MonadDB ssc m
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

getTipMaybe :: MonadDB ssc m => m (Maybe HeaderHash)
getTipMaybe = getBi tipKey

getBotMaybe :: MonadDB ssc m => m (Maybe HeaderHash)
getBotMaybe = getBi botKey

putTip :: MonadDB ssc m => HeaderHash -> m ()
putTip = putBi tipKey

putBot :: MonadDB ssc m => HeaderHash -> m ()
putBot = putBi botKey
