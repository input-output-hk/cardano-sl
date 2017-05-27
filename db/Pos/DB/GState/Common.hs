{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common functions used by different parts of GState DB.

module Pos.DB.GState.Common
       (
         -- * Getters
         getTip
       , getBot
       , getTipBlock
       , getTipHeader
       , getTipSomething

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
import           Formatting          (bprint, sformat, stext, (%))
import           Universum

import           Pos.Binary.Class    (Bi, encodeStrict)
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (HeaderHash)
import           Pos.Crypto          (shortHashF)
import           Pos.DB.Class        (DBTag (GStateDB),
                                      MonadBlockDBGeneric (dbGetBlock, dbGetHeader),
                                      MonadDB, MonadDBPure, getGStateDB)
import           Pos.DB.Error        (DBError (DBMalformed))
import           Pos.DB.Functions    (RocksBatchOp (..), dbGetBi, rocksDelete, rocksPutBi,
                                      rocksWriteBatch)
import           Pos.Util.Util       (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

gsGetBi
    :: (MonadDBPure m, Bi v)
    => ByteString -> m (Maybe v)
gsGetBi k = dbGetBi GStateDB k

gsPutBi
    :: (MonadDB m, Bi v)
    => ByteString -> v -> m ()
gsPutBi k v = rocksPutBi k v =<< getGStateDB

gsDelete :: (MonadDB m) => ByteString -> m ()
gsDelete k = rocksDelete k =<< getGStateDB

writeBatchGState :: (RocksBatchOp a, MonadDB m) => [a] -> m ()
writeBatchGState batch = rocksWriteBatch batch =<< getGStateDB

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: MonadDBPure m => m HeaderHash
getTip = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from GState DB.
getBot :: MonadDBPure m => m HeaderHash
getBot = maybeThrow (DBMalformed "no bot in GState DB") =<< getBotMaybe

-- | Get 'Block' corresponding to tip.
getTipBlock
    :: forall block header undo m.
       MonadBlockDBGeneric header block undo m
    => m block
getTipBlock = getTipSomething "block" (dbGetBlock @_ @block)

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader
    :: forall block header undo m.
       MonadBlockDBGeneric header block undo m
    => m header
getTipHeader = getTipSomething "header" (dbGetHeader @_ @block)

getTipSomething
    :: forall m smth.
       MonadDBPure m
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

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
prepareGStateCommon :: (MonadDB m, MonadDBPure m) => HeaderHash -> m ()
prepareGStateCommon initialTip = do
    whenNothingM_ getTipMaybe putGenesisTip
    whenNothingM_ getBotMaybe putGenesisBot
  where
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

getTipMaybe :: MonadDBPure m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

getBotMaybe :: MonadDBPure m => m (Maybe HeaderHash)
getBotMaybe = gsGetBi botKey

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

putBot :: MonadDB m => HeaderHash -> m ()
putBot = gsPutBi botKey
