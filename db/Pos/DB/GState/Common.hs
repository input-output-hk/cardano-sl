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
       , isInitialized
       , setInitialized
       , initGStateCommon

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
import           Pos.DB.BatchOp      (RocksBatchOp (..), dbWriteBatch')
import           Pos.DB.Class        (DBTag (GStateDB),
                                      MonadBlockDBGeneric (dbGetBlock, dbGetHeader),
                                      MonadDB (dbDelete), MonadDBRead)
import           Pos.DB.Error        (DBError (DBMalformed))
import           Pos.DB.Functions    (dbGetBi, dbPutBi)
import           Pos.Util.Util       (maybeThrow)

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

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: MonadDBRead m => m HeaderHash
getTip = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe

-- | Get the hash of the first genesis block from GState DB.
getBot :: MonadDBRead m => m HeaderHash
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
       MonadDBRead m
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
-- Initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into GState DB.
initGStateCommon :: (MonadDB m) => HeaderHash -> m ()
initGStateCommon initialTip = do
    putTip initialTip
    putBot initialTip

-- | Checks if gstate is initialized.
isInitialized :: MonadDBRead m => m Bool
isInitialized = do
    (x :: Maybe ()) <- gsGetBi initKey
    pure $ isJust x

-- | Marks gstate as initialized
setInitialized :: MonadDB m => m ()
setInitialized = gsPutBi initKey ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

initKey :: ByteString
initKey = "init/gstate"

tipKey :: ByteString
tipKey = "c/tip"

botKey :: ByteString
botKey = "c/bot"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getTipMaybe :: MonadDBRead m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

getBotMaybe :: MonadDBRead m => m (Maybe HeaderHash)
getBotMaybe = gsGetBi botKey

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

putBot :: MonadDB m => HeaderHash -> m ()
putBot = gsPutBi botKey
