{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common functions used by different parts of GState DB.

module Pos.DB.GState.Common
       (
         -- * Getters
         getTip
       , getTipSomething
       , getMaxSeenDifficulty
       , getMaxSeenDifficultyMaybe

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

import           Universum

import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, int, sformat, stext, (%))

import           Pos.Binary.Class (Bi)
import           Pos.Core (ChainDifficulty, HeaderHash)
import           Pos.Core.Configuration (CoreConfiguration)
import           Pos.Crypto (shortHashF)
import           Pos.DB.BatchOp (RocksBatchOp (..), dbWriteBatch')
import           Pos.DB.Class (DBTag (GStateDB), MonadDB (dbDelete),
                     MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi, dbPutBi, dbSerializeValue)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Common Helpers
----------------------------------------------------------------------------

gsGetBi
    :: (MonadDBRead m, Bi v)
    => CoreConfiguration -> ByteString -> m (Maybe v)
gsGetBi cc = dbGetBi cc GStateDB

gsPutBi
    :: (MonadDB m, Bi v)
    => CoreConfiguration -> ByteString -> v -> m ()
gsPutBi cc = dbPutBi cc GStateDB

gsDelete :: (MonadDB m) => ByteString -> m ()
gsDelete = dbDelete GStateDB

writeBatchGState :: (RocksBatchOp a, MonadDB m) => CoreConfiguration -> [a] -> m ()
writeBatchGState cc = dbWriteBatch' cc GStateDB

----------------------------------------------------------------------------
-- Common getters
----------------------------------------------------------------------------

-- | Get current tip from GState DB.
getTip :: MonadDBRead m => CoreConfiguration -> m HeaderHash
getTip cc = maybeThrow (DBMalformed "no tip in GState DB") =<< getTipMaybe cc

getTipSomething
    :: forall m smth.
       MonadDBRead m
    => CoreConfiguration -> Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething cc smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip cc
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

-- | Get maximum seen chain difficulty (used to prevent improper rollbacks).
getMaxSeenDifficulty :: MonadDBRead m => CoreConfiguration -> m ChainDifficulty
getMaxSeenDifficulty cc =
    getMaxSeenDifficultyMaybe cc
       >>= maybeThrow (DBMalformed "no max chain difficulty in GState DB")

----------------------------------------------------------------------------
-- Common operations
----------------------------------------------------------------------------

data CommonOp = PutTip HeaderHash | PutMaxSeenDifficulty ChainDifficulty

instance Buildable CommonOp where
    build (PutTip h) =
        bprint ("PutTip ("%shortHashF%")") h
    build (PutMaxSeenDifficulty d) =
        bprint ("PutMaxSeenDifficulty ("%int%")") d

instance RocksBatchOp CommonOp where
    toBatchOp cc (PutTip h) =
        [Rocks.Put tipKey (dbSerializeValue cc h)]
    toBatchOp cc (PutMaxSeenDifficulty h) =
        [Rocks.Put maxSeenDifficultyKey (dbSerializeValue cc h)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into GState DB.
initGStateCommon :: (MonadDB m) => CoreConfiguration -> HeaderHash -> m ()
initGStateCommon cc initialTip = do
    putTip cc initialTip
    putMaxSeenDifficulty cc 0

-- | Checks if gstate is initialized.
isInitialized :: MonadDBRead m => CoreConfiguration -> m Bool
isInitialized cc = do
    (x :: Maybe ()) <- gsGetBi cc initKey
    pure $ isJust x

-- | Marks gstate as initialized
setInitialized :: MonadDB m => CoreConfiguration -> m ()
setInitialized cc = gsPutBi cc initKey ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

initKey :: ByteString
initKey = "init/gstate"

tipKey :: ByteString
tipKey = "c/tip"

maxSeenDifficultyKey :: ByteString
maxSeenDifficultyKey = "c/maxsd"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getTipMaybe :: MonadDBRead m => CoreConfiguration -> m (Maybe HeaderHash)
getTipMaybe cc = gsGetBi cc tipKey

getMaxSeenDifficultyMaybe :: MonadDBRead m => CoreConfiguration -> m (Maybe ChainDifficulty)
getMaxSeenDifficultyMaybe cc = gsGetBi cc maxSeenDifficultyKey

putTip :: MonadDB m => CoreConfiguration -> HeaderHash -> m ()
putTip cc = gsPutBi cc tipKey

putMaxSeenDifficulty :: MonadDB m => CoreConfiguration -> ChainDifficulty -> m ()
putMaxSeenDifficulty cc = gsPutBi cc maxSeenDifficultyKey
