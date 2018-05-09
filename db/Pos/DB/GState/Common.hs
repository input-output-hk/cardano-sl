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
import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core.Common (ChainDifficulty, HeaderHash)
import           Pos.Core.Configuration (HasCoreConfiguration)
import           Pos.Crypto (shortHashF)
import           Pos.DB.BatchOp (RocksBatchOp (..), dbWriteBatch')
import           Pos.DB.Class (DBTag (GStateDB), MonadDB (dbDelete), MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbGetBi, dbPutBi, dbSerializeValue)
import           Pos.Util.Util (maybeThrow)

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

getTipSomething
    :: forall m smth.
       MonadDBRead m
    => Text -> (HeaderHash -> m (Maybe smth)) -> m smth
getTipSomething smthDescription smthGetter =
    maybe onFailure pure =<< smthGetter =<< getTip
  where
    fmt = "there is no "%stext%" corresponding to tip"
    onFailure = throwM $ DBMalformed $ sformat fmt smthDescription

-- | Get maximum seen chain difficulty (used to prevent improper rollbacks).
getMaxSeenDifficulty :: MonadDBRead m => m ChainDifficulty
getMaxSeenDifficulty =
    maybeThrow (DBMalformed "no max chain difficulty in GState DB") =<<
    getMaxSeenDifficultyMaybe

----------------------------------------------------------------------------
-- Common operations
----------------------------------------------------------------------------

data CommonOp = PutTip HeaderHash | PutMaxSeenDifficulty ChainDifficulty

instance Buildable CommonOp where
    build (PutTip h) =
        bprint ("PutTip ("%shortHashF%")") h
    build (PutMaxSeenDifficulty d) =
        bprint ("PutMaxSeenDifficulty ("%int%")") d

instance HasCoreConfiguration => RocksBatchOp CommonOp where
    toBatchOp (PutTip h) =
        [Rocks.Put tipKey (dbSerializeValue h)]
    toBatchOp (PutMaxSeenDifficulty h) =
        [Rocks.Put maxSeenDifficultyKey (dbSerializeValue h)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- | Put missing initial common data into GState DB.
initGStateCommon :: (MonadDB m) => HeaderHash -> m ()
initGStateCommon initialTip = do
    putTip initialTip
    putMaxSeenDifficulty 0

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

maxSeenDifficultyKey :: ByteString
maxSeenDifficultyKey = "c/maxsd"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getTipMaybe :: MonadDBRead m => m (Maybe HeaderHash)
getTipMaybe = gsGetBi tipKey

getMaxSeenDifficultyMaybe :: MonadDBRead m => m (Maybe ChainDifficulty)
getMaxSeenDifficultyMaybe = gsGetBi maxSeenDifficultyKey

putTip :: MonadDB m => HeaderHash -> m ()
putTip = gsPutBi tipKey

putMaxSeenDifficulty :: MonadDB m => ChainDifficulty -> m ()
putMaxSeenDifficulty = gsPutBi maxSeenDifficultyKey
