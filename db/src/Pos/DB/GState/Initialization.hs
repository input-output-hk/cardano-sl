module Pos.DB.GState.Initialization
       ( isInitialized
       , setInitialized
       , initGStateCommon
       ) where

import           Universum

import           Pos.Chain.Block (HeaderHash)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.DB.GState.ChainDifficulty (putMaxSeenDifficulty)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.DB.GState.Tip (putTip)

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
