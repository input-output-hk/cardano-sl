-- | Richmen part of LRC DB.

module Pos.DB.Lrc.Richmen
       (
         -- * Getters
         getRichmen

       -- * Operations
       , putRichmen

       -- * Initialization
       , prepareLrcRichmen
       ) where

import           Universum

-- import           Pos.Binary.Class  (encodeStrict)
import           Pos.Binary.Types ()
import           Pos.DB.Class     (MonadDB)
-- import           Pos.DB.Lrc.Common (getBi, putBi)
import           Pos.Types        (EpochIndex)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getRichmen :: MonadDB ssc m => EpochIndex -> m ()
getRichmen = undefined

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen :: MonadDB ssc m => EpochIndex -> m ()
putRichmen = undefined

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen :: MonadDB ssc m => m ()
prepareLrcRichmen = pass

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------
