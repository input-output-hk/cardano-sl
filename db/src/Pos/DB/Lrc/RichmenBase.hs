{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Pos.DB.Lrc.RichmenBase
       (
         -- * Generalization
         RichmenComponent (..)

         -- * Getters
       , getRichmen

       -- * Operations
       , putRichmen
       ) where

import           Universum

import           Pos.Binary.Class (Bi, serialize')
import           Pos.Chain.Lrc (FullRichmenData, RichmenComponent (..))
import           Pos.Core.Slotting (EpochIndex)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.DB.Lrc.Common (getBi, putBi)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getRichmen
    :: (Bi richmenData, MonadDBRead m)
    => RichmenComponent richmenData
    -> EpochIndex
    -> m (Maybe richmenData)
getRichmen = getBi ... richmenKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen
    :: (Bi richmenData, MonadDB m)
    => RichmenComponent richmenData
    -> EpochIndex
    -> FullRichmenData
    -> m ()
putRichmen rc e = putBi (richmenKey rc e) . rcToData rc

richmenKey :: RichmenComponent richmenData -> EpochIndex -> ByteString
richmenKey rc e = mconcat ["r/", rcTag rc, "/", serialize' e]
