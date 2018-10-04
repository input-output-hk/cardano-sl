{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Pos.Lrc.DB.RichmenBase
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
import           Pos.Core.Slotting (EpochIndex)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (FullRichmenData)

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
