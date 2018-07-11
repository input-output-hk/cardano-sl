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

import           Pos.Core (CoreConfiguration)
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
    => CoreConfiguration
    -> RichmenComponent richmenData
    -> EpochIndex
    -> m (Maybe richmenData)
getRichmen cc = getBi cc ... richmenKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen
    :: (Bi richmenData, MonadDB m)
    => CoreConfiguration
    -> RichmenComponent richmenData
    -> EpochIndex
    -> FullRichmenData
    -> m ()
putRichmen cc rc e = putBi cc (richmenKey rc e) . rcToData rc

richmenKey :: RichmenComponent richmenData -> EpochIndex -> ByteString
richmenKey rc e = mconcat ["r/", rcTag rc, "/", serialize' e]
