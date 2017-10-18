{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | This module defines type class for local data storage.

module Pos.Ssc.Class.LocalData
       ( LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       ) where

import qualified Crypto.Random       as Rand
import           System.Wlog         (WithLogger)
import           Universum

import           Pos.Core            (BlockVersionData, EpochIndex, SlotId)
import           Pos.DB.Class        (MonadDBRead)
import           Pos.Lrc.Types       (RichmenStakes)
import           Pos.Slotting.Class  (MonadSlots)
import           Pos.Ssc.Class.Types (Ssc (..))

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type LocalQuery a = forall m . (MonadReader SscLocalData m, WithLogger m) => m a
type LocalUpdate a = forall m . (MonadState SscLocalData m, WithLogger m, Rand.MonadRandom m) => m a

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc => SscLocalDataClass where
    -- | Get local payload to be put into main block and for given
    -- 'SlotId'. If payload for given 'SlotId' can't be constructed,
    -- empty payload can be returned.
    sscGetLocalPayloadQ :: SlotId -> LocalQuery SscPayload
    -- | Make 'SscLocalData' valid for given epoch, richmen and global state.
    -- of best known chain).
    sscNormalizeU :: (EpochIndex, RichmenStakes)
                  -> BlockVersionData
                  -> SscGlobalState
                  -> LocalUpdate ()
    -- | Create new (empty) local data. We are using this function instead of
    -- 'Default' class, because it gives more flexibility. For instance, one
    -- can read something from DB or get current slot.
    sscNewLocalData :: (MonadSlots ctx m, MonadDBRead m) => m SscLocalData
