{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines type class for local data storage.

module Pos.Ssc.Class.LocalData
       ( LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       ) where

import           System.Wlog         (WithLogger)
import           Universum

import           Pos.DB.Class        (MonadDB)
import           Pos.Lrc.Types       (RichmenStake)
import           Pos.Slotting.Class  (MonadSlots)
import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types           (EpochIndex, SlotId)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type LocalQuery ssc a =  forall m . (MonadReader (SscLocalData ssc) m, WithLogger m) => m a
type LocalUpdate ssc a = forall m . (MonadState (SscLocalData ssc) m, WithLogger m) => m a

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc ssc => SscLocalDataClass ssc where
    -- | Get local payload to be put into main block and for given
    -- 'SlotId'.  If payload for given 'SlotId' can't be constructed,
    -- empty payload can be returned.
    sscGetLocalPayloadQ :: SlotId -> LocalQuery ssc (SscPayload ssc)
    -- | Make 'SscLocalData' valid for given epoch, richmen and global state.
    -- of best known chain).
    sscNormalizeU :: EpochIndex
                  -> RichmenStake
                  -> SscGlobalState ssc
                  -> LocalUpdate ssc ()
    -- | Create new (empty) local data. We are using this function instead of
    -- 'Default' class, because it gives more flexibility. For instance, one
    -- can read something from DB or get current slot.
    sscNewLocalData :: (MonadSlots m, MonadDB ssc m) => m (SscLocalData ssc)
