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

import           Universum

import           Pos.DB.Class        (MonadDB)
import           Pos.Lrc.Types       (Richmen)
import           Pos.Slotting        (MonadSlots)
import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types           (SlotId)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type LocalQuery ssc a =  forall m . (MonadReader (SscLocalData ssc) m) => m a
type LocalUpdate ssc a = forall m . (MonadState (SscLocalData ssc) m) => m a

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc ssc => SscLocalDataClass ssc where
    -- | Get local payload to be put into main block and SlotId for
    -- which it's valid (wrt current tip).
    sscGetLocalPayloadQ :: LocalQuery ssc (SlotId, SscPayload ssc)
    -- | Update LocalData using global data from blocks (last version
    -- of best known chain).
    sscApplyGlobalStateU :: Richmen -> SscGlobalState ssc -> LocalUpdate ssc ()
    -- | Create new (empty) local data. We are using this function instead of
    -- 'Default' class, because it gives more flexibility. For instance, one
    -- can read something from DB or get current slot.
    sscNewLocalData :: (MonadSlots m, MonadDB ssc m) => m (SscLocalData ssc)
