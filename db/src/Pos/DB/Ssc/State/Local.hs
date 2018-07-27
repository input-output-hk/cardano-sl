{-# LANGUAGE Rank2Types #-}

-- | Methods that operate on 'SscLocalData'.

module Pos.DB.Ssc.State.Local
       (
         sscResetLocal
       , sscNewLocalData
       ) where

import           Universum

import           Pos.Chain.Ssc (MonadSscMem, SscLocalData (..), askSscMem,
                     sscLocal)
import           Pos.Core (HasProtocolConstants, SlotId (..))
import           Pos.Core.Slotting (MonadSlots (getCurrentSlot))
import           Pos.DB (MonadDBRead)

-- | Reset local data to empty state.  This function can be used when
-- we detect that something is really bad. In this case it makes sense
-- to remove all local data to be sure it's valid.
sscResetLocal ::
       forall ctx m.
       ( MonadDBRead m
       , MonadSscMem ctx m
       , MonadSlots ctx m
       )
    => m ()
sscResetLocal = do
    emptyLD <- sscNewLocalData
    localVar <- sscLocal <$> askSscMem
    atomically $ writeTVar localVar emptyLD

-- | Create new (empty) local data. We are using this function instead of
-- 'Default' class, because it gives more flexibility. For instance, one
-- can read something from DB or get current slot.
sscNewLocalData :: (MonadSlots ctx m, HasProtocolConstants) => m SscLocalData
sscNewLocalData =
    SscLocalData mempty . siEpoch . fromMaybe slot0 <$> getCurrentSlot <*>
    pure 1
  where
    slot0 = SlotId 0 minBound
