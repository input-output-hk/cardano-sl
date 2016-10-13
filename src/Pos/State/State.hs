{-# LANGUAGE TypeFamilies #-}

-- | This module adds extra ecapsulation by hiding acid-state.

module Pos.State.State
       ( NodeState
       , openState
       , openMemState
       , closeState

       -- * Simple getters.
       , getLeaders

       -- * Operations with effects.
       , addTx
       ) where

import           Data.Acid         (EventResult, EventState, QueryEvent, UpdateEvent)
import           Universum

import           Pos.Slotting      (MonadSlots, getCurrentSlot)
import           Pos.State.Acidic  (DiskState, tidyState)
import qualified Pos.State.Acidic  as A
import           Pos.State.Storage (Storage)
import           Pos.Types         (EpochIndex, SlotId, SlotLeaders, Tx)

-- | NodeState encapsulates all the state stored by node.
type NodeState = DiskState

-- | Open NodeState, reading existing state from disk (if any).
openState :: (MonadIO m, MonadSlots m) => Bool -> FilePath -> m NodeState
openState deleteIfExists fp = openStateDo (A.openState deleteIfExists fp)

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState :: (MonadIO m, MonadSlots m) => m NodeState
openMemState = openStateDo A.openMemState

-- TODO: get current slot and do some update before proceeding.
openStateDo :: (MonadIO m, MonadSlots m) => m DiskState -> m NodeState
openStateDo openDiskState = do
    st <- openDiskState
    processNewSlot st =<< getCurrentSlot
    st <$ tidyState st

-- | Safely close NodeState.
closeState :: MonadIO m => NodeState -> m ()
closeState = A.closeState

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
queryDisk = A.query

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
updateDisk = A.update

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: MonadIO m => NodeState -> EpochIndex -> m SlotLeaders
getLeaders ns = queryDisk ns . A.GetLeaders

-- | Add transaction to state if it is fully valid. Returns True iff
-- transaction has been added.
addTx :: MonadIO m => NodeState -> Tx -> m Bool
addTx ns = updateDisk ns . A.AddTx

-- | Notify NodeState about beginning of new slot.
processNewSlot :: MonadIO m => NodeState -> SlotId -> m ()
processNewSlot ns = updateDisk ns . A.ProcessNewSlot
