{-# LANGUAGE TypeFamilies #-}

-- | Abstraction layer on top of node state.

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
import           Data.IORef        (IORef, newIORef)
import           Universum

import           Pos.Crypto        (PublicKey)
import           Pos.Slotting      (MonadSlots, getCurrentSlot)
import           Pos.State.Acidic  (DiskState)
import qualified Pos.State.Acidic  as A
import           Pos.State.Memory  (MemoryState, mkMemoryState)
import           Pos.State.Storage (Storage)
import           Pos.Types         (EpochIndex, Tx)

-- | NodeState is an abstraction on top of node state. It encapsulates
-- memory and disk state into a single type.
--
-- WARNING: be careful about ACID properties. Updates which affect both states
-- can't be atomic. It must be taken into account.
data NodeState = NodeState
    { nsMemory :: !(IORef MemoryState)
    , nsDisk   :: !DiskState
    }

-- | Open NodeState, reading existing state from disk (if any).
openState :: (MonadIO m, MonadSlots m) => Bool -> FilePath -> m NodeState
openState deleteIfExists fp = openStateDo (A.openState deleteIfExists fp)

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState :: (MonadIO m, MonadSlots m) => m NodeState
openMemState = openStateDo A.openMemState

openStateDo :: (MonadIO m, MonadSlots m) => m DiskState -> m NodeState
openStateDo openDiskState =
    NodeState <$> (liftIO . newIORef =<< mkMemoryState <$> getCurrentSlot) <*>
    openDiskState

-- | Safely close NodeState.
closeState :: MonadIO m => NodeState -> m ()
closeState = A.closeState . nsDisk

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
queryDisk = A.query . nsDisk

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
updateDisk = A.update . nsDisk

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: MonadIO m => NodeState -> EpochIndex -> m [PublicKey]
getLeaders ns = queryDisk ns . A.GetLeaders

-- | Add transaction to state if it is fully valid. Returns True iff
-- transaction has been added.
addTx :: MonadIO m => NodeState -> Tx -> m Bool
addTx ns = updateDisk ns . A.AddTx
