{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module adds extra ecapsulation by hiding acid-state.

module Pos.State.State
       ( NodeState
       , MonadDB (getNodeState)
       , openState
       , openMemState
       , closeState

       -- * Simple getters.
       , getLeaders
       , getBlock
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , addTx
       , processBlock
       , processNewSlot
       ) where

import           Data.Acid         (EventResult, EventState, QueryEvent, UpdateEvent)
import           Universum

import           Pos.Slotting      (MonadSlots, getCurrentSlot)
import           Pos.State.Acidic  (DiskState, tidyState)
import qualified Pos.State.Acidic  as A
import           Pos.State.Storage (ProcessBlockRes (..), Storage)
import           Pos.Types         (Block, EpochIndex, HeaderHash, MainBlockHeader,
                                    SlotId, SlotLeaders, Tx)

-- | NodeState encapsulates all the state stored by node.
type NodeState = DiskState

-- | Convenient type class to avoid passing NodeState throughout the code.
class MonadDB m where
    getNodeState :: m NodeState

instance (Monad m, MonadDB m) => MonadDB (ReaderT r m) where
    getNodeState = lift getNodeState

type WorkModeDB m = (MonadIO m, MonadDB m)

-- | Open NodeState, reading existing state from disk (if any).
openState :: (MonadIO m, MonadSlots m) => Bool -> FilePath -> m NodeState
openState deleteIfExists fp = openStateDo (A.openState deleteIfExists fp)

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState :: (MonadIO m, MonadSlots m) => m NodeState
openMemState = openStateDo A.openMemState

openStateDo :: (MonadIO m, MonadSlots m) => m DiskState -> m NodeState
openStateDo openDiskState = do
    st <- openDiskState
    A.update st . A.ProcessNewSlot =<< getCurrentSlot
    st <$ tidyState st

-- | Safely close NodeState.
closeState :: MonadIO m => NodeState -> m ()
closeState = A.closeState

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, WorkModeDB m)
    => event -> m (EventResult event)
queryDisk e = flip A.query e =<< getNodeState

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, WorkModeDB m)
    => event -> m (EventResult event)
updateDisk e = flip A.update e =<< getNodeState

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: WorkModeDB m => EpochIndex -> m SlotLeaders
getLeaders = queryDisk . A.GetLeaders

-- | Get Block by hash.
getBlock :: WorkModeDB m => HeaderHash -> m (Maybe Block)
getBlock = queryDisk . A.GetBlock

mayBlockBeUseful :: WorkModeDB m => MainBlockHeader -> m Bool
mayBlockBeUseful = queryDisk . A.MayBlockBeUseful

-- | Add transaction to state if it is fully valid. Returns True iff
-- transaction has been added.
addTx :: WorkModeDB m => Tx -> m Bool
addTx = updateDisk . A.AddTx

-- | Notify NodeState about beginning of new slot.
processNewSlot :: WorkModeDB m => SlotId -> m ()
processNewSlot = updateDisk . A.ProcessNewSlot

-- | Process some Block received from the network.
processBlock :: WorkModeDB m => Block -> m ProcessBlockRes
processBlock = updateDisk . A.ProcessBlock
