-- | Abstraction layer on top of node state.

module Pos.State.State
       ( NodeState
       , openMemState
       ) where

import           Data.IORef       (IORef, newIORef)
import           Universum

import           Pos.Slotting     (MonadSlots, getCurrentSlot)
import qualified Pos.State.Acidic as A
import           Pos.State.Memory (MemoryState, mkMemoryState)

-- | NodeState is an abstraction on top of node state. It encapsulates
-- memory and disk state into a single type.
data NodeState = NodeState
    { nsMemory :: !(IORef MemoryState)
    , nsDisk   :: !A.DiskState
    }

openMemState :: (MonadIO m, MonadSlots m) => m NodeState
openMemState =
    NodeState <$> (liftIO . newIORef =<< mkMemoryState <$> getCurrentSlot) <*>
    A.openMemState
