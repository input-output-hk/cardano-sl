-- | In-memory state of node.

module Pos.State.Memory
       ( MemoryState
       , mkMemoryState
       ) where

import           Universum

import           Pos.Types (CommitmentsMap, SlotId)

data MemoryState = MemoryState
    { -- | Id of last seen slot.
      _msSlotId            :: !SlotId
    , -- | Local set of Commitments. These are valid Commitments which
      -- are known to the node and not stored in blockchain.  It is
      -- useful only for the first `k` slots, after that it should be
      -- discarded.
      _msLocalCommitments  :: !CommitmentsMap
    , -- | Set of Commitments stored in blocks for current epoch. This
      -- can be calculated by mconcating stored commitments, but it
      -- would be inefficient to do it every time we need to know if
      -- commitments is stored in blocks.
      _msGlobalCommitments :: !CommitmentsMap
    }

mkMemoryState :: SlotId -> MemoryState
mkMemoryState slotId =
    MemoryState
    { _msSlotId = slotId
    , _msLocalCommitments = mempty
    , _msGlobalCommitments = mempty
    }
