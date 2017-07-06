-- | Slog-related types.

module Pos.Block.Slog.Types
       ( SlogUndo (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting          (bprint)

import           Pos.Core            (FlatSlotId, slotIdF, unflattenSlotId)

-- | Undo data from Slog, i. e. data which is necessary do rollback a
-- block inside Slog.
--
-- If block is one of the first 'blkSecurityParam' blocks, we don't
-- need to store anything. We also don't need to store anything for
-- genesis blocks. Otherwise we store 'FlatSlotId' of the oldest block
-- from those for which we stored slots before given block was
-- applied.
newtype SlogUndo = SlogUndo
    { getSlogUndo :: Maybe FlatSlotId
    } deriving (NFData)

instance Buildable SlogUndo where
    build (SlogUndo oldSlot) =
        "SlogUndo: " <>
        maybe "<nothing>" (bprint slotIdF . unflattenSlotId) oldSlot
