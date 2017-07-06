-- | Slog-related types.

module Pos.Block.Slog.Types
       ( LastBlkSlots
       , noLastBlkSlots
       , SlogContext (..)
       , scLastBlkSlots
       , HasSlogContext (..)

       , SlogUndo (..)
       ) where

import           Universum

import           Control.Lens        (makeLenses)
import qualified Data.Text.Buildable
import           Formatting          (bprint)

import           Pos.Core            (FlatSlotId, slotIdF, unflattenSlotId)
import           Pos.Util.Chrono     (OldestFirst (..))

-- | This type contains 'FlatSlotId's of the blocks whose depth is
-- less than 'blkSecurityParam'. 'FlatSlotId' is chosen in favor of
-- 'SlotId', because the main use case is chain quality calculation,
-- for which flat slot is more convenient.
type LastBlkSlots = OldestFirst [] FlatSlotId

noLastBlkSlots :: LastBlkSlots
noLastBlkSlots = OldestFirst []

-- | All in-memory data used by Slog.
data SlogContext = SlogContext
    { _scLastBlkSlots :: IORef LastBlkSlots
    -- ^ Slots for which last blocks in our chain were created. This
    -- information is also stored in DB, but we don't want to read it
    -- every time.
    }

makeLenses ''SlogContext

-- | Type class encapsulating everything that has 'SlogContext sa'.
class HasSlogContext s where
    slogContextL :: Lens' s SlogContext

instance HasSlogContext SlogContext where
    slogContextL = identity

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
