-- | Slog-related types.

module Pos.Block.Slog.Types
       ( LastBlkSlots
       , noLastBlkSlots

       , SlogGState (..)
       , HasSlogGState (..)

       , SlogContext (..)
       , HasSlogContext (..)

       , SlogUndo (..)
       ) where

import           Universum

import           Control.Lens          (makeClassy)
import qualified Data.Text.Buildable
import           Formatting            (bprint)

import           Pos.Core              (FlatSlotId, HasCoreConstants, slotIdF,
                                        unflattenSlotId)
import           Pos.Reporting.Metrics (DistrMonitorState)
import           Pos.Util.Chrono       (OldestFirst (..))

-- | This type contains 'FlatSlotId's of the blocks whose depth is
-- less than 'blkSecurityParam'. 'FlatSlotId' is chosen in favor of
-- 'SlotId', because the main use case is chain quality calculation,
-- for which flat slot is more convenient.
type LastBlkSlots = OldestFirst [] FlatSlotId

noLastBlkSlots :: LastBlkSlots
noLastBlkSlots = OldestFirst []

-- | In-memory representation of Slog (aka BlockExtra) part of
-- GState. Note that it contains only part of BlockExtra.
data SlogGState = SlogGState
    { _sgsLastBlkSlots   :: IORef LastBlkSlots
    -- ^ Slots for which last blocks in our chain were created. This
    -- information is also stored in DB, but we don't want to read it
    -- every time.
    }

makeClassy ''SlogGState

-- | All in-memory data used by Slog.
data SlogContext = SlogContext
    { _scGState         :: !SlogGState
    -- ^ Slots for which last blocks in our chain were created. This
    -- information is also stored in DB, but we don't want to read it
    -- every time.
    , _scCQMonitorState :: !DistrMonitorState
    -- ^ Internal state of 'DistrMonitor' to keep track of chain quality.
    }

makeClassy ''SlogContext

instance HasSlogGState SlogContext where
    slogGState = scGState

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

instance HasCoreConstants => Buildable SlogUndo where
    build (SlogUndo oldSlot) =
        "SlogUndo: " <>
        maybe "<nothing>" (bprint slotIdF . unflattenSlotId) oldSlot
