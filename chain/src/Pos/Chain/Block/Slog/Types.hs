-- | Slog-related types.

module Pos.Chain.Block.Slog.Types
       ( LastBlkSlots
       , LastSlotInfo (..)
       , noLastBlkSlots

       , ConsensusEraLeaders (..)

       , SlogGState (..)
       , HasSlogGState (..)

       , SlogContext (..)
       , HasSlogContext (..)

       , SlogUndo (..)
       , buildSlogUndo
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Lens (makeClassy)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import           Formatting (Format, bprint, int, later, string, (%))
import qualified Formatting.Buildable as Buildable

import           System.Metrics.Label (Label)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core (BlockCount, ChainDifficulty, EpochIndex, FlatSlotId,
                     LocalSlotIndex, SlotCount, SlotLeaders, StakeholderId,
                     slotIdF, unflattenSlotId)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Reporting (MetricMonitorState)
import           Pos.Crypto (PublicKey (..))


data LastSlotInfo = LastSlotInfo
    { lsiFlatSlotId       :: !FlatSlotId
    -- ^ The flattened SlotId of this block.
    , lsiLeaderPubkeyHash :: !PublicKey
    -- ^ The hash of the public key of the slot leader for this slot.
    } deriving (Eq, Show, Generic)

instance Buildable LastSlotInfo where
    build (LastSlotInfo i (PublicKey pk)) =
        bprint ( "LastSlotInfo "% int %" "% string)
            i (take 16 . BS.unpack . B16.encode $ CC.xpubPublicKey pk)

instance NFData LastSlotInfo

-- | This type contains 'FlatSlotId's of the blocks whose depth is
-- less than 'blkSecurityParam'. 'FlatSlotId' is chosen in favor of
-- 'SlotId', because the main use case is chain quality calculation,
-- for which flat slot is more convenient.
-- Version 1 of this data type was:
--      type LastBlkSlots = OldestFirst [] FlatSlotId
type LastBlkSlots = OldestFirst [] LastSlotInfo


noLastBlkSlots :: LastBlkSlots
noLastBlkSlots = OldestFirst []

-- | This data type is used for block verification. It specifies which slot
-- leader verification algorithm to use and the parameters required to do so.
data ConsensusEraLeaders
    -- | A follow-the-satoshi slot leader schedule for some epoch.
    = OriginalLeaders SlotLeaders
    -- | An OBFT round-robin slot leader schedule for some epoch.
    | ObftStrictLeaders SlotLeaders
    -- | A 'Set' of acceptable slot leaders for some epoch, the value of k,
    -- and the last k blocks which have been minted.
    -- We need to specify k since it's not guaranteed that 'LastBlkSlots' is
    -- of length k.
    | ObftLenientLeaders (Set StakeholderId) BlockCount LastBlkSlots
    deriving (Eq, Show, Generic)

instance NFData ConsensusEraLeaders

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
    { _scGState                 :: !SlogGState
    -- ^ Slots for which last blocks in our chain were created. This
    -- information is also stored in DB, but we don't want to read it
    -- every time.
    , _scCQkMonitorState        :: !(MetricMonitorState Double)
    -- ^ Internal state of 'MetricMonitor' to keep track of chain
    -- quality for last 'k' blocks.
    , _scCQOverallMonitorState  :: !(MetricMonitorState Double)
    -- ^ Internal state of 'MetricMonitor' to keep track of overall chain
    -- quality.
    , _scCQFixedMonitorState    :: !(MetricMonitorState Double)
    -- ^ Internal state of 'MetricMonitor' to keep track of chain
    -- quality for fixed amount of time.
    , _scDifficultyMonitorState :: !(MetricMonitorState ChainDifficulty)
    -- ^ Internal state of 'MetricMonitor' to keep track of overall
    -- chain difficulty (i. e. total number of main blocks created so far).
    , _scEpochMonitorState      :: !(MetricMonitorState EpochIndex)
    -- ^ Internal state of 'MetricMonitor' to keep track of current epoch.
    , _scLocalSlotMonitorState  :: !(MetricMonitorState LocalSlotIndex)
    -- ^ Internal state of 'MetricMonitor' to keep track of current local slot.
    , _scGlobalSlotMonitorState :: !(MetricMonitorState FlatSlotId)
    -- ^ Internal state of 'MetricMonitor' to keep track of current global slot.
    , _scCrucialValuesLabel     :: !Label
    -- ^ A 'Label' for crucial values.
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
    } deriving (Eq, Show, NFData, Generic)

buildSlogUndo :: SlotCount -> Format r (SlogUndo -> r)
buildSlogUndo epochSlots = later $ \(SlogUndo oldSlot) ->
    "SlogUndo: " <>
    maybe "<nothing>" (bprint slotIdF . unflattenSlotId epochSlots) oldSlot

-- TH derived instances at the end of the file.

deriveSimpleBi ''SlogUndo [
    Cons 'SlogUndo [
        Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]

deriveSimpleBi ''LastSlotInfo [
    Cons 'LastSlotInfo [
        Field [| lsiFlatSlotId :: FlatSlotId |],
        Field [| lsiLeaderPubkeyHash :: PublicKey |]
        ]
    ]
