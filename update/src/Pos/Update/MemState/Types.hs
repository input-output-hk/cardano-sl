-- | Types which are stored in memory.

module Pos.Update.MemState.Types
       ( MemPool (..)
       , UpdateProposals
       , LocalVotes

       , MemState (..)
       , MemVar (..)
       , newMemVar
       ) where

import           Universum

import           Data.Default (Default (def))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core (HeaderHash, SlotCount, SlotId (..), UpdateProposals,
                     localSlotIndexMinBound)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Common (getTip)
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Update.Poll.Modifier (PollModifier)
import           Pos.Update.Poll.Types (LocalVotes)

-- | MemPool is data maintained by node to be included into block and
-- relayed to other nodes.
data MemPool = MemPool
    { mpProposals  :: !UpdateProposals
    , mpLocalVotes :: !LocalVotes
    , mpSize       :: !Byte
    } deriving (Show)

instance Default MemPool where
    def = MemPool mempty mempty 2

-- | MemState contains all in-memory data necesary for Update System.
data MemState = MemState
    { msSlot     :: !SlotId
    -- ^ Slot for which data is valid.
    -- In reality EpochIndex should be enough, but we sometimes
    -- overgeneralize things.
    , msTip      :: !HeaderHash
    -- ^ Tip for which data is valid.
    , msPool     :: !MemPool
    -- ^ Pool of data to be included into block.
    , msModifier :: !PollModifier
    -- ^ Modifier of GState corresponding to 'msPool'.
    }

-- | MemVar stores MemState inside 'TVar'.
newtype MemVar = MemVar
    { mvState :: TVar MemState  -- ^ MemState itself.
    }

-- | Create new 'MemVar' using slotting and read-only access to DB.
newMemVar
    :: (MonadIO m, MonadDBRead m, MonadSlots ctx m) => SlotCount -> m MemVar
newMemVar epochSlots = do
    let slot0 = SlotId 0 localSlotIndexMinBound
    msSlot <- fromMaybe slot0 <$> getCurrentSlot epochSlots
    msTip <- getTip
    let ms = MemState { msPool = def, msModifier = mempty, .. }
    liftIO $ MemVar <$> newTVarIO ms
