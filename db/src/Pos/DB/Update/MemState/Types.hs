-- | Types which are stored in memory.

module Pos.DB.Update.MemState.Types
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

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Update (LocalVotes, PollModifier)
import           Pos.Core (SlotId (..))
import           Pos.Core.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Core.Update (UpdateProposals)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Common (getTip)

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
    :: (MonadIO m, MonadDBRead m, MonadSlots ctx m)
    => m MemVar
newMemVar = do
    let slot0 = SlotId 0 minBound
    msSlot <- fromMaybe slot0 <$> getCurrentSlot
    msTip <- getTip
    let ms = MemState { msPool = def, msModifier = mempty, .. }
    liftIO $ MemVar <$> newTVarIO ms
