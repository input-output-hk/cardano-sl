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

import           Control.Concurrent.Lock    (Lock, new)
import           Data.Default               (Default (def))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core                   (HeaderHash, SlotId (..))
import           Pos.DB.Class               (MonadDBRead)
import           Pos.DB.GState.Common       (getTip)
import           Pos.Slotting               (MonadSlots (getCurrentSlot))
import           Pos.Update.Core            (LocalVotes, UpdateProposals)
import           Pos.Update.Poll.Modifier   ()
import           Pos.Update.Poll.Types      (PollModifier)

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

-- | MemVar uses concurrency primitives and stores MemState.
data MemVar = MemVar
    { mvState :: !(TVar MemState)  -- ^ MemState itself.
    , mvLock  :: !Lock             -- ^ Lock for modifting MemState.
    }

-- | Create new 'MemVar' using slotting and read-only access to DB.
newMemVar :: (MonadIO m, MonadSlots m, MonadDBRead m) => m MemVar
newMemVar = do
    msSlot <- fromMaybe (SlotId 0 minBound) <$> getCurrentSlot
    msTip <- getTip
    let ms = MemState { msPool = def, msModifier = mempty, .. }
    liftIO $ MemVar <$> newTVarIO ms <*> new
