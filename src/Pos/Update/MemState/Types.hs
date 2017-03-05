-- | Types which are stored in memory.

module Pos.Update.MemState.Types
       ( MemPool (..)
       , UpdateProposals
       , LocalVotes

       , MemState (..)
       , MemVar (..)
       , mkMemState
       , newMemVar
       ) where

import           Control.Concurrent.Lock  (Lock, new)
import           Control.Concurrent.STM   (TVar, newTVarIO)
import           Data.Default             (Default (def))
import           Universum

import           Pos.Crypto               (unsafeHash)
import           Pos.Types                (HeaderHash, SlotId (..))
import           Pos.Update.Core          (LocalVotes, UpdateProposals)
import           Pos.Update.Poll.Modifier ()
import           Pos.Update.Poll.Types    (PollModifier)

-- | MemPool is data maintained by node to be included into block and
-- relayed to other nodes.
data MemPool = MemPool
    { mpProposals  :: !UpdateProposals
    , mpLocalVotes :: !LocalVotes
    } deriving (Show)

instance Default MemPool where
    def = MemPool mempty mempty

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

mkMemState :: MemState
mkMemState = MemState
    { msSlot = SlotId 0 0
    , msTip = unsafeHash ("dratuti" :: Text)
    , msPool = def
    , msModifier = def
    }

-- | MemVar uses concurrency primitives and stores MemState.
data MemVar = MemVar
    { mvState :: !(TVar MemState)  -- ^ MemState itself.
    , mvLock  :: !Lock             -- ^ Lock for modifting MemState.
    }

newMemVar
    :: MonadIO m
    => m MemVar
newMemVar = liftIO $ MemVar <$> newTVarIO mkMemState <*> new
