-- | In-memory state of Update System.

module Pos.Update.MemState.MemState
       ( MemState (..)
       , MemVar (..)
       , newMemVar
       ) where

import           Control.Concurrent.Lock   (Lock, new)
import           Control.Concurrent.STM    (TVar, newTVarIO)
import           Data.Default              (Default (def))
import           Universum

import           Pos.Crypto                (unsafeHash)
import           Pos.Types                 (HeaderHash, SlotId (..))
import           Pos.Update.MemState.Types (MemPool)
import           Pos.Update.Poll.Types     (PollModifier)

-- TODO: store tip here.
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
