{-# LANGUAGE TemplateHaskell #-}

-- | Storage with node local state.

module Pos.State.Storage
       (
         Storage
       , State
       , pendingEntries
       , epochLeaders
       , blocks

       , Update
       , addLeaders
       , createBlock
       , getLeader

       , withNodeState
       ) where

import           Control.Lens             (at, ix, makeLenses, preuse, use, (%=), (.=),
                                           (<<.=))
import           Crypto.Hash              (hashlazy)
import qualified Data.Binary              as Bin (encode)
import           Data.Default             (Default, def)
import           Data.Fixed               (div')
import           Data.IORef               (IORef, atomicModifyIORef', modifyIORef',
                                           newIORef, readIORef, writeIORef)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set (fromList, insert, toList, (\\))
import qualified Data.Text                as T
import           Formatting               (build, int, sformat, (%))
import           Protolude                hiding (for, wait, (%))
import           System.IO.Unsafe         (unsafePerformIO)
import           System.Random            (randomIO, randomRIO)

import           Control.TimeWarp.Logging (LoggerName (..), logError, logInfo,
                                           setLoggerName, usingLoggerName)
import           Control.TimeWarp.Timed   (Microsecond, for, fork, ms, repeatForever,
                                           runTimedIO, sec, sleepForever, till,
                                           virtualTime, wait)
import           Serokell.Util            ()

import           Pos.Constants            (epochSlots, n, slotDuration, t)
import           Pos.Crypto               (encrypt, shareSecret)
import           Pos.Types.Types          (Block, Entry (..), Message (..), NodeId (..),
                                           displayEntry, node)
import           Pos.WorkMode             (RealMode, WorkMode)

data Storage = Storage
    { -- | List of entries that the node has received but that aren't included
      -- into any block yet
      _pendingEntries :: Set Entry
      -- | Leaders for epochs (currently it just stores leaders for all
      -- epochs, but we really only need the leader list for this epoch and
      -- the next epoch)
    , _epochLeaders   :: Map Int [NodeId]
      -- | Blocks
    , _blocks         :: [Block]
    }

makeLenses ''Storage

instance Default Storage where
    def =
        Storage
        { _pendingEntries = mempty
        , _epochLeaders = mempty
        , _blocks = mempty
        }

type NodeState = IORef Storage

type Update = State Storage

withNodeState :: MonadIO m => NodeState -> Update a -> m a
withNodeState nodeState act =
    liftIO $ atomicModifyIORef' nodeState (swap . runState act)

-- Empty the list of pending entries and create a block
createBlock :: MonadIO m => NodeState -> m Block
createBlock nodeState =
    withNodeState nodeState $
    do es <- pendingEntries <<.= mempty
       return (Set.toList es)

addLeaders :: MonadIO m => NodeState -> Int -> [NodeId] -> m ()
addLeaders nodeState epoch leaders =
    withNodeState nodeState $
    pendingEntries %= Set.insert (ELeaders (epoch + 1) leaders)

getLeader :: MonadIO m => NodeState -> Int -> Int -> m (Maybe NodeId)
getLeader nodeState epoch slot =
    withNodeState nodeState $ preuse (epochLeaders . ix epoch . ix slot)
