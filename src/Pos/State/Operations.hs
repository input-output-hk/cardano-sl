-- | Operations on node state.

module Pos.State.Operations
       (
         NodeState
       , mkNodeState

       , addLeaders
       , addEntry
       , adoptBlock
       , createBlock
       , getLeader
       , getLeaders
       , setLeaders
       ) where



import           Data.Default      (def)
import           Data.IORef        (IORef, atomicModifyIORef', newIORef)
import           Protolude         hiding (for, wait, (%))
import           Serokell.Util     ()

import qualified Pos.State.Storage as S
import           Pos.Types.Types   (Block, Entry (..), NodeId (..))

type NodeState = IORef S.Storage
type Update = State S.Storage

mkNodeState :: MonadIO m => m NodeState
mkNodeState = liftIO $ newIORef def

withNodeState :: MonadIO m => NodeState -> Update a -> m a
withNodeState nodeState act =
    liftIO $ atomicModifyIORef' nodeState (swap . runState act)

-- Empty the list of pending entries and create a block
createBlock :: MonadIO m => NodeState -> m Block
createBlock nodeState = withNodeState nodeState S.createBlock

addLeaders :: MonadIO m => NodeState -> Int -> [NodeId] -> m ()
addLeaders nodeState epoch = withNodeState nodeState . S.addLeaders epoch

getLeader :: MonadIO m => NodeState -> Int -> Int -> m (Maybe NodeId)
getLeader nodeState epoch = withNodeState nodeState . S.getLeader epoch

getLeaders :: MonadIO m => NodeState -> Int -> m (Maybe [NodeId])
getLeaders nodeState = withNodeState nodeState . S.getLeaders

addEntry :: MonadIO m => NodeState -> Entry -> m ()
addEntry nodeState = withNodeState nodeState . S.addEntry

adoptBlock :: MonadIO m => NodeState -> Block -> m ()
adoptBlock nodeState = withNodeState nodeState . S.adoptBlock

setLeaders :: MonadIO m => NodeState -> Int -> [NodeId] -> m ()
setLeaders nodeState epoch = withNodeState nodeState . S.setLeaders epoch
