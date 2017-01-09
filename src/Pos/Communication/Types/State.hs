{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( SocketState
       , MutSocketState
       , newMutSocketState
       , peerVersion
       , StateHolder(..)
       , newStateHolder
       ) where

import           Control.Concurrent.STM         (TVar, newTVarIO)
import           Control.Lens                   (makeClassy)
import           Data.Default                   (Default (def))
import           Mockable                       (Mockable, SharedAtomic, SharedAtomicT,
                                                 newSharedAtomic)
import           Node                           (NodeId)
import qualified STMContainers.Map              as STM
import           Universum

import           Pos.Block.Network.Server.State (BlockSocketState,
                                                 HasBlockSocketState (blockSocketState))
import           Pos.Types                      (ProtocolVersion)

--

-- | SocketState type aggregates socket states needed for different
-- parts of system.
data SocketState ssc = SocketState
    { __blockSocketState :: !(BlockSocketState ssc)
      -- ^ State of block/header logic
    , _peerVersion       :: !(Maybe ProtocolVersion)
      -- ^ Version of the protocol peer uses
    }

-- | Classy lenses for SocketState.
makeClassy ''SocketState

instance Default (SocketState ssc) where
    def =
        SocketState
        { __blockSocketState = def
        , _peerVersion = Nothing
        }

instance HasBlockSocketState (SocketState ssc) ssc where
    blockSocketState = _blockSocketState

-- [CSL-447] TODO remove these types after refactoring `Transfer` out

-- | Mutable SocketState.
type MutSocketState ssc = TVar (SocketState ssc)

-- | Create a new mutable socket state
newMutSocketState :: IO (MutSocketState ssc)
newMutSocketState = newTVarIO def

data PeerStateHolder ssc m = PeerStateHolder
    { getState   :: NodeId -> m (SharedAtomicT m (SocketState ssc))
    , clearState :: NodeId -> m ()
    }

newStateHolder :: (MonadIO m, Mockable SharedAtomic m) => m (StateHolder ssc m)
newStateHolder = do
    m <- liftIO STM.newIO
    let getState nodeId = do
          mV <- liftIO . atomically $ nodeId `STM.lookup` m
          case mV of
            Just v -> return v
            _ -> do
              st <- newSharedAtomic def
              liftIO . atomically $ do
                  mV' <- nodeId `STM.lookup` m
                  case mV' of
                    Just v -> return v
                    _      -> STM.insert st nodeId m $> st
        clearState nodeId = liftIO . atomically $ nodeId `STM.delete` m
    pure $ StateHolder getState clearState
