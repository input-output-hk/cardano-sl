{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       , MutPeerState
       , newMutPeerState
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

import           Pos.Block.Network.Server.State (BlockPeerState,
                                                 HasBlockPeerState (blockPeerState))
import           Pos.Types                      (ProtocolVersion)

--

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState ssc = PeerState
    { __blockPeerState :: !(BlockPeerState ssc)
      -- ^ State of block/header logic
    , _peerVersion       :: !(Maybe ProtocolVersion)
      -- ^ Version of the protocol peer uses
    }

-- | Classy lenses for PeerState.
makeClassy ''PeerState

instance Default (PeerState ssc) where
    def =
        PeerState
        { __blockPeerState = def
        , _peerVersion = Nothing
        }

instance HasBlockPeerState (PeerState ssc) ssc where
    blockPeerState = _blockPeerState

-- [CSL-447] TODO remove these types after refactoring `Transfer` out

-- | Mutable PeerState.
type MutPeerState ssc = TVar (PeerState ssc)

-- | Create a new mutable socket state
newMutPeerState :: IO (MutPeerState ssc)
newMutPeerState = newTVarIO def

data PeerStateHolder ssc m = PeerStateHolder
    { getState   :: NodeId -> m (SharedAtomicT m (PeerState ssc))
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
