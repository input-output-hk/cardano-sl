{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Class which provides access to NodePeerState.

module Pos.Communication.PeerState
       ( WithPeerState (..)
       , HasPeerState (..)
       , PeerStateCtx
       , PeerStateSnapshot
       , peerStateFromSnapshot
       , module Pos.Communication.Types.State
       , getPeerStateDefault
       , clearPeerStateDefault
       , getAllStatesDefault
       ) where

import           Control.Monad.Trans.Class        (MonadTrans)
import           Data.Default                     (Default (def))
import qualified ListT                            as LT
import           Mockable                         (Mockable, SharedAtomic, SharedAtomicT,
                                                   newSharedAtomic, readSharedAtomic)
import           Pos.Communication.Types.Protocol (NodeId)
import qualified STMContainers.Map                as STM
import           Universum

import           Pos.Communication.Types.State
import           Pos.Util.Util                    ()

type PeerStateCtx' sa = STM.Map NodeId (sa PeerState)

type PeerStateCtx m = PeerStateCtx' (SharedAtomicT m)

-- | PeerStateCtx with no dependency on `m` type.
newtype PeerStateSnapshot = PeerStateSnapshot [(NodeId, PeerState)]

class WithPeerState m where
    getPeerState   :: NodeId -> m (SharedAtomicT m PeerState)
    clearPeerState :: NodeId -> m ()
    getAllStates   :: m PeerStateSnapshot

instance {-# OVERLAPPABLE #-}
    ( WithPeerState m, Monad m, MonadTrans t, Monad (t m)
    , SharedAtomicT m ~ SharedAtomicT (t m) ) =>
        WithPeerState (t m)
  where
    getPeerState = lift . getPeerState
    clearPeerState = lift . clearPeerState
    getAllStates = lift getAllStates

peerStateFromSnapshot
    :: (MonadIO m, Mockable SharedAtomic m)
    => PeerStateSnapshot -> m (PeerStateCtx m)
peerStateFromSnapshot (PeerStateSnapshot snapshot) = do
    ctx <- forM snapshot $ mapM newSharedAtomic
    m   <- liftIO STM.newIO
    atomically $ for_ ctx $ \(k, v) -> STM.insert v k m
    return m

class HasPeerState sa ctx | ctx -> sa where
    peerState :: Lens' ctx (PeerStateCtx' sa)

type PeerStateDefaultEnv ctx m =
    ( MonadIO m, Mockable SharedAtomic m
    , MonadReader ctx m
    , HasPeerState (SharedAtomicT m) ctx )

getPeerStateDefault
    :: PeerStateDefaultEnv ctx m
    => NodeId -> m (SharedAtomicT m PeerState)
getPeerStateDefault nodeId = view peerState >>= \m -> do
    mV <- atomically $ nodeId `STM.lookup` m
    case mV of
      Just v -> return v
      _ -> do
        st <- newSharedAtomic def
        atomically $ do
            mV' <- nodeId `STM.lookup` m
            case mV' of
              Just v -> return v
              _      -> STM.insert st nodeId m $> st

clearPeerStateDefault
    :: PeerStateDefaultEnv ctx m
    => NodeId -> m ()
clearPeerStateDefault nodeId = do
    m <- view peerState
    atomically $ nodeId `STM.delete` m

getAllStatesDefault
    :: PeerStateDefaultEnv ctx m
    => m PeerStateSnapshot
getAllStatesDefault = do
    m <- view peerState
    stream <- atomically $ LT.toList $ STM.stream m
    PeerStateSnapshot <$> forM stream (mapM readSharedAtomic)
