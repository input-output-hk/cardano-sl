{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Class which provides access to NodePeerState.

module Pos.Communication.PeerState
       ( WithPeerState (..)
       , runPeerStateHolder
       , PeerStateHolder
       , PeerStateCtx
       , PeerStateSnapshot
       , peerStateFromSnapshot
       , module Pos.Communication.Types.State
       ) where

import           Control.Monad.Reader             (ReaderT (..))
import           Control.Monad.Trans.Class        (MonadTrans)
import           Data.Default                     (Default (def))
import qualified Ether
import qualified ListT                            as LT
import           Mockable                         (Mockable, SharedAtomic, SharedAtomicT,
                                                   newSharedAtomic, readSharedAtomic)
import           Pos.Communication.Types.Protocol (PeerId)
import qualified STMContainers.Map                as STM
import           Universum

import           Pos.Communication.Types.State
import           Pos.Util.Util                    (TaggedTrans, ether)

type PeerStateCtx m = STM.Map PeerId (SharedAtomicT m PeerState)

newtype PeerStateCtx' m = PeerStateCtx
    { unPeerStateCtx :: PeerStateCtx m
    }

-- | PeerStateCtx with no dependency on `m` type.
newtype PeerStateSnapshot = PeerStateSnapshot [(PeerId, PeerState)]

class WithPeerState m where
    getPeerState   :: PeerId -> m (SharedAtomicT m PeerState)
    clearPeerState :: PeerId -> m ()
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

data PeerStateTag

-- | Wrapper for monadic action which brings 'NodePeerState'.
type PeerStateHolder m = Ether.ReaderT PeerStateTag (PeerStateCtx' m) m

-- | Run 'PeerStateHolder' action.
runPeerStateHolder :: PeerStateCtx m -> PeerStateHolder m a -> m a
runPeerStateHolder psc m =
    Ether.runReaderT @PeerStateTag m (PeerStateCtx psc)

instance
    ( MonadIO m, Mockable SharedAtomic m
    , trans ~ ReaderT (PeerStateCtx' m) ) =>
        WithPeerState (TaggedTrans PeerStateTag trans m)
  where
    getPeerState nodeId = ether (asks unPeerStateCtx) >>= \m -> do
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

    clearPeerState nodeId = ether (asks unPeerStateCtx) >>= \m -> atomically $ nodeId `STM.delete` m
    getAllStates = ether (asks unPeerStateCtx) >>= \m -> do
        stream <- atomically $ LT.toList $ STM.stream m
        PeerStateSnapshot <$> forM stream (mapM readSharedAtomic)
