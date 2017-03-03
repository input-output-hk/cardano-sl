{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodePeerState.

module Pos.Communication.PeerState
       ( WithPeerState (..)
       , runPeerStateHolder
       , PeerStateHolder (..)
       , PeerStateCtx
       , PeerStateSnapshot
       , peerStateFromSnapshot
       , module Pos.Communication.Types.State
       ) where

import           Control.Lens                     (iso)
import           Control.Monad.Base               (MonadBase (..))
import           Control.Monad.Fix                (MonadFix)
import           Control.Monad.Reader             (ReaderT (..))
import           Control.Monad.Trans.Class        (MonadTrans)
import           Data.Default                     (Default (def))
import qualified ListT                            as LT
import           Mockable                         (ChannelT, Counter, Distribution, Gauge,
                                                   Gauge, MFunctor',
                                                   Mockable (liftMockable), Promise,
                                                   SharedAtomic, SharedAtomicT,
                                                   SharedExclusiveT, SharedExclusiveT,
                                                   ThreadId, liftMockableWrappedM,
                                                   newSharedAtomic, readSharedAtomic)
import           Pos.Communication.Types.Protocol (PeerId)
import           Serokell.Util.Lens               (WrappedM (..))
import qualified STMContainers.Map                as STM
import           System.Wlog                      (CanLog, HasLoggerName)
import           Universum

import           Pos.Communication.Types.State

type PeerStateCtx m = STM.Map PeerId (SharedAtomicT m PeerState)

-- | PeerStateCtx with no dependency on `m` type.
newtype PeerStateSnapshot = PeerStateSnapshot [(PeerId, PeerState)]

class WithPeerState m where
    getPeerState   :: PeerId -> m (SharedAtomicT m PeerState)
    clearPeerState :: PeerId -> m ()
    getAllStates   :: m PeerStateSnapshot

instance (Monad m, WithPeerState m) => WithPeerState (ReaderT r m) where
    getPeerState = lift . getPeerState
    clearPeerState = lift . clearPeerState
    getAllStates = lift getAllStates

peerStateFromSnapshot
    :: (MonadIO m, Mockable SharedAtomic m)
    => PeerStateSnapshot -> m (PeerStateCtx m)
peerStateFromSnapshot (PeerStateSnapshot snapshot) = do
    ctx <- forM snapshot $ mapM newSharedAtomic
    m   <- liftIO STM.newIO
    liftIO . atomically $ forM_ ctx $ \(k, v) -> STM.insert v k m
    return m

-- | Wrapper for monadic action which brings 'NodePeerState'.
newtype PeerStateHolder m a = PeerStateHolder
    { getPeerStateHolder :: ReaderT (PeerStateCtx m) m a
    } deriving (Functor, Applicative, Monad,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog,
                MonadFix)

instance MonadTrans PeerStateHolder where
    lift = PeerStateHolder . lift

-- | Run 'PeerStateHolder' action.
runPeerStateHolder :: PeerStateCtx m -> PeerStateHolder m a -> m a
runPeerStateHolder ctx = flip runReaderT ctx . getPeerStateHolder

instance Monad m => WrappedM (PeerStateHolder m) where
    type UnwrappedM (PeerStateHolder m) = ReaderT (PeerStateCtx m) m
    _WrappedM = iso getPeerStateHolder PeerStateHolder

instance MonadBase IO m => MonadBase IO (PeerStateHolder m) where
    liftBase = lift . liftBase

type instance ThreadId (PeerStateHolder m) = ThreadId m
type instance Promise (PeerStateHolder m) = Promise m
type instance SharedAtomicT (PeerStateHolder m) = SharedAtomicT m
type instance Counter (PeerStateHolder m) = Counter m
type instance Distribution (PeerStateHolder m) = Distribution m
type instance SharedExclusiveT (PeerStateHolder m) = SharedExclusiveT m
type instance Gauge (PeerStateHolder m) = Gauge m
type instance ChannelT (PeerStateHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (PeerStateCtx m) m) m
         , MFunctor' d (PeerStateHolder m) (ReaderT (PeerStateCtx m) m)
         ) => Mockable d (PeerStateHolder m) where
    liftMockable = liftMockableWrappedM

instance (MonadIO m, Mockable SharedAtomic m) => WithPeerState (PeerStateHolder m) where
    getPeerState nodeId = (PeerStateHolder ask) >>= \m -> do
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

    clearPeerState nodeId = (PeerStateHolder ask) >>= \m -> liftIO . atomically $ nodeId `STM.delete` m
    getAllStates = PeerStateHolder ask >>= \m -> do
        stream <- liftIO . atomically $ LT.toList $ STM.stream m
        PeerStateSnapshot <$> forM stream (mapM readSharedAtomic)
