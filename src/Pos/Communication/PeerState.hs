{-# LANGUAGE ScopedTypeVariables  #-}
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
       ) where

import           Control.Lens                  (iso)
import           Control.Monad.Base            (MonadBase (..))
import           Control.Monad.Fix             (MonadFix)
import           Control.Monad.Reader          (ReaderT (..))
import           Control.Monad.Trans.Class     (MonadTrans)
import           Data.Default                  (Default (def))
import qualified ListT                         as LT
import           Mockable                      (ChannelT, Counter, Distribution, Gauge,
                                                Gauge, MFunctor', Mockable (liftMockable),
                                                Promise, SharedAtomic, SharedAtomicT,
                                                SharedExclusiveT, SharedExclusiveT,
                                                ThreadId, liftMockableWrappedM,
                                                newSharedAtomic, readSharedAtomic)
import           Pos.Communication.Protocol    (NodeId)
import           Serokell.Util.Lens            (WrappedM (..))
import qualified STMContainers.Map             as STM
import           System.Wlog                   (CanLog, HasLoggerName)
import           Universum

import           Pos.Communication.Types.State (PeerState)

type PeerStateCtx ssc m = STM.Map NodeId (SharedAtomicT m (PeerState ssc))

-- | PeerStateCtx with no dependency on `m` type.
newtype PeerStateSnapshot ssc = PeerStateSnapshot [(NodeId, PeerState ssc)]

class WithPeerState ssc m | m -> ssc where
    getPeerState   :: NodeId -> m (SharedAtomicT m (PeerState ssc))
    clearPeerState :: NodeId -> m ()
    getAllStates   :: m (PeerStateSnapshot ssc)

instance (Monad m, WithPeerState ssc m) => WithPeerState ssc (ReaderT r m) where
    getPeerState = lift . getPeerState
    clearPeerState = lift . clearPeerState
    getAllStates = lift getAllStates

peerStateFromSnapshot
    :: (MonadIO m, Mockable SharedAtomic m)
    => PeerStateSnapshot ssc -> m (PeerStateCtx ssc m)
peerStateFromSnapshot (PeerStateSnapshot snapshot) = do
    ctx <- forM snapshot $ mapM newSharedAtomic
    m   <- liftIO STM.newIO
    liftIO . atomically $ forM_ ctx $ \(k, v) -> STM.insert v k m
    return m

-- | Wrapper for monadic action which brings 'NodePeerState'.
newtype PeerStateHolder ssc m a = PeerStateHolder
    { getPeerStateHolder :: ReaderT (PeerStateCtx ssc m) m a
    } deriving (Functor, Applicative, Monad,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog,
                MonadFix)

instance MonadTrans (PeerStateHolder ssc) where
    lift = PeerStateHolder . lift

-- | Run 'PeerStateHolder' action.
runPeerStateHolder :: PeerStateCtx ssc m -> PeerStateHolder ssc m a -> m a
runPeerStateHolder ctx = flip runReaderT ctx . getPeerStateHolder

instance Monad m => WrappedM (PeerStateHolder ssc m) where
    type UnwrappedM (PeerStateHolder ssc m) = ReaderT (PeerStateCtx ssc m) m
    _WrappedM = iso getPeerStateHolder PeerStateHolder

instance MonadBase IO m => MonadBase IO (PeerStateHolder ssc m) where
    liftBase = lift . liftBase

type instance ThreadId (PeerStateHolder ssc m) = ThreadId m
type instance Promise (PeerStateHolder ssc m) = Promise m
type instance SharedAtomicT (PeerStateHolder ssc m) = SharedAtomicT m
type instance Counter (PeerStateHolder ssc m) = Counter m
type instance Distribution (PeerStateHolder ssc m) = Distribution m
type instance SharedExclusiveT (PeerStateHolder ssc m) = SharedExclusiveT m
type instance Gauge (PeerStateHolder ssc m) = Gauge m
type instance ChannelT (PeerStateHolder ssc m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (PeerStateCtx ssc m) m) m
         , MFunctor' d (PeerStateHolder ssc m) (ReaderT (PeerStateCtx ssc m) m)
         ) => Mockable d (PeerStateHolder ssc m) where
    liftMockable = liftMockableWrappedM

instance (MonadIO m, Mockable SharedAtomic m) => WithPeerState ssc (PeerStateHolder ssc m) where
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
