{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodePeerState.

module Pos.Communication.PeerState
       ( WithPeerState (..)
       , runPeerStateHolder
       , PeerStateHolder (..)
       ) where

import           Control.Lens                  (iso)
import           Control.Monad.Base            (MonadBase (..))
import           Control.Monad.Fix             (MonadFix)
import           Control.Monad.Reader          (ReaderT (..))
import           Control.Monad.Trans.Class     (MonadTrans)
import           Data.Default                  (Default (def))
import qualified ListT                         as LT
import           Mockable                      (ChannelT, MFunctor' (hoist'),
                                                Mockable (liftMockable), Promise,
                                                SharedAtomic, SharedAtomicT, ThreadId,
                                                newSharedAtomic)
import           Node                          (NodeId)
import           Serokell.Util.Lens            (WrappedM (..))
import qualified STMContainers.Map             as STM
import           System.Wlog                   (CanLog, HasLoggerName)
import           Universum

import           Pos.Communication.Types.State (PeerState)
import           Pos.Txp.Class                 (MonadTxpLD)

type PeerStateCtx ssc m = STM.Map NodeId (SharedAtomicT m (PeerState ssc))

class WithPeerState ssc m | m -> ssc where
    getPeerState   :: NodeId -> m (SharedAtomicT m (PeerState ssc))
    clearPeerState :: NodeId -> m ()
    getAllStates   :: m (PeerStateCtx ssc m)
    setAllStates   :: PeerStateCtx ssc m -> m ()

instance (Monad m, WithPeerState ssc m) => WithPeerState ssc (ReaderT r m) where
    getPeerState = lift . getPeerState
    clearPeerState = lift . clearPeerState
    getAllStates = lift getAllStates
    setAllStates = lift . setAllStates

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
type instance ChannelT (PeerStateHolder ssc m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT (PeerStateCtx ssc m) m) m
         , MFunctor' d (PeerStateHolder ssc m) (ReaderT (PeerStateCtx ssc m) m)
         ) => Mockable d (PeerStateHolder ssc m) where
    liftMockable dmt = PeerStateHolder $ liftMockable $ hoist' getPeerStateHolder dmt

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
    getAllStates = PeerStateHolder ask
    setAllStates s = (PeerStateHolder ask) >>= \m -> liftIO . atomically $ do
        STM.deleteAll m
        LT.traverse_ (\(v, k) -> STM.insert k v m) $ STM.stream s
