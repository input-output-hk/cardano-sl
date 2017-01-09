{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Class
       ( DelegationWrap
       , dwProxyMsgCache
       , dwProxyConfCache
       , MonadDelegation (..)
       , DelegationT (..)
       , runDelegationT
       , runDelegationTFromTVar
       ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens                (makeLenses)
import           Control.Lens                (iso)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Data.Default                (Default (def))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.Time.Clock             (UTCTime)
import           Mockable                    (ChannelT, MFunctor' (hoist'),
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Delegation.Types        (SendProxySK)
import           Pos.DHT.Model.Class         (DHTResponseT)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLD (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types                   (ProxySKEpoch)
import           Pos.Types.Utxo.Class        (MonadUtxo, MonadUtxoRead)
import           Pos.Util.JsonLog            (MonadJL (..))

---------------------------------------------------------------------------
-- Delegation in-memory data
----------------------------------------------------------------------------

-- | In-memory storage needed for delegation logic
-- Maybe ncProxyCache should be LRU instead of hashmap, but that's not
-- urgent optimization idea.
data DelegationWrap = DelegationWrap
    { _dwProxyMsgCache  :: HashMap SendProxySK UTCTime
      -- ^ Message cache to prevent infinite propagation of useless certs
    , _dwProxyConfCache :: HashMap ProxySKEpoch UTCTime
      -- ^ Confirmation cache for lightweight PSKs
    }

makeLenses ''DelegationWrap

instance Default DelegationWrap where
    def = DelegationWrap HM.empty HM.empty

----------------------------------------------------------------------------
-- Class definition
----------------------------------------------------------------------------

-- | Equivalent of @MonadReader (TVar DelegationWrap) m@. Currently
-- we're locking on the whole delegation wrap at once. Locking on
-- independent components is better in performance, so there's a place
-- for optimization here.
class (Monad m) => MonadDelegation m where
    askDelegationState :: m (TVar DelegationWrap)
    -- ^ Retrieves 'TVar' on 'DelegationWrap'

    default askDelegationState
        :: (MonadTrans t, MonadDelegation m', t m' ~ m) => m (TVar DelegationWrap)
    askDelegationState = lift askDelegationState
    -- ^ Default implementation for 'MonadTrans'

instance MonadDelegation m => MonadDelegation (ReaderT s m)
instance MonadDelegation m => MonadDelegation (StateT s m)
instance MonadDelegation m => MonadDelegation (DHTResponseT s m)
instance MonadDelegation m => MonadDelegation (KademliaDHT m)

----------------------------------------------------------------------------
-- Class implementation
----------------------------------------------------------------------------

type ReaderTCtx = TVar DelegationWrap

-- | Wrapper of @ReaderT (TVar DelegationWrap)@, nothing smart.
newtype DelegationT m a = DelegationT
    { getDelegationT :: ReaderT ReaderTCtx m a
    } deriving (Functor, Applicative, Monad, MonadTrans,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, WithNodeContext ssc, MonadJL,
                CanLog, MonadMask, MonadSscLD ssc, MonadSscGS ssc,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc, MonadFix)

deriving instance MonadDB ssc m => MonadDB ssc (DelegationT m)

instance (Monad m) => MonadDelegation (DelegationT m) where
    askDelegationState = DelegationT ask

instance Monad m => WrappedM (DelegationT m) where
    type UnwrappedM (DelegationT m) = ReaderT ReaderTCtx m
    _WrappedM = iso getDelegationT DelegationT

type instance ThreadId (DelegationT m) = ThreadId m
type instance Promise (DelegationT m) = Promise m
type instance SharedAtomicT (DelegationT m) = SharedAtomicT m
type instance ChannelT (DelegationT m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT ReaderTCtx m) m
         , MFunctor' d (DelegationT m) (ReaderT ReaderTCtx m)
         ) => Mockable d (DelegationT m) where
    liftMockable = liftMockableWrappedM

-- | Executes delegationT transformer creating tvar from given wrap.
runDelegationT :: MonadIO m => DelegationWrap -> DelegationT m a -> m a
runDelegationT wrap action =
    liftIO (newTVarIO wrap) >>= runReaderT (getDelegationT action)

-- | Executes delegation wrap using existing delegation wrap tvar.
runDelegationTFromTVar :: TVar DelegationWrap -> DelegationT m a -> m a
runDelegationTFromTVar var action = runReaderT (getDelegationT action) var
