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
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Default                (Default (def))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.Time.Clock             (UTCTime)
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
import           Pos.Txp.Class               (MonadTxpLD (..), TxpLDWrap (..))
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

-- | Wrapper of @ReaderT (TVar DelegationWrap)@, nothing smart.
newtype DelegationT m a = DelegationT
    { getDelegationT :: ReaderT (TVar DelegationWrap) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask, MonadSscLD ssc, MonadSscGS ssc,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc, MonadBase io)

instance (Monad m) => MonadDelegation (DelegationT m) where
    askDelegationState = DelegationT ask

instance MonadTransfer s m => MonadTransfer s (DelegationT m)

type instance ThreadId (DelegationT m) = ThreadId m

instance Monad m => WrappedM (DelegationT m) where
    type UnwrappedM (DelegationT m) = ReaderT (TVar DelegationWrap) m
    _WrappedM = iso getDelegationT DelegationT

instance MonadTransControl DelegationT where
    type StT (DelegationT) a = StT (ReaderT (TVar DelegationWrap)) a
    liftWith = defaultLiftWith DelegationT getDelegationT
    restoreT = defaultRestoreT DelegationT

instance MonadBaseControl IO m => MonadBaseControl IO (DelegationT m) where
    type StM (DelegationT m) a = ComposeSt DelegationT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Executes delegationT transformer creating tvar from given wrap.
runDelegationT :: MonadIO m => DelegationWrap -> DelegationT m a -> m a
runDelegationT wrap action =
    liftIO (newTVarIO wrap) >>= runReaderT (getDelegationT action)

-- | Executes delegation wrap using existing delegation wrap tvar.
runDelegationTFromTVar :: Monad m => (TVar DelegationWrap) -> DelegationT m a -> m a
runDelegationTFromTVar var action = runReaderT (getDelegationT action) var
