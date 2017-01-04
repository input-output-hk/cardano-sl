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
       ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Lens                (makeLenses)
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Trans.Class   (MonadTrans)
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

instance MonadDelegation m => MonadDelegation (ReaderT r m) where
    askDelegationState = lift askDelegationState
instance MonadDelegation m => MonadDelegation (DHTResponseT s m) where
    askDelegationState = lift askDelegationState
instance MonadDelegation m => MonadDelegation (KademliaDHT m) where
    askDelegationState = lift askDelegationState

----------------------------------------------------------------------------
-- Class implementation
----------------------------------------------------------------------------

-- | Wrapper of @ReaderT (TVar DelegationWrap)@, nothing smart.
newtype DelegationT m a = DelegationT
    { getDelegationT :: ReaderT (TVar DelegationWrap) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadSlots, MonadCatch, MonadIO, MonadFail,
                HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog, MonadMask, MonadSscLD ssc, MonadSscGS ssc)

instance MonadTransfer s m => MonadTransfer s (DelegationT m)
type instance ThreadId (DelegationT m) = ThreadId m

instance MonadBase IO m => MonadBase IO (DelegationT m) where
    liftBase = lift . liftBase

instance Monad m => WrappedM (DelegationT m) where
    type UnwrappedM (DelegationT m) = ReaderT (TVar DelegationWrap) m
    _WrappedM = iso getDelegationT DelegationT

runDelegationT :: MonadIO m => DelegationWrap -> DelegationT m a -> m a
runDelegationT wrap action =
    liftIO (newTVarIO wrap) >>= runReaderT (getDelegationT action)

--deriving instance DelegationT ssc m => DelegationT ssc (DBHolder ssc m)

--instance MonadTransControl (DelegationT ssc) where
--    type StT (DelegationT ssc) a = StT (ReaderT (TxpLDWrap ssc)) a
--    liftWith = defaultLiftWith DelegationT getDelegationT
--    restoreT = defaultRestoreT DelegationT
--
--instance MonadBaseControl IO m => MonadBaseControl IO (DelegationT ssc m) where
--    type StM (DelegationT ssc m) a = ComposeSt (DelegationT ssc) m a
--    liftBaseWith     = defaultLiftBaseWith
--    restoreM         = defaultRestoreM
