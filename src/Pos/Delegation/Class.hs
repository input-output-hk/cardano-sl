{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Class
       ( DelegationWrap (..)
       , dwProxyMsgCache
       , dwProxyConfCache
       , dwProxySKPool
       , MonadDelegation (..)
       ) where

import           Control.Concurrent.STM    (TVar)
import           Control.Lens              (makeLenses)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Default              (Default (def))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.Time.Clock           (UTCTime)
import           Universum

import           Pos.Crypto                (PublicKey)
import           Pos.Delegation.Types      (SendProxySK)
import           Pos.Types                 (ProxySKEpoch, ProxySKSimple)

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
    , _dwProxySKPool    :: HashMap PublicKey ProxySKSimple
      -- ^ Memory pool of hardweight proxy secret keys. Keys of this
      -- map are issuer public keys.
    }

makeLenses ''DelegationWrap

instance Default DelegationWrap where
    def = DelegationWrap HM.empty HM.empty HM.empty

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
