{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Chain.Delegation.Class
       ( DlgMemPool
       , DelegationWrap (..)
       , dwMessageCache
       , dwProxySKPool
       , dwPoolSize
       , dwTip

       , DelegationVar

       , MonadDelegation
       , askDelegationState
       ) where

import           Universum

import           Control.Lens (makeLenses)
import qualified Data.Cache.LRU as LRU
import           Data.Time.Clock (UTCTime)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Chain.Block.Union (HeaderHash)
import           Pos.Chain.Delegation.Types (DlgMemPool, ProxySKHeavy)
import           Pos.Util.Util (HasLens (..))

---------------------------------------------------------------------------
-- Delegation in-memory data
----------------------------------------------------------------------------

-- Notice: LRU caches in datatypes are only there to emulate
-- throw-away-old-entries queue behaviour, we don't ever update LRUs
-- with LRU.lookup.
-- | In-memory storage needed for delegation logic.
data DelegationWrap = DelegationWrap
    { _dwMessageCache :: !(LRU.LRU ProxySKHeavy UTCTime)
      -- ^ Message cache to prevent infinite propagation of useless
      -- certs.
    , _dwProxySKPool  :: !DlgMemPool
      -- ^ Memory pool of hardweight proxy secret keys. Keys of this
      -- map are issuer public keys.
    , _dwPoolSize     :: !Byte
      -- ^ Size of '_dwProxySKPool' in bytes.
      -- It's not exact size for a variety of reasons, but it should be
      -- a good approximation.
    , _dwTip          :: !HeaderHash
      -- ^ Header tip 'DelegationWrap' is correct in relation to.
    }

makeLenses ''DelegationWrap

-- This variable is not used to actually lock on something. We use
-- 'StateLock' for thread communication, this is used mostly as
-- 'IORef' with atomic updates.
type DelegationVar = TVar DelegationWrap

----------------------------------------------------------------------------
-- Class definition
----------------------------------------------------------------------------

-- | We're locking on the whole delegation wrap at once. Locking on
-- independent components is better in performance, so there's a place
-- for optimization here.
type MonadDelegation ctx m =
    ( MonadReader ctx m
    , HasLens DelegationVar ctx DelegationVar
    )

askDelegationState :: MonadDelegation ctx m => m DelegationVar
askDelegationState = view (lensOf @DelegationVar)
