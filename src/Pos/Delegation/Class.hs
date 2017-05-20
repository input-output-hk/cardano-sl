{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Class
       ( DlgMemPool
       , DelegationWrap (..)
       , dwMessageCache
       , dwConfirmationCache
       , dwProxySKPool
       , dwPoolSize
       , dwEpochId
       , dwThisEpochPosted
       , MonadDelegation
       , askDelegationState
       ) where

import           Control.Concurrent.STM     (TVar)
import           Control.Lens               (makeLenses)
import qualified Data.Cache.LRU             as LRU
import           Data.Default               (Default (def))
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Time.Clock            (UTCTime)
import qualified Ether
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Constants              (dlgCacheParam)
import           Pos.Crypto                 (PublicKey)
import           Pos.Types                  (EpochIndex, ProxySKEither, ProxySKHeavy,
                                             ProxySKLight)

---------------------------------------------------------------------------
-- Delegation in-memory data
----------------------------------------------------------------------------

type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | In-memory storage needed for delegation logic.
data DelegationWrap = DelegationWrap
    { _dwMessageCache      :: LRU.LRU ProxySKEither UTCTime
      -- ^ Message cache to prevent infinite propagation of useless
      -- certs.
    , _dwConfirmationCache :: LRU.LRU ProxySKLight UTCTime
      -- ^ Confirmation cache for lightweight PSKs.
    , _dwProxySKPool       :: DlgMemPool
      -- ^ Memory pool of hardweight proxy secret keys. Keys of this
      -- map are issuer public keys.
    , _dwPoolSize          :: !Byte
      -- ^ Size of '_dwProxySKPool' in bytes.
      -- It's not exact size for a variety of reasons, but it should be
      -- a good approximation.
    , _dwEpochId           :: EpochIndex
      -- ^ Epoch index 'DelegationWrap' is correct in relation to.
    , _dwThisEpochPosted   :: HashSet PublicKey
      -- ^ Set of stakeholders that have already posted their PSKs
      -- this epoch.
    }

makeLenses ''DelegationWrap

instance Default DelegationWrap where
    def =
        DelegationWrap
        { _dwMessageCache = LRU.newLRU msgCacheLimit
        , _dwConfirmationCache = LRU.newLRU confCacheLimit
        , _dwProxySKPool = HM.empty
        , _dwPoolSize = 1
        , _dwEpochId = 0
        , _dwThisEpochPosted = HS.empty
        }
      where
        msgCacheLimit = Just dlgCacheParam
        confCacheLimit = Just (dlgCacheParam `div` 5)

----------------------------------------------------------------------------
-- Class definition
----------------------------------------------------------------------------

-- | Equivalent of @MonadReader (TVar DelegationWrap) m@. Currently
-- we're locking on the whole delegation wrap at once. Locking on
-- independent components is better in performance, so there's a place
-- for optimization here.
type MonadDelegation = Ether.MonadReader' (TVar DelegationWrap)

askDelegationState :: MonadDelegation m => m (TVar DelegationWrap)
askDelegationState = Ether.ask'
