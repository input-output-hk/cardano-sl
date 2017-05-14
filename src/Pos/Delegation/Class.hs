{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.Delegation.Class
       ( DelegationWrap (..)
       , dwMessageCache
       , dwConfirmationCache
       , dwProxySKPool
       , dwEpochId
       , dwThisEpochPosted
       , MonadDelegation
       , askDelegationState
       ) where

import           Control.Concurrent.STM (TVar)
import           Control.Lens           (makeLenses)
import qualified Data.Cache.LRU         as LRU
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Time.Clock        (UTCTime)
import qualified Ether
import           Universum

import           Pos.Constants          (dlgCacheParam)
import           Pos.Crypto             (PublicKey)
import           Pos.Delegation.Types   (SendProxySK)
import           Pos.Types              (EpochIndex, ProxySKHeavy, ProxySKLight)

---------------------------------------------------------------------------
-- Delegation in-memory data
----------------------------------------------------------------------------

-- | In-memory storage needed for delegation logic.
-- Size of this storage is limited for the following reasons:
-- • both caches are limited by setting limit on LRU;
-- • heavyweight PSKs pool is limited because only richmen can issue a valid
--   heavyweight PSK and there can't be more than one heavyweight PSK from one
--   richman in epoch, so the limit is '1 / threshold';
-- • set of issuers for the current epoch is limited for the same reason;
-- • everything else has a constant size.
data DelegationWrap = DelegationWrap
    { _dwMessageCache      :: LRU.LRU SendProxySK UTCTime
      -- ^ Message cache to prevent infinite propagation of useless
      -- certs.
    , _dwConfirmationCache :: LRU.LRU ProxySKLight UTCTime
      -- ^ Confirmation cache for lightweight PSKs.
    , _dwProxySKPool       :: HashMap PublicKey ProxySKHeavy
      -- ^ Memory pool of hardweight proxy secret keys. Keys of this
      -- map are issuer public keys.
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
