-- | Helpers for delegation that depend on lib and thus cannot be placed
-- in cardano-sl-delegation.

module Pos.Delegation.HelpersLib
    ( mkDelegationVar
    ) where

import           Universum

import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HM

import           Pos.Configuration (HasNodeConfiguration, dlgCacheParam)
import           Pos.Core (HasConfiguration, headerHash)
import           Pos.DB (MonadBlockDBRead)
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.Delegation.Class (DelegationVar, DelegationWrap (..))

-- | Make a new 'DelegationVar' and initialize it.
--
-- * Sets '_dwEpochId' to epoch of tip.
-- * Initializes mempools/LRU caches.
mkDelegationVar ::
       (MonadIO m, MonadBlockDBRead m, HasConfiguration, HasNodeConfiguration)
    => m DelegationVar
mkDelegationVar = do
    tip <- getTipHeader
    newTVarIO
        DelegationWrap
        { _dwMessageCache = LRU.newLRU msgCacheLimit
        , _dwProxySKPool = HM.empty
        , _dwPoolSize = 1 -- approximate size of the empty mempool.
        , _dwTip = headerHash tip
        }
  where
    msgCacheLimit = Just dlgCacheParam
