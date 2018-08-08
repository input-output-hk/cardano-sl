{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Pos.DB.Delegation.Var
       ( mkDelegationVar
       ) where

import           Universum

import qualified Data.Cache.LRU as LRU

import           Pos.Chain.Block (headerHash)
import           Pos.Chain.Delegation (DelegationVar, DelegationWrap (..),
                     HasDlgConfiguration, dlgCacheParam)
import           Pos.DB (MonadBlockDBRead)
import           Pos.DB.BlockIndex (getTipHeader)

-- | Make a new 'DelegationVar' and initialize it. Accepts
-- 'dlgCacheParam' as input parameter. It's supposed to be passed from
-- configuration.
--
-- * Sets '_dwEpochId' to epoch of tip.
-- * Initializes mempools/LRU caches.
mkDelegationVar ::
       (MonadIO m, MonadBlockDBRead m, HasDlgConfiguration)
    => m DelegationVar
mkDelegationVar = do
    tip <- getTipHeader
    newTVarIO
        DelegationWrap
        { _dwMessageCache = LRU.newLRU (Just dlgCacheParam)
        , _dwProxySKPool = mempty
        , _dwPoolSize = 1 -- approximate size of the empty mempool.
        , _dwTip = headerHash tip
        }
