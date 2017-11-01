{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( BaseNodeApi
       , baseNodeApi

       , HealthCheckApi
       , healthCheckApi

       , SscApi
       , sscApi

       , SscNodeApi
       , sscNodeApi
       ) where

import           Universum

import           Servant.API   ((:<|>), (:>), Capture, Get, JSON, PlainText, Post,
                                QueryParam)

import           Pos.Crypto    (PublicKey)
import           Pos.Txp       (TxOut)
import           Pos.Types     (EpochIndex, HeaderHash, SlotLeaders)
import           Pos.Web.Types (CConfirmedProposalState)

-- | Servant API which provides access to full node internals.
--
-- Implementations of these methods are in
-- 'Pos.Web.Server.baseServantHandlers'.
type BaseNodeApi =
    -- "current_slot"
    --     :> Get '[JSON] SlotId
    -- :<|>
    "leaders"
        :> QueryParam "epoch" EpochIndex
        :> Get '[JSON] SlotLeaders
    :<|>
    "utxo"
        :> Get '[JSON] [TxOut]
    :<|>
    "spending_key"
        :> Get '[JSON] PublicKey
    :<|>
    "head_hash"
        :> Get '[JSON] HeaderHash
    :<|>
    "local_txs_num"
        :> Get '[JSON] Word
    :<|>
    "confirmed_proposals"
        :> Get '[JSON] [CConfirmedProposalState]

-- | Helper Proxy.
healthCheckApi :: Proxy HealthCheckApi
healthCheckApi = Proxy

type HealthCheckApi =
    "healthcheck" :> "route53" :> Get '[PlainText] String

-- | Helper Proxy.
baseNodeApi :: Proxy BaseNodeApi
baseNodeApi = Proxy

-- | SSC specific API.
type SscApi =
    "toggle"
        :> Capture "enable" Bool
        :> Post '[JSON] ()
    -- :<|>
    -- "has_secret" :> Get '[JSON] Bool :<|>
    -- "secret" :> Get '[JSON] SharedSeed :<|>
    -- "stage" :> Get '[JSON] SscStage

-- | Helper Proxy.
sscApi :: Proxy SscApi
sscApi = Proxy

-- | Servant API which provides access to full node internals with SSC.
type SscNodeApi =
    BaseNodeApi
    :<|>
    "god_tossing"
        :> SscApi

-- | Helper Proxy.
sscNodeApi :: Proxy SscNodeApi
sscNodeApi = Proxy
