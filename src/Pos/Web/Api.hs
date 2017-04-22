{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( BaseNodeApi
       , baseNodeApi

       , GodTossingApi
       , godTossingApi

       , GtNodeApi
       , gtNodeApi
       ) where

import           Servant.API        ((:<|>), (:>), Capture, Get, JSON, Post, QueryParam)
import           Universum

import           Pos.Crypto         (PublicKey)
import           Pos.Ssc.GodTossing (SscGodTossing)
import           Pos.Txp            (TxOut)
import           Pos.Types          (EpochIndex, HeaderHash, SlotLeaders)

-- | Servant API which provides access to full node internals.
--
-- Implementations of these methods are in
-- 'Pos.Web.Server.baseServantHandlers'.
type BaseNodeApi ssc =
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

-- | Helper Proxy.
baseNodeApi :: Proxy (BaseNodeApi ssc)
baseNodeApi = Proxy

-- | GodTossing specific API.
type GodTossingApi =
    "toggle"
        :> Capture "enable" Bool
        :> Post '[JSON] ()
    -- :<|>
    -- "has_secret" :> Get '[JSON] Bool :<|>
    -- "secret" :> Get '[JSON] SharedSeed :<|>
    -- "stage" :> Get '[JSON] GodTossingStage

-- | Helper Proxy.
godTossingApi :: Proxy GodTossingApi
godTossingApi = Proxy

-- | Servant API which provides access to full node internals with
-- GodTossing SSC.
type GtNodeApi =
    BaseNodeApi SscGodTossing
    :<|>
    "god_tossing"
        :> GodTossingApi

-- | Helper Proxy.
gtNodeApi :: Proxy GtNodeApi
gtNodeApi = Proxy
