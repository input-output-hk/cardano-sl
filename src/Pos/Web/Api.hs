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

import           Data.Proxy         (Proxy (Proxy))
import           Servant.API        ((:<|>), (:>), Capture, Get, JSON, Post, QueryParam)
import           Universum

import           Pos.Crypto         (LSecret, PublicKey)
import           Pos.Ssc.GodTossing (SscGodTossing)
import           Pos.Types          (EpochIndex, HeaderHash, SlotId, SlotLeaders)

-- | Servant API which provides access to full node internals.
type BaseNodeApi ssc =
    "current_slot" :> Get '[JSON] SlotId :<|>
    "leaders" :> QueryParam "epoch" EpochIndex :> Get '[JSON] SlotLeaders :<|>
    "spending_key" :> Get '[JSON] PublicKey :<|>
    "head_hash" :> Get '[JSON] (HeaderHash ssc)

-- | Helper Proxy.
baseNodeApi :: Proxy (BaseNodeApi ssc)
baseNodeApi = Proxy

-- | GodTossing specific API.
type GodTossingApi =
    "toggle" :> Capture "enable" Bool :> Post '[JSON] () :<|>
    "secret" :> Get '[JSON] LSecret

-- | Helper Proxy.
godTossingApi :: Proxy GodTossingApi
godTossingApi = Proxy

-- | Servant API which provides access to full node internals with
-- GodTossing SSC.
type GtNodeApi =
    BaseNodeApi SscGodTossing :<|>
    "god_tossing" :> GodTossingApi

-- | Helper Proxy.
gtNodeApi :: Proxy GtNodeApi
gtNodeApi = Proxy
