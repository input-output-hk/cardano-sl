{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( NodeApi
       , nodeApi
       ) where

import           Data.Proxy  (Proxy (Proxy))
import           Servant.API ((:<|>), (:>), Get, JSON, QueryParam)
-- import           Universum

import           Pos.Crypto  (PublicKey)
import           Pos.Types   (EpochIndex, HeaderHash, SlotId, SlotLeaders)

type NodeApi ssc =
    "current_slot" :> Get '[JSON] SlotId :<|>
    "leaders" :> QueryParam "epoch" EpochIndex :> Get '[JSON] SlotLeaders :<|>
    "spending_key" :> Get '[JSON] PublicKey :<|>
    "head_hash" :> Get '[JSON] (HeaderHash ssc)

nodeApi :: Proxy (NodeApi ssc)
nodeApi = Proxy
