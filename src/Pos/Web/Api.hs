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

import           Pos.Types   (EpochIndex, SlotId, SlotLeaders)

type NodeApi = "current_slot" :> Get '[JSON] SlotId :<|>
               "leaders" :> QueryParam "epoch" EpochIndex :> Get '[JSON] SlotLeaders

nodeApi :: Proxy NodeApi
nodeApi = Proxy
