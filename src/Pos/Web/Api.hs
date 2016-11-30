{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( NodeApi
       , nodeApi
       ) where

import           Data.Proxy  (Proxy (Proxy))
import           Servant.API ((:>), Get, JSON)
-- import           Universum

import           Pos.Types   (SlotId)

type NodeApi = "current_slot" :> Get '[JSON] SlotId

nodeApi :: Proxy NodeApi
nodeApi = Proxy
