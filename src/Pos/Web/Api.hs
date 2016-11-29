{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Pos.Web.Api
       ( NodeApi
       , nodeApi
       ) where

import           Data.Proxy  (Proxy (Proxy))
import           Servant.API ((:>), Get, JSON)
import           Universum

type NodeApi = "patak" :> Get '[JSON] Text

nodeApi :: Proxy NodeApi
nodeApi = Proxy
