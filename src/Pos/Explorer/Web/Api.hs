{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for explorer

module Pos.Explorer.Web.Api
       ( ExplorerApi
       , explorerApi
       ) where

import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (CAddress, CBlockEntry, CTxEntry, CTxId)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Pos.Types                    (Coin, SoftwareVersion)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON, Post,
                                               QueryParam, ReqBody)
import           Universum

-- | Servant API which provides access to explorer
type ExplorerApi =
    "last_blocks" :> QueryParam "limit" Word :> QueryParam "offset" Word :> Get '[JSON] (Either ExplorerError [CBlockEntry])
    :<|>
    "last_txs" :> QueryParam "limit" Word :> QueryParam "offset" Word :> Get '[JSON] (Either ExplorerError [CTxEntry])

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
