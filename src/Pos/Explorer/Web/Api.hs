{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for explorer

module Pos.Explorer.Web.Api
       ( ExplorerApi
       , explorerApi
       ) where

import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (CHash, CAddress, CAddressSummary,
                                               CBlockEntry, CBlockSummary,
                                               CTxEntry, CTxId, CTxSummary)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Pos.Types                    (Coin, SoftwareVersion)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON,
                                               QueryParam)
import           Universum

-- | Servant API which provides access to explorer
type ExplorerApi =
    "api" :> "last_blocks" :> QueryParam "limit" Word :> QueryParam "offset" Word :> Get '[JSON] (Either ExplorerError [CBlockEntry])
    :<|>
    "api" :> "last_txs" :> QueryParam "limit" Word :> QueryParam "offset" Word :> Get '[JSON] (Either ExplorerError [CTxEntry])
    :<|>
    "api" :> "block_summary" :> Capture "hash" CHash :> Get '[JSON] (Either ExplorerError CBlockSummary)
    :<|>
    "api" :> "block_txs" :> Capture "hash" CHash :> QueryParam "limit" Word :> QueryParam "offset" Word :> Get '[JSON] (Either ExplorerError [CTxEntry])
    :<|>
    "api" :> "address_summary" :> Capture "address" CAddress :> Get '[JSON] (Either ExplorerError CAddressSummary)
    :<|>
    "api" :> "tx_summary" :> Capture "txid" CTxId :> Get '[JSON] (Either ExplorerError CTxSummary)

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
