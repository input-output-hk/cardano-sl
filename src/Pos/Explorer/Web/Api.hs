{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for explorer

module Pos.Explorer.Web.Api
       ( ExplorerApi
       , explorerApi
       ) where

import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary,
                                               CBlockEntry, CBlockSummary,
                                               CSearchId, CHash, CTxEntry, CTxId,
                                               CTxSummary, CHashSearchResult)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON,
                                               QueryParam)
import           Pos.Types                    (EpochIndex)
import           Universum

-- | Servant API which provides access to explorer
type ExplorerApi =
      "api"
      :> "blocks"
      :> "last"
      :> QueryParam "limit" Word
      :> QueryParam "offset" Word
      :> Get '[JSON] (Either ExplorerError [CBlockEntry])
    :<|>
      "api"
      :> "blocks"
      :> "summary"
      :> Capture "hash" CHash
      :> Get '[JSON] (Either ExplorerError CBlockSummary)
    :<|>
      "api"
      :> "blocks"
      :> "txs"
      :> Capture "hash" CHash
      :> QueryParam "limit" Word
      :> QueryParam "offset" Word
      :> Get '[JSON] (Either ExplorerError [CTxEntry])
    :<|>
      "api"
      :> "txs"
      :> "last"
      :> QueryParam "limit" Word
      :> QueryParam "offset" Word
      :> Get '[JSON] (Either ExplorerError [CTxEntry])
    :<|>
      "api"
      :> "txs"
      :> "summary"
      :> Capture "txid" CTxId
      :> Get '[JSON] (Either ExplorerError CTxSummary)
    :<|>
      "api"
      :> "addresses"
      :> "summary"
      :> Capture "address" CAddress
      :> Get '[JSON] (Either ExplorerError CAddressSummary)
    :<|>
      "api"
      :> "search"
      :> Capture "hash" CSearchId
      :> Get '[JSON] (Either ExplorerError CHashSearchResult)
    :<|>
      "api"
      :> "search"
      :> "epoch"
      :> Capture "epoch" EpochIndex
      :> QueryParam "slot" Word16
      :> Get '[JSON] (Either ExplorerError [CBlockEntry])

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
