{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API for explorer

module Pos.Explorer.Web.Api
       ( ExplorerApi
       , explorerApi
       , BlocksPages
       , BlocksPagesTotal
       , BlocksSummary
       , BlocksTxs
       , TxsLast
       , TxsSummary
       , AddressSummary
       , EpochSlotSearch
       ) where

import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary,
                                               CBlockEntry, CBlockSummary,
                                               CHash, CTxBrief, CTxEntry, CTxId,
                                               CTxSummary)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Pos.Types                    (EpochIndex)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON,
                                               QueryParam)
import           Universum

-- | Common prefix for all endpoints.
type API = "api"

type BlocksPages = API
    :> "blocks"
    :> "pages"
    :> QueryParam "page" Word
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError (Integer, [CBlockEntry]))

type BlocksPagesTotal = API
    :> "blocks"
    :> "pages"
    :> "total"
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError Integer)

type BlocksSummary = API
    :> "blocks"
    :> "summary"
    :> Capture "hash" CHash
    :> Get '[JSON] (Either ExplorerError CBlockSummary)

type BlocksTxs = API
    :> "blocks"
    :> "txs"
    :> Capture "hash" CHash
    :> QueryParam "limit" Word
    :> QueryParam "offset" Word
    :> Get '[JSON] (Either ExplorerError [CTxBrief])

type TxsLast = API
    :> "txs"
    :> "last"
    :> QueryParam "limit" Word
    :> QueryParam "offset" Word
    :> Get '[JSON] (Either ExplorerError [CTxEntry])

type TxsSummary = API
    :> "txs"
    :> "summary"
    :> Capture "txid" CTxId
    :> Get '[JSON] (Either ExplorerError CTxSummary)

type AddressSummary = API
    :> "addresses"
    :> "summary"
    :> Capture "address" CAddress
    :> Get '[JSON] (Either ExplorerError CAddressSummary)

type EpochSlotSearch = API
    :> "search"
    :> "epoch"
    :> Capture "epoch" EpochIndex
    :> QueryParam "slot" Word16
    :> Get '[JSON] (Either ExplorerError [CBlockEntry])

-- | Servant API which provides access to explorer
type ExplorerApi =
         BlocksPages
    :<|> BlocksPagesTotal
    :<|> BlocksSummary
    :<|> BlocksTxs
    :<|> TxsLast
    :<|> TxsSummary
    :<|> AddressSummary
    :<|> EpochSlotSearch

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
