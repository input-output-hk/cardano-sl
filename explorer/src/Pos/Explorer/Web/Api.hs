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
       , EpochPageSearch
       , EpochSlotSearch
       ) where

import           Universum

import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (Byte, CAda, CAddress, CAddressSummary,
                                               CBlockEntry, CBlockSummary,
                                               CGenesisAddressInfo, CGenesisSummary,
                                               CHash, CTxBrief, CTxEntry, CTxId,
                                               CTxSummary)
import           Pos.Explorer.Web.Error       (ExplorerError)
import           Pos.Types                    (EpochIndex)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON,
                                               QueryParam)


type PageNumber = Integer

-- | Common prefix for all endpoints.
type API = "api"

type TotalAda = API
    :> "supply"
    :> "ada"
    :> Get '[JSON] (Either ExplorerError CAda)

type BlocksPages = API
    :> "blocks"
    :> "pages"
    :> QueryParam "page" Word
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError (PageNumber, [CBlockEntry]))

type BlocksPagesTotal = API
    :> "blocks"
    :> "pages"
    :> "total"
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError PageNumber)

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

type EpochPageSearch = API
    :> "epochs"
    :> Capture "epoch" EpochIndex
    :> QueryParam "page" Int
    :> Get '[JSON] (Either ExplorerError (PageNumber, [CBlockEntry]))

type EpochSlotSearch = API
    :> "epochs"
    :> Capture "epoch" EpochIndex
    :> Capture "slot" Word16
    :> Get '[JSON] (Either ExplorerError [CBlockEntry])

type GenesisSummary = API
    :> "genesis"
    :> "summary"
    :> Get '[JSON] (Either ExplorerError CGenesisSummary)

type GenesisPagesTotal = API
    :> "genesis"
    :> "address"
    :> "pages"
    :> "total"
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError PageNumber)

type GenesisAddressInfo = API
    :> "genesis"
    :> "address"
    :> QueryParam "page" Word
    :> QueryParam "pageSize" Word
    :> Get '[JSON] (Either ExplorerError [CGenesisAddressInfo])

type TxsStats = (PageNumber, [(CTxId, Byte)])
type StatsTxs = API
    :> "stats"
    :> "txs"
    :> QueryParam "page" Word
    :> Get '[JSON] (Either ExplorerError TxsStats)

-- | Servant API which provides access to explorer
type ExplorerApi =
         TotalAda
    :<|> BlocksPages
    :<|> BlocksPagesTotal
    :<|> BlocksSummary
    :<|> BlocksTxs
    :<|> TxsLast
    :<|> TxsSummary
    :<|> AddressSummary
    :<|> EpochPageSearch
    :<|> EpochSlotSearch
    :<|> GenesisSummary
    :<|> GenesisPagesTotal
    :<|> GenesisAddressInfo
    :<|> StatsTxs

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
