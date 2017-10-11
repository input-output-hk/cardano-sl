{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}

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

import           Universum

import           Control.Monad.Catch          (try, tryJust)
import           Data.Proxy                   (Proxy (Proxy))

import           Pos.Explorer.Web.ClientTypes (Byte, CAda, CAddress, CAddressesFilter,
                                               CAddressSummary, CBlockEntry, CBlockSummary,
                                               CGenesisAddressInfo, CGenesisSummary,
                                               CHash, CTxBrief, CTxEntry, CTxId,
                                               CTxSummary)
import           Pos.Explorer.Web.Error       (ExplorerError, _Internal)
import           Pos.Types                    (EpochIndex)
import           Pos.Util.Servant             (ModifiesApiRes (..), VerbMod)
import           Servant.API                  ((:<|>), (:>), Capture, Get, JSON,
                                               QueryParam)

type PageNumber = Integer

-- | API result modification mode used here.
data ExplorerVerbTag

-- | Wrapper for Servants 'Verb' data type,
-- which allows to catch exceptions thrown by Explorer's endpoints.
type ExplorerVerb verb = VerbMod ExplorerVerbTag verb

-- | Shortcut for common api result types.
type ExRes verbMethod a = ExplorerVerb (verbMethod '[JSON] a)

instance ModifiesApiRes ExplorerVerbTag where
    type ApiModifiedRes ExplorerVerbTag a = Either ExplorerError a
    modifyApiResult _ =
        try . catchEndpointErrors . (either throwM pure =<<)
        where
            catchEndpointErrors :: (MonadCatch m) => m a -> m (Either ExplorerError a)
            catchEndpointErrors = tryJust $ \e -> (e ^? _Internal) $> e

-- | Common prefix for all endpoints.
type API = "api"

type TotalAda = API
    :> "supply"
    :> "ada"
    :> ExRes Get CAda

type BlocksPages = API
    :> "blocks"
    :> "pages"
    :> QueryParam "page" Word
    :> QueryParam "pageSize" Word
    :> ExRes Get (PageNumber, [CBlockEntry])

type BlocksPagesTotal = API
    :> "blocks"
    :> "pages"
    :> "total"
    :> QueryParam "pageSize" Word
    :> ExRes Get PageNumber

type BlocksSummary = API
    :> "blocks"
    :> "summary"
    :> Capture "hash" CHash
    :> ExRes Get CBlockSummary

type BlocksTxs = API
    :> "blocks"
    :> "txs"
    :> Capture "hash" CHash
    :> QueryParam "limit" Word
    :> QueryParam "offset" Word
    :> ExRes Get [CTxBrief]

type TxsLast = API
    :> "txs"
    :> "last"
    :> ExRes Get [CTxEntry]

type TxsSummary = API
    :> "txs"
    :> "summary"
    :> Capture "txid" CTxId
    :> ExRes Get CTxSummary

type AddressSummary = API
    :> "addresses"
    :> "summary"
    :> Capture "address" CAddress
    :> ExRes Get CAddressSummary

type EpochSlotSearch = API
    :> "search"
    :> "epoch"
    :> Capture "epoch" EpochIndex
    :> QueryParam "slot" Word16
    :> ExRes Get [CBlockEntry]

type GenesisSummary = API
    :> "genesis"
    :> "summary"
    :> ExRes Get CGenesisSummary

type GenesisPagesTotal = API
    :> "genesis"
    :> "address"
    :> "pages"
    :> "total"
    :> QueryParam "pageSize" Word
    :> QueryParam "filter" CAddressesFilter
    :> ExRes Get PageNumber

type GenesisAddressInfo = API
    :> "genesis"
    :> "address"
    :> QueryParam "page" Word
    :> QueryParam "pageSize" Word
    :> QueryParam "filter" CAddressesFilter
    :> ExRes Get [CGenesisAddressInfo]

type TxsStats = (PageNumber, [(CTxId, Byte)])
type StatsTxs = API
    :> "stats"
    :> "txs"
    :> QueryParam "page" Word
    :> ExRes Get TxsStats

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
    :<|> EpochSlotSearch
    :<|> GenesisSummary
    :<|> GenesisPagesTotal
    :<|> GenesisAddressInfo
    :<|> StatsTxs

-- | Helper Proxy
explorerApi :: Proxy ExplorerApi
explorerApi = Proxy
