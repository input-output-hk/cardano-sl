module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (CBlockEntries, CTxBriefs, CTxEntries, DashboardAPICode, Search)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash, CTxId, CTxSummary)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    -- QR code
    | GenerateQrCode CAddress
    -- socket endpoints
    | SocketConnected Boolean
    | SocketLatestBlocks (Either Error CBlockEntries)
    | SocketLatestTransactions (Either Error CTxEntries)
    -- http endpoints
    | RequestInitialBlocks
    | ReceiveInitialBlocks (Either Error CBlockEntries)
    | RequestBlockSummary CHash
    | ReceiveBlockSummary (Either Error CBlockSummary)
    | RequestBlockTxs CHash
    | ReceiveBlockTxs (Either Error CTxBriefs)
    | RequestInitialTxs
    | ReceiveInitialTxs (Either Error CTxEntries)
    | RequestTxSummary CTxId
    | ReceiveTxSummary (Either Error CTxSummary)
    | RequestAddressSummary CAddress
    | ReceiveAddressSummary (Either Error CAddressSummary)
    -- search
    | DashboardSearch                       -- dasboard search
    | UpdateSelectedSearch Search
    -- dashboard
    | DashboardExpandBlocks Boolean         -- toggle blocks
    | DashboardPaginateBlocks Int           -- current pagination of blocks
    | DashboardExpandTransactions Boolean   -- dashboard transactions
    | DashboardShowAPICode DashboardAPICode -- dashboard api
    | DashboardFocusSearchInput Boolean
    -- address detail
    | AddressPaginateTxs Int       -- current pagination of transactions
    -- block detail
    | BlockPaginateTxs Int       -- current pagination of transactions
    -- misc
    | NoOp


type ActionChannel = Channel Action
