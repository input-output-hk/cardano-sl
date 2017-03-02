module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode, CBlockEntries, CTxEntries)
import Pos.Explorer.Web.ClientTypes (CBlockSummary, CHash)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
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
    | ReceiveBlockTxs (Either Error CTxEntries)
    | RequestInitialTxs
    | ReceiveInitialTxs (Either Error CTxEntries)
    -- dashboard
    | DashboardExpandBlocks Boolean         -- toggle blocks
    | DashboardPaginateBlocks Int           -- current pagination of blocks
    | DashboardExpandTransactions Boolean   -- dashboard transactions
    | DashboardShowAPICode DashboardAPICode -- dashboard api
    | DashboardSearch                       -- dasboard search
    | DashboardFocusSearchInput Boolean
    -- address detail
    | AddressPaginateTransactions Int       -- current pagination of transactions
    -- block detail
    | BlockPaginateTransactions Int       -- current pagination of transactions
    -- misc
    | NoOp


type ActionChannel = Channel Action
