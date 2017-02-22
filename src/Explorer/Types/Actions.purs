module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode, CBlockEntries, CTxEntries)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    -- sockets
    | SocketConnected Boolean
    | SocketLatestBlocks (Either Error CBlockEntries)
    | SocketLatestTransactions (Either Error CTxEntries)
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
    -- Debugging
    -- TODO (jk) remove all actions for debugging later
    | RequestLatestBlocks
    | ReceiveLatestBlocks (Either Error CBlockEntries)
    | RequestLatestTransactions
    | ReceiveLatestTransactions (Either Error CTxEntries)


type ActionChannel = Channel Action
