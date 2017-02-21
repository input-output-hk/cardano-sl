module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode, CBlockEntries, CTxEntries)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    | UpdateView Route
    | ScrollTop
    | SocketConnected Boolean
    | SocketLatestBlocks (Either Error CBlockEntries)
    | SocketLatestTransactions (Either Error CTxEntries)
    | Search
    | DashboardExpandBlocks Boolean
    | DashboardExpandTransactions Boolean
    | DashboardShowAPICode DashboardAPICode
    | DashboardFocusSearchInput Boolean
    | NoOp
    -- TODO (jk) for debugging + remove it later
    | RequestLatestBlocks
    | ReceiveLatestBlocks (Either Error CBlockEntries)
    | RequestLatestTransactions
    | ReceiveLatestTransactions (Either Error CTxEntries)


type ActionChannel = Channel Action
