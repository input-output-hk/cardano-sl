module Explorer.Types.Actions where

import Data.Either (Either)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode, CBlockEntries)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    | UpdateView Route
    | ScrollTop
    | SocketConnected Boolean
    | SocketLatestBlocks (Either String CBlockEntries)
    | Search
    | DashboardExpandBlocks Boolean
    | DashboardExpandTransactions Boolean
    | DashboardShowAPICode DashboardAPICode
    | DashboardFocusSearchInput Boolean
    | NoOp


type ActionChannel = Channel Action
