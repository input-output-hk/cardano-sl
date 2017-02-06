module Explorer.Types.Actions where

import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    | UpdateView Route
    | ScrollTop
    | SocketConnected Boolean
    | Search
    | DashboardExpandBlocks Boolean
    | DashboardExpandTransactions Boolean
    | DashboardShowAPICode DashboardAPICode
    | DashboardFocusSearchInput Boolean
    | NoOp


type ActionChannel = Channel Action
