module Explorer.Types.Actions where

import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (DashboardAPICode)

data Action
    = SetLanguage Language
    | UpdateView Route
    | ScrollTop
    | Search
    | DashboardExpandBlocks Boolean
    | DashboardExpandTransactions Boolean
    | DashboardShowAPICode DashboardAPICode
    | DashboardFocusSearchInput Boolean
    | NoOp
