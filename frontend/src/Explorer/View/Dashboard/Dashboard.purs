module Explorer.View.Dashboard.Dashboard (dashboardView) where


import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Dashboard.Api (apiView)
import Explorer.View.Dashboard.Blocks (blocksView)
import Explorer.View.Dashboard.Hero (heroView)
import Explorer.View.Dashboard.Network (networkView)
import Explorer.View.Dashboard.Offer (offerView)
import Explorer.View.Dashboard.Transactions (transactionsView)
import Pux.Html (Html, div) as P
import Pux.Html.Attributes (className) as P

dashboardView :: State -> P.Html Action
dashboardView state =
    P.div
        [ P.className "explorer-dashboard" ]
        [ heroView state
        , networkView state
        , blocksView state
        , transactionsView state
        , offerView state
        , apiView state
        ]
