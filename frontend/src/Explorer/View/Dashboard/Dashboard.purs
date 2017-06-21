module Explorer.View.Dashboard.Dashboard (dashboardView) where

import Pux.DOM.HTML (Html) as P
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Dashboard.Blocks (dashBoardBlocksView)
import Explorer.View.Dashboard.Hero (heroView)
import Explorer.View.Dashboard.Transactions (transactionsView)

-- TODO (ks): Currently network- offer- and api views are removed
-- since they don't have any meaningful data
dashboardView :: State -> P.Html Action
dashboardView state =
    div ! className "explorer-dashboard" $ do
        heroView state
        -- networkView state
        dashBoardBlocksView state
        transactionsView state
        -- offerView state
        -- apiView state
