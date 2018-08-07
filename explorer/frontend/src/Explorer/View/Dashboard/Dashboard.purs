module Explorer.View.Dashboard.Dashboard (dashboardView) where

import Prelude

import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.CSS as CSS
import Explorer.View.Dashboard.Blocks (dashBoardBlocksView)
import Explorer.View.Dashboard.Transactions (transactionsView)

import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))

-- TODO (ks): Currently network- offer- and api views are removed
-- since they don't have any meaningful data
dashboardView :: State -> P.HTML Action
dashboardView state =
    S.div ! S.className CSS.pureGContainer $ do
        dashBoardBlocksView state
        transactionsView state
        -- offerView state
        -- apiView state
