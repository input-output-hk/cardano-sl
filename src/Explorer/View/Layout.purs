module Explorer.View.Layout where

import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Address (addressView)
import Explorer.View.Block (blockView)
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard.Dashboard (dashboardView)
import Explorer.View.Footer (footerView)
import Explorer.View.Header (headerView)
import Explorer.View.NotFound (notFoundView)
import Explorer.View.Transaction (transactionView)
import Pux.Html (Html, div, main) as P
import Pux.Html.Attributes (className) as P

view :: State -> P.Html Action
view state =
    P.div
      []
      [
        P.div
          [ P.className "explorer-bg__container"]
          []
        , P.div
              [ P.className "explorer-content__wrapper"]
              [ P.main
                    [ P.className "explorer-content" ]
                    [ case state.route of
                          Dashboard -> dashboardView state
                          (Transaction hash) -> transactionView state hash
                          (Address address) -> addressView state
                          Calculator -> calculatorView state
                          (Block hash) -> blockView state
                          NotFound -> notFoundView state
                    ]
                , footerView state
                ]
        , headerView state
      ]
