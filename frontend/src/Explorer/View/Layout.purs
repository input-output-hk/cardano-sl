module Explorer.View.Layout where

import Prelude
import Data.Lens ((^.))
import Explorer.Lenses.State (globalViewState, gViewMobileMenuOpenend, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Address (addressView)
import Explorer.View.Block (blockView)
import Explorer.View.Blocks (blocksView)
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard.Dashboard (dashboardView)
import Explorer.View.Footer (footerView)
import Explorer.View.Header (headerView)
import Explorer.View.NotFound (notFoundView)
import Explorer.View.Playground (playgroundView)
import Explorer.View.Transaction (transactionView)
import Pux.Html (Html, div, main) as P
import Pux.Html.Attributes (className) as P

view :: State -> P.Html Action
view state =
    let mobileMenuClazz = if state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
                          then " mobile__menu--opened"
                          else ""
    in
    P.div
      [ P.className $ "explorer-container" <> mobileMenuClazz ]
      [ P.div
          [ P.className "explorer-bg__container" ]
          []
        , P.div
              [ P.className "explorer-content__wrapper"]
              [ P.main
                  [ P.className "explorer-content" ]
                  [ case state.route of
                      Dashboard -> dashboardView state
                      (Tx id) -> transactionView state
                      (Address address) -> addressView state
                      (Epoch epoch) -> blocksView state
                      (EpochSlot epoch slot) -> blocksView state
                      Calculator -> calculatorView state
                      (Block hash) -> blockView state
                      Playground -> playgroundView state
                      NotFound -> notFoundView state
                  ]
              , footerView state
              ]
        , headerView state
      ]
