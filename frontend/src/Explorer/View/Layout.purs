module Explorer.View.Layout where

import Prelude
import Data.Lens ((^.))

import Pux.DOM.HTML (Html) as P

import Text.Smolder.HTML (div, main)
import Text.Smolder.HTML.Attributes (className)

import Explorer.Lenses.State (gViewMobileMenuOpenend, globalViewState, route, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Address (addressView)
import Explorer.View.Block (blockView)
import Explorer.View.Blocks (blocksView)
import Explorer.View.CSS (route) as CSS
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard.Dashboard (dashboardView)
import Explorer.View.Footer (footerView)
import Explorer.View.Header (headerView)
import Explorer.View.NotFound (notFoundView)
import Explorer.View.Playground (playgroundView)
import Explorer.View.Transaction (transactionView)

view :: State -> P.Html Action
view state =
    let mobileMenuClazz = if state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
                          then " mobile__menu--opened"
                          else ""
        routeClazz = CSS.route $ state ^. route
    in
    div ! className ("explorer-container" <> mobileMenuClazz <> " " <> routeClazz) $ do
        div ! className "explorer-bg__container" $ do
            div ! className "explorer-content__wrapper" $ do
                main ! className "explorer-content" $ do
                    case state ^. route of
                        Dashboard -> dashboardView state
                        (Tx id) -> transactionView state
                        (Address address) -> addressView state
                        (Epoch epoch) -> blocksView state
                        (EpochSlot epoch slot) -> blocksView state
                        Calculator -> calculatorView state
                        (Block hash) -> blockView state
                        Playground -> playgroundView state
                        NotFound -> notFoundView state
                    footerView state
            headerView state
