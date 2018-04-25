module Explorer.View.Layout where

import Prelude
import Data.Lens ((^.))

import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))

import Explorer.Lenses.State (gViewMobileMenuOpenend, globalViewState, route, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Address (addressView)
import Explorer.View.Block (blockView)
import Explorer.View.Blocks (epochBlocksView)
import Explorer.View.CSS (route) as CSS
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard.Dashboard (dashboardView)
import Explorer.View.Footer (footerView)
import Explorer.View.GenesisBlock (genesisBlockView)
import Explorer.View.Header (headerView)
import Explorer.View.NotFound (notFoundView)
import Explorer.View.Playground (playgroundView)
import Explorer.View.Transaction (transactionView)

view :: State -> P.HTML Action
view state =
    let mobileMenuClazz = if state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
                          then " mobile__menu--opened"
                          else ""
        routeClazz = CSS.route $ state ^. route
    in
    S.div ! S.className ("explorer-container" <> mobileMenuClazz <> " " <> routeClazz)
          $ S.div ! S.className "explorer-bg__container"
                  $ S.div ! S.className "explorer-content__wrapper" $ do
                      S.div ! S.className "explorer-content" $ do
                            case state ^. route of
                                Dashboard -> dashboardView state
                                (Tx id) -> transactionView state
                                (Address address) -> addressView state
                                (Epoch epoch) -> epochBlocksView state
                                (EpochSlot epoch slot) -> epochBlocksView state
                                Calculator -> calculatorView state
                                (Block hash) -> blockView state
                                GenesisBlock -> genesisBlockView state
                                Playground -> playgroundView state
                                NotFound -> notFoundView state
                            footerView state
                      headerView state
