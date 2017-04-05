module Explorer.View.Dashboard.Blocks (dashBoardBlocksView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (dashboard, dbLastBlocks, common, dbExploreBlocks, cNoData) as I18nL
import Explorer.Lenses.State (lang, latestBlocks)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.Blocks (blockRow, blocksFooterView, blocksHeaderView, currentBlocks)
import Explorer.View.CSS (blocksBody, blocksFooter, blocksWaiting, dashboardContainer, dashboardWrapper) as CSS
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Network.RemoteData (RemoteData(..))
import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P

dashBoardBlocksView :: State -> P.Html Action
dashBoardBlocksView state =
    P.div
        [ P.className CSS.dashboardWrapper ]
        [ P.div
            [ P.className CSS.dashboardContainer ]
            [ headerView state headerOptions
            , case state ^. latestBlocks of
                  NotAsked  -> emptyBlocksView ""
                  Loading   -> emptyBlocksView ""
                  Failure _ -> emptyBlocksView $ translate (I18nL.common <<< I18nL.cNoData) lang'
                  Success blocks ->
                      P.div
                          []
                          [ blocksHeaderView state
                          , P.div
                              [ P.className CSS.blocksBody ]
                              $ map (blockRow state) (currentBlocks state)
                          , P.div
                              [ P.className CSS.blocksFooter ]
                              [ blocksFooterView state ]
                          ]
            ]
        ]
      where
        headerOptions = HeaderOptions
            { headline: translate (I18nL.dashboard <<< I18nL.dbLastBlocks) lang'
            , link: Just $ HeaderLink { label: translate (I18nL.dashboard <<< I18nL.dbExploreBlocks) lang'
                                      , action: NoOp }
            }
        lang' = state ^. lang

emptyBlocksView :: String -> P.Html Action
emptyBlocksView message =
    P.div
        [ P.className CSS.blocksWaiting ]
        [ P.text message ]
