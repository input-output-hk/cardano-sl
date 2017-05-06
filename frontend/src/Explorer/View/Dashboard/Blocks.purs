module Explorer.View.Dashboard.Blocks (dashBoardBlocksView) where

import Prelude
import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (cExpand, cOf, cLoading, dashboard, dbLastBlocks, common, dbExploreBlocks, cNoData) as I18nL
import Explorer.Lenses.State (dbViewBlockPagination, dbViewBlockPaginationEditable, dbViewBlocksExpanded, dbViewLoadingBlockPagination, lang, latestBlocks, totalBlocks)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State, CBlockEntries)
import Explorer.View.Blocks (blockRow, blocksHeaderView, maxBlockRows, minBlockRows)
import Explorer.View.CSS (blocksBody, blocksBodyWrapper, blocksBodyCover, blocksBodyCoverLabel, blocksFooter, blocksWaiting, dashboardContainer, dashboardWrapper) as CSS
import Explorer.View.Common (getMaxPaginationNumber, paginationView)
import Explorer.View.Dashboard.Lenses (dashboardBlocksExpanded, dashboardViewState)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Network.RemoteData (RemoteData(..), withDefault)
import Pux.Html (Html, div, p, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P

dashBoardBlocksView :: State -> P.Html Action
dashBoardBlocksView state =
    P.div
        [ P.className CSS.dashboardWrapper ]
        [ P.div
            [ P.className CSS.dashboardContainer ]
            [ headerView state headerOptions
            , case state ^. latestBlocks of
                  NotAsked  -> emptyBlocksView ""
                  Loading -> if hasBlocks then blocksView else emptyBlocksView ""
                  Failure _ -> emptyBlocksView $ translate (I18nL.common <<< I18nL.cNoData) lang'
                  Success _ -> blocksView
            ]
        ]
      where
        headerOptions = HeaderOptions
            { headline: translate (I18nL.dashboard <<< I18nL.dbLastBlocks) lang'
            , link: Just $ HeaderLink { label: translate (I18nL.dashboard <<< I18nL.dbExploreBlocks) lang'
                                      , action: NoOp }
            }
        lang' = state ^. lang
        hasBlocks = not null $ withDefault [] $ state ^. latestBlocks
        blocksView =
            P.div
                []
                [ blocksHeaderView (withDefault [] $ state ^. latestBlocks) lang'
                , P.div
                    [ P.className CSS.blocksBodyWrapper ]
                    [ P.div
                        [ P.className CSS.blocksBody ]
                        $ map (blockRow state) (currentBlocks state)
                    , P.div
                        [ P.className $ CSS.blocksBodyCover
                        <>  if state ^. (dashboardViewState <<< dbViewLoadingBlockPagination)
                            then " show"
                            else ""
                        ]
                        [ P.p
                              [ P.className CSS.blocksBodyCoverLabel ]
                              [ P.text $ translate (I18nL.common <<< I18nL.cLoading) lang' ]
                        ]
                    ]

                , P.div
                    [ P.className CSS.blocksFooter ]
                    [ blocksFooterView state ]
                ]

emptyBlocksView :: String -> P.Html Action
emptyBlocksView message =
    P.div
        [ P.className CSS.blocksWaiting ]
        [ P.text message ]

currentBlocks :: State -> CBlockEntries
currentBlocks state =
    if expanded
    then slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
    else slice 0 minBlockRows blocks
    where
        blocks = withDefault [] $ state ^. latestBlocks
        expanded = state ^. dashboardBlocksExpanded
        currentBlockPage = state ^. (dashboardViewState <<< dbViewBlockPagination)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows

blocksFooterView :: State -> P.Html Action
blocksFooterView state =
    if expanded then
        paginationView { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                        , currentPage: currentBlockPage
                        , minPage: minPagination
                        , maxPage: getMaxPaginationNumber totalBlocks' maxBlockRows
                        , changePageAction: DashboardPaginateBlocks
                        , editable: state ^. (dashboardViewState <<< dbViewBlockPaginationEditable)
                        , editableAction: DashboardEditBlocksPageNumber
                        , invalidPageAction: DashboardInvalidBlocksPageNumber
                        }
    else
        P.div
            [ P.className $ "btn-expand" <> visibleBtnExpandClazz
            , P.onClick clickHandler ]
            [ P.text $ translate (I18nL.common <<< I18nL.cExpand) lang']
    where
        lang' = state ^. lang
        blocks = withDefault [] $ state ^. latestBlocks
        -- Note: A value of `0` will not be displayed, because paginator is hidden in such a case
        totalBlocks' = withDefault 0 $ state ^. totalBlocks
        expanded = state ^. (dashboardViewState <<< dbViewBlocksExpanded)
        expandable = length blocks > minBlockRows
        currentBlockPage = state ^. (dashboardViewState <<< dbViewBlockPagination)
        clickHandler _ =
            if expandable
            then DashboardExpandBlocks true
            else NoOp
        visibleBtnExpandClazz = if expandable then "" else " disabled"
