module Explorer.View.Dashboard.Blocks (blocksView) where

import Prelude
import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (dashboard, dbLastBlocks, cOf, common, dbExploreBlocks, cUnknown, cEpoch, cSlot, cExpand, cNoData, cAge, cTransactions, cTotalSent, cRelayedBy, cSizeKB) as I18nL
import Explorer.Lenses.State (dashboardBlockPagination, lang, latestBlocks)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State, CBlockEntries)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.Util.Time (prettyDuration)
import Explorer.View.CSS (blocksBody, blocksBodyRow, blocksColumnAge, blocksColumnEpoch, blocksColumnRelayedBy, blocksColumnSize, blocksColumnSlot, blocksColumnTotalSent, blocksColumnTxs, blocksFooter, blocksHeader, blocksWaiting, dashboardContainer, dashboardWrapper) as CSS
import Explorer.View.Common (noData, paginationView)
import Explorer.View.Dashboard.Lenses (dashboardBlocksExpanded, dashboardViewState)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeBlkHash, cbeEpoch, cbeSlot, cbeRelayedBy, cbeSize, cbeTimeIssued, cbeTotalSent, cbeTxNum)
import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P


-- blocks


maxBlockRows :: Int
maxBlockRows = 10

minBlockRows :: Int
minBlockRows = 3

blocksView :: State -> P.Html Action
blocksView state =
    P.div
        [ P.className CSS.dashboardWrapper ]
        [ P.div
            [ P.className CSS.dashboardContainer ]
            [ headerView state headerOptions
            , if null blocks then
                P.div
                    [ P.className CSS.blocksWaiting ]
                    [ P.text $ translate (I18nL.common <<< I18nL.cNoData) lang' ]
              else
                P.div
                    []
                    [ blocksHeaderView state
                    , P.div
                        [ P.className CSS.blocksBody ]
                        $ map (blockRow state) latestBlocks'
                    , P.div
                        [ P.className CSS.blocksFooter ]
                        [ blocksFooterView ]
                    ]
            ]
        ]
      where
        headerOptions = HeaderOptions
            { headline: translate (I18nL.dashboard <<< I18nL.dbLastBlocks) lang'
            , link: Just $ HeaderLink { label: translate (I18nL.dashboard <<< I18nL.dbExploreBlocks) lang'
                                      , action: NoOp }
            }
        blocks = state ^. latestBlocks
        lang' = state ^. lang
        expanded = state ^. dashboardBlocksExpanded
        expandable = length blocks > minBlockRows
        noBlocks = null blocks
        currentBlockPage = state ^. (dashboardViewState <<< dashboardBlockPagination)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows

        latestBlocks' :: CBlockEntries
        latestBlocks' = if expanded
            then slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
            else slice 0 minBlockRows blocks


        blocksFooterView :: P.Html Action
        blocksFooterView =
            if expanded then
                let paginationViewProps =
                      { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                      , currentPage: currentBlockPage
                      , maxPage: flip (/) maxBlockRows <<< length $ state ^. latestBlocks
                      , changePageAction: DashboardPaginateBlocks
                      , onFocusAction: SelectInputText <<< targetToHTMLInputElement
                      }
                in
                    paginationView paginationViewProps
            else
                let
                    clickHandler _ =
                        if expandable
                        then DashboardExpandBlocks true
                        else NoOp
                    visibleBtnExpandClazz = if expandable then "" else " disabled"
                in
                    P.div
                      [ P.className $ "btn-expand" <> visibleBtnExpandClazz
                      , P.onClick clickHandler ]
                      [ P.text $ translate (I18nL.common <<< I18nL.cExpand) lang']


blockRow :: State -> CBlockEntry -> P.Html Action
blockRow state (CBlockEntry entry) =
    P.link (toUrl <<< Block $ entry ^. cbeBlkHash)
        [ P.className CSS.blocksBodyRow ]
        [ blockColumn { label: show $ entry ^. cbeEpoch
                      , clazz: CSS.blocksColumnEpoch
                      }
        , blockColumn { label: show $ entry ^. cbeSlot
                      , clazz: CSS.blocksColumnSlot
                      }
        , blockColumn { label: labelAge
                      , clazz: CSS.blocksColumnAge
                      }
        , blockColumn { label: show $ entry ^. cbeTxNum
                      , clazz: CSS.blocksColumnTxs
                      }
        , blockColumn { label: show $ entry ^. (cbeTotalSent <<< _Coin <<< getCoin)
                      , clazz: CSS.blocksColumnTotalSent
                      }
        , blockColumn { label: labelRelayed
                      , clazz: CSS.blocksColumnRelayedBy
                      }
        , blockColumn { label: show $ entry ^. cbeSize
                      , clazz: CSS.blocksColumnSize
                      }
        ]
    where
        language = state ^. lang
        labelAge = case entry ^. cbeTimeIssued of
                        Just time -> prettyDuration language (Milliseconds
                            $ unwrap $ time ^. _NominalDiffTime)
                        Nothing -> noData
        labelRelayed = fromMaybe (translate (I18nL.common <<< I18nL.cUnknown) language)
                            $ entry ^. cbeRelayedBy



type BlockColumnProps =
    { label :: String
    , clazz :: String
    }

blockColumn :: BlockColumnProps -> P.Html Action
blockColumn props =
    P.div
        [ P.className props.clazz ]
        [ P.text props.label ]

type BlocksHeaderProps =
    { label :: String
    , clazz :: String
    }

mkBlocksHeaderProps :: Language -> Array BlocksHeaderProps
mkBlocksHeaderProps lang =
    [ { label: translate (I18nL.common <<< I18nL.cEpoch) lang
      , clazz: CSS.blocksColumnEpoch
      }
    , { label: translate (I18nL.common <<< I18nL.cSlot) lang
      , clazz: CSS.blocksColumnSlot
      }
    , { label: translate (I18nL.common <<< I18nL.cAge) lang
      , clazz: CSS.blocksColumnAge
      }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , clazz: CSS.blocksColumnTxs
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalSent) lang
      , clazz: CSS.blocksColumnTotalSent
      }
    , { label: translate (I18nL.common <<< I18nL.cRelayedBy) lang
      , clazz: CSS.blocksColumnRelayedBy
      }
    , { label: translate (I18nL.common <<< I18nL.cSizeKB) lang
      , clazz: CSS.blocksColumnSize
      }
    ]

blocksHeaderView :: State -> P.Html Action
blocksHeaderView state =
    P.div
          [ P.className $ CSS.blocksHeader
                <> if null $ state ^. latestBlocks then " invisible" else ""
          ]
          $ map (blockHeaderItemView state) $ mkBlocksHeaderProps state.lang

blockHeaderItemView :: State -> BlocksHeaderProps -> P.Html Action
blockHeaderItemView state props =
    P.div
        [ P.className props.clazz ]
        [ P.text props.label ]
