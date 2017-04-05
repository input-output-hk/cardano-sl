module Explorer.View.Blocks
    ( maxBlockRows
    , minBlockRows
    , blocksView
    , blockRow
    , latestBlocks'
    , currentBlocks
    , blocksHeaderView
    , blockHeaderItemView
    , blocksFooterView
    ) where

import Prelude
import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (block, blNotFound, cLoading, cOf, common, cUnknown, cEpoch, cSlot, cExpand, cAge, cTransactions, cTotalSent, cRelayedBy, cSizeKB) as I18nL
import Explorer.Lenses.State (dashboardBlockPagination, lang, latestBlocks)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State, CBlockEntries)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.Util.Time (prettyDuration)
import Explorer.View.CSS (blocksBody, blocksBodyRow, blocksColumnAge, blocksColumnEpoch, blocksColumnRelayedBy, blocksColumnSize, blocksColumnSlot, blocksColumnTotalSent, blocksColumnTxs, blocksFooter, blocksHeader) as CSS
import Explorer.View.Common (noData, paginationView)
import Explorer.View.Dashboard.Lenses (dashboardBlocksExpanded, dashboardViewState)
import Network.RemoteData (RemoteData(..))
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeBlkHash, cbeEpoch, cbeSlot, cbeRelayedBy, cbeSize, cbeTimeIssued, cbeTotalSent, cbeTxNum)
import Pux.Html (Html, div, text, h3) as P
import Pux.Html.Attributes (className, dangerouslySetInnerHTML) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P

maxBlockRows :: Int
maxBlockRows = 10

minBlockRows :: Int
minBlockRows = 3

blocksView :: State -> P.Html Action
blocksView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-blocks" ]
        [ P.div
            [ P.className "explorer-blocks__wrapper" ]
            [ P.div
                [ P.className "explorer-blocks__container" ]
                [ P.h3
                      [ P.className "headline" ]
                      [ P.text $
                            (translate (I18nL.common <<< I18nL.cEpoch) lang')
                            <> " / " <>
                            (translate (I18nL.common <<< I18nL.cSlot) lang')
                      ]
                , case state ^. latestBlocks of
                      NotAsked  -> emptyBlocksView ""
                      Loading   -> emptyBlocksView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                      Failure _ -> emptyBlocksView $ translate (I18nL.block <<< I18nL.blNotFound) lang'
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
        ]



emptyBlocksView :: String -> P.Html Action
emptyBlocksView message =
    P.div
        [ P.dangerouslySetInnerHTML message ]
        []

latestBlocks' :: State -> CBlockEntries
latestBlocks' state =
    case state ^. latestBlocks of
            Success blocks' -> blocks'
            _ -> []

currentBlocks :: State -> CBlockEntries
currentBlocks state =
    if expanded
    then slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
    else slice 0 minBlockRows blocks
    where
        blocks = latestBlocks' state
        expanded = state ^. dashboardBlocksExpanded
        currentBlockPage = state ^. (dashboardViewState <<< dashboardBlockPagination)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows


blocksFooterView :: State -> P.Html Action
blocksFooterView state =
    if expanded then
        let paginationViewProps =
              { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
              , currentPage: currentBlockPage
              , maxPage: flip (/) maxBlockRows $ length blocks
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
    where
        lang' = state ^. lang
        blocks = latestBlocks' state
        expanded = state ^. dashboardBlocksExpanded
        expandable = length blocks > minBlockRows
        currentBlockPage = state ^. (dashboardViewState <<< dashboardBlockPagination)


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
                <> if null $ latestBlocks' state then " invisible" else ""
          ]
          $ map (blockHeaderItemView state) $ mkBlocksHeaderProps state.lang

blockHeaderItemView :: State -> BlocksHeaderProps -> P.Html Action
blockHeaderItemView state props =
    P.div
        [ P.className props.clazz ]
        [ P.text props.label ]
