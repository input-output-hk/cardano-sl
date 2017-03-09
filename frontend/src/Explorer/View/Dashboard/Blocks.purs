module Explorer.View.Dashboard.Blocks (blocksView, prettyDuration) where

import Prelude
import Data.Array (length, null, slice)
import Data.DateTime (Millisecond)
import Data.Int (floor, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith, trim)
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..), Hours(..), Days(..), convertDuration, toDuration, fromDuration)
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Data.Tuple (uncurry, Tuple(..))
import Explorer.I18n.Lang (Language, languageNativeName, translate)
import Explorer.I18n.Lenses (dashboard, dbLastBlocks, cOf, common, dbExploreBlocks, cUnknown, cHeight, cExpand, cNoData, cAge, cTransactions, cTotalSent, cRelayedBy, cSizeKB, cDays, cHours, cMinutes, cSeconds) as I18nL
import Explorer.Lenses.State (dashboardBlockPagination, lang, latestBlocks)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State, CBlockEntries)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.View.Common (paginationView)
import Explorer.View.Dashboard.Lenses (dashboardBlocksExpanded, dashboardViewState)
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeBlkHash, cbeHeight, cbeRelayedBy, cbeSize, cbeTimeIssued, cbeTotalSent, cbeTxNum)
import Pos.Types.Lenses.Core (_Coin, getCoin)
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
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
            [ P.className "explorer-dashboard__container" ]
            [ headerView state headerOptions
            , blocksHeaderView state
            , P.div
                [ P.className $ "blocks-waiting" <> visibleWaitingClazz ]
                [ P.text $ translate (I18nL.common <<< I18nL.cNoData) lang' ]
            , P.div
                [ P.className $ "blocks-body" <> visibleBlockClazz ]
                $ map (blockRow state) latestBlocks'
            , P.div
                [ P.className $ "blocks-footer" <> visibleBlockClazz ]
                [ blocksFooterView ]
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
        visibleBlockClazz = if noBlocks then " invisible" else ""
        visibleWaitingClazz = if not noBlocks then " invisible" else ""
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
        [ P.className "blocks-body__row" ]
        [ blockColumn <<< show $ entry ^. cbeHeight
        , blockColumn (prettyDuration language (Milliseconds $ unwrap $ entry ^. (cbeTimeIssued <<< _NominalDiffTime)))
        -- , blockColumn <<< show <<<  $ unwrap $ entry ^. (cbeTimeIssued <<< _NominalDiffTime)
        , blockColumn <<< show $ entry ^. cbeTxNum
        , blockColumn <<< show $ entry ^. (cbeTotalSent <<< _Coin <<< getCoin)
        , blockColumn <<< fromMaybe (translate (I18nL.common <<< I18nL.cUnknown) language) $ entry ^. cbeRelayedBy
        , blockColumn <<< show $ entry ^. cbeSize
        ]
    where
      language      = state ^. lang

-- cDays cHours cMinutes cSeconds
prettyDuration :: forall a. Duration a => Language -> a -> String
prettyDuration lang dur | convertDuration dur < Minutes 1.0 = "< 1 " <> translate (I18nL.common <<< I18nL.cMinutes) lang
                        | otherwise = trim $ joinWith " " $ map (uncurry showIfNonZero)
                             [ Tuple d translationDays
                             , Tuple h translationHours
                             , Tuple m translationMinutes
                             ]
  where
    translationMinutes  = translate (I18nL.common <<< I18nL.cMinutes) lang
    translationHours    = translate (I18nL.common <<< I18nL.cHours) lang
    translationDays     = translate (I18nL.common <<< I18nL.cDays) lang
    m = floor <<< unMinutes $ convertDuration dur `sub` convertDuration (Days $ toNumber d) `sub` convertDuration (Hours $ toNumber h)
    h = floor <<< unHours $ convertDuration dur `sub` convertDuration (Days $ toNumber d)
    d = floor <<< unDays $ convertDuration dur
    unHours     (Hours m) = m
    unMinutes (Minutes m) = m
    unDays       (Days m) = m
    showIfNonZero nu st =
        if nu == 0
            then ""
            else show nu <> " " <> st

blockColumn :: String -> P.Html Action
blockColumn value =
    P.div
        [ P.className "blocks-body__column" ]
        [ P.text value ]

blocksHeaderView :: State -> P.Html Action
blocksHeaderView state =
    P.div
          [ P.className $ "blocks-header" <> if null $ state ^. latestBlocks then " invisible" else "" ]
          $ map (blockHeaderItemView state) $ mkBlocksHeaderItems state.lang

blockHeaderItemView :: State -> String -> P.Html Action
blockHeaderItemView state label =
    P.div
        [ P.className "blocks-header__item" ]
        [ P.text label ]

mkBlocksHeaderItems :: Language -> Array String
mkBlocksHeaderItems lang =
    [ translate (I18nL.common <<< I18nL.cHeight) lang
    , translate (I18nL.common <<< I18nL.cAge) lang
    , translate (I18nL.common <<< I18nL.cTransactions) lang
    , translate (I18nL.common <<< I18nL.cTotalSent) lang
    , translate (I18nL.common <<< I18nL.cRelayedBy) lang
    , translate (I18nL.common <<< I18nL.cSizeKB) lang
    ]
