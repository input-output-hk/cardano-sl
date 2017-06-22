module Explorer.View.Transaction (transactionView) where

import Prelude

import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Foldable (for_)

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cBack2Dashboard, cDateFormat, cEpoch, cLoading, cSlot, cTransaction, txNotFound, txFees, cSummary, tx, cTotalOutput, txIncluded, txTime) as I18nL
import Explorer.Lenses.State (currentTxSummary, lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.Factory (mkEpochIndex)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Common (currencyCSSClass, emptyTxHeaderView, mkTxBodyViewProps, mkTxHeaderViewProps, noData, txBodyView, txHeaderView)

import Network.RemoteData (RemoteData(..))

import Pos.Explorer.Web.ClientTypes (CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (ctsBlockEpoch, ctsBlockHash, ctsBlockSlot, ctsFees, ctsTotalOutput, ctsTxTimeIssued)

import Pux.DOM.HTML (HTML) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (a, div, h3, p, span, table, tr, td) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!), (#!))

transactionView :: State -> P.HTML Action
transactionView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-transaction" $ do
        S.div ! S.className "explorer-transaction__wrapper"
              $ S.div ! S.className "explorer-transaction__container" $ do
                  S.h3  ! S.className "headline"
                        $ S.text (translate (I18nL.common <<< I18nL.cTransaction) lang')
                  case state ^. currentTxSummary of
                      NotAsked  -> emptyTxHeaderView
                      Loading   -> textTxHeaderView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                      Failure _ -> failureView lang'
                      Success txSum@(CTxSummary txSummary) ->
                          S.div do
                              txHeaderView lang' $ mkTxHeaderViewProps txSum
                              txBodyView lang' $ mkTxBodyViewProps txSum
        S.div ! S.className "explorer-transaction__wrapper" $ do
            case state ^. currentTxSummary of
                NotAsked  -> emptySummaryView
                Loading   -> emptySummaryView
                Failure _ -> emptySummaryView
                Success txSum@(CTxSummary txSummary) ->
                    S.div ! S.className "explorer-transaction__container" $ do
                        S.h3 ! S.className "headline" $ S.text $ translate (I18nL.common <<< I18nL.cSummary) lang'
                        S.table ! S.className "table-summary"
                                $ for_ (summaryItems txSum lang') summaryRow

type SummaryItems = Array SummaryItem

type SummaryItem =
    { label :: String
    , value :: P.HTML Action
    }

summaryItems :: CTxSummary -> Language -> SummaryItems
summaryItems (ctxSum@CTxSummary txSummary) lang =
    [ { label: translate (I18nL.tx <<< I18nL.txTime) lang
      , value: let  dateFormat = translate (I18nL.common <<< I18nL.cDateFormat) lang
                    dateValue = fromMaybe noData <<< prettyDate dateFormat $ txSummary ^. ctsTxTimeIssued
                in summaryRowSimpleValue dateValue
      }
    , { label: translate (I18nL.tx <<< I18nL.txIncluded) lang
      , value: summaryRowEpochSlot ctxSum lang
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , value:  let adaValue = formatADA (txSummary ^. ctsTotalOutput) lang
                in summaryRowCurrency adaValue ADA
      }
    , { label: translate (I18nL.tx <<< I18nL.txFees) lang
      , value:  let adaValue = formatADA (txSummary ^. ctsFees) lang
                in summaryRowCurrency adaValue ADA
      }
    ]

summaryRowSimpleValue :: String -> P.HTML Action
summaryRowSimpleValue = S.text

summaryRowEpochSlot :: CTxSummary -> Language -> P.HTML Action
summaryRowEpochSlot (CTxSummary ctxSummary) lang =
    S.div do
        case ctxSummary ^. ctsBlockEpoch of
            Just epoch ->
                let epochLabel = translate (I18nL.common <<< I18nL.cEpoch) lang
                    epochRoute = Epoch $ mkEpochIndex epoch
                in
                S.a ! S.href (toUrl epochRoute)
                    #! P.onClick (Navigate $ toUrl epochRoute)
                    ! S.className "link"
                    $ S.text (epochLabel <> " " <> show epoch)
            Nothing ->
                S.span $ S.text noData
        S.text " / "
        case ctxSummary ^. ctsBlockSlot of
            Just slot ->
                let slotLabel   = translate (I18nL.common <<< I18nL.cSlot) lang
                    mBlockHash  = ctxSummary ^. ctsBlockHash
                    slotTag     = maybe S.span (\bHash -> S.a ! S.href (toUrl $ Block bHash)
                                                              #! P.onClick (Navigate <<< toUrl $ Block bHash)
                                             ) mBlockHash
                    slotClazz   = maybe "" (\_ -> "link") mBlockHash
                in
                slotTag ! S.className slotClazz
                        $ S.text (slotLabel <> " " <> show slot)
            Nothing ->
                S.span $ S.text noData

summaryRowCurrency :: String -> CCurrency -> P.HTML Action
summaryRowCurrency value currency =
    S.span  ! S.className (currencyCSSClass $ Just currency)
            $ S.text value

summaryRow :: SummaryItem -> P.HTML Action
summaryRow item =
    S.tr do
        S.td $ S.text item.label
        S.td $ item.value

textTxHeaderView :: String -> P.HTML Action
textTxHeaderView message =
    S.div ! S.className "explorer-transaction__message"
          $ S.div ! P.dangerouslySetInnerHTML message
                  $ mempty


emptySummaryView :: P.HTML Action
emptySummaryView =
    S.div ! S.className "explorer-transaction__container"
          $ mempty

failureView :: Language -> P.HTML Action
failureView lang =
    S.div do
        S.p ! S.className "tx-failed"
            $ S.text (translate (I18nL.tx <<< I18nL.txNotFound) lang)
        S.a ! S.className "btn-back"
            ! S.href (toUrl Dashboard)
            #! P.onClick (Navigate $ toUrl Dashboard)
            $ S.text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
