module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cBack2Dashboard, cDateFormat, cEpoch, cLoading, cSlot, cTransaction, txNotFound, txFees, cSummary, tx, cTotalOutput, txIncluded, txTime) as I18nL
import Explorer.Lenses.State (currentTxSummary, lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.Factory (mkEpochIndex)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Common (currencyCSSClass, emptyTxHeaderView, mkTxBodyViewProps, mkTxHeaderViewProps, noData, txBodyView, txHeaderView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, ctsBlockEpoch, ctsBlockHash, ctsBlockSlot, ctsFees, ctsTotalOutput, ctsTxTimeIssued)

import Text.Smolder.HTML (div, h3, p, span, table, tr, td)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!))

import Pux.DOM.HTML (HTML) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P

transactionView :: State -> P.HTML Action
transactionView state =
    let lang' = state ^. lang in
    div ! className "explorer-transaction" $ do
        div ! className "explorer-transaction__wrapper" $ do
            div ! className "explorer-transaction__container" $ do
                h3  ! className "headline"
                    $ text (translate (I18nL.common <<< I18nL.cTransaction) lang')
                case state ^. currentTxSummary of
                    NotAsked  -> emptyTxHeaderView
                    Loading   -> textTxHeaderView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                    Failure _ -> failureView lang'
                    Success txSum@(CTxSummary txSummary) ->
                        div do
                            txHeaderView lang' $ mkTxHeaderViewProps txSum
                            txBodyView lang' $ mkTxBodyViewProps txSum

        div ! className "explorer-transaction__wrapper" $ do
            case state ^. currentTxSummary of
                NotAsked  -> emptySummaryView
                Loading   -> emptySummaryView
                Failure _ -> emptySummaryView
                Success txSum@(CTxSummary txSummary) ->
                    div ! className "explorer-transaction__container" $ do
                        h3 ! className "headline" $ text $ translate (I18nL.common <<< I18nL.cSummary) lang'
                        table ! className "table-summary" $ do
                            map summaryRow $ summaryItems txSum lang'

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

emptySummaryRow :: P.HTML Action
emptySummaryRow = tr

summaryRowSimpleValue :: String -> P.HTML Action
summaryRowSimpleValue = text

summaryRowEpochSlot :: CTxSummary -> Language -> P.HTML Action
summaryRowEpochSlot (CTxSummary ctxSummary) lang =
    div do
        case ctxSummary ^. ctsBlockEpoch of
            Just epoch ->
                let epochLabel = translate (I18nL.common <<< I18nL.cEpoch) lang
                    epochRoute = Epoch $ mkEpochIndex epoch
                in
                a ! href (toUrl epochRoute)
                  #! onClick (Navigate $ toUrl epochRoute)
                  ! className "link"
                  $ text (epochLabel <> " " <> show epoch)
            Nothing ->
                span $ text noData
        text $ " / "
        case ctxSummary ^. ctsBlockSlot of
            Just slot ->
                let slotLabel   = translate (I18nL.common <<< I18nL.cSlot) lang
                    mBlockHash  = ctxSummary ^. ctsBlockHash
                    slotTag     = maybe span (\bHash -> a ! href (toUrl $ Block bHash)
                                                          #! onClick (toUrl $ Block bHash)
                                             ) mBlockHash
                    slotClazz   = maybe "" (\_ -> "link") mBlockHash
                in
                slotTag ! className slotClazz
                        $ text (slotLabel <> " " <> show slot)
            Nothing ->
                span $ text noData

summaryRowCurrency :: String -> CCurrency -> P.HTML Action
summaryRowCurrency value currency =
    span  ! className (currencyCSSClass $ Just currency)
          $ text value

summaryRow :: SummaryItem -> P.HTML Action
summaryRow item =
    tr do
        td $ text item.label
        td $ text item.value

textTxHeaderView :: String -> P.HTML Action
textTxHeaderView message =
    div ! className "explorer-transaction__message" $ do
        div ! P.dangerouslySetInnerHTML message


emptySummaryView :: P.HTML Action
emptySummaryView =
    div ! className "explorer-transaction__container"

failureView :: Language -> P.HTML Action
failureView lang =
    div do
        p ! className "tx-failed"
          $ text (translate (I18nL.tx <<< I18nL.txNotFound) lang)
        a ! className "btn-back"
          ! href (toUrl Dashboard)
          #! onClick (toUrl Dashboard)
          $ text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
