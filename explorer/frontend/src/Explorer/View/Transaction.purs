module Explorer.View.Transaction (transactionView) where

import Prelude

import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Explorer.View.CSS as CSS
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderOptions(..))

import Network.RemoteData (RemoteData(..))

import Pos.Explorer.Web.ClientTypes (CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (ctsBlockEpoch, ctsBlockHash, ctsBlockSlot, ctsFees, ctsTotalOutput, ctsTxTimeIssued)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML as S
import Text.Smolder.HTML.Attributes as SA
import Text.Smolder.Markup as SM
import Text.Smolder.Markup ((!), (#!))

transactionView :: State -> P.HTML Action
transactionView state = do
  S.div ! SA.className CSS.pureGContainer $ do
    S.div ! SA.className "pure-u-1-1" $ do
      headerView state $ headerOptions (translate (I18nL.common <<< I18nL.cTransaction) lang')
      case state ^. currentTxSummary of
          NotAsked  -> emptyTxHeaderView
          Loading   -> textTxHeaderView $ translate (I18nL.common <<< I18nL.cLoading) lang'
          Failure _ -> failureView lang'
          Success txSum@(CTxSummary txSummary) -> do
            S.table ! SA.className "pure-table pure-table-horizontal" $ do
                txHeaderView lang' $ mkTxHeaderViewProps txSum
                txBodyView lang' $ mkTxBodyViewProps txSum
      S.div $ do
          case state ^. currentTxSummary of
              NotAsked  -> emptySummaryView
              Loading   -> emptySummaryView
              Failure _ -> emptySummaryView
              Success txSum@(CTxSummary txSummary) ->
                  S.div $ do
                    headerView state $ headerOptions (translate (I18nL.common <<< I18nL.cSummary) lang')
                    S.table ! SA.className "pure-table pure-table-horizontal" $
                      for_ (summaryItems txSum lang') summaryRow
    where
      lang' = state ^. lang
      headerOptions t = HeaderOptions
          { headline: t
          , link: Nothing
          , icon: Just "fa-cube"
          }


type SummaryItems = Array SummaryItem

type SummaryItem =
    { label :: String
    , value :: P.HTML Action
    }

summaryItems :: CTxSummary -> Language -> SummaryItems
summaryItems (ctxSum@CTxSummary txSummary) lang =
    [ { label: translate (I18nL.tx <<< I18nL.txTime) lang
      , value: let  dateFormat = translate (I18nL.common <<< I18nL.cDateFormat) lang
                    mDate      = txSummary ^. ctsTxTimeIssued
                    mDateValue = mDate >>= prettyDate dateFormat
                    dateValue  = fromMaybe noData mDateValue
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
summaryRowSimpleValue = SM.text

summaryRowEpochSlot :: CTxSummary -> Language -> P.HTML Action
summaryRowEpochSlot (CTxSummary ctxSummary) lang =
    S.div do
        case ctxSummary ^. ctsBlockEpoch of
            Just epoch ->
                let epochLabel = translate (I18nL.common <<< I18nL.cEpoch) lang
                    epochRoute = Epoch $ mkEpochIndex epoch
                in
                S.a ! SA.href (toUrl epochRoute)
                    #! P.onClick (Navigate $ toUrl epochRoute)
                    ! SA.className "link"
                    $ SM.text (epochLabel <> " " <> show epoch)
            Nothing ->
                S.span $ SM.text noData
        SM.text " / "
        case ctxSummary ^. ctsBlockSlot of
            Just slot ->
                let slotLabel   = translate (I18nL.common <<< I18nL.cSlot) lang
                    mBlockHash  = ctxSummary ^. ctsBlockHash
                    slotTag     = maybe S.span (\bHash -> S.a ! SA.href (toUrl $ Block bHash)
                                                              #! P.onClick (Navigate <<< toUrl $ Block bHash)
                                             ) mBlockHash
                    slotClazz   = maybe "" (\_ -> "link") mBlockHash
                in
                slotTag ! SA.className slotClazz
                        $ SM.text (slotLabel <> " " <> show slot)
            Nothing ->
                S.span $ SM.text noData

summaryRowCurrency :: String -> CCurrency -> P.HTML Action
summaryRowCurrency value currency =
    S.span  ! SA.className (currencyCSSClass $ Just currency)
            $ SM.text value

summaryRow :: SummaryItem -> P.HTML Action
summaryRow item =
    S.tr do
        S.td $ SM.text item.label
        S.td $ item.value

textTxHeaderView :: String -> P.HTML Action
textTxHeaderView message =
    S.div ! SA.className "explorer-transaction__message"
          $ S.div $ SM.text message


emptySummaryView :: P.HTML Action
emptySummaryView =
    S.div ! SA.className "explorer-transaction__container"
          $ SM.text ""

failureView :: Language -> P.HTML Action
failureView lang =
    S.div do
        S.p ! SA.className "tx-failed"
            $ SM.text (translate (I18nL.tx <<< I18nL.txNotFound) lang)
        S.a ! SA.className "btn-back"
            ! SA.href (toUrl Dashboard)
            #! P.onClick (Navigate $ toUrl Dashboard)
            $ SM.text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
