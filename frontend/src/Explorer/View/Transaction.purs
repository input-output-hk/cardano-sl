module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cBack2Dashboard, cDateFormat, cLoading, cTransaction, txNotFound, txFees, cSummary, tx, cTotalOutput, txRelayed, txIncluded, txTime) as I18nL
import Explorer.Lenses.State (currentTxSummary, lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Common (currencyCSSClass', emptyTxHeaderView, mkTxBodyViewProps, mkTxHeaderViewProps, noData, txBodyView, txHeaderView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CCoin, getCoin, _CNetworkAddress, ctsBlockHeight, ctsFees, ctsRelayedBy, ctsTotalOutput, ctsTxTimeIssued)
import Pux.Html (Html, div, text, h3, p, span, table, tr, td) as P
import Pux.Html.Attributes (className, dangerouslySetInnerHTML) as P
import Pux.Router (link) as P

transactionView :: State -> P.Html Action
transactionView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-transaction" ]
        [ P.div
            [ P.className "explorer-transaction__wrapper" ]
            [ P.div
                [ P.className "explorer-transaction__container" ]
                [ P.h3
                    [ P.className "headline"]
                    [ P.text $ translate (I18nL.common <<< I18nL.cTransaction) lang' ]
                  , case state ^. currentTxSummary of
                        NotAsked  -> emptyTxHeaderView
                        Loading   -> textTxHeaderView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                        Failure _ -> failureView lang'
                        Success txSum@(CTxSummary txSummary) ->
                            P.div
                                []
                                [ txHeaderView lang' $ mkTxHeaderViewProps txSum
                                , txBodyView $ mkTxBodyViewProps txSum
                                ]
                ]
            ]
        , P.div
            [ P.className "explorer-transaction__wrapper" ]
            [ case state ^. currentTxSummary of
                  NotAsked  -> emptySummaryView
                  Loading   -> emptySummaryView
                  Failure _ -> emptySummaryView
                  Success txSum@(CTxSummary txSummary) ->
                      P.div
                          [ P.className "explorer-transaction__container" ]
                          [ P.h3
                              [ P.className "headline"]
                              [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
                          , P.table
                              [ P.className "table-summary" ]
                              <<< map summaryRow $ summaryItems txSum lang'
                          ]
            ]
        ]

type SummaryItems = Array SummaryItem

type SummaryItem =
    { label :: String
    , value :: String
    , mCurrency :: Maybe CCurrency
    }

summaryItems :: CTxSummary -> Language -> SummaryItems
summaryItems (CTxSummary txSummary) lang =
    [ { label: translate (I18nL.tx <<< I18nL.txTime) lang
      , value: let dateFormat = translate (I18nL.common <<< I18nL.cDateFormat) lang in
               fromMaybe noData <<< prettyDate dateFormat $ txSummary ^. ctsTxTimeIssued
      , mCurrency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txIncluded) lang
      , value: let  bHeight = case txSummary ^. ctsBlockHeight of
                                Nothing -> noData
                                Just bHeight' -> show bHeight'
                    bTime = case txSummary ^. ctsRelayedBy of
                                Nothing -> noData
                                Just bTime' -> show $ bTime' ^. _CNetworkAddress
                in bHeight <> " (" <> bTime <> ")"
      , mCurrency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txRelayed) lang
      , value:  case txSummary ^. ctsRelayedBy of
                    Nothing -> noData
                    Just relayedBy -> show $ relayedBy ^. _CNetworkAddress
      , mCurrency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , value: txSummary ^. (ctsTotalOutput <<< _CCoin <<< getCoin)
      , mCurrency: Just ADA
      }
    , { label: translate (I18nL.tx <<< I18nL.txFees) lang
      , value: txSummary ^. (ctsFees <<< _CCoin <<< getCoin)
      , mCurrency: Just ADA
      }
    ]

emptySummaryRow :: P.Html Action
emptySummaryRow = P.tr [] []

summaryRow :: SummaryItem -> P.Html Action
summaryRow item =
    P.tr
        []
        [ P.td
            []
            [ P.text item.label ]
        , P.td
            []
            if isJust item.mCurrency
            then
            [ P.span
              [ P.className $ currencyCSSClass' item.mCurrency ]
              [ P.text item.value ]
            ]
            else
            [ P.text item.value ]
        ]

textTxHeaderView :: String -> P.Html Action
textTxHeaderView message =
    P.div
        [ P.className "explorer-transaction__message" ]
        [ P.div
            [ P.dangerouslySetInnerHTML message ]
            []
        ]


emptySummaryView :: P.Html Action
emptySummaryView =
    P.div
        [ P.className "explorer-transaction__container" ]
        []

failureView :: Language -> P.Html Action
failureView lang =
    P.div
        []
        [ P.p
            [ P.className "tx-failed" ]
            [ P.text $ translate (I18nL.tx <<< I18nL.txNotFound) lang ]
        , P.link (toUrl Dashboard)
            [ P.className "btn-back" ]
            [ P.text $ translate (I18nL.common <<< I18nL.cBack2Dashboard) lang ]
        ]
