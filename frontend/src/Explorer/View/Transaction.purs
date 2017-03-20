module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Time.NominalDiffTime.Lenses (_NominalDiffTime)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cTransaction, cTransactionFeed, cSummary, tx, cTotalOutput, txRelayed, txIncluded, txTime) as I18nL
import Explorer.Lenses.State (currentTxSummary, lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (CCurrency(..), State)
import Explorer.View.Common (currencyCSSClass, emptyTxHeaderView, mkTxHeaderViewProps, noData, transactionBodyView', txHeaderView)
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Explorer.Web.ClientTypes (CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CNetworkAddress, ctsBlockHeight, ctsFees, ctsRelayedBy, ctsTotalOutput, ctsTxTimeIssued)
import Pux.Html (Html, div, text, h3, table, tr, td) as P
import Pux.Html.Attributes (className) as P

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
                        Nothing ->
                            emptyTxHeaderView state
                        Just txSummary ->
                            P.div
                                []
                                [ txHeaderView $ mkTxHeaderViewProps txSummary
                                , transactionBodyView' txSummary
                                ]
                ]
            ]
        , P.div
            [ P.className "explorer-transaction__wrapper" ]
            [ P.div
                [ P.className "explorer-transaction__container" ]
                [ P.h3
                    [ P.className "headline"]
                    [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
                  , P.table
                      [ P.className "table-summary" ]
                      case state ^. currentTxSummary of
                          Nothing ->
                              [ emptySummaryRow ]
                          Just txSummary ->
                              map summaryRow $ summaryItems txSummary lang'
                ]
            ]
        ]

type SummaryItems = Array SummaryItem

type SummaryItem =
    { label :: String
    , value :: String
    , currency :: Maybe CCurrency
    }

summaryItems :: CTxSummary -> Language -> SummaryItems
summaryItems (CTxSummary txSummary) lang =
    [ { label: translate (I18nL.tx <<< I18nL.txTime) lang
      , value: show $ txSummary ^. (ctsTxTimeIssued <<< _NominalDiffTime)
      , currency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txIncluded) lang
      , value: let  bHeight = case txSummary ^. ctsBlockHeight of
                                Nothing -> noData
                                Just bHeight' -> show bHeight'
                    bTime = case txSummary ^. ctsRelayedBy of
                                Nothing -> noData
                                Just bTime' -> show $ bTime' ^. _CNetworkAddress
                in bHeight <> " (" <> bTime <> ")"
      , currency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txRelayed) lang
      , value:  case txSummary ^. ctsRelayedBy of
                    Nothing -> noData
                    Just relayedBy -> show $ relayedBy ^. _CNetworkAddress
      , currency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , value: show $ txSummary ^. (ctsTotalOutput <<< _Coin <<< getCoin)
      , currency: Just ADA
      }
    , { label: translate (I18nL.common <<< I18nL.cTransactionFeed) lang
      , value: show $ txSummary ^. (ctsFees <<< _Coin <<< getCoin)
      , currency: Just ADA
      }
    ]

emptySummaryRow :: P.Html Action
emptySummaryRow = P.tr [] []

summaryRow :: SummaryItem -> P.Html Action
summaryRow item =
    P.tr
        []
        [ P.td
            [ P.className "" ]
            [ P.text item.label ]
        , P.td
            [ P.className $ "" <> currencyCSSClass item.currency  ]
            [ P.text item.value ]
        ]
