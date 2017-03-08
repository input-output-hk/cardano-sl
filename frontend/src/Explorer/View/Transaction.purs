module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cTransaction, cSummary, tx, cTotalOutput, txRelayed, txIncluded, txTime) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.Factory (mkEmptyCTxEntry)
import Explorer.View.Common (currencyCSSClass, transactionHeaderView, transactionBodyView)
import Pos.Explorer.Web.ClientTypes (CHash)
import Pux.Html (Html, div, text, h3, table, tr, td) as P
import Pux.Html.Attributes (className) as P

transactionView :: State -> CHash -> P.Html Action
transactionView state hash =
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
                -- TODO (jk) use empty CTxEntry if we'll have real data
                , transactionHeaderView mkEmptyCTxEntry
                , transactionBodyView state
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
                      <<< map summaryRow $ summaryItems lang'
                ]
            ]
        ]


-- currency

type SummaryItems = Array SummaryItem

type SummaryItem =
    { label :: String
    , value :: String
    , currency :: Maybe CCurrency
    }

summaryItems :: Language -> SummaryItems
summaryItems lang =
    [ { label: translate (I18nL.tx <<< I18nL.txTime) lang
      , value: "2016-07-08 11:56:48"
      , currency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txIncluded) lang
      , value: "419827 (2016-07-08 12:02:52 + 6 minutes)"
      , currency: Nothing
      }
    , { label: translate (I18nL.tx <<< I18nL.txRelayed) lang
      , value: "78.129.167.5 (whois)"
      , currency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , value: "3,027,500"
      , currency: Just ADA
      }
    ]

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
