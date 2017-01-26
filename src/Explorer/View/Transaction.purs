module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translate)
import Explorer.Types (Action, CCurrency(..), State)
import Explorer.View.Common (currencyCSSClass, transactionHeaderView, transactionBodyView)
import Pux.Html (Html, div, text, h3, table, tr, td) as P
import Pux.Html.Attributes (className) as P

transactionView :: State -> P.Html Action
transactionView state =
    P.div
        [ P.className "explorer-transaction" ]
        [ P.div
              [ P.className "explorer-transaction__wrapper" ]
              [ P.div
                    [ P.className "explorer-transaction__container" ]
                    [ P.h3
                        [ P.className "headline"]
                        [ P.text $ translate _.transaction state.lang ]
                    , transactionHeaderView state
                    , transactionBodyView state
                    ]
              ]
        ,  P.div
                [ P.className "explorer-transaction__wrapper" ]
                [ P.div
                      [ P.className "explorer-transaction__container" ]
                      [ P.h3
                              [ P.className "headline"]
                              [ P.text $ translate _.summary state.lang ]
                        , P.table
                              [ P.className "table-summary" ]
                              $ map summaryRow summaryItems

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

summaryItems :: SummaryItems
summaryItems =
    [ { label: "#Received time", value: "2016-07-08 11:56:48", currency: Nothing }
    , { label: "#Included In Blocks", value: "419827 (2016-07-08 12:02:52 + 6 minutes)", currency: Nothing }
    , { label: "#Relayed by IP", value: "78.129.167.5 (whois)", currency: Nothing }
    , { label: "#Total Output", value: "3,027,500", currency: Just ADA }
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
