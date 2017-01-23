module Explorer.View.Transaction (transactionView) where

import Prelude

import Pux.Html (Html, div, text, h3, table, tr, td) as P
import Pux.Html.Attributes (className) as P

import Explorer.I18n.Lang (translate)
import Explorer.State (State, Action)

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
                    ]
              ]
        ,  P.div
                [ P.className "explorer-transaction__wrapper" ]
                [ P.div
                      [ P.className "explorer-transaction__container" ]
                      [ P.h3
                              [ P.className "headline"]
                              [ P.text "#Summary" ]
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
    }

summaryItems :: SummaryItems
summaryItems =
    [ { label: "#Received time", value: "2016-07-08 11:56:48" }
    , { label: "#Included In Blocks", value: "419827 (2016-07-08 12:02:52 + 6 minutes)" }
    , { label: "#Relayed by IP", value: "78.129.167.5 (whois)" }
    , { label: "#Total Output", value: "3,027,500" }
    ]

summaryRow :: SummaryItem -> P.Html Action
summaryRow item =
    P.tr
        []
        [ P.td
            [ P.className "" ]
            [ P.text item.label ]
        , P.td
            [ P.className "" ]
            [ P.text item.value ]
        ]
