module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translate)
import Explorer.State (Action, CCurrency(..), State)
import Pux.Html (Html, div, text, h3, table, tr, td, a) as P
import Pux.Html.Attributes (href)
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
                      , transactionHeader state
                      , transactionTable state
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

transactionHeader :: State -> P.Html Action
transactionHeader state =
    P.div
          [ P.className "transaction-header"]
          [ P.a
              [ P.className "hash"
              , href "#" ]
              [ P.text "SCRs8ojgKbClMEXH9IQO1ClGYs-qwXD0V09lxlcQaAw="]
          , P.div
              [ P.className "date"]
              [ P.text "2016-10-17 18:10:05" ]
          , P.div
              [ P.className "amount-container" ]
              [ P.a
                  [ P.className "amount bg-ada"
                  , href "#" ]
                  [ P.text "3,042,900"]
              ]
          ]

transactionTable :: State -> P.Html Action
transactionTable state =
    P.table
          [ P.className "transaction-table"]
          []


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
summaryRow item = do
    let extraCSSClazz =
            case item.currency of
                  Just ADA -> " ada bg-ada-dark"
                  _ -> ""
    P.tr
        []
        [ P.td
            [ P.className "" ]
            [ P.text item.label ]
        , P.td
            [ P.className $ "" <> extraCSSClazz  ]
            [ P.text item.value ]
        ]
