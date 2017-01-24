module Explorer.View.Transaction (transactionView) where

import Prelude
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translate)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (Action, CCurrency(..), State)
import Pux.Html (Html, div, text, h3, table, tr, td, a, span, p) as P
import Pux.Html.Attributes (href) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

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
                      , transactionBody state
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

transactionHeader :: State -> P.Html Action
transactionHeader state =
    P.div
          [ P.className "transaction-header"]
          [ P.link
              (toUrl Transaction)
              [ P.className "hash" ]
              [ P.text "SCRs8ojgKbClMEXH9IQO1ClGYs-qwXD0V09lxlcQaAw="]
          , P.div
              [ P.className "date"]
              [ P.text "2016-10-17 18:10:05" ]
          , P.div
              [ P.className "amount-container" ]
              [ P.a
                  [ P.className "amount bg-ada"
                  , P.href "#" ]
                  [ P.text "3,042,900"]
              ]
          ]

transactionBody :: State -> P.Html Action
transactionBody state =
    P.div
        [ P.className "transaction-body" ]
        [ P.div
          [ P.className "from-hash-container" ]
          [ P.a
              [ P.className "from-hash", P.href "#" ]
              [ P.text "zrVjWkH9pgc9Ng13dXD6C4KQVqnZGFTmuZ" ]
          ]
        , P.div
              [ P.className "to-hash-container bg-transaction-arrow" ]
              [ P.link
                    (toUrl Address)
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9" ]
              , P.link
                    (toUrl Address)
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf" ]
              , P.link
                    (toUrl Address)
                    [ P.className "to-hash"]
                    [ P.text "1NPj2Y8yswHLuw8Yr1FDdobKasdfadsfaf" ]
              ]
        , P.div
              [ P.className "to-alias-container" ]
              [ P.p
                  [ P.className "to-alias" ]
                  [ P.text "to red" ]
              , P.p
                  [ P.className "to-alias" ]
                  [ P.text "to blue" ]
              , P.p
                  [ P.className "to-alias" ]
                  [ P.text "to grey" ]
              ]
        , P.div
              [ P.className "amount-container" ]
              [ P.span
                  [ P.className "amount bg-ada-dark" ]
                  [ P.text "131,100"]
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
