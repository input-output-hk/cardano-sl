module Explorer.View.Common (
    placeholderView
    , transactionHeaderView
    , transactionBodyView
    , currencyCSSClass
    ) where

import Data.Maybe (Maybe(..))
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types (Action, State, CCurrency(..))
import Pux.Html (Html, text, div, a, p, span) as P
import Pux.Html.Attributes (className, href) as P
import Pux.Router (link) as P

-- transactions

transactionHeaderView :: State -> P.Html Action
transactionHeaderView state =
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

transactionBodyView :: State -> P.Html Action
transactionBodyView state =
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

-- helper

currencyCSSClass :: Maybe CCurrency -> String
currencyCSSClass mCurrency =
  case mCurrency of
      Just ADA -> " ada bg-ada-dark"
      _ -> ""

-- TODO (jk) Remove placeholderView if all views are implemented
placeholderView :: String -> P.Html Action
placeholderView label =
    P.div
        [ P.className "explorer-dashboard__content" ]
        [ P.text label ]
