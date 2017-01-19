module Explorer.View.Layout where

import Pux.Html (Html, div, h1, main, text) as P
import Pux.Html.Attributes (className) as P

import Explorer.Routes (Route(..))
import Explorer.State (State, Action)
import Explorer.View.Address (addressView)
import Explorer.View.Header (headerView)
import Explorer.View.Footer (footerView)
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard (dashboardView)
import Explorer.View.Transaction (transactionView)

view :: State -> P.Html Action
view state =
    P.div
      []
      [
        headerView state
        , P.main
          [ P.className "explorer-content" ]
          [ case state.route of
                Dashboard -> dashboardView state
                Transaction -> transactionView state
                Address -> addressView state
                Calculator -> calculatorView state
                NotFound -> notFoundView
          ]
        , footerView state
      ]

notFoundView :: P.Html Action
notFoundView =
  P.div [] [
    P.h1 [] [P.text "404 Not Found"]
  ]
