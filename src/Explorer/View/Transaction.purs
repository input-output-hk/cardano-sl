module Explorer.View.Transaction where

import Prelude

import Pux.Html (Html, div, h1, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (translate)
import Explorer.State (State, Action)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

transactionView :: State -> P.Html Action
transactionView state =
    P.div [ P.className "explorer-transaction__container" ]
        [ P.link
              (toUrl Dashboard)
              [ P.className "btn" ]
              [ P.text $ translate _.back state.lang ]
        , placeholderView $ translate _.transaction state.lang
        ]
