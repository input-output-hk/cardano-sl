module Explorer.View.Calculator where

import Prelude

import Pux.Html (Html, div, h1, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (translate)
import Explorer.State (State, Action)
import Explorer.Routes (Route(Dashboard), toUrl)

calculatorView :: State -> P.Html Action
calculatorView state =
    P.div [ P.className "calculator" ]
        [ P.h1
              []
              [ P.text $ translate _.calculator state.lang ]
        , P.link
              (toUrl Dashboard)
              [ P.className "btn btn-big btn-outline" ]
              [ P.text $ translate _.back state.lang ]
        ]
