module Explorer.View.Address where

import Prelude

import Pux.Html (Html, div, h1, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (translate)
import Explorer.State (State, Action)
import Explorer.Routes (Route(Dashboard), toUrl)

addressView :: State -> P.Html Action
addressView state =
    P.div [ P.className "address" ]
        [ P.h1
              []
              [ P.text $ translate _.address state.lang ]
        , P.link
              (toUrl Dashboard)
              [ P.className "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect" ]
              [ P.text $ translate _.back state.lang ]
        ]
