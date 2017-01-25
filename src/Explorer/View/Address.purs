module Explorer.View.Address where

import Prelude

import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (translate)
import Explorer.Types (State, Action)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

addressView :: State -> P.Html Action
addressView state =
    P.div
        [ P.className "explorer-address" ]
        [ P.div
            [ P.className "explorer-address__container" ]
            [ P.link
                  (toUrl Dashboard)
                  [ P.className "btn" ]
                  [ P.text $ translate _.back state.lang ]
            , placeholderView $ translate _.address state.lang
            ]
        ]
