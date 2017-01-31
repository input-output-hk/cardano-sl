module Explorer.View.Calculator where

import Prelude

import Pux.Html (Html, div, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (back, calculator) as I18nL
import Explorer.Types.State (State)
import Explorer.Types.Actions (Action)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

calculatorView :: State -> P.Html Action
calculatorView state =
    P.div
        [ P.className "explorer-calculator" ]
        [ P.div
            [ P.className "explorer-calculator__container" ]
            [ P.link
                  (toUrl Dashboard)
                  [ P.className "" ]
                  [ P.text $ translate I18nL.back state.lang ]
            , placeholderView $ translate I18nL.calculator state.lang
            ]
        ]
