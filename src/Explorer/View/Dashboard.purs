module Explorer.View.Dashboard where

import Prelude

import Pux.Html (Html, button, div, h3, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P

import Explorer.I18n.Lang (Language(..), translate)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (State, Action(..))
import Explorer.Util.String (substitute)

dashboardView :: State -> P.Html Action
dashboardView state = do
    let lang = state.lang
    let btnClazz = "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"
    P.div
        [ P.className "dashboard" ]
        [ P.h3
            [ P.className "label-count" ]
            [ P.text <<< flip substitute [show state.count] $ translate _.counted lang ]
        , P.button
            [ P.className btnClazz, P.onClick (const $ Count) ]
            [ P.text $ translate _.count lang ]
        , P.link (toUrl Transaction)
            [ P.className btnClazz ]
            [ P.text $ translate _.transaction lang ]
        , P.link (toUrl Address)
            [ P.className btnClazz ]
            [ P.text $ translate _.address lang ]
        , P.link (toUrl Calculator)
            [ P.className btnClazz ]
            [ P.text $ translate _.calculator lang ]
        , P.button
            [ P.className btnClazz, P.onClick (const $ SetLanguage English)]
            [ P.text "EN" ]
        , P.button
            [ P.className btnClazz, P.onClick (const $ SetLanguage German)]
            [ P.text "DE" ]
        ]
