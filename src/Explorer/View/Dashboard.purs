module Explorer.View.Dashboard where

import Pux.Html (Html, button, div, h3, text)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)
import Prelude hiding (div)
import Explorer.State (State, Action(..))
import Explorer.I18n.Lang (Language(..), translate)

dashboardView :: State -> Html Action
dashboardView state =
    div [ className "dashboard" ]
        [ h3  [ className "label-count" ]
              [ text $ translate _.counted lang <> ": " <> show state.count]
        , button  [ className btnClazz, onClick (const $ Count)]
                  [ text $ translate _.count lang ]
        , button  [ className btnClazz, onClick (const $ SetLanguage English)]
                  [ text "EN" ]
        , button  [ className btnClazz, onClick (const $ SetLanguage German)]
                  [ text "DE" ]
        ]
      where
        lang = state.lang
        btnClazz = "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"
