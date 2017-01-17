module Explorer.View.Dashboard where

import Pux.Html (Html, button, div, h3, text)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)
import Prelude hiding (div)
import Explorer.State (State, Action(..))

dashboardView :: State -> Html Action
dashboardView state =
    div [ className "dashboard" ]
        [ h3  [ className "label-count" ]
              [ text $ "counted: " <> show state.count]
        , button  [ className "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"
                  , onClick (const $ Count)
                  ]
                  [ text "count" ]
        ]
