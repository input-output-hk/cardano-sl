module App.Example where

import Pux.Html (Html, button, div, h1, h3, text)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)
import Prelude hiding (div)

data Action
    = Count
    | NoOp

newtype State = State {
    count :: Int
}

initialState :: State
initialState = State {
    count: 0
}

update :: Action -> State -> State
update Count (State state) = State state { count = state.count + 1 }
update NoOp state = state

view :: State -> Html Action
view (State state) =
  div []
    [ h1 [] [ text "Let's start!" ]
    , h3 [className "label-count"] [ text $ "counted: " <> show state.count]
    , button [ onClick (const $ Count) ] [ text "count" ]
    ]
