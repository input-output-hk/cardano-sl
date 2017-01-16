module App.Example where

import Pux.Html (Html, button, div, h1, h3, text, footer, ul, li, header)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className)
import Prelude hiding (div)
import Util.Version (version, commitHash)

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
  div [ className "app-container mdl-layout"]
    [ header [ className "mdl-layout__header"]
      [ div [ className "mdl-layout__header-row" ]
        [ h1 []
          [ text "cardano-sl explorer" ]
        ]
      ]
    , div [ className "content" ]
      [ h3 [ className "label-count" ] [ text $ "counted: " <> show state.count]
      , button
          [ className "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"
          , onClick (const $ Count) ]
          [ text "count" ]
      ]
    , footer [ className "mdl-mega-footer" ]
      [ div [ className "mdl-mega-footer__bottom-section" ]
        [ ul [ className "mdl-mega-footer__link-list" ]
          [ li [] [ text $ "version: " <> show version ]
          , li [] [ text $ "commit: " <> commitHash ]
          ]
        ]
      ]
    ]
