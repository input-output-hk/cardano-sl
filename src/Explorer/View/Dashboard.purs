module Explorer.View.Dashboard (dashboardView) where

import Prelude
import Explorer.I18n.Lang (translate)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types (State, Action(..))
import Explorer.Util.String (substitute)
import Explorer.View.Common (placeholderView)
import Pux.Html (Html, button, div, h3, text, h1, h2, input) as P
import Pux.Html.Attributes (className, type_, placeholder) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P

dashboardView :: State -> P.Html Action
dashboardView state = do
    let lang = state.lang
    P.div
        [ P.className "explorer-dashboard" ]
        [ P.div
              [ P.className "" ]
              [ P.div
                  [ P.className "explorer-dashboard__hero"]
                  [ P.div
                        [ P.className "explorer-dashboard__container" ]
                        [ P.h1
                              [ P.className "headline__hero"]
                              [ P.text $ translate _.title state.lang ]
                          , P.h2
                              [ P.className "subheadline__hero"]
                              [ P.text $ translate _.subtitle lang ]
                          , P.input
                              [ P.className "input__hero"
                                , P.type_ "text"
                                , P.placeholder "# Search for address, block, token"]
                              []
                        ]
                  ]
              ]
        , P.div
              [ P.className "explorer-dashboard__wrapper" ]
              [ P.div
                [ P.className "explorer-dashboard__container" ]
                [ placeholderView "placeholder 1"
                , placeholderView "placeholder 2"
                , placeholderView "placeholder 3"
                , P.h3
                    [ P.className "label-count" ]
                    [ P.text <<< flip substitute [show state.count] $ translate _.counted lang ]
                , P.button
                    [ P.className "btn", P.onClick (const $ Count) ]
                    [ P.text $ translate _.count lang ]
                , P.link (toUrl Transaction)
                    [ P.className "btn" ]
                    [ P.text $ translate _.transaction lang ]
                , P.link (toUrl Address)
                    [ P.className "btn" ]
                    [ P.text $ translate _.address lang ]
                , P.link (toUrl Calculator)
                    [ P.className "btn" ]
                    [ P.text $ translate _.calculator lang ]
                ]
              ]
        ]
