module Explorer.View.NotFound (notFoundView) where

import Data.Lens ((^.))
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.Html (Html, div) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P


notFoundView :: State -> P.Html Action
notFoundView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-404" ]
        [ P.div
            [ P.className "explorer-404__wrapper" ]
            [ P.div
                [ P.className "explorer-404__container" ]
                [ P.link (toUrl Dashboard)
                  [ P.className "bg-image-404" ]
                  []
                ]
            ]
        ]
