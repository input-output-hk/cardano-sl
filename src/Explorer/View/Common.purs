module Explorer.View.Common (placeholderView) where

import Explorer.State (Action)
import Pux.Html (Html, text, div) as P
import Pux.Html.Attributes (className) as P

placeholderView :: String -> P.Html Action
placeholderView label =
    P.div
        [ P.className "explorer-dashboard__content" ]
        [ P.text label ]
