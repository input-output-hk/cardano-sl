module Explorer.View.NotFound (notFoundView) where

import Data.Lens ((^.))
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)

import Text.Smolder.HTML (div, a)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!))

import Pux.DOM.HTML (HTML) as P

notFoundView :: State -> P.HTML Action
notFoundView state =
    let lang' = state ^. lang in
    div $ className "explorer-404" $ do
        div $ className "explorer-404__wrapper" $ do
            div ! className "explorer-404__container" $ do
                a ! href (toUrl Dashboard)
                  #! onClick (toUrl Dashboard)
                  ! className "bg-image-404"
