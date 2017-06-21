module Explorer.View.NotFound (notFoundView) where

import Prelude

import Data.Lens ((^.))
import Data.Monoid (mempty)

import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)

import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (div, a)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!), (#!))

import Pux.DOM.HTML (HTML) as P

notFoundView :: State -> P.HTML Action
notFoundView state =
    let lang' = state ^. lang in
    div ! className "explorer-404"
        $ div ! className "explorer-404__wrapper"
              $ div ! className "explorer-404__container"
                    $ a ! href (toUrl Dashboard)
                        #! P.onClick (Navigate (toUrl Dashboard))
                        ! className "bg-image-404"
                        $ mempty
