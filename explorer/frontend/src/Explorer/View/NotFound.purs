module Explorer.View.NotFound (notFoundView) where

import Prelude

import Data.Lens ((^.))
import Data.Monoid (mempty)

import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)

import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (div, a) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup ((!), (#!))

import Pux.DOM.HTML (HTML) as P

notFoundView :: State -> P.HTML Action
notFoundView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-404"
          $ S.div ! S.className "explorer-404__wrapper"
                  $ S.div ! S.className "explorer-404__container"
                          $ S.a ! S.href (toUrl Dashboard)
                              #! P.onClick (Navigate (toUrl Dashboard))
                              ! S.className "bg-image-404"
                              $ mempty
