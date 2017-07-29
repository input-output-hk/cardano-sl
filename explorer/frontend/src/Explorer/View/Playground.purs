module Explorer.View.Playground where

import Prelude

import Data.Monoid (mempty)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup ((!))

playgroundView :: State -> P.HTML Action
playgroundView state =
    S.div ! S.className "explorer-calculator"
          $ S.div ! S.className "explorer-calculator__container"
                  $ mempty
