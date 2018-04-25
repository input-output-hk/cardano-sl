module Explorer.View.Calculator where

import Prelude

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (cBack, cCalculator, common) as I18nL
import Explorer.Types.State (State)
import Explorer.Types.Actions (Action(..))
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

import Text.Smolder.HTML (div, a) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!))

import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P

calculatorView :: State -> P.HTML Action
calculatorView state =
    S.div ! S.className "explorer-calculator"
          $ S.div ! S.className "explorer-calculator__container" $ do
              S.a ! S.className ""
                  ! S.href (toUrl Dashboard)
                  #! P.onClick (Navigate $ toUrl Dashboard)
                  $ S.text (translate (I18nL.common <<< I18nL.cBack) state.lang)
              placeholderView $ translate (I18nL.common <<< I18nL.cCalculator) state.lang
