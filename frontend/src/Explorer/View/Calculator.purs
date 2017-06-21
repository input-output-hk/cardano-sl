module Explorer.View.Calculator where

import Prelude

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (cBack, cCalculator, common) as I18nL
import Explorer.Types.State (State)
import Explorer.Types.Actions (Action)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

import Text.Smolder.HTML (div, a)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (#!), (!))

import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (Html) as P

calculatorView :: State -> P.Html Action
calculatorView state =
    div ! className "explorer-calculator" $ do
        div
            ! className "explorer-calculator__container" $ do
            a
                ! href (toUrl Dashboard)
                ! className ""
                #! onClick (toUrl Dashboard)
                $ text (translate (I18nL.common <<< I18nL.cBack) state.lang)
            placeholderView $ translate (I18nL.common <<< I18nL.cCalculator) state.lang
