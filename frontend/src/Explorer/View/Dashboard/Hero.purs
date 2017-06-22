module Explorer.View.Dashboard.Hero
    ( heroView
    ) where

import Prelude

import Data.Lens ((^.))

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (common, hero, cTitle, hrSubtitle) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.State (heroSearchContainerId)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (logoView)
import Explorer.View.Search (searchInputView)

import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div, h1, h2) as S
import Text.Smolder.HTML.Attributes (className, id) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

heroView :: State -> P.HTML Action
heroView state =
    let
        lang' = state ^. lang
    in
    S.div ! S.className "explorer-dashboard__hero"
          ! S.id "explorer-dashboard__hero-id"
          $ S.div ! S.className "hero-container" $ do
                logoView
                S.h1  ! S.className "hero-headline"
                      $ S.text (translate (I18nL.common <<< I18nL.cTitle) lang')
                S.h2  ! S.className "hero-subheadline"
                      $ S.text $ (translate (I18nL.hero <<< I18nL.hrSubtitle) lang')
                searchInputView heroSearchContainerId state
