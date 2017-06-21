module Explorer.View.Dashboard.Hero
    ( heroView
    ) where

import Prelude hiding (id)

import Data.Lens ((^.))

import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div, text, h1, h2)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (text)

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (common, hero, cTitle, hrSubtitle) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.State (heroSearchContainerId)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (logoView)
import Explorer.View.Search (searchInputView)

heroView :: State -> P.HTML Action
heroView state =
    let
        lang' = state ^. lang
    in
    div ! className "explorer-dashboard__hero"
        ! id_ "explorer-dashboard__hero-id"
        $ div ! className "hero-container" $ do
              logoView
              h1 ! className "hero-headline"
                 $ text (translate (I18nL.common <<< I18nL.cTitle) lang')
              h2 ! className "hero-subheadline"
                 $ text $ (translate (I18nL.hero <<< I18nL.hrSubtitle) lang')
              searchInputView heroSearchContainerId state
