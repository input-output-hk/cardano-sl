module Explorer.View.Hero
    ( heroView
    ) where

import Prelude

import Data.Lens ((^.))

import Explorer.Lenses.State (testnet, lang)
import Explorer.State (heroSearchContainerId)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (logoView)
import Explorer.View.Search (searchInputView)

import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div) as S
import Text.Smolder.HTML.Attributes (className, id) as S
import Text.Smolder.Markup ((!))

heroView :: State -> P.HTML Action
heroView state =
    let
        lang' = state ^. lang
        testnet' = state ^. testnet
    in
    S.div ! S.className "home-menu pure-g pure-menu-fixed pure-menu-horizontal"
          ! S.id "explorer-dashboard__hero-id" $ do
      S.div ! S.className "pure-u-1-3 pure-u-md-1-2" $ do
            logoView state
      S.div ! S.className "pure-u-2-3 pure-u-md-1-2" $ do
        -- S.h2  ! S.className "hero-subheadline"
        --       $ S.text $ (translate (I18nL.hero <<< I18nL.hrSubtitle) lang')
        searchInputView heroSearchContainerId state
