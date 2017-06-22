module Explorer.View.Header (headerView) where

import Prelude
import Data.Lens ((^.))
import Data.Monoid (mempty)

import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewSelectedSearch, gViewTitle, globalViewState, lang, viewStates)
import Explorer.Routes (Route(..))
import Explorer.State (headerSearchContainerId, mobileMenuSearchContainerId)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.CSS (header, headerId) as CSS
import Explorer.View.Common (clickableLogoView, langView)
import Explorer.View.Search (searchInputView, searchItemViews)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML ( div, header) as S
import Text.Smolder.HTML.Attributes (className, id) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!))

headerView :: State -> P.HTML Action
headerView state =
    let lang' = state ^. lang
        selectedSearch = state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch)
        mobileMenuOpenend = state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
    in
    S.header  ! S.className CSS.header
              ! S.id CSS.headerId $ do
        S.div ! S.className "explorer-header__wrapper--vtop"
              $ S.div ! S.className "explorer-header__container" $ do
                  clickableLogoView Dashboard
                  -- desktop views
                  S.div ! S.className "middle-content__search"
                        $ S.div ! S.className "middle-content__search--wrapper"
                                $ searchInputView headerSearchContainerId state
                  S.div ! S.className "right-content__currency"
                        $ mempty
                  -- mobile views
                  S.div ! S.className "middle-content__title"
                        $ if mobileMenuOpenend
                              then searchItemViews lang' selectedSearch
                              else S.text (state ^. (viewStates <<< globalViewState <<< gViewTitle))
                  S.div ! S.className "right-content__hamburger"
                        $ S.div ! S.className (if mobileMenuOpenend
                                          then "cross__icon bg-icon-cross"
                                          else "hamburger__icon bg-icon-hamburger")
                        #! P.onClick (const <<< GlobalToggleMobileMenu $ not mobileMenuOpenend)
                        $ mempty
        S.div ! S.className "explorer-header__wrapper--vmiddle" $ do
            S.div ! S.className "vmiddle-content__search--wrapper"
                  $ searchInputView mobileMenuSearchContainerId state
        S.div ! S.className "explorer-header__wrapper--vbottom"
            $ langView state
