module Explorer.View.Header (headerView) where

import Prelude hiding (id)
import Data.Lens ((^.))

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML ( div, header)
import Text.Smolder.HTML.Attributes (className, id)
import Text.Smolder.Markup (text, (#!), (!))

import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewSelectedSearch, gViewTitle, globalViewState, lang, viewStates)
import Explorer.Routes (Route(..))
import Explorer.State (headerSearchContainerId, mobileMenuSearchContainerId)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.CSS (header, headerId) as CSS
import Explorer.View.Common (clickableLogoView, langView)
import Explorer.View.Search (searchInputView, searchItemViews)

headerView :: State -> P.HTML Action
headerView state =
    let lang' = state ^. lang
        selectedSearch = state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch)
        mobileMenuOpenend = state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
    in
    header  ! className CSS.header
            ! id CSS.headerId $ do
            div ! className "explorer-header__wrapper--vtop"
                    div ! className "explorer-header__container" $ do
                        clickableLogoView Dashboard
                        -- desktop views
                        div ! className "middle-content__search" $ do
                            div ! className "middle-content__search--wrapper"
                                $ searchInputView headerSearchContainerId state
                        div ! className "right-content__currency"
                        -- mobile views
                        div ! className "middle-content__title"
                            $ if mobileMenuOpenend
                                  then searchItemViews lang' selectedSearch
                                  else text $ state ^. (viewStates <<< globalViewState <<< gViewTitle)
                        div ! className "right-content__hamburger" $ do
                            div ! className if mobileMenuOpenend
                                                then "cross__icon bg-icon-cross"
                                                else "hamburger__icon bg-icon-hamburger"
                                #! onClick <<< const <<< GlobalToggleMobileMenu $ not mobileMenuOpenend
            div ! className "explorer-header__wrapper--vmiddle" $ do
                div ! className "vmiddle-content__search--wrapper"
                    $ searchInputView mobileMenuSearchContainerId state
            div ! className "explorer-header__wrapper--vbottom"
                $ langView state
