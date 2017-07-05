module Explorer.View.Header (headerView) where

import Prelude

import Data.Lens ((^.))
import Data.Monoid (mempty)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cAddress, cBlock, cCalculator, cEpoch, cSlot, cTitle, cTransaction, notfound, nfTitle) as I18nL
import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewSelectedSearch, globalViewState, lang, route, viewStates)
import Explorer.Routes (Route(..))
import Explorer.State (headerSearchContainerId, mobileMenuSearchContainerId)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.CSS (header, headerId) as CSS
import Explorer.View.Common (clickableLogoView, langView)
import Explorer.View.Search (searchInputView, searchItemViews)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, header) as S
import Text.Smolder.HTML.Attributes (className, id) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

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
                              else S.text $ title (state ^. route) lang'
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

title :: Route -> Language -> String
title Dashboard lang = translate (I18nL.common <<< I18nL.cTitle) lang
title (Tx id) lang = translate (I18nL.common <<< I18nL.cTransaction) lang
title (Address address) lang = translate (I18nL.common <<< I18nL.cAddress) lang
title (Epoch epoch) lang = translate (I18nL.common <<< I18nL.cEpoch) lang
title (EpochSlot epoch slot) lang =
  epochTitle <> " / " <> slotTitle
  where
    epochTitle = translate (I18nL.common <<< I18nL.cEpoch) lang
    slotTitle = translate (I18nL.common <<< I18nL.cSlot) lang
title Calculator lang = translate (I18nL.common <<< I18nL.cCalculator) lang
title (Block hash) lang = translate (I18nL.common <<< I18nL.cBlock) lang
title Playground _ = "Playground"
title NotFound lang = translate (I18nL.notfound <<< I18nL.nfTitle) lang
