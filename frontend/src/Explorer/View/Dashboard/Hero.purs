module Explorer.View.Dashboard.Hero (heroView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (hero, hrTitle, hrSearch, hrSubtitle) as I18nL
import Explorer.Lenses.State (searchInput)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Pux.Html (Html, div, text, h1, h2, input) as P
import Pux.Html.Attributes (className, type_, placeholder) as P
import Pux.Html.Events (onClick, onFocus, onBlur) as P

heroView :: State -> P.Html Action
heroView state =
    let
        searchInputFocused = state ^. (dashboardViewState <<< searchInput)
        focusedClazz = if searchInputFocused then " focused" else ""
        searchIconClazz = if searchInputFocused then " bg-icon-search-hover" else " bg-icon-search"
    in
    P.div
        [ P.className "explorer-dashboard__hero" ]
        [ P.div
            [ P.className "hero-container" ]
            [ P.h1
                [ P.className "hero-headline" ]
                [ P.text $ translate (I18nL.hero <<< I18nL.hrTitle) state.lang ]
            , P.h2
                [ P.className "hero-subheadline"]
                [ P.text $ translate (I18nL.hero <<< I18nL.hrSubtitle) state.lang ]
            , P.div
                [ P.className $ "hero-search-container" <> focusedClazz ]
                [ P.input
                    [ P.className $ "hero-input" <> focusedClazz
                      , P.type_ "text"
                      , P.placeholder $ if searchInputFocused
                                        then ""
                                        else translate (I18nL.hero <<< I18nL.hrSearch) state.lang
                      , P.onFocus <<< const $ DashboardFocusSearchInput true
                      , P.onBlur <<< const $ DashboardFocusSearchInput false ]
                    []
                , P.div
                    [ P.className $ "hero-search-btn" <> searchIconClazz <> focusedClazz
                    , P.onClick $ const DashboardSearch ]
                    []
                ]
            ]
        ]
