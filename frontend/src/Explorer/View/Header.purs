module Explorer.View.Header (headerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewTitle, globalViewState, viewStates)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Pux.Html (Html, div, header, text) as P
import Pux.Html.Attributes (className, href) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P

headerView :: State -> P.Html Action
headerView state =
    let lang = state.lang
        mobileMenuOpenend = state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
    in
    P.header
        [ P.className "explorer-header"]
        [ P.div
            [ P.className "explorer-header__top"]
            [ P.div
                [ P.className "explorer-header__container" ]
                [ P.div
                    [ P.className "logo__container"]
                    [ P.div
                        [ P.className "logo__wrapper"]
                        [ P.link (toUrl Dashboard)
                            [ P.className "logo__img bg-logo"
                            , P.href "/"]
                            []
                        ]
                    ]
                -- desktop views
                , P.div
                    [ P.className "nav__container" ]
                    []
                , P.div
                    [P.className "currency__container"]
                    []
                -- mobile views
                , P.div
                    [ P.className "title__container" ]
                    [ P.text $ state ^. (viewStates <<< globalViewState <<< gViewTitle) ]
                , P.div
                    [ P.className "hamburger__container" ]
                    [ P.div
                        [ P.className
                              if mobileMenuOpenend
                              then "cross__icon bg-icon-cross"
                              else "hamburger__icon bg-icon-hamburger"
                        , P.onClick <<< const <<< GlobalToggleMobileMenu $ not mobileMenuOpenend
                        ]
                        []
                    ]
                ]

            ]
        ]
