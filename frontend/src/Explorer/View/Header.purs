module Explorer.View.Header (headerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.Lenses.State (gViewMobileMenuOpenend, gViewTitle, globalViewState, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.View.Common (clickableLogoView, langView)
import Explorer.View.Search (searchInputView, searchItemViews)
import Pux.Html (Html, div, header, text) as P
import Pux.Html.Attributes (className) as P
import Pux.Html.Events (onClick) as P

headerView :: State -> P.Html Action
headerView state =
    let lang = state.lang
        mobileMenuOpenend = state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)
    in
    P.header
        [ P.className "explorer-header"]
        [ P.div
            [ P.className "explorer-header__wrapper--vtop"]
            [ P.div
                [ P.className "explorer-header__container" ]
                [ clickableLogoView Dashboard
                -- desktop views
                , P.div
                    [ P.className "middle-content__search"]
                    [ P.div
                        [ P.className "middle-content__search--wrapper"]
                        [ searchInputView state ]
                    ]
                , P.div
                    [P.className "right-content__currency"]
                    []
                -- mobile views
                , P.div
                    [ P.className "middle-content__title" ]
                    [ if mobileMenuOpenend
                      then searchItemViews state
                      else P.text $ state ^. (viewStates <<< globalViewState <<< gViewTitle)
                    ]
                , P.div
                    [ P.className "right-content__hamburger" ]
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
          , P.div
              [ P.className "explorer-header__wrapper--vmiddle"]
              [ P.div
                  [ P.className "vmiddle-content__search--wrapper" ]
                  [ searchInputView state
                  ]
              ]
          , P.div
              [ P.className "explorer-header__wrapper--vbottom"]
              [ langView state
              ]
        ]
