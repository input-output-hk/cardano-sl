module Explorer.View.Header (headerView) where

import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.Routes (Route(Dashboard), toUrl)
import Pux.Html (Html, div, header) as P
import Pux.Html.Attributes (className, href) as P
import Pux.Router (link) as P

headerView :: State -> P.Html Action
headerView state = do
    let lang = state.lang
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
                , P.div
                    [ P.className "nav__container" ]
                    []
                , P.div
                    [P.className "currency__container"]
                    []
                ]
            ]
        ]
