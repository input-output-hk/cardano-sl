module Explorer.View.Header (headerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (navBlockchain, navCharts, navHome, navMarket, navigation, navTools) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.Html (Html, div, text, header, nav, span) as P
import Pux.Html.Attributes (className) as P

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
                        [ P.div
                            [ P.className "logo__img bg-logo" ]
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
