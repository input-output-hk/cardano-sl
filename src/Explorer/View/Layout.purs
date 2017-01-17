module Explorer.View.Layout where

import Pux.Html (Html, div, h1, h2, text, footer, ul, li, header, main, nav, a, span)
import Pux.Html.Attributes (className, href)
import Prelude hiding (div)
import Explorer.Util.Version (version, commitHash)
import Explorer.State (State, Action)
import Explorer.View.Dashboard (dashboardView)
import Explorer.I18n.Lang (translate)

view :: State -> Html Action
view state =
    div [ className "mdl-layout__container"]
        [
          div [ className "mdl-layout mdl-js-layout"]
              [ headerView state
              , main [ className "mdl-layout__content content-container" ]
              [ dashboardView state ]
              , footerView state
              ]
        ]

headerView :: State -> Html Action
headerView state =
    header  [ className "mdl-layout__header mdl-layout__header--waterfall explorer-header"]
            [ div [ className "mdl-layout__header-row explorer-header__top" ]
                  [ nav [ className "mdl-navigation explorer-header__nav" ]
                        [ a [ className "mdl-navigation__link", href "" ]
                            [ text $ translate (_.nav.home) lang ]
                        , a [ className "mdl-navigation__link", href "" ]
                            [ text $ translate (_.nav.blockchain) lang ]
                        , a [ className "mdl-navigation__link", href "" ]
                            [ text $ translate (_.nav.market) lang ]
                        , a [ className "mdl-navigation__link", href "" ]
                            [ text $ translate (_.nav.charts) lang ]
                        , a [ className "mdl-navigation__link", href "" ]
                            [ text $ translate (_.nav.tools) lang ]
                        ]
                  ]
            , div [ className "mdl-layout__header-row explorer-header__content" ]
                  [ div [className "mdl-layout__title"]
                        [ h1  [ className "mdl-layout__title headline"]
                              [ text $ translate _.title state.lang ]
                        , h2  [ className "mdl-layout__title subheadline"]
                              [ text $ translate _.subtitle lang ]
                        ]
                  ]
            ]
    where
      lang = state.lang

footerView :: State -> Html Action
footerView state =
    footer [ className "mdl-mega-footer explorer-footer" ]
    [ div [ className "mdl-mega-footer__bottom-section explorer-footer__content" ]
          [ ul  [ className "mdl-mega-footer__link-list" ]
                [ li [] [ text $ "version: " <> show version ]
                , li [] [ text $ "commit: " <> commitHash ]
                , li [] [ text $ "lang: " <> show state.lang ]
        ]
      ]
    ]
